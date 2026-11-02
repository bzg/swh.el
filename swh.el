;;; swh.el --- Software Heritage archival status from Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry

;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: vc, tools
;; URL: https://github.com/bzg/swh.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Software Heritage (https://www.softwareheritage.org) archives source
;; code and identifies every artifact with a SWHID.  This package tells
;; whether the code in front of you is archived, and lets you cite it.
;;
;; `swh-swhid' copies the SWHID of the current file or region, or in
;; Dired of the file or directory at point.  `swh-cite' shows a BibTeX
;; entry citing it, and `org-store-link' stores it as a swh: link.
;; `swh-mode' shows the archival status in the mode line;
;; `global-swh-mode' turns it on everywhere.
;;
;; `swh-status' tells whether the repository's origin is archived,
;; `swh-visits' lists its last visits and `swh-save-code-now' asks to
;; archive it.  With a prefix argument, or without a public origin,
;; these commands ask for the URL of a public repository instead.
;;
;; `swh-scan' shows which parts of the working tree are archived;
;; there, `swh-provenance' tells where the archive found them, if your
;; API token gives access to the Provenance API.
;;
;; Nothing but Git is needed: swh.el talks to the archive's Web API.
;; When the local swhd daemon runs, swh.el goes through it and its
;; persistent cache of SWHIDs, which spares the API's rate limits.

;;; Code:

(require 'url)
(require 'iso8601)
(require 'auth-source)
(require 'seq)
(require 'vc-git)
(require 'outline)

(defgroup swh nil
  "Software Heritage client."
  :group 'tools
  :prefix "swh-")

(defcustom swh-daemon-url "http://127.0.0.1:8642"
  "Base URL of the local swhd daemon, used when it answers there.
The API token goes to it with each request, unencrypted: keep it local."
  :type 'string
  :package-version '(swh . "0.1"))

(defcustom swh-archive-url "https://archive.softwareheritage.org"
  "Web root of the Software Heritage archive, its Web API at /api/1.
The daemon tells its own: point them at the same instance."
  :type 'string
  :package-version '(swh . "0.1"))

(defcustom swh-org-store-link nil
  "Whether `org-store-link' stores a swh: link in buffers under Git.
With `ask', ask each time.  Org prefers such links to file: links;
\\[universal-argument] \\[universal-argument] \\[org-store-link] stores a file: link anyway."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Yes" t)
                 (const :tag "Ask each time" ask))
  :package-version '(swh . "0.1"))

(defcustom swh-mode-lighters
  '((archived . " SWH✓")
    (not-archived . " SWH✗")
    (origin-archived . " SWH~")
    (pending . " SWH…")
    (error . " SWH!"))
  "Mode-line lighters of `swh-mode', by archival status.
`origin-archived': the file is not archived, but its origin is."
  :type '(alist :key-type symbol :value-type string)
  :package-version '(swh . "0.1"))

(defvar url-http-response-status)
(defvar url-http-end-of-headers)
(declare-function org-link-set-parameters "ol")
(declare-function org-link-store-props "ol")
(declare-function dired-get-filename "dired")
(declare-function org-export-derived-backend-p "ox")

;;; HTTP

(defun swh--fresh-p (time ttl)
  "Non-nil when TIME is less than TTL seconds ago; nil TIME is never fresh."
  (and time (< (- (float-time) time) ttl)))

;; An HTTP response is (STATUS BODY RETRY-AFTER), see `swh--response'.

(defsubst swh--resp-status (resp)
  "The HTTP status of the response RESP."
  (car resp))

(defsubst swh--resp-body (resp)
  "The parsed JSON body of the response RESP, or `not-json'."
  (nth 1 resp))

(defsubst swh--resp-retry-after (resp)
  "The Retry-After of the response RESP in seconds, if any."
  (nth 2 resp))

(defun swh--2xx-p (resp)
  "Whether the status of the response RESP is 2xx."
  (and resp (<= 200 (swh--resp-status resp) 299)))

(defun swh--base-url (url)
  "URL, a base URL option, without its trailing slashes."
  (string-trim-right url "/+"))

(defun swh--api-url (path)
  "URL of PATH in the archive's Web API, see `swh-archive-url'."
  (concat (swh--base-url swh-archive-url) "/api/1" path))

(defvar swh--rate-limit nil
  "(USED LIMIT RESET): the API's tokenless rate limit, RESET in epoch seconds.")

(defun swh--header-number (name end)
  "The number the header NAME gives in the headers ending at END, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (and end
           (re-search-forward (format "^%s: *\\([0-9]+\\)" (regexp-quote name)) end t)
           (string-to-number (match-string 1))))))

(defun swh--track-rate-limit (end)
  "Remember the rate limit the headers, ending at END, tell."
  (let ((limit (swh--header-number "X-RateLimit-Limit" end))
        (remaining (swh--header-number "X-RateLimit-Remaining" end)))
    (when (and limit remaining)
      (setq swh--rate-limit
            (list (- limit remaining) limit
                  (or (swh--header-number "X-RateLimit-Reset" end)
                      (+ (float-time) 3600))))
      (force-mode-line-update t))))

(defun swh--response (buffer &optional track)
  "The HTTP response in BUFFER, which is killed: (STATUS BODY RETRY-AFTER).
BODY is the parsed JSON, or `not-json'; nil without a status, as on a
refused connection.  With TRACK, update `swh--rate-limit'."
  (unwind-protect
      (with-current-buffer buffer
        (let ((status (bound-and-true-p url-http-response-status))
              (headers-end (bound-and-true-p url-http-end-of-headers)))
          (when status
            (when track (swh--track-rate-limit headers-end))
            (let ((retry-after (swh--header-number "Retry-After" headers-end)))
              (goto-char (or headers-end (point-min)))
              (list status
                    (condition-case nil
                        (json-parse-buffer :object-type 'alist
                                           :false-object nil :null-object nil)
                      (json-error 'not-json))
                    retry-after)))))
    (kill-buffer buffer)))

(defun swh--tracked-p (url headers)
  "Whether a request to URL with HEADERS counts against the tokenless limit."
  (and (string-prefix-p (swh--api-url "") url)
       (not (assoc "Authorization" headers))))

(defun swh--url-apply (method body headers fn &rest args)
  "Apply FN to ARGS with url.el set to send METHOD, the JSON BODY and HEADERS.
Never prompts, not even on a 401."
  (let ((url-request-method method)
        (url-request-data (and body (encode-coding-string body 'utf-8)))
        (url-request-extra-headers headers)
        (url-request-noninteractive t))
    (apply fn args)))

(defun swh--retrieve-async (method url body headers callback &optional timeout)
  "Send METHOD to URL with the JSON string BODY and HEADERS, in the background.
CALLBACK gets the response, see `swh--response', once: nil when URL
cannot be reached within TIMEOUT seconds (60 by default)."
  (let* ((answered nil)
         (buffer nil)
         (tracked (swh--tracked-p url headers))
         (answer (lambda (resp)
                   (unless answered
                     (setq answered t)
                     (funcall callback resp))))
         (timer (run-at-time (or timeout 60) nil
                             (lambda ()
                               ;; give up on the request and free its connection
                               (when (buffer-live-p buffer)
                                 (when-let* ((proc (get-buffer-process buffer)))
                                   (set-process-sentinel proc #'ignore)
                                   (delete-process proc))
                                 (kill-buffer buffer))
                               (funcall answer nil)))))
    (condition-case nil
        ;; url.el puts an :error in STATUS for every 4xx/5xx too; the
        ;; response says it all, and must be consumed to free its buffer
        (setq buffer
              (swh--url-apply method body headers #'url-retrieve url
                              (lambda (_status)
                                (cancel-timer timer)
                                (if answered
                                    (kill-buffer (current-buffer))
                                  (funcall answer
                                           (swh--response (current-buffer) tracked))))
                              nil t t))
      (error (cancel-timer timer)
             (funcall answer nil)))
    ;; url.el would have Emacs ask, on exit, about a request still
    ;; waiting for its answer; a background request is not worth that
    (when-let* ((proc (and buffer (get-buffer-process buffer))))
      (set-process-query-on-exit-flag proc nil))))

(defun swh--wait (fn)
  "Call FN with a callback and a FAILED function, and wait for one of them.
Return what the callback got; signal what FAILED got as a `user-error'."
  ;; the backend is asynchronous, for the mode line's sake: this is
  ;; how the commands, which wait, use it
  (let ((result 'pending))
    (funcall fn (lambda (value) (setq result (list value)))
             (lambda (why) (setq result why)))
    (while (eq result 'pending)
      (accept-process-output nil 0.1))
    (if (consp result) (car result) (user-error "%s" result))))

(defun swh--retrieve (method url &optional body headers timeout)
  "The response to METHOD URL with BODY, HEADERS and TIMEOUT, waited for.
See `swh--retrieve-async'."
  (swh--wait (lambda (callback _)
               (swh--retrieve-async method url body headers callback timeout))))

(defconst swh--rate-limit-message "Software Heritage API rate limit reached"
  "How a `user-error' about the API's rate limit starts.")

(defun swh--rate-limited (retry-after)
  "Signal that the API's rate limit is reached, for RETRY-AFTER seconds."
  (user-error "%s; retry in %s seconds" swh--rate-limit-message (or retry-after "a few")))

(defun swh--archive-answered (status about)
  "Signal a `user-error': the archive answered HTTP STATUS, ABOUT says to what."
  (user-error "The Software Heritage archive answered HTTP %s (%s)" status about))

(defun swh--showing-errors (fn &optional failed)
  "The callback FN, its errors passed as messages to FAILED, else shown."
  (lambda (arg)
    (condition-case err
        (funcall fn arg)
      (error
       (let ((why (error-message-string err)))
         (if failed (funcall failed why) (message "%s" why)))))))

(defun swh--remembered (var ttl fn)
  "What FN gives, remembered in VAR, a (TIME . VALUE) variable, for TTL seconds."
  (unless (swh--fresh-p (car (symbol-value var)) ttl)
    (set var (cons (float-time) (funcall fn))))
  (cdr (symbol-value var)))

(defun swh--try (failed fn)
  "Call FN, its errors passed to FAILED as in `swh--showing-errors'."
  (funcall (swh--showing-errors (lambda (_) (funcall fn)) failed) nil))

;;; The daemon backend

(defconst swh-daemon-min-version "0.8.0"
  "The oldest swhd that swh.el works with.")

;; Three calls of 60 s to the archive; a request Emacs waits for gets
;; 60 s, a slow archive is not worth more of the user's time
(defconst swh--daemon-timeout 190
  "Seconds to wait for swhd in the background.")

(defvar swh--daemon-checked nil
  "(TIME . PONG): the last ping of the daemon, see `swh--daemon-pong'.")

(defvar swh--daemon-outdated-warned nil
  "Non-nil once the user has been told that swhd is outdated.")

(defun swh--daemon-version-ok-p (pong)
  "Whether PONG, the daemon's /ping, comes from a recent enough swhd.
0.9.0-dev counts as 0.9.0."
  (let ((version (and (listp pong) (alist-get 'version pong))))
    (and (stringp version)
         (string-match "\\`[0-9]+\\(?:\\.[0-9]+\\)*" version)
         (not (version< (match-string 0 version) swh-daemon-min-version)))))

(defun swh--daemon-ping ()
  "The daemon's answer to /ping, nil when it is gone or too old (warned once)."
  (pcase (swh--retrieve "GET" (concat (swh--base-url swh-daemon-url) "/ping") nil nil 2)
    (`(200 ,pong ,_)
     (if (swh--daemon-version-ok-p pong)
         pong
       (unless swh--daemon-outdated-warned
         (setq swh--daemon-outdated-warned t)
         (display-warning
          'swh (format "swhd %s is older than %s; swh.el goes without it"
                       (or (and (listp pong) (alist-get 'version pong)) "< 0.2.0")
                       swh-daemon-min-version)))
       nil))))

(defun swh--daemon-pong ()
  "The daemon's answer to /ping, remembered for half a minute."
  (swh--remembered 'swh--daemon-checked 30 #'swh--daemon-ping))

(defun swh--daemon-available-p ()
  "Whether to go through the daemon: it answers, see `swh--daemon-pong'."
  (and (swh--daemon-pong) t))

(defun swh--daemon-url (route path params)
  "URL of the daemon's ROUTE about PATH (or nil), with the (KEY VALUE) PARAMS."
  (let ((query (url-build-query-string (append (and path `(("path" ,path))) params))))
    (concat (swh--base-url swh-daemon-url) route
            (unless (string-empty-p query) (concat "?" query)))))

(defun swh--daemon (method route &rest args)
  "Send METHOD ROUTE to the daemon, with the keyword ARGS.
:path is the repository the request is about, :params its query
parameters and :body its JSON body.  :callback gets the answer, see
`swh--daemon-answer', within :timeout seconds; errors go to :failed.
Without :callback, wait and return the answer."
  (let ((path (plist-get args :path))
        (params (plist-get args :params))
        (body (plist-get args :body))
        (callback (plist-get args :callback))
        (failed (plist-get args :failed))
        (timeout (plist-get args :timeout)))
    (if (not callback)
        (swh--wait (lambda (callback failed)
                     (swh--daemon method route :path path :params params :body body
                                  :callback callback :failed failed
                                  :timeout (or timeout 60))))
      (swh--retrieve-async method (swh--daemon-url route path params) body
                           (swh--api-headers body)
                           (swh--showing-errors
                            (lambda (resp)
                              (funcall callback (swh--daemon-answer method route resp)))
                            failed)
                           (or timeout swh--daemon-timeout)))))

(defun swh--daemon-answer (method route resp)
  "The body of RESP, the daemon's response to METHOD ROUTE, when 2xx.
Else signal a `user-error' telling what went wrong, from its body."
  (unless resp
    ;; gone: ping it again next time
    (setq swh--daemon-checked nil)
    (user-error "Cannot reach swhd at %s, or it is too slow (is it still running?)"
                swh-daemon-url))
  (let* ((status (swh--resp-status resp))
         (body (swh--resp-body resp))
         (upstream (and (listp body) (alist-get 'upstream-status body))))
    (when (string-prefix-p "/provenance" route)
      (swh--note-provenance-refusal upstream))
    (cond
     ((eq body 'not-json)
      ;; something else answers at `swh-daemon-url'
      (user-error "Invalid response from swhd (HTTP %s, not JSON)" status))
     ((swh--2xx-p resp) body)
     ((eql status 429) (swh--rate-limited (alist-get 'retry-after body)))
     ((eql status 404)
      ;; every route swh.el uses exists in a recent enough daemon
      (user-error "Unknown to swhd, %s %s (swh.el needs %s)"
                  method route swh-daemon-min-version))
     (upstream (swh--archive-answered upstream (alist-get 'error body)))
     ((alist-get 'error body)
      ;; already a user-facing message ("No remote", …)
      (user-error "%s" (alist-get 'error body)))
     (t (user-error "Unexpected response from swhd: %s" status)))))

;;; The direct backend: Git and the archive's Web API

(defun swh--git-run (dir args &optional stderr)
  "Run `vc-git-program' with ARGS in DIR: (STATUS . OUTPUT), OUTPUT trimmed.
Its standard error goes nowhere, or with STDERR into OUTPUT."
  (with-temp-buffer
    (setq default-directory (file-name-as-directory (expand-file-name dir)))
    (cons (apply #'process-file vc-git-program nil (list t stderr) nil args)
          (string-trim (buffer-string)))))

(defun swh--git (dir &rest args)
  "Trimmed output of git ARGS in DIR; nil when it fails or says nothing."
  (pcase-let ((`(,status . ,out) (swh--git-run dir args)))
    (and (eq status 0) (not (string-empty-p out)) out)))

(defun swh--git-check (dir &rest args)
  "Trimmed output of git ARGS in DIR; a `user-error' with its message on failure.
For the commands that say nothing on their standard error when they succeed."
  (pcase-let ((`(,status . ,out) (swh--git-run dir args t)))
    (if (eq status 0)
        out
      (user-error "Git %s failed in %s: %s" (car args) dir out))))

(defun swh--core (type hash)
  "Core SWHID of TYPE (\"cnt\", \"dir\", \"rev\" or \"snp\") for HASH, a SHA-1."
  (when (> (length hash) 40)
    (user-error "SWHIDs are SHA-1 based: this repository uses SHA-256"))
  (concat "swh:1:" type ":" hash))

(defun swh--type (swhid)
  "The object type of SWHID, e.g. \"cnt\"."
  (substring swhid 6 9))

(defun swh--hash (core)
  "The hash of the core SWHID CORE."
  (substring core 10))

(defun swh--short-hash (swhid)
  "The first seven digits of the hash of SWHID."
  (substring swhid 10 17))

(defun swh--browse-url (swhid)
  "URL of SWHID in the archive's web interface."
  (concat (swh--base-url swh-archive-url) "/" swhid))

(defun swh--link (text swhid)
  "TEXT as a button that opens SWHID in the archive's web interface."
  (let ((url (swh--browse-url swhid)))
    (buttonize text #'browse-url url (concat "Browse " url))))

(defun swh--short-swhid (swhid)
  "SWHID with its hash cut to its first ten digits."
  (replace-regexp-in-string "\\`\\(swh:1:[a-z]\\{3\\}:[0-9a-f]\\{10\\}\\)[0-9a-f]*" "\\1" swhid))

(defun swh--swhid-link (swhid)
  "SWHID, its hash shortened, as a button that opens it in the archive."
  (swh--link (swh--short-swhid swhid) swhid))

(defun swh--repo-root-of (file)
  "Root of the Git repository holding FILE, nil outside one.
nil for a remote FILE too: swh.el handles local files only, see `swh--local'."
  (unless (file-remote-p file)
    (when-let* ((root (vc-git-root file)))
      (expand-file-name root))))

(defun swh--remote-url (root)
  "URL of the remote of the repository at ROOT, nil without one.
The one the branch tracks, else origin, else the only one."
  ;; Git's own rule, which `git ls-remote --get-url' tells in one run,
  ;; but a branch tracking a local branch (".") or a remote that is
  ;; not configured counts as none
  (let ((url (swh--git root "ls-remote" "--get-url")))
    (if (and url (string-match-p "[:/]" url))
        url
      (let ((remotes (split-string (or (swh--git root "remote") "") "\n" t)))
        (when-let* ((remote (cond ((member "origin" remotes) "origin")
                                  ((= 1 (length remotes)) (car remotes)))))
          (swh--git root "remote" "get-url" remote))))))

(defun swh--normalize-origin (url)
  "The Git remote URL in the archive's form: no user, slash or \".git\" suffix.
ssh://, git:// and \"git@host:path\" URLs become https ones; local paths
and file:// URLs are returned as is."
  (let ((case-fold-search t)
        (strip (lambda (u)
                 (replace-regexp-in-string
                  "\\.git\\'" "" (replace-regexp-in-string "/+\\'" "" u)))))
    (cond
     ((string-match (concat "\\`\\(https?\\|ssh\\|git\\|git\\+ssh\\|ssh\\+git\\)://"
                            "\\(?:[^@/]+@\\)?\\([^/:@]+\\)\\(:[0-9]+\\)?\\(/.*\\)\\'")
                    url)
      ;; ssh:// and git:// become https; their port is not the web one
      (let ((convert (not (member (downcase (match-string 1 url)) '("http" "https")))))
        (funcall strip
                 (concat (if convert "https" (downcase (match-string 1 url))) "://"
                         (downcase (match-string 2 url))
                         (unless convert (match-string 3 url))
                         (match-string 4 url)))))
     ((and (not (string-match-p "\\`[[:alpha:]]:[/\\\\]" url))
           (string-match "\\`\\(?:[^@/:]+@\\)?\\([^@/:]+\\):\\([^:]+\\)\\'" url)
           (not (string-prefix-p "//" (match-string 2 url))))
      (funcall strip
               (concat "https://" (downcase (match-string 1 url)) "/"
                       (replace-regexp-in-string "\\`/+" "" (match-string 2 url)))))
     (t url))))

(defun swh--public-origin-p (origin)
  "Non-nil when ORIGIN is a public URL Software Heritage can fetch."
  (string-match-p "\\`https?://" origin))

(defun swh--origin-candidates (url)
  "Origin URLs to look up for the remote URL: normalized, then with \".git\".
A non-public one alone."
  (let ((n (swh--normalize-origin url)))
    (if (swh--public-origin-p n) (list n (concat n ".git")) (list n))))

(defun swh--token-or-nil (token)
  "TOKEN trimmed; nil when it is not a string or is blank."
  (and (stringp token) (not (string-blank-p token)) (string-trim token)))

(defvar swh--auth-source-memo nil
  "(TIME . TOKEN): what auth-source last said, for a minute.")

(defun swh--token ()
  "The archive's API token, or nil.
SWH_API_TOKEN in the environment, else auth-source's password for the
archive's host (machine archive.softwareheritage.org password TOKEN)."
  (or (swh--token-or-nil (getenv "SWH_API_TOKEN"))
      ;; a search is slow, and every request needs it
      (swh--remembered 'swh--auth-source-memo 60
                       (lambda ()
                         (swh--token-or-nil
                          (auth-source-pick-first-password
                           :host (url-host (url-generic-parse-url swh-archive-url))))))))

(defun swh--api-headers (body)
  "Headers of a Web API request, with a JSON BODY or not.
Authorization carries the token, when there is one."
  (let ((token (swh--token)))
    `(("Accept" . "application/json")
      ,@(and body '(("Content-Type" . "application/json")))
      ,@(and token `(("Authorization" . ,(concat "Bearer " token)))))))

(defun swh--api (method path &optional body)
  "The archive's response to METHOD PATH with the JSON string BODY, waited for.
See `swh--api-then'."
  (swh--wait (lambda (callback failed) (swh--api-then method path body callback failed))))

(defun swh--api-answer (resp)
  "RESP, the archive's response; a `user-error' when nil or rate-limited."
  (unless resp
    (user-error "Cannot reach the Software Heritage archive at %s" swh-archive-url))
  (when (eql (swh--resp-status resp) 429)
    (swh--rate-limited (swh--resp-retry-after resp)))
  resp)

(defun swh--api-then (method path body callback failed)
  "CALLBACK gets the archive's response to METHOD PATH and the JSON string BODY.
See `swh--api-answer'; errors go to FAILED as in `swh--showing-errors'."
  (swh--retrieve-async method (swh--api-url path) body (swh--api-headers body)
                       (swh--showing-errors
                        (lambda (resp) (funcall callback (swh--api-answer resp)))
                        failed)))

(defun swh--api-body (resp what)
  "The parsed body of the 2xx response RESP; a `user-error' about WHAT otherwise."
  (cond ((not (swh--2xx-p resp)) (swh--archive-answered (swh--resp-status resp) what))
        ((eq (swh--resp-body resp) 'not-json)
         (user-error "The Software Heritage archive answered no JSON (%s)" what))
        (t (swh--resp-body resp))))

;;;; What the archive said

(defconst swh--memo-ttl 600
  "Seconds during which an answer of the archive that may change holds.")

(defvar swh--origin-memo (make-hash-table :test #'equal)
  "Remote URL -> (TIME . ORIGIN-INFO), see `swh--memo-ttl'.")

(defvar swh--known-memo (make-hash-table :test #'equal)
  "SWHID -> (TIME . ARCHIVED), see `swh--memo-get'.")

(defvar swh--provenance-memo (make-hash-table :test #'equal)
  "Core SWHID -> (TIME . FOUND), what `swh--provenance' found; see `swh--memo-get'.")

(defun swh--memo-get (memo key)
  "What MEMO, a hash table of (TIME . VALUE), says of KEY, or `unknown'.
A non-nil VALUE holds for the session, nil for `swh--memo-ttl' seconds."
  ;; a SWHID is immutable, but the archive may get the artifact later
  (pcase (gethash key memo)
    (`(,_ . ,(and value (pred identity))) value)
    (`(,time . ,_) (if (swh--fresh-p time swh--memo-ttl) nil 'unknown))
    (_ 'unknown)))

(defun swh--memo-put (memo key value)
  "Remember in MEMO that KEY is VALUE, see `swh--memo-get'; return VALUE."
  (puthash key (cons (float-time) value) memo)
  value)

(defun swh--known-p (swhid)
  "Whether `swh--known-memo' says that the archive holds SWHID."
  (eq t (swh--memo-get swh--known-memo swhid)))

(defun swh--remember-known (swhids callback failed)
  "Ask the archive about those of SWHIDS not in `swh--known-memo' yet.
Then CALLBACK gets nil; FAILED is as in `swh--showing-errors'."
  (named-let ask ((batches (seq-partition
                            (seq-filter (lambda (s)
                                          (eq 'unknown (swh--memo-get swh--known-memo s)))
                                        (seq-uniq swhids))
                            1000)))
    (if (null batches)
        (funcall callback nil)
      (swh--api-then
       "POST" "/known/" (json-serialize (vconcat (car batches)))
       (lambda (resp)
         (pcase-dolist (`(,swhid . ,answer) (swh--api-body resp "the SWHID lookup"))
           (swh--memo-put swh--known-memo (symbol-name swhid)
                          (eq t (alist-get 'known answer))))
         (ask (cdr batches)))
       failed))))

(defun swh--known (swhids)
  "Ask the archive about SWHIDS into `swh--known-memo', and wait."
  (swh--wait (lambda (callback failed) (swh--remember-known swhids callback failed))))

(defun swh--known-async (core callback failed)
  "CALLBACK gets whether the archive holds CORE, t or nil.
FAILED is as in `swh--showing-errors'."
  (swh--remember-known (list core)
                       (lambda (_) (funcall callback (swh--known-p core)))
                       failed))

(defun swh--archived-p (swhid)
  "Whether the archive holds SWHID, asked when `swh--known-memo' does not tell."
  (swh--known (list swhid))
  (swh--known-p swhid))

;;;; Origins

(defun swh--path-origin (origin)
  "ORIGIN as it goes in a Web API path: all but its pchars percent-encoded.
A remote URL from Git may hold \"?\", \"#\", blanks or non-ASCII
characters, which url-http cannot send."
  (url-hexify-string origin (append (string-to-list "/:@!$&'()*+,;=")
                                    url-unreserved-chars)))

(defun swh--latest-visit (urls callback failed)
  "CALLBACK gets (URL . VISIT) for the first of URLS the archive holds.
nil when none; VISIT is its latest visit with a snapshot.  FAILED is
as in `swh--showing-errors'."
  (if (null urls)
      (funcall callback nil)
    (swh--api-then "GET" (format "/origin/%s/visit/latest/?require_snapshot=true"
                                 (swh--path-origin (car urls)))
                   nil
                   (lambda (resp)
                     (if (eql 404 (swh--resp-status resp))
                         (swh--latest-visit (cdr urls) callback failed)
                       (funcall callback
                                (cons (car urls)
                                      (swh--api-body resp "the origin's visits")))))
                   failed)))

(defun swh--origin-info (candidates found)
  "The origin alist, as the daemon's /origin gives it.
FOUND is (URL . VISIT) for the first of the CANDIDATES archived, or nil."
  (if found
      `((origin . ,(car found))
        (archived . t)
        ,@(seq-filter (lambda (e) (memq (car e) '(date status snapshot)))
                      (cdr found)))
    `((origin . ,(car candidates)) (archived . nil))))

(defun swh--find-origin (candidates callback failed)
  "CALLBACK gets the origin alist for the first of CANDIDATES the archive holds.
See `swh--origin-info'; FAILED is as in `swh--showing-errors'."
  ;; the archive holds only public origins: look up none otherwise
  (swh--latest-visit (and (swh--public-origin-p (car candidates)) candidates)
                     (lambda (found)
                       (funcall callback (swh--origin-info candidates found)))
                     failed))

(defun swh--origin-status (origin)
  "Archival status of the public repository URL ORIGIN, see `swh--origin-info'."
  (unless (swh--public-origin-p origin)
    (user-error "Not a public repository URL: %s" origin))
  (swh--wait (lambda (callback failed)
               (swh--find-origin (swh--origin-candidates origin) callback failed))))

(defun swh--direct-origin-async (root callback failed)
  "CALLBACK gets the origin of the repository at ROOT, nil without a remote.
See `swh--origin-info'; remembered for `swh--memo-ttl' seconds.  FAILED
is as in `swh--showing-errors'."
  (if-let* ((remote (swh--remote-url root)))
      (pcase-let ((`(,time . ,info) (gethash remote swh--origin-memo)))
        (if (swh--fresh-p time swh--memo-ttl)
            (funcall callback info)
          (swh--find-origin (swh--origin-candidates remote)
                            (lambda (info)
                              (funcall callback
                                       (swh--memo-put swh--origin-memo remote info)))
                            failed)))
    (funcall callback nil)))

(defun swh--direct-origin (root)
  "The origin of the repository at ROOT, waited for.
See `swh--direct-origin-async'."
  (swh--wait (lambda (callback failed) (swh--direct-origin-async root callback failed))))

(defun swh--origin-visits (info)
  "The last ten visits of the origin INFO tells, as the daemon's /visits."
  (let ((origin (alist-get 'origin info)))
    `((origin . ,origin)
      (visits . ,(if (alist-get 'archived info)
                     (swh--api-body
                      (swh--api "GET" (format "/origin/%s/visits/?per_page=10"
                                              (swh--path-origin origin)))
                      "the origin's visits")
                   [])))))

(defun swh--direct-upstream (root info)
  "Upstream and upstream-archived of ROOT's branch, as the daemon's /origin.
INFO is ROOT's origin; nil when not archived, or without a remote upstream."
  (when-let* (((alist-get 'archived info))
              (upstream (swh--git root "rev-parse" "--symbolic-full-name" "@{upstream}"))
              ((string-prefix-p "refs/remotes/" upstream))
              (commit (swh--git root "rev-parse" "--verify" "-q" "@{upstream}^{commit}"))
              (rev (swh--rev commit)))
    `((upstream . ,rev)
      (upstream-archived . ,(swh--archived-p rev)))))

(defun swh--forget-before-save (origin)
  "Forget what a save request of ORIGIN is about to change."
  (let ((candidates (swh--origin-candidates origin)))
    (maphash (lambda (remote entry)
               (when (member (alist-get 'origin (cdr entry)) candidates)
                 (remhash remote swh--origin-memo)))
             swh--origin-memo))
  (maphash (lambda (swhid answer) (unless (cdr answer) (remhash swhid swh--known-memo)))
           swh--known-memo))

(defun swh--save-origin-async (origin callback)
  "Ask the archive to save ORIGIN now; CALLBACK gets the save request."
  (unless (swh--public-origin-p origin)
    (user-error "The origin is not a public URL"))
  (swh--api-then "POST" (format "/origin/save/git/url/%s/" (swh--path-origin origin)) nil
                 (lambda (resp)
                   (let ((request (swh--api-body resp "the save request")))
                     (swh--forget-before-save origin)
                     (funcall callback (cons (cons 'origin origin) request))))
                 nil))

;;;; SWHIDs

(defun swh--content-swhid (file root)
  "Core content SWHID of FILE, as Git stores it in ROOT, or as is without ROOT."
  (swh--core "cnt"
             (or (if root
                     (swh--git root "hash-object" "--path" (file-relative-name file root)
                               "--" (expand-file-name file))
                   (swh--git (file-name-directory file) "hash-object" "--no-filters"
                             "--" (expand-file-name file)))
                 (user-error "Cannot hash %s with git" file))))

(defun swh--encode-origin (origin)
  "ORIGIN as a qualifier value: \"%\", \";\", blanks and controls escaped."
  (mapconcat (lambda (c)
               (if (or (memq c '(?% ?\;))
                       (memq (get-char-code-property c 'general-category) '(Zs Zl Zp Cc)))
                   (url-hexify-string (string c))
                 (string c)))
             origin ""))

(defun swh--encode-path (path)
  "PATH as a qualifier value, as the daemon and swh.model encode it."
  (url-hexify-string path (cons ?/ url-unreserved-chars)))

(defun swh--qualify (core origin snapshot anchor rel lines)
  "CORE qualified with ORIGIN when public, SNAPSHOT, ANCHOR, REL and LINES.
As the daemon does; the path REL only with an ANCHOR."
  (let ((public (and origin (swh--public-origin-p origin))))
    (concat core
            (and public (concat ";origin=" (swh--encode-origin origin)))
            (and public snapshot (concat ";visit=" (swh--core "snp" snapshot)))
            (and anchor (concat ";anchor=" anchor))
            (and anchor rel
                 (concat ";path=/" (unless (equal rel ".")
                                     (swh--encode-path rel))))
            (and lines (concat ";lines=" lines)))))

(defconst swh--anchor-candidates-max 100
  "Number of the most recent commits that may anchor a SWHID.")

(defun swh--rev (commit)
  "Core revision SWHID of the hash COMMIT."
  (swh--core "rev" commit))

(defun swh--rev-list (root &rest args)
  "The `swh--anchor-candidates-max' first commits git rev-list ARGS lists at ROOT."
  (split-string (or (apply #'swh--git root "rev-list"
                           (format "--max-count=%d" swh--anchor-candidates-max) args)
                    "")
                "\n" t))

(defun swh--object-at (root commit rel &optional type)
  "Hash of the object at REL (\".\" for the root) in COMMIT at ROOT, or nil.
With TYPE, \"blob\" or \"tree\", only an object of that type."
  (when-let* ((object (if (equal rel ".")
                          (concat commit "^{tree}")
                        (concat commit ":" rel)))
              (hash (swh--git root "rev-parse" "--verify" "-q" object)))
    (and (or (null type) (equal type (swh--git root "cat-file" "-t" hash)))
         hash)))

(defun swh--archived-version (root rel dir-p)
  "(COMMIT . HASH) of REL, a directory when DIR-P, in the latest archived commit.
Among HEAD's last `swh--anchor-candidates-max' at ROOT; nil when none."
  (seq-some (lambda (commit)
              (when-let* ((hash (swh--object-at root commit rel
                                                (if dir-p "tree" "blob"))))
                (cons commit hash)))
            (swh--archived-commits (swh--rev-list root "HEAD"))))

(defun swh--archived-commits (commits)
  "Those of the hashes COMMITS the archive holds, in order."
  (swh--known (mapcar #'swh--rev commits))
  (seq-filter (lambda (c) (swh--known-p (swh--rev c))) commits))

(defun swh--last-commit (root rel)
  "The commit of HEAD's history that last changed REL, at ROOT; nil if none.
Plumbing: `git log' would mix in signatures, see log.showSignature."
  (swh--git root "--literal-pathspecs" "rev-list" "-1" "HEAD" "--" rel))

(defun swh--anchor-candidates (root rel hash)
  "Commits of HEAD's history at ROOT that may hold the object HASH at REL.
Most recent first, back to the one that last changed REL; nil when
HEAD does not hold it."
  (when-let* (((equal hash (swh--object-at root "HEAD" rel)))
              (last (swh--last-commit root rel)))
    (delete-dups (append (swh--rev-list root "HEAD" "--not" (concat last "^@"))
                         (list last)))))

(defun swh--anchor (root rel core)
  "Most recent archived commit at ROOT that holds CORE at REL, or nil."
  (let* ((hash (swh--hash core))
         (commits (swh--anchor-candidates root rel hash)))
    (swh--known (cons core (mapcar #'swh--rev commits)))
    (seq-find (lambda (commit) (equal hash (swh--object-at root commit rel)))
              (swh--archived-commits commits))))

(defun swh--dir-swhid (dir root)
  "Core SWHID of DIR in the tracked working tree of the repository at ROOT.
nil when nothing in DIR is tracked."
  (when-let* ((hash (swh--object-at root (swh--worktree-tree root)
                                    (directory-file-name (file-relative-name dir root)))))
    (swh--core "dir" hash)))

(defun swh--worktree-swhid (file root)
  "Core SWHID of FILE, or of a tracked directory, in the working tree at ROOT."
  (if (file-directory-p file)
      (or (swh--dir-swhid file (or root (user-error "Not inside a Git repository")))
          (user-error "Nothing is tracked in %s" file))
    (swh--content-swhid file root)))

(defun swh--archived-target (root rel dir-p)
  "The version of REL at ROOT the archive holds, a directory when DIR-P.
An alist: core, anchor (the commit holding it), version (that
commit's SWHID) and archived; nil when none, see `swh--archived-version'."
  (when-let* ((version (swh--archived-version root rel dir-p)))
    (let ((core (swh--core (if dir-p "dir" "cnt") (cdr version))))
      ;; the commit of the archived version holds it, hence its content
      (swh--memo-put swh--known-memo core t)
      `((core . ,core)
        (anchor . ,(car version))
        (version . ,(swh--rev (car version)))
        (archived . t)))))

(defun swh--worktree-target (file root rel public)
  "FILE as it is in the working tree at ROOT, at REL there.
An alist: core, anchor and archived.  The archive is asked about it
only with a PUBLIC origin, or outside a repository."
  (let ((core (swh--worktree-swhid file root)))
    `((core . ,core)
      (anchor . ,(and public (swh--anchor root rel core)))
      (archived . ,(and (or public (null root)) (swh--archived-p core))))))

(defun swh--swhid-info (core swhid &optional extra)
  "The SWHID alist of the qualified SWHID, of core CORE, with the EXTRA entries."
  `((swhid . ,swhid) (core . ,core) ,@extra))

(defun swh--direct-swhid (file lines &optional archived-version)
  "The SWHID alist of FILE, or its LINES, from Git and the Web API.
As the daemon's /swhid, of the ARCHIVED-VERSION when asked; see
`swh--qualified-swhid'."
  (let* ((root (swh--repo-root-of file))
         (rel (and root (directory-file-name (file-relative-name file root))))
         (info (and root (swh--direct-origin root)))
         (origin (alist-get 'origin info))
         (public (and origin (swh--public-origin-p origin))))
    (let-alist (or (and archived-version public
                        (swh--archived-target root rel (file-directory-p file)))
                   (swh--worktree-target file root rel public))
      (swh--swhid-info
       .core (swh--qualify .core origin (alist-get 'snapshot info)
                           (and .anchor (swh--rev .anchor)) rel (unless .version lines))
       `((archived . ,.archived)
         ,@(and origin `((origin . ,origin)
                         (origin-archived . ,(alist-get 'archived info))))
         ,@(and .version `((version . ,.version))))))))

(defun swh--unchecked-swhid (file lines why)
  "The SWHID of FILE, or of its LINES, from Git alone: the archive failed WHY."
  (let ((core (swh--worktree-swhid file (swh--repo-root-of file))))
    (swh--swhid-info core (swh--qualify core nil nil nil nil lines)
                     `((archived . nil) (unchecked . ,why)))))

;;; One front, two backends

(defun swh--origin (root &optional upstream)
  "Archival status of ROOT's origin, with UPSTREAM its branch's upstream's."
  (if (swh--daemon-available-p)
      (swh--daemon "GET" "/origin" :path root :params (and upstream '(("upstream"))))
    (let ((info (or (swh--direct-origin root) (user-error "No remote"))))
      (append info (and upstream (swh--direct-upstream root info))))))

(defun swh--origin-async (root callback failed)
  "Like `swh--origin' on ROOT: CALLBACK gets it, nil without a remote.
FAILED is as in `swh--showing-errors'."
  (cond ((not (swh--remote-url root)) (funcall callback nil))
        ((swh--daemon-available-p)
         (swh--daemon "GET" "/origin" :path root :callback callback :failed failed))
        (t (swh--direct-origin-async root callback failed))))

(defun swh--save-async (root callback)
  "Ask to save the repository at ROOT; CALLBACK gets the save request."
  (if (swh--daemon-available-p)
      (swh--daemon "POST" "/save" :path root :callback callback)
    (let ((origin (or (swh--origin-of root) (user-error "No remote"))))
      (unless (swh--public-origin-p origin)
        (user-error "The origin is not a public URL"))
      (swh--direct-origin-async
       root
       (lambda (info) (swh--save-origin-async (alist-get 'origin info) callback))
       nil))))

(defun swh--qualified-swhid (file lines &optional archived-version)
  "Qualified SWHID of FILE, or a directory, or its LINES (\"A-B\", \"A\" or nil).
An alist: swhid, core, archived, origin, origin-archived and, with
ARCHIVED-VERSION, version; see `swh--direct-swhid', `swh--unchecked-swhid'."
  (let* ((root (swh--repo-root-of file))
         (origin (and root (swh--origin-of root))))
    (if (and (swh--daemon-available-p)
             (or (null root) (and origin (swh--public-origin-p origin))))
        (swh--daemon "GET" "/swhid" :path file
                     :params (append (and archived-version '(("archived")))
                                     (and lines `(("lines" ,lines)))))
      (condition-case err
          (swh--direct-swhid file lines archived-version)
        (user-error (swh--unchecked-swhid file lines (error-message-string err)))))))

(defun swh--file-status-async (file callback failed &optional core)
  "CALLBACK gets ((swhid . S) (archived . B)) for FILE.
FAILED is as in `swh--showing-errors'.  CORE, FILE's core SWHID when
already known, spares hashing it."
  (if (swh--daemon-available-p)
      (swh--daemon "GET" "/file" :path file :callback callback :failed failed)
    (swh--try failed
              (lambda ()
                (let ((core (or core (swh--content-swhid file (swh--repo-root-of file)))))
                  (swh--known-async core
                                    (lambda (archived)
                                      (funcall callback
                                               `((swhid . ,core) (archived . ,archived))))
                                    failed))))))

;;; Whole-repository scans

(defun swh--ancestors (path)
  "The directories above the relative PATH, the root \".\" first."
  (unless (equal path ".")
    (let ((acc (list ".")) (dir nil))
      (dolist (part (butlast (split-string path "/")))
        (setq dir (if dir (concat dir "/" part) part))
        (push dir acc))
      (nreverse acc))))

(defun swh--node (path swhid dir-p &optional archived)
  "A file or directory of the tracked working tree, see `swh--direct-tree'.
The node is (PATH SWHID DIR-P ARCHIVED)."
  (list path swhid dir-p archived))

(defsubst swh--node-path (node)
  "The path of NODE relative to the repository root."
  (car node))

(defsubst swh--node-swhid (node)
  "The SWHID of NODE."
  (nth 1 node))

(defsubst swh--node-dir-p (node)
  "Whether NODE is a directory."
  (nth 2 node))

(defsubst swh--node-archived (node)
  "Whether NODE is archived."
  (nth 3 node))

(defun swh--tree-nodes (tree ls-tree)
  "The `swh--node's of TREE, \".\" first, from its LS-TREE -r -t -z."
  (cons (swh--node "." (swh--core "dir" tree) t)
        (mapcar (lambda (line)
                  ;; "<mode> <type> <hash>\t<path>", the path may hold tabs
                  (pcase-let* ((tab (string-search "\t" line))
                               (`(,_mode ,type ,hash)
                                (split-string (substring line 0 tab) " ")))
                    (swh--node (substring line (1+ tab))
                               (swh--core
                                (pcase type ("tree" "dir") ("blob" "cnt") (_ "rev"))
                                hash)
                               (equal type "tree"))))
                (split-string (or ls-tree "") "\0" t))))

(defun swh--with-index-copy (root fn)
  "Call FN with a copy of the index of the repository at ROOT as Git's index."
  (let* ((index (expand-file-name (swh--git-check root "rev-parse" "--git-path" "index")
                                  root))
         (tmp (make-temp-file "swh-index"))
         (process-environment (cons (concat "GIT_INDEX_FILE=" tmp) process-environment)))
    (unwind-protect
        (progn
          ;; git rejects an empty index file: none at all is an empty index;
          ;; the copy keeps the index's date, against which git tells
          ;; whether a file's recorded stat can be trusted
          (if (file-exists-p index) (copy-file index tmp t t) (delete-file tmp))
          (funcall fn))
      (ignore-errors (delete-file tmp)))))

(defun swh--worktree-tree (root)
  "Hash of the tree of the tracked working tree of the repository at ROOT."
  ;; staged in a copy of the index, which writes the blobs and trees as
  ;; `git stash' would; skip-worktree and assume-unchanged files count
  ;; as indexed
  (swh--with-index-copy root (lambda ()
                               (swh--git-check root "add" "-u")
                               (swh--git-check root "write-tree"))))

(defun swh--head-tree (dir root)
  "Hash of the tree of DIR in HEAD, nil when its tracked files differ.
In the working tree or the index at ROOT; a changed date alone does not."
  (swh--with-index-copy
   root (lambda ()
          (swh--git-run root '("update-index" "-q" "--refresh"))
          (and (eq 0 (car (swh--git-run dir '("diff-files" "--quiet" "--" "."))))
               (eq 0 (car (swh--git-run dir '("diff-index" "--cached" "--quiet"
                                              "HEAD" "--" "."))))
               (swh--git dir "rev-parse" "--verify" "-q" "HEAD:./")))))

(defun swh--direct-tree (root)
  "The `swh--node's of the tracked working tree at ROOT, \".\" first.
As the daemon's /tree; the check is Merkle-pruned."
  (let* ((tree (swh--worktree-tree root))
         (nodes (swh--tree-nodes tree (swh--git root "ls-tree" "-r" "-t" "-z" tree)))
         (swhids (make-hash-table :test #'equal))
         ;; a directory holds a submodule's commit, not the commit itself
         (covered (lambda (n)
                    (and (not (equal "rev" (swh--type (swh--node-swhid n))))
                         (seq-some (lambda (dir) (swh--known-p (gethash dir swhids)))
                                   (swh--ancestors (swh--node-path n))))))
         (ask (lambda (dir-p)
                (swh--known
                 (seq-keep (lambda (n)
                             (and (eq (swh--node-dir-p n) dir-p)
                                  (not (funcall covered n))
                                  (swh--node-swhid n)))
                           nodes)))))
    (dolist (n nodes) (puthash (swh--node-path n) (swh--node-swhid n) swhids))
    ;; directories first: an archived one covers everything under it
    (funcall ask t)
    (funcall ask nil)
    (mapcar (lambda (n)
              (swh--node (swh--node-path n) (swh--node-swhid n) (swh--node-dir-p n)
                         (or (swh--known-p (swh--node-swhid n)) (funcall covered n))))
            nodes)))

(defconst swh--tree-timeout 600
  "Seconds to wait for swhd's /tree: a large repository takes many calls.")

(defun swh--tree-async (root callback failed)
  "CALLBACK gets `swh--direct-tree' of ROOT, through the daemon when it runs.
FAILED is as in `swh--showing-errors'."
  (if (swh--daemon-available-p)
      (swh--daemon
       "GET" "/tree" :path root :timeout swh--tree-timeout :failed failed
       :callback
       (lambda (answer)
         (funcall callback
                  (mapcar (lambda (entry)
                            (let-alist (cdr entry)
                              (when-let* ((found (swh--provenance-found .provenance)))
                                (swh--memo-put swh--provenance-memo .swhid found))
                              (swh--node (symbol-name (car entry))
                                         .swhid .dir? .archived)))
                          answer))))
    (swh--try failed (lambda () (funcall callback (swh--direct-tree root))))))

(defvar swh--provenance-refused nil
  "The token the archive refused the provenance API to, `daemon' for its own.")

(defun swh--provenance-token ()
  "Our token, else `daemon' when the daemon has one, else nil."
  (or (swh--token)
      (and (swh--daemon-available-p)
           (eq t (alist-get 'authenticated (swh--daemon-pong)))
           'daemon)))

(defun swh--provenance-available-p ()
  "Whether the provenance API may answer: a token it has not refused."
  (when-let* ((token (swh--provenance-token)))
    (not (equal token swh--provenance-refused))))

(defun swh--provenance-check ()
  "Signal a `user-error' when the provenance API cannot answer."
  (unless (swh--provenance-available-p)
    (user-error (cond ((swh--provenance-token)
                       "The archive refused the provenance API to this token")
                      ((swh--daemon-available-p)
                       "The provenance API needs a token, and neither swh.el \
(SWH_API_TOKEN or auth-source) nor swhd (SWH_API_TOKEN in its \
environment, e.g. ~/.config/swhd/env) has one")
                      (t
                       "The provenance API needs a token (SWH_API_TOKEN or auth-source)")))))

(defun swh--note-provenance-refusal (status)
  "Remember our token as refused the provenance API if the STATUS says so.
STATUS is that of the archive's answer."
  (when (memq status '(401 403))
    (setq swh--provenance-refused (swh--provenance-token))))

(defun swh--provenance-body (resp)
  "The body of RESP, the archive's provenance answer; note a refusal."
  (swh--note-provenance-refusal (swh--resp-status resp))
  (swh--api-body resp "the provenance lookup"))

(defun swh--provenance (swhid)
  "Where the archive found the core SWHID: a qualified SWHID, nil when unknown.
Remembered in `swh--provenance-memo'."
  (pcase (swh--memo-get swh--provenance-memo swhid)
    ('unknown
     (swh--provenance-check)
     (swh--memo-put swh--provenance-memo swhid (swh--provenance-ask swhid)))
    (found found)))

(defun swh--provenance-ask (swhid)
  "Ask the archive where it found the core SWHID: a qualified SWHID, or nil."
  (swh--provenance-found
   (if (swh--daemon-available-p)
       (alist-get 'provenance
                  (swh--daemon "GET" "/provenance" :params `(("swhid" ,swhid))))
     (swh--provenance-body (swh--api "GET" (format "/provenance/whereis/%s/" swhid))))))

(defun swh--provenances-async (swhids callback failed)
  "Ask where the archive found SWHIDS into `swh--provenance-memo', in one request.
Then CALLBACK gets nil; FAILED is as in `swh--showing-errors'."
  (let ((body (json-serialize (vconcat swhids)))
        (remember (lambda (found)
                    (pcase-dolist (`(,swhid . ,provenance) found)
                      (swh--memo-put swh--provenance-memo swhid
                                     (swh--provenance-found provenance)))
                    (funcall callback nil))))
    (if (swh--daemon-available-p)
        (swh--daemon "POST" "/provenance" :body body :failed failed
                     :callback (lambda (answer)
                                 (funcall remember
                                          (mapcar (lambda (e)
                                                    (cons (symbol-name (car e)) (cdr e)))
                                                  answer))))
      (swh--api-then "POST" "/provenance/whereare/" body
                     (lambda (resp)
                       (funcall remember
                                (seq-mapn #'cons swhids (swh--provenance-body resp))))
                     failed))))

(defun swh--qualifiers (swhid)
  "The qualifiers of SWHID, an alist of symbols to decoded strings."
  (mapcar (lambda (q)
            (let ((pos (or (string-search "=" q) (length q))))
              (cons (intern (substring q 0 pos))
                    (decode-coding-string
                     (url-unhex-string (substring q (min (1+ pos) (length q)))) 'utf-8))))
          (cdr (split-string swhid ";"))))

(defun swh--provenance-found (answer)
  "ANSWER, a qualified SWHID of the provenance API, when it names an origin.
A bare SWHID means that the provenance index does not cover it yet."
  (and answer (alist-get 'origin (swh--qualifiers answer)) answer))

(defun swh--provenance-text (found &optional link)
  "FOUND, from `swh--provenance', in words, the anchor's hash shortened.
With LINK, the release or revision named is a button to the archive."
  (if found
      (let-alist (swh--qualifiers found)
        (concat .origin
                (and .anchor (concat " at " (if link
                                                (swh--swhid-link .anchor)
                                              (swh--short-swhid .anchor))))))
    "no known provenance"))

(defun swh--scan-annotation (found)
  "The text `swh-scan' puts after a line whose provenance is FOUND."
  (let* ((text (concat "  ← " (swh--provenance-text found t)))
         (link (or (text-property-not-all 0 (length text) 'button nil text)
                   (length text))))
    (add-face-text-property 0 link 'shadow nil text)
    (propertize text 'swh-provenance t)))

(defconst swh--scan-marks
  '((archived "✓" success)
    (partial "~" warning)
    (not-archived "✗" error))
  "Status -> (MARK FACE) in the `swh-scan' report.")

(defun swh--scan-counts (nodes)
  "Directory path -> (FILES . ARCHIVED), the files of NODES under it."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (n (seq-remove #'swh--node-dir-p nodes))
      (dolist (dir (swh--ancestors (swh--node-path n)))
        (let ((c (swh--count counts dir)))
          (puthash dir (cons (1+ (car c)) (+ (cdr c) (if (swh--node-archived n) 1 0)))
                   counts))))
    counts))

(defun swh--count (counts dir)
  "(FILES . ARCHIVED) under DIR, see `swh--scan-counts' and its COUNTS."
  (gethash dir counts '(0 . 0)))

(defun swh--scan-status (node counts)
  "Status of NODE, a key of `swh--scan-marks'; COUNTS as `swh--scan-counts'."
  (cond ((swh--node-archived node) 'archived)
        ((and (swh--node-dir-p node)
              (< 0 (cdr (swh--count counts (swh--node-path node)))))
         'partial)
        (t 'not-archived)))

(defun swh--percent (k n)
  "K out of N, in percent."
  (if (zerop n) 0 (/ (* 100 k) n)))

(defun swh--scan-text (root tree via)
  "The scan report of ROOT, from TREE, made VIA a backend: (TEXT . SUMMARY)."
  (let* ((nodes (sort tree :key (lambda (n)
                                  (let ((path (swh--node-path n)))
                                    (unless (equal path ".") (split-string path "/"))))))
         (counts (swh--scan-counts nodes))
         (files (swh--count counts ".")))
    (cons (concat (swh--scan-header root nodes counts via)
                  (mapconcat (lambda (n)
                               (swh--scan-line n (swh--scan-status n counts) counts))
                             nodes "\n")
                  "\n")
          (format "%d of %d files archived" (cdr files) (car files)))))

(defun swh--scan-insert (root tree via)
  "Insert the scan report of ROOT, from TREE, made VIA a backend; its summary.
Archived directories are folded, see `swh--scan-text'."
  (pcase-let ((`(,text . ,summary) (swh--scan-text root tree via))
              (inhibit-read-only t))
    (erase-buffer)
    (insert text)
    (goto-char (point-min))
    (while (text-property-search-forward 'swh-fold t)
      (unless (invisible-p (point)) (outline-hide-subtree)))
    (goto-char (point-min))
    summary))

(defun swh--scan-header (root nodes counts via)
  "The head of the scan report of ROOT, its NODES and COUNTS, made VIA a backend."
  (let* ((files (swh--count counts "."))
         (dirs (seq-filter #'swh--node-dir-p nodes))
         (full (seq-count #'swh--node-archived dirs))
         (partial (seq-count (lambda (n) (eq 'partial (swh--scan-status n counts)))
                             dirs)))
    (concat
     (format "Software Heritage archival status of %s (tracked working tree, %s)\n"
             root via)
     (format "Files: %d, archived %d (%d%%)   Directories: %d, archived %d (%d%%), partially %d (%d%%)\n"
             (car files) (cdr files) (swh--percent (cdr files) (car files))
             (length dirs) full (swh--percent full (length dirs))
             partial (swh--percent partial (length dirs)))
     (substitute-command-keys
      ;; not starting with a mark: it would be a heading
      (concat "Marks: ✓ archived  ~ partially  ✗ not   "
              "\\<swh-scan-mode-map>\\[outline-cycle] folds"
              (and (swh--provenance-available-p)
                   (concat "   \\[swh-provenance] provenance of the line, "
                           "\\[swh-scan-provenance-visible] of the visible lines"))
              "\n\n")))))

(defun swh--scan-line (node status counts)
  "The line of the scan report for NODE, of STATUS; COUNTS as `swh--scan-counts'.
It carries the SWHID as the `swh-swhid' property, and `swh-fold' when
it is an archived directory, which `swh--scan-insert' folds."
  (pcase-let* ((`(,path ,swhid ,dir-p . ,_) node)
               (`(,mark ,face) (alist-get status swh--scan-marks))
               (name (if (equal path ".") "./"
                       (concat (file-name-nondirectory path) (and dir-p "/"))))
               (found (swh--memo-get swh--provenance-memo swhid)))
    (propertize
     (concat (make-string (* 2 (length (swh--ancestors path))) ?\s)
             (propertize mark 'face face) " "
             ;; only what the archive holds can be browsed there
             (if (eq status 'archived) (swh--link name swhid) name)
             (and dir-p (let ((c (swh--count counts path)))
                          (format "  %d/%d" (cdr c) (car c))))
             (unless (eq found 'unknown) (swh--scan-annotation found)))
     'swh-swhid swhid 'swh-fold (and dir-p (eq status 'archived)))))

(defvar-keymap swh-scan-mode-map
  :doc "Keymap of `swh-scan-mode'."
  :parent special-mode-map
  "TAB" #'outline-cycle
  "<backtab>" #'outline-cycle-buffer
  "w" #'swh-provenance
  "W" #'swh-scan-provenance-visible
  "n" #'outline-next-visible-heading
  "p" #'outline-previous-visible-heading)

(define-derived-mode swh-scan-mode special-mode "SWH-Scan"
  "Major mode of the `swh-scan' report, a tree folded with Outline.
\\<swh-scan-mode-map>\\[swh-provenance] tells where the archive found the artifact of a line,
\\[swh-scan-provenance-visible] those of the visible archived lines.
\\{swh-scan-mode-map}"
  (setq-local outline-regexp " *[✓~✗] ")
  (setq-local outline-level (lambda () (1+ (/ (current-indentation) 2))))
  (outline-minor-mode 1))

;;;###autoload
(defun swh-scan ()
  "Show which parts of the current repository the archive holds.
The tracked working tree, as a folded tree: each file and directory
archived, partially or not.  \\<swh-scan-mode-map>\\[revert-buffer] scans again."
  (interactive)
  (let ((root (swh--repo-root)))
    (with-current-buffer (get-buffer-create "*swh scan*")
      (swh-scan-mode)
      (setq default-directory root)
      (setq-local revert-buffer-function
                  (lambda (&rest _) (swh--scan-start root)))
      (swh--scan-start root)
      (pop-to-buffer (current-buffer)))))

(defvar-local swh--scan-latest nil
  "The buffer's latest scan, an object of its own: late answers are ignored.")

(defun swh--scan-callback (fn)
  "FN as a callback of the current scan, run in its buffer while it is current."
  (let ((buffer (current-buffer))
        (scan swh--scan-latest))
    (lambda (arg)
      (when (and (buffer-live-p buffer)
                 (eq scan (buffer-local-value 'swh--scan-latest buffer)))
        (with-current-buffer buffer (funcall fn arg))))))

(defun swh--scan-start (root)
  "Scan the repository at ROOT into the current buffer, see `swh--tree-async'."
  (setq swh--scan-latest (list root))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "Scanning %s…\n" root)))
  ;; now: a scan may outlast what `swh--daemon-pong' remembers
  (let ((via (if (swh--daemon-available-p) "via swhd" "via the archive's API")))
    (swh--tree-async
     root
     (swh--scan-callback (lambda (tree) (message "%s" (swh--scan-insert root tree via))))
     (swh--scan-callback (lambda (why)
                           (let ((inhibit-read-only t))
                             (erase-buffer)
                             (insert "Scan failed: " why "\n")))))))

(defun swh--scan-annotate (found)
  "Put FOUND, from `swh--provenance', after the current line, if not yet."
  (unless (text-property-any (pos-bol) (pos-eol) 'swh-provenance t)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (pos-eol))
        (insert (swh--scan-annotation found))))))

(defconst swh--provenance-batch-size 100
  "SWHIDs per provenance request: the archive answers these in 2 s or so.")

(defconst swh--provenance-confirm-threshold 100
  "Provenance requests beyond which `swh-scan-provenance-visible' asks first.")

(defun swh--scan-pending ()
  "Annotate the visible archived lines of the report whose provenance is known.
Return the SWHIDs of the others, not annotated yet, to ask about."
  (let ((archived (concat " *" (car (alist-get 'archived swh--scan-marks)) " "))
        (pending nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((swhid (get-text-property (point) 'swh-swhid))
                    ((not (invisible-p (point))))
                    ((looking-at archived))
                    ((not (text-property-any (point) (pos-eol) 'swh-provenance t))))
          (let ((found (swh--memo-get swh--provenance-memo swhid)))
            (if (eq found 'unknown)
                (push swhid pending)
              (swh--scan-annotate found))))
        (forward-line 1)))
    (seq-uniq (nreverse pending))))

(defun swh--scan-provenance-step (batches done total)
  "Ask the provenance of BATCHES, one request each, DONE of TOTAL being sent.
The visible lines are annotated as the answers come, see `swh--scan-pending'."
  (swh--scan-pending)
  (if (null batches)
      (message "Provenance of the visible lines (%d request%s)"
               total (if (= total 1) "" "s"))
    (message "Provenance: %d/%d requests" done total)
    (swh--provenances-async
     (car batches)
     (swh--scan-callback
      (lambda (_) (swh--scan-provenance-step (cdr batches) (1+ done) total)))
     (swh--scan-callback
      (lambda (why)
        (message "%s (%d of %d requests done; %s resumes)" why done total
                 (substitute-command-keys "\\[swh-scan-provenance-visible]")))))))

(defun swh-scan-provenance-visible ()
  "Show where the archive found the artifacts of the visible archived lines.
As \\[swh-provenance] on each of them; a folded directory stands for
what it holds.  Beyond `swh--provenance-confirm-threshold' requests,
ask first; after a failure, calling the command again resumes."
  (interactive nil swh-scan-mode)
  (let* ((unknown (swh--scan-pending))
         (batches (seq-partition unknown swh--provenance-batch-size)))
    (when unknown (swh--provenance-check))
    (when (and (> (length batches) swh--provenance-confirm-threshold)
               (not (y-or-n-p (format "Send %d provenance requests? " (length batches)))))
      (user-error "No provenance requests sent"))
    (swh--scan-provenance-step batches 0 (length batches))))

;;;###autoload
(defun swh-provenance ()
  "Show where the archive found the artifact at point, or the current file.
In the `swh-scan' report, the line's file or directory; elsewhere,
what `swh-swhid' would take.  An origin and the release or revision
holding it there, whose link is copied.  The API needs a special token."
  (interactive)
  (let* ((scan (derived-mode-p 'swh-scan-mode))
         (swhid (if scan
                    (or (get-text-property (pos-bol) 'swh-swhid)
                        (user-error "No file or directory on this line"))
                  (let ((path (if (buffer-file-name (buffer-base-buffer))
                                  (swh--file-name)
                                (swh--dir-name nil t))))
                    (swh--worktree-swhid path (swh--repo-root-of path)))))
         (found (swh--provenance swhid)))
    (when scan (swh--scan-annotate found))
    (when found (kill-new (swh--browse-url found)))
    (message "%s%s" (swh--provenance-text found) (if found " (link copied)" ""))
    found))

;;; Commands

(defun swh--local (path)
  "PATH, which must be local: a `user-error' for a remote one."
  (when (file-remote-p path)
    (user-error "Remote files are not supported"))
  path)

(defun swh--repo-root ()
  "Root of the Git repository of the current buffer, or a `user-error'."
  (or (swh--repo-root-of (swh--local default-directory))
      (user-error "Not inside a Git repository")))

(defun swh--format-date (date)
  "The ISO 8601 DATE in local time, as is when it does not parse."
  (condition-case nil
      (format-time-string "%Y-%m-%d %H:%M" (encode-time (iso8601-parse date)))
    (error date)))

(defun swh--show (name mode text)
  "Show TEXT in the buffer NAME, in the major MODE, and return the buffer."
  (with-current-buffer (get-buffer-create name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text))
    (funcall mode)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (current-buffer)))

(defun swh--visits-text (origin visits)
  "The report listing VISITS, the archival visits of ORIGIN."
  (concat (format "Last %d archival visits of %s\n\n" (length visits) origin)
          (mapconcat (lambda (visit)
                       (let-alist visit
                         (concat (format "%s  %-8s " (swh--format-date .date) .status)
                                 (and .snapshot
                                      (swh--swhid-link (swh--core "snp" .snapshot)))
                                 "\n")))
                     visits)))

(defun swh--origin-of (root)
  "Origin of the repository at ROOT as the archive names it, nil without a remote."
  (when-let* ((remote (swh--remote-url root)))
    (swh--normalize-origin remote)))

(defun swh--local-origin ()
  "Public origin of the current directory's repository, nil without one.
A remote directory has none: swh.el does not look into it."
  (when-let* ((root (swh--repo-root-of default-directory))
              (origin (swh--origin-of root)))
    (and (swh--public-origin-p origin) origin)))

(defun swh--read-origin (prompt default)
  "Ask with PROMPT for the URL of a public repository, DEFAULT when empty."
  (let ((url (string-trim (read-string (format-prompt prompt default)
                                       nil nil default))))
    (when (string-empty-p url)
      (user-error "No origin"))
    ;; a typo, or a web page rather than a repository
    (when (string-match-p "[[:space:]?#]" url)
      (user-error "Not a repository URL: %s" url))
    (let ((origin (swh--normalize-origin
                   (if (string-match-p "\\`[[:alnum:]-]+\\(?:\\.[[:alnum:]-]+\\)+/" url)
                       (concat "https://" url)
                     url))))
      (unless (swh--public-origin-p origin)
        (user-error "Not a public repository URL: %s" url))
      origin)))

(defun swh--origin-arg (verb)
  "The ORIGIN argument of a command that can VERB another origin, or nil.
Asked for with a prefix argument, or without a public origin here."
  (let ((local (swh--local-origin)))
    (and (or current-prefix-arg (not local))
         (swh--read-origin
          (format (if local "Origin to %s" "No public origin here; origin to %s")
                  verb)
          local))))

(defun swh--status-text (info)
  "The archival status INFO in words: (TEXT . UNSAVED).
UNSAVED when saving would help: the origin is not archived, or its
last commit pushed is not yet."
  (let-alist info
    (let ((date (if .date (swh--format-date .date) "unknown")))
      (cond ((not .archived) (cons (format "%s is not archived" .origin) t))
            ((and .upstream (not .upstream-archived))
             (cons (format "%s is archived (last visit %s), upstream %s not yet"
                           .origin date (swh--short-hash .upstream))
                   t))
            (t (cons (format "%s is archived%s (last visit %s)"
                             .origin (if .upstream ", upstream included" "") date)
                     nil))))))

(defun swh--report-status (info &optional asked)
  "Report the archival status INFO, of an origin ASKED for or of the repository.
Offer a \"save code now\" when it would help, see `swh--status-text'."
  (pcase-let ((`(,text . ,unsaved) (swh--status-text info)))
    (if (and unsaved (y-or-n-p (concat text "; ask SWH to save it now? ")))
        (swh-save-code-now (and asked (alist-get 'origin info)))
      (message "%s" text))))

;;;###autoload
(defun swh-status (&optional origin)
  "Tell whether the current repository is archived by Software Heritage.
And when, and whether the branch's upstream commit is; else offer to
save it.  With a prefix argument, or without a public origin, ask for
the URL of a public repository, the ORIGIN, and tell about it instead."
  (interactive (list (swh--origin-arg "check")))
  (swh--report-status (if origin
                          (swh--origin-status origin)
                        (swh--origin (swh--repo-root) t))
                      origin))

;;;###autoload
(defun swh-visits (&optional origin)
  "List the last ten archival visits of the current repository's origin.
With a prefix argument, or where there is no public origin, ask for
the URL of a public repository, the ORIGIN, and list its visits instead."
  (interactive (list (swh--origin-arg "list the visits of")))
  (let-alist (cond (origin (swh--origin-visits (swh--origin-status origin)))
                   ((swh--daemon-available-p)
                    (swh--daemon "GET" "/visits" :path (swh--repo-root)))
                   (t (swh--origin-visits (swh--origin (swh--repo-root)))))
    (if (seq-empty-p .visits)
        (message "%s is not archived" .origin)
      (pop-to-buffer (swh--show "*swh visits*" #'special-mode
                                (swh--visits-text .origin .visits))))))

;;;###autoload
(defun swh-save-code-now (&optional origin)
  "Ask Software Heritage to archive the current repository now.
A message tells the answer, see `swh-add-forge-now' when it is refused.
With a prefix argument, or without a public origin, ask for the URL
of a public repository, the ORIGIN, and archive it instead."
  (interactive (list (swh--origin-arg "save")))
  (message "Sending the save request for %s…" (or origin "this repository"))
  (if origin
      (swh--save-origin-async origin #'swh--report-save)
    (swh--save-async (swh--repo-root) #'swh--report-save)))

(defun swh--report-save (request)
  "Tell the answer to REQUEST, a save request, without a prompt."
  (let ((origin (alist-get 'origin request))
        (status (alist-get 'save_request_status request)))
    (pcase status
      ("accepted"
       (message "Save request accepted for %s" origin))
      ((or "pending" "rejected")
       (message "Save request %s: the forge of %s may be unknown to SWH (%s)"
                status origin (substitute-command-keys "\\[swh-add-forge-now]")))
      (_
       (message "Save request for %s: %s" origin status)))))

;;;###autoload
(defun swh-add-forge-now ()
  "Open the form asking Software Heritage to add a forge, see `swh-archive-url'."
  (interactive)
  (browse-url (concat (swh--base-url swh-archive-url) "/add-forge/request/create/")))

;;; The SWHID of the file at hand

(defun swh--file-name ()
  "True name of the file the current buffer visits, offering to save it first.
A `user-error' without one, on a remote file, or when it changed on disk."
  (let* ((base (or (buffer-base-buffer) (current-buffer)))
         (file (swh--local (or (buffer-file-name base)
                               (user-error "This buffer is not visiting a file")))))
    (cond ((buffer-modified-p base)
           (if (y-or-n-p "The buffer is modified; save it first? ")
               (with-current-buffer base (save-buffer))
             (user-error "A SWHID identifies the file on disk: save the buffer first")))
          ;; its lines would not be the file's
          ((not (verify-visited-file-modtime base))
           (user-error "The file changed on disk: revert the buffer first")))
    ;; a symbolic link stands for its target: Git holds the link's
    ;; own text at the link's path
    (file-truename file)))

(defun swh--region-lines ()
  "Line range of the active region in a file buffer, as \"A-B\" or \"A\", or nil."
  (when (and (use-region-p) (buffer-file-name (buffer-base-buffer)))
    ;; absolute: the file's line numbers, even in a narrowed buffer
    (let ((from (line-number-at-pos (region-beginning) t))
          (to (line-number-at-pos (max (region-beginning)
                                       (1- (region-end)))
                                  t)))
      (if (= from to)
          (number-to-string from)
        (format "%d-%d" from to)))))

(defun swh--dir-name (&optional noerror at-point)
  "True name of the current directory, or with AT-POINT of the Dired target.
That is the file or directory at point, the current directory when on
none, or on . or ...  A `user-error' outside Git, nil with NOERROR."
  (let* ((dir (swh--local default-directory))
         (file (and at-point (derived-mode-p 'dired-mode)
                    (dired-get-filename nil t)))
         ;; the target's repository, which may not be the link's
         (path (file-truename
                (directory-file-name
                 (if (and file (not (member (file-name-nondirectory file) '("." ".."))))
                     file
                   (expand-file-name dir))))))
    (cond ((swh--repo-root-of path) path)
          ((not noerror) (user-error "Not inside a Git repository")))))

(defun swh--unarchived-why (path)
  "Why the archive cannot hold PATH as it is, in words, or nil."
  (when-let* ((root (swh--repo-root-of path)))
    (let ((rel (file-relative-name path root))
          (origin (swh--origin-of root)))
      (cond ((not origin) "the repository has no remote")
            ((not (swh--public-origin-p origin))
             (format "the origin %s is not a public URL" origin))
            ((not (swh--git root "--literal-pathspecs" "ls-files" "--" rel))
             "Git does not track it")
            ((not (eq 0 (car (swh--git-run root (list "--literal-pathspecs" "diff"
                                                      "--quiet" "HEAD" "--" rel)))))
             "it has uncommitted changes")
            ;; not HEAD: a later commit elsewhere in the tree does not
            ;; keep this content from being archived
            ((not (when-let* ((last (swh--last-commit root rel)))
                    (swh--git root "for-each-ref" "--count=1" "--contains" last
                              "refs/remotes/")))
             "the commit holding it is not pushed")))))

(defconst swh--swhid-forms
  '((?s "swhid" "qualified SWHID")
    (?b "bare" "SWHID w/o context")
    (?l "link" "SWH archive permalink")
    (?L "bare link" "permalink w/o context")
    (?o "open" "open in SWH archive")
    (?c "bibtex" "BibTeX entry")
    (?a "archived" "SWHID of SWH-archived version"))
  "The forms `swh-swhid' offers, for `read-multiple-choice'.")

;;;###autoload
(defun swh-swhid (&optional form path)
  "Copy the qualified SWHID of the current file, or of the region's lines.
In Dired, of the file or directory at point; outside a file buffer,
of the current directory.  PATH, when given, is the file or directory.
With a prefix argument, ask for another FORM, a character:
  ?s  the qualified SWHID;
  ?b  the bare SWHID;
  ?l  the archive's permalink, ?L with the bare SWHID;
  ?o  open it in the archive's web interface;
  ?c  a BibTeX entry citing it, see `swh-cite';
  ?a  the SWHID of the version the archive holds, see `swh--archived-target'.
The message tells when the archive does not hold it, and why.  Return
the text."
  (interactive (list (and current-prefix-arg
                          (car (read-multiple-choice "SWHID" swh--swhid-forms)))))
  (let* ((form (or form ?s))
         (archived-version (eq form ?a))
         (path (cond (path)
                     ((buffer-file-name (buffer-base-buffer)) (swh--file-name))
                     (t (swh--dir-name nil t))))
         ;; once saved: a save hook may have moved them
         (lines (swh--region-lines))
         (info (swh--qualified-swhid path lines archived-version))
         (version (alist-get 'version info))
         ;; the region's lines are those of the working tree's version
         (lines (unless version lines)))
    (pcase-let ((`(,text . ,what) (swh--swhid-form-text info lines form path)))
      (let-alist info
        (if (and .archived (eq form ?o))
            (progn (browse-url (swh--browse-url .swhid))
                   (message "Opening in the SWH archive"))
          (kill-new text)
          (cond (.unchecked
                 (message "Copied %s (not checked against the archive: %s)"
                          what .unchecked))
                ((not .archived) (swh--swhid-unarchived what path .origin))
                (t (message "Copied %s%s" what
                            (if version
                                (format " (version of commit %s)"
                                        (swh--short-hash version))
                              ""))))))
      text)))

(defun swh--swhid-form-text (info lines form path)
  "(TEXT . WHAT): FORM, see `swh-swhid', for the SWHID INFO of PATH or LINES."
  (if (eq form ?c)
      (pcase-let ((`(,entry . ,why) (swh--bibtex info lines (swh--repo-root-of path))))
        ;; say what it is rather than show it: it spans lines
        (cons entry (swh--citation-what why)))
    (cons (let-alist info
            (let ((bare (concat .core (and lines (concat ";lines=" lines)))))
              (pcase form
                ((or ?s ?o ?a) .swhid)
                (?b bare)
                (?l (swh--browse-url .swhid))
                (?L (swh--browse-url bare)))))
          ;; not archived, ?o opens nothing and ?a finds no version
          (nth 2 (assq (if (and (memq form '(?o ?a)) (not (alist-get 'archived info)))
                           ?s
                         form)
                       swh--swhid-forms)))))

(defun swh--swhid-unarchived (what path origin)
  "Say why the archive does not hold PATH, WHAT of which was just copied.
Or offer to save ORIGIN, when public."
  (if-let* ((why (swh--unarchived-why path)))
      (message "Copied %s (not archived: %s)" what why)
    (if (and origin
             (swh--public-origin-p origin)
             (y-or-n-p (format "Not archived since the last push (copied); \
ask SWH to save %s now? " origin)))
        ;; through a symbolic link, the file's repository may not
        ;; be the current directory's: then that origin, by name
        (swh-save-code-now (unless (equal (swh--normalize-origin origin)
                                          (swh--local-origin))
                             origin))
      (message "Copied %s (not archived yet)" what))))

;;; Citations

(defun swh--archive-citation (target &optional origin-p)
  "The archive's BibTeX entry citing the SWHID, or with ORIGIN-P origin, TARGET.
`no-metadata' when it has no citation metadata, `failed' on other errors."
  (pcase-let ((`(,status ,body ,_)
               (swh--api "GET" (concat "/raw-intrinsic-metadata/citation/"
                                       (if origin-p "origin" "swhid") "/?"
                                       (url-build-query-string
                                        `(("citation_format" "bibtex")
                                          (,(if origin-p "origin_url" "target_swhid")
                                           ,target)))))))
    (cond ((and (<= 200 status 299) (listp body) (alist-get 'content body)))
          ((memq status '(400 404)) 'no-metadata)
          (t 'failed))))

(defun swh--origin-name (origin)
  "The repository's name in the URL ORIGIN, e.g. swh.el."
  (string-remove-suffix ".git" (file-name-nondirectory (directory-file-name origin))))

(defun swh--citation (answer core swhid title url date lines)
  "(ENTRY . WHY) citing the qualified SWHID, of core CORE and LINES.
ANSWER is that of `swh--archive-citation', or `not-archived'.  When not
an entry, a minimal one from TITLE, URL and DATE, each optional."
  (if (stringp answer)
      (cons answer 'archive)
    (let* ((date (and date (string-limit date 10)))
           (fields `((title . ,title) (url . ,url) (date . ,date)
                     (year . ,(and date (string-limit date 4))) (swhid . ,swhid))))
      (cons (format "@%s{%s,\n%s\n}\n"
                    ;; the types the archive gives
                    (pcase (swh--type core)
                      ("cnt" "codefragment")
                      ("snp" "software")
                      (_ "softwareversion"))
                    ;; as the archive makes it, e.g. swh-cnt-67c1849-L1-L3
                    (concat "swh-" (swh--type core) "-" (swh--short-hash core)
                            (and lines (concat "-L" (string-replace "-" "-L" lines))))
                    (mapconcat (lambda (field)
                                 (format "    %s = {%s}" (car field) (cdr field)))
                               (seq-filter #'cdr fields) ",\n"))
            answer))))

(defun swh--bibtex (info lines root)
  "BibTeX entry citing the SWHID INFO of LINES in ROOT, see `swh--citation'."
  (let* ((swhid (alist-get 'swhid info))
         (answer (if (alist-get 'archived info)
                     (swh--archive-citation swhid)
                   'not-archived))
         (origin (alist-get 'origin info))
         (anchor (alist-get 'anchor (swh--qualifiers swhid))))
    (swh--citation answer (alist-get 'core info) swhid
                   (cond (origin (swh--origin-name origin))
                         (root (file-name-nondirectory (directory-file-name root))))
                   (and origin (swh--public-origin-p origin) origin)
                   (and anchor root (not (stringp answer))
                        (swh--git root "show" "-s" "--no-show-signature" "--format=%cs"
                                  (swh--hash anchor)))
                   lines)))

(defun swh--citation-what (why)
  "The BibTeX entry `swh--citation' gives for WHY, in words."
  (pcase why
    ('archive "the archive's BibTeX entry")
    ('no-metadata "a minimal BibTeX entry (no citation metadata in the archive)")
    ('failed "a minimal BibTeX entry (the archive failed to make one)")
    (_ "a minimal BibTeX entry")))

(defun swh--origin-bibtex (origin)
  "BibTeX entry citing the latest snapshot of ORIGIN, see `swh--citation'."
  (let* ((info (swh--origin-status origin))
         (url (if (alist-get 'archived info)
                  (alist-get 'origin info)
                (user-error "Software Heritage has not archived %s" origin)))
         (core (swh--core "snp" (alist-get 'snapshot info))))
    (swh--citation (swh--archive-citation url t)
                   core (concat core ";origin=" (swh--encode-origin url))
                   (swh--origin-name url) url (alist-get 'date info) nil)))

;;;###autoload
(defun swh-cite (&optional origin)
  "Copy a BibTeX entry citing the current file or region, show and return it.
The archive makes it from the repository's codemeta.json or
CITATION.cff, else it is a minimal one.  With a prefix argument, or
without a public origin, ask for the URL of a public repository, the
ORIGIN, and cite its latest snapshot instead."
  (interactive (list (and (or current-prefix-arg
                              (not (buffer-file-name (buffer-base-buffer))))
                          (swh--origin-arg "cite"))))
  (let ((entry (if origin
                   (pcase-let ((`(,text . ,why) (swh--origin-bibtex origin)))
                     (kill-new text)
                     (message "Copied %s for %s" (swh--citation-what why) origin)
                     text)
                 (swh-swhid ?c))))
    (display-buffer (swh--show "*swh cite*" #'bibtex-mode entry))
    entry))

;;; Org links

;;;###autoload
(defun swh-org-follow (path _arg)
  "Open the SWHID \"swh:PATH\" in the archive's web interface."
  (browse-url (swh--browse-url (concat "swh:" path))))

;;;###autoload
(defun swh-org-export (path description backend _info)
  "Export the SWHID \"swh:PATH\" with DESCRIPTION for BACKEND."
  (let* ((swhid (concat "swh:" path))
         (url (swh--browse-url swhid))
         (desc (or description swhid)))
    (cond
     ((org-export-derived-backend-p backend 'html)
      (format "<a href=\"%s\">%s</a>" url desc))
     ((org-export-derived-backend-p backend 'latex)
      ;; as `org-latex-link' does: percent-encoded qualifiers are full
      ;; of "%", which starts a comment in LaTeX
      (let ((url (replace-regexp-in-string "[\\{}$%&_#~^]" "\\\\\\&" url)))
        (if description
            (format "\\href{%s}{%s}" url description)
          (format "\\url{%s}" url))))
     ((org-export-derived-backend-p backend 'md)
      (format "[%s](%s)" desc url))
     (description (format "%s (%s)" desc url))
     (t url))))

;;;###autoload
(defun swh-org-store-link (&optional _interactive)
  "Store a swh: link to the current file or region, for `org-store-link'.
In Dired, to the file or directory at point.  Only under Git and when
`swh-org-store-link' says so; a failure is reported, and stores nothing."
  (when-let* ((visited (and swh-org-store-link
                            (or (buffer-file-name (buffer-base-buffer))
                                (derived-mode-p 'dired-mode))))
              ((swh--repo-root-of default-directory)))
    (condition-case err
        (let* ((file (if (stringp visited) (swh--file-name) (swh--dir-name t t)))
               (lines (swh--region-lines))
               (swhid (and file (alist-get 'swhid (swh--qualified-swhid file lines)))))
          (when (and swhid
                     (or (not (eq swh-org-store-link 'ask))
                         (y-or-n-p (format "Store %s as a swh: link (else the usual one)? "
                                           (swh--short-swhid swhid)))))
            (org-link-store-props
             :type "swh"
             :link swhid
             :description (concat (file-name-nondirectory file)
                                  (and lines (format ", lines %s" lines))))
            t))
      (error (message "swh: %s" (error-message-string err)) nil))))

;;;###autoload
(with-eval-after-load 'ol
  (org-link-set-parameters
   "swh"
   :follow #'swh-org-follow
   :export #'swh-org-export
   ;; from the autoloads, before swh.el is loaded: `org-store-link'
   ;; asks in every buffer, and loading it is only worth it once the
   ;; user has asked for swh: links
   :store (lambda (&optional interactive)
            (and (bound-and-true-p swh-org-store-link)
                 (swh-org-store-link interactive)))))

;;; swh-mode: the archival status of the file in the mode line

(defvar-local swh--status nil
  "Archival status of the buffer's file, nil when unknown.
One of archived, not-archived, origin-archived, pending or error.")

(defvar-local swh--swhid nil
  "Core SWHID of the buffer's file, as last checked.")

(defvar-local swh--check 0
  "Number of the buffer's latest status check; late answers are ignored.")

(defvar-local swh--checked nil
  "(TIME . STAMP) of the buffer's latest status check, see `swh--stamp'.")

(defvar-local swh--dired-timer nil
  "The timer of a status check put off, see `swh--dired-refresh'.")

;; `normal-mode' kills the local variables, then turns on
;; `global-swh-mode' twice: before the major mode and after it.  What
;; is kept tells the second time that this very file is already checked.
(dolist (var '(swh--status swh--swhid swh--check swh--checked))
  (put var 'permanent-local t))

(defvar swh-mode)

(defun swh-mode-line-copy (event)
  "Copy the qualified SWHID of the file whose lighter got the mouse EVENT.
In Dired, that of the directory, see `swh-swhid'."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (swh-swhid nil (unless (buffer-file-name (buffer-base-buffer)) (swh--dir-name)))))

(defvar-keymap swh-mode-line-map
  :doc "Keymap of the `swh-mode' lighter."
  "<mode-line> <mouse-1>" #'swh-mode-line-copy)

(defun swh--rate-limit-text ()
  "\" USED/LIMIT\" of the API's tokenless rate limit, nil when not known."
  (pcase swh--rate-limit
    (`(,used ,limit ,reset)
     (and (< (float-time) reset) (format " %d/%d" used limit)))))

(defun swh--mode-line-help-echo (window &rest _)
  "Tooltip of the `swh-mode' lighter shown in WINDOW."
  (with-current-buffer (window-buffer window)
    (concat (or swh--swhid "Software Heritage")
            (when-let* ((used (swh--rate-limit-text)))
              (format "\n%s API requests this hour, no token" (string-trim used)))
            "\nmouse-1: Copy the qualified SWHID")))

(defvar swh-mode-line-format
  ;; the keymap itself: the mode line takes properties as they are
  `(swh--status
    (:propertize ((:eval (alist-get swh--status swh-mode-lighters))
                  (:eval (swh--rate-limit-text)))
                 mouse-face mode-line-highlight
                 help-echo swh--mode-line-help-echo
                 keymap ,swh-mode-line-map))
  "Mode-line construct of `swh-mode': its lighter, when the status is known.")
(put 'swh-mode-line-format 'risky-local-variable t)

(defun swh--mode-path ()
  "The file, or in Dired the directory, whose status `swh-mode' shows, or nil.
nil for a modified, unsaved or remote file, or a directory outside Git."
  (let ((path (cond (buffer-file-name
                     (and (not (buffer-modified-p))
                          ;; not saved yet: nothing to tell
                          (file-exists-p buffer-file-name)
                          buffer-file-name))
                    ((derived-mode-p 'dired-mode)
                     (directory-file-name (expand-file-name default-directory))))))
    (and path
         (not (file-remote-p path))
         (or buffer-file-name (swh--repo-root-of (file-truename path)))
         path)))

(defun swh--untracked-p (dir)
  "Non-nil when DIR holds untracked files that Git does not ignore."
  (swh--git dir "ls-files" "--others" "--exclude-standard"
            "--directory" "--no-empty-directory" "-z" "--" "."))

(defun swh--dir-status-async (dir root set-status failed)
  "SET-STATUS gets the status of DIR in ROOT, and its tree's SWHID if asked.
Archived when the origin and the unchanged tree are, origin-archived
when only the origin is.  FAILED is as in `swh--showing-errors'."
  (swh--origin-async
   root
   (lambda (info)
     (if-let* ((tree (and (alist-get 'archived info)
                          (not (swh--untracked-p dir))
                          (swh--head-tree dir root))))
         ;; the daemon tells it from its cache, else the archive
         (swh--file-status-async
          dir (lambda (result)
                (funcall set-status
                         (if (alist-get 'archived result) 'archived 'origin-archived)
                         (alist-get 'swhid result)))
          failed (swh--core "dir" tree))
       (funcall set-status
                (if (alist-get 'archived info) 'origin-archived 'not-archived))))
   failed))

(defun swh--file-mode-status-async (file root set-status failed)
  "SET-STATUS gets the status of FILE in ROOT (or nil), and its SWHID.
Origin-archived when FILE is not archived but ROOT's origin is.  FAILED
is as in `swh--showing-errors'."
  (if (and root (let ((origin (swh--origin-of root)))
                  (not (and origin (swh--public-origin-p origin)))))
      ;; as `swh--qualified-swhid' has it: the archive is not asked
      (funcall set-status 'not-archived (ignore-errors (swh--content-swhid file root)))
    (swh--file-status-async
     file
     (lambda (result)
       (let-alist result
         (funcall set-status (if .archived 'archived 'not-archived) .swhid)
         (when (and root (not .archived))
           (swh--origin-async root
                              (lambda (info)
                                (when (alist-get 'archived info)
                                  (funcall set-status 'origin-archived)))
                              #'ignore))))
     failed)))

(defun swh--stamp (path)
  "What tells that PATH changed: PATH and its modification time."
  (cons path (file-attribute-modification-time (file-attributes path))))

(defconst swh--dired-check-interval 5
  "Seconds between two status checks of a Dired buffer.
Auto-revert may read it in over and over.")

(defun swh--dired-refresh ()
  "Check the status of the Dired buffer, see `swh--dired-check-interval'.
A check too soon after the last one is put off to the end of the interval."
  (let ((wait (- (+ (or (car swh--checked) 0) swh--dired-check-interval) (float-time))))
    (cond ((<= wait 0) (swh--refresh))
          ((not swh--dired-timer)
           (setq swh--dired-timer
                 (run-at-time wait nil
                              (lambda (buffer)
                                (when (buffer-live-p buffer)
                                  (with-current-buffer buffer
                                    (setq swh--dired-timer nil)
                                    (swh--refresh))))
                              (current-buffer)))))))

(defun swh--status-setter ()
  "A function that sets the status of the current buffer's file, for a new check.
It takes the status and, when known, the SWHID, and does nothing once
the buffer is gone or a later check started."
  (let ((buffer (current-buffer))
        (check (setq swh--check (1+ swh--check))))
    (lambda (status &optional swhid)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (= check swh--check)
            (when swhid (setq swh--swhid swhid))
            (setq swh--status status)
            (force-mode-line-update)))))))

(defun swh--refresh ()
  "Ask for the archival status of the buffer's file, or Dired directory."
  (when-let* ((path (and swh-mode (swh--mode-path))))
    (setq swh--status 'pending
          swh--checked (cons (float-time) (swh--stamp path)))
    (force-mode-line-update)
    (let* ((file (file-truename path))
           (root (swh--repo-root-of file))
           (set-status (swh--status-setter))
           (failed (lambda (why)
                     ;; the one failure the user can act on
                     (when (string-prefix-p swh--rate-limit-message why)
                       (message "%s" why))
                     (funcall set-status 'error))))
      (swh--try failed
                (lambda ()
                  (if (not (file-directory-p file))
                      (swh--file-mode-status-async file root set-status failed)
                    ;; the tree's SWHID, when told, is that of the tree as it is now
                    (setq swh--swhid nil)
                    (swh--dir-status-async file root set-status failed)))))))

;;;###autoload
(define-minor-mode swh-mode
  "Show in the mode line whether the file's content is in the archive.
The status, a lighter of `swh-mode-lighters', is checked when the mode
is turned on and after each save; in Dired, when the directory is
read in.  Clicking the lighter copies the qualified SWHID, see
`swh-swhid'.  Without a token, the lighter also counts the API's
hourly requests used, e.g. 48/120."
  :lighter swh-mode-line-format
  (if swh-mode
      (progn
        (add-hook 'after-save-hook #'swh--refresh nil t)
        (add-hook 'dired-after-readin-hook #'swh--dired-refresh nil t)
        ;; Dired reads the directory in after the modes are on, and
        ;; checks it then; a file checked as it is needs no other check
        (unless (or (and (derived-mode-p 'dired-mode) (zerop (buffer-size)))
                    (when-let* ((path (swh--mode-path)))
                      (equal (cdr swh--checked) (swh--stamp path))))
          (swh--refresh)))
    (remove-hook 'after-save-hook #'swh--refresh t)
    (remove-hook 'dired-after-readin-hook #'swh--dired-refresh t)
    (setq swh--status nil
          swh--checked nil)))

(defun swh--turn-on ()
  "Turn on `swh-mode' on a local file or Dired directory under Git."
  (let ((path (or buffer-file-name
                  (and (derived-mode-p 'dired-mode) default-directory))))
    (when (and path
               (not (string-match-p "/\\.git\\(?:/\\|\\'\\)" (expand-file-name path)))
               (swh--repo-root-of path))
      (swh-mode 1))))

;;;###autoload
(define-globalized-minor-mode global-swh-mode swh-mode swh--turn-on)

(provide 'swh)
;;; swh.el ends here
