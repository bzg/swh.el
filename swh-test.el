;;; swh-test.el --- Tests for swh.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry

;;; Commentary:

;; Run with:
;;   emacs -Q -batch -L . -l swh-test.el -f ert-run-tests-batch-and-exit
;;
;; The daemon is mocked at the `url-retrieve' level, the archive's Web
;; API at the `swh--api' level, so these tests pin both contracts.  The direct backend's Git side runs against real
;; temporary repositories, without the user's or the system's Git
;; configuration.  In batch, the tests run without the user's token
;; and auth-source, and no request leaves Emacs unless a test fakes it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'swh)

(defun swh-test--no-network (&rest _)
  "What url.el does when nothing answers."
  (error "No network in tests"))

(when noninteractive
  (setenv "SWH_API_TOKEN" nil)
  (advice-add 'auth-source-pick-first-password :override #'ignore)
  (dolist (fn '(url-retrieve-synchronously url-retrieve))
    (advice-add fn :override #'swh-test--no-network)))

;;; Helpers

(defun swh-test--api-async (method url body _headers callback &optional _timeout)
  "Fake `swh--retrieve-async' that answers through `swh--api', at once."
  (funcall callback (swh--api method (string-remove-prefix (swh--api-url "") url) body)))

(defun swh-test--answering (responder)
  "A fake `url-retrieve': RESPONDER gets the URL, and gives a response buffer.
nil stands for a refused connection, as url.el does: a buffer without
any response status."
  (lambda (url callback &rest _)
    (with-current-buffer (or (funcall responder url) (swh-test--response-buffer nil nil))
      (funcall callback nil))
    nil))

(defun swh-test--response-buffer (status json &optional headers)
  "Fake response buffer of `url-retrieve' replying STATUS, HEADERS and JSON."
  (let ((buf (generate-new-buffer " *swh-test-response*")))
    (with-current-buffer buf
      (insert (or headers ""))
      (let ((end (point)))
        (insert (or json ""))
        (setq-local url-http-response-status status)
        (setq-local url-http-end-of-headers end)))
    buf))

(defun swh-test--daemon (&optional gone)
  "A fresh `swh--daemon-checked' saying that the daemon answers, or is GONE.
This settles the backend without a ping, until it fails."
  (cons (float-time) (unless gone `((pong . t) (version . ,swh-daemon-min-version)))))

(defmacro swh-test--with-daemon (status json &rest body)
  "Run BODY with the daemon backend mocked to reply STATUS and JSON."
  (declare (indent 2))
  `(let ((swh--daemon-checked (swh-test--daemon))
         (swh--provenance-refused nil))
     (cl-letf (((symbol-function 'url-retrieve)
                (swh-test--answering (lambda (_) (swh-test--response-buffer ,status ,json)))))
       ,@body)))

(defmacro swh-test--user-error (form)
  "The message of the `user-error' FORM signals, failing the test without one."
  `(cadr (should-error ,form :type 'user-error)))

(defmacro swh-test--last-message (&rest body)
  "The last message BODY shows."
  (declare (indent 0))
  `(let ((said nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format &rest args) (setq said (apply #'format-message format args)))))
       ,@body)
     said))

(defun swh-test--git (dir &rest args)
  "Run git ARGS in DIR, failing the test when git fails."
  (with-temp-buffer
    (let ((code (apply #'call-process "git" nil t nil "-C" dir
                       "-c" "user.name=Test" "-c" "user.email=test@example.org" args)))
      (unless (eq 0 code)
        (error "git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(defun swh-test--tree (root)
  "The tree `swh--tree-async' gives for ROOT."
  (let (nodes)
    (swh--tree-async root (lambda (tree) (setq nodes tree)) #'ignore)
    nodes))

(defmacro swh-test--with-repo (&rest body)
  "Run BODY with DIR a Git repository holding a.txt (tracked) and sub/b.txt.
Its origin is git@example.org:me/repo.git."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "swh-test" t)))
          (file (expand-file-name "a.txt" dir))
          ;; for every git run, the tests' and swh.el's
          (process-environment (append (list (concat "GIT_CONFIG_GLOBAL=" null-device)
                                             "GIT_CONFIG_NOSYSTEM=1")
                                       process-environment)))
     (unwind-protect
         (progn
           (with-temp-file file (insert "aaa\nbbb\nccc\n"))
           (make-directory (expand-file-name "sub" dir))
           (with-temp-file (expand-file-name "sub/b.txt" dir) (insert "bbb\n"))
           (swh-test--git dir "init" "-q" "-b" "main")
           (swh-test--git dir "remote" "add" "origin" "git@example.org:me/repo.git")
           (swh-test--git dir "add" "-A")
           (swh-test--git dir "commit" "-q" "-m" "one")
           ,@body)
       (delete-directory dir t))))

(defmacro swh-test--with-file (&rest body)
  "Run BODY in a buffer visiting the saved a.txt of a `swh-test--with-repo'."
  (declare (indent 0))
  `(swh-test--with-repo
     (with-temp-buffer
       (insert-file-contents file)
       (setq buffer-file-name file
             default-directory dir)
       (set-buffer-modified-p nil)
       ,@body)))

(defconst swh-test--snapshot "0000000000000000000000000000000000000042")

(defconst swh-test--core "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2")

(defconst swh-test--qualified
  (concat swh-test--core
          ";origin=https://example.org/me/repo"
          ";visit=swh:1:snp:" swh-test--snapshot
          ";anchor=swh:1:rev:0000000000000000000000000000000000000007"
          ";path=/a.txt;lines=2-3"))

(defmacro swh-test--with-reports (&rest body)
  "Run BODY with the report buffers not displayed, and killed after."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
             ((symbol-function 'display-buffer) #'ignore))
     (unwind-protect (progn ,@body)
       (dolist (name '("*swh scan*" "*swh visits*" "*swh cite*"))
         (when (get-buffer name) (kill-buffer name))))))

(defmacro swh-test--with-token (&rest body)
  "Run BODY with an API token, which the provenance API needs."
  (declare (indent 0))
  `(with-environment-variables (("SWH_API_TOKEN" "tok"))
     (let ((swh--provenance-refused nil))
       ,@body)))

(defun swh-test--swhid-json (archived)
  "Daemon reply to /swhid, ARCHIVED or not."
  (format "{\"swhid\":\"%s\",\"core\":\"%s\",\"archived\":%s,\"origin\":\"https://example.org/me/repo\",\"origin-archived\":true,\"browse-url\":\"https://archive.softwareheritage.org/%s\"}"
          swh-test--qualified swh-test--core (if archived "true" "false") swh-test--qualified))

;;; HTTP and the daemon backend

(ert-deftest swh-test-request-errors ()
  (pcase-dolist (`(,status ,json ,said)
                 ;; nil: what url.el does on a refused connection, a
                 ;; buffer without any response status, no signal
                 '((nil nil "Cannot reach swhd")
                   (429 "{\"error\":\"SWH API rate limit\",\"retry-after\":42}"
                        "retry in 42 seconds")
                   ;; a user-facing message, shown plain
                   (400 "{\"error\":\"No remote\"}" "\\`No remote\\'")
                   (502 "{\"error\":\"SWH API error 503\",\"upstream-status\":503}"
                        "archive answered HTTP 503")
                   (200 "<html>not the daemon</html>" "Invalid response")
                   ;; an outdated daemon, taken for a recent one
                   (404 "{\"error\":\"not found\"}" "Unknown to swhd")))
    (swh-test--with-daemon status json
      (should (string-match-p said (swh-test--user-error
                                    (swh--daemon "GET" "/origin" :path "/tmp/repo"))))))
  ;; the archive's reason, told as swhd tells it
  (should (string-match-p "HTTP 403 (the save request: not allowed)\\'"
                          (swh-test--user-error
                           (swh--api-body '(403 ((exception . "ForbiddenExc")
                                                 (reason . "not allowed")))
                                          "the save request"))))
  ;; an empty object is an answer
  (swh-test--with-daemon 200 "{}"
    (should (null (swh--daemon "GET" "/origin" :path "/tmp/repo"))))
  (let ((swh--daemon-checked (swh-test--daemon)))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest _) (error "Connection refused"))))
      (should (string-match-p "Cannot reach swhd" (swh-test--user-error
                                                   (swh--daemon "GET" "/origin" :path "/tmp/repo"))))))
  ;; gone: it is pinged again next time
  (swh-test--with-daemon nil nil
    (should-error (swh--daemon "GET" "/origin" :path "/tmp/repo") :type 'user-error)
    (should (null swh--daemon-checked))))

(ert-deftest swh-test-pending-request-does-not-hold-emacs ()
  ;; the real url.el, on a local server that never answers: Emacs
  ;; must not ask about the request on exit
  (let* ((server (make-network-process :name "swh-test-server" :server t :noquery t
                                       :host "127.0.0.1" :service t :family 'ipv4))
         (swh-daemon-url (format "http://127.0.0.1:%d" (process-contact server :service)))
         (swh--daemon-checked (swh-test--daemon))
         (answered nil))
    (advice-remove 'url-retrieve #'swh-test--no-network)
    (unwind-protect
        (progn
          (swh--daemon "GET" "/file" :path "/tmp/x" :timeout 1
                       :callback (lambda (_) (setq answered t))
                       :failed (lambda (_) (setq answered t)))
          (let ((pending (seq-filter (lambda (p) (string-prefix-p "127.0.0.1" (process-name p)))
                                     (process-list))))
            (should pending)
            (should-not (seq-some #'process-query-on-exit-flag pending)))
          ;; the timeout gives up on it
          (while (not answered) (accept-process-output nil 0.1))
          (should-not (get-buffer (format " *http 127.0.0.1:%d*" (process-contact server :service)))))
      (advice-add 'url-retrieve :override #'swh-test--no-network)
      (delete-process server))))

(ert-deftest swh-test-backend-daemon-availability ()
  "The daemon is used when its ping answers, once, and it is recent enough."
  (let ((swh--daemon-checked nil)
        (pings 0))
    (cl-letf (((symbol-function 'url-retrieve)
               (swh-test--answering
                (lambda (_)
                  (setq pings (1+ pings))
                  (swh-test--response-buffer
                   200 (format "{\"pong\":true,\"version\":%S}" swh-daemon-min-version))))))
      (should (swh--daemon-available-p))
      (should (swh--daemon-available-p))
      ;; the answer is remembered
      (should (= 1 pings)))
    (setq swh--daemon-checked nil)
    (cl-letf (((symbol-function 'url-retrieve) (swh-test--answering #'ignore)))
      (should-not (swh--daemon-available-p))))
  ;; older than `swh-daemon-min-version': not used, with one warning
  (let ((swh--daemon-outdated-warned nil)
        (warnings nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &rest _) (push message warnings))))
      (dolist (pong '("{\"pong\":true}" "{\"pong\":true,\"version\":\"0.1.1\"}"))
        (let ((swh--daemon-checked nil))
          (cl-letf (((symbol-function 'url-retrieve)
                     (swh-test--answering (lambda (_) (swh-test--response-buffer 200 pong)))))
            (should-not (swh--daemon-available-p))))))
    (should (= 1 (length warnings)))
    (should (string-match-p "is older than" (car warnings))))
  (should (swh--daemon-version-ok-p '((version . "0.10.0"))))
  (should (swh--daemon-version-ok-p '((version . "0.9.0-dev+abc"))))
  (should-not (swh--daemon-version-ok-p '((version . "0.1.9"))))
  (should-not (swh--daemon-version-ok-p '((version . "dev")))))

;;; Commands through the daemon

(ert-deftest swh-test-status-and-save ()
  (cl-letf (((symbol-function 'swh--repo-root) (lambda () "/tmp/repo")))
    (pcase-dolist (`(,json . ,said)
                   '(("{\"origin\":\"https://example.org/repo.git\",\"archived\":true,\"date\":\"2026-07-01T03:14:15+00:00\"}"
                      . "is archived (last visit 2026-07-01")
                     ("{\"origin\":\"https://example.org/repo.git\",\"archived\":true}"
                      . "last visit unknown")))
      (swh-test--with-daemon 200 json
        (should (string-match-p said (swh-status)))))
    (swh-test--with-daemon 200
        "{\"origin\":\"https://example.org/repo.git\",\"save_request_status\":\"accepted\"}"
      (should (string-match-p "Save request accepted"
                              (swh-test--last-message (swh-save)))))
    ;; the answer comes at any moment: said, never asked
    (swh-test--with-daemon 200
        "{\"origin\":\"https://example.org/repo.git\",\"save_request_status\":\"pending\"}"
      (should (string-match-p "awaits review"
                              (swh-test--last-message (swh-save)))))
    ;; and so does an error, with no command loop to catch it
    (swh-test--with-daemon 502 "{\"error\":\"the save\",\"upstream-status\":500}"
      (should (string-match-p "answered HTTP 500"
                              (swh-test--last-message (swh-save)))))))

(ert-deftest swh-test-status-upstream ()
  (cl-letf (((symbol-function 'swh--repo-root) (lambda () "/tmp/repo")))
    (let ((swh--daemon-checked (swh-test--daemon))
          (json "{\"origin\":\"https://example.org/repo\",\"archived\":true,\"date\":\"2026-07-01T03:14:15+00:00\",\"upstream\":\"swh:1:rev:2db1aa0000000000000000000000000000000000\",\"upstream-archived\":%s}"))
      (cl-letf (((symbol-function 'url-retrieve)
                 (swh-test--answering
                  (lambda (_) (swh-test--response-buffer 200 (format json "true"))))))
        (should (string-match-p "is archived, upstream included"
                                (swh-status))))
      ;; the last commit pushed is not archived yet: offer to save
      (let ((asked nil) (saved nil))
        (cl-letf (((symbol-function 'url-retrieve)
                   (swh-test--answering
                    (lambda (_) (swh-test--response-buffer 200 (format json "false")))))
                  ((symbol-function 'swh-save) (lambda (&optional _) (setq saved t)))
                  ((symbol-function 'y-or-n-p) (lambda (prompt) (setq asked prompt) nil)))
          (should (string-match-p "upstream 2db1aa0 not yet\\'" (swh-status)))
          (should (string-match-p "save it now" asked))
          (should-not saved))))))

;;; Origins and SWHIDs, as the daemon makes them

(defconst swh-test--fixtures-file
  (expand-file-name "../swhd/test/swhd/fixtures.json"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The fixture shared with the daemon's tests, in the sibling checkout.")

(defun swh-test--fixtures ()
  "The shared fixture, or skip the test when the daemon's checkout is absent."
  ;; `skip-unless' only exists inside `ert-deftest' bodies
  (unless (file-readable-p swh-test--fixtures-file)
    (ert-skip (list 'file-missing swh-test--fixtures-file)))
  (with-temp-buffer
    (insert-file-contents swh-test--fixtures-file)
    (json-parse-buffer :object-type 'alist :array-type 'list
                       :false-object nil :null-object nil)))

(ert-deftest swh-test-normalize-origin-fixture ()
  "Origin normalization agrees with the daemon's, case by case."
  (dolist (case (alist-get 'normalize (swh-test--fixtures)))
    (let ((url (alist-get 'url case)))
      (should (equal (alist-get 'origin case) (swh--normalize-origin url)))
      (should (eq (alist-get 'public case)
                  (and (swh--public-origin-p (swh--normalize-origin url)) t)))
      (should (equal (if (alist-get 'public case)
                         (list (alist-get 'origin case) (concat (alist-get 'origin case) ".git"))
                       (list (alist-get 'origin case)))
                     (swh--origin-candidates url))))))

(ert-deftest swh-test-qualify-fixture ()
  "SWHID qualification agrees with the daemon's, case by case."
  (dolist (case (alist-get 'qualify (swh-test--fixtures)))
    (let ((lines (alist-get 'lines case)))
      (should (equal (alist-get 'swhid case)
                     (swh--qualify (alist-get 'core case)
                                   (alist-get 'origin case)
                                   (alist-get 'snapshot case)
                                   (alist-get 'anchor case)
                                   (alist-get 'rel case)
                                   (and lines (mapconcat #'number-to-string lines "-"))))))))

;; values from swhid-clj, which the daemon renders qualifiers with
(ert-deftest swh-test-qualify-encoding ()
  (should (equal "https://h/o%20r%09x%C2%A0é"
                 (swh--encode-origin "https://h/o r\tx\u00a0é")))
  (should (equal "/src/my%20file%20%C3%A9%3B%25.el"
                 (swh--encode-path "/src/my file é;%.el")))
  (should (equal "swh:1:cnt:x;origin=https://h/o%20r;anchor=swh:1:rev:y;path=/src/my%20file%20%C3%A9.el"
                 (swh--qualify "swh:1:cnt:x" "https://h/o r" nil "swh:1:rev:y"
                               "src/my file é.el" nil)))
  (should (equal '((origin . "https://h/o r") (path . "/src/my file é.el"))
                 (swh--qualifiers "swh:1:cnt:x;origin=https://h/o%20r;path=/src/my%20file%20%C3%A9.el"))))

;;; The direct backend

(defvar swh-test--asked nil "SWHIDs the fake archive was last asked about.")

(defvar swh-test--posts 0 "Number of POSTs the fake archive received.")

(defun swh-test--archive (archived-origin &optional known)
  "Responder for `swh-test--with-archive': ARCHIVED-ORIGIN has a visit, the
SWHIDs in KNOWN are archived, save requests are accepted."
  (lambda (method path)
    (cond
     ((string-match "\\`/origin/\\(.*\\)/visits/" path)
      (list 200 (if (equal (match-string 1 path) archived-origin)
                    (vector `((date . "2026-10-19T09:15:55.367000+00:00") (status . "full")
                              (snapshot . ,swh-test--snapshot))
                            '((date . "2026-07-01T03:14:15+00:00") (status . "partial")
                              (snapshot . nil)))
                  [])))
     ((string-match "\\`/origin/\\(.*\\)/visit/latest/" path)
      (if (equal (match-string 1 path) archived-origin)
          `(200 ((origin . ,archived-origin) (date . "2026-07-01T03:14:15+00:00")
                 (status . "full") (snapshot . ,swh-test--snapshot)))
        '(404 ((exception . "NotFoundExc")))))
     ((equal path "/known/")
      (list 200 (mapcar (lambda (s) (cons (intern s) `((known . ,(and (member s known) t)))))
                        swh-test--asked)))
     ((and (equal method "POST") (string-prefix-p "/origin/save/git/url/" path))
      '(200 ((save_request_status . "accepted"))))
     ((string-prefix-p "/raw-intrinsic-metadata/citation/" path)
      '(404 ((exception . "NotFoundExc"))))
     (t (error "unexpected %s %s" method path)))))

(defmacro swh-test--with-archive (archived-origin known &rest body)
  "Run BODY against the fake archive (see `swh-test--archive')."
  (declare (indent 2))
  `(let ((swh--daemon-checked (swh-test--daemon 'gone))
         (swh--origin-memo (make-hash-table :test #'equal))
         (swh--known-memo (make-hash-table :test #'equal))
         (swh-test--asked nil)
         (swh-test--posts 0))
     (cl-letf (((symbol-function 'swh--api)
                (let ((responder (swh-test--archive ,archived-origin ,known)))
                  (lambda (method path &optional body)
                    (when body
                      (setq swh-test--posts (1+ swh-test--posts)
                            swh-test--asked (append (json-parse-string body :array-type 'list) nil)))
                    (append (funcall responder method path) '(nil)))))
               ((symbol-function 'swh--retrieve-async) #'swh-test--api-async))
       ,@body)))

(ert-deftest swh-test-direct-origin ()
  (swh-test--with-repo
    (swh-test--with-archive "https://example.org/me/repo" nil
      (let ((info (swh--origin dir)))
        (should (equal "https://example.org/me/repo" (alist-get 'origin info)))
        (should (eq t (alist-get 'archived info)))
        (should (equal swh-test--snapshot (alist-get 'snapshot info))))
      ;; a changed remote is not answered for by the old one
      (swh-test--git dir "remote" "set-url" "origin" "https://example.org/me/new")
      (should (equal "https://example.org/me/new" (alist-get 'origin (swh--origin dir))))
      (swh-test--git dir "remote" "set-url" "origin" "git@example.org:me/repo.git"))
    (swh-test--with-archive "https://example.org/me/repo.git" nil
      ;; the .git form when that is the archived one
      (should (equal "https://example.org/me/repo.git"
                     (alist-get 'origin (swh--origin dir)))))
    (swh-test--with-archive nil nil
      (let ((info (swh--origin dir)))
        (should (equal "https://example.org/me/repo" (alist-get 'origin info)))
        (should (null (alist-get 'archived info)))))))

(ert-deftest swh-test-direct-origin-upstream ()
  (swh-test--with-repo
    (swh-test--git dir "remote" "set-url" "origin" "https://example.org/me/repo.git")
    (let ((pushed (concat "swh:1:rev:" (swh-test--git dir "rev-parse" "HEAD"))))
      ;; no upstream: nothing to tell
      (swh-test--with-archive "https://example.org/me/repo" (list pushed)
        (should-not (assq 'upstream (swh--origin dir t))))
      ;; as after a push, then a commit not pushed yet
      (swh-test--git dir "update-ref" "refs/remotes/origin/main" "HEAD")
      (swh-test--git dir "config" "branch.main.remote" "origin")
      (swh-test--git dir "config" "branch.main.merge" "refs/heads/main")
      (with-temp-file file (insert "new\n"))
      (swh-test--git dir "commit" "-q" "-am" "two")
      (swh-test--with-archive "https://example.org/me/repo" (list pushed)
        (should-not (assq 'upstream (swh--origin dir)))
        (let ((info (swh--origin dir t)))
          (should (equal pushed (alist-get 'upstream info)))
          (should (eq t (alist-get 'upstream-archived info)))))
      (swh-test--with-archive "https://example.org/me/repo" nil
        (let ((info (swh--origin dir t)))
          (should (equal pushed (alist-get 'upstream info)))
          (should (assq 'upstream-archived info))
          (should-not (alist-get 'upstream-archived info))))
      ;; an origin not archived: the archive is not asked about it
      (swh-test--with-archive nil (list pushed)
        (should-not (assq 'upstream (swh--origin dir t)))
        (should (zerop swh-test--posts)))
      ;; a local branch as upstream
      (swh-test--git dir "config" "branch.main.remote" ".")
      (swh-test--with-archive "https://example.org/me/repo" (list pushed)
        (should-not (assq 'upstream (swh--origin dir t)))))))

(ert-deftest swh-test-visits-report ()
  (swh-test--with-file
    (swh-test--with-archive "https://example.org/me/repo" nil
      (swh-test--with-reports
        (swh-visits)
        (with-current-buffer "*swh visits*"
          (should (string-match-p "Last 2 archival visits of https://example.org/me/repo"
                                  (buffer-string)))
          (should (string-match-p "full +swh:1:snp:" (buffer-string)))
          (should (string-match-p "partial" (buffer-string)))
          (should (eq major-mode 'special-mode)))))
    (swh-test--with-archive nil nil
      (should (equal "https://example.org/me/repo is not archived" (swh-visits))))))

(ert-deftest swh-test-direct-save ()
  (swh-test--with-repo
    (swh-test--with-archive nil nil
      (swh--memo-put swh--known-memo "swh:1:cnt:aa" nil)
      (swh--memo-put swh--known-memo "swh:1:cnt:bb" t)
      (let (resp)
        (swh--save-async dir (lambda (r) (setq resp r)))
        (should (equal "accepted" (alist-get 'save_request_status resp)))
        (should (equal "https://example.org/me/repo" (alist-get 'origin resp))))
      ;; a save forgets the negative answers, keeps the positive ones
      (should (eq 'unknown (swh--memo-get swh--known-memo "swh:1:cnt:aa")))
      (should (eq t (swh--memo-get swh--known-memo "swh:1:cnt:bb"))))
    ;; and the status of the origin saved, this repository's or not
    (swh-test--with-archive nil nil
      (swh--origin dir)
      (let ((remote (swh--remote-url dir)))
        (should (gethash remote swh--origin-memo))
        ;; another origin: the repository's status holds
        (should (string-match-p "Save request accepted for https://example.org/me/other"
                                (swh-test--last-message
                                  (swh-save "https://example.org/me/other"))))
        (should (gethash remote swh--origin-memo))
        (should-error (swh-save "/srv/git/repo") :type 'user-error)
        ;; its own, as C-u M-x swh-save offers it
        (swh-save "https://example.org/me/repo")
        (should-not (gethash remote swh--origin-memo))))
    (swh-test--git dir "remote" "set-url" "origin" "/srv/git/repo")
    (swh-test--with-archive nil nil
      (should (string-match-p "not a public URL"
                              (swh-test--user-error (swh--save-async dir #'ignore)))))
    (swh-test--git dir "remote" "remove" "origin")
    (swh-test--with-archive nil nil
      (should (equal "No remote" (swh-test--user-error (swh--save-async dir #'ignore)))))))

(ert-deftest swh-test-direct-swhid ()
  (swh-test--with-repo
    (let* ((cnt (concat "swh:1:cnt:" (swh-test--git dir "hash-object" "a.txt")))
           (rev (concat "swh:1:rev:" (swh-test--git dir "rev-parse" "HEAD"))))
      (swh-test--with-archive "https://example.org/me/repo" (list cnt rev)
        (let ((info (swh--qualified-swhid file "2-3")))
          (should (equal (concat cnt ";origin=https://example.org/me/repo"
                                 ";visit=swh:1:snp:" swh-test--snapshot
                                 ";anchor=" rev ";path=/a.txt;lines=2-3")
                         (alist-get 'swhid info)))
          (should (equal cnt (alist-get 'core info)))
          (should (eq t (alist-get 'archived info)))
          (should (eq t (alist-get 'origin-archived info))))
        (should (string-suffix-p ";path=/sub/b.txt"
                                 (alist-get 'swhid (swh--qualified-swhid
                                                    (expand-file-name "sub/b.txt" dir) nil)))))
      (swh-test--with-archive "https://example.org/me/repo" (list cnt rev)
        ;; an edited file: HEAD does not hold this content, no anchor
        (with-temp-file file (insert "edited\n"))
        (let ((info (swh--qualified-swhid file "1")))
          (should (string-suffix-p (concat ";visit=swh:1:snp:" swh-test--snapshot ";lines=1")
                                   (alist-get 'swhid info)))
          (should-not (string-match-p "anchor=" (alist-get 'swhid info))))
        (swh-test--git dir "checkout" "-q" "a.txt"))
      (swh-test--with-archive "https://example.org/me/repo" (list cnt)
        (let ((info (swh--qualified-swhid file "2")))
          (should (equal (concat cnt ";origin=https://example.org/me/repo"
                                 ";visit=swh:1:snp:" swh-test--snapshot ";lines=2")
                         (alist-get 'swhid info)))
          (should (eq t (alist-get 'archived info)))))
      (swh-test--with-archive nil nil
        (let ((info (swh--qualified-swhid file nil)))
          (should (equal (concat cnt ";origin=https://example.org/me/repo")
                         (alist-get 'swhid info)))
          (should (null (alist-get 'archived info)))
          (should (null (alist-get 'origin-archived info)))))
      ;; no public origin: the bare SWHID, and the archive is not asked
      (swh-test--git dir "remote" "set-url" "origin" "/srv/git/repo")
      (swh-test--with-archive nil nil
        (should (equal cnt (alist-get 'swhid (swh--qualified-swhid file nil))))
        (should (= 0 swh-test--posts))
        (should (equal "the origin /srv/git/repo is not a public URL"
                       (swh--unarchived-why file))))
      (swh-test--git dir "remote" "remove" "origin")
      (swh-test--with-archive nil nil
        (should (equal cnt (alist-get 'swhid (swh--qualified-swhid file nil))))
        (should (= 0 swh-test--posts))
        (should (equal "the repository has no remote" (swh--unarchived-why file)))))))

(ert-deftest swh-test-direct-swhid-outside-a-repository ()
  (let ((loose (make-temp-file "swh-test" nil ".txt" "loose\n")))
    (unwind-protect
        (swh-test--with-archive nil nil
          (let ((info (swh--qualified-swhid loose "1"))
                (hash (swh-test--git (file-name-directory loose) "hash-object" "--" loose)))
            (should (equal (concat "swh:1:cnt:" hash ";lines=1")
                           (alist-get 'swhid info)))
            (should (null (alist-get 'origin info)))))
      (delete-file loose)))
  ;; hashed as is, as the daemon does, whatever the global autocrlf
  (let ((crlf (make-temp-file "swh-test" nil ".txt" "a\r\nb\r\n")))
    (unwind-protect
        (with-environment-variables (("GIT_CONFIG_COUNT" "1")
                                     ("GIT_CONFIG_KEY_0" "core.autocrlf")
                                     ("GIT_CONFIG_VALUE_0" "true"))
          (should (equal "swh:1:cnt:c30dea8a3641ea99b125d04d599d843712292759"
                         (swh--content-swhid crlf nil))))
      (delete-file crlf))))

;;; Origins asked for

(ert-deftest swh-test-remote-directories-are-refused ()
  (with-temp-buffer
    (setq default-directory "/ssh:nowhere.invalid:/srv/repo/")
    ;; refused before any connection
    (cl-letf (((symbol-function 'vc-git-root) (lambda (&rest _) (error "Connected")))
              ((symbol-function 'read-string) (lambda (&rest _) "example.org/me/other")))
      (dolist (command '(swh-swhid swh-scan swh-provenance))
        (should (equal "Remote files are not supported"
                       (swh-test--user-error (call-interactively command)))))
      ;; no public origin there: the commands that can take one ask for it
      (dolist (command '(swh-status swh-visits swh-save swh-cite))
        (should (equal '("https://example.org/me/other")
                       (advice-eval-interactive-spec
                        (cadr (interactive-form command)))))))
    ;; nor are the automatic features turned on
    (setq buffer-file-name "/ssh:nowhere.invalid:/srv/repo/a.txt")
    (swh--turn-on)
    (should-not swh-mode)
    (setq buffer-file-name nil)))

(ert-deftest swh-test-status-of-an-origin ()
  (with-temp-buffer
    (setq default-directory temporary-file-directory)
    (swh-test--with-archive "https://example.org/me/other" nil
      (should (string-match-p "\\`https://example.org/me/other is archived (last visit "
                              (swh-status "https://example.org/me/other")))
      (swh-test--with-reports
        (swh-visits "https://example.org/me/other")
        (with-current-buffer "*swh visits*"
          (should (string-match-p "Last 2 archival visits of https://example.org/me/other"
                                  (buffer-string)))))
      (should-error (swh-status "/srv/git/repo") :type 'user-error)
      ;; not archived: the save is for that origin
      (let ((saved 'none))
        (cl-letf (((symbol-function 'y-or-n-p) #'always)
                  ((symbol-function 'swh-save) (lambda (&optional o) (setq saved o))))
          (swh-status "https://example.org/me/new")
          (should (equal "https://example.org/me/new" saved)))))))

(ert-deftest swh-test-commands-ask-for-the-origin ()
  (let ((outside (file-name-as-directory (make-temp-file "swh-test" t)))
        (prompted nil))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'read-string)
                     (lambda (prompt &rest _)
                       (setq prompted prompt)
                       "example.org/me/other.git")))
            (pcase-dolist (`(,command . ,verb) '((swh-status . "check")
                                                 (swh-visits . "list the visits of")
                                                 (swh-save . "save")
                                                 (swh-cite . "cite")))
              (let ((spec (cadr (interactive-form command))))
                ;; outside a repository: asked, saying why, and normalized
                (setq default-directory outside prompted nil)
                (should (equal '("https://example.org/me/other")
                               (advice-eval-interactive-spec spec)))
                (should (equal (concat "No public origin here; origin to " verb ": ") prompted))
                ;; in a repository with a public origin: only with a
                ;; prefix argument, any, and that origin by default
                (swh-test--with-repo
                  (setq default-directory dir prompted nil)
                  (should (equal '(nil) (advice-eval-interactive-spec spec)))
                  (should-not prompted)
                  (dolist (arg '((4) (16) 1 -))
                    (let ((current-prefix-arg arg))
                      (should (equal '("https://example.org/me/other")
                                     (advice-eval-interactive-spec spec)))))
                  (should (equal (concat "Origin to " verb " (default https://example.org/me/repo): ")
                                 prompted)))))))
      (delete-directory outside t))))

;;; Whole-repository scans

(ert-deftest swh-test-direct-tree ()
  (swh-test--with-repo
    (let* ((root-swhid (concat "swh:1:dir:" (swh-test--git dir "rev-parse" "HEAD^{tree}")))
           (sub-swhid (concat "swh:1:dir:" (swh-test--git dir "rev-parse" "HEAD:sub")))
           (a-swhid (concat "swh:1:cnt:" (swh-test--git dir "rev-parse" "HEAD:a.txt")))
           (b-swhid (concat "swh:1:cnt:" (swh-test--git dir "rev-parse" "HEAD:sub/b.txt"))))
      (swh-test--with-archive nil (list root-swhid)
        (let ((nodes (swh-test--tree dir)))
          (should (equal (list "." root-swhid t t) (car nodes)))
          (should (equal (list a-swhid sub-swhid b-swhid)
                         (mapcar #'cadr (cdr nodes))))
          (should (seq-every-p (lambda (n) (nth 3 n)) nodes))
          ;; the archived root covers everything: one request, directories only
          (should (= 1 swh-test--posts))
          (should (equal (sort (list root-swhid sub-swhid) #'string<)
                         (sort swh-test--asked #'string<)))))
      (swh-test--with-archive nil (list sub-swhid a-swhid)
        (let ((nodes (swh-test--tree dir)))
          (should (equal '(nil t t t) (mapcar (lambda (n) (nth 3 n)) nodes)))
          ;; directories, then the files no directory covers: a.txt only
          (should (= 2 swh-test--posts))
          (should (equal (list a-swhid) swh-test--asked))))
      (swh-test--with-archive nil nil
        (should (equal '(nil nil nil nil)
                       (mapcar (lambda (n) (nth 3 n)) (swh-test--tree dir))))))))

(ert-deftest swh-test-worktree-tree ()
  (swh-test--with-repo
    (let ((head (swh-test--git dir "rev-parse" "HEAD^{tree}")))
      (should (equal head (swh--worktree-tree dir)))
      (with-temp-file file (insert "edited\n"))
      (with-temp-file (expand-file-name "untracked.txt" dir) (insert "u\n"))
      (let ((tree (swh--worktree-tree dir)))
        (should-not (equal head tree))
        ;; the edit is in, the untracked file is not
        (should (equal (swh-test--git dir "hash-object" "a.txt")
                       (swh-test--git dir "rev-parse" (concat tree ":a.txt"))))
        (should-not (swh--git dir "rev-parse" "-q" "--verify" (concat tree ":untracked.txt"))))
      ;; the index is left alone
      (should (equal "" (or (swh-test--git dir "diff" "--cached" "--name-only") "")))
      ;; a file git cannot read is an error, not a stale tree; root reads all
      (unless (zerop (user-uid))
        (set-file-modes file 0)
        (unwind-protect
            (should (string-match-p "Git add failed"
                                    (swh-test--user-error (swh--worktree-tree dir))))
          (set-file-modes file #o644))))))

(ert-deftest swh-test-head-tree ()
  (swh-test--with-repo
    (let* ((sub (expand-file-name "sub" dir))
           (index (expand-file-name ".git/index" dir))
           (state (lambda ()
                    (list (file-attribute-modification-time (file-attributes index))
                          (swh-test--git dir "count-objects")))))
      (should (equal (swh-test--git dir "rev-parse" "HEAD^{tree}") (swh--head-tree dir dir)))
      (should (equal (swh-test--git dir "rev-parse" "HEAD:sub") (swh--head-tree sub dir)))
      ;; a date alone changed: as in HEAD, and the index is left alone
      (set-file-times file (time-add nil 10))
      (let ((before (funcall state)))
        (should (swh--head-tree dir dir))
        (should (equal before (funcall state))))
      ;; modified, but elsewhere than in sub: nothing is written either
      (with-temp-file file (insert "edited\n"))
      (let ((before (funcall state)))
        (should-not (swh--head-tree dir dir))
        (should (swh--head-tree sub dir))
        (should (equal before (funcall state))))
      (swh-test--git dir "checkout" "-q" "a.txt")
      ;; staged, not committed
      (with-temp-file (expand-file-name "sub/b.txt" dir) (insert "new\n"))
      (swh-test--git dir "add" "sub/b.txt")
      (should-not (swh--head-tree sub dir)))))

(ert-deftest swh-test-direct-tree-submodule ()
  (swh-test--with-repo
    (swh-test--git dir "update-index" "--add" "--cacheinfo"
                   "160000,1111111111111111111111111111111111111111,sub/mod")
    (swh-test--git dir "commit" "-q" "-m" "submodule")
    (make-directory (expand-file-name "sub/mod" dir))
    (let ((root (concat "swh:1:dir:" (swh-test--git dir "rev-parse" "HEAD^{tree}")))
          (rev "swh:1:rev:1111111111111111111111111111111111111111"))
      ;; its directory holds the commit, not what the commit is
      (swh-test--with-archive nil (list root)
        (should-not (nth 3 (assoc "sub/mod" (swh-test--tree dir))))
        (should (member rev swh-test--asked)))
      (swh-test--with-archive nil (list root rev)
        (should (nth 3 (assoc "sub/mod" (swh-test--tree dir))))))))

(defmacro swh-test--with-scan (nodes &rest body)
  "Run BODY in the `swh-scan' report of NODES, as `swh--tree-async' gives them."
  (declare (indent 1))
  `(swh-test--with-file
     (swh-test--with-reports
       (cl-letf (((symbol-function 'swh--tree-async)
                  (lambda (_root callback _failed) (funcall callback (copy-sequence ,nodes)))))
         (swh-scan)
         (with-current-buffer "*swh scan*" ,@body)))))

(ert-deftest swh-test-scan ()
  (let ((swh--daemon-checked (swh-test--daemon 'gone)))
    (swh-test--with-scan '(("." "swh:1:dir:aa" t nil)
                           ("sub/b.txt" "swh:1:cnt:dd" nil nil)
                           ("sub" "swh:1:dir:cc" t nil)
                           ("a.txt" "swh:1:cnt:bb" nil t)
                           ("arch" "swh:1:dir:ee" t t)
                           ("arch/c.txt" "swh:1:cnt:ff" nil t))
      (should (eq major-mode 'swh-scan-mode))
      (should (string-match-p ", via the archive's API)$" (buffer-string)))
      (let ((swh--daemon-checked (swh-test--daemon)))
        (revert-buffer)
        (should (string-match-p ", via swhd)$" (buffer-string))))
      (should (equal "2 of 3 files archived" (swh-test--last-message (revert-buffer))))
      (should (string-match-p "Directories: 3, archived 1 (33%), partially 1 (33%)"
                              (buffer-string)))
      ;; the legend is no heading
      (goto-char (point-min))
      (forward-line 2)
      (should-not (looking-at-p outline-regexp))
      ;; a tree, in path order
      (should (string-match-p
               (concat "^~ \\./  2/3\n"
                       "  ✓ a\\.txt\n"
                       "  ✓ arch/  1/1\n"
                       "    ✓ c\\.txt\n"
                       "  ✗ sub/  0/1\n"
                       "    ✗ b\\.txt\n")
               (buffer-substring-no-properties (point-min) (point-max))))
      ;; what is archived links to the archive, the rest to nothing
      (let (browsed)
        (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq browsed url))))
          (goto-char (point-min))
          (search-forward "a.txt")
          (push-button (match-beginning 0))
          (should (equal (concat swh-archive-url "/swh:1:cnt:bb") browsed))
          (search-forward "sub/")
          (should-not (button-at (match-beginning 0)))
          (search-forward "b.txt")
          (should-not (button-at (match-beginning 0)))))
      ;; archived directories are folded
      (goto-char (point-min))
      (search-forward "c.txt")
      (should (invisible-p (match-beginning 0)))
      (search-forward "b.txt")
      (should-not (invisible-p (match-beginning 0)))
      (outline-show-all)
      (search-backward "c.txt")
      (should-not (invisible-p (point))))))

;;; Provenance

(defmacro swh-test--with-provenance (&rest body)
  "Run BODY with a token and a fake provenance API.
It finds swh:1:cnt:ff and swh:1:dir:ee, answers the bare SWHID for
swh:1:dir:dd, and counts its CALLS."
  (declare (indent 0))
  `(swh-test--with-token
     (let ((swh--daemon-checked (swh-test--daemon 'gone))
           (swh--provenance-memo (make-hash-table :test #'equal))
           (calls 0))
       (cl-letf* ((find (lambda (swhid)
                          (cond ((member swhid '("swh:1:cnt:ff" "swh:1:dir:ee"))
                                 (concat swhid ";origin=https://example.org/lib"
                                         ";anchor=swh:1:rel:11"))
                                ((equal swhid "swh:1:dir:dd") swhid))))
                  ((symbol-function 'swh--api)
                   (lambda (_method path &optional body)
                     (setq calls (1+ calls))
                     (list 200
                           (if body
                               (vconcat (mapcar find (json-parse-string body :array-type 'list)))
                             (and (string-match "whereis/\\(.*\\)/" path)
                                  (funcall find (match-string 1 path))))
                           nil)))
                  ((symbol-function 'swh--retrieve-async) #'swh-test--api-async))
         ,@body))))

(ert-deftest swh-test-provenance ()
  (swh-test--with-provenance
    (should (equal "swh:1:cnt:ff;origin=https://example.org/lib;anchor=swh:1:rel:11"
                   (swh--provenance "swh:1:cnt:ff")))
    (should (null (swh--provenance "swh:1:cnt:00")))
    ;; the bare SWHID names no origin
    (should (null (swh--provenance "swh:1:dir:dd")))
    ;; both answers are remembered
    (swh--provenance "swh:1:cnt:ff")
    (swh--provenance "swh:1:cnt:00")
    (should (= 3 calls))
    (should (equal "https://example.org/lib at swh:1:rel:11"
                   (swh--provenance-text (swh--provenance "swh:1:cnt:ff"))))
    (should (equal "no known provenance" (swh--provenance-text nil)))
    ;; no provenance is forgotten after a while, a provenance is not
    (dolist (s '("swh:1:cnt:ff" "swh:1:cnt:00"))
      (setcar (gethash s swh--provenance-memo) (- (float-time) swh--memo-ttl 1)))
    (swh--provenance "swh:1:cnt:ff")
    (should (= 3 calls))
    (should (null (swh--provenance "swh:1:cnt:00")))
    (should (= 4 calls))))

(ert-deftest swh-test-daemon-provenance ()
  "Through the daemon, with our token, else with the daemon's own."
  (swh-test--with-token
    (let ((swh--daemon-checked (swh-test--daemon))
          (swh--provenance-memo (make-hash-table :test #'equal))
          (json "{\"swhid\":\"swh:1:cnt:ff\",\"provenance\":\"swh:1:cnt:ff;origin=https://example.org/lib\"}"))
      (cl-letf (((symbol-function 'url-retrieve)
                 (swh-test--answering (lambda (_) (swh-test--response-buffer 200 json)))))
        (should (equal "swh:1:cnt:ff;origin=https://example.org/lib"
                       (swh--provenance "swh:1:cnt:ff")))
        (setq json "{\"swhid\":\"swh:1:cnt:00\",\"provenance\":null}")
        (should (null (swh--provenance "swh:1:cnt:00"))))))
  ;; without a token of ours, the daemon's own makes the provenance available
  (let ((swh--provenance-refused nil)
        (swh--daemon-checked nil)
        (authenticated "true"))
    (cl-letf (((symbol-function 'auth-source-pick-first-password) #'ignore)
              ((symbol-function 'url-retrieve)
               (swh-test--answering
                (lambda (url)
                  (swh-test--response-buffer
                   (if (string-suffix-p "/ping" url) 200 502)
                   (if (string-suffix-p "/ping" url)
                       (format "{\"pong\":true,\"version\":%S,\"authenticated\":%s}"
                               swh-daemon-min-version authenticated)
                     "{\"upstream-status\":403,\"error\":\"Forbidden\"}"))))))
      (with-environment-variables (("SWH_API_TOKEN" ""))
        (should (swh--provenance-available-p))
        ;; the daemon's token refused
        (let ((swh--provenance-memo (make-hash-table :test #'equal)))
          (should-error (swh--provenance "swh:1:cnt:ff") :type 'user-error))
        (should (eq 'daemon swh--provenance-refused))
        (should-not (swh--provenance-available-p))
        ;; a daemon without a token either (its ping is remembered:
        ;; forget it)
        (setq swh--provenance-refused nil
              swh--daemon-checked nil
              authenticated "false")
        (should-not (swh--provenance-available-p))
        ;; the error tells that the daemon has none either
        (let ((swh--provenance-memo (make-hash-table :test #'equal)))
          (should (string-match-p "nor swhd"
                                  (swh-test--user-error (swh--provenance "swh:1:cnt:ff")))))
        ;; not through the daemon
        (let ((swh--daemon-checked (swh-test--daemon 'gone))
              (swh--provenance-memo (make-hash-table :test #'equal)))
          (setq authenticated "true")
          (should-not (swh--provenance-available-p))
          (should (equal "The provenance API needs a token (SWH_API_TOKEN or auth-source)"
                         (swh-test--user-error (swh--provenance "swh:1:cnt:ff")))))))))

;; the daemon's /tree brings the provenances it kept: they show at once
(ert-deftest swh-test-scan-daemon-provenances ()
  (swh-test--with-file
    (let ((swh--daemon-checked (swh-test--daemon))
          (swh--provenance-memo (make-hash-table :test #'equal))
          (json "{\".\":{\"swhid\":\"swh:1:dir:aa\",\"dir?\":true,\"archived\":false},
\"arch\":{\"swhid\":\"swh:1:dir:ee\",\"dir?\":true,\"archived\":true,
         \"provenance\":\"swh:1:dir:ee;origin=https://example.org/lib\"},
\"arch/c.txt\":{\"swhid\":\"swh:1:cnt:ff\",\"dir?\":false,\"archived\":true}}"))
      (swh-test--with-reports
        (let ((answers nil))
          (cl-letf (((symbol-function 'swh--retrieve-async)
                     (lambda (_method _url _body _headers callback &optional _timeout)
                       (push callback answers))))
            (swh-scan)
            (with-current-buffer "*swh scan*"
              (should (string-prefix-p "Scanning " (buffer-string)))
              ;; a second scan: the first one's answer comes too late
              (revert-buffer)
              (funcall (cadr answers)
                       (list 200 (json-parse-string "{\"late\":{}}" :object-type 'alist) nil))
              (should (string-prefix-p "Scanning " (buffer-string)))
              (funcall (car answers)
                       (list 200 (json-parse-string json :object-type 'alist :false-object nil) nil))
              (should (equal "swh:1:dir:ee;origin=https://example.org/lib"
                             (swh--memo-get swh--provenance-memo "swh:1:dir:ee")))
              (should (eq 'unknown (swh--memo-get swh--provenance-memo "swh:1:cnt:ff")))
              (should (string-match-p "arch/  1/1  ← https://example.org/lib$" (buffer-string)))
              (should (= 1 (how-many "←" (point-min) (point-max))))
              ;; a failure says so, and does not leave "Scanning"
              (revert-buffer)
              (funcall (car answers) nil)
              (should (string-prefix-p "Scan failed: Cannot reach swhd" (buffer-string))))))))))

(ert-deftest swh-test-scan-provenance ()
  (let ((kill-ring nil))
    (swh-test--with-provenance
      (swh-test--with-scan '(("." "swh:1:dir:aa" t nil)
                             ("arch" "swh:1:dir:ee" t t)
                             ("arch/c.txt" "swh:1:cnt:ff" nil t)
                             ("b.txt" "swh:1:cnt:dd" nil nil))
        (goto-char (point-min))
        ;; on the folded, archived directory
        (search-forward "arch/")
        (should (equal "swh:1:dir:ee;origin=https://example.org/lib;anchor=swh:1:rel:11"
                       (swh-provenance)))
        (should (equal (car kill-ring)
                       (concat swh-archive-url "/" (swh--provenance "swh:1:dir:ee"))))
        (should (string-match-p "arch/  1/1  ← https://example.org/lib at swh:1:rel:11$"
                                (buffer-substring-no-properties (pos-bol) (pos-eol))))
        (should-not (invisible-p (1- (pos-eol))))
        ;; asked again: one annotation, no new request
        (swh-provenance)
        (should (= 1 calls))
        (should (= 1 (how-many "←" (point-min) (point-max))))
        ;; nothing known
        (search-forward "b.txt")
        (should (null (swh-provenance)))
        (should (string-match-p "← no known provenance$"
                                (buffer-substring-no-properties (pos-bol) (pos-eol))))
        ;; a new scan keeps the answers
        (revert-buffer)
        (should (= 2 (how-many "←" (point-min) (point-max))))
        (goto-char (point-min))
        (should-error (swh-provenance) :type 'user-error)))))

;; `W' looks up the visible archived lines: the folded directory, not its file
(ert-deftest swh-test-scan-provenance-visible ()
  (swh-test--with-provenance
    (swh-test--with-scan '(("." "swh:1:dir:aa" t nil)
                           ("arch" "swh:1:dir:ee" t t)
                           ("arch/c.txt" "swh:1:cnt:ff" nil t)
                           ("b.txt" "swh:1:cnt:dd" nil nil)
                           ("d.txt" "swh:1:cnt:00" nil t))
      ;; one request for all
      (let ((calls-before calls))
        (swh-scan-provenance-visible)
        (should (= (1+ calls-before) calls)))
      (clrhash swh--provenance-memo)
      (revert-buffer)
      (setq calls 0)
      ;; beyond the threshold, ask first
      (let ((swh--provenance-batch-size 1)
            (swh--provenance-confirm-threshold 1)
            (asked nil))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (p) (setq asked p) nil)))
          (should-error (swh-scan-provenance-visible) :type 'user-error)
          (should (equal "Send 2 provenance requests? " asked))
          (should (= 0 calls)))
        (cl-letf (((symbol-function 'y-or-n-p) #'always))
          (swh-scan-provenance-visible)))
      (should (= 2 calls))
      (should (string-match-p "arch/  1/1  ← https://example.org/lib at swh:1:rel:11$"
                              (buffer-string)))
      (should (string-match-p "d.txt  ← no known provenance$" (buffer-string)))
      (should (= 2 (how-many "←" (point-min) (point-max))))
      ;; done lines are skipped
      (swh-scan-provenance-visible)
      (should (= 2 calls))
      ;; a refusal keeps the lines done, and calling again resumes
      (clrhash swh--provenance-memo)
      (revert-buffer)
      (cl-letf* ((swh--provenance-batch-size 1)
                 (api (symbol-function 'swh--api))
                 ((symbol-function 'swh--api)
                  (lambda (&rest args)
                    (if (= calls 3) (list 429 nil 5) (apply api args)))))
        (should (string-match-p "rate limit.*(1 of 2 requests done"
                                (swh-test--last-message (swh-scan-provenance-visible)))))
      (should (= 1 (how-many "←" (point-min) (point-max))))
      (swh-scan-provenance-visible)
      (should (= 2 (how-many "←" (point-min) (point-max)))))))

;;; Qualified SWHIDs and citations

(ert-deftest swh-test-swhid-not-archived-offers-save ()
  (swh-test--with-file
    (swh-test--git dir "update-ref" "refs/remotes/origin/main" "HEAD")
    (let ((asked nil) (saved 'none) (kill-ring nil))
      (swh-test--with-daemon 200 (swh-test--swhid-json nil)
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (prompt) (setq asked prompt) t))
                  ((symbol-function 'swh-save)
                   (lambda (&optional origin) (setq saved origin))))
          (swh-swhid)
          (should (string-match-p "since the last push" asked))
          ;; the current repository's: saved its own way, the daemon's
          (should (null saved))
          (should (equal swh-test--qualified (car kill-ring)))
          ;; through a symbolic link, another repository's: by name
          (let ((other (make-temp-file "swh-test" t)))
            (unwind-protect
                (progn
                  (swh-test--git other "init" "-q")
                  (swh-test--git other "remote" "add" "origin" "https://example.org/me/other")
                  (setq default-directory (file-name-as-directory other))
                  (swh-swhid)
                  (should (equal "https://example.org/me/repo" saved)))
              (delete-directory other t))))))))

(ert-deftest swh-test-swhid-not-archived-says-why ()
  (swh-test--with-repo
    (let ((said nil) (asked nil) (kill-ring nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq said (apply #'format fmt args))))
                ((symbol-function 'y-or-n-p) (lambda (prompt) (setq asked prompt) nil)))
        (swh-test--with-archive "https://example.org/me/repo" nil
          (with-temp-buffer
            (setq default-directory dir)
            (pcase-dolist (`(,name ,why)
                           '(("a.txt" "the commit holding it is not pushed")
                             ("new.txt" "Git does not track it")
                             ("sub/b.txt" "it has uncommitted changes")))
              (let ((path (expand-file-name name dir)))
                (when (equal name "new.txt") (with-temp-file path (insert "new\n")))
                (when (equal name "sub/b.txt") (with-temp-file path (insert "changed\n")))
                (setq buffer-file-name path)
                (set-buffer-modified-p nil)
                (swh-swhid)
                (should (string-suffix-p (format "(not archived: %s)" why) said))
                ;; saving would not archive it: not offered
                (should-not asked)))
            ;; the commit holding a.txt is pushed, a later one is not:
            ;; saving would archive a.txt, so that is offered
            (swh-test--git dir "checkout" "-q" "--" "sub/b.txt")
            (swh-test--git dir "update-ref" "refs/remotes/origin/main" "HEAD")
            (with-temp-file (expand-file-name "sub/b.txt" dir) (insert "two\n"))
            (swh-test--git dir "commit" "-q" "-am" "two")
            (setq buffer-file-name (expand-file-name "a.txt" dir))
            (set-buffer-modified-p nil)
            (swh-swhid)
            (should (string-match-p "since the last push" asked))
            (setq asked nil)
            (setq buffer-file-name (expand-file-name "sub/b.txt" dir))
            (set-buffer-modified-p nil)
            (swh-swhid)
            (should (string-suffix-p "(not archived: the commit holding it is not pushed)" said))
            (should-not asked)))))))

(ert-deftest swh-test-swhid-forms ()
  (swh-test--with-file
    (let ((kill-ring nil)
          (bare swh-test--core))
      (swh-test--with-daemon 200 (swh-test--swhid-json t)
        (should (equal swh-test--qualified (swh-swhid ?s)))
        (should (equal swh-test--qualified (car kill-ring)))
        (should (equal bare (swh-swhid ?b)))
        (should (equal (concat "https://archive.softwareheritage.org/" swh-test--qualified)
                       (swh-swhid ?l)))
        (should (equal (car kill-ring) (swh-swhid ?l)))
        ;; without the contextual information: the bare content SWHID
        (should (equal (concat "https://archive.softwareheritage.org/" bare)
                       (swh-swhid ?L)))
        ;; the lines stay, as the archive keeps them
        (cl-letf (((symbol-function 'use-region-p) (lambda () t)))
          (set-mark 5)
          (goto-char 11)
          (should (equal (concat bare ";lines=2-3") (swh-swhid ?b)))
          (should (equal (concat "https://archive.softwareheritage.org/" bare ";lines=2-3")
                         (swh-swhid ?L))))))))

(ert-deftest swh-test-swhid-of-a-directory ()
  (swh-test--with-repo
    (let* ((sub (swh-test--git dir "rev-parse" "HEAD:sub"))
           (rev (swh-test--git dir "rev-parse" "HEAD"))
           (core (concat "swh:1:dir:" sub))
           (context (concat ";origin=https://example.org/me/repo"
                            ";visit=swh:1:snp:" swh-test--snapshot)))
      (with-temp-buffer
        (setq default-directory (expand-file-name "sub/" dir))
        (let ((kill-ring nil))
          (swh-test--with-archive "https://example.org/me/repo" (list core (concat "swh:1:rev:" rev))
            ;; a region does not make lines of a directory
            (cl-letf (((symbol-function 'use-region-p) (lambda () t)))
              (let ((swhid (concat core context ";anchor=swh:1:rev:" rev ";path=/sub")))
                (should (equal swhid (swh-swhid)))
                (should (equal swhid (car kill-ring)))
                (should (equal (concat "https://archive.softwareheritage.org/" swhid)
                               (swh-swhid ?l)))
                (should (equal (concat "https://archive.softwareheritage.org/" core)
                               (swh-swhid ?L)))
                ;; in Dired, the directory at point
                (let ((buffer (dired-noselect dir)))
                  (unwind-protect
                      (with-current-buffer buffer
                        (dired-goto-file (expand-file-name "sub" dir))
                        (should (equal swhid (swh-swhid)))
                        ;; and `swh-provenance' takes the same target
                        (let (asked)
                          (cl-letf (((symbol-function 'swh--provenance)
                                     (lambda (s) (setq asked s) nil)))
                            (swh-provenance))
                          (should (equal core asked))))
                    (kill-buffer buffer))))))
          ;; an edited directory: not in HEAD, no anchor nor path
          (with-temp-file (expand-file-name "sub/b.txt" dir) (insert "new\n"))
          (swh-test--with-archive "https://example.org/me/repo" nil
            (cl-letf (((symbol-function 'y-or-n-p) #'ignore))
              (let ((swhid (swh-swhid)))
                (should (string-prefix-p "swh:1:dir:" swhid))
                (should-not (string-prefix-p core swhid))
                (should (string-suffix-p context swhid))))))))
    ;; the root, unedited: path=/
    (swh-test--git dir "checkout" "--" "sub/b.txt")
    (with-temp-buffer
      (setq default-directory dir)
      (swh-test--with-archive "https://example.org/me/repo"
          (list (concat "swh:1:rev:" (swh-test--git dir "rev-parse" "HEAD")))
        (cl-letf (((symbol-function 'y-or-n-p) #'ignore))
          (should (string-suffix-p ";path=/" (swh-swhid))))))))

(ert-deftest swh-test-swhid-archived-commit ()
  (swh-test--with-file
    (let* ((one (swh-test--git dir "rev-parse" "HEAD"))
           (core (concat "swh:1:cnt:" (swh-test--git dir "rev-parse" "HEAD:a.txt")))
           (anchored (concat core ";origin=https://example.org/me/repo"
                             ";visit=swh:1:snp:" swh-test--snapshot
                             ";anchor=swh:1:rev:" one ";path=/a.txt"))
           (kill-ring nil))
      (swh-test--with-archive "https://example.org/me/repo" (list core (concat "swh:1:rev:" one))
        ;; HEAD, not archived, holds a.txt as ONE does: anchored at ONE
        (with-temp-file (expand-file-name "sub/b.txt" dir) (insert "two\n"))
        (swh-test--git dir "commit" "-q" "-am" "two")
        (should (equal anchored (swh-swhid)))
        ;; a.txt changed since: ?a gives ONE's version, and the
        ;; region's lines, of the working tree, do not apply to it
        (with-temp-file file (insert "new\n"))
        (swh-test--git dir "commit" "-q" "-am" "three")
        (cl-letf (((symbol-function 'swh--file-name) (lambda () file))
                  ((symbol-function 'swh--region-lines) (lambda () "1")))
          (should (equal anchored (swh-swhid ?a))))))))

(ert-deftest swh-test-swhid-without-the-archive ()
  ;; Git alone gives the bare SWHID, and it says so
  (swh-test--with-file
    (let ((kill-ring nil)
          (core (concat "swh:1:cnt:" (swh-test--git dir "rev-parse" "HEAD:a.txt"))))
      (swh-test--with-archive nil nil
        (cl-letf (((symbol-function 'swh--api)
                   (lambda (&rest _) (user-error "Cannot reach the archive"))))
          (should (string-match-p "not checked against the archive: Cannot reach"
                                  (swh-test--last-message
                                    (should (equal core (swh-swhid))))))
          (should (equal core (car kill-ring))))))))

(ert-deftest swh-test-swhid-of-a-linked-directory ()
  ;; a symbolic link to a directory stands for its target: Git holds
  ;; the link's own text, a blob, at the link's path
  (swh-test--with-repo
    (make-symbolic-link "sub" (expand-file-name "link" dir))
    (swh-test--git dir "add" "link")
    (let ((core (concat "swh:1:dir:" (swh-test--git dir "rev-parse" "HEAD:sub"))))
      (with-temp-buffer
        (setq default-directory (expand-file-name "link/" dir))
        (swh-test--with-archive "https://example.org/me/repo" (list core)
          (let ((kill-ring nil))
            (should (string-prefix-p (concat core ";") (swh-swhid))))))))
  ;; to a directory outside any repository: refused, whatever the backend
  (swh-test--with-repo
    (let ((out (make-temp-file "swh-test" t)))
      (unwind-protect
          (progn
            (make-symbolic-link out (expand-file-name "out" dir))
            (with-temp-buffer
              (setq default-directory (expand-file-name "out/" dir))
              (let ((swh--daemon-checked (swh-test--daemon)) (requested nil))
                (cl-letf (((symbol-function 'url-retrieve)
                           (swh-test--answering (lambda (_) (setq requested t) nil))))
                  (should-error (swh-swhid) :type 'user-error)
                  (should-not requested)))))
        (delete-directory out t)))))

(ert-deftest swh-test-swhid-lines-once-saved ()
  (swh-test--with-file
    (let ((kill-ring nil))
      ;; a save hook that moves the lines
      (add-hook 'before-save-hook
                (lambda () (save-excursion (goto-char (point-min)) (insert "top\n")))
                nil t)
      (goto-char (point-max))
      (insert "ddd\n")
      (swh-test--with-archive nil nil
        (cl-letf (((symbol-function 'use-region-p) #'always)
                  ((symbol-function 'y-or-n-p) (lambda (prompt) (string-search "save it" prompt))))
          ;; "bbb": line 2, line 3 once saved
          (set-mark 5)
          (goto-char 9)
          (should (string-suffix-p ";lines=3" (swh-swhid ?b))))))))

;; The archive's answer to a citation request for swh-test--qualified
(defconst swh-test--citation
  "@codefragment{swh-cnt-94a9ed0-L2-L3,\n    title = \"Repo\",\n    swhid = \"x\"\n}\n")

(ert-deftest swh-test-cite ()
  "The archive's BibTeX entry for the file, else a minimal one saying why."
  (swh-test--with-file
    (let ((kill-ring nil))
      (swh-test--with-daemon 200 (swh-test--swhid-json t)
        (swh-test--with-reports
          (cl-letf (((symbol-function 'swh--api)
                     (lambda (&rest _)
                       `(200 ((format . "bibtex") (content . ,swh-test--citation)) nil))))
            (should (equal swh-test--citation (swh-cite)))
            (should (equal swh-test--citation (car kill-ring)))
            (with-current-buffer "*swh cite*"
              (should (derived-mode-p 'bibtex-mode))
              (should (equal swh-test--citation (buffer-string))))))))
    (let ((kill-ring nil)
          (said nil)
          ;; the anchor is not a local commit: no date
          (minimal (concat "@codefragment{swh-cnt-94a9ed0,\n"
                           "    title = {repo},\n"
                           "    url = {https://example.org/me/repo},\n"
                           "    swhid = {" swh-test--qualified "}\n}\n")))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq said (apply #'format fmt args))))
                ((symbol-function 'y-or-n-p) #'ignore))
        (pcase-dolist (`(,archived ,status ,why)
                       ;; not archived: the archive is not asked
                       '((nil nil "not archived: the commit holding it is not pushed")
                         (t 404 "no citation metadata in the archive")
                         (t 400 "no citation metadata in the archive")
                         (t 500 "the archive failed to make one")))
          (swh-test--with-daemon 200 (swh-test--swhid-json archived)
            (cl-letf (((symbol-function 'swh--api)
                       (lambda (&rest _) (if status (list status nil nil) (error "Asked")))))
              (should (equal minimal (swh-swhid ?c)))
              (should (equal minimal (car kill-ring)))
              (should (equal (format "Copied a minimal BibTeX entry (%s)" why) said)))))))))

(ert-deftest swh-test-cite-an-origin ()
  (let ((kill-ring nil)
        (minimal (concat "@software{swh-snp-0000000,\n"
                         "    title = {repo},\n"
                         "    url = {https://example.org/me/repo},\n"
                         "    date = {2026-07-01},\n"
                         "    year = {2026},\n"
                         "    swhid = {swh:1:snp:" swh-test--snapshot
                         ";origin=https://example.org/me/repo}\n}\n")))
    (swh-test--with-reports
      ;; no metadata in the archive: the minimal entry
      (swh-test--with-archive "https://example.org/me/repo" nil
        (should (equal minimal (swh-cite "https://example.org/me/repo")))
        (should (equal minimal (car kill-ring)))
        (should (equal minimal (with-current-buffer "*swh cite*" (buffer-string))))
        (should-error (swh-cite "https://example.org/me/other") :type 'user-error)
        (should-error (swh-cite "/local/repo") :type 'user-error))
      ;; the archive's entry, for the origin it holds
      (let ((swh--daemon-checked (swh-test--daemon 'gone)))
        (cl-letf (((symbol-function 'swh--api)
                   (lambda (_ path &optional _)
                     (if (string-match-p "/visit/latest/" path)
                         `(200 ((snapshot . ,swh-test--snapshot)) nil)
                       `(200 ((content . ,swh-test--citation)) nil))))
                  ((symbol-function 'swh--retrieve-async) #'swh-test--api-async))
          (should (equal swh-test--citation (swh-cite "https://example.org/me/repo"))))))))

;;; Org links

(ert-deftest swh-test-org-follow-and-export ()
  (require 'ox)
  (let ((opened nil))
    (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
      (swh-org-follow (concat (substring swh-test--core 4) ";lines=9-15") nil)
      (should (equal (concat "https://archive.softwareheritage.org/" swh-test--core ";lines=9-15")
                     opened))))
  (should (equal "<a href=\"https://archive.softwareheritage.org/swh:1:cnt:aa;lines=1\">x.el</a>"
                 (swh-org-export "1:cnt:aa;lines=1" "x.el" 'html nil)))
  (should (equal "\\url{https://archive.softwareheritage.org/swh:1:cnt:aa}"
                 (swh-org-export "1:cnt:aa" nil 'latex nil)))
  (should (equal "\\href{https://archive.softwareheritage.org/swh:1:cnt:aa;path=/a\\%20b\\_c.el}{x.el}"
                 (swh-org-export "1:cnt:aa;path=/a%20b_c.el" "x.el" 'latex nil)))
  (should (equal "https://archive.softwareheritage.org/swh:1:cnt:aa"
                 (swh-org-export "1:cnt:aa" nil 'ascii nil))))

(ert-deftest swh-test-org-store-link ()
  (require 'ol)
  (let ((swh-org-store-link t))
    (swh-test--with-file
      (swh-test--with-daemon 200 (swh-test--swhid-json t)
        (let ((org-store-link-plist nil))
          (should (swh-org-store-link))
          (should (equal swh-test--qualified (plist-get org-store-link-plist :link)))
          (should (equal "swh" (plist-get org-store-link-plist :type)))
          (should (equal "a.txt" (plist-get org-store-link-plist :description))))))
    ;; `ask': a swh: link only if the user wants one
    (let ((swh-org-store-link 'ask))
      (swh-test--with-file
        (swh-test--with-daemon 200 (swh-test--swhid-json t)
          (cl-letf (((symbol-function 'y-or-n-p) #'ignore))
            (should (null (swh-org-store-link))))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
            (should (swh-org-store-link))))))
    ;; Dired: the file or directory at point, without lines
    (swh-test--with-repo
      (let ((buf (dired-noselect dir))
            (org-store-link-plist nil)
            asked)
        (unwind-protect
            (with-current-buffer buf
              (dired-goto-file (expand-file-name "sub" dir))
              (cl-letf (((symbol-function 'swh--qualified-swhid)
                         (lambda (path lines &rest _)
                           (setq asked (list path lines))
                           '((swhid . "swh:1:dir:aa")))))
                (should (swh-org-store-link)))
              (should (equal (list (file-truename (expand-file-name "sub" dir)) nil) asked))
              (should (equal "swh:1:dir:aa" (plist-get org-store-link-plist :link)))
              (should (equal "sub" (plist-get org-store-link-plist :description))))
          (kill-buffer buf))))
    ;; daemon down: no link, no error, so other link types get their turn
    (swh-test--with-file
      (swh-test--with-daemon nil nil
        (should (null (swh-org-store-link)))))
    ;; any other failure: no link either
    (swh-test--with-file
      (cl-letf (((symbol-function 'swh--qualified-swhid)
                 (lambda (&rest _) (error "Git hash-object failed"))))
        (should (null (swh-org-store-link)))))
    ;; not a file buffer
    (with-temp-buffer
      (should (null (swh-org-store-link)))))
  ;; opt-in: nothing stored by default
  (swh-test--with-file
    (swh-test--with-daemon 200 (swh-test--swhid-json t)
      (should (null (swh-org-store-link))))))

;;; swh-mode

(ert-deftest swh-test-mode-lighter ()
  (swh-test--with-file
    (let ((answer '((swhid . "swh:1:cnt:aa") (archived . t))))
      (cl-letf (((symbol-function 'swh--file-status-async)
                 (lambda (_file callback failed &rest _)
                   (if answer (funcall callback answer) (funcall failed "No answer")))))
        ;; `format-mode-line' renders nothing in batch: check the
        ;; pieces the construct is made of.
        (swh-mode 1)
        (should (eq 'archived swh--status))
        (should (equal " SWH✓" (alist-get swh--status swh-mode-lighters)))
        (set-window-buffer (selected-window) (current-buffer))
        (should (equal "swh:1:cnt:aa\nmouse-1: Copy the qualified SWHID"
                       (swh--mode-line-help-echo (selected-window))))
        (setq answer '((swhid . "swh:1:cnt:aa") (archived . nil)))
        (cl-letf (((symbol-function 'swh--origin-async)
                   (lambda (_root callback _) (funcall callback '((archived . nil))))))
          (swh--refresh)
          (should (eq 'not-archived swh--status)))
        (cl-letf (((symbol-function 'swh--origin-async)
                   (lambda (_root callback _) (funcall callback '((archived . t))))))
          (swh--refresh)
          (should (eq 'origin-archived swh--status))
          (should (equal " SWH~" (alist-get swh--status swh-mode-lighters))))
        ;; a late origin answer does not override a newer file status
        (let (late)
          (cl-letf (((symbol-function 'swh--origin-async)
                     (lambda (_root callback _) (setq late callback))))
            (swh--refresh)
            (setq answer '((swhid . "swh:1:cnt:bb") (archived . t)))
            (swh--refresh)
            (funcall late '((archived . t)))
            (should (eq 'archived swh--status))))
        ;; answers arriving out of order: the latest check wins
        (let (callbacks)
          (cl-letf (((symbol-function 'swh--file-status-async)
                     (lambda (_file callback &rest _) (push callback callbacks))))
            (swh--refresh)
            (swh--refresh)
            (funcall (car callbacks) '((swhid . "swh:1:cnt:new") (archived . t)))
            (funcall (cadr callbacks) '((swhid . "swh:1:cnt:old") (archived . nil)))
            (should (eq 'archived swh--status))
            (should (equal "swh:1:cnt:new" swh--swhid))))
        (setq answer nil)
        (swh--refresh)
        (should (eq 'error swh--status))
        (should (equal " SWH!" (alist-get swh--status swh-mode-lighters)))
        (swh-mode -1)
        (should (null swh--status))))))

;; `dired' needs a real buffer, and its readin hook turns the mode on
(ert-deftest swh-test-mode-dired ()
  (swh-test--with-repo
    (let* ((swh--daemon-checked (swh-test--daemon 'gone))
           (swh--dired-check-interval 0)
           (head (concat "swh:1:dir:" (swh-test--git dir "rev-parse" "HEAD^{tree}")))
           (origin '((archived . t)))
           (archived t)
           (asked nil)
           (copies 0))
      (cl-letf (((symbol-function 'swh--origin-async)
                 (lambda (_root callback failed)
                   (if (eq origin 'error) (funcall failed "No answer") (funcall callback origin))))
                ((symbol-function 'swh--known-async)
                 (lambda (swhid callback failed)
                   (push swhid asked)
                   (if (eq archived 'error) (funcall failed "No answer") (funcall callback archived))))
                ((symbol-function 'swh--with-index-copy)
                 (let ((copy (symbol-function 'swh--with-index-copy)))
                   (lambda (&rest args) (setq copies (1+ copies)) (apply copy args)))))
        (let ((buffer (dired-noselect dir)))
          (unwind-protect
              (with-current-buffer buffer
                (swh--turn-on)
                (should swh-mode)
                (should (eq 'archived swh--status))
                ;; unchanged: HEAD's tree, from a single copy of the index
                (should (equal head swh--swhid))
                (should (equal (list head) asked))
                (should (= 1 copies))
                ;; a read too soon after a check waits for its own
                (let ((swh--dired-check-interval 3600) (timers timer-list))
                  (revert-buffer)
                  (should swh--dired-timer)
                  (should (= 1 copies))
                  (cancel-timer swh--dired-timer)
                  (setq swh--dired-timer nil)
                  (should (equal timers timer-list)))
                ;; not archived, or the archive cannot tell
                (setq archived nil)
                (swh--refresh)
                (should (eq 'origin-archived swh--status))
                (setq archived 'error)
                (swh--refresh)
                (should (eq 'error swh--status))
                (setq archived t)
                ;; a tracked file modified: the tree is not asked about
                (with-temp-file file (insert "edited\n"))
                (setq asked nil)
                (swh--refresh)
                (should (eq 'origin-archived swh--status))
                (should-not asked)
                (swh-test--git dir "checkout" "-q" "a.txt")
                ;; an untracked file: the tree is not asked about
                (with-temp-file (expand-file-name "new.txt" dir) (insert "n\n"))
                (revert-buffer)
                (should (eq 'origin-archived swh--status))
                (should-not swh--swhid)
                (should-not asked)
                (delete-file (expand-file-name "new.txt" dir))
                ;; through the daemon: its answer, from its cache
                (let ((swh--daemon-checked (swh-test--daemon)))
                  (cl-letf (((symbol-function 'swh--file-status-async)
                             (lambda (path callback &rest _)
                               (push path asked)
                               (funcall callback '((swhid . "swh:1:dir:aa") (archived . t))))))
                    (swh--refresh)
                    (should (eq 'archived swh--status))
                    (should (equal "swh:1:dir:aa" swh--swhid))
                    (should (equal (list (directory-file-name (file-truename dir))) asked))))
                (setq origin '((archived . nil)))
                (swh--refresh)
                (should (eq 'not-archived swh--status))
                ;; no remote
                (setq origin nil)
                (swh--refresh)
                (should (eq 'not-archived swh--status))
                ;; the archive cannot tell
                (setq origin 'error)
                (swh--refresh)
                (should (eq 'error swh--status))
                ;; git failing: an error status, not a check left pending
                (setq origin '((archived . t)))
                (cl-letf (((symbol-function 'swh--untracked-p) (lambda (_) (error "Git failed"))))
                  (swh--refresh))
                (should (eq 'error swh--status)))
            (kill-buffer buffer)))))))

(ert-deftest swh-test-global-mode ()
  (swh-test--with-repo
    (let ((swh--daemon-checked (swh-test--daemon 'gone))
          (checks 0))
      (cl-letf (((symbol-function 'swh--file-status-async)
                 ;; an answer that has not come back yet
                 (lambda (&rest _) (setq checks (1+ checks)))))
        (unwind-protect
            (progn
              (global-swh-mode 1)
              ;; `normal-mode' turns it on twice: one check
              (let ((buffer (find-file-noselect file)))
                (should (buffer-local-value 'swh-mode buffer))
                (should (= 1 checks))
                (kill-buffer buffer))
              ;; not in Git's own files
              (with-temp-file (expand-file-name ".git/COMMIT_EDITMSG" dir) (insert "msg\n"))
              (let ((buffer (find-file-noselect (expand-file-name ".git/COMMIT_EDITMSG" dir))))
                (should-not (buffer-local-value 'swh-mode buffer))
                (kill-buffer buffer))
              ;; a file not saved yet: nothing to tell
              (let ((buffer (find-file-noselect (expand-file-name "new.txt" dir))))
                (should-not (buffer-local-value 'swh--status buffer))
                (kill-buffer buffer))
              (should (= 1 checks)))
          (global-swh-mode -1)))))
  ;; a directory outside a repository has no SWHID: no status, no error
  (let* ((dir (make-temp-file "swh-test" t))
         (buffer (dired-noselect dir)))
    (unwind-protect
        (with-current-buffer buffer
          (swh-mode 1)
          (should-not swh--status)
          (revert-buffer)
          (should-not swh--status))
      (kill-buffer buffer)
      (delete-directory dir t))))

(ert-deftest swh-test-mode-direct-file-status ()
  "The direct backend asks the archive once per content, then remembers."
  (swh-test--with-file
    (let ((swh--daemon-checked (swh-test--daemon 'gone))
          (swh--known-memo (make-hash-table :test #'equal))
          (posts 0)
          (result 'none))
      (cl-letf (((symbol-function 'swh--retrieve-async)
                 (lambda (_method _url body _headers callback)
                   (setq posts (1+ posts))
                   (let ((asked (aref (json-parse-string body) 0)))
                     (funcall callback
                              (list 200 (list (cons (intern asked) '((known . t)))) nil))))))
        (swh--file-status-async file (lambda (r) (setq result r)) #'ignore)
        (should (eq t (alist-get 'archived result)))
        (should (string-prefix-p "swh:1:cnt:" (alist-get 'swhid result)))
        (setq result 'none)
        (swh--file-status-async file (lambda (r) (setq result r)) #'ignore)
        (should (eq t (alist-get 'archived result)))
        (should (= 1 posts))
        ;; no public origin: not archived, whatever the archive would say
        (swh-test--git dir "remote" "set-url" "origin" "/srv/git/repo")
        (swh-mode 1)
        (should (eq 'not-archived swh--status))
        (should (string-prefix-p "swh:1:cnt:" swh--swhid))
        (should (= 1 posts))))))

(provide 'swh-test)
;;; swh-test.el ends here
