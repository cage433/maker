;;; maker-mode.el - Functions for discovering the current maker project
;;; based heavily on https://github.com/hvesalai/sbt-mode/
;;; TODO publish on MELPA

(require 'compile)
(require 'comint)
(require 'dash)
(require 's)

(defcustom maker:program-name "maker"
  "Program invoked by the `maker:run-maker' command."
  :type 'string
  :group 'maker)

(defcustom maker:default-command "testCompile"
  "The default command to run with maker:command."
  :type 'string
  :group 'maker)

(defcustom maker:clear-buffer-before-command t
  "Whether to clear the maker buffer before running a command."
  :type 'boolean
  :group 'maker)

(defface maker:error
  '((t :inherit error))
  "Face for displaying some maker error messages"
  :group 'maker)

(defface maker:info
  '((t :inherit success))
  "A face for displaying some maker info messages"
  :group 'maker)

(defface maker:warning
  '((t :inherit warning))
  "A face for displaying some maker warning messages"
  :group 'maker)

(defvar maker:error-face 'maker:error)
(defvar maker:info-face 'maker:info)
(defvar maker:warning-face 'maker:warning)

(defvar-local maker:previous-command maker:default-command)

(defvar maker:command-history-temp nil)

(defgroup maker nil
  "Support for maker build REPL."
  :group 'maker
  :prefix "maker:")

;;;
;;; user commands
;;;

;;;###autoload
(defun maker-start ()
  "Start maker"
  (interactive) (maker:run-maker nil t))

(defun maker-clear ()
  "Clear the current maker buffer and send RET to maker to re-display the prompt"
  (interactive) (maker:clear))

(defun maker-kill ()
  "Tell maker to kill the currently executing process (main method or test)"
  (interactive)
  (with-current-buffer (maker:buffer-name)
    (display-buffer (current-buffer))
    (comint-send-string (current-buffer) (concat "" "\n"))))


(defun maker-completion-at-point ()
  "Complete the command at point. Works both in maker shell and
scala console."
 (interactive) (maker:completion-at-point))

;;;###autoload
(defun maker-command (command)
  "Send a command to the maker process of the current buffer's maker project.
Prompts for the command to send when in interactive mode.

This command does the following:
  - displays the buffer without moving focus to it
  - erases the buffer
  - forgets about compilation errors"
  (interactive
   (progn
     (setq maker:command-history-temp
           (ignore-errors (with-current-buffer (maker:buffer-name) (ring-elements comint-input-ring))))

     (list (completing-read (format "Command to run (default %s): " (maker:get-previous-command))
                            (completion-table-dynamic 'maker:get-maker-completions)
                            nil nil nil 'maker:command-history-temp (maker:get-previous-command)))))
  (maker:command command))

;;;###autoload
(defun maker-run-previous-command ()
  "Repeat the command that was previously executed (or run the
maker:default-command, if no other command has yet been run)."
  (interactive)
  (maker:command (maker:get-previous-command)))

(defun maker:clear (&optional buffer)
  "Clear (erase) the MAKER buffer."
  (with-current-buffer (or buffer (maker:buffer-name))
    (let ((proc (get-buffer-process (current-buffer)))
          (inhibit-read-only t))
      (ignore-errors (compilation-forget-errors))
      (erase-buffer)
      (ignore-errors (comint-send-string proc (kbd "C-l"))))))

(defun maker:command (command)
  (unless command (error "Please specify a command"))

  (when (not (comint-check-proc (maker:buffer-name)))
    (maker:run-maker))

  (with-current-buffer (maker:buffer-name)
    (display-buffer (current-buffer))
    (if maker:clear-buffer-before-command
        (maker:clear (current-buffer))
      (ignore-errors (compilation-forget-errors)))
    (comint-send-string (current-buffer) (concat command "\n"))
    (setq maker:previous-command command)))

(defun maker:get-previous-command ()
  (if (not (get-buffer (maker:buffer-name)))
      maker:default-command
    (with-current-buffer (maker:buffer-name)
      maker:previous-command)))

(defun maker:run-maker (&optional kill-existing-p pop-p)
  "Start or restarts (if kill-existing-p is non-NIL) maker in a
buffer called *maker*projectdir."
  (let* ((project-root (maker:find-root))
         (maker-command-line (split-string maker:program-name " "))
	 (maker-bin (or (executable-find (nth 0 maker-command-line))
			(executable-find (concat project-root "/bin/" (nth 0 maker-command-line)))
			(executable-find (concat project-root "/bin/maker.sh"))))
         (buffer-name (maker:buffer-name))
         (inhibit-read-only 1))
    (when (null project-root)
      (error "Could not find project root, type `C-h f maker:find-root` for help."))

    (unless maker-bin
      (error "Could not find %s in %s/bin or on PATH. Please customise the maker:program-name variable." maker-bin project-root))

    ;; kill existing maker
    (when (and kill-existing-p (get-buffer buffer-name))
      (maker:clear buffer-name)
      (kill-buffer buffer-name))

    ;; start new maker
    (with-current-buffer (get-buffer-create buffer-name)
      (when pop-p (pop-to-buffer-same-window (current-buffer)))
      (unless (comint-check-proc (current-buffer))
        (unless (derived-mode-p 'maker-mode) (maker-mode))
        (cd project-root)
        (buffer-disable-undo)
        (message "Starting maker in buffer %s " buffer-name)
        ;;(erase-buffer)

        ;; insert a string to buffer so that process mark comes after
        ;; compilation-messages-start mark.
        (insert (concat "Running " maker:program-name "\n"))
        (goto-char (point-min))
        (ignore-errors (compilation-forget-errors))
        (comint-exec (current-buffer) buffer-name maker-bin nil (cdr maker-command-line)))
      (current-buffer))))

(defun maker:initialize-for-compilation-mode ()
  ;; can't be nil, so set to a regexp that will never match
  (setq-local compilation-directory-matcher '("\\`a\\`"))
  (setq-local
   compilation-error-regexp-alist
   `((,(rx line-start
           (zero-or-more ".")
           ?[ (or (group "error") (group "warn") ) ?]
           " " (group (zero-or-one letter ":") (1+ (not (any ": "))))
           ?: (group (1+ digit)) ?:)
      3 4 nil (2 . nil) 3 )))
  (setq-local
   compilation-mode-font-lock-keywords
   '(("^\\.*\\[\\(error\\)\\]"
      (1 maker:error-face))
     ("^\\.*\\[\\(warn\\)\\]"
      (1 maker:warning-face))
     ("^\\.*\\[\\(success\\)\\]"
      (1 maker:info-face))))
  (compilation-setup t))

(defvar maker:mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map
                       (make-composed-keymap compilation-shell-minor-mode-map
                                             comint-mode-map))
    (define-key map (kbd "TAB") 'maker-completion-at-point)
    (define-key map (kbd "C-c l") 'maker-clear)
    (define-key map (kbd "C-]") 'maker-kill)
    map)
  "Basic mode map for `maker-start'")

(define-derived-mode maker-mode comint-mode "maker"
  "Major mode for `maker-start'.

\\{maker:mode-map}"
  (use-local-map maker:mode-map)
  (ignore-errors (scala-mode:set-scala-syntax-mode))
  (add-hook 'maker-mode-hook 'maker:initialize-for-comint-mode)
  (add-hook 'maker-mode-hook 'maker:initialize-for-compilation-mode))

(defcustom maker:maker-prompt-regexp "^scala>[ ]*"
  "A regular expression to match maker REPL prompt"
  :type 'string
  :group 'maker)

(defcustom maker:ansi-support 'filter
  "See `ansi-color-for-comint-mode' in `ansi-color.el'"
  :type '(choice (const :tag "Do nothing" nil)
                 (const :tag "Filter" filter)
                 (const :tag "Translate" t))
  :group 'maker)

(defun maker:initialize-for-comint-mode ()
  (maker:require-buffer)
  (when (derived-mode-p 'comint-mode)
    (setq comint-process-echoes t)
    (setq comint-scroll-to-bottom-on-output t)
    (setq comint-prompt-regexp maker:maker-prompt-regexp)
    (setq-local comint-use-prompt-regexp t)
    (setq-local comint-prompt-read-only t)
    (setq-local comint-buffer-maximum-size 4096)
    (setq-local comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom))
    (setq ansi-color-for-comint-mode maker:ansi-support)
    (setq comint-input-sender 'maker:input-sender)
    (add-hook 'comint-output-filter-functions 'maker:ansi-filter)))

(defconst maker:ansi-clear-line "M\\[2K"
  "'Ansi code' sequence sent by JLine to clear the previous line.")

(defun maker:input-sender (proc string)
  (sit-for 0) ; the purpose of this sit-for 0 is to let emacs show the
              ; newline that the user just inserted. Without this
              ; sometimes emacs will not give the user any feedback
              ; that the input has been sent.
  (comint-simple-send proc string))

(defun maker:ansi-filter (input)
  (when (maker:mode-p)
    (save-excursion
      ;; go to start of first line just inserted
      (comint-goto-process-mark)
      (goto-char (max (point-min) (- (point) (string-width input))))
      (forward-line 0)
      (while (re-search-forward maker:ansi-clear-line nil t)
        ;; delete the ansi code and the previous line
        (delete-region (save-excursion (forward-line -1) (point)) (match-end 0))))
    input))


;;;
;;; Completion functionality
;;;

(defun maker:get-maker-completions-regex (input)
  ;; TODO: ignore "words" with escape characters. This can
  ;;       sometimes produce junk because escape characters
  ;;       are partially interpreted as results.
  "\\([[:alnum:]]++\\)")

(defun maker:get-maker-completions (input)
  (maker:require-buffer)
  (when (not (comint-check-proc (current-buffer)))
    (error "maker is not running in buffer %s" (current-buffer)))
  (when (save-excursion
	  (comint-goto-process-mark)
	  (beginning-of-line)
	  (not (looking-at-p maker:maker-prompt-regexp)))
    (error "maker is not ready (no prompt found)"))
  (message "Querying maker for completions for %s..." input)
  (when (not (or (null input) (string-match "^\\s *$" input)))
    ;; jiggery pokery to workaround comint appending a newline to
    ;; commands. We override process-send-string to temporarily chomp
    (fset 'old-process-send-string (symbol-function 'process-send-string))
    (flet ((process-send-string (b x) (old-process-send-string b (s-chomp x))))
      (let* ((completer (concat input "\t"))
	     (matcher (maker:get-maker-completions-regex input))
	     (completions (comint-redirect-results-list completer matcher 1)))
	(comint-redirect-send-command-to-process
	 ;; jline puts the search back on the prompt, so we have to clear it
	 "\n" " *Comint Redirect Work Buffer*"
	 (get-buffer-process (current-buffer)) nil 1)
	(message nil)
	;; we need to prefix member completions: the REPL doesn't echo
	;; the whole replacement string.
	(mapcar (lambda(x)
		  (if (s-ends-with? "." input)
		      (concat input x)
		    (let* ((common (s-shared-start input x))
			   (uncommon (s-chop-prefix common x)))
		      (if (s-blank? uncommon) ""
			(concat common uncommon)))))
		(-distinct completions))))))

(defun maker:completion-at-point ()
  (maker:require-buffer)
  (let ((point (point))
        (beg (save-excursion (comint-goto-process-mark)
                             (point)))
        (end (max (point) (save-excursion (end-of-line)(skip-chars-backward " \t\n\r")(point))))
        mid)
    (goto-char beg)
    (beginning-of-line)
    (if (> beg end)
        (comint-goto-process-mark)
      (cond ((looking-at-p maker:maker-prompt-regexp)
             (goto-char point)
             (let* ((completions (maker:get-maker-completions (buffer-substring beg end))))
               (completion-in-region beg end completions `(lambda (s) (> (string-width s) 0)))))
            (t
             (goto-char point)
             "No maker or scala prompt found before process mark")))))


(defvar maker:buffer-project-root nil)

(defun maker:find-root-impl (name-or-pred &optional dir best-root)
  (when (null dir) (setq dir default-directory))
  (let ((parent (if (string-match locate-dominating-stop-dir-regexp dir) nil
                  (file-name-directory (directory-file-name dir)))))
    (cond ((or (null parent)
               (equal dir parent))
           (and best-root (abbreviate-file-name best-root)))
          ((if (stringp name-or-pred)
               (file-exists-p (expand-file-name name-or-pred dir))
             (funcall name-or-pred dir))
	   dir)
          ('t
           (maker:find-root-impl name-or-pred parent best-root)))))

(defun maker:find-root ()
  "Starting from the current default-directory, find the top-most
parent directory that is a maker root. A maker root directory is
identified by the following rules:

  - a directory containing a 'bin/maker' or 'bin/maker.sh' in it.
"
  (or maker:buffer-project-root
      (let ((root (or (maker:find-root-impl "bin/maker.sh")
		      (maker:find-root-impl "bin/maker"))))
        (when root
          (setq-local maker:buffer-project-root root)))))

(defun maker:buffer-in-project-function (root)
  "Return a lambda that returns 't if the current buffer is in the ROOT project."
  `(lambda () (equal (maker:find-root) ,root)))


(defcustom maker:buffer-name-base "*maker*"
  "Buffer name for maker"
  :type 'string
  :group 'maker)

(defun maker:buffer-name ()
  "Return the buffer name for running maker."
  (format "%s<%s>"
          maker:buffer-name-base
          (expand-file-name (maker:find-root))))

(defun maker:require-buffer ()
  "Throw error if the current buffer is not a maker-buffer"
  (unless (derived-mode-p 'maker-mode) 
    (error "Current buffer %s is not a maker-buffer" (current-buffer))))

(defun maker:mode-p ()
  "Return non-nil if the current buffer is maker-buffer"
  (derived-mode-p 'maker-mode))


(provide 'maker-mode)

;; Local Variables:
;; no-byte-compile: t
;; End:
;;; maker-mode.el ends here
