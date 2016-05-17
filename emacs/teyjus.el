;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Teyjus Lambda Prolog mode to go with COMINT package by Kamal Aboul-Hosn
;;; Reduced for Teyjus 2.0 by Andrew Gacek
;;; The Comint derived teyjus-inferior-mode added by Shon Feder
;;;
;;; This package handles the following features:
;;;   - Syntax highlighting for .mod and .sig files
;;;   - Running an inferior tjcc process within Emacs
;;;   - Teyjus support for emacs compilation mode (M-x compile)
;;;
;;; Commands have been added to make the use of Teyjus easier:
;;;
;;;  Command                 Shortcut     Function
;;;  ----------              --------     --------
;;;  M-x teyjus-compile      C-c C-c      Compile a module
;;;  M-x teyjus-next-error   C-c C-n      Find the next compilation error
;;;  M-x teyjus-prev-error   C-c C-p      Find the previous compilation error
;;;  M-x teyjus-run                       Start tjsim as an inferior process
;;;  M-x teyjus-load-buffer  C-c C-f      Save, compile, link, load current file into inferior process.
;;;  M-x teyjus-path-add-current-buffer   Add current buffer's directory to $TJPATH, making modules available for `tjsim' and `tjlink'.
;;;  M-x teyjus-path-add-remove-buffer    Remove current buffer's directory from $TJPATH.

;;; These commands are also available via the Teyjus menu
;;;
;;;
;;; To use the Teyjus Emacs interface, add this line to your .emacs file:
;;;
;;;   (load "~/teyjus/emacs/teyjus.el")
;;;
;;; Adjust the path as neccesary. If tjcc, tjlink, and tjsim are not in your
;;; $PATH then you should set the variables to the precise location. For example,
;;;
;;;   (setq tjcc   "~/teyjus/tjcc")
;;;   (setq tjlink "~/teyjus/tjlink")
;;;   (setq tjsim  "~/teyjus/tjsim")
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'easymenu)
(require 'comint)
(require 'font-lock)

(provide 'teyjus)

(setq auto-mode-alist (cons '("\\.mod\\'" . teyjus-edit-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sig\\'" . teyjus-edit-mode) auto-mode-alist))

(defvar tjcc "tjcc"
  "The path to the Teyjus compiler executable.

By default, it assumes the binary `tjcc' is available as a command in your $PATH.
Otherwise, set this variable to the path where your `tjcc' is located.")

(defvar tjlink "tjlink"
  "The path to the Teyjus linker executable.

By default, it assumes the binary `tjlink' is available as a command in your $PATH.
Otherwise, set this variable to the path where your `tjlink' is located.")

(defvar tjsim "tjsim"
  "The path to the Teyjus simulator executable.

By default, it assumes the binary `tjsim' is available as a command in your $PATH.
Otherwise, set this variable to the path where your `tjsim' is located.")

(defvar tjpath (if (getenv "TJPATH")
                   (split-string  (getenv "TJPATH") ":")
                 nil)
  "Initialize `tjpath' with the environment variable TJPATH.

`tjpath' as a list of the paths in the environment variable. This variable is used
as intermediary for updating the $TJPATH environment variable.
`tjpath' is initialized as an empty list, in the event that $TJPATH is not set.")

(defvar teyjus-edit-mode-map nil)
(cond ((not teyjus-edit-mode-map)
       (setq teyjus-edit-mode-map (make-sparse-keymap))
       (define-key teyjus-edit-mode-map "\C-c\C-c" 'teyjus-compile)
       (define-key teyjus-edit-mode-map "\C-c\C-p" 'teyjus-prev-error)
       (define-key teyjus-edit-mode-map "\C-c\C-n" 'teyjus-next-error)
       (define-key teyjus-edit-mode-map "\C-c\C-f" 'teyjus-load-buffer)))

(defvar teyjus-mode-hook '()
  "*Hook for customizing teyjus mode")

;; Define the menu for teyjus
(defvar teyjus-menu
  '("Teyjus"
    ["Compile current module"           teyjus-compile     t]
    ["Show next compilation error"      teyjus-next-error  t]
    ["Show previous compilation error"  teyjus-prev-error  t]
    ["-----" nil t]
    ["Start simulator"                  teyjus-run-buffer  t]
    ["Load current module in simulator" teyjus-load-buffer t]
    ["-----" nil t]
    ["Add buffer directory to path"       teyjus-path-add-current-buffer t]
    ["Remove buffer directory from path"  teyjus-path-add-remove-buffer  t]
    ))

(defun teyjus-edit-mode ()
  "Mode for editing Lambda Prolog Files"
  (interactive)
  (kill-all-local-variables)
  (easy-menu-define nil teyjus-edit-mode-map "Teyjus menu" teyjus-menu)
  (easy-menu-add teyjus-menu)
  (set-syntax-table teyjus-mode-syntax-table)
  (use-local-map teyjus-edit-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(teyjus-font-lock-keywords))
  (turn-on-font-lock)
  (set (make-local-variable 'comment-start) "%")
  (setq major-mode 'teyjus-edit-mode)
  (setq mode-name "Teyjus")
  (run-hooks 'teyjus-mode-hook))

;; Location of last error message
(defvar teyjus-last-error nil)

;; Regular expression for errors/Warnings
(defvar teyjus-error-regexp
  "^\\(.+\\)(\\([0-9]+\\),\\([0-9]+\\)).*\\(?:Error\\|\\(Warning\\)\\)")

;; Create a regexp for use with compilation mode (M-x compile)
(require 'compile)
(cond
 ((boundp 'compilation-error-regexp-systems-list)
  ;; xemacs21
  (add-to-list 'compilation-error-regexp-alist-alist
               (list 'teyjus (list teyjus-error-regexp 1 2 3)))
  (compilation-build-compilation-error-regexp-alist))

 ((boundp 'compilation-error-regexp-alist-alist)
  ;; emacs22
  (add-to-list 'compilation-error-regexp-alist-alist
               (list 'teyjus teyjus-error-regexp 1 2 3 '(4)))
  (add-to-list 'compilation-error-regexp-alist 'teyjus))

 (t
  ;; emacs21
  (add-to-list 'compilation-error-regexp-alist
               (list teyjus-error-regexp 1 2 3))))

;; Function to find the next error in the *tjcc* buffer,
;; split the window, and goto the line in the program with
;; the error.
(defun teyjus-next-error ()
  (interactive)
  (set-buffer "*tjcc*")
  (condition-case nil
      (progn
        (if teyjus-last-error
            (progn (goto-char teyjus-last-error)
                   (end-of-line))
          (goto-char (point-min)))
        (re-search-forward teyjus-error-regexp)
        (setq teyjus-last-error (point))
        (teyjus-show-error))
      (error (message "No more errors"))))

;; Function to find the previous error in the *tjcc* buffer,
;; split the window, and goto the line in the program with
;; the error.
(defun teyjus-prev-error ()
  (interactive)
  (set-buffer "*tjcc*")
  (condition-case ()
      (progn
        (if teyjus-last-error
            (progn (goto-char teyjus-last-error)
                   (beginning-of-line)))
        (re-search-backward teyjus-error-regexp)
        (setq teyjus-last-error (point))
        (teyjus-show-error))
    (error (message "No more errors"))))

(defun teyjus-show-error ()
  (interactive)
  (delete-other-windows)
  (let ((filename
         (buffer-substring (match-beginning 1) (match-end 1)))
        (line-number
         (string-to-int
          (buffer-substring (match-beginning 2) (match-end 2))))
        (column-number
         (string-to-int
          (buffer-substring (match-beginning 3) (match-end 3)))))
    (switch-to-buffer (find-file-noselect filename))
    (goto-line line-number)
    (goto-char (+ (point) column-number))
    (set-window-buffer (split-window) "*tjcc*")))

(defun teyjus-compile ()
  "Compile the current buffer with tjcc"
  (interactive)
  (let ((module (file-name-nondirectory (buffer-file-name (current-buffer)))))
    (if (get-buffer "*tjcc*") (kill-buffer "*tjcc*"))
    (setq teyjus-last-error nil)
    (delete-other-windows)
    (set-window-buffer (split-window)
                       (make-comint "tjcc" tjcc nil module))))

(defvar teyjus-mode-syntax-table
  (let ((table (make-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?/ ". 14n" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?+ "w" table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?= "w" table)
    (modify-syntax-entry ?% "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?< "w" table)
    (modify-syntax-entry ?> "w" table)
    (modify-syntax-entry ?\' "w" table)
    table))

(defun make-regex (&rest args)
  (regexp-opt args 'words))

(defvar teyjus-font-lock-keywords
  (list
   ;; Variables
   (cons "\\<[A-Z][A-Za-z0-9'/]*\\>" font-lock-variable-name-face)

   ;; Cut
   (cons "!" font-lock-warning-face)

   ;; Headers
   (cons (make-regex "module" "sig") font-lock-function-name-face)

   ;; Builtin functions
   (cons (make-regex "abs" "sqrt" "sin" "cos" "arctan" "ln" "floor" "ceil"
                     "truncate" "rabs" "size" "chr" "string_to_int" "substring"
                     "int_to_string" "real_to_string" "std_in" "std_out"
                     "std_err" "is" "open_in" "open_out" "open_string"
                     "open_append" "close_in" "close_out" "term_to_string"
                     "string_to_term" "input" "output" "input_line" "lookahead"
                     "eof" "flush" "print" "read" "printterm" "readterm" "nil"
                     "not" "true" "fail" "pi" "sigma" "type" "kind"
                     "infix" "infixl" "infixr" "prefix" "prefixr"
                     "postfix" "postfixl" "local" "exportdef" "useonly"
                     "accum_sig" "import" "accumulate")
         font-lock-keyword-face)

   ;; Types
   (cons (make-regex  "int" "real" "string" "list" "out_stream" "in_stream" "o")
         font-lock-type-face)))


;; Dealing with TJPATH
;; This code is mainly intended to circumvent difficulties relating to
;; the inability to pass file paths to `tjsim' and `tjlink'.
;; However, it is probably useful in its own right, as a way of
;; loading and testing out several dependent modules.

(defun teyjus-path-update ()
  "Set the environment variable $TJPATH to the paths in `tjpath'."
  (setenv "TJPATH" (mapconcat 'identity tjpath ":")))

(defun teyjus-path-add-directory (dir)
  "Add `dir' to `tjpath' and update $TJPATH."
  (interactive)
  (add-to-list 'tjpath dir 't)
  (teyjus-path-update))

(defun teyjus-path-remove-directory (dir)
  "Remove `dir' from `tjpath' and update $TJPATH"
  (interactive)
  (setq tjpath (delete dir tjpath))
  (teyjus-path-update))

(defun teyjus-path-add-current-buffer ()
  "Add directory of current buffer's file to `tjpath' and update $TJPATH"
  (interactive)
  (teyjus-path-add-directory (file-name-directory (buffer-file-name)))
  (teyjus-path-update))

(defun teyjus-path-remove-current-buffer ()
  "Remove directory of current buffer's file from `tjpath', update $TJPATH"
  (interactive)
  (teyjus-path-remove-directory (file-name-directory (buffer-file-name)))
  (teyjus-path-update))

;; teyjus-inferior-mode
;; Implemented with reference to
;; https://www.masteringemacs.org/article/comint-writing-command-interpreter


;; General utility functions
;;

(defun get-buffer-size (buffer-name)
  "get-buffer-size: string -> int

Given a string naming a buffer, buffer-name it returns the value of
(buffer-size (get-buffer buffer-name)) if there is a buffer by that name
or else 0."
  (interactive)
  (let ((buffer (get-buffer buffer-name)))
    (if buffer
        (buffer-size buffer)
      0)))

(defun buffer-file-name-base ()
  "Return current buffers basename, sans extension."
  (file-name-base (buffer-file-name (current-buffer)))
  )


;; teyjus-inferior-mode helper functions
;;

(defun teyjus-process ()
  "Returns the process running in the *Teyjus* buffer"
  (get-buffer-process (get-buffer "*Teyjus*")))

(defun teyjus-process-stop ()
  "Stop the process running in the *Teyjus* buffer, by sending it the 'stop.' goal."
  (let* ((teyjus-buffer   (get-buffer "*Teyjus*")))
    (comint-redirect-send-command-to-process "stop."
                                             teyjus-buffer
                                             (teyjus-process)
                                             "Teyjus process ended.")))

;; teyjus-inferior-mode Principle functions
;;

(defun teyjus-run (&optional module)
  "Run `tjsim' in an inferior process.

If `module' is supplied, it will try to load the corresponding .lp file
into the simulator."
  (interactive)
  (let* ((proc-buffer-name  "*Teyjus*")
         (proc-is-running   (comint-check-proc proc-buffer-name)))

    ;; If the buffer has a running Teyjus process, stop it.
    (when proc-is-running
      (teyjus-process-stop)
      ;; Wait for the process to return its exit code.
      (accept-process-output (teyjus-process)))

    ;; Start a new tjsim process in the "*Teyjus*" buffer
    (apply 'make-comint-in-buffer "Teyjus" nil tjsim nil (list module))

    ;; pop to the "*Teyjus*" buffer
    (pop-to-buffer
     (if (or proc-is-running
             (not (derived-mode-p 'teyjus-inferior-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create "*Teyjus*")
       (current-buffer)))

    ;; put the buffer into inferior mode
    (teyjus-inferior-mode)))

(defun teyjus-run-buffer ()
  "Loads the current buffer module into tjsim."
  (interactive)
  ;; Add the directory holding buffer file to $TJPATH
  (teyjus-path-add-current-buffer)
  (teyjus-run (buffer-file-name-base)))

(defun teyjus-link ()
  "Link the current buffer module with tjlink."
  (interactive)
  ;; Add the directory holding buffer file to $TJPATH
  (teyjus-path-add-current-buffer)
  (let ((module (buffer-file-name-base)))
    (make-comint "tjlink" tjlink nil module)))

(defun teyjus-load-buffer ()
  "Save, compile, link, and run the file being visited."
  (interactive)
  (let* ((module (buffer-file-name-base)))
    ;; prompt to save buffer with `modulename.*'
    ;; e.g., the modulename.mod and modulename.sig files.
    (save-some-buffers nil (lambda ()
                             (string= (buffer-file-name-base)
                                      module)))
    ;; Compile the module
    (teyjus-compile)

    ;; If compilation produces errors, terminate, leaving the
    ;; *tjcc* buffer open so you can fix compilation errors.
    (unless (accept-process-output (get-buffer-process (get-buffer "*tjcc*")))

      ;; Otherwise, delete the *tjcc* window
      (delete-windows-on "*tjcc*")

      ;; And link the module
      (teyjus-link)

      ;; If tjlink fails
      (if (accept-process-output (get-buffer-process (get-buffer "*tjlink*")))
          ;; Then pop to the *tjlink* buffer
          (pop-to-buffer "*tjlink*")
          ;; Otherwise, load and run the module
          (teyjus-run-buffer)))))

(defun teyjus-inferior--initialize ()
  "Helper function to initialize teyjus-inferior-mode."
  (setq comint-process-echoes t))

(define-derived-mode teyjus-inferior-mode comint-mode "Teyjus"
  "`teyjus-inferior-mode' is a derived mode for running the `teyjus' simulator in emacs.

- Use `teyjus-run-buffer' to start the simulator top-level as an inferior process.
- Use `teyjus-load-buffer' to start the simulator after compiling, linking and
loading the file being visited."

  nil "Teyjus"
  ;; Make the prompt read-only.
  (setq comint-prompt-read-only t)
  ;; Not sure what this does
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(teyjus-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp))

(add-hook 'teyjus-inferior-mode-hook 'teyjus-inferior--initialize)
