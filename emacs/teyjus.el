;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Teyjus Lambda Prolog mode to go with COMINT package by Kamal Aboul-Hosn
;;; Reduced for Teyjus 2.0 by Andrew Gacek
;;;
;;; This package handles the following features:
;;;   - Syntax highlighting for .mod and .sig files
;;;   - Running an inferior tjcc process within Emacs
;;;   - Teyjus support for emacs compilation mode (M-x compile)
;;;
;;; Commands have been added to make easier the use of Teyjus:
;;;
;;;  Command                Shortcut     Function
;;;  ----------             --------     --------
;;;  M-x teyjus-compile     C-c C-c      Compile a module
;;;  M-x teyjus-next-error  C-c C-n      Find the next compilation error
;;;  M-x teyjus-prev-error  C-c C-p      Find the previous compilation error
;;;
;;; These commands are also available via the Teyjus menu
;;;
;;;
;;; To use the Teyjus Emacs interface, add this line to your .emacs file:
;;;
;;;   (load "~/teyjus/emacs/teyjus.el")
;;;
;;; Adjust the path as neccesary. If tjcc is not in your $PATH then you
;;; should set the variable tjcc to the precise location. For example,
;;;
;;;   (setq tjcc "~/teyjus/tjcc")
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'easymenu)
(require 'comint)
(require 'font-lock)

(provide 'teyjus)

(setq auto-mode-alist (cons '("\\.mod\\'" . teyjus-edit-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sig\\'" . teyjus-edit-mode) auto-mode-alist))

(defvar tjcc "tjcc")

(defvar teyjus-edit-mode-map nil)
(cond ((not teyjus-edit-mode-map)
       (setq teyjus-edit-mode-map (make-sparse-keymap))
       (define-key teyjus-edit-mode-map "\C-c\C-c" 'teyjus-compile)
       (define-key teyjus-edit-mode-map "\C-c\C-p" 'teyjus-prev-error)
       (define-key teyjus-edit-mode-map "\C-c\C-n" 'teyjus-next-error)))

(defvar teyjus-mode-hook '()
  "*Hook for customizing teyjus mode")

;; Define the menu for teyjus
(defvar teyjus-menu
  '("Teyjus"
    ["Compile current module"           teyjus-compile    t]
    ["Show next compilation error"      teyjus-next-error t]
    ["Show previous compilation error"  teyjus-prev-error t]))

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
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
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
