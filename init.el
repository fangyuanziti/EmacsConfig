;; Always load newest byte code
(setq load-prefer-newer t)

;;; prepare load-path 
(add-to-list 'load-path user-emacs-directory)	; default config dir
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "el-get/el-get"))) ;el-get package path
(add-to-list 'load-path	(expand-file-name (concat user-emacs-directory "custom"))) ;custom path

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(package-initialize)

;;;General config

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(setq scroll-margin 3)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name 
                (concat user-emacs-directory "auto-save")) t)))
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(expand-file-name (concat user-emacs-directory "bin"))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; hide the splash-screen
(setq inhibit-splash-screen t)


(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))


;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.
(setq
 el-get-sources
 '((:name rscope
          :type git
          :url "https://github.com/rjarzmik/rscope.git"
          :features rscope
          :compile "rscope.el")
   (:name elscreen
          :type elpa)
   (:name evil-extra-operator
          :type git
          :url "https://github.com/Dewdrops/evil-extra-operator.git"
          :features evil-extra-operator
          :compile "evil-extra-operator.el")
   (:name evil-matchit
          :type elpa)

   (:name evil-tabs
          :type git
          :url "https://github.com/krisajenkins/evil-tabs.git"
          :features evil-tabs
          :compile "el-tabs.el")))
(setq
 my:el-get-packages
 '(el-get                   ; el-get is self-hosting
   escreen                  ; screen for emacs, C-\ C-h
   switch-window            ; takes over C-x o
   auto-complete            ; complete as you type with overlays
   yasnippet
   highlight-parentheses
   undo-tree
   multi-term
   color-theme-solarized
   evil
   evil-leader
   ace-jump-mode
   evil-surround
   evil-numbers
   evil-matchit
   rscope))
(el-get 'sync my:el-get-packages)


;; auto complete config
(require 'auto-complete-config)
(ac-config-default)

;; reset PATH environment variable
(setenv "PATH"
        (concat 
         (expand-file-name (concat user-emacs-directory "bin")) ":"
         (getenv "PATH")))

;; fix ZDOTDIR variable
(setenv "ZDOTDIR" 
        (expand-file-name (concat user-emacs-directory "zsh/zdotdir")))

;;; Multi-term config
(require 'multi-term)
(setq multi-term-program "/bin/zsh")

'(multi-term-scroll-to-bottom-on-output t)

(setq term-eol-on-send t)
(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 0)

            (setq scroll-margin 0)		; disable scroll 

            (define-key term-raw-map (kbd "C-y") 'term-paste)
            
            ;; add shell compilation mode support
            (compilation-shell-minor-mode))) 

;; Actually I don't know why the "C-m" make my zsh in term mode terrible
;; However, change the binding make everything normal.
(delete '("C-m" . term-send-input) term-bind-key-alist)
(add-to-list 'term-bind-key-alist '("C-t" . term-send-input))

;; add window moving binding in multi-term(ansi-mode) mode
(add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
(add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
(add-to-list 'term-bind-key-alist '("C-h" . windmove-left))
(add-to-list 'term-bind-key-alist '("C-l" . windmove-right))
(add-to-list 'term-bind-key-alist '("C-k" . windmove-up))
(add-to-list 'term-bind-key-alist '("C-j" . windmove-down))



(defun reload-dotemacs-file ()
  ;; "reload your .emacs file without restarting Emacs"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;;; solarized configs
(setq dark-or-light 'solarized-dark)
(load-theme 'solarized-dark t)

(defun toggle-theme ()
  (interactive)
  (if (eq dark-or-light 'solarized-light)
      (setq dark-or-light 'solarized-dark)
    (setq dark-or-light 'solarized-light)
    )
  (load-theme dark-or-light t))

(global-set-key (kbd "<f5>") 'toggle-theme)

(require 'rscope)

;;; config evil-leader (before evil)
(require 'evil-leader)
(global-evil-leader-mode)

;;; evil configs
(require 'evil)
(evil-mode t)

(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)
(setq evil-auto-indent t)

(setq evil-normal-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

;;; keep special mode when in those buffer
(loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
                              (nrepl-mode . insert)
                              (pylookup-mode . emacs)
                              (comint-mode . normal)
                              (shell-mode . insert)
                              (term-mode . emacs)
                              (git-commit-mode . insert)
                              (git-rebase-mode . emacs)
                              (helm-grep-mode . emacs)
                              (bc-menu-mode . emacs)
                              (magit-branch-manager-mode . emacs)
                              (rdictcc-buffer-mode . emacs)
                              (dired-mode . emacs)
                              (Wdired-mode . normal))
      do (evil-set-initial-state mode state))

;; remove the evil key binding
(defun evil-delete-key (keymap key)
  (define-key keymap key nil))


(evil-delete-key evil-motion-state-map (kbd "RET"))
(evil-delete-key evil-motion-state-map " ")

;; leader key config
(evil-leader/set-leader ",")
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "s" 'evil-window-split
  "v" 'evil-window-vsplit
  "w" 'evil-write
  "fw" 'ace-jump-word-mode
  "fl" 'ace-jump-line-mode
  "ff" 'ace-jump-char-mode)

(setq ace-jump-word-mode-use-query-char nil)


(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;;; esc quits for vim mode
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(define-key evil-normal-state-map "\C-h" 'evil-window-left)
(define-key evil-normal-state-map "\C-j" 'evil-window-down)
(define-key evil-normal-state-map "\C-k" 'evil-window-up)
(define-key evil-normal-state-map "\C-l" 'evil-window-right)
(define-key evil-normal-state-map "\C-l" 'evil-window-right)

(define-key evil-normal-state-map "\S-H" 'evil-beginning-of-line)
(define-key evil-normal-state-map "\S-L" 'evil-end-of-line)

;; evil surround
(global-surround-mode 1)

;; evil matchit
(global-evil-matchit-mode 1)

;; evil extra operateor

;; Default major mode
(setq initial-major-mode 'eshell)


;; pair configuration
(electric-pair-mode 1)

(define-globalized-minor-mode global-highlight-parentheses-mode highlight-parentheses-mode
  (lambda nil (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(show-paren-mode 1)


;; yasnippet config
(yas-global-mode 1)
(add-hook 'term-mode-hook (lambda()
                            (yas-minor-mode -1)))


(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(global-set-key (kbd "C-$") '(lambda () (interactive) (eshell t)))


;; C mode config
(setq c-default-style "linux"
      c-basic-offset 4)

(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)

(electric-indent-mode 1)

;; Server
(server-start)
(defun ff/raise-frame-and-give-focus ()
  (when window-system
    (raise-frame)
    (x-focus-frame (selected-frame))
    (set-mouse-pixel-position (selected-frame) 4 4)
    ))
(add-hook 'server-switch-hook 'ff/raise-frame-and-give-focus)
