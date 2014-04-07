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

(setq scroll-margin 3)
;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; hide the splash-screen
(setq inhibit-splash-screen t)

;; Set up load path
(add-to-list 'load-path user-emacs-directory)



;;; prepare el-get environment
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

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
   undo-tree
   multi-term
   color-theme-solarized
   evil
   evil-leader
   rscope))
(el-get 'sync my:el-get-packages)


;; auto complete config
(require 'auto-complete-config)
(ac-config-default)


;;; Multi-term config
(require 'multi-term)
(setq multi-term-program "/bin/zsh")

(add-hook 'term-mode-hook
      (lambda ()
        (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
        (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))))

(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 0)))

(add-hook 'term-mode-hook
          (lambda ()
            (define-key term-raw-map (kbd "C-y") 'term-paste)))

(add-hook 'term-mode-hook
      (lambda ()
        (add-to-list 'term-bind-key-alist '("C-h" . windmove-left))
        (add-to-list 'term-bind-key-alist '("C-l" . windmove-right))
        (add-to-list 'term-bind-key-alist '("C-k" . windmove-up))
        (add-to-list 'term-bind-key-alist '("C-j" . windmove-down))))


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
  "w" 'evil-write)

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

 ;; change mode-line color by evil state
;(lexical-let ((default-color (cons (face-background 'mode-line)
                                   ;(face-foreground 'mode-line))))
             ;(add-hook 'post-command-hook
                       ;(lambda ()
                         ;(let ((color (cond ((minibufferp) default-color)
                                            ;((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                            ;((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                            ;((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                            ;(t default-color))))
                           ;(set-face-background 'mode-line (car color))
                           ;(set-face-foreground 'mode-line (cdr color))))))

;(require 'powerline)
;(powerline-center-evil-theme)


;(load "elscreen" "ElScreen" t)
;(define-key evil-normal-state-map (kbd "C-w t") 'elscreen-create) ;creat tab
;(define-key evil-normal-state-map (kbd "C-w x") 'elscreen-kill) ;kill tab
;(define-key evil-normal-state-map "gT" 'elscreen-previous) ;previous tab
;(define-key evil-normal-state-map "gt" 'elscreen-next) ;next tab
;; Server! ____________________________________________________________________

(server-start)
(defun ff/raise-frame-and-give-focus ()
  (when window-system
    (raise-frame)
    (x-focus-frame (selected-frame))
    (set-mouse-pixel-position (selected-frame) 4 4)
    ))
(add-hook 'server-switch-hook 'ff/raise-frame-and-give-focus)


;; Default major mode
(setq initial-major-mode 'multi-term)
