;;; inti.el --- emacs config main file
;;; Commentary:

;;; Code:


;;; Always load newest byte code
(setq load-prefer-newer t)


(defun emacs-subdir (dirname)
 "Get subdir path by DIRNAME."
  (expand-file-name
   (concat user-emacs-directory dirname "/")))

;;; prepare load-path
(add-to-list 'load-path (emacs-subdir "el-get/el-get")) ;el-get package path
(add-to-list 'load-path	(emacs-subdir "custom")) ;custom path

;;;General config

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(setq scroll-margin 3)


;; Disable interlock symlink
(setq create-lockfiles nil)

;; Write backup files to own directory
(setq backup-directory-alist `(("." . ,(emacs-subdir "backups")))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

(setq auto-save-file-name-transforms
     `((".*" ,(emacs-subdir "auto-save") t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(expand-file-name (concat user-emacs-directory "bin"))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; hide the splash-screen
(setq inhibit-splash-screen t)


(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(el-get-bundle el-get)
(el-get-bundle escreen)
(el-get-bundle switch-window)
(el-get-bundle company-mode)
(el-get-bundle yasnippet)
(el-get-bundle highlight-parentheses)
(el-get-bundle undo-tree)
(el-get-bundle multi-term)
(el-get-bundle color-theme-solarized)
(el-get-bundle evil)
(el-get-bundle evil-leader)
(el-get-bundle ace-jump-mode)
(el-get-bundle evil-numbers)
(el-get-bundle flycheck)
(el-get-bundle flycheck-rust)
(el-get-bundle rust-mode)
(el-get-bundle elpa:evil-matchit)
(el-get-bundle coffee-mode)
(el-get-bundle scala-mode)
(el-get-bundle puml-mode)
(el-get-bundle typescript-mode)
(el-get-bundle js2-mode)
(el-get-bundle go-mode)
(el-get-bundle haskell-mode)
(el-get-bundle yaml-mode)


(defun prefix-path (path)
  "Prefix path into enviroment variable `PATH`."
  (setenv "PATH"
          (concat path
                  ":"
                  (getenv "PATH"))))

;; reset PATH environment variable
(prefix-path (emacs-subdir "bin"))

;; fix ZDOTDIR variable
(setenv "ZDOTDIR"
        (emacs-subdir "zsh/zdotdir"))

;;; Multi-term config
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


(defun reload-dotemacs-file ()
  "Reload your .emacs file without restarting Emacs."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;;; solarized configs
(color-theme-solarized)

;;; config evil-leader (before evil)
(global-evil-leader-mode)

;;; evil configs
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

(defun evil-delete-key (keymap key)
  "Remove the evil key binding by KEYMAP and KEY."
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

(define-key evil-normal-state-map "\S-H" 'evil-beginning-of-line)
(define-key evil-normal-state-map "\S-L" 'evil-end-of-line)

;; evil surround
;;(global-surround-mode 1)

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
  "Clear function for eshell."
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
  "Rasie a new frame and give focus."
  (when window-system
    (raise-frame)
    (x-focus-frame (selected-frame))
    (set-mouse-pixel-position (selected-frame) 4 4)
    ))
(add-hook 'server-switch-hook 'ff/raise-frame-and-give-focus)

;;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(add-hook 'after-init-hook 'global-company-mode)


(setq coffee-tab-width 2)

;; Enable puml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.puml\\'" . puml-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . puml-mode))
(setq puml-plantuml-jar-path
      (expand-file-name "~/.emacs.d/el-get/puml-mode/plantuml.jar"))

(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init.el ends here
