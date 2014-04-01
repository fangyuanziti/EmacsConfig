(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;;; from purcell/emacs.d
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

;;; evil configs
(require-package 'evil)

(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)

(setq evil-auto-indent t)

(require 'evil)
(evil-mode t)

(require-package 'undo-tree)
(require 'undo-tree)

;;; solarized configs
(require-package 'solarized-theme)
(require 'solarized-dark-theme)

;;; hide the splash-screen
(setq inhibit-splash-screen t)


;;;General config
(setq scroll-margin 3)


(require-package 'multi-term)
(require 'multi-term)
(setq multi-term-program "/bin/bash")

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
