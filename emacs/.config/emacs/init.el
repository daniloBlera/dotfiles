;;;; The journey into structured editing begins here
;;;
;;; obs.: The first time running this file should download all the necessary packages and
;;; tree-sitter grammars
;;;
;;; tested under Emacs version 31.0.50

;;; Packages
;; add Melpa repository
(use-package package
  :config (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; listing and configuring packages
(use-package ace-window :ensure t :defer t :bind ("M-o" . ace-window))
(use-package avy :ensure t :defer t
  :bind (("C-;" . avy-goto-char)
         ("C-:" . avy-goto-char-2)))
(use-package breadcrumb :ensure t :defer t :hook (prog-mode . breadcrumb-mode))
(use-package company :ensure t :config (setq global-company-mode t))
(use-package ef-themes :ensure t)
(use-package envrc :ensure t :defer t :hook (after-init . envrc-global-mode))
(use-package flycheck :ensure t :defer t)
(use-package format-all :ensure t :defer t)
(use-package htmlize :ensure t :defer t)
(use-package jinx :ensure t :defer t
  :bind ("C-c t j" . jinx-mode)
  :hook ((org-mode prog-mode text-mode) . jinx-mode))
(use-package magit :ensure t :defer t)
(use-package olivetti :ensure t :defer t
  :bind ("C-c t o" . olivetti-mode)
  :init (setq olivetti-body-width 100 olivetti-style 'fancy)
  :hook ((org-mode prog-mode text-mode) . olivetti-mode))
(use-package paredit :ensure t :defer t
  :hook ((emacs-lisp-mode lisp-mode) . paredit-mode))
(use-package rainbow-delimiters :ensure t :defer t
  :hook ((org-mode prog-mode text-mode) . rainbow-delimiters-mode))
(use-package shades-of-purple-theme :ensure t)
(use-package sly :ensure t :defer t
  :config (setq inferior-lisp-program "/usr/bin/sbcl"))
(use-package transpose-frame :ensure t :defer t)
(use-package unicode-fonts :ensure t)
(use-package vundo :ensure t :defer t :bind ("C-c d u" . vundo))

;;; General configuration
;; specify the file for `customize` and friends
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)

;; backup edited files under a centralized location
(setq backup-directory-alist '(("." . "~/.cache/emacs/backups"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; setting the default font
(add-to-list 'default-frame-alist '(font . "MonaspiceRn Nerd Font Mono-10"))

;; scroll line-by-line instead of in chunks
(setq scroll-step 1
      scroll-conservatively 10000)

;;; Hooks
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'text-mode-hook #'whitespace-mode)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(add-hook 'occur-hook (lambda () (switch-to-buffer-other-window "*Occur*")))
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'sh-mode-hook #'flycheck-mode)

;;; Tree-Sitter configuration
;; mapping major modes with their tree-sitter variants
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (html-mode . html-ts-mode)
        (json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (toml-mode . toml-ts-mode)
        (yaml-mode . yaml-ts-mode)))

;; defining the grammar sources
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

;; download missing grammars
(dolist (grammar treesit-language-source-alist)
  (let ((language (car grammar)))
    (unless (treesit-language-available-p language)
      (treesit-install-language-grammar language))))

;; org stuff
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;;; Keymaps
(keymap-global-set "C-c d b" #'fill-region)
(keymap-global-set "C-c d c" #'comment-dwim)
(keymap-global-set "C-c d o" #'occur)
(keymap-global-set "C-c d r" #'repeat)
(keymap-global-set "C-c d s" #'screenshot-svg)
(keymap-global-set "C-c t r" #'view-mode)
(keymap-global-set "C-c t w" #'whitespace-mode)
(keymap-global-set "C-x C-b" #'ibuffer)
(global-unset-key (kbd "C-z"))  ; disable suspend

;; re-enable uppercase and lowercase region commands
(put 'upcase-region 'disabled nil)      ; C-x C-u
(put 'downcase-region 'disabled nil)    ; C-x C-l

;; re-enable narrowing region -- check C-x n {d,n,p,w}
(put 'narrow-to-region 'disabled nil)   ; C-x n n

;;; Features to disable
(put 'enriched-mode 'disabled
     "this inserts lots of markup, which can be difficult to remove.")

(put 'enriched-toggle-markup 'disabled
     "maybe avoid `enriched-mode' if you don't know what you're doing.")

;;; Custom functions
;; save an SVG screenshot of emacs into `/tmp`
(defun dcb/screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.  Saves to a temp file and puts
the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))
