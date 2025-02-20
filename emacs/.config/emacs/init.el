;;; The journey into structured editing begins here
;;;
;;; obs.: The first time running this file should download all the necessary packages and
;;; tree-sitter grammars
;;;
;;; tested under Emacs version 31.0.50

;;; Packages
;; add MELPA repository
(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package avy
  :ensure t
  :hook (after-init . avy-setup-default)
  :bind (("C-c g c" . avy-goto-char)
         ("C-c g C" . avy-goto-char-2)
         ("C-c g t" . avy-goto-char-timer)
         ("C-c g ;" . avy-goto-line)
         ("C-c g r" . avy-resume)))

(use-package breadcrumb
  :ensure t
  :hook (prog-mode . breadcrumb-mode))

(use-package company
  :ensure t
  :init (setq company-lighter "")
  :hook (after-init . global-company-mode))

(use-package ef-themes
  :ensure t)

(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

(use-package erlang-ts
 :ensure t
 :defer t
 :mode ("\\.erl\\'" . erlang-ts-mode))

(use-package company-erlang
  :ensure t)

(use-package flycheck
  :ensure t
  :defer t)

(use-package format-all
  :ensure t
  :defer t
  :config (setq-default format-all-mode-lighter "FMT"
                        format-all-formatters '(("Python" black isort))))

(use-package htmlize
  :ensure t
  :defer t)

(use-package jinx
  :ensure t
  :hook ((prog-mode text-mode) . jinx-mode))

(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-ts-mode)
  :interpreter ("lua" . lua-ts-mode)
  :config (setq lua-indent-nested-calls t))

(use-package olivetti
  :ensure t
  :init (setq olivetti-body-width 100
              olivetti-style 'fancy
              olivetti-lighter "")
  :hook ((org-mode prog-mode text-mode) . olivetti-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package replace
  :bind ("C-c o" . occur)
  :hook (occur . (lambda () (switch-to-buffer-other-window "*Occur*"))))

(use-package shades-of-purple-theme
  :ensure t)

(use-package sly
  :ensure t
  :init (setq inferior-lisp-program "sbcl")
  :bind ("M-h" . 'sly-documentation-lookup)
  :hook (lisp-mode . sly-editing-mode))

(use-package transpose-frame
  :ensure t
  :bind (("C-c t t" . transpose-frame)
         ("C-c t c" . rotate-frame-clockwise)
         ("C-c t C" . rotate-frame-anticlockwise)
         ("C-c t v" . flip-frame)
         ("C-c t h" . flop-frame)))

(use-package unicode-fonts
  :ensure t
  :defer t
  :config (unicode-fonts-setup))

(use-package vundo
  :ensure t
  :bind ("C-x u" . vundo))

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
(add-to-list 'default-frame-alist '(font . "0xProto Nerd Font Mono-10"))

;; do not recenter the window after moving the cursor beyond the view
(setq scroll-conservatively 100)

(defun my/daylight-p ()
  "Check if the current time is in the 6:00am-6:00pm window."
  (< 5 (string-to-number (format-time-string "%H")) 17))

;; set light or dark theme depending on the time
(let ((light-theme 'ef-summer)
      (dark-theme (if (display-graphic-p) 'shades-of-purple 'ef-maris-dark)))
  (load-theme (if (my/daylight-p) light-theme dark-theme) t))

;;; Hooks
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'text-mode-hook #'auto-fill-mode)

;;; Tree-Sitter configuration
;; mapping major modes to their tree-sitter variants
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (erlang-mode . erlang-ts-mode)
        (html-mode . html-ts-mode)
        (json-mode . json-ts-mode)
        (lua-mode . lua-ts-mode)
        (python-mode . python-ts-mode)
        (toml-mode . toml-ts-mode)
        (yaml-mode . yaml-ts-mode)))

;; defining the grammar sources
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (erlang "https://github.com/WhatsApp/tree-sitter-erlang")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

;; download missing grammars
(dolist (grammar treesit-language-source-alist)
  (let ((language (car grammar)))
    (unless (treesit-language-available-p language)
      (treesit-install-language-grammar language))))

;; configure eglot's language server protocol servers
(with-eval-after-load 'eglot
  ;; (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio"))))

;; org stuff
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; screenshots of emacs
(defun my/screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.

Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

;;; Keymaps
(keymap-global-set "C-c e" #'eval-buffer)
(keymap-global-set "C-c f" #'fill-region)
(keymap-global-set "C-c s" #'my/screenshot-svg)
(keymap-global-set "C-c v" #'view-mode)
(keymap-global-set "C-c ;" #'comment-dwim)
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
