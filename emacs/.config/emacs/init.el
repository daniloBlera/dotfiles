;;; -*- lexical-binding: t; -*-
;;; The journey into structured editing begins here
;;;
;;; obs.: The first time running this file should download all the necessary packages and
;;; tree-sitter grammars
;;;
;;; tested under Emacs version 31.0.50

;;; Loading extra files
;; specify the file for `customize' and its friends
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

;; load private configurations that shouldn't be found in vcs
(let ((user-settings (expand-file-name "default.el" user-emacs-directory)))
  (if (file-exists-p user-settings)
      (load-file user-settings)))

;;; Packages
;; add MELPA repository
(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

;; simpler themes
(use-package doric-themes :ensure t :defer t)

;; fancier themes
(use-package ef-themes :ensure t :defer t)

;; automatically change themes based on time -- or sunrise and sunset, if configured
(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '(("4:00" . ef-dream)
                           ("6:00" . ef-trio-light)
                           ("16:00" . ef-dream)
                           ("18:00" . ef-cherie)))
  (circadian-setup))

;; window selection motions
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?j ?k ?l ?\; ?h)))

;; text motions
(use-package avy
  :ensure t
  :hook (after-init . avy-setup-default)
  :bind (("C-;" . avy-goto-char-timer)
         ("C-/" . avy-goto-line)
         ("C-_" . avy-goto-line)))

;; display a trail of function definitions relative to the cursor
(use-package breadcrumb
  :ensure t
  :hook (prog-mode . breadcrumb-mode))

;; smarter commenting
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2))

;; simpler completions through inline previews
(use-package completion-preview
  :ensure t
  :init (global-completion-preview-mode t)
  :config
  (setq completion-preview-minimum-symbol-length 2)
  (push 'org-self-insert-command completion-preview-commands)
  (push 'paredit-backward-delete completion-preview-commands)
  :bind
  ((:map completion-preview-active-mode-map
         ("M-n" . completion-preview-next-candidate)
         ("M-p" . completion-preview-prev-candidate)
         ("M-i" . completion-preview-insert))))

;; support to direnv
(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

;; erlang's tree-sitter mode
(use-package erlang-ts
  :ensure t
  :mode ("\\.erl\\'" . erlang-ts-mode))

;; modify `find-file' to auto complete file paths under the cursor
(use-package ffap
  :hook (after-init . ffap-bindings))

;; better syntax checking
(use-package flycheck
  :ensure t
  :bind (("C-c t c" . flycheck-mode)))

;; `flycheck' support for eglot
(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config (global-flycheck-eglot-mode 1))

;; run formatters on buffers based on their major-mode
(use-package format-all
  :ensure t
  :defer t
  :config (setq-default format-all-mode-lighter "FMT"
                        format-all-formatters '(("Python" black isort))))

;; get the remote URL for the buffer location
(use-package git-link
  :ensure t
  :bind (("C-c g l" . git-link)))       ; use `C-u C-c g l' to select the remote

;; major mode for Go
(use-package go-mode :ensure t :defer t)

;; major mode for Haskell
(use-package haskell-mode :ensure t :defer t)

;; an even more helpful help
(use-package helpful
  :ensure t
  :bind (("C-h f" . 'helpful-callable)
         ("C-h F" . 'helpful-function)
         ("C-h v" . 'helpful-variable)
         ("C-h k" . 'helpful-key)
         ("C-h x" . 'helpful-command)
         ("C-c C-d" . 'helpful-at-point)))

;; convert buffer contents to html
(use-package htmlize :ensure t :defer t)

;; (built-in) list buffer definitions (functions, variables, etc.) on the minibuffer
(use-package imenu
  :hook ((prog-mode org-mode) . imenu-add-menubar-index)
  :config (setq imenu-sort-function 'imenu--sort-by-name))

;; indent guides
(use-package indent-bars
  :ensure t
  :config (setq indent-bars-prefer-character t)
  :hook ((python-ts-mode lua-ts-mode json-ts-mode yaml-ts-mode) . indent-bars-mode))

;; better, just-in-time spell-checker
(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; a great git interface integrated right where you need it
(use-package magit :ensure t :defer t)

;; display information about minibufer entries
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

;; major mode for markdown
(use-package markdown-mode :ensure t)

;; major mode for lua
(use-package lua-mode
  :ensure t
  :mode ("\\.lua\\'" . lua-ts-mode)
  :interpreter ("lua" . lua-ts-mode)
  :config (setq-default lua-indent-nested-calls t))

;; balance window margins so you don't have to look all the way to the left
(use-package olivetti
  :ensure t
  :init (setq olivetti-body-width 110
              olivetti-style 'fancy
              olivetti-lighter "")
  :bind (("C-c t o" . olivetti-mode))
  :hook ((org-mode prog-mode text-mode Info-mode) . olivetti-mode))

;; a more flexible completion style that matches any order
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; structural editing that avoids unbalanced parenthesis, square brackets, curly braces, etc.
(use-package paredit
  :ensure t
  :hook
  ((emacs-lisp-mode lisp-mode inferior-emacs-lisp-mode sly-mrepl-mode) . enable-paredit-mode))

;; quickly toggle buffers
(use-package popper
  :ensure t
  :bind (("C-`" . popper-toggle)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Output\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Python\\*"
          "\\*sly-mrepl for sbcl\\*"
          "\\*sly-description\\*"
          "\\*sly-scratch\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; color-code matching pair delimiters like parenthesis, brackets, braces, etc.
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; (built-in) list recently opened files
(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :config (recentf-mode t))

;; (built-in) list all lines matching pattern
(use-package replace
  :bind ("C-c o" . occur)
  :hook (occur . (lambda () (switch-to-buffer-other-window "*Occur*"))))

;; move through `imenu' entries in a side-bar
(use-package side-hustle
  :ensure t
  :bind (("C-c t s" . side-hustle-toggle)))

;; a common lisp development environment
(use-package sly
  :ensure t
  :init (setq inferior-lisp-program "sbcl")
  :bind (:map sly-mode-map ("M-h" . sly-documentation-lookup))
  :hook (lisp-mode . sly-editing-mode))

;; swap frame positions
(use-package transpose-frame
  :ensure t
  :bind (("C-c r t" . transpose-frame)
         ("C-c r c" . rotate-frame-clockwise)
         ("C-c r C" . rotate-frame-anticlockwise)
         ("C-c r -" . flip-frame)
         ("C-c r /" . flop-frame)))

;; undo the filling/breaking of paragraphs
(use-package unfill
  :ensure t
  :bind ([remap fill-paragraph] . unfill-toggle))

;; vertical completion system
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :bind (:map vertico-map ("TAB" . minibuffer-complete)))

;; open files larger than your system's memory
(use-package vlf :ensure t)

;; navigate an undo tree
(use-package vundo
  :ensure t
  :bind ("C-z" . vundo))

;;; General configuration
;; backup edited files under a centralized location
(setq backup-directory-alist '(("." . "~/.cache/emacs/backups"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; setting the default font
(add-to-list 'default-frame-alist '(font . "0xProto Nerd Font Mono-11"))

;;; Hooks
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;; Tree-Sitter configuration
;; mapping major modes to their tree-sitter variants
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (erlang-mode . erlang-ts-mode)
        (go-mode . go-ts-mode)
        (html-mode . html-ts-mode)
        (js-json-mode . json-ts-mode)
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
        (go "https://github.com/tree-sitter/tree-sitter-go")
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

;; open files under specific directories as read-only
(add-hook 'find-file-hook
          (lambda ()
            (dolist (pattern '("~/.config/glirc/logs/.*" "~/.config/glirc/logs-acer/.*"))
              (if (string-match (expand-file-name pattern) buffer-file-name)
                  (read-only-mode)))))

;; org stuff
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

;; screenshots of Emacs
(defun my/screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.

Create a screenshot under /tmp and put the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

;;; Global key bindings
(global-set-key (kbd "C-c e") #'eval-buffer)
(global-set-key (kbd "C-c s") #'my/screenshot-svg)
(global-set-key (kbd "C-c t v") #'view-mode)
(global-set-key (kbd "C-c t b") #'menu-bar-mode)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c f s") #'isearch-forward)
(global-set-key (kbd "C-c f S") #'isearch-backward)
(global-set-key (kbd "C-c f r") #'isearch-forward-regexp)
(global-set-key (kbd "C-c f R") #'isearch-backward-regexp)
(global-set-key (kbd "C-c f w") #'isearch-forward-word)
(global-set-key (kbd "C-h a") #'apropos)

;; re-enable uppercase and lowercase region commands
(put 'upcase-region 'disabled nil)      ; C-x C-u
(put 'downcase-region 'disabled nil)    ; C-x C-l

;; re-enable narrowing region -- check C-x n [dnpw]
(put 'narrow-to-region 'disabled nil)   ; C-x n n

;;; Features to disable
(put 'enriched-mode 'disabled
     "this inserts lots of markup, which can be difficult to remove.")

(put 'enriched-toggle-markup 'disabled
     "maybe avoid `enriched-mode' if you don't know what you're doing.")
