;; Init -- Emacs config for James Collier
;;; Commentary:

;;; Code:

;; Fix backups
(setq backup-by-copying-when-linked t)

;;Remove interface cruft
(setq inhibit-splash-screen t
      inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;;Global useful stuff
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq debug-on-error t)

;; Fix PATH
(eval-when-compile
  (setenv "PATH"
          (concat (expand-file-name "~/.ghcup/bin/")
                  ":" (expand-file-name "~/.local/bin/")
                  ":" (expand-file-name "~/.npm/bin/")
                  ":" (expand-file-name "~/.cargo/bin/")
                  ":" (expand-file-name "~/.poetry/bin/")
                  ":" (expand-file-name "~/.pyenv/bin/")
                  ":" (expand-file-name "~/.cabal/bin/")
                  ":" (expand-file-name "~/.nix-profile/bin/")
                  ":" (getenv "PATH")))
  (setq exec-path
        (append exec-path (list (expand-file-name "~/.ghcup/bin")
                                (expand-file-name "~/.local/bin")
                                (expand-file-name "~/.npm/bin")
                                (expand-file-name "~/.cargo/bin")
                                (expand-file-name "~/.poetry/bin")
                                (expand-file-name "~/.pyenv/bin")
                                (expand-file-name "~/.cabal/bin")
                                (expand-file-name "~/.nix-profile/bin")))))


;; Package management
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			                   ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;; Customize
(setq custom-file "~/.emacs.d/.emacs-customize.el")
(load custom-file)

;; Use 'y' or 'n' rather than 'yes' or 'no\
(fset 'yes-or-no-p 'y-or-n-p)

;; General dev
(setq column-number-mode t)
(setq c-default-style "linux"
      c-basic-offset 2)
(setq-default indent-tabs-mode nil
              tab-width 2
              indicate-empty-lines nil)

(defun my-prog-mode-setup ()
  "General customisation for programming modes."
  (hs-minor-mode 1) ;; Fold code with C-c @ C-c
  (display-line-numbers-mode)
  (prettify-symbols-mode)

  ;; Special setup for HTML+ and tagedit
  (if (string= "mhtml-mode" major-mode)
      (progn
        (require 'tagedit)
        (tagedit-mode 1)
        (tagedit-add-paredit-like-keybindings)
        (tagedit-add-experimental-features)))
  )

(add-hook 'prog-mode-hook #'my-prog-mode-setup)

;; Devdocs
(use-package devdocs
  :ensure t
  :bind (("C-h D" . devdocs-lookup)))

;; Change active window
(use-package windmove
  :ensure nil
  :bind (("C-s-<left>" . windmove-left)
         ("C-s-<right>" . windmove-right)
         ("C-s-<up>" . windmove-up)
         ("C-s-<down>" . windmove-down)))

;; Theme
(use-package ample-theme
  :ensure t
  :init (progn (load-theme 'ample t t)
		           (enable-theme 'ample))

  :defer t)

;; Show number of search matches
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode +1))

;; markdown
(use-package markdown-preview-mode
  :ensure t)

;; YAML
(use-package yaml-mode
  :ensure t

  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package paren
  :config
  (show-paren-mode)
  (setq show-paren-style 'mixed))

(use-package multiple-cursors
  :ensure t

  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package ggtags
  :ensure t

  :bind
  (("C-c g s" . ggtags-find-other-symbol)
   ("C-c g h" . ggtags-view-tag-history)
   ("C-c g r" . ggtags-find-reference)
   ("C-c g f" . ggtags-find-file)
   ("C-c g c" . ggtags-create-tags)
   ("C-c g u" . ggtags-update-tags)))

(use-package direnv
  :ensure

  :init
  (add-hook 'prog-mode-hook #'direnv-update-environment)
  :config
  (direnv-mode))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :ensure t
  :init
  (setq-default flycheck-disabled-checkers '(python-flake8))
  (global-flycheck-mode)

  :config
  (flycheck-define-checker python-mypy-custom
    "Custom version of the mypy checker"
    :command ("mypy"
              (config-file "--config-file" flycheck-python-mypy-config)
              (option "--cache-dir" flycheck-python-mypy-cache-dir)
              "--ignore-missing-imports"
              source-original)
    :error-patterns
    ((error line-start (file-name) ":" line (optional ":" column)
            ": error:" (message) line-end)
     (warning line-start (file-name) ":" line (optional ":" column)
              ": warning:" (message) line-end)
     (info line-start (file-name) ":" line (optional ":" column)
           ": note:" (message) line-end))
    :modes python-mode
    :predicate flycheck-buffer-saved-p)
  (add-to-list 'flycheck-checkers 'python-mypy-custom)
  (flycheck-add-next-checker 'python-pylint 'python-mypy-custom))

;; Git
(use-package magit
  :ensure t

  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-c g") 'magit-file-dispatch))

(use-package magit-todos
  :ensure t)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t

  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode) . rainbow-delimiters-mode))

;; Language server protocol
(use-package lsp-mode
  :ensure t
  :hook ((elm-mode . lsp)
         (haskell-mode . lsp)
         (haskell-literate-mode . lsp)
         (purescript-mode . lsp)
         (typescript-mode . lsp)
         (rust-mode . lsp))
  :config
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.direnv")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.git")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\output")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.spago")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\elm-stuff")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\node_modules")
  ;;(setq gc-cons-threshold 100000000)
  ;;(setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;;(setq lsp-use-plists 1)
  ;;(setq lsp-log-io nil)

  :commands lsp)

(use-package lsp-ui
  :ensure t

  :bind (([remap xref-find-references] . lsp-ui-peek-find-references)
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions))

  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package format-all
  :ensure t

  :hook ((prog-mode . format-all-mode)
         (format-all-mode . format-all-ensure-formatter)))

;; Nix + JSON
(use-package json-mode
  :ensure t)

(use-package nix-mode
  :ensure t)

;; HTML
(use-package tagedit
  :ensure t
  :demand

  :after (sgml-mode html-mode mhtml-mode)

  :config
  (tagedit-add-paredit-like-keybindings)
  (tagedit-add-experimental-features))

;; Lisp
(use-package paredit
  :ensure t

  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

  :hook ((emacs-lisp-mode
	        eval-expression-minibuffer-setup
	        lisp-mode
	        lisp-interaction-mode
	        scheme-mode) . enable-paredit-mode))

(use-package slime
  :ensure t

  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"))


;; Scheme
(use-package geiser
  :ensure t

  :config
  (setq geiser-active-implementations '(guile)))

;; Haskell
(use-package haskell-mode
  :ensure t

  :init
  (setq haskell-stylish-on-save nil)

  :hook ((haskell-mode . subword-mode)
         (haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-doc-mode)))

(use-package lsp-haskell
  :ensure t
  :config
  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper"))

(use-package rust-mode
  :ensure t)

;; C-type lanaguages
(add-hook 'c-mode-common-hook
	        (lambda ()
	          (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
	            (ggtags-mode 1))))
(setq gdb-many-windows t
      gdb-show-main t)


;; Python
(use-package elpy
  :after (repl-toggle)
  :ensure t

  :init
  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "-i")
  (elpy-enable)

  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-modules (delq 'elpy-module-folding elpy-modules))
  (setq elpy-rpc-python-command "python3")

  :hook ((elpy-mode . flycheck-mode)))

;; Dhall
(use-package dhall-mode
  :ensure t)

;; Purescript
;; (use-package psci
;;   :ensure t)

;; (use-package psc-ide
;;   :ensure t
;;   :config
;;   (setq psc-ide-use-npm-bin t))

(use-package purescript-mode
  :ensure t

  :bind
  (("C-," . purescript-move-nested-left)
   ("C-." . purescript-move-nested-right))

  ;; The unicode input method inserts characters not accepted by purs
  :hook ((purescript-mode . turn-on-purescript-indentation)
	       ;;(purescript-mode . psc-ide-mode)
	       (purescript-mode . flycheck-mode)
	       ;;(purescript-mode . inferior-psci-mode)
         ))


;; Elm
(use-package elm-mode
  :ensure t
  :config
  (setq elm-tags-on-save t
	      elm-tags-exclude-elm-stuff t
	      elm-package-json "elm.json")

  :init
  (add-hook 'elm-mode-hook 'elm-format-on-save-mode))


;; TypeScript
(defun setup-tide-mode ()
  "Tide mode is a bit complicated to set up."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-auto-change-delay 1.5)
  (let* ((checker 'javascript-eslint)
         (root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))
    (flycheck-select-checker checker))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (setq typescript-indent-level 4
        typescript-expr-indent-offset 0))

(use-package typescript-mode
  :ensure t

  :after (flycheck)

  :hook ((typescript-mode . setup-tide-mode)))

(use-package tide
  :ensure t

  :after (flycheck typescript-mode)

  :config
  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
                                                                :placeOpenBraceOnNewLineForFunctions nil
                                                                :indentSize 2
                                                                :tabSize 2))

  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
                                        ;(before-save . tide-format-before-save)
         ))

;; Javascript
(use-package prettier-js
  :ensure t

  :hook ((prettier-js . javascript-mode)
         (prettier-js . web-mode)))

;; SASS
(use-package sass-mode
  :ensure t)

;; Web
(defun setup-web-mode ()
  "Set up for web mode when not on tsx files.")
(use-package web-mode
  :ensure t
  :mode (("\\.tsx$" . web-mode))
  :after (flycheck)

  :init
  (flycheck-mode +1)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-to-list 'flycheck-checkers 'javascript-eslint)

  :hook ((web-mode . (lambda ()
                       (pcase (file-name-extension buffer-file-name)
                         ("tsx" (setup-tide-mode))
                         (_ (setup-web-mode)))))))

;; TODO highlighting
(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":")

  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)
         (purescript-mode . hl-todo-mode)))

(provide 'init)
;;; init.el ends here
