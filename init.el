;; Init -- Emacs config for James Collier
;;; Commentary:

;;; Code:

;; Fix PATH
(eval-when-compile
  (setenv "PATH"
          (concat (expand-file-name "~/.ghcup/bin/")
                  ":" (expand-file-name "~/.local/bin/")
                  ":" (expand-file-name "~/.npm/bin/")
                  ;":" (expand-file-name "~/.cargo/bin/")
                  ":" (expand-file-name "~/.poetry/bin/")
                  ":" (expand-file-name "~/.pyenv/bin/")
                  ":" (expand-file-name "~/.pyenv/shims/")
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
                                (expand-file-name "~/.pyenv/shims")
                                (expand-file-name "~/.cabal/bin")
                                (expand-file-name "~/.nix-profile/bin")))))


;; Package management
(defvar bootstrap-version)
(setq straight-use-package-by-default t)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir) user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)


(defun my-prog-mode-setup ()
  "General customisation for programming modes."
  (hs-minor-mode 1) ;; Fold code with C-c @ C-c
  (display-line-numbers-mode)
  (prettify-symbols-mode)
  (electric-pair-mode)

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
  :straight t
  :bind (("C-h D" . devdocs-lookup)))

;; Change active window
(use-package windmove
  :straight nil
  :bind (("C-s-<left>" . windmove-left)
         ("C-s-<right>" . windmove-right)
         ("C-s-<up>" . windmove-up)
         ("C-s-<down>" . windmove-down)))

;; Theme
(use-package ample-theme
  :straight t
  :init (progn (load-theme 'ample t t)
		           (enable-theme 'ample))

  :defer t)

;; Show number of search matches
(use-package anzu
  :straight t
  :config
  (global-anzu-mode +1))

;; markdown
(use-package markdown-preview-mode
  :straight t)

;; YAML
(use-package yaml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package paren
  :straight t
  :config
  (show-paren-mode)
  (setq show-paren-style 'mixed))

(use-package multiple-cursors
  :straight t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package ggtags
  :straight t
  :bind
  (("C-c g s" . ggtags-find-other-symbol)
   ("C-c g h" . ggtags-view-tag-history)
   ("C-c g r" . ggtags-find-reference)
   ("C-c g f" . ggtags-find-file)
   ("C-c g c" . ggtags-create-tags)
   ("C-c g u" . ggtags-update-tags)))

(use-package direnv
  :straight t
  :init
  (add-hook 'prog-mode-hook #'direnv-update-environment)
  :config
  (direnv-mode))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :straight t
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
  (add-to-list 'flycheck-checkers 'python-mypy-custom))

;; Git
(use-package transient
  :straight (transient
             :type git
             :host github
             :repo "magit/transient"
             :version 0.4.1))
(use-package magit
  :straight (magit
             :type git
             :host github
             :repo "magit/magit"
             :version 3.2.1)
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-c g") 'magit-file-dispatch))

(use-package magit-todos
  :straight t)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :straight t
  :init
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25)
  :init
  (global-corfu-mode))

(use-package tree-sitter
  :straight t)

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(use-package emacs
  :init
  ;; Customize
  ;; Fix backups
  (setq backup-by-copying-when-linked t)

  ;;Remove interface cruft
  (setq inhibit-splash-screen t
        inhibit-startup-screen t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1)

  ;;Global useful stuff
  (setq load-prefer-newer t)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq debug-on-error t)
  (put 'downcase-region 'disabled nil)

  (setq custom-file "~/.emacs.d/.emacs-customize.el")
  (load custom-file)

  (setenv "GPG_AGENT_INFO" nil)
  (setq epg-pinentry-mode 'loopback)

  
  ;; Wayand inter-process copy/paste
  ;; credit: yorickvP on Github
  (setq wl-copy-process nil)
  (defun wl-copy (text)
    (setq wl-copy-process (make-process :name "wl-copy"
                                        :buffer nil
                                        :command '("wl-copy" "-f" "-n")
                                        :connection-type 'pipe
                                        :noquery t))
    (process-send-string wl-copy-process text)
    (process-send-eof wl-copy-process))
  (defun wl-paste ()
    (if (and wl-copy-process (process-live-p wl-copy-process))
        nil ; should return nil if we're the current paste owner
        (shell-command-to-string "wl-paste -n | tr -d \r")))
  (setq interprogram-cut-function 'wl-copy)
  (setq interprogram-paste-function 'wl-paste)


  ;; Use 'y' or 'n' rather than 'yes' or 'no'
  (setopt use-short-answers t)

  ;; General dev
  (setq column-number-mode t)
  (setq c-default-style "linux"
        c-basic-offset 2)
  (setq-default indent-tabs-mode nil
                tab-width 2
                indicate-empty-lines nil)

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Eglot
  (cl-defmethod project-root ((project (head eglot-project)))
    (cdr project))
  
  (defun my-project-try-tsconfig-json (dir)
    (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
      (cons 'eglot-project found)))

  (add-hook 'project-find-functions
            'my-project-try-tsconfig-json nil nil)

  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(python-mode . ("ruff" "server")))
    (add-to-list 'eglot-server-programs
                 '(unison-ts-mode . ("127.0.0.1" 5757)))
    ;; (add-to-list 'eglot-server-programs
    ;;              '(typescript-mode . ("typescript-language-server" "--stdio")))
    )

  (setq treesit-language-source-alist
        '((unison "https://github.com/fmguerreiro/tree-sitter-unison-kylegoetz" "build/include-parser-in-src-control")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python"))))

(use-package cape
  :straight t)

;; AI Code assistant
;; (use-package codeium
;;   :straight (codeium :type git :host github :repo "Exafunction/codeium.el")
;;   :init
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (setq-local completion-at-point-functions
;;                           (list ;(cape-capf-buster #'codeium-completion-at-point)
;;                            (cape-capf-buster #'lsp-completion-at-point)
;;                            ))))
;;   :config
;;   (setq use-dialog-box t
;;         codeium-mode-line t)
;;   (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;;   (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t))

;; Language server protocol
;; (use-package lsp-mode
;;   :straight t
;;   :custom
;;   (lsp-completion-provider :none)
;;   (lsp-completion-enable nil)
;;   :init
;;   (defun my/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(flex))) ;; Configure flex style
;;   :hook ((lsp-completion-mode . my/lsp-mode-setup-completion)
;;          (elm-mode . lsp)
;;          (haskell-mode . lsp)
;;          (haskell-literate-mode . lsp)
;;          (purescript-mode . lsp)
;;          (typescript-mode . lsp)
;;          (rust-mode . lsp)
;;          (python-mode . lsp)
;;          (java-mode . lsp))
;;   :config
;;   (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.direnv")
;;   (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.git")
;;   (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\output")
;;   (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.spago")
;;   (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\elm-stuff")
;;   (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\node_modules")
;;   ;;(setq gc-cons-threshold 100000000)
;;   ;;(setq read-process-output-max (* 1024 1024)) ;; 1mb
;;   ;;(setq lsp-use-plists 1)
;;   ;;(setq lsp-log-io nil)
;;   ;; (add-hook 'completion-at-point-functions
;;   ;;           #'codeium-completion-at-point nil 'local)

;;   :commands lsp)

;; (use-package lsp-ui
;;   :straight t
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :bind (([remap xref-find-references] . lsp-ui-peek-find-references)
;;          ([remap xref-find-definitions] . lsp-ui-peek-find-definitions))

;;   :commands lsp-ui-mode)

;; (use-package lsp-treemacs
;;   :straight t
;;   :after lsp
;;   :commands lsp-treemacs-errors-list)

;; (use-package format-all
;;   :straight t
;;   :hook ((prog-mode . format-all-mode)
;;          (format-all-mode . format-all-ensure-formatter)))

;; Nix + JSON
(use-package json-mode
  :straight t)

(use-package nix-mode
  :straight t)

;; HTML
(use-package tagedit
  :straight t
  :demand
  :after (sgml-mode html-mode mhtml-mode)
  :config
  (tagedit-add-paredit-like-keybindings)
  (tagedit-add-experimental-features))

;; Lisp
(use-package paredit
  :straight t
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

  :hook ((emacs-lisp-mode
	        eval-expression-minibuffer-setup
	        lisp-mode
	        lisp-interaction-mode
	        scheme-mode) . enable-paredit-mode))

(use-package slime
  :straight t
  :defer t
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"))


;; Scheme
(use-package geiser-guile
  :straight t)

(use-package geiser
  :straight t
  :config
  (setq geiser-active-implementations '(guile)))

;; Haskell
(use-package haskell-mode
  :straight t
  :init
  (setq haskell-stylish-on-save nil)

  :hook ((haskell-mode . subword-mode)
         (haskell-mode . haskell-doc-mode)))

;; (use-package lsp-haskell
;;   :straight t
;;   :config
;;   (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper"))

;; Unison
(use-package unison-ts-mode
  :straight (unison-ts-mode :type git :host github :repo "fmguerreiro/unison-ts-mode" :files ("*.el")))

;; Rust
(use-package rust-mode
  :straight t)

;; C-type lanaguages
(add-hook 'c-mode-common-hook
	        (lambda ()
	          (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
	            (ggtags-mode 1))))
(setq gdb-many-windows t
      gdb-show-main t)


;; Python
(use-package elpy
  :straight t
  :after (repl-toggle)
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
  :straight t)

(use-package purescript-mode
  :straight t
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
  :straight t
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
  (tide-hl-identifier-mode +1))

(use-package typescript-mode
  :straight t
  :mode (("\\.tsx" . typescript-mode))
  :after (flycheck company)

  :hook ((typescript-mode . setup-tide-mode)))

(use-package tide
  :straight t
  :after (flycheck typescript-mode)

  :config
  (setq tide-format-options
        '(
          :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
          :placeOpenBraceOnNewLineForFunctions nil
          :indentSize 2
          :tabSize 2))

  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
                                        ;(before-save . tide-format-before-save)
         ))

;; Javascript
(use-package prettier-js
  :straight t
  :hook ((prettier-js . javascript-mode)
         (prettier-js . web-mode)))


;; Java
;; (use-package lsp-java
;;   :straight t)

;; SASS
(use-package sass-mode
  :straight t)

;; Web
(use-package web-mode
  :straight t
  :mode (("\\.jsx$" . web-mode))
  :after (flycheck)

  :init
  (flycheck-mode +1)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-to-list 'flycheck-checkers 'javascript-eslint))

;; Snippets
(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

;; TODO highlighting
(use-package hl-todo
  :straight t
  :config
  (setq hl-todo-highlight-punctuation ":")

  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode)
         (purescript-mode . hl-todo-mode)))

(use-package mastodon
  :straight t
  :config
  (setq mastodon-instance-url "https://aus.social"
        mastodon-active-user "MaybeJustJames"))

(use-package ement
  :straight (:type git
             :host github
             :repo "alphapapa/ement.el"))

(use-package pinentry
  :defer nil
  :straight t
  :config (pinentry-start))

(provide 'init)
;;; init.el ends here
