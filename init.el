;;; Init -- Emacs config for James Collier
;;; Commentary:

;;; Code:

;; Fix backups
(setq backup-by-copying-when-linked t)

;;Remove interface cruft
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;;Global useful stuff
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Fix PATH
(eval-when-compile
  (setenv "PATH"
          (concat (getenv "PATH") ":" (expand-file-name "~/.local/bin/")))
  (setq exec-path
        (append exec-path (list (expand-file-name "~/.local/bin")))))


;; Package management
(require 'package)
(if (and (version< emacs-version "26.3") (>= libgnutls-version 30604))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
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

;; project management
(use-package projectile
  :ensure t

  :config
  (projectile-mode +1)

  :bind
  (("C-c p" . projectile-command-map)))

;; Change active window
(use-package windmove
  :ensure nil
  :bind (("C-s-<left>" . windmove-left)
         ("C-s-<right>" . windmove-right)
         ("C-s-<up>" . windmove-up)
         ("C-s-<down>" . windmove-down)))

;; Window title
(defun frame-title-format ()
  "Evaluate to current project name, where applicable."
  (concat
   "emacs - "
   (when (and (bound-and-true-p projectile-mode)
              (projectile-project-p))
     (format "[%s] - " (projectile-project-name)))
   (let ((file buffer-file-name))
     (if file
         (abbreviate-file-name file)
       "%b"))))
(setq-default frame-title-format '((:eval (frame-title-format))))


;; Email
(use-package mu4e
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-maildir "/home/jacol/.local/mail/VIB")
  (setq mu4e-sent-folder   "/Sent")
  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-trash-folder  "/Trash")
  (setq mu4e-maildir-shortcuts
	'(("/INBOX"  . ?i)
          ("/Sent"   . ?s)
          ("/Trash"  . ?t)))
  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-compose-reply-to-address "james.collier@vib.be"
	user-mail-address "james.collier@vib.be"
	user-full-name  "James Collier"))

(use-package smtpmail
  :init
  (setq smtpmail-default-smtp-server "dev.bits.vib.be")

  :config
  (setq message-send-mail-function 'smtpmail-send-it
	send-mail-function 'smtpmail-send-it
	user-mail-address "james.collier@vib.be"
	smtpmail-smtp-server "dev.bits.vib.be"
	smtpmail-local-domain "vib.be"
	smtpmail-sendto-domain "vib.be"
	smtpmail-smtp-user "jacol"
	smtpmail-stream-type 'ssl
	smtpmail-smtp-service 465
	smtpmail-debug-info t))

;; Viewing markdown
(use-package flymd
  :ensure t

  :init
  (defun my-flymd-browser (url)
    (let ((browse-url-browser-function 'browse-url-firefox))
      (browse-url url)))
  (setq flymd-browser-open-function 'my-flymd-browser))

;; General dev
(setq column-number-mode t)
(setq c-default-style "linux"
      c-basic-offset 2)
(setq-default indent-tabs-mode nil
              tab-width 2
              indicate-empty-lines nil)
(global-hl-line-mode 1)

(defun my-prog-mode-setup ()
  "General customisation for programming modes."
  (hs-minor-mode 1) ;; Fold code with C-c @ C-c
  (display-line-numbers-mode)

  ;; Special setup for HTML+ and tagedit
  (if (string= "mhtml-mode" major-mode)
      (progn
        (require 'tagedit)
        (tagedit-mode 1)
        (tagedit-add-paredit-like-keybindings)
        (tagedit-add-experimental-features)))
  )

(add-hook 'prog-mode-hook #'my-prog-mode-setup)

(use-package paren
  :config
  (show-paren-mode)
  (setq show-paren-style 'mixed))

(use-package repl-toggle
  :ensure t

  :config
  (setq rtog/mode-repl-alist '((lisp-mode . slime)
                               (emacs-lisp-mode . ielm)
                               (python-mode . elpy-shell-switch-to-shell)
                               (purescript-mode . psci))))

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

(use-package company
  :ensure t)

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(defun +sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

(use-package ibuffer-sidebar
  :ensure t
  :commands (ibuffer-sidebar-toggle-sidebar))

(use-package dired-sidebar
  :ensure t

  :after ibuffer-sidebar

  :commands (dired-sidebar-toggle-sidebar)

  :hook
  (dired-sidebar-mode-hook . (lambda ()
                               (unless (file-remote-p default-directory)
                                 (auto-revert-mode))))

  :config
  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-use-term-integration t))
(global-set-key (kbd "C-c C-b") '+sidebar-toggle)

;; Git
(use-package magit
  :ensure t

  :config
  (global-set-key (kbd "C-x g") 'magit-status))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t

  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode
          clojure-mode
          cider-repl-mode) . rainbow-delimiters-mode))

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
	        scheme-mode
          clojure-mode
          cider-repl-mode) . enable-paredit-mode))

(use-package slime
  :ensure t

  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "sbcl"))

;; Clojure
(defun repl-cider-refresh ()
  "Reset the REPL."
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun repl-cider-user-ns ()
  "Set the REPL ns to user."
  (interactive)
  (cider-repl-set-ns "user"))

(use-package clojure-mode
  :ensure

  :hook ((clojure-mode . subword-mode)))

(use-package cider
  :ensure t

  :after (clojure)

  :config
  (setq nrepl-log-messages t)

  :bind
  (:map clojure-mode-map
   ("C-M-r" . repl-cider-refresh)
   ("C-c u" . repl-cider-user-ns))

  :hook ((cider-mode . eldoc-mode)
         (cider-repl-mode . eldoc-mode)))

;; Haskell
(use-package haskell-mode
  :ensure t

  :hook ((haskell-mode . subword-mode)
         (haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-doc-mode)))


;; Idris
(use-package idris-mode
  :ensure t)


;; Scala
(use-package scala-mode
  :ensure t
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; Language server protocol
(use-package lsp-mode
  :ensure t
  ; :load-path "~/Documents/lsp-mode"
  ;; Optional - enable lsp-mode automatically in scala files
  :hook ((scala-mode . lsp)
         (elm-mode . lsp))
  :config (setq lsp-prefer-flymake nil)

  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Add company-lsp backend for metals
(use-package company-lsp
  :ensure t)



;; C-type lanaguages
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
	      (ggtags-mode 1))))
(setq gdb-many-windows t
      gdb-show-main t)


;; Python
(use-package py-autopep8
  :ensure t)

(use-package elpy
  :after (flycheck py-autopep8 repl-toggle)
  :ensure t

  :init
  (setq python-shell-interpreter "python3"
	python-shell-interpreter-args "-i")
  (elpy-enable)

  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-rpc-python-command "python3")

  :hook ((elpy-mode . flycheck-mode)
	       (elpy-mode . py-autopep8-enable-on-save)))

;; Purescript
(use-package psci
  :ensure t)

(use-package purescript-mode
  :after (repl-toggle psci)
  :ensure t

  :init
  (setq exec-path (cons (expand-file-name "~/.npm/bin") exec-path))

  :config
  (use-package psc-ide
    :ensure t
    :config
    (setq psc-ide-use-npm-bin t))

  :bind
  (("C-," . purescript-move-nested-left)
   ("C-." . purescript-move-nested-right))

  :hook ((purescript-mode . turn-on-purescript-unicode-input-method)
	       (purescript-mode . turn-on-purescript-indentation)
	       (purescript-mode . psc-ide-mode)
	       (purescript-mode . company-mode)
	       (purescript-mode . flycheck-mode)
	       (purescript-mode . inferior-psci-mode)))


;; Elm
(use-package elm-mode
  :ensure t
  :load-path "~/Documents/elm-mode"
  :config
  (setq elm-format-on-save t)
  (add-to-list 'company-backends 'company-elm))


;; TypeScript
(defun setup-tide-mode ()
  "Tide mode is a bit complicated to set up."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-auto-change-delay 1.5)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (setq typescript-indent-level 2
        typescript-expr-indent-offset 0))

(use-package typescript-mode
  :ensure t

  :after (company flycheck)

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

;; Finally, initialisation
;; (add-hook 'emacs-startup-hook
;; 	          (lambda ()
;; 	            (let ((w (split-window-right)))
;; 	              (select-window w)
;; 	              (mu4e))))
(put 'upcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here
(put 'downcase-region 'disabled nil)
