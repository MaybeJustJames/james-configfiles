;; Emacs config for James Collier
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


;; Package management
(require 'package)
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


;; Customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"])
 '(custom-enabled-themes (quote (manoj-dark)))
 '(haskell-stylish-on-save t)
 '(package-selected-packages
   (quote
    (tide projectile repl-toggle psc-ide psci purescript-mode py-autopep8 flycheck elpy mu4e use-package indium js2-mode flymd ghc haskell-mode slime paredit multiple-cursors magit klere-theme ggtags color-theme-solarized atom-dark-theme arc-dark-theme ample-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; UGent login
;;; jacollie

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
	smtpmail-auth-credentials '(("dev.bits.vib.be" 465 "jacol" "1!just4do1it2SQN"))
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

(use-package paren
  :config
  (show-paren-mode)
  (setq show-paren-style 'mixed))

(use-package repl-toggle
  :ensure t

  :config
  (setq rtog/mode-repl-alist '((lisp-mode . slime)
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

(use-package projectile
  :ensure t

  :config
  (projectile-mode +1)

  :bind
  (("C-c p" . projectile-command-map)))

(use-package company
  :ensure t)

;; Git
(use-package magit
  :ensure t

  :config
  (global-set-key (kbd "C-x g") 'magit-status))


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

;; Haskell


;; C-type lanaguages
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
	      (ggtags-mode 1))))
(setq gdb-many-windows t
      gdb-show-main t)


;; Python
(use-package flycheck
  :ensure t)

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


;; TypeScript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq lycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package typescript-mode
  :ensure t

  :after (company flycheck)

  :hook ((typescript-mode . setup-tide-mode)))

(use-package tide
  :ensure t

  :after (typescript-mode)

  :init
  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
          :placeOpenBraceOnNewLineForFunctions nil
          :indentSize 4
          :tabSize 4))

  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;; Finally, initialisation
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (let ((w (split-window-right)))
	      (select-window w)
	      (mu4e))))
