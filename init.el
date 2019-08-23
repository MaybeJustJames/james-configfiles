;; Emacs config for James Collier
;;; Code:

;; Fix backups
(setq backup-by-copying-when-linked t)

;;Remove interface cruft
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

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
    (use-package indium js2-mode flymd ghc haskell-mode slime paredit multiple-cursors magit klere-theme ggtags color-theme-solarized atom-dark-theme arc-dark-theme ample-theme)))
 '(smtpmail-smtp-server "smtp.ugent.be")
 '(smtpmail-smtp-service 587))
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
  (setq smtpmail-default-smtp-server "smtp.ugent.be")

  :config
  (setq message-send-mail-function 'smtpmail-send-it
	send-mail-function 'smtpmail-send-it
	user-mail-address "james.collier@ugent.be"
	smtpmail-auth-credentials '(("smtp.ugent.be" 465 "jacollie" "!just4do1it2SQN"))
	smtpmail-smtp-server "smtp.ugent.be"
	smtpmail-local-domain "ugent.be"
	smtpmail-sendto-domain "ugent.be"
	smtpmail-smtp-user "jacollie"
	smtpmail-stream-type 'ssl
	smtpmail-smtp-service 465
	smtpmail-debug-info t))

;; Viewing markdown
(use-package flymd
  :init
  (defun my-flymd-browser (url)
    (let ((browse-url-browser-function 'browse-url-firefox))
      (browse-url url)))
  (setq flymd-browser-open-function 'my-flymd-browser))

;; General dev
(show-paren-mode 1)
(setq column-number-mode t)
(setq show-paren-style 'mixed
      c-default-style "linux"
      c-basic-offset 2)

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package ggtags
  :config
  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags))


;; Git
(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))


;; Lisp
(use-package paredit
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(use-package slime
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


;; Finally, initialisation
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (let ((w (split-window-right)))
	      (select-window w)
	      (mu4e))))
