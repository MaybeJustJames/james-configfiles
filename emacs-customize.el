;;; package -- Isolated for Emacs Custom

;;; Commentary:

;;; Code:
;; Customizations
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"])
 '(custom-enabled-themes (quote (manoj-dark)))
 '(debug-on-error t)
 '(elm-package-json "elm.json")
 '(elm-tags-exclude-elm-stuff nil)
 '(elm-tags-on-save t)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults)))
 '(flycheck-checker-error-threshold 1000)
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-mypy-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3")
 '(haskell-font-lock-symbols t)
 '(haskell-stylish-on-save t)
 '(idris-interpreter-path "/home/jacol/.cabal/bin/idris")
 '(js-indent-level 2)
 '(lsp-elm-elm-path "/home/jacol/.npm/bin/elm")
 '(lsp-log-io t)
 '(package-selected-packages
   (quote
    (anzu fira-code-mode hasklig-mode clj-refactor tagedit rainbow-delimiters php-mode elm-mode company-lsp lsp-ui lsp-mode sbt-mode scala-mode idris-mode cider clojure-mode all-the-icons dired-sidebar ibuffer-sidebar tide projectile repl-toggle psc-ide psci purescript-mode py-autopep8 flycheck elpy mu4e use-package indium js2-mode flymd ghc haskell-mode slime paredit multiple-cursors magit klere-theme ggtags color-theme-solarized atom-dark-theme arc-dark-theme ample-theme)))
 '(typescript-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number-current-line ((t (:inherit line-number :background "DarkGoldenrod4")))))
(provide 'emacs-customize)
;;; emacs-customize.el ends here
