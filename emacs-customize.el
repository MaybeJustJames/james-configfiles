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
 '(elpy-modules
   '(elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults))
 '(flycheck-checker-error-threshold 1000)
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-mypy-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3")
 '(format-all-default-formatters
   '(("Assembly" asmfmt)
     ("ATS" atsfmt)
     ("Bazel" buildifier)
     ("BibTeX" emacs-bibtex)
     ("C" clang-format)
     ("C#" clang-format)
     ("C++" clang-format)
     ("Cabal Config" cabal-fmt)
     ("Clojure" cljfmt)
     ("CMake" cmake-format)
     ("Crystal" crystal)
     ("CSS" prettier)
     ("Cuda" clang-format)
     ("D" dfmt)
     ("Dart" dart-format)
     ("Dhall" dhall)
     ("Dockerfile" dockfmt)
     ("Elixir" mix-format)
     ("Elm" elm-format)
     ("Emacs Lisp" emacs-lisp)
     ("F#" fantomas)
     ("Fish" fish-indent)
     ("Fortran Free Form" fprettify)
     ("GLSL" clang-format)
     ("Go" gofmt)
     ("GraphQL" prettier)
     ("Haskell" ormolu)
     ("HTML" html-tidy)
     ("Java" clang-format)
     ("JavaScript" prettier)
     ("JSON" prettier)
     ("Jsonnet" jsonnetfmt)
     ("JSX" prettier)
     ("Kotlin" ktlint)
     ("LaTeX" latexindent)
     ("Less" prettier)
     ("Literate Haskell" brittany)
     ("Lua" lua-fmt)
     ("Markdown" prettier)
     ("Nix" nixpkgs-fmt)
     ("Objective-C" clang-format)
     ("OCaml" ocp-indent)
     ("Perl" perltidy)
     ("PHP" prettier)
     ("Protocol Buffer" clang-format)
     ("PureScript" purs-tidy)
     ("Python" black)
     ("R" styler)
     ("Reason" bsrefmt)
     ("ReScript" rescript)
     ("Ruby" rufo)
     ("Rust" rustfmt)
     ("Scala" scalafmt)
     ("SCSS" prettier)
     ("Shell" shfmt)
     ("Solidity" prettier)
     ("SQL" sqlformat)
     ("Svelte" prettier)
     ("Swift" swiftformat)
     ("Terraform" terraform-fmt)
     ("TOML" prettier)
     ("TSX" prettier)
     ("TypeScript" prettier)
     ("V" v-fmt)
     ("Verilog" istyle-verilog)
     ("Vue" prettier)
     ("XML" html-tidy)
     ("YAML" prettier)
     ("_Angular" prettier)
     ("_Flow" prettier)
     ("_Gleam" gleam)
     ("_Ledger" ledger-mode)
     ("_Nginx" nginxfmt)
     ("_Snakemake" snakefmt)))
 '(haskell-font-lock-symbols t)
 '(haskell-process-type 'cabal-repl)
 '(js-indent-level 2)
 '(lsp-elm-elm-path "/home/jacol/.npm/bin/elm")
 '(markdown-command "pandoc -f gfm -t html")
 '(org-babel-load-languages '((haskell . t) (emacs-lisp . t) (C . t) (python . t)))
 '(package-selected-packages
   '(mastodon magit-todos cider clojure-mode helm rust-mode devdocs prettier-js typescript-mode dhall-mode format-all flycheck yasnippet direnv purescript-mode lsp-treemacs org-mode json-mode nix-mode lsp-haskell web-mode sass-mode markdown-preview-mode yaml-mode anzu tagedit rainbow-delimiters elm-mode lsp-ui lsp-mode all-the-icons tide elpy use-package indium js2-mode ghc haskell-mode slime geiser paredit multiple-cursors magit ggtags ample-theme hl-todo))
 '(python-shell-extra-pythonpaths
   '("/usr/lib/python3/dist-packages" "/usr/lib/python3/dist-packages/pip" "/usr/lib/python3/dist-packages/jedi"))
 '(safe-local-variable-values
   '((python-shell-interpreter-args . "run python -i")
     (python-shell-interpreter . "poetry")
     (sgml-basic-offset . 4)))
 '(set-mark-command-repeat-pop t)
 '(typescript-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number-current-line ((t (:inherit line-number :background "DarkGoldenrod4")))))
(provide 'emacs-customize)
;;; emacs-customize.el ends here
