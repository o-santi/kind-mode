# kind mode
simple (and very bad) [kind-lang](https://github.com/kind-lang/Kind) emacs' major mode.

# installation

since this package is not yet on melpa, i recommend using `straight.el` to fetch it.

with `straight.el` it's as simple as adding this repo as the source and then add it to `'auto-mode-alist`
```emacs-lisp
 (use-package kind-mode
   :defer t
   :straight (kind-mode :type git :host github :repo "o-santi/kind-mode")
   :config (add-to-list 'auto-mode-alist '("\\.kind" . kind-mode)))
```

with normal `use-package`, you will need to download `kind-mode.el`, add it's dir path (not the actual file) to `load-path` and then do something like:

```emacs-lisp
 (use-package kind-mode
   :defer t
   :load-path "/your/path/to/kind-mode/dir/"
   :config (add-to-list 'auto-mode-alist '("\\.kind" . kind-mode)))
```

# usage

it currently offers a very loose (and bad) syntax highlighting and two handy commands: `kind-typecheck-buffer` (`C-c C-c`) and `kind-run-buffer` (`C-c C-r`). i believe their usage is self-explanatory.

indentation is yet to be done, so for now you will need to bear it.
