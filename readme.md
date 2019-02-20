# ivy-hoogle

A really simple menu for search and navigating to hoogle entries.
Supports both identifier-based and type-directed searching, and syntax
highlights results.

## Quick setup

Via `use-package-quelpa`:

| Key | Command | Description |
| --- | --- | --- |
| `C-c h` | `ivy-hoogle` | Start a hoogle search. |

```lisp
(use-package ivy-hoogle
  :quelpa (ivy-hoogle
           :fetcher git
           :url "https://github.com/sjsch/ivy-hoogle.git"
           :upgrade t)
  :bind ("C-c h" . ivy-hoogle))
```
