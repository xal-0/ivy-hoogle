# ivy-hoogle

A really simple menu for searching and navigating to hoogle entries.
Supports both identifier-based and type-directed searching, and syntax
highlights results.

## Demo

![Live demo](https://user-images.githubusercontent.com/33556084/53057283-9bb13780-3463-11e9-8d06-c7337d34ed86.gif)

## Quick setup

Via `use-package-quelpa`:

| Key | Command | Description |
| --- | --- | --- |
| `C-c h` | `ivy-hoogle` | Start a hoogle search. |

```elisp
(use-package ivy-hoogle
  :quelpa (ivy-hoogle
           :fetcher git
           :url "https://github.com/sjsch/ivy-hoogle.git"
           :upgrade t)
  :bind ("C-c h" . ivy-hoogle))
```
