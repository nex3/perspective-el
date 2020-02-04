# Perspective for Emacs

## Description

The Perspective package provides multiple named workspaces (or "perspectives")
in Emacs, similar to multiple desktops in window managers like Awesome and
XMonad, and Spaces on the Mac.

Each perspective has its own buffer list and its own window layout. This makes
it easy to work on many separate projects without getting lost in all the
buffers. Switching to a perspective activates its window configuration, and when
in a perspective, only its buffers are available (by default).

Each Emacs frame has a distinct list of perspectives.

Perspective supports saving its state to a file, so long-lived work sessions may
be saved and recovered as needed.


## Sample Use Cases

### Multiple Projects

Working on multiple projects can become difficult to organize as their buffer
lists mix together during a long-running Emacs session. Searching for a buffer
by name works well if you know what to search for, but sometimes picking from a
list is easier — in which case, keeping the list well-pruned for relevant
buffers becomes an important source of efficiency in file and buffer management.
Perspective helps out by letting you keep separate named buffer lists and window
layouts.

This use case works really well in conjunction with
[Projectile](https://github.com/bbatsov/projectile). Projectile helps with
buffer navigation (and other project-specific tasks) in cases when a project has
a well-defined root directory. Perspective then steps in to help manage
unrelated buffers: shells, REPLs, `dired` buffers visiting directories outside
the project, or files relevant to the project not under the same root as the
rest of the source. It also helps deal with the situation of one project with
multiple source repositories where having a shared window layout or buffer list
makes sense.


### [Yak shaving](http://catb.org/jargon/html/Y/yak-shaving.html)

Suppose you're developing feature X in perspective `feature-X`. This keeps you
working with one set of files and windows. You then realize that this feature
requires you to fix a bug in an unrelated set of files. You don't want to lose
all the context you have built up for feature X, so you open a new perspective,
`bugfix-Y`, letting you open new files and buffers without disturbing your work
on `feature-X`. Then you are asked to urgently look into something related to
development of feature Z, but again: you don't want to lose context. So you open
a new perspective `feature-Z`, and fill it with a whole bunch of new files and
windows — all without losing any of the context for your work on bug Y or
feature X.

When you finish looking at Z, you close perspective `feature-Z`, and return to
`bugfix-Y`, and restore its window layout and buffer list. When you finish with
Y, you close perspective `bugfix-Y` and return to `feature-X`.

(Hint: this workflow works best with the `persp-sort` variable set to `'created`
— see documentation below.)


## Similar Packages

The following Emacs packages implement comparable functionality:

- [persp-mode](https://github.com/Bad-ptr/persp-mode.el): A Perspective
  fork, which implements perspective sharing between Emacs frames. It also has a
  different approach to saving state and different configuration options. There
  has been some
  [interest](https://github.com/nex3/perspective-el/issues/88#issuecomment-513996542)
  [expressed](https://github.com/nex3/perspective-el/issues/111) in merging the
  two projects. _Due to conflicting function names, `persp-mode.el` and
  Perspective cannot be installed simultaneously._
- [Workgroups 2](https://github.com/pashinin/workgroups2): Similar to
  Perspective in terms of features. Its [original
  codebase](https://github.com/tlh/workgroups.el) seems to predate Emacs
  acquiring a native ability to serialize window layouts, so it has custom
  serialization code.
- [eyebrowse](https://github.com/wasamasa/eyebrowse): Supports window layouts
  but not buffer lists.
- [wconf](https://github.com/ilohmar/wconf): Supports window layouts but not
  buffer lists.
- [ElScreen](https://github.com/knu/elscreen): Supports window layouts but not
  buffer lists; seems unmaintained.


## Compatibility

Perspective does not work with [Emacs
`desktop.el`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html).
This is because Perspective state stores buffer and window information in frame
parameters, and `desktop-save-mode` does not support saving those types of data.

Instead, Perspective provides its own [disk save and
load](#saving-sessions-to-disk) feature, which cleanly saves perspectives.


## Installation

You should install Perspective from [MELPA](https://melpa.org/) or [MELPA Stable](https://stable.melpa.org/).

Alternately, put `perspective.el` from this source repository in your load path
and run `(require 'perspective)`.

Users of Debian 9 or later or Ubuntu 16.04 or later may simply `apt-get install
elpa-perspective`, though be aware that the stable version provided in these
repositories is likely to be (extremely) outdated.


## Usage

To activate perspective use `(persp-mode)`. This creates a single default `main`
perpsective.

Commands are all prefixed by `C-x x` by default. To change the prefix key,
customize `persp-mode-prefix-key`. Additionally, creating a key binding for
`persp-mode-map` will also activate the prefix.

Here are the main commands defined in `persp-mode-map`:

- `s` — `persp-switch`: Query a perspective to switch to, or create
- `k` — `persp-remove-buffer`: Query a buffer to remove from current perspective
- `c` — `persp-kill` : Query a perspective to kill
- `r` — `persp-rename`: Rename current perspective
- `a` — `persp-add-buffer`: Query an open buffer to add to current perspective
- `A` — `persp-set-buffer`: Add buffer to current perspective and remove it from all others
- `i` — `persp-import`: Import a given perspective from another frame.
- `n`, `<right>` — `persp-next`: Switch to next perspective
- `p`, `<left>` — `persp-prev`: Switch to previous perspective
- `C-s` — `persp-state-save`: Save all perspectives in all frames to a file
- `C-l` — `persp-state-load`: Load all perspectives from a file

Since Perspective maintains distinct buffer lists for each perspective, it helps
to use a Perspective-aware buffer switcher.

**Ido**: [Interactive Do (Ido,
`ido-mode`)](https://www.gnu.org/software/emacs/manual/html_node/ido/index.html),
in particular its `ido-switch-buffer` command, is automatically
Perspective-aware when `persp-mode` is enabled.

**`bs.el`**: Perspective provides a wrapper for
[`bs-show`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Buffer-Menus.html):
`persp-bs-show`. When this function is called normally, it shows a list of
buffers filtered by the current perspective. With a prefix argument, it shows a
list of buffers in all perspectives.

**IBuffer**: Perspective provides a wrapper for
[`ibuffer`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Buffer-Menus.html):
`persp-ibuffer`. When this function is called normally, it shows a list of
buffers filtered by the current perspective. With a prefix argument, it shows a
list of buffers in all perspectives.

**Helm**: Helm relies on the machinery of `ido-mode` for listing buffers, so it
is automatically Perspective-aware when `persp-mode` is enabled.

**Ivy / Counsel**: Perspective provides two commands for listing buffers using
Ivy and Counsel: `persp-ivy-switch-buffer` and `persp-counsel-switch-buffer`.
When these functions are called normally, they show a list of buffers filtered
by the current perspective. With a prefix argument, they shows a list of buffers
in all perspectives. The distinction between the `ivy` and `counsel` versions is
the same as between `ivy-switch-buffer` and `counsel-switch-buffer`: the latter
shows a preview of the buffer to switch to, and the former does not.

Globally binding one of these helper functions to a buffer-switching key is a
good idea, e.g.:

```emacs-lisp
(global-set-key (kbd "C-x C-b") (lambda (arg)
                                  (interactive "P")
                                  (if (fboundp 'persp-bs-show)
                                      (persp-bs-show arg)
                                    (bs-show "all"))))
```


## Saving Sessions to Disk

A pair of functions, `persp-state-save` and `persp-state-load`, implement
perspective durability on disk. When called interactively, they prompt for files
to save sessions to and restore from.

A custom variable, `persp-state-default-file`, sets a default file to use for
saving and restoring perspectives. When it is set, `persp-state-save` may be
called non-interactively without an argument and it will save to the file
referenced by that variable. This makes it easy to automatically save
perspective sessions when Emacs exists:

```
(add-hook 'kill-emacs-hook #'persp-state-save)
```

A limitation of `persp-state-save` and `persp-state-load` is that they do not
attempt to deal with non-file-visiting buffers with non-trivial state. Saving
shell, REPL, and `compilation-mode` buffers is not supported. When saved to a
file, any windows pointing to them are changed to point to the perspective's
`*scratch*` buffer. (Live windows are, of course, left alone.)


## Customization

Perspective supports several custom variables (see its section in `M-x
customize`). The following are likely to be of most interest:

- `persp-sort`: Select the order in which to sort perspectives when calling
  `persp-switch`. Defaults to `'name` (alphabetical), but `'access` (by most
  recently accessed) and `'created` (by order created) are available.
- `persp-interactive-completion-function`: Used for prompting for a perspective
  name. `completing-read` is the default, with `ido-completing-read` enabled
  with `ido-mode`. `ivy-completing-read` is broadly compatible, but
  unfortunately sorts alphabetically and therefore breaks the `persp-sort`
  setting. Helm, unfortunately, does not have a `completing-read` compatible
  implementation out of the box (`helm-completing-read-default-1` purports to be
  this but does not have the same `&optional` defaults). _`ido-completing-read`
  is the recommended setting here for most users._
- `persp-mode-prefix-key`: Changes the default key prefix for Perspective
  commands.
- `persp-state-default-file`: Changes the default file to use for saving and
  loading Perspective state.


## Some Musings on Emacs Window Layouts

The following discussion exceeds the needs of documenting Perspective, but it
falls in the category of helping users learn to manage Emacs sessions, and
therefore will likely help potential users of Perspective make the experience
smoother.

Emacs has bad default behavior when it comes to window handling: many commands
and modes have a habit of splitting existing windows and changing the user's
carefully thought-out window layout. This tends to be a more serious problem for
people who run Emacs on large displays (possibly in full-screen mode): the
greater amount of screen real estate makes it easy to split the frame into many
smaller windows, making any unexpected alterations more disruptive.

As a result of indiscriminate-seeming window splits and buffer switching in
existing windows, new Emacs users can get into the habit of expecting Emacs and
its packages to lack basic respect for their layouts. Hence the popularity of
things like `winner-mode`, and packages like
[shackle](https://github.com/wasamasa/shackle).

This may make the value of Perspective seem questionable: why bother with
carefully preserving window layouts if Emacs will just throw them away on a `M-x
compile`? The answer is to fix Emacs' broken defaults. This is actually fairly
easy.

tl;dr: try the following settings:

```emacs-lisp
(setq display-buffer-alist
      '(("\\*compilation\\*"
         (display-buffer-reuse-window display-buffer-same-window))
        ;; default
        (".*"
         (display-buffer-same-window))))

(setq display-buffer-reuse-frames t)         ; reuse windows in other frames
(setq pop-up-windows nil)                    ; display-buffer: avoid splitting
(setq even-window-heights nil)               ; display-buffer: avoid resizing
```

The Emacs framework responsible for "pop-up" windows is `display-buffer`. The
relevant [section of the Emacs
manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Displaying-Buffers.html#Displaying-Buffers)
is dense and difficult to read, so there have been attempts to summarize the
most important bits:

- https://ess.r-project.org/Manual/ess.html#Controlling-buffer-display
- https://old.reddit.com/r/emacs/comments/cpdr6m/any_additional_docstutorials_on_displaybuffer_and/ews94n1/
