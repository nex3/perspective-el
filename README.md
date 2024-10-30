# Perspective for Emacs

![Test perspective](https://github.com/nex3/perspective-el/actions/workflows/test-perspective.yml/badge.svg)

The Perspective package provides multiple named workspaces (or "perspectives")
in Emacs, similar to multiple desktops in window managers like Awesome and
XMonad, and Spaces on the Mac.

Each perspective has its own buffer list and its own window layout, along with
some other isolated niceties, like the
[xref](https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html)
ring. This makes it easy to work on many separate projects without getting lost
in all the buffers. Switching to a perspective activates its window
configuration, and when in a perspective, only its buffers are available (by
default).

Each Emacs frame has a distinct list of perspectives.

Perspective supports saving its state to a file, so long-lived work sessions may
be saved and recovered as needed.

At long last this project has a
[changelog](https://github.com/nex3/perspective-el/blob/master/CHANGELOG.md);
please refer to it for release notes.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
- [Sample Use Cases](#sample-use-cases)
    - [Multiple Projects](#multiple-projects)
    - [Yak Shaving](#yak-shaving)
    - [Perspective Merging](#perspective-merging)
- [Similar Packages](#similar-packages)
- [Compatibility](#compatibility)
- [Installation](#installation)
- [Usage](#usage)
    - [Buffer Switching](#buffer-switching)
    - [Notes on `completing-read` Enhancements](#notes-on-completing-read-enhancements)
- [Saving Sessions to Disk](#saving-sessions-to-disk)
- [Customization](#customization)
- [Some Musings on Emacs Window Layouts](#some-musings-on-emacs-window-layouts)
<!-- markdown-toc end -->


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


### [Yak Shaving](http://catb.org/jargon/html/Y/yak-shaving.html)

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


### Perspective Merging

Yak shaving is useful for working on projects that are largely unrelated but
sometimes you are working on multiple projects that are very much related, to
the point that you want to view files from both projects at the same time. This
is where perspective merging comes in.

Suppose you are working on a project that requires developing multiple auxiliary
libraries. It may get messy to develop both the main project and all the
libraries from the same perspective so instead you put each library in its own
perspective so you can work on them in isolation. All of a sudden though you
wish to see library code from the main projects perspective. Instead of
switching back and forth between the library and main projects perspectives you
can run `M-x persp-merge` and import the buffers from the libraries perspective.
When you are done you can run remove the imported buffers with
`M-x persp-unmerge`.

The purpose of perspective merging is to combine the buffer lists of different
perspectives while keeping a clear distinction of which buffers belong to which
perspective.

- You can merge together as many perspectives as you want.
- Merging is one directional so if you merge A into B, B's buffers will not be
available in A.
- Merging is not transitive so if you merge A into B, then B into C, the buffers
in A will not be available in C.
- The merge state is saved across sessions when using [persp-state-{save,load}](#saving-sessions-to-disk).


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
- [Burly](https://github.com/alphapapa/burly.el): An approach to persisting window and frame configurations using Emacs bookmarks.

Emacs 27 includes two new buffer and window organizing features: Tab Line
(`global-tab-line-mode`) and Tab Bar (`tab-bar-mode`).
- Tab Line maintains a list of buffers which had been opened in a given window,
  and anchors it to that window. It is analogous to tabs as used in web browsers
  and other text editors, and therefore orthogonal to Perspective.
- Tab Bar maintains window layouts (with optional names). In this, it is similar
  to Perspective. Unlike Perspective, it does not support buffer lists. Using
  Perspective and Tab Bar at the same time is not recommended at this time,
  since the tab list is global (i.e., will show up in all perspectives) and is
  likely to cause confusion. It would be an interesting future feature for
  Perspective to adopt the tab bar and allow keeping a distinct set of tabs
  per-perspective.


## Compatibility

Perspective does not work with [Emacs
`desktop.el`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html).
This is because Perspective state stores buffer and window information in frame
parameters, and `desktop-save-mode` does not support saving those types of data.

Instead, Perspective provides its own [disk save and
load](#saving-sessions-to-disk) feature, which cleanly saves perspectives.


## Installation

You should install Perspective from [MELPA](https://melpa.org/) or [MELPA Stable](https://stable.melpa.org/).

Users of [`use-package`](https://github.com/jwiegley/use-package) can install Perspective as follows:

```elisp
(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))
```

Replace the binding for `C-x C-b`, the default Emacs buffer switcher, with one
of the nicer implementations described in the [Buffer
switchers](#buffer-switchers) section.

If not using `use-package`, put `perspective.el` from this source repository
somewhere on your load path, and use something similar to this:

```elisp
(require 'perspective)
(global-set-key (kbd "C-x C-b") 'persp-list-buffers)
(customize-set-variable 'persp-mode-prefix-key (kbd "C-c M-p"))
(persp-mode)
```

Users of Debian 9 or later or Ubuntu 16.04 or later may simply `apt-get install
elpa-perspective`, though be aware that the stable version provided in these
repositories is likely to be (extremely) outdated.


## Usage

To activate Perspective, use `(persp-mode)`. This creates a single default
`main` perpsective.

> :information_source: Since the release of Emacs 28, Perspective no longer
> ships with a default command prefix. Users should pick a prefix comfortable
> for them. In the days of Emacs 27 and earlier, the default prefix was `C-x x`.
> This conflicts with bindings built into Emacs 28.

To set a prefix key for all Perspective commands, customize
`persp-mode-prefix-key`. Reasonable choices include `C-x x` (for users who don't
care about the Emacs buffer-related commands this shadows), `C-z` (for users who
don't suspend Emacs to shell background), `C-c C-p` (for users who don't mind
the conflicting keys with `org-mode` and `markdown-mode`), `C-c M-p` (for users
who don't mind the slightly awkward chord), and `H-p` (for users who don't mind
relying exclusively on a non-standard Hyper modifier).

The actual command keys (the ones pressed after the prefix) are defined in
`perspective-map`. Here are the main commands defined in `perspective-map`:

- `s` — `persp-switch`: Query a perspective to switch to, or create
- `` ` `` — `persp-switch-by-number`: Switch to perspective by number, or switch
  quickly using numbers `1, 2, 3.. 0` as prefix args; note this will probably be
  most useful with `persp-sort` set to `'created`
- `k` — `persp-remove-buffer`: Query a buffer to remove from current perspective
- `c` — `persp-kill` : Query a perspective to kill
- `r` — `persp-rename`: Rename current perspective
- `a` — `persp-add-buffer`: Query an open buffer to add to current perspective
- `A` — `persp-set-buffer`: Add buffer to current perspective and remove it from all others
- `b` - `persp-switch-to-buffer`: Like `switch-to-buffer`; includes all buffers
  from all perspectives; changes perspective if necessary
- `i` — `persp-import`: Import a given perspective from another frame.
- `n`, `<right>` — `persp-next`: Switch to next perspective
- `p`, `<left>` — `persp-prev`: Switch to previous perspective
- `m` — `persp-merge`: Temporarily merge the buffers from one perspective into another
- `u` — `persp-unmerge`: Undo the effects of a `persp-merge`
- `g` — `persp-add-buffer-to-frame-global`: Add buffer to a frame-specific "global" perspective
- `C-s` — `persp-state-save`: Save all perspectives in all frames to a file
- `C-l` — `persp-state-load`: Load all perspectives from a file


### Buffer Switching

Since Perspective maintains distinct buffer lists for each perspective, it helps
to use Perspective-aware methods for buffer switching.

Since Emacs 27.1, the commands `previous-buffer` and `next-buffer` can be made
Perspective-aware using the `switch-to-prev-buffer-skip` variable as follows:

```elisp
(setq switch-to-prev-buffer-skip
      (lambda (win buff bury-or-kill)
        (not (persp-is-current-buffer buff))))
```

When using one of the following buffer switchers, you will only be prompted for
buffers in the current perspective and the frame-specific "global" shared
perspective. (The `persp-add-buffer-to-frame-global` command adds a buffer to
this special frame-specific perspective, whose name is determined by
`persp-frame-global-perspective-name` and defaults to `GLOBAL`.)


**Ido**: [Interactive Do (Ido,
`ido-mode`)](https://www.gnu.org/software/emacs/manual/html_node/ido/index.html),
in particular its `ido-switch-buffer` command, is automatically
Perspective-aware when `persp-mode` is enabled.

**list-buffers / buffer-menu**: Perspective provides wrappers for the similar
[`list-buffers` and
`buffer-menu`](https://www.gnu.org/software/emacs/manual/html_node/emacs/List-Buffers.html):
`persp-list-buffers` and `persp-buffer-menu`. (Note that Emacs binds `C-x C-b`
to `list-buffers` by default.) When these functions are called normally, they
show the buffer menu filtered by the current perspective. With a prefix
argument, they show the buffer menu of all the buffers in all perspectives. (The
difference between `list-buffers` and `buffer-menu`: the former calls
`display-buffer`, i.e., may split windows depending on `display-buffer-alist`,
and the latter calls `switch-to-buffer`, i.e., flips the current window to the
buffer list buffer.)

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

If you want to group buffers by persp-name in ibuffer buffer, use
`persp-ibuffer-set-filter-groups`. Or, make it the default:
```
(add-hook 'ibuffer-hook
          (lambda ()
            (persp-ibuffer-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))
```

**Helm**: Perspective ships with buffer-listing advice for Helm, so Helm's
buffer listing code should be automatically Perspective-aware when `persp-mode`
is enabled. (Older versions of Helm relied on the machinery of `ido-mode` for
listing buffers, so they did not require this advice; see [`this Helm
commit`](https://github.com/emacs-helm/helm/commit/f7fa3a9e0ef1f69c42e0c513d02c9f76ea9a4344)
and [`this Perspective
commit`](https://github.com/nex3/perspective-el/commit/c2d3542418967b55f05d5b5ba71c9fbfe4cd3d4f)
for details.) If `helm-buffers-list` is called with a prefix argument, it will
show buffers in all perspectives. In addition, Perspective adds actions to
`helm-buffers-list` to add buffers to the current perspective (mainly relevant
to the prefix-argument version) and to remove buffers from the current
perspective.

**Consult**: Perspective provides `persp-consult-source` source that will list
buffers in current perspective. You can hide default buffer source
and add `persp-consult-source` to `consult-buffer-sources` for consult
to only list buffers in current perspective like so:

```emacs-lisp
(consult-customize consult--source-buffer :hidden t :default nil)
(add-to-list 'consult-buffer-sources persp-consult-source)
```
Note that you can still access list of all buffers in all perspectives by
[narrowing](https://github.com/minad/consult#narrowing-and-grouping)
using prefix `b`.

**Ivy / Counsel**: Perspective provides two commands for listing buffers using
Ivy and Counsel: `persp-ivy-switch-buffer` and `persp-counsel-switch-buffer`.
When these functions are called normally, they show a list of buffers filtered
by the current perspective. With a prefix argument, they shows a list of buffers
in all perspectives. The distinction between the `ivy` and `counsel` versions is
the same as between `ivy-switch-buffer` and `counsel-switch-buffer`: the latter
shows a preview of the buffer to switch to, and the former does not.

It is a good idea to bind one these helper functions with the `:bind` form of
`use-package`. Or, if you do not use `use-package`, it can also be bound
globally, e.g.:

```emacs-lisp
(global-set-key (kbd "C-x C-b") (lambda (arg)
                                  (interactive "P")
                                  (if (fboundp 'persp-bs-show)
                                      (persp-bs-show arg)
                                    (bs-show "all"))))
```


### Notes on `completing-read` Enhancements

Users of a `completing-read` enhancement framework (such as Ivy,
[Selectrum](https://github.com/raxod502/selectrum), or
[Vertico](https://github.com/minad/vertico)) may wish to use the following two
functions:
- `persp-switch-to-buffer*` replaces `switch-to-buffer`
- `persp-kill-buffer*` replaces `kill-buffer`

Both these functions behave like the built-ins, but use `completing-read`
directly. When called normally, they list buffers filtered by the current
perspective. With a prefix argument, they list buffers in all perspectives.

The following sample `use-package` invocation changes Emacs default key bindings
to use the replacements:

```
(use-package perspective
  :bind (("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*))
  :config
  (persp-mode))
```


## Saving Sessions to Disk

A pair of functions, `persp-state-save` and `persp-state-load`, implement
perspective durability on disk. When called interactively, they prompt for files
to save sessions to and restore from.

A custom variable, `persp-state-default-file`, sets a default file to use for
saving and restoring perspectives. When it is set, `persp-state-save` may be
called non-interactively without an argument and it will save to the file
referenced by that variable. This makes it easy to automatically save
perspective sessions when Emacs exits:

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
  recently accessed) and `'created` (by order created) are available. Note that
  `persp-switch-by-number` is likely to be confusing when this is set to
  `'access`, as the numbers associated with a perspective will change all the time.
- `persp-interactive-completion-function`: Used for prompting for a perspective
  name. `completing-read` is the default, with `ido-completing-read` enabled
  with `ido-mode`. `ivy-completing-read` is broadly compatible, but
  unfortunately sorts alphabetically and therefore breaks the `persp-sort`
  setting. Helm, unfortunately, does not have a `completing-read` compatible
  implementation out of the box (`helm-completing-read-default-1` purports to be
  this but does not have the same `&optional` defaults). _`ido-completing-read`
  is the recommended setting here unless a `completing-read` enhancement
  framework is used._
- `persp-mode-prefix-key`: Changes the default key prefix for Perspective
  commands.
- `persp-state-default-file`: Changes the default file to use for saving and
  loading Perspective state.
- `persp-show-modestring`: Determines if Perspective should show its status in
  the modeline. It defaults to `t`, but can also be `nil` (turning off the
  modeline status display) or `'header` (which uses the header line instead of
  the modeline).
- `persp-modestring-short`: When set to `t`, show a shortened modeline string
  with only the current perspective instead of the full list. Defaults to `nil`.
- `persp-purge-initial-persp-on-save`: When set to `t`, will kill all buffers
  of the initial perspective upon calling `perps-state-save`. The buffers whose name
  match a regexp in the list `persp-purge-initial-persp-on-save-exceptions` won't
  get killed. This allows using the initial perspective as a kind of scratch space.

To change keys used after the prefix key, with `use-package` you can do:

    ;; remap n to N to switch to next perspective
    (use-package perspective
      :bind (
        :map perspective-map
          ("n" . nil)
          ("N" . persp-next)))

Or without `use-package`:

    (define-key perspective-map (kbd "n") nil)
    (define-key perspective-map (kbd "N") 'persp-next)


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
carefully preserving window layouts if Emacs just throws them away on a `M-x
compile`? The answer is to fix the broken defaults. This is fairly easy:

```emacs-lisp
(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)     ; avoid resizing
```

These settings do the following:

1. Tell `display-buffer` to reuse existing windows as much as possible,
   including in other frames. For example, if there is already a `*compilation*`
   buffer in a visible window, switch to that window. This means that Emacs will
   usually switch windows in a "do what I mean" manner for a warmed-up workflow
   (one with, say, a couple of source windows, a compilation output window, and
   a Magit window).
2. Prevent splits by telling `display-buffer` to switch to the target buffer in
   the _current_ window. For example, if there is no `*compilation*` buffer
   visible, then the buffer in whichever window was current when `compile` was
   run will be replaced with `*compilation*`. This may seem intrusive, since it
   changes out the current buffer, but keep in mind that most buffers popped up
   in this manner are easy to dismiss, either with a dedicated keybinding (often
   `q`) or the universally-applicable `kill-buffer`. This is easier than
   restoring window arrangements. It is also easier to handle for pre-arranged
   window layouts, since the appropriate command can simply be run in a window
   prepared for it in advance. (If this is a step too far, then replace
   `display-buffer-same-window` with `display-buffer-pop-up-window`.)

(An earlier version of this hint modified `display-buffer-alist` instead of
`display-buffer-base-action`. This was [too
aggressive](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=49069#25) and can
impact packages which may legitimately want to split windows.)

[Documentation of the Emacs framework responsible for "pop-up" windows,
`display-buffer`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Displaying-Buffers.html#Displaying-Buffers),
is dense and difficult to read, so there have been attempts to summarize the
most important bits:

- https://ess.r-project.org/Manual/ess.html#Controlling-buffer-display
- https://old.reddit.com/r/emacs/comments/cpdr6m/any_additional_docstutorials_on_displaybuffer_and/ews94n1/
- https://www.masteringemacs.org/article/demystifying-emacs-window-manager
