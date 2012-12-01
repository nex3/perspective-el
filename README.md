This package provides tagged workspaces in Emacs, similar to workspaces in
windows managers such as Awesome and XMonad (and somewhat similar to multiple
desktops in Gnome or Spaces in OS X).

perspective.el provides multiple workspaces (or "perspectives") for each Emacs
frame. This makes it easy to work on many separate projects without getting lost
in all the buffers.

Each perspective is composed of a window configuration and a set of buffers.
Switching to a perspective activates its window configuration, and when in a
perspective only its buffers are available by default.

It's recommended that you install perspective.el from [Marmalade][] using `M-x
package-install`. Alternately, you may put it in your load path and run
`(require 'perspective)`.

[Marmalade]: http://marmalade-repo.org/
