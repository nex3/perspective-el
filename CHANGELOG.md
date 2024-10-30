# Changelog

All notable changes to this project will be documented in this file. Note that
Perspective was started in 2008 and this log was only added in 2021.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [2.19] — 2024-10-30

### Fixed

- `persp-ibuffer-generate-filter-groups`: load `ibuf-ext` library if needed ([#202](https://github.com/nex3/perspective-el/issues/202)).


### Added

- `persp-kill-other-buffers`: kill other buffers in the current perspectives.


## [2.18] — 2022-09-20

### Added

- `persp-purge-initial-persp-on-save`: support for optionally killing all buffers of the initial perspective upon calling `perps-state-save`, thus treating the initial perspective as a disposable scratch area.
- `persp-add-buffer-to-frame-global`: support for special frame-specific "global" perspectives; buffers which they contain will appear in Perspective-aware buffer listings in _all_ perspectives in their containing frames
- `persp-switch-to-scratch-buffer`: interactive function to switch to the current perspective's scratch buffer, creating one if missing.
- `persp-forget-buffer`: disassociate buffer with perspective without the risk of killing it.  This balances `persp-add-buffer`.  Newly created buffers via `get-buffer-create` are rogue buffers not found in any perspective, this function allows to get back to that state.
- Support for using Consult's `consult-buffer` as a Perspective-aware buffer switcher.
- `persp-merge` and `persp-unmerge`: temporarily import buffers from one perspective into another.


### Changed

- Avoid killing the last buffer in a perspective by default (`persp-avoid-killing-last-buffer-in-perspective`, default `t`; this was formerly the experimental feature flag `persp-feature-flag-prevent-killing-last-buffer-in-perspective` which defaulted to `nil`).
- **Breaking change:** (Emacs 28+ only): no longer provide a default `persp-mode-prefix-key`. It was `C-x x` in the past, which conflicts with key bindings shipping with Emacs 28. Users of Emacs 28 must now pick their own prefix key. The default remains unchanged for users of Emacs 27 and earlier.
- `persp-remove-buffer`: do not kill/remove a perspective's last left buffer.
- `persp-switch-to-buffer*`: tag with `'buffer` category so Marginalia can add its annotations.
- `persp-other-buffer`: rewrite so it respects ignored buffer list.


### Fixed

- `persp-new`: enable `initial-major-mode` only if the scratch buffer is in `fundamental-mode`.
- `persp-new`: properly substitute command keys when inserting `initial-scratch-message` into scratch buffers.
- `persp-new`: do not recreate existing perspectives.  This prevents from resetting perspectives to a state where in the perspective there's only the scratch buffer.
- `persp-reset-windows`: set `switch-to-buffer-preserve-window-point` to `nil` before calling `delete-window`, that up to Emacs 27.2 updates `window-prev-buffers` of all windows, unless the former is turned off.
- `persp-remove-buffer`: force update the `current-buffer` to the current window's buffer due to `with-selected-window` saving/restoring the `current-buffer` when executing it's BODY.  This properly updates the `current-buffer` to what should be the real current buffer when burying the current buffer.
- `persp-activate`: force update the `current-buffer` to the current window's buffer due to `make-persp` saving/restoring the `current-buffer` when executing it's BODY.  This properly updates the `current-buffer` to what should be the real current buffer when switching to a new perspective.
- `persp-add-buffer`: discard unexisting buffer as argument.
- Added a workaround for potential problems caused by recursive minibuffer use.
- Made activating `persp-mode` repeatedly idempotent (should fix [interactive enable-theme invocation bug](https://github.com/nex3/perspective-el/issues/185)).


## [2.17] — 2021-09-18

### Added

- Improved Helm integration. `helm-buffers-list` now lists buffers in all perspectives when called with a prefix argument. It also now has actions to add to the current perspective, and to remove buffers from the current perspective.


### Changed

- Rewrote Ivy / Counsel buffer switchers to make better use of the Ivy API. As a result, <kbd>C-k</kbd> to kill buffers directly from the switcher now works.


## [2.16] — 2021-07-31

### Added

- `persp-kill-others`
- Make `xref` rings perspective-specific (so popping back will not inadvertently jump to a file in a different perspective).
- Support for grouping buffers by `persp-name` in ibuffer.


### Changed

- Switched from now-defunct Travis CI to GitHub Actions.


### Fixed

- `header-line-format` wrangling bug.
