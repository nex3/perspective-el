# Changelog

All notable changes to this project will be documented in this file. Note that
Perspective was started in 2008 and this log was only added in 2021.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## Unreleased

### ERT tests added

- `basic-persp-forget-buffer`: evaluate `persp-forget-buffer`.
- `basic-persp-killing-buffers`: test killing buffers, a perspective's last left live buffer should not be killable/removable.
- `basic-persp-window-prev-buffers`: evaluate if `window-perv-buffers` gets buffers of other perspectives.
- `basic-persp-set-buffer`: evaluate `persp-set-buffer`.
- `basic-persp-add-buffer`: evaluate `persp-add-buffer`.
- `basic-persp-get-buffer-names`: evaluate `persp-get-buffer-names`.
- `basic-persp-get-buffers`: evaluate `persp-get-buffers`.
- `basic-persp-test-buffer-in-persps`: evaluate `persp-test-buffer-in-persps`.
- `persp-test-buffer-in-persps`: utility function to verify that a buffer is in specified perspectives only.
- `baisc-persp-test-kill-extra-buffers`: evaluate `persp-test-kill-extra-buffers`
- `persp-test-kill-extra-buffers`: utility function for cleaning up and killing extra buffers.
- `basic-persp-test-match-scratch-buffers`: evaluate `persp-test-match-scratch-buffers`.
- `persp-test-match-scratch-buffers`: utility function to list live scratch buffers.
- `basic-persp-test-with-persp`: evaluate `persp-test-with-persp` macro.


### ERT tests changed

- `basic-persp-switching`: tests the `current-buffer` too.
- `basic-persp-creation-deletion`: test killing all perspectives, main perspective included.
- `persp-test-with-persp`: `unwind-protect` BODY to ensure a proper cleanup upon failures.
- `persp-test-with-persp`: kill other perspectives than the main perspective when cleaning up.


### ERT tests fixed

- `persp-test-with-persp`: verify that a `*scratch* (NAME)` buffer exists before killing it.


### Added

- `persp-forget-buffer`: disassociate buffer with perspective without the risk of killing it.  This balances `persp-add-buffer`.  Newly created buffers via `get-buffer-create` are rogue buffers not found in any perspective, this function allows to get back to that state.
- `persp-maybe-kill-buffer`: designed as `kill-buffer-query-functions` hook to keep a perspective's last left buffer from being killed.
- `persp-get-buffer-names`: get any perspective's list of live buffers.
- `persp-get-buffers`: get any perspective's list of buffers.


### Changed

- `persp-mode`: add/remove `persp-maybe-kill-buffer` hook.
- `persp-kill`: switch `persp-maybe-kill-buffer` on/off to allow killing a perspective's last left buffer.
- `persp-set-buffer`: walk perspectives rather than using a while `persp-buffer-in-other-p` loop, since the former isn't prone to infinite loops.  This is needed due to buffers kept in perspectives by `persp-maybe-kill-buffer` and `persp-remove-buffer` when a buffer is a perspective`s last left buffer.
- `persp-remove-buffer`: do not kill/remove a perspective's last left buffer.
- `persp-remove-buffer`: when burying a buffer, walk windows rather than using a while loop, since the former isn't prone to infinite loops.
- `make-persp`: document that executing BODY saves/restores the `current-buffer`.
- `persp-set-buffer`: follow the coding style of `persp-add-buffer`.


### Fixed

- `persp-reset-windows`: set `switch-to-buffer-preserve-window-point` to `nil` before calling `delete-window`, that up to Emacs 27.2 updates `window-prev-buffers` of all windows, unless the former is turned off.
- `persp-remove-buffer`: force update the `current-buffer` to the current window's buffer due to `with-selected-window` saving/restoring the `current-buffer` when executing it's BODY.  This properly updates the `current-buffer` to what should be the real current buffer when burying the current buffer.
- `persp-activate`: force update the `current-buffer` to the current window's buffer due to `make-persp` saving/restoring the `current-buffer` when executing it's BODY.  This properly updates the `current-buffer` to what should be the real current buffer when switching to a new perspective.
- `persp-add-buffer`: discard unexisting buffer as argument.
- Added a workaround for potential problems caused by recursive minibuffer use.


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
