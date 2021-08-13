# Changelog

All notable changes to this project will be documented in this file. Note that
Perspective was started in 2008 and this log was only added in 2021.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## Unreleased

### ERT tests added

- `persp-test-buffer-in-persps`: utility function to verify that a buffer is in specified perspectives only.
- `baisc-persp-test-kill-extra-buffers`: evaluate `persp-test-kill-extra-buffers`
- `persp-test-kill-extra-buffers`: utility function for cleaning up and killing extra buffers.
- `basic-persp-test-match-scratch-buffers`: evaluate `persp-test-match-scratch-buffers`.
- `persp-test-match-scratch-buffers`: utility function to list live scratch buffers.
- `basic-persp-test-with-persp`: evaluate `persp-test-with-persp` macro.


### ERT tests changed

- `persp-test-with-persp`: `unwind-protect` BODY to ensure a proper cleanup upon failures.
- `persp-test-with-persp`: kill other perspectives than the main perspective when cleaning up.


### ERT tests fixed

- `persp-test-with-persp`: verify that a `*scratch* (NAME)` buffer exists before killing it.


### Fixed

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
