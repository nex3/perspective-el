# Changelog

All notable changes to this project will be documented in this file. Note that
Perspective was started in 2008 and this log was only added in 2021.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).


## [Unreleased]

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
