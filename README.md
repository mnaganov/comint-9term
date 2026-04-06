# comint-9term: Advanced Terminal Emulation for Emacs Comint

`comint-9term` is an Emacs extension that brings advanced terminal emulation capabilities to `comint-mode` (used by `M-x shell`) and `compilation-mode`. It implements a hybrid model that bridges the gap between Emacs's infinite, line-oriented buffers and the fixed-grid, absolute-addressing model of "smart" terminals.

## Purpose

Standard Emacs `comint-mode` acts as a "dumb" terminal. While this is great for maintaining a searchable, persistent command history, it fails when tools use advanced ANSI escape sequences for in-place updates, such as:
- **Progress bars** (e.g., `apt`, `docker`, `pip`)
- **Status lines** at the bottom of the screen (e.g., `ninja`, `tmux`, `zsh` with fancy prompts)
- **Absolute cursor positioning** (CUP)

`comint-9term` enables these features without losing the benefits of a standard Emacs buffer. Inspired by the **9term** terminal from the Plan 9 operating system, it treats program output as a text buffer while supporting the subset of "smart" terminal commands needed for modern CLI tools.

## Key Features

- **Advanced ANSI Support:** Handles cursor movement (CUU, CUD, CUF, CUB, CHA), absolute positioning (CUP, HVP), and erasure (ED, EL).
- **Monotonic Viewport:** Implements a "sliding window" at the end of the buffer. This ensures that even when a tool jumps to the "top" of its viewport to draw a status bar, the operation is pinned to the current downward progress of the buffer, preserving historical scrollback.
- **Scroll Region Support (DECSTBM):** Correctly handles tools like `ninja` that pin status bars to a specific region of the terminal.
- **OSC Handling:** Supports Operating System Commands (OSC) for features like **Hyperlinks** and **Directory Tracking**, delegating to Emacs's built-in handlers.
- **Performance Optimized:** Uses marker reuse, line-count caching, and efficient buffer management to handle high-frequency output (like rapid progress bar updates) with minimal CPU overhead.
- **Window-Aware:** Automatically captures window dimensions, allowing absolute positioning to work correctly even in split Emacs windows.

## Installation & Setup

Add `comint-9term.el` to your `load-path` and require it:

```elisp
(add-to-list 'load-path "/path/to/comint-9term")
(require 'comint-9term)
```

The package automatically hooks into `comint-mode` and `compilation-shell-minor-mode`. To ensure it's active, you can also add it to your hooks explicitly:

```elisp
(add-hook 'comint-mode-hook #'comint-9term-setup)
(add-hook 'compilation-shell-minor-mode-hook #'comint-9term-setup)
```

`comint-9term` integrates with `window-adjust-process-window-size-function` to dynamically track the window size in Emacs.

## Usage Examples

- **Package Managers:** Run `sudo apt install ...` and see a smooth, in-place progress bar instead of pages of control character garbage.
- **Build Tools:** Use `ninja` or `cmake` and enjoy a pinned status line at the bottom of your compilation buffer.
- **Shell Prompts:** Use Zsh themes (like Powerlevel10k) that rely on `\r` and cursor movement for right-side prompts or transient status lines.

## Tracing and Debugging

If you encounter a tool with a "creative" approach to terminal output that causes issues, you can enable tracing to capture the raw escape sequences for analysis:

1. `M-x comint-9term-trace-mode`
2. Run the problematic command.
3. Inspect the `*comint-9term-trace*` buffer.
4. Use `comint-9term-replay-trace` to reproduce the behavior in a controlled environment.

## Background

For a deeper dive into the motivation behind this project and the history of terminal evolution (from paper teletypes to the "vibe coding" era), see the blog post: [On Terminals, Emacs, and AI Coding](https://mnaganov.github.io/2026/03/on-terminals-emacs-and-ai-coding.html).
