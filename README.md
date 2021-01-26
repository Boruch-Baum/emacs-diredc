[![MELPA](https://melpa.org/packages/diredc-badge.svg)](https://melpa.org/#/diredc) [![MELPA Stable](https://stable.melpa.org/packages/diredc-badge.svg)](https://stable.melpa.org/#/diredc)
# diredc - extensions to emacs dired-mode

This package extends emacs' `dired-mode` with features found in almost
all file managers, and also some unique features:

   * Resilient dedicated dual-pane frame.
     * similar look to `midnight commander`.
     * intelligent recovery of manually altered frame configuration
     * exit diredc/dired cleanly and totally
   * Navigable directory history
     * backward, forward, or to a direct history entry
   * File quick-preview mode
     * inspired by, and similar to, midnight commander's "C-x q"
   * Current file's supplemental information in minibuffer (optional)
     * eg. output from `getfattr`, `getfacl`, `stat`, `exif`.
   * Multiple panel views
     * inspired by, and similar to, midnight commander's "M-t"
       * superior configurability
       * directly choose a specific panel view, or toggle to next
   * Trash management
     * per xfreedesktop standard
     * restore trashed files to their original locations
     * empty the trash, along with its administrative overhead
     * view trash summary information
   * Navigate "up" n parent directories ("^" or C-u ^")
   * Quick shell window
     * choose your default shell / terminal emulation mode
     * choose your default shell program
     * easily opt for pre-configured alternatives
     * useful pre-defined shell variables
       * $d1, $d2  dired-directory in this/other pane
       * $f1, $f2  current dired file in this/other pane
       * $t1, $t2  tagged elements in this other pane
         * as a shell array variable, if supported by the shell
   * Bookmark support
   * Edit dired buffers (really `wdired-mode`, not `diredc`)
   * Set both panels to same directory (use "=" or "C-u =")
     * inspired by `midnight commander's "M-i"


## Bonus customization features
   * Customize colors for chmod bits (font-lock)
   * toggle display of "hidden" or "undesirable" files (dired-omit mode)
   * auto-refresh buffers (auto-revert-mode)
   * highlight current line (hl-line-mode)
   * don`t wrap long lines (toggle-truncate-lines)
   * to disable:
     * option 1: M-x customize-variable diredc-bonus-configuration
     * option 2: (setq diredc-bonus-configuration nil)


## Dependencies (all are already part of emacs):

   * autorevert -- for auto-revert-mode
   * dired       -- ... (doh) ...
   * dired-x     -- for dired-guess-default, dired-omit-mode
   * dired-aux   -- for dired-compress-files
   * hl-line     -- for hl-line-mode
   * term        -- for term-line-mode, term-send-input
   * view        -- for view-mode

   Suggested (not part of emacs):
   * popup       -- for popup-menu*


## Installation:

 1) Evaluate or load this file.

 2) I recommend defining a global keybinding for function `diredc',
    with a preference for Shift-F11, as follows:

       `(global-set-key (kbd "S-<f11>") 'diredc))`

    An alternative or additional option would be:

       `(global-set-key [remap dired-other-frame] 'diredc)`


## Operation:

 Running `diredc` an initial time creates and selects a frame named
 `diredc` with two side-by-side `dired` windows / buffers. Repeating
 the command will return you to your prior frame / window / buffer.
 Subsequent use of the command continues to toggle back and forth
 to/from the named `diredc` frame. Navigation from one `dired` panel
 to another can be accomplished using `<TAB>` or `S-<TAB>`. As long
 as you are in `diredc` mode, navigating to new directories should
 not accumulate additional `dired` buffers and your directory
 navigation history for each panel should be remembered. If ever you
 find that the frame configuration has become botched, or you
 somehow accumulate or have lost `dired` buffers, Run M-x
 `diredc-recover`. You can also cleanly kill all `dired` buffers and
 the `diredc` frame using `C-q` (M-x `diredc-quit`). And, if you
 want to use `dired` without the `diredc` features, run M-x
 `diredc-mode` to toggle the mode off.

 As mentioned above, each `dired` panel now "remembers" its
 navigation history. The history can be traversed sequentially
 backward `C-<left>` or forward `C-<right>` without losing elements,
 and can be viewed and traversed non-sequentially using `C-u /`. Use
 `/` to directly navigate to a directory not `nearby`.

 A `file preview` mode can be entered or exited using `C-x q` (M-x
 `diredc-browse-mode`). In that mode, whenever a `dired` buffer's
 POINT is on a file's line, that file will be opened on the other
 pane, in read-only emacs `view-mode` (see there for the navigation
 and other features of that mode). The `view-mode` buffer is deleted
 automatically when you either disable the mode or you move point to
 another line in the `dired` buffer. Use `<TAB>` or `S-<TAB>` to
 navigate between the `dired` buffer window and the file preview
 window.

 The traditional `dired` operations that `find` or `open` a file
 should do so to a separate frame, most likely the one from which
 you came to the `diredc` frame.

 The display format of `dired` buffers can be "hot-swapped" using
 `M-t` (M-x `diredc-display-toggle`). Use `C-u M-t` to select from
 available display formats, and customize the list using defcustom
 variable `diredc-display--listing-switches-list`. Four views are
 provided by default, all long-format but with different file
 block-sizes (byte, Kb, Mb), and several other differences.

 While emacs does have a native defcustom variable
 `delete-by-moving-to-trash` to control whether to "really" delete
 files, `diredc` allows one or more trashed items to be restored,
 allows the trash to be emptied, and conveniently present trash
 statistics. Here are the relevant commands and their default
 keybindings. See each's docstring for more details:

     C-<delete> SPC           `diredc-trash-toggle`
     C-<delete> <insertchar>  `diredc-trash-toggle`

     C-<delete> i             `diredc-trash-info`

     C-<delete> j             `diredc-trash-view`
     C-<delete> v             `diredc-trash-view`

     C-<delete> C-<delete>    `diredc-trash-empty`
     C-<delete> x             `diredc-trash-empty`

     C-<delete> r             `diredc-trash-restore`

     C-k                      `diredc-trash-quick-delete`

     C-<delete> ?             `diredc-trash-assistant`

 A limitation in `dired` is its inability to natively present a
 file's supplemental information, such as its possible extended
 access control list or extended file attributes. `diredc` allows
 this and more to be presented in the minibuffer echo area as you
 navigate a `diredc` buffer. Use M-x `diredc-show-more-file-info` to
 toggle through the default possibilities, or customize the
 `diredc-show-more-file-info-list` to present the metadata of your
 choice.

     C-c ?     `diredc-show-more-file-info`

 `diredc` brings bookmarks to dired:

     C-c + a   `diredc-bookmark-add`
     C-c + j   `diredc-bookmark-jump`
     C-c + e   `diredc-bookmark-edit`

 Emacs has a nifty mode to "edit" a `dired` buffer, bringing the
 power of emacs to the application of renaming files. `diredc` just
 gives you the little bit of extra help with convenient keybindings
 `E` and `e` to enter the mode. If you're happy with your edits, you
 apply them and exit the mode with `C-c C-c`, or abort your editing
 session with `C-c C-k`.

 The `diredc-shell` command opens up any type of emacs shell or
 terminal emulator window and pre-seeds it with useful `dired`
 values (see section "Extra Features", below).

     '         `diredc-shell`
     C-c !     `diredc-shell`
     C-c C-k   `diredc-shell-kill`

 For further information, KRTM, the package's docstrings and the
 package's defcustom group.


## Configuration

 You can browse and edit this mode's list of `defcustom`s using "M-x
 `customize-group` diredc", but there isn't too much to be found
 there. Separately, you might want to redefine the default
 keybindings, but otherwise there is nothing really important about
 `diredc' itself to configure.

 `dired` mode itself, however, is a complex and highly configurable
 package that has been under development for over 25 years. That's a
 long time for options to accumulate and for opinions to multiply.
 The `diredc` developer (ahem: me) has considerately imposed his
 preferences upon you by default, in a way trivial to over-ride. If
 you don't want them, toggle the value of defcustom
 `diredc-bonus-configuration` to nil, an the settings will revert
 upon selecting new buffers. The bonus customization features are
 listed above, in the 'Commentary' section, or you could peek at the
 source code of function `diredc-bonus-configuration`.

 The colorization for each buffer's selected line is set as part of
 `diredc-bonus-configuration`, but because it's controlled by
 `hl-line-mode`; you can independently toggle the feature
 per-buffer, and you can change the highlighting colors using "M-x
 `customize-face` hl-line". The colorization of the chmod bits are
 also set as part of `diredc-bonus-configuration`; you can find
 their definitions and edit them using  "M-x
 `customize-group` diredc".


## Extra Features:

 *] Navigating to a parent directory with `dired-up-directory`
    (default: `^`) can use the prefix-argument to navigate multiple
    levels in one operation.

 *] Use `dired-hist-change-directory` (default: `/`) to jump to a
    location not nearby without losing the current dired buffer's
    history.

 *] Use the `prefix-argument` with `diredc-hist-change-directory` to
    have `diredc-hist-select` display all elements of the Dired
    buffer's history and allow you to jump directly to any of them.

 *] Use `diredc-hist-duplicate` (default: `=`) to either navigate to
    another `dired` buffer to your current one's directory, or with
    the `prefix-argument` to navigate your current `dired` buffer to
    another's directory.

 *] Modify data structure `diredc-recover-schemes` to apply your own
    custom recovery strategies. Share them for others` benefit!

 *) Use `diredc-trash-quick-delete` (default: `C-k`) on a POINT or a
    REGION to quick-delete the selected files. Use the prefix-arg to
    toggle between "trashing" or deleting.

 *] When `diredc-hist-mode` is disabled, the following functions
    continue to operate, but without updating the history records,
    so you can use them as your default `dired` functions even if
    you don`t always want to use `diredc-hist-mode`.

      `diredc-hist-change-directory`
      `diredc-hist-up-directory`
      `diredc-hist-duplicate`
      `diredc-hist-find-file`
      `diredc-hist-find-file-other-window`
      `diredc-hist-find-alternate-file`

 *] `diredc` passes to the shell/terminal-emulator instance the
    following shell variables:

      $$d1 - this `diredc` windows's directory name
      $d2 - directory name of other visible `diredc` window
      $f1 - this `diredc` window's file name at POINT
      $f2 - file name at POINT  of other visible `diredc` window
      $t1 - this `diredc` window's list of tagged file names
      $t2 - list of tagged file names  of other visible `diredc` window

    If the selected shell supports array variables, then $t1 and $t2
    will be set as such; Otherwise, elements will be quoted and
    delimited with a space.

 *) universal fallback guess shell command(s)


## Compatibility

 This package has been tested under debian linux emacs version 26.1
 and a recent snapshot of emacs 28.0. The main compatibility issue to
 be aware of is that this suite needs to modify[1] a single line in
 function `dired-internal-no-select` of the standard emacs file
 `dired.el`. This was accomplished by advising a wrapper function
 `diredc--advice--dired-internal-noselect` around the original. If
 that function ever changes, that advice function and this suite will
 need to account for that.

 [1] emacs bug #44023: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=44023"

## Known limitations

 *] Line highlighting of the non-selected `diredc` buffer is limited
    by package `hl-line` to be the same face as that of the selected
    buffer.


## Colophon

* Copyright © 2020, Boruch Baum <boruch_baum@gmx.com>
* Author/Maintainer: Boruch Baum <boruch_baum@gmx.com>
* Homepage: https://github.com/Boruch-Baum/emacs-diredc
* License: GPL3+
