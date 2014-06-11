# VIMonad

VIMonad is a fork of XMonad that manages windows in a modal way like that in VIM. Some of the featurs include

* [Split layout](#layout): a 3-level nested layout that divides each workspace into a grid of rectangular areas each containing unlimited number of tabs (like [Notion](http://notion.sourceforge.net/), with added advantage of dynamic resizing)
* [Motion](#motion): navigating around windows, split panes, workspaces with countable motion keys
* [Command](#command): delete, yank windows with register + motion support
* [Macro](#macro): record and play macros
* [Visual](#visual): advanced visual mode support that allows selection of windows via motion keys
* [Prompt](#prompt): execute common actions via a variety of prompts that are capable of
    * dynamically displaying the response of an action (e.g., `find` and `grep`) as completeable options (for selecting files)
    * previewing files in the completion window
    * providing application-specific completion window (e.g, `taskwarrior`)

## Acknowledgements

VIMonad is built upon XMonad and it borrows a lot of great modules already existing in its Contrib library. I'd like to thank all the people who have made XMonad a fantastic window manager in the first place.

## Install

### Dependencies

* `cabal`: for installing packages
* [wmctrl](http://tomas.styblo.name/wmctrl/): for activating windows from the command line
* [FMD](http://github.com/lynnard/fmd) and [FMC](http://github.com/lynnard/fmc): if you'd like to use the radio service in VIMonad
* [taskwarrior](http://taskwarrior.org/): task management from the prompt
* [vimb](https://github.com/fanglingsu/vimb/): light-weight browser with its histories and bookmarks accessable from the prompt

### Steps

1. clone the repo somewhere
2. move all the scripts under [bin](bin) to a directory included in `$PATH`
3. copy files under [.xmonad](.xmonad) to `~/.xmonad` (note your old xmonad configuration will be overriden)
    * [.xmonad/xmonad.hs][xmonad.hs] serves as a template config file; modify it if you'd like
4. `cd` into [xmonad](xmonad); `cabal install`
5. `cd` into [XMonadContrib](XMonadContrib); `cabal install`

## Concepts

### Layout

![](images/layout.jpg)

(Zoom in to see how the layout is represented on the statusbar)

#### Hierarchy

~~~
workspace(s)
└── row(s)
    └── line(s)
        └── tab(s)
~~~

* **workspaces**: the conventional sense of workspace as used in most window managers
    * the workspace labels are the symbols before `:<name>` 
    * the system starts up with only one workspace `` ` `` (the tmp workspace)
    * new workspaces are added dynamically via [prompt](#workspace-prompt); the aim is to organize workspaces by **context** (each workspace for one task)
    * each workspace has its own *current directory*; this can be changed by `cd` in the [DynamicPrompt](#dynamic-prompt)
* **rows**: vertical columns each spanning the entire the height of the screen
    * each row has a label attached to it - the first row in the workspace gets `1`, the second gets `2`, so on and so forth; the entire *stream* of symbols used as labels can be found in the source
    * row labels are displayed in the statusbar (before each group of square brackets)
* **lines**: horizontal rectangular regions contained in *rows*, each holding an arbitrary number of tabs 
    * the lines don't have labels; however, you'll be able to reference lines using `[num]G` as in vim
* **tabs**: as the ones seen in modern browsers; each tab is a window
    * the tab labels are shown near the leftmost edge of each tab
    * each tab might have a different colorscheme according to the definition of the task group it belongs to e.g., in the image above, vim windows have a brownish tab color, whereas vimb windows have a green one
    * tabs are only shown for lines with more than one window (due to a bug in the tabbed layout, currently it's not possible to show tabs at all time)
* **minimized**: a special case is that windows can be minimized in each workspace
    * this happens when the window is deleted into a specifed register (see [delete](#delete))
    * or the window is moved (into the unnamed or specified register) (see [move](#move))
    * the minimized windows are shown in the statusbar with `|` between them and labelled with the names of the registers they belong to

#### Resizing

* `M-<`: shrink current row
* `M->`: expand current row
* `M-\`: restore default size for the current row
* `M--`: shrink current line
* `M-+`: expand current line
* `M-=`: restore default size for the current line

### Task group

A major concept of VIMonad is task group. A task group defines a groups of windows that share the same or similar characteristics e.g., vim terminal windows, browser windows, etc.

Each task group might have a `filterKey` to be referenced in motions. This can be a single key or a sequence of keys.

Checkout [xmonad.hs][xmonad.hs] for how to define task groups.

Checkout [Taskgroup.hs](XMonadContrib/XMonad/Vim/TaskGroup.hs) for source about task groups.

### Register

Registers server two purposes in VIMonad:

1. they act as *marks* attached to windows, by referencing which we can cycle within a specific group of windows
2. they also act as *registers* like those in vim - we can delete, move, yank windows into registers
    * when we delete or move windows into registers, they are actually minimized as mentioned before
    * when we yank windows into registers, the windows stay in their original places

## Keys

Some points to note: 

* to perform any key sequence mentioned below, you need to press the first character with the modMask defined by you, e.g., to trigger `g2w` (move down 2 windows), press `M-g` followed by `2` and `w`.
    * exceptions are keystrokes with a dash inside, e.g., `M1-<tab>`, for which you press the exact key combo
* `{}` measn the key inside is not needed when the motion is passed as an argument to a command
* `[]` means the key inside is optional
* `<num>` can be any number from 2 to 9
* `<tab>` means any tab label in the current *line*
* `<row>` means any row label in the current workspace
* `<workspace>` means any workspace label in the current X session
* `<reg>` means a single character register; the character can be any that can be typed on the keyboard, however some of them are special
    * `/`: a special register that pops out a prompt for you to enter the exact arbitrary name for the register
    * `"`: the unnamed register used by [move](#move) and [paste](#paste)
        * as in vim, the unnamed register pushes content through registers `1` to `9`
    * `*`: always references the minimized windows that aren't present in any other register
    * `'`: always references the last visited window
    * an uppercase letter register *appends* the content to its lowercase letter register; the lowercase register *replaces* its original content (like in vim)
        * e.g., `'Adw` deletes the window and *appends* it to register `a`, whereaas `'adw` deletes the window and it then *replaces* the original content in register `a`
* `<group>` means the `filterKey` of a task group in the current workspace

### Motion

* `f<tab>`: moves to the tab with label `<tab>`
* `M1-<tab>`: leaps to the given tab 
    * the difference between *leap* and `f` is that *leap* references the tab without travelling along any window
    * when used as selection *leap* only selects the given specific tab
* `f C-<row>`: moves to the given row
* `C-<row>`: leaps to the given row
* `f M-<workspace>`: moves to the given workspace
* `M-<workspace>`: leaps to the given workspace
* `{g}[<num>]b`: the number of windows/tabs backwards
* `b`: one window/tab backwards
* `{g}[<num>]w`: the number of windows/tabs forward
* `w`: one window/tab forward
* `{g}0`: to the beginning of the line
* `{g}$`: travel to the end of the line
* `{g} C-0`: to the first row
* `{g} C-$`: to the last row
* `{g} M-0`: to the first workspace
* `{g} M-$`: to the last workspace
* `gg`: go to the top line
* `gG`: go to the bottom line
* `{g}[<num>]G`: go to the n'th line
* `g<group>`: cycle to the next window in `<group>`
    * in selection mode this selects all windows in that group in the current workspace
* `G<group>`: cycle to the previous window in `<group>`
* `{g}'<reg>`: cycle to the next window in the `<reg>`
    * in selection mode this selects all windows in that register
* `{g}[<num>]k`: to the `<num>`'th line up
* `k`: one line up
* `{g}[<num>]j`: to the `<num>`'th line down
* `j`: one line down
* `{g}[<num>]h`: to the `<num>`'th row left
* `h`: one row left
* `{g}[<num>]l`: to the `<num>`'th row right
* `l`: one row right
* `{g}[<num>][`: to the `<num>`'th workspace left
* `[`: one workspace left
* `{g}[<num>]]`: to the `<num>`'th workspace right
* `]`: one workspace right

### Command

### Macro

### Visual

### Prompt

#### Workspace Prompt

#### Dynamic Prompt

[xmonad.hs]: .xmonad/xmonad.hs "VIMonad template configuration"
