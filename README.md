# VIMonad

VIMonad is a fork of XMonad that manages windows in a modal way like that in VIM. Some of the featurs include

* [Split layout](#layout): a 3-level nested layout that divides each workspace into a grid of rectangular areas each containing unlimited number of tabs (like [Notion](http://notion.sourceforge.net/), with added advantage of dynamic resizing)
* [Motion](#motion): navigating around windows, split panes, workspaces with countable motion keys
* [Command](#command): delete, yank windows with register + motion support
* [Macro](#macro): record and play macros
* [Visual](#visual): advanced visual mode support that allows selection of windows via motion keys
* [Prompt](#prompt): execute common actions via advanced prompts that is capable of
    * dynamically displaying the response of the action (e.g., `find` and `grep`) as completable options (for selecting files)
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

### Steps

1. clone the repo somewhere
2. move all the scripts under [bin](bin) to a directory included in `$PATH`
3. copy files under [.xmonad](.xmonad) to `~/.xmonad` (note your old xmonad configuration will be overriden)
    * [.xmonad/xmonad.hs][xmonad.hs] serves as a template config file; modify it if you'd like
4. `cd` into [xmonad](xmonad); `cabal install`
5. `cd` into [XMonadContrib](XMonadContrib); `cabal install`

## Documentation

Some points to note: 

1. to perform any key sequence mentioned below, you need to press the first character with the modMask defined by you, e.g., to trigger `g2w` (move down 2 windows), press `M-g` followed by `2` and `w`.
2. 

### Task group

A major concept of VIMonad is task group. A task group defines a groups of windows that share the same or similar characteristics e.g., vim terminal windows, browser windows, etc.

Checkout [xmonad.hs][xmonad.hs] for how to define task groups.

Checkout [Taskgroup.hs](XMonadContrib/XMonad/Vim/TaskGroup.hs) for source about task groups.

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
    * row labels are displayed in the status bar (before each group of square brackets)
* **lines**: horizontal rectangular regions contained in *rows*, each holding an arbitrary number of tabs 
    * the lines don't have labels; however, you'll be able to reference lines using `[num]G` as in vim
* **tabs**: as the ones seen in modern browsers; each tab is a window
    * the tab labels are shown near the leftmost edge of each tab
    * each tab might have a different colorscheme according to the definition of the task group it belongs to e.g., in the image above, vim windows have a brownish tab color, whereas vimb (light-weight browser) windows have a green one
    * tabs are only shown for lines with more than one window (due to a bug in the tabbed layout, currently it's not possible to show tabs at all time)

#### Resizing

* `M-<`: shrink current row
* `M->`: expand current row
* `M-\`: restore default size for the current row
* `M--`: shrink current line
* `M-+`: expand current line
* `M-=`: restore default size for the current line

### Motion

* `f<tab>`: moves to the given tab
* `M1-<tab>`: leaps to the given tab (the difference with f<tab> is that it references that tab without moving along the tabs); so in the visual mode pressing M1-<tab> actually toggles just that window (toggling the passive tag of that window)
* `F<column>`: moves to the given column
* `C-<column>`: leaps to the given column; when toggling this should toggle off all the selection in that row
* `{g}[<num>]b`: the number of windows back (excluding the current window)
* `b`: the same as {g}1b
* `{g}[<num>]w`: the number of windows (including the current window)
* `w`: the same as {g}1w
* `{g}0`: to the beginning of the group (excluding the current window)
* `{g}$`: travel to the end of the group (including the current window)
* `gg`: Go to the top row
* `gG`: go to the bottom row
* `{g}[<num>]G`: go to the n'th row
* `g<group>`: the task group given by the symbol in the current workspace; gc means the group of the current window (Note this means you shouldn't have a group named c)
* `{g}['reg]`: go to the window in the reg; if passed as argument, then returns all the windows in that reg
* `{g}[<num>]k`: to the <num> row up
* `k`: same as {g}1k
* `{g}[<num>]j`: to the <num> row down
* `j`: same as {g}1j
* `{g}[<num>]h`: to the <num> column left
* `h`: same as {g}1h
* `{g}[<num>]l`: to the <num> column right
* `l`: same as {g}1l
* `{g}[<num>][`: to the <num> workspace left
* `[`: same as `{g}1[`
* `{g}[<num>]]`: to the <num> workspace right
* `]`: same as {g}1]

### Command

### Macro

### Visual

### Prompt

#### Workspace Prompt

#### Dynamic Prompt

[xmonad.hs]: .xmonad/xmonad.hs "VIMonad template configuration"
