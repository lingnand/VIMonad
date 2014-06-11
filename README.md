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

* [cabal](http://www.haskell.org/cabal/): for installing packages
* [wmctrl](http://tomas.styblo.name/wmctrl/): for activating windows from the command line
* [FMD](http://github.com/lynnard/fmd) and [FMC](http://github.com/lynnard/fmc): if you'd like to use the radio service in VIMonad
* [taskwarrior](http://taskwarrior.org/): task management from the prompt
* [vimb](https://github.com/fanglingsu/vimb/): light-weight browser with its histories and bookmarks accessable from the prompt
* [xdotool](http://www.semicomplete.com/projects/xdotool/xdotool.xhtml): for playing back text macros

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

Each task group also comes with a construction function that constructs a new instance of the group. 

Checkout [xmonad.hs][xmonad.hs] for how to define task groups.

Checkout [Taskgroup.hs](XMonadContrib/XMonad/Vim/TaskGroup.hs) for source about task groups.

### Register

Registers server two purposes in VIMonad:

1. they act as *marks* attached to windows, by referencing which we can cycle within a specific group of windows
2. they also act as *registers* like those in vim - we can delete, move, yank windows into registers
    * when we delete or move windows into registers, they are actually minimized as mentioned before
    * when we yank windows into registers, the windows stay in their original places

Attached registers are displayed for each window within the sqaure brackets of its tab label as `'<reg>['<another reg>]`.

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
* `<macro>` means a single- or multi- character macro register; the characters can be any that can be typed on the keyboard, except
    * `/`: a special register that pops out a prompt for you to enter the exact name for the macro

### Motion

* `f<tab>`: moves to the tab with label `<tab>`
* `M1-<tab>`: leaps to the given tab 
    * the difference between *leap* and `f` is that *leap* references the tab without travelling along any window
    * when used as selection *leap* only selects the given specific tab
* `f C-<row>`: moves to the given row
* `C-<row>`: leaps to the given row
    * when used as selection only selects the given specific row
* `f M-<workspace>`: moves to the given workspace
* `M-<workspace>`: leaps to the given workspace
    * when used as selection only selects the given specific workspace
* `[{g}<num>]b`: to the `<num>`'th windows/tabs back
* `[{g}<num>]w`: to the `<num>`'th windows/tabs forward
* `{g}0`: to the beginning of the line
* `{g}$`: to the end of the line
* `{g} C-0`: to the first row
* `{g} C-$`: to the last row
* `{g} M-0`: to the first workspace
* `{g} M-$`: to the last workspace
* `gg`: go to the top line
    * when used as selection this performs line-wise selection i.e. from the current line to the first line
* `{g}G`: go to the bottom line
    * when used as selection this performs line-wise selection
* `{g}<num>G`: go to the n'th line
    * when used as selection this performs line-wise selection
* `g<group>`: cycle to the next window in `<group>`
    * when used as selection this selects all windows in that group in the current workspace
* `G<group>`: cycle to the previous window in `<group>`
* `{g}'<reg>`: cycle to the next window in the `<reg>`
    * when used as selection this selects all windows in that register
* `[{g}<num>]k`: to the `<num>`'th line up
    * when used as selection this performs line-wise selection
* `[{g}<num>]j`: to the `<num>`'th line down
    * when used as selection this performs line-wise selection
* `[{g}<num>]h`: to the `<num>`'th row back
    * when used as selection this performs row-wise selection
* `[{g}<num>]l`: to the `<num>`'th row forward
    * when used as selection this performs row-wise selection
* `[{g}<num>][`: to the `<num>`'th workspace back
    * when used as selection this performs workspace-wise selection
* `[{g}<num>]]`: to the `<num>`'th workspace forward
    * when used as selection this performs workspace-wise selection

### History jumplist

* `M-o`: move to the previous window in the jumplist
* `M-i`: move to the next window in the jumplist

### Object

Objects are used with commands, where the same command is applied to all the windows contained in the object.

* `[<num>]<command>`: apply the command to `<num>` of lines starting from the current line
    * `<command>` means the same letter as the command, e.g., `dd` deletes the current line
* `[<num>]r`: apply the command to `<num>` of rows starting from the current one
* `[<num>]s`: apply the command to `<num>` of workspaces starting from the current one

### Command

#### Delete

    ['<reg>]d<motion/object>

* if `'<reg>` is given, minimize the windows selected by the `<motion/object>` and then attach them to register `<reg>`
* else delete the windows selected by `<motion/object>`

#### Move

    ['<reg>]m<motion/object>

Same as [delete](#delete), except

* when `'<reg>` is not given, minimize the windows into `"` register
    * push windows in registers `"12345678` one step forward to `123456789` and then forgetting the windows in register `9`
    * the 'forgotten' windows are not deleted, they are just not referenced by any register anymore (except `*`)

#### Yank

    '<reg>y<motion/object>

yank the windows selected by `<motion/object>` into register `<reg>`

### Macro

Macros are by default stored under `~/.macros`. There are two types of macros

1. a saved list of key sequences in VIMonad; these can be recorded directly in VIMonad
2. an arbitrary piece of text; when triggered it is typed into the focused window via `xdotool`, like a snippet

#### Actions

* `q<macro>`: start recording the ensuing key sequences into `<macro>`
* `q<Esc>`: stop recording
* `a<macro>`: play the macro stored in `<macro>`

### Visual

### Prompt

#### Workspace Prompt

#### Dynamic Prompt

[xmonad.hs]: .xmonad/xmonad.hs "VIMonad template configuration"
