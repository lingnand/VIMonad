# VIMonad

VIMonad is a fork of XMonad that manages windows in a modal way like that in VIM. Some of the featurs include

* [Split layout](#split): a 3-level nested layout that divides each workspace into a grid of rectangular areas each containing unlimited number of tabs (like [Notion](http://notion.sourceforge.net/), with added advantage of dynamic resizing)
* [Motion](#motion): navigating around windows, split panes, workspaces with countable motion keys
* [Command](#command): delete, yank windows with register + motion support
* [Macro](#macro): record and play macros
* [Visual](#visual): advanced visual mode support that allows selection of windows via motion keys
* [Prompt](#prompt): execute common actions via advanced prompts that is capable of
    * dynamically displaying the response of the action (e.g., `find` and `grep`) as completable options (for selecting files)
    * previewing files in the completion window
    * providing application-specific completion window (e.g, `taskwarrior`)

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
    * [.xmonad/xmonad.hs](.xmonad/xmonad.hs) serves as a template config file; modify it if you'd like
4. `cd` into [xmonad](xmonad); `cabal install`
5. `cd` into [XMonadContrib](XMonadContrib); `cabal install`

## <a id="split"></a>Layout

## <a id="motion"></a>Motion

## <a id="command"></a>Command

## <a id="macro"></a>Macro

## <a id="visual"></a>Visual

## <a id="prompt"></a>Prompt
