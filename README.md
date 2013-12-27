Pakej
=====

Pakej is a status bar daemon. It does not actually draw any status bar,
but executes arbitrary I/O in the background and shares its results.
This is most useful together with tools like [tmux][tmux] or [xmobar][xmobar]

Installation
------------

Installation is a fairly involved process (sorry). First, you need to install Pakej library:

```
% git clone https://github.com/supki/pakej
% cd pakej
% cabal install
```

Next, you'll need to create `~/.pakej` directory and place `pakej.hs` file inside. For an example
of `pakej.hs` see [examples/Main.hs][simple-example] (very simple) or [my pakej.hs][supki-example]
(it's more complicated but shows more features)

Almost done; you need to compile `.pakej/pakej.hs` into an executable now (if you happen to be familiar
with [xmonad][xmonad] all this may ring a bell):

```
% pakej --recompile
```

and, finally, run it!

```
% pakej
```

Usage
-----

You can query running `pakej` instance. Here I assume you've used [examples/Main.hs][simple-example] as
your `pakej.hs`:

```
% pakej date
12.27.13, Fri, 19:20 PM
```

if you're not in the mood of guessing available Pakej commands, just ask it to show them:

```
% pakej shto-to
date
```

  [tmux]: http://tmux.sourceforge.net
  [xmobar]: http://projects.haskell.org/xmobar
  [xmonad]: http://xmonad.org
  [simple-example]: https://github.com/supki/pakej/blob/master/example/Main.hs
  [supki-example]: https://github.com/supki/.dotfiles/blob/master/core/pakej.hs
