☞ Pakej ☜
=====
[![Build Status](https://secure.travis-ci.org/supki/pakej.png?branch=master)](https://travis-ci.org/supki/pakej)

Pakej is a status bar daemon. It does not actually draw any status bars,
only executes I/O actions in the background and stores their results.
This is most useful together with tools like [tmux][tmux] or [xmobar][xmobar].

Install
-------

Installation is a fairly involved process (sorry). First, you need to install Pakej library:

```
% git clone https://github.com/supki/pakej
% cd pakej
% cabal install
```

Next, you need to create `~/.pakej` directory and place `pakej.hs` file there. For an example
of `pakej.hs` see [examples/Main.hs][simple-example] (very simple) or [my pakej.hs][supki-example]
(it's more complicated but shows more features)

Almost done; you need to compile `.pakej/pakej.hs` into an executable now (if you happen to be familiar
with [xmonad][xmonad] this may ring a bell):

```
% pakej --recompile
```

and, finally, run it!

```
% pakej
```

Use
---

You can query running `pakej` instance. Here I assume you've used [examples/Main.hs][simple-example] as
your `pakej.hs`:

```
% pakej date
12.27.13, Fri, 19:20 PM
```

if you're not in the mood of guessing available Pakej commands, just ask it to show them:

```
% pakej --stat
date
```

By default, Pakej daemon communicates with clients through [UNIX domain socket][unix-domain-socket] at
`~/.pakej/pakej.sock`. You can use `--unix` and `--port` options to override the default:

```
% pakej --port 1234
% pakej date --hostname 127.0.0.1 --port 1234
12.28.13, Sat, 13:56:37
```

You can actually run Pakej listening on many different sockets, for instance, `pakej --unix
~/.pakej/pakej.sock --port 1234 --port 5678` will listen on ports 1234, 5678, and also on the default UNIX
domain socket

_Note_: Pakej will not allow to start daemon if there is an instance already running:

```
% pakej
% pakej
Can't proceed, found running instance: 13994
```

To replace the running daemon, use the `--replace` option.

Customize
---------

You customize your Pakej instance by writing your own `~/.pakej/pakej.hs` file
which is the base for both daemon and client. Pakej package provides `pakej` function as the
default `main`, so, typical `pakej.hs` will start with:

```haskell
import Pakej

main :: IO ()
main = pakej ...
```

where in place of `...` you write a Pakej `Widget` you want to execute and query afterwards

### More

[Haddocks][pakej-haddocks] cover the entire Pakej API.

_Note_: If you make changes to `pakej.hs` and recompile Pakej, don't forget to replace currently running
daemon instance


  [tmux]: http://tmux.sourceforge.net
  [xmobar]: http://projects.haskell.org/xmobar
  [xmonad]: http://xmonad.org
  [simple-example]: https://github.com/supki/pakej/blob/master/example/Main.hs
  [supki-example]: https://github.com/supki/.dotfiles/blob/master/core/pakej.hs
  [unix-domain-socket]: http://en.wikipedia.org/wiki/Unix_domain_socket
  [pakej-haddocks]: http://supki.github.io/pakej/Pakej.html
