# Chaos ant

This is a Haskell implementation of [Langton's ant][wiki].

The program draws on the terminal window, marking "black" and "white" squares
with a `#` character or a space.

There is a very incomplete stub GTK GUI implementation.

## Try it

```{sh}
$ ghc -O2 -dynamic ant
$ ./ant
```

The screenshot below shows the state of the grid after a few iterations (you can
see the "ant" as a `,` near the top-right corner):

![Screenshot][screenshot]

[wiki]: https://en.wikipedia.org/wiki/Langton%27s_ant
[screenshot]: https://github.com/TimoWilken/chaos-ant/raw/master/doc/screenshot.png
