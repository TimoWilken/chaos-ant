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

[wiki]: https://en.wikipedia.org/wiki/Langton%27s_ant
