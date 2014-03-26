fancy-narrow
============

Emacs package to immitate [`narrow-to-region`](http://bruce-connor.github.io/emacs-online-documentation/Fun%2Fnarrow-to-region.html) with more eye-candy.

Unlike [`narrow-to-region`](http://bruce-connor.github.io/emacs-online-documentation/Fun%2Fnarrow-to-region.html), which completely hides text outside
the narrowed region, this package simply deemphasizes the text,
makes it readonly, and makes it unreachable.

This leads to a much more natural feeling, where the region stays
static (instead of being brutally moved to a blank slate) and is
clearly highlighted with respect to the rest of the buffer.

Simply call `fancy-narrow-to-region` to see it in action. To widen the
region again afterwards use `fancy-widen`.

To customise the face used to deemphasize unreachable text, customise
`fancy-narrow-blocked-face`. There is a known bug at the moment, which
is that comments and strings don't deemphasize correctly.

Note this is designed for user interaction. For using within lisp code,
the standard [`narrow-to-region`](http://bruce-connor.github.io/emacs-online-documentation/Fun%2Fnarrow-to-region.html) is preferable, because the fancy
version is susceptible to [`inhibit-read-only`](http://bruce-connor.github.io/emacs-online-documentation/Var/inhibit-read-only.html) and some corner cases.
