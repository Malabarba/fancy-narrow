fancy-narrow
============

Emacs package to immitate narrow-to-region with more eye-candy.
Instead of completely hiding text beyond the narrowed region, the text
is de-emphasized and becomes unreachable.

Simply call `fancy-narrow-to-region` to see it in action. Remember to
`fancy-widen` afterwards.

To change the face used on the blocked text, customise `fancy-narrow-blocked-face`.

Note this is designed for user interaction. For using within lisp code,
the standard `narrow-to-region` is preferable, because the fancy
version is susceptible to `inhibit-read-only`.
