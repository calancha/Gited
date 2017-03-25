# Gited: Operate on Git branches like dired
This library lists the branches in a Git repository.  Then you can
operate on them with a dired-like interface.

The command **gited-list-branches** prompts for the kind of branch
(local branches, remote branches or tags) and lists them.
This command is used quite often, thus it might be convenient
to give it a key binding.  For instance, if *gited.el* is in
your *load-path*, then you can bind it to **C-x C-g** in Dired buffers
by adding the following lines into your .emacs file:

```
(require 'gited)
(define-key dired-mode-map "\C-x\C-g" 'gited-list-branches)
```

If you are familiar with Dired, then you already know how to use
Gited; that's because most of the Gited commands with a Dired equivalent
share same keybindings.
For instance *gited-rename-branch* is bound to 'R' as *dired-do-rename*.
Similarly, *gited-mark* is bound to 'm' as *dired-mark*.
