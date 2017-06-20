# LadderLogic

A graphical ladder logic parser, written in Haskell.

```
!!   This is a line of comments    !!
!!  The file is called test.ladder !!
##---[/A]--+---[B]---------+--(D)--##
##         |               |       ##
##         +----[/C]-------+       ##
##                                 ##
##------------[/F]-------(G)-------##
```

From the command line:

```
$ ladderc test.ladder

"[And (And (Not (Input \"A\")) (Or (Not (Input \"C\")) (Input \"B\"))) (Output \"D\"),And (Not (Input \"F\")) (Output \"G\")]"
```
