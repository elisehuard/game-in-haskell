# Game Programming Haskell

code with [https://leanpub.com/gameinhaskell](https://leanpub.com/gameinhaskell)

I recommend using a sandbox. checkout the repo, and then

    cabal sandbox init
    cabal install --only-dependencies
    cabal build

## Chapter 1

Showing how to set up a window and draw basic shapes:

    cabal run shapes-demo

## Chapter 2

State in a more traditional way:

    cabal run state-demo

State with FRP:

    cabal run frp-demo
