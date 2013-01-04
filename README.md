A tiny language called Z
===========================

A strict, impure, curried, partially applied language with rather
peculiar syntax. See [documentation](http://chrisdone.com/z).

Setup
=====

Z is currently implemented in Haskell.

You can download Haskell [here](http://www.haskell.org/platform/).

After installing Haskell, you should be able to do

    $ cd <path/to/z/dir>
    $ ghc Setup.hs
    $ ./Setup configure
    $ ./Setup build

which will yield a binary in ``dist/build/z/z``.

When run, it provides an interactive prompt that evaluates Z.

If you want to run the contents of a file, you can use shell redirection:

    $ ./dist/build/z/z < examples.zz

MacPorts install
----------------

If you use MacPorts, you can install Haskell and the necessary Haskell libraries from MacPorts with this command:

    sudo port install ghc hs-platform-regex-compat hs-platform-parsec
