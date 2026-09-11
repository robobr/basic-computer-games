## aceyducey

Haskell port of [aceyducey](../../../01_Acey_Ducey/README.md)

### Porting Notes

* Built with [The Glasgow Haskell Compiler](https://www.haskell.org/get-started/)

* Uses GHC and its included libraries only.  No Cabal builds or external packages are required.

* Code conventions aim to meet the essentials of Simple, Boring, Low Complexity Haskell.

* Stays true to the original program with regard for text display and game play.  Some quirks of BASIC are not replicated, e.g. the printing and parsing rules for numbers.

* Install GHC with [GHCup](https://www.haskell.org/get-started/#ghcup-universal-installer):

    ```sh
    ghcup install ghc latest
    ```

* Run directly (like a script):

    ```sh
    runghc aceyducey.hs
    ```

* Or compile first:

    ```sh
    ghc aceyducey.hs
    ./aceyducey
    ```

