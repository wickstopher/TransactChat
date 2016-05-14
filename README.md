__TransactChat: A simple chat server written in Haskell with transactional events__

*Build requirements:*

    * ghc Version 7.6.3+
        + https://www.haskell.org/ghc/download
    * tx-events 0.4 (Matthew Fluet, Kevin Donelly, Edward Amsden)
        + https://www.cs.rit.edu/~mtf/research/tx-events/index.html
        + A modified tarball of the source from the above link is included in 
          this distribution. The only modification made was to the .cabal file
          (tx-events.cabal) to enable the package to build under ghc 7.6.3.

*Installing tx-events:*
    
    * cabal install reqs/tx-events-0.4.tar.gz
