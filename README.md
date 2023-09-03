# futr2

nostr types, relay and harvester ... wip 

`stack build` -- build exec

`stack exec futr`  -- start relay on 9487 and start collecting events (see app/Main.hs)

Running over tor works: 

`torify <path to futr>`


to explore in ghci, need to link the signing library: 
```
$ pkg-config --cflags --libs libsecp256k1
<FLAGS>
$ stack ghci --ghc-options "<FLAGS>"
\> 
```

dependencies 
- [libsecp256k1](https://github.com/bitcoin-core/secp256k1#building-with-autotools)
- ...

nips (todo ?)
- [x] 1
- [ ] ?
- [ ] ?

there are some tests

`stack test` 

