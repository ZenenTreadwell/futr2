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

[nostr improvement proposals](https://github.com/nostr-protocol/nips) 
- [x] 0
- [x] 1
- [x] 10
- [ ] 2
- [ ] 12
- [ ] 33
- [ ] 23
- [ ] 57
- [ ] ?

Run the tests!

`stack test` 

Donations? 

`bc1q236vwrzwnedv6vuvfpgnmrwyvknc98js3fc6y9`


