# futr2

"The name 'futr' was chosen from respect for the honorable Canadians." - [prolic of futr1](https://github.com/prolic/futr)

nostr Library, relay and harvester ...  

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

NIP Checklist ([Nostr Improvement Possibilities](https://github.com/nostr-protocol/nips))
- [ ] 1
- [ ] 42
- [ ] 50
- [ ] 30
- [ ] 25
- [ ] 7
- [ ] 2
- [ ] 3
- [x] 10
- [ ] 12
- [ ] 13
- [ ] 31
- [ ] 33
- [ ] 23
- [ ] 58
- [ ] 57
- [ ] 99
- [ ] 15
- [ ] 4
- [ ] ?

Run the tests!

`stack test` 

Donate? 

`bc1q236vwrzwnedv6vuvfpgnmrwyvknc98js3fc6y9`


