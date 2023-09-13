# futr2

"The name 'futr' was chosen from respect for the honorable Canadians." - [prolic of futr](https://github.com/prolic/futr)

nostr Library, Relay and Harvester  

`stack build` -- build exec

`stack exec futr` 
- runs relay on 9481
- collect events from default relays 

Running over tor works: 

`torify <path to futr>`

dependencies 
- [libsecp256k1](https://github.com/bitcoin-core/secp256k1#building-with-autotools)
- ...

NIP Checklist ([Nostr Improvement Possibilities](https://github.com/nostr-protocol/nips))
wip (relay focus)
- [x] 1 
- [x] 10 
- [x] 42 - auth
- [ ] 13 - pow
- [ ] 4 - aes 
- [ ] 45 - count
- [ ] 11 - relay-meta
            
client?    
- [ ] 52 - calendar 
- [ ] 15
- [ ] 25  
- [ ] 30

avoiding 
- [ ] 57 - lightning interaction should not require dns setup?
- [ ] 50 - search



`futr.sqlite` is created in the local directory   

Run the tests!

`stack test` 


to explore in ghci needs flags: 
```
$ pkg-config --cflags --libs libsecp256k1
<FLAGS>
$ stack ghci --ghc-options "<FLAGS>"
\> 
```

Donate? 

`bc1q236vwrzwnedv6vuvfpgnmrwyvknc98js3fc6y9`


