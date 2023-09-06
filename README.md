# futr2

"The name 'futr' was chosen from respect for the honorable Canadians." - [prolic of futr](https://github.com/prolic/futr)

nostr Library, Relay and Harvester  

`stack build` -- build exec

`stack exec futr` 
- runs relay on 9487
- collect events from default relays 

Running over tor works: 

`torify <path to futr>`


ghci needs flags: 
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
|wip |possible |avoiding |
|- [ ] 1 - [ ] 25  - [ ] 42 - [x] 10 - [ ] 12 - [ ] 13 - [ ] 31 - [ ] 33 - [ ] 65
    |  - [ ] 30 - [ ] 65 - [ ] 7 
        |  - [ ] 4 - [ ] 57
            |

Desktop Application (monomer) 

`futr.sqlite` is created in the local directory   

Run the tests!

`stack test` 

Donate? 

`bc1q236vwrzwnedv6vuvfpgnmrwyvknc98js3fc6y9`


