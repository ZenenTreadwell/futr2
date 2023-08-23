# futr2

nostr client from future

`stack build`

recomment to run through proxy: 

`torify <path to futr>`
`stack exec futr`  -- no proxy


ghci tip: 
```
$ pkg-config --cflags --libs libsecp256k1
<FLAGS>
$ stack ghci --ghc-options "<FLAGS>"
\> 
```

dependencies 
- libsecp256k1
- ...

nips 
- [ ] 1
- [ ] ?
- [ ] ?

there are some tests

`stack test` 

