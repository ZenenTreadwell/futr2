# futr

The name futr was chosen from respect for the honorable Canadians.  

Install the dependencies 
- [libsecp256k1](https://github.com/bitcoin-core/secp256k1#building-with-autotools)
- [SDL2 and GLEW](https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/00-setup.md#libraries-sdl2-and-glew)

Build and run with stack:     
- `stack build` 
- `stack test`
- `stack exec futr` 


Example creating and broadcasting reply

```
import Nostr.Keys (Hex96, Hex32)
import Nostr.Event (signE, Event, Content, Tags)
import Nostr.Pool (castAll, Pool)
import Nostr.Wire (Up(..))

bioing :: Pool -> Hex96 -> Hex32 -> IO ()
bioing pool kp re = do 
    e <- signE kp $ Content 1 [Etag re Nothing Nothing] "bioing" sec 
    castAll pool (Submit e)
```

Relay options in ~/.futr/futr.conf are: 

```
name = 
contact =  
description =  
pubkey = 
port = 9481
```

App storage in ~/.futr/events.sqlite.


Client wip
