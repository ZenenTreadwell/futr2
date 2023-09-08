{-# LANGUAGE QuasiQuotes #-}
module Nostr.Boots where 

import Text.URI (URI)
import Text.URI.QQ (uri)

defaultRelay :: [URI] 
defaultRelay =  
    [ [uri|ws://cvpcawhvxk27qvu5xrcblx7ingfxfivukesdpj7rwg63mflaly3tbuid.onion|]
    , [uri|wss://relay.nostr.info|]
    , [uri|wss://relay.snort.social|]
    , [uri|wss://nostr-pub.wellorder.net|]
    , [uri|wss://nostr.oxtr.dev|]
    , [uri|wss://brb.io|]
    , [uri|wss://nostr-pub.semisol.dev|]
    , [uri|wss://nostr.zebedee.cloud|]
    , [uri|wss://relay.stoner.com|]
    , [uri|wss://relay.nostr.bg|]
    , [uri|wss://nostr-relay.untethr.me|]
    , [uri|wss://nostr.wine|]
    , [uri|wss://nostr.sandwich.farm|]
    , [uri|wss://nostr.rocks|] 
    , [uri|wss://relay.nostr.com.au|]
    , [uri|wss://nostrja-kari.heguro.com|]
    , [uri|wss://nostrja-kari-nip50.heguro.com|]
    , [uri|wss://purplepag.es|]
    , [uri|wss://nostr.lol|]
    , [uri|wss://nostr.wine|]
    ]
