module Nostr.Relay where 






-- type Listen = IO (Either WS.ConnectionException LB.ByteString)

-- wsr :: SQL.Connection -> ClientApp ()
-- wsr db ws = forever do
--     print "server"
--     eo <- E.try . WS.receiveData $ ws :: Listen
--     case decode <$> eo of 
--         Right (Just d) -> case d of 
--             Subscribe s fx -> print "sub"  
--             Submit e -> do 
--                 trust <- verifyE e 
--                 when trust (mask_ $ insertEv db e)  
--             End s -> pure () --myThreadId >>= killThread 
--         Right Nothing -> print . (<> " - nothing") . show $ eo
--         Left z -> killing z eo

--     -- threadDelay 3000000  
--     -- kp <- genKeyPair 
--     -- pu <- exportPub kp
--     -- e <- signE kp $ Content 1 [
          
--     --       PTag pu Nothing
--     --     , 
--     --       ETag evref Nothing Nothing 

--     --     ] "test futr3 " sec  
--     -- sendE conn e
--     threadDelay maxBound








