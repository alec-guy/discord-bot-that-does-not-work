{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.Free 
import Discord
import Discord.Handle
import Discord.Types
import Data.Text 
-- import Control.Monad.Trans
import Control.Monad.Reader 
import Control.Concurrent.Chan 
import Control.Exception (catch, SomeException)
import System.Exit
import Discord.Internal.Rest.Channel
import Discord.Internal.Types.Interactions
import Discord.Internal.Rest.Interactions
import Discord.Internal.Rest.ApplicationCommands
import Discord.Internal.Types.ApplicationCommands
import Commands 


-- runDiscord :: RunDiscordOpts -> IO Text

myGatewayIntents = 
    GatewayIntent 
    { gatewayIntentGuilds = False 
    , gatewayIntentMembers = False 
    , gatewayIntentBans    = False 
    , gatewayIntentEmojis = False 
    , gatewayIntentIntegrations = False 
    , gatewayIntentWebhooks = False 
    , gatewayIntentInvites = False 
    , gatewayIntentVoiceStates = False 
    , gatewayIntentPresences = False 
    , gatewayIntentMessageChanges = False
    , gatewayIntentMessageReactions = False
    , gatewayIntentMessageTyping    = False 
    , gatewayIntentDirectMessageChanges =  False
    , gatewayIntentDirectMessageReactions = False
    , gatewayIntentDirectMessageTyping = False 
    , gatewayIntentMessageContent = True 
    , gatewayIntentAutoModerationConfiguration = False 
    , gatewayIntentAutoModerationExecution = False
    }

-- ai did this sendMessage function. 

getSecretToken :: IO Text 
getSecretToken = pack <$> readFile "secretToken.txt "

myDiscordOptions :: RunDiscordOpts 
myDiscordOptions = RunDiscordOpts
                 { discordToken = ""
                 , discordOnStart = myDiscordOnStart
                 , discordOnEnd = putStrLn "End"
                 , discordOnEvent = eventHandler
                 , discordOnLog = \_ -> return () 
                 , discordForkThreadForEvents = False
                 , discordGatewayIntent = myGatewayIntents
                 , discordEnableCache = False
                 }


handleNameAndOptions :: Text -> Maybe OptionsData ->  InteractionId -> InteractionToken ->  Text -> DiscordHandler () 
handleNameAndOptions name options id token username = 
            case name of 
             "hello"     -> do 
                result <- restCall (CreateInteractionResponse id token (InteractionResponseChannelMessage (sayhello username)))
                case result of 
                    Left e -> lift $ putStrLn $ "Error executing hello command: " ++ (show e)
                    Right _ -> do
                       lift $ putStrLn "success executing hello command to guild member" 
                       return () 
             "wordcount" ->  do 
                let text = case options of 
                            Nothing -> ""
                            Just (OptionsDataValues [OptionDataValueString {optionDataValueName = t, optionDataValueString = e}]) -> 
                               case e of 
                                Left _ -> ""
                                Right i -> i                                                         
                result2 <- restCall (CreateInteractionResponse id token (InteractionResponseChannelMessage (countWords text)))
                case result2 of 
                    Left e -> lift $ putStrLn $ "Error executing count words command: " ++ (show e)
                    Right _ -> do
                       lift $ putStrLn "success executing count words command" 
                       return ()  
             _ -> lift $ putStrLn "User tried another command" 

applicationCommandDataHandler :: InteractionId -> InteractionToken -> MemberOrUser -> ApplicationCommandData  -> DiscordHandler ()
applicationCommandDataHandler id token memberoruser data1 = 
    case memberoruser of 
        (MemberOrUser (Left guildmem)) -> do 
            let nickname = case memberNick guildmem of 
                            Nothing -> ""
                            Just x  -> x 
            case data1 of 
              ApplicationCommandDataChatInput {applicationCommandDataName = name, optionsData = options} -> 
                handleNameAndOptions name options id token nickname 
              _      -> lift $ putStrLn "Another kind of data"
        (MemberOrUser (Right user)) -> do 
            let username = userName user
            case data1 of 
              ApplicationCommandDataChatInput {applicationCommandDataName = name, optionsData = options} -> 
                handleNameAndOptions name options id token username
                    
              _ -> lift $ putStrLn "Another kind of data"

                

eventHandler :: Event -> DiscordHandler () 
eventHandler event = 
    case event of
        InteractionCreate interaction -> 
            case interaction of 
                (InteractionApplicationCommand {interactionId = id, interactionUser = u, interactionToken = token , applicationCommandData = data1, ..}) -> 
                    applicationCommandDataHandler id token u data1 
                _  -> do
                    lift $ putStrLn "Different type of interaction" 
                    return () 
        _ -> return () 



     

myDiscordOnStart :: DiscordHandler () 
myDiscordOnStart = do 
    lift $ putStrLn "App starting"
    cache <- readCache 
    let fullapp = cacheApplication cache 
        appid   = fullApplicationID fullapp 
    result  <- restCall $ CreateGlobalApplicationCommand appid sayhelloCommand 
    case result of 
        Left e -> lift $ putStrLn $ "Error creating command: say hello" ++ (show e)
        Right _ -> return () 
    result2 <- restCall $ CreateGlobalApplicationCommand appid countWordsCommand 
    case result2 of 
        Left e -> lift $ putStrLn $ "Error creating command count words: " ++ (show e)
        Right _ -> return () 

main :: IO ()
main = do
   token <- getSecretToken
   text  <- catch ((runDiscord (myDiscordOptions {discordToken = token})) :: IO Text) $ \e -> do 
                  putStrLn "Caught exception"
                  putStrLn $ show (e :: SomeException)
                  putStrLn "Exiting program..."
                  exitFailure

   putStrLn $ show $ text