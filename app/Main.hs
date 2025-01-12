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



-- runDiscord :: RunDiscordOpts -> IO Text

sayhello :: Text -> InteractionResponseMessage 
sayhello name = 
    InteractionResponseMessage 
    { interactionResponseMessageTTS = Nothing 
    , interactionResponseMessageContent = Just $ "Hello " `append` name `append` "!"
    , interactionResponseMessageEmbeds = Nothing 
    , interactionResponseMessageAllowedMentions = Nothing 
    , interactionResponseMessageFlags = Nothing 
    , interactionResponseMessageComponents = Nothing 
    , interactionResponseMessageAttachments = Nothing
    }

sayhelloCommand :: CreateApplicationCommand 
sayhelloCommand = 
    CreateApplicationCommandChatInput 
    { createName = "hello"
    , createDescription = "bot will say hello to you"
    , createLocalizedName = Nothing 
    , createLocalizedDescription = Nothing 
    , createOptions = Nothing 
    , createDefaultMemberPermissions = Nothing 
    , createDMPermission = Just False
    }

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

eventHandler :: Event -> DiscordHandler () 
eventHandler event = 
    case event of
        InteractionCreate interaction -> 
            case interaction of 
                (InteractionApplicationCommand {interactionId = id, interactionUser = u, interactionToken = token , applicationCommandData = data1, ..}) -> 
                    case data1 of 
                       ApplicationCommandDataMessage {applicationCommandDataName = t} -> 
                             case u of 
                                (MemberOrUser (Left _)) -> do
                                    lift $ putStrLn "Guild member tried to say hello"
                                    return () 
                                (MemberOrUser (Right user)) -> 
                                    let username = userName user
                                    in case t of 
                                        "hello" -> do 
                                           result <- restCall (CreateInteractionResponse id token (InteractionResponseChannelMessage (sayhello username)))
                                           case result of 
                                             Left e -> lift $ putStrLn $ "Error creating command: " ++ (show e)
                                             Right _ -> do
                                                lift $ putStrLn "success" 
                                                return () 
                                        _ -> lift $ putStrLn "User tried hello but it wasn't an option" 

                                 

                       _   -> do 
                        lift $ putStrLn "Different type of data"
                        return () 
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
        Left e -> lift $ putStrLn $ "Error creating command: " ++ (show e)
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