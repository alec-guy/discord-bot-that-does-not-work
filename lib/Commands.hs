{-# LANGUAGE OverloadedStrings #-}

module Commands where 

import Discord.Internal.Types.Interactions
import Discord.Internal.Rest.Interactions
import Discord.Internal.Rest.ApplicationCommands
import Discord.Internal.Types.ApplicationCommands

import Data.Text 

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
countWords :: Text -> InteractionResponseMessage 
countWords text = 
    InteractionResponseMessage 
    { interactionResponseMessageTTS = Nothing 
    , interactionResponseMessageContent = Just $ pack $ show $ (Prelude.length $ (Data.Text.words text), text)
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
countWordsCommand :: CreateApplicationCommand 
countWordsCommand = 
    CreateApplicationCommandChatInput 
    { createName = "wordcount"
    , createDescription = "bot will count words in your text"
    , createLocalizedName = Nothing 
    , createLocalizedDescription = Nothing 
    , createOptions = Just $ OptionsValues [OptionValueString {optionValueName = "text", optionValueLocalizedName = Nothing , optionValueDescription = "enter text to echo back", optionValueLocalizedDescription = Nothing, optionValueRequired = True, optionValueStringChoices = Left False, optionValueStringMinLen = Just 1, optionValueStringMaxLen = Just 50}]
    , createDefaultMemberPermissions = Nothing 
    , createDMPermission = Just False
    }