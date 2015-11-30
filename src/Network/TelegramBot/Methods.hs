{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.TelegramBot.Methods where

import Network.TelegramBot.Common
import Network.TelegramBot.Types

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client
import Text.Show

import qualified Data.Text    as T
import qualified Data.Text.IO as T

data GetUpdatesM = GetUpdatesM
    { _getUpdatesMOffset :: Maybe Int32
    , _getUpdatesMLimit :: Maybe Int32
    , _getUpdatesMTimeout :: Maybe Int32
    } deriving (Eq, Show)

getUpdatesM :: GetUpdatesM
getUpdatesM = GetUpdatesM Nothing Nothing Nothing

data SetWebhookM = SetWebhookM
    { _setWebhookMCertificate :: Maybe InputFile
    , _setWebhookMUrl :: Maybe Text
    } deriving (Eq, Show)

setWebhookM :: SetWebhookM
setWebhookM = SetWebhookM Nothing Nothing

data SendMessageM = SendMessageM
    { _sendMessageMParseMode :: Maybe Text
    , _sendMessageMDisableWebPagePreview :: Maybe Bool
    , _sendMessageMReplyToMessageId :: Maybe Int32
    , _sendMessageMReplyMarkup :: Maybe ReplyMarkup
    , _sendMessageMChatId :: ChatId
    , _sendMessageMText :: Text
    } deriving (Eq, Show)

sendMessageM :: ChatId -> Text -> SendMessageM
sendMessageM = SendMessageM Nothing Nothing Nothing Nothing

data ForwardMessageM = ForwardMessageM
    { _forwardMessageMChatId :: ChatId
    , _forwardMessageMFromChatId :: ChatId
    , _forwardMessageMMessageId :: Int32
    } deriving (Eq, Show)

forwardMessageM :: ChatId -> ChatId -> Int32 -> ForwardMessageM
forwardMessageM = ForwardMessageM

data SendContentM c = SendContentM
    { _sendContentMReplyToMessageId :: Maybe Int32
    , _sendContentMReplyMarkup :: Maybe ReplyMarkup
    , _sendContentMChatId :: ChatId
    , _sendContentMContent :: c
    } deriving (Eq, Show)

sendContentM :: ChatId -> content -> SendContentM content
sendContentM = SendContentM Nothing Nothing

withContentM :: (a -> content) -> (ChatId -> a -> SendContentM content)
withContentM = (flip . ((.).)) sendContentM

-- Officially the weirdest looking code I've ever unironically written.
withContentM2 :: (a -> b -> content) -> (ChatId -> a -> b -> SendContentM content)
withContentM2 = (flip . (((.).(.)).)) sendContentM

withContentM3 :: (a -> b -> c -> content) -> (ChatId -> a -> b -> c -> SendContentM content)
withContentM3 = (flip . ((((.).(.)).(.)).)) sendContentM

instance ToJSON c => ToJSON (SendContentM c) where
    toJSON m = Object $ HM.union (HM.filter (/= Null) $ HM.fromList
        [ ("reply_to_message_id", toJSON $ _sendContentMReplyToMessageId m)
        , ("reply_markup", toJSON $ _sendContentMReplyMarkup m)
        , ("chat_id", toJSON $ _sendContentMChatId m)
        ]) contentMap
        where (Object contentMap) = toJSON $ _sendContentMContent m

instance FromJSON c => FromJSON (SendContentM c) where
    parseJSON (Object o) = SendContentM
        <$> o .:? "reply_to_message_id"
        <*> o .:? "reply_markup"
        <*> o .: "chat_id"
        <*> (parseJSON . Object
            . HM.delete "chat_id"
            . HM.delete "reply_to_message_id"
            . HM.delete "reply_markup") o
    parseJSON _ = empty

type SendPhotoM = SendContentM SendPhotoM'
data SendPhotoM' = SendPhotoM'
    { _sendPhotoMCaption :: Maybe Text
    , _sendPhotoMPhoto :: InputFile
    } deriving (Eq, Show)

sendPhotoM :: ChatId -> InputFile -> SendPhotoM
sendPhotoM = withContentM $ SendPhotoM' Nothing

type SendAudioM = SendContentM SendAudioM'
data SendAudioM' = SendAudioM'
    { _sendAudioMDuration :: Maybe Int32
    , _sendAudioMPerformer :: Maybe Text
    , _sendAudioMTitle :: Maybe Text
    , _sendAudioMAudio :: InputFile
    } deriving (Eq, Show)

sendAudioM :: ChatId -> InputFile -> SendAudioM
sendAudioM = withContentM $ SendAudioM' Nothing Nothing Nothing

type SendDocumentM = SendContentM SendDocumentM'
data SendDocumentM' = SendDocumentM'
    { _sendDocumentMDocument :: InputFile
    } deriving (Eq, Show)

sendDocumentM :: ChatId -> InputFile -> SendDocumentM
sendDocumentM = withContentM SendDocumentM'

type SendStickerM = SendContentM SendStickerM'
data SendStickerM' = SendStickerM'
    { _sendStickerMSticker :: InputFile
    } deriving (Eq, Show)

sendStickerM :: ChatId -> InputFile -> SendStickerM
sendStickerM = withContentM SendStickerM'

type SendVideoM = SendContentM SendVideoM'
data SendVideoM' = SendVideoM'
    { _sendVideoMDuration :: Maybe Int32
    , _sendVideoMCaption :: Maybe Text
    , _sendVideoMVideo :: InputFile
    } deriving (Eq, Show)

sendVideoM :: ChatId -> InputFile -> SendVideoM
sendVideoM = withContentM $ SendVideoM' Nothing Nothing

type SendVoiceM = SendContentM SendVoiceM'
data SendVoiceM' = SendVoiceM'
    { _sendVoiceMDuration :: Maybe Int32
    , _sendVoiceMVoice :: InputFile
    } deriving (Eq, Show)

sendVoiceM :: ChatId -> InputFile -> SendVoiceM
sendVoiceM = withContentM $ SendVoiceM' Nothing

type SendLocationM = SendContentM SendLocationM'
data SendLocationM' = SendLocationM'
    { _sendLocationMLatitude :: Float
    , _sendLocationMLongitude :: Float
    } deriving (Eq, Show)

sendLocationM :: ChatId -> Float -> Float -> SendLocationM
sendLocationM = withContentM2 SendLocationM'

data SendChatActionM = SendChatActionM
    { _sendChatActionMAction :: ChatAction
    , _sendChatActionMChatId :: ChatId
    } deriving (Eq, Show)

sendChatActionM :: ChatAction -> ChatId -> SendChatActionM
sendChatActionM = SendChatActionM

data GetUserProfilePhotosM = GetUserProfilePhotosM
    { _getUserProfilePhotosMOffset :: Maybe Int32
    , _getUserProfilePhotosMLimit :: Maybe Int32
    , _getUserProfilePhotosMUserId :: ChatId
    } deriving (Eq, Show)

getUserProfilePhotosM :: ChatId -> GetUserProfilePhotosM
getUserProfilePhotosM = GetUserProfilePhotosM Nothing Nothing

data GetFileM = GetFileM
    { _getFileMFileId :: Text
    } deriving (Eq, Show)

getFileM :: Text -> GetFileM
getFileM = GetFileM

$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 12} ''GetUpdatesM)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 12} ''SetWebhookM)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 13} ''SendMessageM)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 16} ''ForwardMessageM)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 11} ''SendPhotoM')
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 11} ''SendAudioM')
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 14} ''SendDocumentM')
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 13} ''SendStickerM')
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 11} ''SendVideoM')
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 11} ''SendVoiceM')
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 14} ''SendLocationM')
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 16} ''SendChatActionM)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 22} ''GetUserProfilePhotosM)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 22} ''GetFileM)
--                                                             _234567890123456789012345

makeLenses ''GetUpdatesM
makeLenses ''SetWebhookM
makeLenses ''SendMessageM
makeLenses ''ForwardMessageM
makeLenses ''SendPhotoM'
makeLenses ''SendAudioM'
makeLenses ''SendDocumentM'
makeLenses ''SendStickerM'
makeLenses ''SendVideoM'
makeLenses ''SendVoiceM'
makeLenses ''SendLocationM'
makeLenses ''SendChatActionM
makeLenses ''GetUserProfilePhotosM
makeLenses ''GetFileM

