{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.TelegramBot.Types where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty
import Data.Char
import Data.Int
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client
import Text.Show

import Network.TelegramBot.Common
import Network.TelegramBot.Files

import qualified Data.Text    as T
import qualified Data.Text.IO as T

type InputFile = Text

newtype TrueTg = TrueTg { _trueTg :: All }
    deriving (Eq, Show)

makePrisms ''All
makePrisms ''TrueTg

_TgBool :: Iso' TrueTg Bool
_TgBool = _TrueTg._All

_BoolTg :: Iso' Bool TrueTg
_BoolTg = from _TgBool

deriving instance ToJSON All
deriving instance FromJSON All

instance FromJSON TrueTg where
    parseJSON (Bool True) = pure $ TrueTg (All True)
    parseJSON _ = pure $ TrueTg (All False)

instance ToJSON TrueTg where
    toJSON (TrueTg (All True)) = Bool True
    toJSON _ = Null

data Response t = Response
    { _responseOk :: All
    , _responseDescription :: Maybe Text
    , _responseResult :: Maybe t
    } deriving (Eq, Show)

instance Monoid t => Monoid (Response t) where
    mempty = Response (All False) Nothing Nothing
    mappend (Response xOk xDesc xRes) (Response yOk yDesc yRes) =
        Response (xOk <> yOk) (xDesc <> yDesc) (xRes <> yRes)

data Update = Update
    { _updateUpdateId :: Int32
    , _updateMessage :: Message
    } deriving (Eq, Show)

data User = User
    { _userId :: Int32
    , _userFirstName :: Text
    , _userLastName :: Maybe Text
    , _userUsername :: Maybe Text
    } deriving (Eq, Show)

data Chat = Chat
    { _chatId :: Int32
    , _chatType :: ChatType
    , _chatTitle :: Maybe Text
    , _chatUsername :: Maybe Text
    , _chatFirstName :: Maybe Text
    , _chatLastName :: Maybe Text
    } deriving (Eq, Show)

data ChatType = Private | Group | Supergroup | Channel
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

type ChatId = OneOf '[Int32, Text]

data ChatAction
    = Typing
    | UploadPhoto
    | RecordVideo | UploadVideo
    | RecordAudio | UploadAudio
    | UploadDocument
    | FindLocation
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

data Message = Message
    { _messageMessageId :: Int32
    , _messageFrom :: Maybe User
    , _messageDate :: Int32
    , _messageChat :: Chat
    , _messageForwardFrom :: Maybe User
    , _messageForwardDate :: Maybe Int32
    , _messageReplyToMessage :: Maybe Message
    , _messageText :: Maybe Text
    , _messageAudio :: Maybe Audio
    , _messageDocument :: Maybe Document
    , _messagePhoto :: Maybe [PhotoSize]
    , _messageSticker :: Maybe Sticker
    , _messageVideo :: Maybe Video
    , _messageVoice :: Maybe Voice
    , _messageCaption :: Maybe Text
    , _messageLocation :: Maybe Location
    , _messageNewChatParticipant :: Maybe User
    , _messageLeftChatParticipant :: Maybe User
    , _messageNewChatTitle :: Maybe Text
    , _messageNewChatPhoto :: Maybe [PhotoSize]
    , _messageDeleteChatPhoto :: Maybe TrueTg
    , _messageGroupChatCreated :: Maybe TrueTg
    , _messageSupergroupChatCreated :: Maybe TrueTg
    , _messageChannelChatCreated :: Maybe TrueTg
    , _messageMigrateToChatId :: Maybe Int32
    , _messageMigrateFromChatId :: Maybe Int32
    } deriving (Eq, Show)

data ParseMode = Markdown
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

data PhotoSize = PhotoSize
    { _photoSizeFileId :: Text
    , _photoSizeWidth :: Int32
    , _photoSizeHeight :: Int32
    , _photoSizeFileSize :: Maybe Int32
    } deriving (Eq, Show)

data Audio = Audio
    { _audioFileId :: Text
    , _audioDuration :: Int32
    , _audioPerformer :: Maybe Text
    , _audioTitle :: Maybe Text
    , _audioMimeType :: Maybe Text
    , _audioFileSize :: Maybe Text
    } deriving (Eq, Show)

data Document = Document
    { _documentFileId :: Text
    , _documentThumb :: Maybe PhotoSize
    , _documentFileName :: Maybe Text
    , _documentMimeType :: Maybe Text
    , _documentFileSize :: Maybe Int32
    } deriving (Eq, Show)

data Sticker = Sticker
    { _stickerFileId :: Text
    , _stickerWidth :: Int32
    , _stickerHeight :: Int32
    , _stickerThumb :: Maybe PhotoSize
    , _stickerFileSize :: Maybe Int32
    } deriving (Eq, Show)

data Video = Video
    { _videoFileId :: Text
    , _videoWidth :: Int32
    , _videoHeight :: Int32
    , _videoDuration :: Int32
    , _videoThumb :: Maybe PhotoSize
    , _videoMimeType :: Maybe Text
    , _videoFileSize :: Maybe Int32
    } deriving (Eq, Show)

data Voice = Voice
    { _voiceFileId :: Text
    , _voiceDuration :: Int32
    , _voiceMimeType :: Maybe Text
    , _voiceFileSize :: Maybe Int32
    } deriving (Eq, Show)

data Contact = Contact
    { _contactPhoneNumber :: Text
    , _contactFirstName :: Text
    , _contactLastName :: Maybe Text
    , _contactUserId :: Maybe Int32
    } deriving (Eq, Show)

data Location = Location
    { _locationLongitude :: Float
    , _locationLatitude :: Float
    } deriving (Eq, Show)

data UserProfilePhotos = UserProfilePhotos
    { _userProfilePhotosTotalCount :: Int32
    , _userProfilePhotosPhoto :: [[PhotoSize]]
    } deriving (Eq, Show)

data File = File
    { _fileId :: Text
    , _fileSize :: Maybe Int32
    , _filePath :: Maybe Text
    } deriving (Eq, Show)

type ReplyMarkup = OneOf '[ReplyKeyboardMarkup, ReplyKeyboardHide, ForceReply]

data ReplyKeyboardMarkup = ReplyKeyboardMarkup
    { _replyKeyboardMarkupKeyboard :: [[Text]]
    , _replyKeyboardMarkupResizeKeyboard :: Maybe Bool
    , _replyKeyboardMarkupOneTimeKeyboard :: Maybe Bool
    , _replyKeyboardMarkupSelective :: Maybe Bool
    } deriving (Eq, Show)

data ReplyKeyboardHide = ReplyKeyboardHide
    { _replyKeyboardHideHideKeyboard :: TrueTg
    , _replyKeyboardHideSelective :: Maybe Bool
    } deriving (Eq, Show)

data ForceReply = ForceReply
    { _forceReplyForceReply :: TrueTg
    , _forceReplySelective :: Maybe Bool
    } deriving (Eq, Show)

$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop  9} ''Response)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop  7} ''Update)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop  5} ''User)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop  5} ''Chat)
$(deriveJSON tgJSONOptions                                    ''ChatType)
$(deriveJSON tgJSONOptions                                    ''ChatAction)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop  8} ''Message)
$(deriveJSON tgJSONOptions                                    ''ParseMode)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 10} ''PhotoSize)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop  6} ''Audio)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop  9} ''Document)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop  8} ''Sticker)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop  6} ''Video)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop  6} ''Voice)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop  8} ''Contact)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop  9} ''Location)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 18} ''UserProfilePhotos)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop  5} ''File)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 20} ''ReplyKeyboardMarkup)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 18} ''ReplyKeyboardHide)
$(deriveJSON tgJSONOptions{fieldLabelModifier = snakeDrop 11} ''ForceReply)
--                                                             _234567890123456789012345

makeLenses ''Response
makeLenses ''Update
makeLenses ''User
makeLenses ''Chat
makeLenses ''ChatType
makeLenses ''ChatAction
makeLenses ''Message
makeLenses ''ParseMode
makeLenses ''PhotoSize
makeLenses ''Audio
makeLenses ''Document
makeLenses ''Sticker
makeLenses ''Video
makeLenses ''Voice
makeLenses ''Contact
makeLenses ''Location
makeLenses ''UserProfilePhotos
makeLenses ''File
makeLenses ''ReplyKeyboardMarkup
makeLenses ''ReplyKeyboardHide
makeLenses ''ForceReply

