{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Network.TelegramBot where

import Network.TelegramBot.Common
import Network.TelegramBot.Types
import Network.TelegramBot.Methods

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.TH
import Data.Char
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import Text.Show

import qualified Data.Text.IO as T

import Servant.API
import Servant.Client
import Servant.Common.Req

apiToken :: String
apiToken = "118862716:AAHrLNzojDJw47Lf6U_6n1GF2Aeexy17m0w"

type BotAPI =
       "getUpdates" :> ReqBody '[JSON] GetUpdatesM :> Post '[JSON] (Response [Update])
  :<|> "setWebhook" :> ReqBody '[JSON] SetWebhookM :> Post '[JSON] (Response Bool)
  :<|> "getMe" :> Get '[JSON] (Response User)
  :<|> "sendMessage" :> ReqBody '[JSON] SendMessageM :> Post '[JSON] (Response Message)
  :<|> "forwardMessage" :> ReqBody '[JSON] ForwardMessageM :> Post '[JSON] (Response Message)
  :<|> "sendPhoto" :> ReqBody '[JSON] SendPhotoM :> Post '[JSON] (Response Message)
  :<|> "sendAudio" :> ReqBody '[JSON] SendAudioM :> Post '[JSON] (Response Message)
  :<|> "sendDocument" :> ReqBody '[JSON] SendDocumentM :> Post '[JSON] (Response Message)
  :<|> "sendSticker" :> ReqBody '[JSON] SendStickerM :> Post '[JSON] (Response Message)
  :<|> "sendVideo" :> ReqBody '[JSON] SendVideoM :> Post '[JSON] (Response Message)
  :<|> "sendVoice" :> ReqBody '[JSON] SendVoiceM :> Post '[JSON] (Response Message)
  :<|> "sendLocation" :> ReqBody '[JSON] SendLocationM :> Post '[JSON] (Response Message)
  :<|> "sendChatAction" :> ReqBody '[JSON] SendChatActionM :> Post '[JSON] (Response TrueTg)
  :<|> "getUserProfilePhotos" :> ReqBody '[JSON] GetUserProfilePhotosM :> Post '[JSON] (Response UserProfilePhotos)
  :<|> "getFile" :> ReqBody '[JSON] GetFileM :> Post '[JSON] (Response File)

botAPI :: Proxy BotAPI
botAPI = Proxy

type APIResult t = EitherT ServantError IO (Response t)
getUpdates :: GetUpdatesM -> APIResult [Update]
setWebhook :: SetWebhookM -> APIResult Bool
getMe :: APIResult User
sendMessage :: SendMessageM -> APIResult Message
forwardMessage :: ForwardMessageM -> APIResult Message
sendPhoto :: SendPhotoM -> APIResult Message
sendAudio :: SendAudioM -> APIResult Message
sendDocument :: SendDocumentM -> APIResult Message
sendSticker :: SendStickerM -> APIResult Message
sendVideo :: SendVideoM -> APIResult Message
sendVoice :: SendVoiceM -> APIResult Message
sendLocation :: SendLocationM -> APIResult Message
sendChatAction :: SendChatActionM -> APIResult TrueTg
getUserProfilePhotos :: GetUserProfilePhotosM -> APIResult UserProfilePhotos
getFile :: GetFileM -> APIResult File

getUpdates :<|> setWebhook :<|> getMe :<|> sendMessage :<|> forwardMessage
    :<|> sendPhoto :<|> sendAudio :<|> sendDocument :<|> sendSticker
    :<|> sendVideo :<|> sendVoice :<|> sendLocation :<|> sendChatAction
    :<|> getUserProfilePhotos :<|> getFile
       = clientWithRoute botAPI defReq{reqPath = "/bot" <> apiToken} (BaseUrl Https "api.telegram.org" 443)

successfulResults :: Applicative f => (a -> f a) -> Either e (Response a) -> f (Either e (Response a))
successfulResults = _Right.responseResult._Just

successfulResults' :: Prism (Either c a) (Either c b) a b
successfulResults' = _Right

