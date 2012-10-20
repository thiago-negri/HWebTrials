{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
 
import Network.Wai (Request, Response, pathInfo, requestMethod, queryString, responseLBS)
import Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import Network.Wai.Handler.Warp (Port, run)
import Network.HTTP.Types (status200, status404, StdMethod(GET, POST), parseMethod)
import Network.HTTP.Types.Header (HeaderName)
import Data.Text (unpack)
import Data.Text.Internal (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Monoid (mconcat)
import Data.String (IsString)
import Data.Maybe (isNothing)
import Control.Monad.Trans.Resource (ResourceT)
import Safe (readMay)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Blaze.ByteString.Builder (copyByteString, fromByteString)
import Blaze.ByteString.Builder.Internal.Types (Builder)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.UTF8 as LBU
import qualified Data.ByteString.UTF8 as SBU

newtype BananaCount = BananaCount Int

data Url = Banana BananaCount
         | Index

class UrlParam a where
    render :: a -> String
    key :: a -> SBU.ByteString

instance UrlParam BananaCount where
    render (BananaCount q) = "bc=" ++ show q
    key _ = "bc"

mkHref :: Url -> H.AttributeValue
mkHref u = case u of (Banana q) -> H.toValue $ "/banana?" ++ render q
                     Index      -> "/"

parseReq :: Request -> ResourceT IO (Maybe Url)
parseReq req =
    case pathInfo req of
      ["banana"] -> case parseMethod (requestMethod req) of
                      Right GET  -> return . parseBanana $ (filterSndNothing . queryString) req
                      Right POST -> parseRequestBody lbsBackEnd req >>= return . parseBanana . fst
                      _ -> return Nothing
      [] -> return $ Just Index
      _ -> return Nothing

type Params = [(SBU.ByteString, SBU.ByteString)]

parseBanana :: Params -> Maybe Url
parseBanana params = 
    maybe Nothing (Just . Banana) $
          lookupRead (key (undefined :: BananaCount)) params >>= return . BananaCount

port :: Port
port = 3000

main :: IO ()
main = do putStrLn $ "Listening on port " ++ show port
          run port app

app :: Request -> ResourceT IO Response
app = (>>= return . maybe h404 handleUrl) . parseReq

h404 :: Response
h404 = responseLBS status404 [mkHeader TextPlain] $ "Not Found"

handleUrl :: Url -> Response
handleUrl u = case u of Banana q -> banana q
                        Index    -> index

filterSndNothing :: [(a, Maybe b)] -> [(a, b)]
filterSndNothing = map fromJustSnd . filter (not . isNothing . snd)

fromJustSnd :: (a, Maybe b) -> (a, b)
fromJustSnd (a, Just b) = (a, b)

lookupRead :: (Read b) => SBU.ByteString -> [(SBU.ByteString, SBU.ByteString)] -> Maybe b
lookupRead name params = lookup name params >>= readMay . SBU.toString

banana :: BananaCount -> Response
banana q = okHtml $ bananaHtml q

index :: Response
index = okHtml indexHtml

okHtml :: H.Html -> Response
okHtml = ok TextHtml . renderHtml

mkLink :: Url -> H.Html -> H.Html
mkLink url text = H.a H.! A.href (mkHref url) $ text

data ContentType = TextPlain | TextHtml

mkHeader :: ContentType -> (HeaderName, SBU.ByteString)
mkHeader t = case t of TextPlain -> ("Content-Type", "text/plain")
                       TextHtml  -> ("Content-Type", "text/html")

ok :: ContentType -> LB.ByteString -> Response
ok contentType = responseLBS status200 [mkHeader contentType]

indexHtml :: H.Html
indexHtml = 
    H.docTypeHtml $ do
      H.head $ do
        H.title "Nice index"
      H.body $ do
        H.p "Hello amazing website!"
        H.p $ do
          "Try "
          mkLink (Banana (BananaCount 5)) "5 delicious bananas"
          "!"
        H.div $ do 
          H.p $ do
            "Get:"
            H.form H.! A.action "banana" H.! A.method "get" $ do
              H.input H.! A.type_ "text" H.! A.name "bc"
        H.div $ do
          H.p $ do
            "Post:"
            H.form H.! A.action "banana" H.! A.method "post" $ do
              H.input H.! A.type_ "text" H.! A.name "bc"

bananaHtml :: BananaCount -> H.Html
bananaHtml (BananaCount q) =
    H.docTypeHtml $ do
      H.head $ do
        H.title "Nice banana"
      H.body $ do
        H.p $ do
          "Banana's count: "
          H.toHtml (show q)
        H.p $ do
          mkLink Index "Go back"

