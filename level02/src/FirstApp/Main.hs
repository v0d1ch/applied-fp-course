{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main (runApp) where

import qualified Data.ByteString.Lazy as LBS
import Data.Either (either)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import FirstApp.Types (ContentType (Json, PlainText), Error (ErrorInvalidPath),
                       RqType (AddRq, ListRq, ViewRq), mkCommentText, mkTopic, renderContentType)
import Network.HTTP.Types (Status, hContentType, status200, status400, status404)
import Network.Wai (Application, Request, Response, pathInfo, requestMethod, responseLBS,
                    strictRequestBody)
import Network.Wai.Handler.Warp (run)
-- --------------------------------------------
-- - Don't start here, go to FirstApp.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse s ct bs =
  responseLBS s [(hContentType , renderContentType ct)] bs

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 ct bs =
  responseLBS status200 [(hContentType , renderContentType ct)] bs

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 ct bs =
  responseLBS status404 [(hContentType , renderContentType ct)] bs

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  error "resp400 not implemented"

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest t lbs = do
  case mkTopic t of
    Left e  -> Left e
    Right to -> do
      case mkCommentText (lazyByteStringToStrictText lbs) of
        Left e   -> Left e
        Right ct -> Right $ AddRq to ct
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest t =
  fmap ViewRq (mkTopic t)

mkListRequest
  :: Either Error RqType
mkListRequest = Right ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse e =
  responseLBS status404 [(hContentType , renderContentType PlainText)]
    (LBS.fromStrict $ encodeUtf8 $ pack $ show e)
-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest r = case pathInfo r of
  [t, "add"]  -> mkAddRequest t <$> strictRequestBody r
  [t, "view"] -> return $ mkViewRequest t
  ["list"]    -> return mkListRequest
  _           -> return $ Left (ErrorInvalidPath "Invalid path")
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest ListRq      = Right (resp200 Json "Not implemented yet")
handleRequest (AddRq _ _) = Right (resp200 Json "Not implemented yet")
handleRequest (ViewRq _) = Right (resp200 Json "Not implemented yet")

-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app :: Application
app req resp = do 
  request <- mkRequest req
  let response = request >>= handleRequest  
  resp $ either mkErrorResponse id response


runApp :: IO ()
runApp = run 3000 app
