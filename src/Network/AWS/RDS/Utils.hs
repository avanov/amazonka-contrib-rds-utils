{-# LANGUAGE TypeFamilies           #-}  -- required for 'Rs'
{-# LANGUAGE FlexibleInstances      #-}  -- required for ToQuery String
{-# OPTIONS_GHC -fno-warn-orphans   #-}
module  Network.AWS.RDS.Utils
    (   generateDbAuthToken
    ,   Endpoint
    ,   Port
    ,   DBUsername
    )
where

import           Prelude                    hiding ( drop, length )
import           Control.Lens               ( (^.) )
import           Control.Monad.Trans.AWS    ( runResourceT, runAWST )
import           Data.ByteString            ( ByteString, drop, length )
import           Data.ByteString.Char8      ( pack )
import           Data.Maybe                 ( fromMaybe )
import qualified Data.Time.Clock            as Clock
import           Network.AWS                ( _svcPrefix )
import qualified Network.AWS.RDS            as RDS
import           Network.AWS.Endpoint       ( setEndpoint )
import qualified Network.AWS.Env            as Env
import qualified Network.AWS.Request        as AWSReq
import qualified Network.AWS.Response       as AWSResp
import           Network.AWS.Data.Path      ( ToPath (..)
                                            )
import           Network.AWS.Data.Query     ( ToQuery (..)
                                            , QueryString ( QList )
                                            )
import           Network.AWS.Data.Headers   ( ToHeaders (..)
                                            )
import           Network.AWS.Presign        as Presign
import           Network.AWS.Types          ( Seconds (..)
                                            , AWSRequest (..)
                                            , Rs
                                            )
import qualified Network.URL                as URL

type Endpoint   = String
type Port       = Int
type DBUsername = String

tokenExpiration :: Seconds
tokenExpiration = Seconds 900  -- 15 minutes

serviceSigningName :: ByteString
serviceSigningName = "rds-db"

dropPrefix :: ByteString -> ByteString
dropPrefix = drop $ length "https://"

-- Amazon docs:             https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html
-- Python implementation:   https://github.com/boto/botocore/blob/77527250093fc97cbf078adab04bdd74b1fd3c03/botocore/signers.py#L409
-- Go implementation:       https://github.com/aws/aws-sdk-go/blob/e2d6cb448883e4f4fcc5246650f89bde349041ec/service/rds/rdsutils/connect.go#L36-L67
-- | Generates RDS auth token that can be used as a temporary password for Postgres connections.
generateDbAuthToken :: Env.Env
                    -> Endpoint
                    -> Port
                    -> DBUsername
                    -> IO ByteString
generateDbAuthToken env endpoint port username = do
    -- it has some overhead, but we're just making sure we're composing a correct URL
    let url = URL.URL
                { URL.url_type = URL.Absolute $ URL.Host { URL.protocol = URL.HTTP True
                                                         , URL.host     = endpoint
                                                         , URL.port     = Just (toInteger port)
                                                         }
                , URL.url_path   = ""
                , URL.url_params = [ ("Action", "connect")
                                   , ("DBUser", username)
                                   ] 
                }

    signingTime <- Clock.getCurrentTime
    
    runResourceT . runAWST env $ do
        val <- Presign.presignURL
                (env ^. Env.envAuth)
                (env ^. Env.envRegion)
                signingTime
                tokenExpiration
                (GetDBAuthToken url)
        pure $ dropPrefix val

urlHost :: URL.URL -> Maybe String
urlHost (URL.URL (URL.Absolute (URL.Host _proto host _port)) _path _params) = Just host
urlHost _ = Nothing

urlPort :: URL.URL -> Maybe Int
urlPort (URL.URL (URL.Absolute (URL.Host _proto _host port)) _path _params) = fromIntegral <$> port
urlPort _ = Nothing


newtype GetDBAuthTokenResponse = GetDBAuthTokenResponse ByteString

newtype GetDBAuthToken = GetDBAuthToken URL.URL

instance AWSRequest GetDBAuthToken where
    type Rs GetDBAuthToken = GetDBAuthTokenResponse
    
    request (GetDBAuthToken url)  =
        AWSReq.defaultRequest svc (GetDBAuthToken url) where
            svc      = (setEndpoint useHTTPS (pack endpoint) port RDS.rds) { _svcPrefix = serviceSigningName }
            useHTTPS = True 
            endpoint = fromMaybe "localhost" (urlHost url)
            port     = fromMaybe 5432        (urlPort url)

    response = AWSResp.receiveBytes $ \_s _h x -> pure $ GetDBAuthTokenResponse x
        


instance ToPath GetDBAuthToken where
    toPath _ = ""


instance ToQuery String where
    toQuery = toQuery . pack


instance ToQuery GetDBAuthToken where
    toQuery (GetDBAuthToken url) = QList (toQuery <$> URL.url_params url)


instance ToHeaders GetDBAuthToken where
    toHeaders _ = []
