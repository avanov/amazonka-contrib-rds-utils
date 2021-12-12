{-# LANGUAGE TypeFamilies           #-}  -- required for 'Rs'
{-# LANGUAGE FlexibleInstances      #-}  -- required for ToQuery String
{-# OPTIONS_GHC -fno-warn-orphans   #-}
module  Network.AWS.RDS.Utils
    (   generateDbAuthToken
    ,   Endpoint
    ,   Port
    ,   DBUsername
    ,   Region
    ,   regionFromText
    )
where

import           Prelude                    hiding ( drop, length )
import           Control.Lens               ( (^.), (&), (.~) )
import           Control.Monad.Trans.AWS    ( runResourceT, runAWST )
import           Data.ByteString            ( ByteString, drop, length )
import           Data.ByteString.Char8      ( pack )
import qualified Data.Text                  as T
import qualified Data.Time.Clock            as Clock
import           Network.AWS                ( _svcPrefix
                                            )
import qualified Network.AWS.RDS            as RDS
import           Network.AWS.Endpoint       ( setEndpoint )
import qualified Network.AWS.Env            as Env
import qualified Network.AWS.Request        as AWSReq
import qualified Network.AWS.Response       as AWSResp
import           Network.AWS.Data.Text      ( fromText )
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
                                            , Service
                                            , Region
                                            )

type Endpoint   = String
type Port       = Int
type DBUsername = String
type Token      = ByteString

tokenExpiration :: Seconds
tokenExpiration = Seconds 900  -- 15 minutes

serviceSigningName :: ByteString
serviceSigningName = "rds-db"

thisService :: Service
thisService = RDS.rds { _svcPrefix = serviceSigningName }

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
                    -> Region
                    -> IO Token
generateDbAuthToken env endp prt username region = do
    -- it has some overhead, but we're just making sure we're composing a correct URL
    let action = GetDBAuthToken $ PresignParams
                                    { endpoint   = endp
                                    , port       = prt
                                    , dbUsername = username
                                    }
        regionalEnv = env & Env.envRegion .~ region

    signingTime <- Clock.getCurrentTime

    runResourceT . runAWST regionalEnv $ do
        val <- Presign.presignURL
                (regionalEnv ^. Env.envAuth)
                (regionalEnv ^. Env.envRegion)
                signingTime
                tokenExpiration
                action
        pure $ dropPrefix val


data PresignParams = PresignParams
    { endpoint   :: Endpoint
    , port       :: Port
    , dbUsername :: DBUsername
    }


newtype GetDBAuthTokenResponse = GetDBAuthTokenResponse ByteString

newtype GetDBAuthToken = GetDBAuthToken PresignParams

instance AWSRequest GetDBAuthToken where
    type Rs GetDBAuthToken = GetDBAuthTokenResponse
    
    request (GetDBAuthToken params)  =
        AWSReq.defaultRequest svc (GetDBAuthToken params) where
            svc      = setEndpoint useHTTPS (pack . endpoint $ params) (port params) thisService
            useHTTPS = True 

    response = AWSResp.receiveBytes $ \_s _h x -> pure $ GetDBAuthTokenResponse x
        

instance ToPath GetDBAuthToken where
    toPath _ = ""


instance ToQuery String where
    toQuery = toQuery . pack


instance ToQuery GetDBAuthToken where
    toQuery (GetDBAuthToken params) = QList (toQuery <$> xs) where
        xs :: [(String, String)]
        xs = [("Action", "connect"), ("DBUser", dbUsername params)]


instance ToHeaders GetDBAuthToken where
    toHeaders _ = []


regionFromText :: T.Text -> Either String Region
regionFromText = fromText
