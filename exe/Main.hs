module Main where

import           Control.Lens               ( (&)
                                            , (.~)
                                            )
import           Data.Text
import           Data.ByteString            ( hPut )
import qualified Network.AWS.Auth           as AWSAuth
import qualified Network.AWS.Env            as AWSEnv
import           System.IO                  ( stdout )

import           Network.AWS                ( LogLevel (Info)
                                            , newLogger
                                            )
import qualified Network.AWS.RDS.Utils      as RDSU

import          Options.Applicative


awsRegionEnvName :: Text
awsRegionEnvName = "AWS_REGION"

-- | Works similarly to / aws rds generate-db-auth-token --hostname --port --username --region /
main :: IO ()
main = run =<< execParser opts
    where
        opts =
            info (awsCreds <**> helper)
                 ( fullDesc
                <> progDesc "Generate a temporary access token for RDS"
                <> header "generate-db-auth-token is a Haskell equivalent of 'aws rds generate-db-auth-token' CLI utility"
                 )

run :: AWSCreds -> IO () 
run creds = do
    lgr     <- newLogger Info stdout
    env     <- AWSEnv.newEnv $ AWSAuth.FromEnv
                                    AWSAuth.envAccessKey
                                    AWSAuth.envSecretKey
                                    (Just AWSAuth.envSessionToken)
                                    (Just awsRegionEnvName)

    x <- RDSU.generateDbAuthToken
            (env & AWSEnv.envLogger .~ lgr)
            (hostname   creds)
            (port       creds)
            (username   creds)
    
    hPut stdout x


data AWSCreds = AWSCreds
    { hostname  :: RDSU.Endpoint
    , port      :: RDSU.Port
    , username  :: RDSU.DBUsername
    , region    :: String
    }

awsCreds :: Parser AWSCreds
awsCreds = AWSCreds
        <$> strOption
            ( long "hostname"
            <> metavar "RDS_ENDPOINT"
            <> help "RDS Endpoint could be found on AWS RDS web console"
            )
        <*> option auto
            ( long "port"
            <> help "RDS Database Port"
            <> showDefault
            <> value 5432
            <> metavar "RDS_PORT"
            )
        <*> strOption
            ( long "username"
            <> help "RDS Username"
            <> metavar "RDS_USERNAME"
            )
        <*> strOption
            ( long "region"
            <> help "AWS Region"
            <> metavar "AWS_REGION"
            )
