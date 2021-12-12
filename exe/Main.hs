module Main where

import           Control.Lens               ( (&)
                                            , (.~)
                                            )
import           Data.Text
import           Data.ByteString            ( hPut )
import qualified Network.AWS.Auth           as AWSAuth
import qualified Network.AWS.Env            as AWSEnv
import           System.Exit                ( die )
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
            info (awsOpts <**> helper)
                 ( fullDesc
                <> progDesc "Generate a temporary access token for RDS"
                <> header "generate-db-auth-token is a Haskell equivalent of 'aws rds generate-db-auth-token' CLI utility"
                 )


run :: AWSOptions -> IO () 
run parsedOpts = do
    let credsMethod =   if      (useDiscover parsedOpts)
                        then    AWSAuth.Discover
                        else    AWSAuth.FromEnv
                                    AWSAuth.envAccessKey
                                    AWSAuth.envSecretKey
                                    (Just AWSAuth.envSessionToken)
                                    (Just awsRegionEnvName)

    lgr     <- newLogger Info stdout
    env     <- AWSEnv.newEnv credsMethod

    case (RDSU.regionFromText . pack . region $ parsedOpts) of
        Left err  -> die $ "Error: " <> err
        Right reg -> do
            token <- RDSU.generateDbAuthToken
                        (env & AWSEnv.envLogger .~ lgr)
                        (hostname   parsedOpts)
                        (port       parsedOpts)
                        (username   parsedOpts)
                        reg
            hPut stdout token


data AWSOptions = AWSOptions
    { hostname      :: RDSU.Endpoint
    , port          :: RDSU.Port
    , username      :: RDSU.DBUsername
    , region        :: String
    , useDiscover   :: Bool
    }


awsOpts :: Parser AWSOptions
awsOpts = AWSOptions
        <$> strOption
            (   long "hostname"
            <>  metavar "RDS_ENDPOINT"
            <>  help "RDS Endpoint could be found on AWS RDS web console"
            )
        <*> option auto
            (   long "port"
            <>  help "RDS Database Port"
            <>  showDefault
            <>  value 5432
            <>  metavar "RDS_PORT"
            )
        <*> strOption
            (   long "username"
            <>  help "RDS Username"
            <>  metavar "RDS_USERNAME"
            )
        <*> strOption
            (   long "region"
            <>  help "AWS Region"
            <>  metavar "AWS_REGION"
            )
        <*> switch
            (   long "discover"
            <>  help "auto-discover AWS credentials. Without this flag AWS env vars will be required"
            )
