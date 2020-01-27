{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Lib (
  Command(..),
  Argument(..),
  EC2Metadata(..),
  KnobKey(..),
  KnobValue(..),
  ec2Metadata,
  execWithKnobs
) where

import           Data.Aeson           (FromJSON (..), FromJSONKey (..),
                                       Value (..), withObject, (.:))
import           Data.Map             (Map, fromList, map, mapKeys, toList,
                                       union)
import           Data.Scientific      (Scientific)
import           Data.Text            (Text, unpack)
import           Network.HTTP.Req
import           System.Environment   (getEnvironment)
import           System.Posix.Process

newtype Command  = Command  { unCommand  :: FilePath }
newtype Argument = Argument { unArgument :: String }


newtype KnobKey = KnobKey Text deriving (Eq, Ord, Show, FromJSONKey)


data KnobValue  = KNumber Scientific | KString Text | KBool Bool

instance FromJSON KnobValue where
  parseJSON (Number n) = return $ KNumber n
  parseJSON (String s) = return $ KString s
  parseJSON (Bool   b) = return $ KBool b
  parseJSON illegal    = fail $ "Expected a scalar knob value, got " <> show illegal


newtype EC2Metadata = EC2Metadata (Map KnobKey KnobValue)

instance FromJSON EC2Metadata where
  parseJSON = withObject "Instance metadata" $ \obj -> EC2Metadata <$> obj .: "knobs"


ec2Metadata :: IO EC2Metadata
ec2Metadata = runReq defaultHttpConfig $
  responseBody <$> req GET (http "169.254.169.254" /: "latest" /: "user-data") NoReqBody jsonResponse mempty

execWithKnobs :: Command -> [Argument] -> EC2Metadata -> IO a
execWithKnobs command args (EC2Metadata knobs) = do
  env <- fromList <$> getEnvironment

  let knobs'' = knobs' `union` env
  executeFile command' True args' (Just $ toList knobs'')

  where
    command'             = unCommand command
    args'                = Prelude.map unArgument args
    knobs'               = mapKeys unkey $ Data.Map.map unknob knobs
    unkey  (KnobKey t)   = unpack t
    unknob (KNumber n)   = show n
    unknob (KString s)   = unpack s
    unknob (KBool True)  = "true"
    unknob (KBool False) = "false"
