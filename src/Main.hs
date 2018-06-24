{-# LANGUAGE OverloadedStrings #-}

module Main where


import Prelude hiding (putStrLn, getLine, concat, drop, take)

import GHC.Exts (fromList)
import System.IO (hFlush, stdout)

import Data.Aeson
import Data.Scientific
import Data.Vector hiding ((++), fromList, concat, drop, take)
import Data.ByteString hiding (putStrLn, appendFile, concat)
import Data.ByteString.Lazy (fromStrict, concat)
import Data.ByteString.Lazy.Char8 (putStrLn)


data Message = Message
  { steering_angle :: Scientific
  , throttle       :: Scientific
  , speed          :: Scientific
  , cte            :: Scientific
  , image          :: String
  } deriving Show

instance FromJSON Message where
  parseJSON (Array a)
    | a ! 0 == "telemetry" = parseJSON (a ! 1)
    | otherwise            = mempty

  parseJSON (Object v) = do
    steering_angle <- v .: "steering_angle"
    throttle       <- v .: "throttle"
    speed          <- v .: "speed"
    cte            <- v .: "cte"
    image          <- v .: "image"

    return
      (Message
      (read steering_angle :: Scientific)
      (read throttle       :: Scientific)
      (read speed          :: Scientific)
      (read cte            :: Scientific)
      image)


pid :: Scientific -> Scientific -> Scientific -> Scientific
pid d_err i_err p_err = -kp*p_err -ki*i_err -kd*d_err
    where
      kp  = 0.25
      ki  = 0.001
      kd  = 10.0


mainLoop d_err i_err p_err = do
  msg <- getLine

  if take 2 msg == "42" then
    case (decodeStrict (drop 2 msg) :: Maybe Message) of
      Nothing -> do
        putStrLn "42[\"manual\",{}]"
      Just obj -> do
        let reponse = Array $ fromList [ "steer",
                                         Object $ fromList [ ("throttle", Number 0.3),
                                                             ("steering_angle", Number $ pid d_err i_err p_err)]]

        putStrLn $ concat ["42", (encode reponse)]
        hFlush stdout

        mainLoop ((cte obj) - p_err) ((cte obj) + i_err) (cte obj)
  else
    putStrLn "42[\"manual\",{}]"

  mainLoop d_err i_err p_err

main = mainLoop 0.0 0.0 0.0