module Main where

import Data.Aeson
import Data.Vector
import Data.ByteString.Lazy

import qualified Network.WebSockets as WS


data Message = Message
  { mSteering_angle :: Double
  , mThrottle       :: Double
  , mSpeed          :: Double
  , mCte            :: Double
  , mImage          :: String
  } deriving Show

data Response = Response
  { rThrottle       :: Double
  , rSteering_angle :: Double
  } deriving Show

instance FromJSON Message where
  parseJSON (Array a)
    | a ! 0 == "telemetry" &&
      a ! 1 /= "empty"     &&
      a ! 1 /= Null = parseJSON (a ! 1)
    | otherwise     = mempty

  parseJSON (Object v) = do
    mSteering_angle <- v .: "steering_angle"
    mThrottle       <- v .: "throttle"
    mSpeed          <- v .: "speed"
    mCte            <- v .: "cte"
    mImage          <- v .: "image"

    return $ Message (read mSteering_angle :: Double)
                     (read mThrottle       :: Double)
                     (read mSpeed          :: Double)
                     (read mCte            :: Double)
                     mImage

  parseJSON _ = mempty

instance ToJSON Response where
  toJSON r =
    Array $ fromList [ "steer"
                     , object [ "throttle"       .= rThrottle r
                              , "steering_angle" .= rSteering_angle r]]


pid :: Double -> Double -> Double -> Double
pid d_err i_err p_err = -kp*p_err -ki*i_err -kd*d_err
  where
    kp  = 0.25
    ki  = 0.001
    kd  = 10.0

server :: WS.ServerApp
server pending = do
  conn <- WS.acceptRequest pending
  server' 0.0 0.0 0.0 conn

server' :: Double -> Double -> Double -> WS.Connection -> IO ()
server' d_err i_err p_err conn = do
  msg <- WS.receiveData conn

  let msgCode = Data.ByteString.Lazy.take 2 msg
  let msgBody = Data.ByteString.Lazy.drop 2 msg

  if msgCode == "42" then
    case decode msgBody :: Maybe Message of
      Nothing ->
        server' d_err i_err p_err conn

      Just msgData -> do
        let cte    = mCte msgData
        let d_err' = cte - p_err
        let i_err' = cte + i_err
        let p_err' = cte
        let steer  = pid d_err' i_err' p_err'

        let response = Response { rThrottle       = 0.3
                                , rSteering_angle = steer }

        let responseMsg = append "42" $ encode response

        WS.sendTextData conn responseMsg
        server' d_err' i_err' p_err' conn
  else
    server' d_err i_err p_err conn

main =
  WS.runServer "127.0.0.1" 4567 server
