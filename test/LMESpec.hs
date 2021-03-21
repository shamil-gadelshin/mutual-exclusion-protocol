module LMESpec (spec) 
where

import Test.Hspec

import Control.Concurrent.Chan
import Data.Maybe

import qualified LME
import qualified CriticalSection as CS
import qualified MessageBroker as MB
import qualified Message as M

spec :: Spec
spec = do
  describe "LME" $ do
      it "`local request sends the message to remote` works" $ do
        outChan <- newChan
        inChan <- newChan
        let outChans = [("RemoteServer", outChan)]

        let broker = MB.new inChan outChans
        lme <- LME.new "LocalServer" broker
        LME.request lme $ CS.DummyResource "Dummy"

        let msgStr = readChan outChan
        maybeMsg <- M.decodeMessage <$> msgStr

        let msg = fromJust maybeMsg
        M.timestamp msg `shouldBe` 2 
        M.msgType msg   `shouldBe` M.Request
        M.serverId msg  `shouldBe` "LocalServer"
        M.requestId msg `shouldBe` Nothing      

      it "`reply for local request releases the resource` works" $ do
        outChan <- newChan
        inChan <- newChan
        let outChans = [("RemoteServer", outChan)]

        let broker = MB.new inChan outChans
        lme <- LME.new "LocalServer" broker
        LME.request lme $ CS.DummyResource "Dummy"

        let requestStr = readChan outChan
        maybeRequest <- M.decodeMessage <$> requestStr
        let request = fromJust maybeRequest
        let requestId = Just $ M.msgId request

        let reply = M.Message "ReplyID" 3 M.Reply "RemoteServer" requestId
        writeChan inChan (M.encodeMessage reply)

        LME.runMessagePipeline lme

        let releaseStr = readChan outChan
        maybeRelease <- M.decodeMessage <$> releaseStr
        let release = fromJust maybeRelease

        M.timestamp release `shouldBe` 4 
        M.msgType release   `shouldBe` M.Release
        M.serverId release  `shouldBe` "LocalServer"
        M.requestId release `shouldBe` requestId     