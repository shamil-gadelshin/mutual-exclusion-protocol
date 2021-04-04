module LMESpec (spec)
where

import           Test.Hspec

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Data.Maybe

import qualified CriticalSection         as CS
import qualified LME
import qualified Message                 as M
import qualified MessageBroker           as MB

spec :: Spec
spec = do
  describe "LME" $ do
      it "`local request sends the message to remote` works" $ do
        outChan <- newChan
        inChan <- newChan
        let outChans = [("RemoteServer", outChan)]

        let broker = MB.new inChan outChans
        lme <- LME.new "LocalServer" broker

        -- Create a local request
        LME.request lme $ CS.DummyResource "Dummy"

        let msgStr = readChan outChan
        maybeMsg <- M.decodeMessage <$> msgStr

        -- Verify message sending
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

        -- Create a local request
        LME.request lme $ CS.DummyResource "Dummy"

        let requestStr = readChan outChan
        maybeRequest <- M.decodeMessage <$> requestStr
        let request = fromJust maybeRequest
        let requestId = Just $ M.msgId request

        -- Compose a mock reply message
        let reply = M.Message "ReplyID" 3 M.Reply "RemoteServer" requestId
        writeChan inChan (M.encodeMessage reply)

        -- Run message handling
        LME.runMessagePipeline lme

        -- Verify release message
        let releaseStr = readChan outChan
        maybeRelease <- M.decodeMessage <$> releaseStr
        let release = fromJust maybeRelease

        M.timestamp release `shouldBe` 4
        M.msgType release   `shouldBe` M.Release
        M.serverId release  `shouldBe` "LocalServer"
        M.requestId release `shouldBe` requestId

      it "`resource execution` works" $ do
        outChan <- newChan
        inChan <- newChan
        let outChans = [("RemoteServer", outChan)]

        let broker = MB.new inChan outChans
        lme <- LME.new "LocalServer" broker
        let initialValue = 100
        pc <- CS.ProtectedCounter <$> newMVar initialValue

        -- Create a local request with counter
        LME.request lme pc

        -- No counter changes
        afterRequestValue <- readMVar (CS.counter pc)
        afterRequestValue `shouldBe` initialValue


        let requestStr = readChan outChan
        maybeRequest <- M.decodeMessage <$> requestStr
        let request = fromJust maybeRequest
        let requestId = Just $ M.msgId request

        -- Compose a mock reply message
        let reply = M.Message "ReplyID" 3 M.Reply "RemoteServer" requestId
        writeChan inChan (M.encodeMessage reply)

        -- Run message handling
        LME.runMessagePipeline lme

        -- Counter was incremented
        finalValue <- readMVar (CS.counter pc)
        finalValue `shouldBe` initialValue + 1
