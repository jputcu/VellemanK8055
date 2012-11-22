module System.Hardware.VellemanK8055 where

import           Control.Concurrent
import           Control.Monad      (void)
import           Data.Bits          (shift, testBit)
import qualified Data.ByteString    as B
import           Data.Serialize
import           Data.Word
import           System.USB

data AnalogChannel =  Analog1
                    | Analog2

data DigitalChannel = Digital1
                    | Digital2
                    | Digital3
                    | Digital4
                    | Digital5
                    | Digital6
                    | Digital7
                    | Digital8
                    deriving (Enum)

data Packet = Reset
            | SetAnalogDigital Word8 Word8 Word8
            | Status Word8 Word8 Word8 Word8 Word16 Word16
            deriving (Show)


instance Serialize Packet where
  put (Reset) =
    let cmd = 0x00
    in putByteString $ B.pack [cmd, 0x00, 0x00, 0x00, 0x00, 0x00 , 0x00, 0x00]

  put (SetAnalogDigital dig an1 an2) =
    let cmd = 0x05
    in putByteString $ B.pack [cmd, dig, an1, an2, 0x00, 0x00, 0x00, 0x00]

  put _ =
    error "n/a"

  get = do
    digital_in <- get
    return $ Status digital_in 0 0 0 0 0


isK8055 :: Device -> Bool
isK8055 candidate =
  let description = deviceDesc candidate
  in case (deviceVendorId description, deviceProductId description) of
    (0x10cf, 0x5500) -> True
    (0x10cf, 0x5501) -> True
    (0x10cf, 0x5502) -> True
    (0x10cf, 0x5503) -> True
    _                -> False


writeData :: DeviceHandle -> Packet -> IO ()
writeData h p =
  let outEndpoint = EndpointAddress 1 Out
      bs = encode p
  in void $ writeInterrupt h outEndpoint bs 0


readData :: DeviceHandle -> IO Packet
readData h =
  let inEndpoint = EndpointAddress 1 In
  in do (data_read, _) <- readInterrupt h inEndpoint 8 0
        let (Right d) = decode data_read
        return d


handleK8055 :: (DeviceHandle -> IO ()) -> DeviceHandle -> IO ()
handleK8055 actions h =
  let interface = 0
  in withDetachedKernelDriver h interface ioAction
  where
    ioAction =
      let config = 1
          interface = 0
      in setConfig h (Just config) >>
         withClaimedInterface h interface (actions h)


--
-- Analog stuff
--

readAnalogChannel:: DeviceHandle -> AnalogChannel -> IO Word8
readAnalogChannel hd ch = do
  (Status _ a1 a2 _ _ _) <- readData hd
  return $ case ch of
              Analog1 -> a1
              Analog2 -> a2

readAllAnalog:: DeviceHandle -> IO (Word8, Word8)
readAllAnalog hd = do
  (Status _ a1 a2 _ _ _) <- readData hd
  return (a1, a2)

outputAnalogChannel:: DeviceHandle -> AnalogChannel -> Word8 -> IO ()
outputAnalogChannel hd ch v = do
  digi <- readAllDigital hd
  writeData hd $ case ch of
                    Analog1 -> SetAnalogDigital digi v 0x00
                    Analog2 -> SetAnalogDigital digi 0x00 v

outputAllAnalog:: DeviceHandle -> Word8 -> Word8 -> IO ()
outputAllAnalog hd a1 a2 = do
  digi <- readAllDigital hd
  writeData hd $ SetAnalogDigital digi a1 a2

clearAnalogChannel:: DeviceHandle -> AnalogChannel -> IO ()
clearAnalogChannel hd ch = outputAnalogChannel hd ch 0x00

clearAllAnalog:: DeviceHandle -> IO ()
clearAllAnalog hd = outputAllAnalog hd 0x00 0x00

setAnalogChannel:: DeviceHandle -> AnalogChannel -> IO ()
setAnalogChannel hd ch = outputAnalogChannel hd ch 0xff

setAllAnalog:: DeviceHandle -> IO ()
setAllAnalog hd = outputAllAnalog hd 0xff 0xff

--
-- Digital stuff
--

readDigitalChannel:: DeviceHandle -> DigitalChannel -> IO Bool
readDigitalChannel hd ch = do
  (Status dig _ _ _ _ _) <- readData hd
  return $ testBit dig (fromEnum ch)

readAllDigital:: DeviceHandle -> IO Word8
readAllDigital hd = do
  (Status dig _ _ _ _ _) <- readData hd
  return dig

--
-- Misc stuff
--

resetBoard :: DeviceHandle -> IO ()
resetBoard h =
  writeData h Reset >>
  readData h >>
  return ()


toggleAllDigitalOut :: DeviceHandle -> IO ()
toggleAllDigitalOut h = do
  writeData h $ SetAnalogDigital 0xff 0x00 0x00
  threadDelay 1000000
  writeData h $ SetAnalogDigital 0x00 0x00 0x00

deviceActions :: DeviceHandle -> IO ()
deviceActions h =
  resetBoard h >>
  toggleAllDigitalOut h >>
  readAllDigital h >>= print

