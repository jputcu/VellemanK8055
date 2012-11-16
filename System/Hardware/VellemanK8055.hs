module System.Hardware.VellemanK8055 where


import System.USB
import Data.Word
import Data.Serialize
import qualified Data.ByteString as B
import Control.Concurrent


data Packet =
    Reset
  | SetAnalogDigital Word8 Word8 Word8
  | Status Word8 Word8 Word8 Word8 Word16 Word16
  deriving Show


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
  in writeInterrupt h outEndpoint bs 0 >>
     return ()


readData :: DeviceHandle -> IO Packet
readData h =
  let inEndpoint = EndpointAddress 1 In
  in do (data_read, _) <- readInterrupt h inEndpoint 8 0
        let (Right d) = decode data_read
        return d


handleK8055 :: (DeviceHandle -> IO ()) -> DeviceHandle -> IO ()
handleK8055 actions h =
  let config = 1
      interface = 0
  in withDetachedKernelDriver h interface ioAction
  where
    ioAction =
      let config = 1
          interface = 0
      in setConfig h (Just config) >>
         withClaimedInterface h interface (actions h)


--
-- Example usage
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


readDigitalIn :: DeviceHandle -> IO Word8
readDigitalIn h = do
  (Status dig _ _ _ _ _) <- readData h
  return dig


deviceActions :: DeviceHandle -> IO ()
deviceActions h =
  resetBoard h >>
  toggleAllDigitalOut h >>
  readDigitalIn h >>= print

