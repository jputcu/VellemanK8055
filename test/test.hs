import System.Hardware.VellemanK8055
import System.USB


main :: IO ()
main = do
  ctx <- newCtx
  devices <- getDevices ctx
  let first_k8055 = head $ filter isK8055 devices
  withDeviceHandle first_k8055 (handleK8055 deviceActions)
