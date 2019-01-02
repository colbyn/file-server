module Static.Server (
    main
) where


import Core
import Core.Record as R

import qualified Prelude              as Pre
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified System.Environment   as Env

-- + Serialization
import qualified Data.FileEmbed as FileEmbed

-- + WAI/Warp
import           Network.HTTP.Types.Header                 (ResponseHeaders)
import           Network.Wai.Middleware.RequestLogger      (logStdoutDev, logStdout)
import qualified Network.Wai                     as W
import qualified Network.Wai.Handler.Warp        as Warp
import qualified Network.HTTP.Types.Status       as Status
import qualified Network.Wai.Application.Static  as Static


-- + Local
import qualified Static.Paths as StaticPaths



main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    ["spa", port_, root] -> 
      let
        port :: Int
        port = read port_
      in do
        putStrLn $ "\nServer running on port: " <> port_
        Warp.run port $ Static.staticApp (settings root)


settings :: Pre.FilePath -> Static.StaticSettings
settings root = initSettings
  { Static.ssRedirectToIndex = False
  , Static.ssAddTrailingSlash = False
  , Static.ss404Handler = Just (indexApp root)
  }
  where
    initSettings :: Static.StaticSettings
    initSettings =
      Static.defaultWebAppSettings root


-- |
-- Given any request, returns the contents of 'index.html'.
--
-- Regarding the above This is because we have client side routing on the cms
-- web client. Assuming the frontend is functioning correctly, this page
-- should only be requested once and used for all subsequent frontend routes.
--
indexApp :: Pre.FilePath -> W.Application
indexApp root req res = res $ W.responseFile status headers filePath Nothing
  where
    filePath :: Pre.FilePath
    filePath = root ++ "/index.html"
    
    status = Status.ok200
    headers :: ResponseHeaders
    headers =
      [ ("Content-Type", "text/html")
      ]

