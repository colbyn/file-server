module Static.Paths (
    distDir
  , distIndexFile
  , distReleaseDir
) where

import Core
import qualified Prelude as Pre


distDir :: Pre.FilePath
distDir = "./dist"

distIndexFile :: Pre.FilePath
distIndexFile = distDir ++ "/index.html"

distReleaseDir :: Pre.FilePath
distReleaseDir = distDir ++ "/release/"


