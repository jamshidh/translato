{-# LANGUAGE OverloadedStrings #-}

module BrowserTools (
  userAgent2UAResult,
  isShimEligible
) where

import qualified Data.ByteString.Char8 as B
import Data.Functor
import qualified Data.Text as T
import Web.UAParser

import ShimConfig

userAgent2UAResult::String->IO UAResult
userAgent2UAResult userAgentString = do
  let userAgent = B.pack userAgentString
  uaParser <- loadUAParser
  return $
     case parseUA uaParser $ userAgent of
       Just x -> x
       Nothing -> error $ "Malformed userAgent: " ++ B.unpack userAgent


versionInRange::Version->VersionRange->Bool
versionInRange v1 (Exact v2) = v1 == v2
versionInRange v1 (LowerBound v2) = v1 >= v2
versionInRange v1 (UpperBound v2) = v1 < v2
versionInRange v1 (Range v2 v3) = v1 >= v2 && v1 <= v3
versionInRange _ AllVersions = True

uaResultToVersion::UAResult->Version
uaResultToVersion UAResult{uarV1=Just v1, uarV2=Just v2, uarV3=Just v3} =
    read <$> T.unpack <$> [v1, v2, v3]
uaResultToVersion UAResult{uarV1=Just v1, uarV2=Just v2, uarV3=Nothing} =
    read <$> T.unpack <$> [v1, v2]
uaResultToVersion UAResult{uarV1=Just v1, uarV2=Nothing, uarV3=Nothing} =
    read <$> T.unpack <$> [v1]
uaResultToVersion uaResult =
    error ("Error: An odd parameter was passed to uaResultToVersion: " ++ show uaResult)

uaInBrowserRange::UAResult->Browser->Bool
uaInBrowserRange _ AllBrowsers = True
uaInBrowserRange uaResult@UAResult{uarFamily="Firefox"} (Mozilla versionRange) =
    versionInRange (uaResultToVersion uaResult) versionRange
uaInBrowserRange uaResult@UAResult{uarFamily="Chrome"} (Webkit versionRange) =
    versionInRange (uaResultToVersion uaResult) versionRange
uaInBrowserRange uaResult@UAResult{uarFamily="IE"} (IE versionRange) =
    versionInRange (uaResultToVersion uaResult) versionRange
uaInBrowserRange _ _ = False

isShimEligible::UAResult->ShimConfig->Bool
isShimEligible userAgent config =
    or $ uaInBrowserRange userAgent <$> browsers config
