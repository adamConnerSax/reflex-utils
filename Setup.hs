import           Distribution.MacOSX
import           Distribution.Simple



main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
         postBuild = appBundleBuildHook guiApps -- no-op if not MacOS X
       }

guiApps :: [MacApp]
guiApps = [
    MacApp "reflex-utils-simpleForm"
      Nothing
      (Just "macos/simpleForm.plist")
      [] -- No other resources.
      [] -- No other binaries.
      DoNotChase -- Try changing to ChaseWithDefaults,
  , MacApp "reflex-utils-layout"
      Nothing
      (Just "macos/layout.plist")
      [] -- No other resources.
      [] -- No other binaries.
      DoNotChase -- Try changing to ChaseWithDefaults
    ]
