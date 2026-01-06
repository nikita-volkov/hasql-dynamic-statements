-- Docs: https://hspec.github.io/hspec-discover.html
module SpecHook where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Test.Hspec
import TestcontainersPostgresql qualified
import Prelude

type HookedSpec = SpecWith Connection.Connection

hook :: HookedSpec -> Spec
hook hookedSpec = parallel do
  byDistro "postgres:10"
  byDistro "postgres:17"
  where
    byDistro tagName =
      describe (toList tagName) do
        aroundAll
          ( \onConnection ->
              TestcontainersPostgresql.run
                TestcontainersPostgresql.Config
                  { tagName,
                    auth = TestcontainersPostgresql.TrustAuth,
                    forwardLogs = False
                  }
                ( \(host, port) -> do
                    let settings =
                          mconcat
                            [ Settings.hostAndPort host port,
                              Settings.user "postgres",
                              Settings.dbname "postgres"
                            ]
                    bracket
                      ( do
                          res <- Connection.acquire settings
                          case res of
                            Left err -> fail ("Connection failed: " <> show err)
                            Right conn -> pure conn
                      )
                      Connection.release
                      onConnection
                )
          )
          (parallel hookedSpec)
