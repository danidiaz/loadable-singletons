# loadable-singletons

(under construction)

**Note:** This package has *nothing* to do with the singletons library for emulating dependent types.

The idea is to load external resources from an sqlite database located in the
same folder as the executable.

Each resource corresponds to a type inside the application. The *value* for the
type is loaded using the facilities provided by the library.

Perhaps I'm overcomplicating things though.

## remember to add the resources to extra-source-files

Like this:

    extra-source-files:  lib/**/*.txt

Requires `cabal-version: 3` or greater.

## making direct-sqlite work on windows

[download page](https://www.sqlite.org/download.html).

On `cabal.project`:

    constraints: direct-sqlite +systemlib

On `cabal.project.local`:

    package direct-sqlite
        extra-include-dirs: C:/Users/someuser/.local/sqlite-amalgamation-3350300
        extra-lib-dirs: C:/Users/someuser/.local/sqlite-dll-win64-x64-3350300

Example `Main.hs`:

    {-# language ImportQualifiedPost #-}
    module Main where

    import Database.SQLite3
    import Data.Text qualified as T

    main :: IO ()
    main = do
        db <- open (T.pack "foo.db")
        execPrint db (T.pack "select * from foo;")
        execPrint db (T.pack "begin transaction;")
        execPrint db (T.pack "alter table foo drop column f3;")
        execPrint db (T.pack "rollback;")
        close db

## Alternatives

- [file-embed: Use Template Haskell to embed file contents directly.](https://hackage.haskell.org/package/file-embed)

- [data-embed: Embed files and other binary blobs inside executables without Template Haskell.](https://hackage.haskell.org/package/data-embed)

