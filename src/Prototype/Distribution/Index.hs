module Prototype.Distribution.Index where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Data.ByteString.Lazy as BS

writeTar :: IO ()
writeTar = do
    BS.writeFile "./deleteme.tar" genTarBits

genTarBits :: BS.ByteString
genTarBits =
    let entry = either undefined id $ flip Tar.fileEntry "here is some text" <$> (Tar.toTarPath False "some/deleteme-inside.txt")
    in Tar.write [entry]


-- writeIncremental :: PackageIndex PkgInfo -> [TarIndexEntry] -> ByteString
-- writeIncremental pkgs =
--     Tar.write . catMaybes . map mkTarEntry
--   where
--     -- This should never return Nothing, it'd be an internal error but just
--     -- in case we'll skip them
--     mkTarEntry :: TarIndexEntry -> Maybe Tar.Entry

--     mkTarEntry (CabalFileEntry pkgid revno timestamp userid username) = do
--         pkginfo   <- PackageIndex.lookupPackageId pkgs pkgid
--         cabalfile <- fmap (cabalFileByteString . fst) $
--                      pkgMetadataRevisions pkginfo Vec.!? revno
--         tarPath   <- either (const Nothing) Just $
--                      Tar.toTarPath False fileName
--         let !tarEntry = addTimestampAndOwner timestamp userid username $
--                           Tar.fileEntry tarPath cabalfile
--         return tarEntry
--       where
--         pkgname = unPackageName (packageName pkgid)
--         fileName = pkgname </> display (packageVersion pkgid)
--                            </> pkgname <.> "cabal"

--     mkTarEntry (MetadataEntry pkgid revno timestamp) = do
--         pkginfo <- PackageIndex.lookupPackageId pkgs pkgid
--         let (filePath, content) = computePkgMetadata pkginfo revno
--         tarPath <- either (const Nothing) Just $ Tar.toTarPath False filePath
--         let !tarEntry = addTimestampAndOwner timestamp (UserId 0) (UserName "Hackage") $
--                           Tar.fileEntry tarPath content
--         return tarEntry

--     mkTarEntry (ExtraEntry fileName content timestamp) = do
--       tarPath <- either (const Nothing) Just $
--                   Tar.toTarPath False fileName
--       let !tarEntry = addTimestampAndOwner timestamp (UserId 0) (UserName "Hackage") $
--                         Tar.fileEntry tarPath content
--       return tarEntry

--     addTimestampAndOwner timestamp (UserId uid) (UserName username) entry =
--       entry {
--         Tar.entryTime      = utcToUnixTime timestamp,
--         Tar.entryOwnership = Tar.Ownership {
--           Tar.ownerName = username,
--           Tar.groupName = "Hackage",
--           Tar.ownerId = uid,
--           Tar.groupId = 0
--         }
--       }
