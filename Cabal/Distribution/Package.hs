{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Package
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Defines a package identifier along with a parser and pretty printer for it.
-- 'PackageIdentifier's consist of a name and an exact version. It also defines
-- a 'Dependency' data type. A dependency is a package name and a version
-- range, like @\"foo >= 1.2 && < 2\"@.

module Distribution.Package (
        -- * Package ids
        PackageName, unPackageName, mkPackageName,
        PackageIdentifier(..),
        PackageId,

        -- * Package keys/installed package IDs (used for linker symbols)
        ComponentId(..),
        UnitId(..),
        hashUnitId,
        rawHashUnitId,
        mkUnitId,
        mkLegacyUnitId,
        unitIdComponentId,
        getHSLibraryName,
        InstalledPackageId, -- backwards compat

        -- * Modules
        Module(..),
        ModuleSubst,
        modSubstToList,
        mkModSubst,
        dispModSubst,
        dispModSubstEntry,
        parseModSubst,
        parseModSubstEntry,

        -- * ABI hash
        AbiHash(..),

        -- * Package source dependencies
        Dependency(..),
        thisPackageVersion,
        notThisPackageVersion,
        simplifyDependency,

        -- * Package classes
        Package(..), packageName, packageVersion,
        HasUnitId(..),
        installedPackageId,
        PackageInstalled(..),
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Version
         ( Version(..), VersionRange, anyVersion, thisVersion
         , notThisVersion, simplifyVersionRange )

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Distribution.Compat.ReadP
import Distribution.Text
import Distribution.ModuleName
import Distribution.Utils.Base62

import Text.PrettyPrint ((<+>), text, hcat)
import qualified Data.Map as Map

-- | A package name.
--
-- Use 'mkPackageName' and 'unPackageName' to convert from/to a
-- 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0
newtype PackageName = PackageName String
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

-- | Convert 'PackageName' to 'String'
unPackageName :: PackageName -> String
unPackageName (PackageName s) = s

-- | Construct a 'PackageName' from a 'String'
--
-- 'mkPackageName' is the inverse to 'unPackageName'
--
-- Note: No validations are performed to ensure that the resulting
-- 'PackageName' is valid
--
-- @since 2.0
mkPackageName :: String -> PackageName
mkPackageName = PackageName

instance Binary PackageName

instance Text PackageName where
  disp = Disp.text . unPackageName
  parse = do
    ns <- Parse.sepBy1 component (Parse.char '-')
    return (mkPackageName (intercalate "-" ns))
    where
      component = do
        cs <- Parse.munch1 isAlphaNum
        if all isDigit cs then Parse.pfail else return cs
        -- each component must contain an alphabetic character, to avoid
        -- ambiguity in identifiers like foo-1 (the 1 is the version number).

instance NFData PackageName where
    rnf (PackageName pkg) = rnf pkg

-- | Type alias so we can use the shorter name PackageId.
type PackageId = PackageIdentifier

-- | The name and version of a package.
data PackageIdentifier
    = PackageIdentifier {
        pkgName    :: PackageName, -- ^The name of this package, eg. foo
        pkgVersion :: Version -- ^the version of this package, eg 1.2
     }
     deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary PackageIdentifier

instance Text PackageIdentifier where
  disp (PackageIdentifier n v) = case v of
    Version [] _ -> disp n -- if no version, don't show version.
    _            -> disp n <<>> Disp.char '-' <<>> disp v

  parse = do
    n <- parse
    v <- (Parse.char '-' >> parse) <++ return (Version [] [])
    return (PackageIdentifier n v)

instance NFData PackageIdentifier where
    rnf (PackageIdentifier name version) = rnf name `seq` rnf version

-- | A module identity uniquely identifies a Haskell module by
-- qualifying a 'ModuleName' with the 'UnitId' which defined
-- it.  This type distinguishes between two packages
-- which provide a module with the same name, or a module
-- from the same package compiled with different dependencies.
-- There are a few cases where Cabal needs to know about
-- module identities, e.g., when writing out reexported modules in
-- the 'InstalledPackageInfo'.  For more details, see the
-- Backpack specification at
--  https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst
--
-- In GHC, this is just a 'Module' constructor for backwards
-- compatibility reasons, but in Cabal we can do it properly.
data Module =
      -- | A \"normal\" concrete module that is defined in a specific package.
      Module UnitId ModuleName

      -- | A module variable, standing for a module that needs to be filled in.
    | ModuleVar ModuleName
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary Module

instance Text Module where
    disp (Module uid mod_name) =
        hcat [disp uid, Disp.text ":", disp mod_name]
    disp (ModuleVar mod_name) =
        hcat [Disp.char '<', disp mod_name, Disp.char '>']
    parse = parseModuleVar <++ parseModule
      where
        parseModuleVar = do
            _ <- Parse.char '<'
            mod_name <- parse
            _ <- Parse.char '>'
            return (ModuleVar mod_name)
        parseModule = do
            uid <- parse
            _ <- Parse.char ':'
            mod_name <- parse
            return (Module uid mod_name)

instance NFData Module where
    rnf (Module uid mod_name) = rnf uid `seq` rnf mod_name
    rnf (ModuleVar mod_name) = rnf mod_name

-- | A 'ComponentId' uniquely identifies the transitive source
-- code closure of a component (i.e. libraries, executables).
--
-- For non-Backpack components, this corresponds one to one with
-- the 'UnitId', which serves as the basis for install paths,
-- linker symbols, etc.
--
data ComponentId
    = ComponentId String
    deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

{-# DEPRECATED InstalledPackageId "Use UnitId instead" #-}
type InstalledPackageId = UnitId

instance Binary ComponentId

instance Text ComponentId where
  disp (ComponentId str) = text str

  parse = ComponentId `fmap` Parse.munch1 abi_char
   where abi_char c = isAlphaNum c || c `elem` "-_."

instance NFData ComponentId where
    rnf (ComponentId pk) = rnf pk

-- | Returns library name prefixed with HS, suitable for filenames
getHSLibraryName :: UnitId -> String
getHSLibraryName (UnitIdVar _) = error "getHSLibraryName: unbound variable"
getHSLibraryName uid = "HS" ++ hashUnitId uid

-- | An explicit substitution on modules.
--
-- NB: These substitutions are NOT idempotent, for example, a
-- valid substitution is (A -> B, B -> A).
type ModuleSubst = Map ModuleName Module

-- | Create a module substitution from a list.
mkModSubst :: [(ModuleName, Module)] -> ModuleSubst
mkModSubst = Map.fromList

-- | Convert a module substitution into a (ascending sorted) list.
modSubstToList :: ModuleSubst -> [(ModuleName, Module)]
modSubstToList = Map.toAscList

-- | Pretty-print the entries of a module substitution, suitable
-- for embedding into a 'UnitId' or passing to GHC via @--instantiate-with@.
dispModSubst :: ModuleSubst -> Disp.Doc
dispModSubst subst
    = Disp.hcat
    . Disp.punctuate Disp.comma
    $ map dispModSubstEntry (Map.toAscList subst)

-- | Pretty-print a single entry of a module substitution.
dispModSubstEntry :: (ModuleName, Module) -> Disp.Doc
dispModSubstEntry (k, v) = disp k <<>> Disp.char '=' <<>> disp v

-- | Inverse to 'dispModSubst'.
parseModSubst :: ReadP r ModuleSubst
parseModSubst = fmap mkModSubst
      . flip Parse.sepBy (Parse.char ',')
      $ parseModSubstEntry

-- | Inverse to 'dispModSubstEntry'.
parseModSubstEntry :: ReadP r (ModuleName, Module)
parseModSubstEntry =
    do k <- parse
       _ <- Parse.char '='
       v <- parse
       return (k, v)

-- | A unit is a specific incarnation of a component (library or executable),
-- and a unit identifier uniquely identifies a unit.
--
-- In the /absence of Backpack/ units and components are exactly the same and
-- a 'UnitId' is the same as a 'ComponentId'.
--
-- For a source component using Backpack however there is more structure as
-- components may be parametrized over some signatures, and these \"holes\"
-- may be partially or wholly filled.
--
-- So a unit is a component plus the additional information on how the holes
-- are filled in. Thus there is a one to many relationship: for a particular
-- component there are many different ways of filling in the holes, and each
-- different combination is a unit (and has a separate 'UnitId').
--
-- For more details see the Backpack spec
-- <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
--
-- The 'UnitId' is used to refer to units throughout the various stages of
-- defining, composing, compiling and installing. The 'UnitId' is the primary
-- key for installed package databases (i.e. @ghc-pkg@ and friends). But note
-- that not all forms of units can exist in an installed state. In particular
-- only units that have /none/ or /all/ of their holes filled in can be
-- installed, (and only those with all their holes filled are installed with
-- object code).
--
data UnitId =

     -- | A \"normal\" non-Backpack component.
     --
     -- In the Backpack view of the world, this is a special case of the more
     -- general 'UnitId', equivalent to @'UnitId' cid Map.empty@.
     --
     SimpleUnitId ComponentId

     -- | A Backpack component in a form that can exist in an installed state.
     -- This is either with all holes filled or none.
     --
     -- An installed unit with all holes unfilled only contains interface or
     -- \"signature\" information, while a unit with all holes filled can be
     -- compiled to object code and installed.
     --
     -- The extra string is a hash of the 'ModuleSubst' which describes the
     -- holes and if and how they are filled in.
     --
   | HashedUnitId ComponentId String

     -- | A Backpack component plus the description of the holes and if and
     -- how they are filled in.
     --
     -- The 'ModuleSubst' is kept in a structured form that allows further
     -- transformation (such as filling in more holes).
     --
     -- This form of unit cannot be installed. It must first be converted to
     -- a 'HashedUnitId'.
     --
   | UnitId ComponentId ModuleSubst

     -- | A form of unit identifier used during intermediate stages of
     -- definition and composition.
     --
     -- This form of unit cannot be installed. It must first be converted to
     -- a 'HashedUnitId'.
     --
   | UnitIdVar !Int -- de Bruijn indexed
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

-- | Hash a unit identifier into a string suitable for use
-- on a file system.
--
-- This function is not in "Distribution.Backpack" because we need it
-- for 'getHSLibraryName' which has traditionally lived
-- in "Distribution.Package". TODO: this is pretty inefficient!
hashUnitId :: UnitId -> String
hashUnitId (SimpleUnitId cid) = display cid
hashUnitId (HashedUnitId cid hash) = display cid ++ "+" ++ hash
hashUnitId (UnitIdVar i)      = show i -- this is never bare
hashUnitId (UnitId cid subst)
  -- A fully indefinite package with no instantiations simply
  -- has its 'ComponentId' as the hashed 'UnitId'.
  | all (\(k, v) -> v == ModuleVar k) (modSubstToList subst)
     = display cid
  | otherwise
     = hashUnitId (HashedUnitId cid (rawHashUnitId subst))

-- | Hash ONLY the 'ModuleSubst' of a 'UnitId'; useful if you need
-- to create separate subdirectories for different instantiations
-- for a particular component id (e.g., if you are building inplace.)
rawHashUnitId :: ModuleSubst -> String
rawHashUnitId subst =
    hashToBase62 $
        concat [ display mod_name ++ "=" ++ hashUnitId uid ++ ":" ++ display m  ++ "\n"
               | (mod_name, Module uid m) <- modSubstToList subst]

instance Binary UnitId

instance NFData UnitId where
    rnf (SimpleUnitId cid) = rnf cid
    rnf (HashedUnitId cid hash) = rnf cid `seq` rnf hash
    rnf (UnitId cid insts) = rnf cid `seq` rnf insts
    rnf (UnitIdVar i) = rnf i

instance Text UnitId where
    disp (SimpleUnitId cid) = disp cid
    disp (UnitIdVar i) = Disp.char '?' <<>> Disp.int i
    disp (HashedUnitId cid hash) = disp cid <<>> Disp.char '+' <<>> Disp.text hash
    disp (UnitId cid insts) = disp cid <<>> Disp.brackets (dispModSubst insts)

    parse = parseUnitIdVar <++ parseUnitId <++ parseHashedUnitId <++ parseSimpleUnitId
      where
        parseUnitIdVar = do _ <- Parse.char '?'
                            fmap UnitIdVar (readS_to_P reads)
        parseUnitId = do cid <- parse
                         insts <- Parse.between (Parse.char '[') (Parse.char ']')
                                    parseModSubst
                         return (UnitId cid insts)
        parseHashedUnitId = do cid <- parse
                               _ <- Parse.char '+'
                               hash <- Parse.munch1 isAlphaNum
                               return (HashedUnitId cid hash)
        parseSimpleUnitId = fmap SimpleUnitId parse

-- | Makes a simple-style UnitId from a string.
mkUnitId :: String -> UnitId
mkUnitId = SimpleUnitId . ComponentId

-- | Make an old-style UnitId from a package identifier
mkLegacyUnitId :: PackageId -> UnitId
mkLegacyUnitId = SimpleUnitId . ComponentId . display

-- | Extract 'ComponentId' from 'UnitId'.
unitIdComponentId :: UnitId -> ComponentId
unitIdComponentId (SimpleUnitId cid) = cid
unitIdComponentId (UnitId cid _) = cid
unitIdComponentId (HashedUnitId cid _) = cid
unitIdComponentId (UnitIdVar _) = error "unitIdComponentId: top-level UnitIdVar"

-- ------------------------------------------------------------
-- * Package source dependencies
-- ------------------------------------------------------------

-- | Describes a dependency on a source package (API)
--
data Dependency = Dependency PackageName VersionRange
                  deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary Dependency

instance Text Dependency where
  disp (Dependency name ver) =
    disp name <+> disp ver

  parse = do name <- parse
             Parse.skipSpaces
             ver <- parse <++ return anyVersion
             Parse.skipSpaces
             return (Dependency name ver)

instance NFData Dependency where rnf = genericRnf

thisPackageVersion :: PackageIdentifier -> Dependency
thisPackageVersion (PackageIdentifier n v) =
  Dependency n (thisVersion v)

notThisPackageVersion :: PackageIdentifier -> Dependency
notThisPackageVersion (PackageIdentifier n v) =
  Dependency n (notThisVersion v)

-- | Simplify the 'VersionRange' expression in a 'Dependency'.
-- See 'simplifyVersionRange'.
--
simplifyDependency :: Dependency -> Dependency
simplifyDependency (Dependency name range) =
  Dependency name (simplifyVersionRange range)

-- | Class of things that have a 'PackageIdentifier'
--
-- Types in this class are all notions of a package. This allows us to have
-- different types for the different phases that packages go though, from
-- simple name\/id, package description, configured or installed packages.
--
-- Not all kinds of packages can be uniquely identified by a
-- 'PackageIdentifier'. In particular, installed packages cannot, there may be
-- many installed instances of the same source package.
--
class Package pkg where
  packageId :: pkg -> PackageIdentifier

packageName    :: Package pkg => pkg -> PackageName
packageName     = pkgName    . packageId

packageVersion :: Package pkg => pkg -> Version
packageVersion  = pkgVersion . packageId

instance Package PackageIdentifier where
  packageId = id

-- | Packages that have an installed unit ID
class Package pkg => HasUnitId pkg where
  installedUnitId :: pkg -> UnitId

{-# DEPRECATED installedPackageId "Use installedUnitId instead" #-}
-- | Compatibility wrapper for Cabal pre-1.24.
installedPackageId :: HasUnitId pkg => pkg -> UnitId
installedPackageId = installedUnitId

-- | Class of installed packages.
--
-- The primary data type which is an instance of this package is
-- 'InstalledPackageInfo', but when we are doing install plans in Cabal install
-- we may have other, installed package-like things which contain more metadata.
-- Installed packages have exact dependencies 'installedDepends'.
class (HasUnitId pkg) => PackageInstalled pkg where
  installedDepends :: pkg -> [UnitId]

-- -----------------------------------------------------------------------------
-- ABI hash

newtype AbiHash = AbiHash String
    deriving (Eq, Show, Read, Generic)
instance Binary AbiHash

instance Text AbiHash where
    disp (AbiHash abi) = Disp.text abi
    parse = fmap AbiHash (Parse.munch isAlphaNum)
