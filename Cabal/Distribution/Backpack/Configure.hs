{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NondecreasingIndentation #-}

-- WARNING: The contents of this module are HIGHLY experimental.
-- We may refactor it under you.

module Distribution.Backpack.Configure (
    configureComponentLocalBuildInfos,
) where

import Prelude ()
import Distribution.Compat.Prelude hiding ((<>))

import Distribution.Backpack
import Distribution.Backpack.PreExistingComponent
import Distribution.Backpack.ConfiguredComponent
import Distribution.Backpack.LinkedComponent
import Distribution.Backpack.InstantiatedComponent
import Distribution.Backpack.ComponentsGraph
import Distribution.Backpack.ModuleShape

import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Package
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.InstalledPackageInfo (InstalledPackageInfo
                                         ,emptyInstalledPackageInfo)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.ModuleName
import Distribution.Simple.Setup as Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.ComponentRequestedSpec
import Distribution.Verbosity
import qualified Distribution.Compat.Graph as Graph
import Distribution.Compat.Graph (Graph, IsNode(..))
import Distribution.Utils.Progress
import Distribution.Utils.LogProgress

import Data.Either
    ( lefts )
import qualified Data.Set as Set
import qualified Data.Map as Map
import Distribution.Text
    ( display )
import Text.PrettyPrint

------------------------------------------------------------------------------
-- Pipeline
------------------------------------------------------------------------------

configureComponentLocalBuildInfos
    :: Verbosity
    -> Bool                   -- use_external_internal_deps
    -> ComponentRequestedSpec
    -> Flag String            -- configIPID
    -> Flag ComponentId       -- configCID
    -> PackageDescription
    -> [PreExistingComponent]
    -> FlagAssignment         -- configConfigurationsFlags
    -> [(ModuleName, Module)] -- configInstantiateWith
    -> InstalledPackageIndex
    -> Compiler
    -> LogProgress ([ComponentLocalBuildInfo], InstalledPackageIndex)
configureComponentLocalBuildInfos
    verbosity use_external_internal_deps enabled ipid_flag cid_flag pkg_descr
    prePkgDeps flagAssignment instantiate_with installedPackageSet comp = do
    -- NB: In single component mode, this returns a *single* component.
    -- In this graph, the graph is NOT closed.
    graph0 <- case toComponentsGraph enabled pkg_descr of
                Left ccycle -> failProgress (componentCycleMsg ccycle)
                Right comps -> return comps
    infoProgress $ hang (text "Source component graph:") 4
                        (dispComponentsGraph graph0)

    let conf_pkg_map = Map.fromList
            [(pc_pkgname pkg, (pc_cid pkg, pc_pkgid pkg))
            | pkg <- prePkgDeps]
        graph1 = toConfiguredComponents use_external_internal_deps
                    flagAssignment
                    ipid_flag cid_flag pkg_descr
                    conf_pkg_map (map fst graph0)
    infoProgress $ hang (text "Configured component graph:") 4
                        (vcat (map dispConfiguredComponent graph1))

    let shape_pkg_map = Map.fromList
            [ (pc_cid pkg, (pc_uid pkg, pc_shape pkg))
            | pkg <- prePkgDeps]
    graph2 <- toLinkedComponents verbosity (package pkg_descr) shape_pkg_map graph1

    infoProgress $
        hang (text "Linked component graph:") 4
             (vcat (map dispLinkedComponent graph2))

    let pid_map = Map.fromList $
            [ (pc_cid pkg, pc_pkgid pkg)
            | pkg <- prePkgDeps] ++
            [ (Installed.installedComponentId pkg, Installed.sourcePackageId pkg)
            | (_, Module uid _) <- instantiate_with
            , Just pkg <- [PackageIndex.lookupUnitId
                                installedPackageSet uid] ] ++
            [ (lc_cid lc, lc_pkgid lc)
            | lc <- graph2 ]
        subst = mkModSubst instantiate_with
        graph3 = toInstantiatedComponents pid_map subst graph2
        graph4 = Graph.topSort (Graph.fromList graph3)

    infoProgress $ hang (text "Instantiated component graph:") 4
                        (vcat (map dispInstantiatedComponent graph4))

    toComponentLocalBuildInfos comp installedPackageSet pkg_descr prePkgDeps graph4

------------------------------------------------------------------------------
-- ComponentLocalBuildInfo
------------------------------------------------------------------------------

toComponentLocalBuildInfos
    :: Compiler
    -> InstalledPackageIndex -- FULL set
    -> PackageDescription
    -> [PreExistingComponent] -- external package deps
    -> [InstantiatedComponent]
    -> LogProgress ([ComponentLocalBuildInfo],
                    InstalledPackageIndex) -- only relevant packages
toComponentLocalBuildInfos
    comp installedPackageSet pkg_descr externalPkgDeps graph = do
    -- Check and make sure that every instantiated component exists.
    -- We have to do this now, because prior to linking/instantiating
    -- we don't actually know what the full set of 'UnitId's we need
    -- are.
    let -- TODO: This is actually a bit questionable performance-wise,
        -- since we will pay for the ALL installed packages even if
        -- they are not related to what we are building.  This was true
        -- in the old configure code.
        external_graph :: Graph (Either InstalledPackageInfo LinkedComponent)
        external_graph = Graph.fromList
                       . map Left
                       $ PackageIndex.allPackages installedPackageSet
        internal_graph :: Graph (Either InstalledPackageInfo LinkedComponent)
        internal_graph = Graph.fromList
                       . map Right
                       $ graph
        combined_graph = Graph.unionRight external_graph internal_graph
        Just local_graph = Graph.closure combined_graph (map nodeKey graph)
        -- The database of transitively reachable installed packages that the
        -- external components the package (as a whole) depends on.  This will be
        -- used in several ways:
        --
        --      * We'll use it to do a consistency check so we're not depending
        --        on multiple versions of the same package (TODO: someday relax
        --        this for private dependencies.)  See right below.
        --
        --      * We'll pass it on in the LocalBuildInfo, where preprocessors
        --        and other things will incorrectly use it to determine what
        --        the include paths and everything should be.
        --
        packageDependsIndex = PackageIndex.fromList (lefts local_graph)
        fullIndex = Graph.fromList local_graph
    case Graph.broken fullIndex of
        [] -> return ()
        broken ->
          -- TODO: ppr this
          failProgress . text $
                "The following packages are broken because other"
             ++ " packages they depend on are missing. These broken "
             ++ "packages must be rebuilt before they can be used.\n"
             -- TODO: Undupe.
             ++ unlines [ "installed package "
                       ++ display (packageId pkg)
                       ++ " is broken due to missing package "
                       ++ intercalate ", " (map display deps)
                        | (Left pkg, deps) <- broken ]
             ++ unlines [ "planned package "
                       ++ display (packageId pkg)
                       ++ " is broken due to missing package "
                       ++ intercalate ", " (map display deps)
                        | (Right pkg, deps) <- broken ]

    -- In this section, we'd like to look at the 'packageDependsIndex'
    -- and see if we've picked multiple versions of the same
    -- installed package (this is bad, because it means you might
    -- get an error could not match foo-0.1:Type with foo-0.2:Type).
    --
    -- What is pseudoTopPkg for? I have no idea.  It was used
    -- in the very original commit which introduced checking for
    -- inconsistencies 5115bb2be4e13841ea07dc9166b9d9afa5f0d012,
    -- and then moved out of PackageIndex and put here later.
    -- TODO: Try this code without it...
    --
    -- TODO: Move this into a helper function
    --
    -- TODO: This is probably wrong for Backpack
    let pseudoTopPkg :: InstalledPackageInfo
        pseudoTopPkg = emptyInstalledPackageInfo {
            Installed.installedUnitId =
               mkLegacyUnitId (packageId pkg_descr),
            Installed.sourcePackageId = packageId pkg_descr,
            Installed.depends =
              map pc_uid externalPkgDeps
          }
    case PackageIndex.dependencyInconsistencies
       . PackageIndex.insert pseudoTopPkg
       $ packageDependsIndex of
      [] -> return ()
      inconsistencies ->
        warnProgress . text $
             "This package indirectly depends on multiple versions of the same "
          ++ "package. This is highly likely to cause a compile failure.\n"
          ++ unlines [ "package " ++ display pkg ++ " requires "
                    ++ display (PackageIdentifier name ver)
                     | (name, uses) <- inconsistencies
                     , (pkg, ver) <- uses ]
    let clbis = mkLinkedComponentsLocalBuildInfo comp graph
    -- forM clbis $ \(clbi,deps) -> info verbosity $ "UNIT" ++ hashUnitId (componentUnitId clbi) ++ "\n" ++ intercalate "\n" (map hashUnitId deps)
    return (clbis, packageDependsIndex)

-- Build ComponentLocalBuildInfo for each component we are going
-- to build.
mkLinkedComponentsLocalBuildInfo
    :: Compiler
    -> [LinkedComponent]
    -> [ComponentLocalBuildInfo]
mkLinkedComponentsLocalBuildInfo comp lcs = map go lcs
  where
    internalUnits = Set.fromList (map lc_uid lcs)
    isInternal x = Set.member x internalUnits
    go lc =
        -- Knot tied!
        let clbi = go' lc internal_deps
            internal_deps =
                filter isInternal (map fst (componentPackageDeps clbi))
               ++ lc_internal_build_tools lc
        in clbi
    go' lc internal_deps =
      case lc_component lc of
      CLib _ ->
        let convExport (modname', modu@(Module uid modname))
                | this_uid == uid
                , modname' == modname
                = Installed.ExposedModule modname' Nothing
                | otherwise
                = Installed.ExposedModule modname' (Just modu)
            convExport (_, ModuleVar _)
                = error "mkLinkedComponentsLocalBuildInfo: bare ModuleVar"
            exports = map convExport (Map.toList (modShapeProvides (lc_shape lc)))
        in LibComponentLocalBuildInfo {
          componentPackageDeps = cpds,
          componentUnitId = this_uid,
          componentInstantiatedWith = lc_insts lc,
          componentLocalName = cname,
          componentInternalDeps = internal_deps,
          componentExeDeps = lc_internal_build_tools lc,
          componentIncludes = includes,
          componentExposedModules = exports,
          componentIsPublic = lc_public lc,
          componentCompatPackageKey = lc_compat_key lc comp,
          componentCompatPackageName = lc_compat_name lc
        }
      CExe _ ->
        ExeComponentLocalBuildInfo {
          componentUnitId = this_uid,
          componentLocalName = cname,
          componentPackageDeps = cpds,
          componentExeDeps = lc_internal_build_tools lc,
          componentInternalDeps = internal_deps,
          componentIncludes = includes
        }
      CTest _ ->
        TestComponentLocalBuildInfo {
          componentUnitId = this_uid,
          componentLocalName = cname,
          componentPackageDeps = cpds,
          componentExeDeps = lc_internal_build_tools lc,
          componentInternalDeps = internal_deps,
          componentIncludes = includes
        }
      CBench _ ->
        BenchComponentLocalBuildInfo {
          componentUnitId = this_uid,
          componentLocalName = cname,
          componentPackageDeps = cpds,
          componentExeDeps = lc_internal_build_tools lc,
          componentInternalDeps = internal_deps,
          componentIncludes = includes
        }
     where
      this_uid = lc_uid lc
      is_definite = Set.null (unitIdFreeHoles this_uid)
      cname = componentName (lc_component lc)
      cpds = if is_definite
                then lc_depends lc
                else map (\(uid, pid) -> (generalizeUnitId uid, pid)) (lc_depends lc)
      includes = map (\(uid, provs) -> (uid, provs)) (lc_includes lc)


