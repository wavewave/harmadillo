{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
--
import FFICXX.Generate.Builder        ( simpleBuilder )
import FFICXX.Generate.Code.Primitive ( bool_
                                      , charpp
                                      , cppclass, cppclass_
                                      , cstring, cstring_
                                      , double, double_
                                      , int, int_
                                      , uint, uint_
                                      , void_, voidp
                                      )
import FFICXX.Generate.Config         ( FFICXXConfig(..)
                                      , SimpleBuilderConfig(..)
                                      )
import FFICXX.Generate.Type.Cabal     ( BuildType(..), Cabal(..), CabalName(..) )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..), ModuleUnitMap(..), ModuleUnitImports(..) )
import FFICXX.Generate.Type.Class     ( Class(..)
                                      , ClassAlias(..)
                                      , CTypes(CTDouble)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TemplateClass(..)
                                      , TemplateFunction(..)
                                      , TopLevelFunction(..)
                                      , Types(..)
                                      , Variable(..)
                                      )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..)
                                      , ModuleUnitImports(..)
                                      )
import FFICXX.Generate.Type.PackageInterface ( Namespace(..), HeaderName(..) )


------------------------
-- import from stdcxx --
------------------------

stdcxx_cabal :: Cabal
stdcxx_cabal = Cabal {
    cabal_pkgname            = CabalName "stdcxx"
  , cabal_version            = "0.5"
  , cabal_cheaderprefix      = "STD"
  , cabal_moduleprefix       = "STD"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = []
  , cabal_license            = Nothing
  , cabal_licensefile        = Nothing
  , cabal_extraincludedirs   = []
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = []
  , cabal_buildType          = Simple
  }

deletable :: Class
deletable =
  AbstractClass {
      class_cabal      = stdcxx_cabal
    , class_name       = "Deletable"
    , class_parents    = []
    , class_protected  = Protected []
    , class_alias      = Nothing
    , class_funcs      = [ Destructor Nothing ]
    , class_vars       = []
    , class_tmpl_funcs = []
    }

-----------------
-- start hgdal --
-----------------

modImports ::
     String
  -> [String]
  -> [HeaderName]
  -> (ModuleUnit,ModuleUnitImports)
modImports n ns hs =
  ( MU_Class n
  , ModuleUnitImports {
      muimports_namespaces = map NS ns
    , muimports_headers    = hs
    }
  )


cabal = Cabal {
    cabal_pkgname            = CabalName "harmadillo"
  , cabal_version            = "0.1.0.0"
  , cabal_cheaderprefix      = "Arma"
  , cabal_moduleprefix       = "Arma"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = [ CabalName "stdcxx" ]
  , cabal_license            = Just "BSD3"
  , cabal_licensefile        = Just "LICENSE"
  , cabal_extraincludedirs   = []
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = [ "armadillo" ]
  , cabal_buildType          = Simple
  }

armaclass :: String -> Maybe String -> [Class] -> [Function] -> Class
armaclass n ma ps fs =
  Class {
      class_cabal      = cabal
    , class_name       = n
    , class_parents    = ps
    , class_protected  = Protected []
    , class_alias      = fmap (\a -> ClassAlias { caHaskellName = a, caFFIName = n }) ma
    , class_funcs      = fs
    , class_vars       = []
    , class_tmpl_funcs = []
    }


arma_rng :: Class
arma_rng =
  armaclass "arma_rng" (Just "ArmaRng")
  []
  [ Static void_ "set_seed_random" [] Nothing
  ]

t_mat :: TemplateClass
t_mat =
  TmplCls cabal "Mat" "arma::Mat" "t"
  [ TFun (TemplateType t_mat) "randu" "randu" [ uint "in_rows", uint "in_cols" ] Nothing
  ]

classes =
  [ arma_rng
  ]

toplevelfunctions :: [TopLevelFunction]
toplevelfunctions =
  [
  ]

templates =
  [ (t_mat, HdrName "armadillo")
  ]

headers =
  [ modImports "arma_rng" ["arma"] ["armadillo"]
  ]

extraLib = []

extraDep = []


main :: IO ()
main = do
  args <- getArgs
  let tmpldir =  if length args == 1
                 then args !! 0
                 else "../template"

  cwd <- getCurrentDirectory
  let fficfg = FFICXXConfig {
                 fficxxconfig_workingDir     = cwd </> "tmp" </> "working"
               , fficxxconfig_installBaseDir = cwd </> "harmadillo"
               , fficxxconfig_staticFileDir  = tmpldir
               }
      sbcfg  = SimpleBuilderConfig {
                 sbcTopModule  = "Arma"
               , sbcModUnitMap = ModuleUnitMap (HM.fromList headers)
               , sbcCabal      = cabal
               , sbcClasses    = classes
               , sbcTopLevels  = toplevelfunctions
               , sbcTemplates  = templates
               , sbcExtraLibs  = extraLib
               , sbcExtraDeps  = extraDep
               , sbcStaticFiles = ["LICENSE"]
               }

  simpleBuilder fficfg sbcfg
