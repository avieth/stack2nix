{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Stack2nix.Render
   (render) where

import           Control.Lens
import           Control.Monad                           (when)
import           Data.Either                             (lefts, rights)
import           Data.List                               (filter, isPrefixOf,
                                                          sort)
import           Data.Monoid                             ((<>))
import           Data.Set                                (Set)
import qualified Data.Set                                as Set
import           Distribution.Nixpkgs.Haskell.BuildInfo  (haskell, pkgconfig,
                                                          system, tool)
import           Distribution.Nixpkgs.Haskell.Derivation (Derivation,
                                                          benchmarkDepends,
                                                          dependencies, doCheck,
                                                          pkgid, runHaddock,
                                                          testDepends)
import           Distribution.Text                       (display)
import           Distribution.Types.PackageId            (PackageIdentifier (..),
                                                          pkgName)
import           Distribution.Types.PackageName          (unPackageName)
import           Language.Nix                            (path)
import           Language.Nix.Binding                    (Binding, reference)
import           Language.Nix.PrettyPrinting             (disp)
import           Paths_stack2nix                         (version)
import           Stack2nix.Types                         (Args (..))
import           System.IO                               (hPutStrLn, stderr)
import qualified Text.PrettyPrint                        as PP
import           Text.PrettyPrint.HughesPJClass          (Doc, fcat, nest,
                                                          pPrint, punctuate,
                                                          semi, space, text)


-- TODO: this only covers GHC 8.0.2
basePackages :: Set String
basePackages = Set.fromList
  [ "array"
  , "base"
  , "binary"
  , "bytestring"
  , "Cabal"
  , "containers"
  , "deepseq"
  , "directory"
  , "filepath"
  , "ghc-boot"
  , "ghc-boot-th"
  , "ghc-prim"
  , "ghci"
  , "haskeline"
  , "hoopl"
  , "hpc"
  , "integer-gmp"
  , "pretty"
  , "process"
  , "rts"
  , "template-haskell"
  , "terminfo"
  , "time"
  , "transformers"
  , "unix"
  , "xhtml"
  ]

render :: [Either Doc Derivation] -> Args -> [String] -> String -> IO ()
render results args locals ghcnixversion = do
   let docs = lefts results
   when (length docs > 0) $ do
     hPutStrLn stderr $ show docs
     error "Error(s) happened during cabal2nix generation ^^"
   let drvs = rights results

   -- See what base packages are missing in the derivations list and null them
   let missing = sort $ Set.toList $ Set.difference basePackages $ Set.fromList (map drvToName drvs)
   let renderedMissing = map (\b -> nest 6 (text (b <> " = null;"))) missing

   let out = defaultNix ghcnixversion $ renderedMissing ++ map (renderOne args locals) drvs

   case argOutFile args of
     Just fname -> writeFile fname out
     Nothing    -> putStrLn out

renderOne :: Args -> [String] -> Derivation -> Doc
renderOne args locals drv' = output
 where
  output = nest 6 $ PP.hang
    (PP.doubleQuotes (text pid) <> " = callPackage")
    2
    ("(" <> pPrint drv <> ") {" <> text (show pkgs) <> "};")
  pid  = drvToName drv
  deps = view dependencies drv
  allDeps = Set.union (view pkgconfig deps) (view system deps)
  nixPkgs :: [Binding]
  nixPkgs  = Set.toList allDeps
  -- filter out libX stuff to prevent breakage in generated set
  -- We're after things that look like
  --
  --   inhert (pkgs.xorg) libX*
  --
  -- which means we want to check the 3rd item of the identifier list.
  pkgFilter e = case view (reference . path) e of
    (_:_:ident:_) -> not ("libX" `Data.List.isPrefixOf` display ident)
    _ -> True
  nonXpkgs = filter pkgFilter nixPkgs
  pkgs = fcat $ punctuate space [ disp b <> semi | b <- nonXpkgs ]
  drv =
    filterDepends args isLocal drv'
      &  doCheck
      .~ (argTest args && isLocal)
      &  runHaddock
      .~ (argHaddock args && isLocal)
  isLocal = elem pid locals

filterDepends :: Args -> Bool -> Derivation -> Derivation
filterDepends args isLocal drv = drv & foldr
  (.)
  id
  (do
    (depend, predicate) <-
      [(Lens testDepends, argTest args), (Lens benchmarkDepends, argBench args)]
    binding <- [Lens haskell, Lens pkgconfig, Lens system, Lens tool]
    pure
      $  runLens depend
      .  runLens binding
      .~ (if predicate && isLocal
           then view (runLens depend . runLens binding) drv
           else Set.empty
         )
  )

drvToName :: Derivation -> String
drvToName drv = unPackageName $ pkgName $ view pkgid drv

defaultNix :: String -> [Doc] -> String
defaultNix ghcnixversion drvs = unlines $
 [ "# Generated using stack2nix " <> display version <> "."
 , "#"
 , "# Only works with sufficiently recent nixpkgs, e.g. \"NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/21a8239452adae3a4717772f4e490575586b2755.tar.gz\"."
 , ""
 , "{ pkgs ? (import <nixpkgs> {})"
 , ", compiler ? pkgs.haskell.packages.ghc" ++ ghcnixversion
 , "}:"
 , ""
 , "with pkgs.haskell.lib;"
 , ""
 , "let"
 , "  stackPackages = { pkgs, stdenv, callPackage }:"
 , "    self: {"
 ] ++ (map PP.render drvs) ++
 [ "    };"
 , "in compiler.override {"
 , "  initialPackages = stackPackages;"
 , "  configurationCommon = { ... }: self: super: {};"
 , "  compilerConfig = self: super: {};"
 , "}"
 ]
