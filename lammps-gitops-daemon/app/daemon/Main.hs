{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Development.LAMMPS.GitOps.App.Daemon (defaultMain, versionInfo)

main :: IO ()
main = defaultMain $$versionInfo
