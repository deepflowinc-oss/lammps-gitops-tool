{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Development.LAMMPS.GitOps.App.Client (defaultMain, versionInfo)

main :: IO ()
main = defaultMain $$versionInfo
