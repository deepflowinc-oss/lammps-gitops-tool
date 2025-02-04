{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Development.LAMMPS.GitOps.App.Admin (defaultMain, versionInfo)

main :: IO ()
main = defaultMain $$versionInfo
