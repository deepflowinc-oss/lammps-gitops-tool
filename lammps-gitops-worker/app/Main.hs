{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Development.LAMMPS.GitOps.Worker

main :: IO ()
main = defaultMain $$versionInfo
