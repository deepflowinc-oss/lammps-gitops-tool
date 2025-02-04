{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.Alias (type (∈)) where

import Effectful ((:>))

type e ∈ es = e :> es
