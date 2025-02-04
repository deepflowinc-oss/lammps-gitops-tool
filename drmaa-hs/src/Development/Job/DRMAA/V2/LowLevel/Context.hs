{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Development.Job.DRMAA.V2.LowLevel.Context (drmaa2Ctx) where

import qualified Data.Map.Strict as Map
import Development.Job.DRMAA.V2.LowLevel.Types
import qualified Development.Job.DRMAA.V2.LowLevel.Types as Raw
import Development.Job.DRMAA.V2.LowLevel.Types.OpaqueStructs (opaqueStructCtx)
import qualified Language.C.Inline as C
import Language.C.Inline.Context
import qualified Language.C.Types as C

drmaa2Ctx :: Context
drmaa2Ctx =
  C.baseCtx
    <> C.funCtx
    <> C.fptrCtx
    <> opaqueStructCtx
    <> drmaa2BasicTypeCtx
    <> drmaa2EnumCtx
    <> mempty
      { ctxTypesTable =
          Map.fromList
            [ (C.TypeName "drmaa2_jtemplate_s", [t|Raw.JobTemplateStruct|])
            , (C.TypeName "drmaa2_jinfo_s", [t|Raw.JobInfoStruct|])
            , (C.Struct "drmaa2_j_s", [t|Raw.JobStruct|])
            , (C.TypeName "drmaa2_jinfo", [t|Raw.JobInfo|])
            , (C.TypeName "drmaa2_jtemplate", [t|Raw.JobTemplate|])
            ]
      }
