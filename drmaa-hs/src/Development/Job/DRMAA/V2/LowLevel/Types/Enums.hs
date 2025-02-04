{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Job.DRMAA.V2.LowLevel.Types.Enums (
  drmaa2EnumTable,
  drmaa2EnumCtx,
  Error (
    ..,
    Success,
    DeniedByDrms,
    DrmCommunication,
    TryLater,
    SessionManagement,
    Timeout,
    Internal,
    InvalidArgument,
    InvalidSession,
    InvalidState,
    OutOfResource,
    UnsupportedAttribute,
    UnsupportedOperation,
    ImplementationSpecific,
    Lasterror
  ),
  JobState (
    ..,
    Undetermined,
    Queued,
    QueuedHeld,
    Running,
    Suspended,
    Requeued,
    RequeuedHeld,
    Done,
    Failed
  ),
  OS (
    ..,
    OtherOS,
    AIX,
    BSD,
    Linux,
    HPUX,
    IRIX,
    MacOS,
    SunOS,
    TRU64,
    UnixWare,
    Win,
    Wint
  ),
  CPU (
    ..,
    OtherCPU,
    ALPHA,
    ARM,
    ARM64,
    CELL,
    PARISC,
    PARISC64,
    X86,
    X64,
    IA64,
    MIPS,
    MIPS64,
    PPC,
    PPC64,
    SPARC,
    SPARC64
  ),
  DRMAAListType (
    ..,
    StringList,
    JobList,
    QueueInfoList,
    MachineInfoList,
    SlotInfoList,
    ReservationList
  ),
) where

import qualified Data.Map.Strict as Map
import Foreign.C
import Foreign.C.Enum.TH (defineCEnum)
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

defineCEnum
  "Error"
  [ "Success"
  , "DeniedByDrms"
  , "DrmCommunication"
  , "TryLater"
  , "SessionManagement"
  , "Timeout"
  , "Internal"
  , "InvalidArgument"
  , "InvalidSession"
  , "InvalidState"
  , "OutOfResource"
  , "UnsupportedAttribute"
  , "UnsupportedOperation"
  , "ImplementationSpecific"
  , "Lasterror"
  ]

defineCEnum
  "JobState"
  [ "Undetermined"
  , "Queued"
  , "QueuedHeld"
  , "Running"
  , "Suspended"
  , "Requeued"
  , "RequeuedHeld"
  , "Done"
  , "Failed"
  ]

defineCEnum
  "OS"
  [ "OtherOS"
  , "AIX"
  , "BSD"
  , "Linux"
  , "HPUX"
  , "IRIX"
  , "MacOS"
  , "SunOS"
  , "TRU64"
  , "UnixWare"
  , "Win"
  , "Wint"
  ]

defineCEnum
  "DRMAAListType"
  [ "StringList"
  , "JobList"
  , "QueueInfoList"
  , "MachineInfoList"
  , "SlotInfoList"
  , "ReservationList"
  ]

defineCEnum
  "CPU"
  [ "OtherCPU"
  , "ALPHA"
  , "ARM"
  , "ARM64"
  , "CELL"
  , "PARISC"
  , "PARISC64"
  , "X86"
  , "X64"
  , "IA64"
  , "MIPS"
  , "MIPS64"
  , "PPC"
  , "PPC64"
  , "SPARC"
  , "SPARC64"
  ]

drmaa2EnumCtx :: C.Context
drmaa2EnumCtx = mempty {C.ctxTypesTable = drmaa2EnumTable}

drmaa2EnumTable :: C.TypesTable
drmaa2EnumTable =
  Map.fromList
    [ (C.TypeName "drmaa2_error", [t|Error|])
    , (C.Enum "drmaa2_error_", [t|Error|])
    , (C.TypeName "drmaa2_jstate", [t|JobState|])
    , (C.Enum "drmaa2_jstate_", [t|JobState|])
    , (C.TypeName "drmaa2_os", [t|OS|])
    , (C.Enum "drmaa2_os_", [t|OS|])
    , (C.TypeName "drmaa2_cpu", [t|CPU|])
    , (C.Enum "drmaa2_cpu_", [t|CPU|])
    , (C.TypeName "drmaa2_listtype", [t|DRMAAListType|])
    , (C.Enum "drmaa2_listtype_", [t|DRMAAListType|])
    ]
