{-
Copyright (C) 2013  Ellis Whitehead

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>
-}

{-# LANGUAGE OverloadedStrings #-}

module Args
( Arguments(..)
--, arguments_empty
, arguments_add
, arguments_close
, arguments
) where

import Data.Monoid
import System.Console.CmdArgs.Explicit

import qualified Data.Map as M
import qualified Data.Set as Set

data Arguments = Arguments
  { argumentsCmd :: String
  , argumentsArgs :: [String]
  , argumentsFlags :: [(String, String)]
  , argumentsHelp :: Bool
  }
  deriving (Show)

arguments_empty name = Arguments name [] [] False

arguments_add :: Mode Arguments
arguments_add = Mode
  { modeGroupModes = mempty
  , modeNames = ["add"]
  , modeValue = arguments_empty "add"
  , modeCheck = Right
  , modeReform = (const Nothing)
  , modeExpandAt = True
  , modeHelp = "Add a new task"
  , modeHelpSuffix = ["Add a new task and be a dude"]
  , modeArgs = ([flagArg updArgs "TITLE"], Nothing)
  , modeGroupFlags = toGroup
    [ flagReq ["parent", "p"] (upd "parent") "ID" "reference to parent of this item"
    , flagReq ["label", "l"] (upd "label") "LABEL" "A unique label for this item."
    , flagReq ["stage", "s"] (upd "stage") "STAGE" "new|incubator|today. Defaults to new."
    , flagReq ["tag", "t"] (upd "tag") "TAG" "Associate this item with the given tag or context.  Maybe be applied multiple times."
    , flagReq ["type"] (upd "type") "TYPE" "list|task. Defaults to task."
    , flagHelpSimple updHelp
    ]
  }

arguments_close :: Mode Arguments
arguments_close = mode "close" (arguments_empty "") "close an item" (flagArg updArgs "REF")
  [-- flagHelpSimple (("help", ""):)
  ]

arguments = modes "otot" (arguments_empty "") "OnTopOfThings for managing lists and tasks" [arguments_add, arguments_close]

updArgs value acc = Right $ acc { argumentsArgs = (argumentsArgs acc ++ [value]) }
upd name value acc = Right $ acc { argumentsFlags = argumentsFlags acc ++ [(name, value)] }
updHelp acc = acc { argumentsHelp = True }

---myModes :: Mode (CmdArgs MyOptions)
---myModes = cmdArgsMode $ modes [arguments_add, arguments_close]
---    &= verbosityArgs [explicit, name "Verbose", name "V"] []
---    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
---    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
---    &= help _PROGRAM_ABOUT
---    &= helpArg [explicit, name "help", name "h"]
---    &= program _PROGRAM_NAME
---
---_PROGRAM_NAME = "otot"
---_PROGRAM_VERSION = "0.0-a"
---_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
---_PROGRAM_ABOUT = "a sample CmdArgs program for you tinkering pleasure"
---_COPYRIGHT = "(C) Ellis Whitehead 2013"
