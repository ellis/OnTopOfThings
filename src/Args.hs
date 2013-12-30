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

module Args where

import System.Console.CmdArgs.Explicit


arguments_add :: Mode [(String, String)]
arguments_add = mode "add" [] "add a new item" (flagArg (upd "title") "TITLE")
  [ flagReq ["parent", "p"] (upd "parent") "PARENT" "reference to parent of this item"
  , flagReq ["stage", "s"] (upd "stage") "TYPE" "new|incubator|today. Defaults to new."
  , flagReq ["type", "t"] (upd "type") "TYPE" "list|task. Defaults to task."
  , flagHelpSimple (("help", ""):)
  ] where
    upd name value acc = Right $ (name, value) : acc

arguments_close :: Mode [(String, String)]
arguments_close = mode "close" [] "close an item" (flagArg (upd "ref") "REF")
  [ flagHelpSimple (("help", ""):)
  ] where
    upd name value acc = Right $ (name, value) : acc

arguments = modes "otot" [] "OnTopOfThings for managing lists and tasks" [arguments_add, arguments_close]

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
