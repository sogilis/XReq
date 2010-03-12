--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with AdaSpecLib.Tables;

use Ada.Strings.Unbounded;

package AdaSpecLib.String_Tables is new AdaSpecLib.Tables  --  GCOV_IGNORE
   (Unbounded_String, "=");
