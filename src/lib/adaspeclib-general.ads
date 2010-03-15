--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib.Args;
with AdaSpecLib.String_Tables;

package AdaSpecLib.General is

   subtype Arg_Type is AdaSpecLib.Args.Arg_Type;

   subtype Table_Type   is AdaSpecLib.String_Tables.Table;
   subtype Table_Cursor is AdaSpecLib.String_Tables.Cursor;

   function "=" (Left, Right : in Table_Type) return Boolean
      renames AdaSpecLib.String_Tables."=";

end AdaSpecLib.General;