--                         Copyright (C) 2010, Sogilis                       --

with XReqLib.Args;
with XReqLib.String_Tables;

package XReqLib.General is

   subtype Arg_Type is XReqLib.Args.Arg_Type;

   subtype Table_Type   is XReqLib.String_Tables.Table;
   subtype Table_Cursor is XReqLib.String_Tables.Cursor;

   function "=" (Left, Right : in Table_Type) return Boolean
      renames XReqLib.String_Tables."=";

end XReqLib.General;