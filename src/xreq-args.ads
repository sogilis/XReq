--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with XReqLib.String_Tables;

use Ada.Strings.Unbounded;

package XReq.Args is

   ---------------------
   --  Argument_Type  --
   ---------------------

   type Argument_Kind is (None, Text, Table);

   type Argument_Type (Typ : Argument_Kind := None) is
      record
         --  GCOV_IGNORE_BEGIN
         case Typ is
            when Text =>
               Text : Unbounded_String;
            when Table =>
               Table : XReqLib.String_Tables.Table;
            when None =>
               null;
         end case;
         --  GCOV_IGNORE_END
      end record;

   function  Text     (A : in     Argument_Type) return String;
   procedure Set_Text (A : in out Argument_Type;
                       T : in     String);

end XReq.Args;