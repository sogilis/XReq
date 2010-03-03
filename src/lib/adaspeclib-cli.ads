--                         Copyright (C) 2010, Sogilis                       --

with Ada.Command_Line;
with GNAT.OS_Lib;
with AdaSpecLib.Format;

use Ada.Command_Line;
use AdaSpecLib.Format;

package AdaSpecLib.CLI is

   subtype Argument_List_Access is GNAT.OS_Lib.Argument_List_Access;

   procedure Get_Arguments   (Args : out    Argument_List_Access);
   procedure Free            (Arg  : in out Argument_List_Access)
      renames GNAT.OS_Lib.Free;

   procedure Parse_Arguments (Args     : in out Argument_List_Access;
                              Format   : out    Format_Ptr;
                              Continue : out    Boolean;
                              Name     : in     String := Command_Name);

   procedure Parse_Arguments (Format   : out    Format_Ptr;
                              Continue : out    Boolean;
                              Name     : in     String := Command_Name);

   procedure Help (Name : in String := Command_Name);

end AdaSpecLib.CLI;