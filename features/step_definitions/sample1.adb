--                         Copyright (C) 2010, Sogilis                       --

with Text_IO;

use  Text_IO;

package body Sample1 is

   --  @given (this step works)
   procedure This_Step_Works (Args : in out Arg_Type) is
      pragma Unreferenced (Args);
   begin
      Put_Line ("this step works");
   end This_Step_Works;

end Sample1;
