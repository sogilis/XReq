--                         Copyright (C) 2010, Sogilis                       --

with Text_IO;

use Text_IO;

package body Sample1 is

   --  @given (this step works)
   procedure This_Step_Works is
   begin
      Put_Line ("this step works");
   end This_Step_Works;

end Sample1;
