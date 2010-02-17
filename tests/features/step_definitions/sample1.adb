--                         Copyright (C) 2010, Sogilis                       --

with Text_IO;

use Text_IO;

package body Sample1 is

   --  @given (this step works)
   procedure This_Step_Works is
   begin
      Put_Line ("this step works");
   end This_Step_Works;

   --  @when (this step works too)
   procedure This_Step_Works_Too is
   begin
      Put_Line ("this step works too");
   end This_Step_Works_Too;

   --  @given

   --  @when toto

   --  @then tata

end Sample1;
