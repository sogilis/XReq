--                         Copyright (C) 2010, Sogilis                       --

with Test1;

package body Suite is

   function Suite return Access_Test_Suite is
      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test (new Test1.Test);
      return Ret;
   end Suite;

end Suite;
