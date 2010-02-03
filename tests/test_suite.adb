--                         Copyright (C) 2010, Sogilis                       --

with Test_Suite.Test1;

package body Test_Suite is

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite :=
            new AUnit.Test_Suites.Test_Suite;
   begin
      Ret.Add_Test (new Test_Suite.Test1.Test);
      return Ret;
   end Suite;

end Test_Suite;
