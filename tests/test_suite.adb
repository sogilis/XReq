--                         Copyright (C) 2010, Sogilis                       --

with Test_Suite.CLI;
with Test_Suite.Job;

package body Test_Suite is

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite :=
            new AUnit.Test_Suites.Test_Suite;
   begin

      Test_Suite.CLI.Add_Tests (Ret);
      Test_Suite.Job.Add_Tests (Ret);
      return Ret;

   end Suite;

end Test_Suite;
