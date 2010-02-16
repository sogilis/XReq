--                         Copyright (C) 2010, Sogilis                       --

with Test_Suite.IO;
with Test_Suite.Strings;

with Test_Suite.Main;
with Test_Suite.CLI;
with Test_Suite.Job;
with Test_Suite.Result;
with Test_Suite.Stanzas;
with Test_Suite.Steps;
with Test_Suite.Steps.Ada;
with Test_Suite.Features;

package body Test_Suite is

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite :=
            new AUnit.Test_Suites.Test_Suite;
   begin

      Test_Suite.IO       .Add_Tests (Ret);
      Test_Suite.Strings  .Add_Tests (Ret);

      Test_Suite.Main     .Add_Tests (Ret);
      Test_Suite.CLI      .Add_Tests (Ret);
      Test_Suite.Job      .Add_Tests (Ret);
      Test_Suite.Stanzas  .Add_Tests (Ret);
      Test_Suite.Result   .Add_Tests (Ret);
      Test_Suite.Steps    .Add_Tests (Ret);
      Test_Suite.Steps.Ada.Add_Tests (Ret);
      Test_Suite.Features .Add_Tests (Ret);

      return Ret;

   end Suite;

end Test_Suite;
