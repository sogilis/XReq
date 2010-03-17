--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib.Format;

use AdaSpecLib.Format;

package body Test_Suite.Lib.Format is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return "AdaSpecLib.Format";
   end Name;

   procedure Run (T : in out Test_1) is
   begin

      T.Assert (Get_Duration (Duration (65)), "1m 5s",
                "Get_Duration (65) /= 1m 5s");

   end Run;

end Test_Suite.Lib.Format;

