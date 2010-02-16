--                         Copyright (C) 2010, Sogilis                       --

with AUnit.Assertions;
with AdaSpec.Result;

use AUnit.Assertions;
use AdaSpec.Result;

package body Test_Suite.Result is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("AdaSpec.Result.Result_Step_Type");
   end Name;

   procedure Run_Test (T : in out Test_1) is
      pragma Unreferenced (T);
      Step : Result_Step_Type;
   begin

      Make (Step, "Proc");

      Assert (Procedure_Name (Step) = "Proc",
              "Wrong procedure name");

   end Run_Test;

end Test_Suite.Result;

