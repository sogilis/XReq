--                         Copyright (C) 2010, Sogilis                       --

with XReq.Args;

use XReq.Args;

package body Test_Suite.Args is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return "XReq.Args";
   end Name;

   procedure Run (T : in out Test_1) is
      A : Argument_Type (Text);
   begin

      Set_Text (A, "Toto");
      T.Assert (Text (A) = "Toto", "not OK");

   end Run;

end Test_Suite.Args;

