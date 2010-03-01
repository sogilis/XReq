--                         Copyright (C) 2010, Sogilis                       --

with AdaSpec.Lang;

use AdaSpec.Lang;

package body Test_Suite.Lang is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return "AdaSpec.Lang";
   end Name;

   procedure Run (T : in out Test_1) is
   begin

      T.Assert (Get_Language ("aDa") = Lang_Ada,
                "Could not detect language aDa");

      declare
         Lang : Language_Type;
         pragma Unreferenced (Lang);
         procedure P;
         procedure P is begin
            Lang := Get_Language ("No_Language");
         end P;
         procedure A is new Assert_Except (Test_1, P);
      begin
         A (T, "Get_Language (No_Language) should raise Invalid_Language",
            Invalid_Language'Identity);
      end;

   end Run;

end Test_Suite.Lang;

