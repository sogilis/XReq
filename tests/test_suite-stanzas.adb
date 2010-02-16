--                         Copyright (C) 2010, Sogilis                       --

with AUnit.Assertions;
with AdaSpec.Stanzas;

use AUnit.Assertions;
use AdaSpec.Stanzas;

package body Test_Suite.Stanzas is

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
      return AUnit.Format ("AdaSpec.Stanzas");
   end Name;

   procedure Run_Test (T : in out Test_1) is
      pragma Unreferenced (T);
   begin

      Assert (To_String (Stanza_Given ("A")) = "Given A",
              "Wrong stanza Given A");

      Assert (To_String (Stanza_When  ("B")) = "When B",
              "Wrong stanza When B");

      Assert (To_String (Stanza_Then  ("C")) = "Then C",
              "Wrong stanza Then C");

   end Run_Test;

end Test_Suite.Stanzas;

