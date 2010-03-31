--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib;
with AdaSpec.Steps;

use AdaSpecLib;
use AdaSpec.Steps;

package body Test_Suite.Steps is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return ("AdaSpec.Steps");
   end Name;

   procedure Run (T : in out Test_1) is
      S : Step_Type;
   begin

      T.Assert (Stanza_Given ("A").To_String = "Given A",
              "Wrong stanza Given A");

      T.Assert (Stanza_When  ("B").To_String = "When B",
              "Wrong stanza When B");

      T.Assert (Stanza_Then  ("C").To_String = "Then C",
              "Wrong stanza Then C");

      declare
         Expect : constant String :=
            "@given ^Something ""([^""]*)"" dumb \(""\)$";
         Found  : constant String :=
            Stanza_Given ("Something ""here"" dumb ("")").To_Regexp;
      begin
         T.Assert (Expect = Found, "To_Regexp not OK." & ASCII.LF &
                  "Expected: " & Expect & ASCII.LF &
                  "Found   : " & Found);
      end;

      S.Set_Position (Position ("toto", 5));
      T.Assert (S.Position = Position ("toto", 5), "Wrong position");

   end Run;

end Test_Suite.Steps;

