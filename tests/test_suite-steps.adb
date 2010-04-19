--                         Copyright (C) 2010, Sogilis                       --

with XReqLib;
with XReq.Steps;

use XReqLib;
use XReq.Steps;

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
      return ("XReq.Steps");
   end Name;

   procedure Run (T : in out Test_1) is
      S1, S2 : Step_Type;
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

      S1 := Stanza_Given ("A");
      S2 := Stanza_Given ("A");

      S1.Set_Position (Position ("toto", 5));
      T.Assert (S1.Position = Position ("toto", 5), "Wrong position");

      S2.Set_Position (Position ("toto", 5));
      T.Assert (Equals (S1, S2), "Wrong Equals");

   end Run;

end Test_Suite.Steps;

