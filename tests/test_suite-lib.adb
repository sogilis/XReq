--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib;

use AdaSpecLib;

package body Test_Suite.Lib is

   package AU renames AUnit.Assertions;

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return "AdaSpecLib.Arg_Type";
   end Name;

   procedure Run (T : in out Test_1) is
      Stanza : constant String := "abcdefghijklm";
      Match1 : constant String := "abc";
      Match2 : constant String := "ghi";
      Args   : Arg_Type;
      First  : Natural;
      Last   : Natural;
   begin

      Args.Make      (Stanza);
      Args.Add_Match (1, 3);
      Args.Add_Match (7, 9);

      T.Assert (Args.First_Match = 0,
                 "First_Match should always be 0");

      T.Assert (Args.Last_Match = 2,
                 "Last_Match should be 2");

      T.Assert (Args.Stanza = Stanza,
                 "Stanza incorrect");

      T.Assert (Args.Match (0) = Args.Stanza,
                 "Stanza should be the same as Match (0)");

      T.Assert (Args.Match (1) = Match1,
                 "Match (1) should be " & Match1);

      T.Assert (Args.Match (2) = Match2,
                 "Match (2) should be " & Match2);

      Args.Match (0, First, Last);
      T.Assert (First = Stanza'First, "Match (0, First) incorrect");
      T.Assert (Last  = Stanza'Last,  "Match (0, Last) incorrect");

      Args.Match (1, First, Last);
      T.Assert (First = 1, "Match (1, First) incorrect");
      T.Assert (Last  = 3, "Match (1, Last) incorrect");

      Args.Match (2, First, Last);
      T.Assert (First = 7, "Match (2, First) incorrect");
      T.Assert (Last  = 9, "Match (2, Last) incorrect");

   end Run;

end Test_Suite.Lib;

