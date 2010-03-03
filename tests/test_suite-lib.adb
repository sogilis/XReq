--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib;

use AdaSpecLib;

package body Test_Suite.Lib is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_Args);
      Ret.Add_Test (new Test_Assert);
   end Add_Tests;

   --  Test_Args  -------------------------------------------------------------

   function  Name (T : in Test_Args) return String is
      pragma Unreferenced (T);
   begin
      return "AdaSpecLib.Arg_Type";
   end Name;

   procedure Run (T : in out Test_Args) is
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

      T.Assert (Args.Num_Text = 0, "No text until Add_Text is called");

      Args.Add_Text ("1st text");
      T.Assert (Args.Num_Text = 1, "1 text should be present");

      Args.Add_Text ("2nd text");
      T.Assert (Args.Num_Text = 2, "2 text should be present");

      T.Assert (Args.Text (0) = "1st text", "First text mismatch");

      T.Assert (Args.Text (1) = "2nd text", "Second text mismatch");

   end Run;

   --  Test_Assert  -----------------------------------------------------------

   function  Name (T : in Test_Assert) return String is
      pragma Unreferenced (T);
   begin
      return "AdaSpecLib.Assert";
   end Name;

   procedure Run (T : in out Test_Assert) is
   begin
      begin
         AdaSpecLib.Assert (False, "errmsg");
         T.Assert (False, "Assert should raise AdaSpecLib.Error");
      exception
         when E : AdaSpecLib.Error =>
            T.Assert (Exception_Message (E) = "errmsg",
                      "Exception message not OK. Found: '" &
                      Exception_Message (E) & "'");
      end;
   end Run;

end Test_Suite.Lib;

