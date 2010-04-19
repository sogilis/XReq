--                         Copyright (C) 2010, Sogilis                       --

with XReqLib.Args;

use XReqLib.Args;

package body Test_Suite.Lib.Args is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return "XReqLib.Args";
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

      T.Assert (Args.First_Text = 0, "First_Text should always be 0");

      T.Assert (Args.Num_Text = 0, "No text until Add_Text is called");
      T.Assert (Args.First = 0,    "First element should be 0");
      T.Assert (Args.Last = -1,    "Last element should be -1");

      Args.Add_Text ("1st text");
      T.Assert (Args.Num_Text = 1, "1 text should be present");

      Args.Add_Text ("2nd text");
      T.Assert (Args.Num_Text = 2, "2 text should be present");
      T.Assert (Args.First = 0,    "First element should be 0");
      T.Assert (Args.Last = 1,     "Last element should be 1");

      T.Assert (Args.Text (0) = "1st text", "First text mismatch");
      T.Assert (Args.Text (1) = "2nd text", "Second text mismatch");

      T.Assert (Args.First_Para = 0,    "First para should be 0");
      T.Assert (Args.Last_Para = -1,    "Last para should be -1");
      Args.Add_Para ("para");
      T.Assert (Args.First_Para = 0,    "First para should be 0");
      T.Assert (Args.Last_Para = 0,     "Last para should be 0");
      T.Assert (Args.Para (0) = "para", "para text incorrect");
      T.Assert (Args.Last = 2,          "Last element should be 2");

      Args.Add_Sep (33);
      T.Assert (Args.Last = 3,          "Last element should be 3");

      T.Assert (Args.Elem_Idx (0) = 0,  "Element 0 should have index 0");
      T.Assert (Args.Elem_Idx (1) = 1,  "Element 1 should have index 1");
      T.Assert (Args.Elem_Idx (2) = 0,  "Element 2 should have index 0");
      T.Assert (Args.Elem_Idx (3) = 33, "Element 3 should have index 33");
      T.Assert (Args.Elem_Type (0) = Arg_Text, "Elem 0 should have type text");
      T.Assert (Args.Elem_Type (1) = Arg_Text, "Elem 1 should have type text");
      T.Assert (Args.Elem_Type (2) = Arg_Paragraph,
                "Elem 2 should have type paragraph");
      T.Assert (Args.Elem_Type (3) = Arg_Separator,
                "Elem 3 should have type separator");

      T.Assert (Args.First_Table = 0, "First_Table should always be 0");

   end Run;

end Test_Suite.Lib.Args;

