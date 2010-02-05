--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with AUnit.Assertions;
with Util.Strings;

use Ada.Strings.Unbounded;
use AUnit.Assertions;
use Util.Strings;

package body Test_Suite.Strings is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_Starts_With);
      Ret.Add_Test (new Test_Find_Token);
   end Add_Tests;

   --  Test_Starts_With  ------------------------------------------------------

   function  Name (T : in Test_Starts_With) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Util.Strings.Test_Starts_With");
   end Name;

   procedure Run_Test (T : in out Test_Starts_With) is
      pragma Unreferenced (T);
      Search : constant String := "abc 123 ABC 456";
   begin

      Assert (Starts_With (Search, "abc"),
              "Should start with 'abc'");

      Assert (Starts_With (Search, "abc 123"),
              "Should start with 'abc 123'");

      Assert (Starts_With (Search, " 123", 4),
              "Should start with ' 123' at position 4");

      Assert (Starts_With (Search, "123 ABC", 5),
              "Should start with '123 ABC' at position 5");

      Assert (not Starts_With (Search, "123"),
              "Should not start with '123'");

      Assert (not Starts_With (Search, "4567", 13),
              "Should not start with '4567' at position 13");

   end Run_Test;

   --  Test_Find_Token  -------------------------------------------------------

   function  Name (T : in Test_Find_Token) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Util.Strings.Find_Token");
   end Name;

   procedure Run_Test (T : in out Test_Find_Token) is
      pragma Unreferenced (T);
      Search : constant String := " @tk1 A @tk2 B @tk3 C ";
      Tokens : constant String_List := (To_Unbounded_String ("@tk3"),
                                        To_Unbounded_String ("@tk2"),
                                        To_Unbounded_String ("@tk1"));
      Index  : Natural;
      Token  : Natural;
   begin

      Find_Token ("", Tokens, Index, Token);

      Assert (Token = 0, "token found found");
      Assert (Index = 0, "index out of bounds");

      Find_Token (Search, Tokens, Index, Token);

      Assert (Token /= 0, "No token found");
      Assert (Index /= 0, "No index found");

      Assert (Token = 3,
              "@tk1 not found, found token #" & Natural'Image (Token) & " " &
              To_String (Tokens (Token)));

      Assert (Index = 6,
              "@tk1 not at the correct position. Found: " &
              Natural'Image (Index) & " instead of 6");

   end Run_Test;

end Test_Suite.Strings;

