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
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Util.Strings.Find_Token");
   end Name;

   procedure Run_Test (T : in out Test_1) is
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

