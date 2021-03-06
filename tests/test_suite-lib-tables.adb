-------------------------------------------------------------------------------
--  XReq  --  Behaviour Driven Developpement tool for compiled languages     --
--  Copyright (c) 2010, SOGILIS <http://sogilis.com>                         --
--                                                                           --
--  This program is free software: you can redistribute it and/or modify     --
--  it under the terms of the GNU Affero General Public License as           --
--  published by the Free Software Foundation, either version 3 of the       --
--  License, or (at your option) any later version.                          --
--                                                                           --
--  This program is distributed in the hope that it will be useful,          --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of           --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            --
--  GNU Affero General Public License for more details.                      --
--                                                                           --
--  You should have received a copy of the GNU Affero General Public License --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.    --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Strings;
with Ada.Strings.Fixed;
with XReqLib.Tables;
with XReqLib.String_Tables;

package body Test_Suite.Lib.Tables is

   package Tables is new XReqLib.Tables (Integer);

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_1);
      Ret.Add_Test (new Test_2);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return "XReqLib.Tables";
   end Name;

   procedure Run (T : in out Test_1) is
      Ta : Tables.Table;
      E  : Integer;
      Ok : Boolean;

      procedure Check_Empty (Id : in String; Reset : in Boolean);
      procedure Check_Empty (Id : in String; Reset : in Boolean) is
      begin

         if Reset then
            Ta.First_X (0);
            Ta.First_Y (0);
         end if;

         T.Assert (Ta.Length.X = 0,  "(" & Id & ") Length.X /= 0 =" &
                                                   Ta.Length.X'Img);
         T.Assert (Ta.Length.Y = 0,  "(" & Id & ") Length.Y /= 0 =" &
                                                   Ta.Length.Y'Img);
         T.Assert (Ta.Is_Empty,      "(" & Id & ") not Is_Empty");
         T.Assert (Ta.Count    = 0,  "(" & Id & ") Count /= 0");
         T.Assert (Ta.First_X  = 0,  "(" & Id & ") First_X /= 0");
         T.Assert (Ta.First_Y  = 0,  "(" & Id & ") First_X /= 0");
         T.Assert (Ta.Last_X   = -1, "(" & Id & ") Last_X /= -1");
         T.Assert (Ta.Last_Y   = -1, "(" & Id & ") Last_Y /= -1");
         T.Assert (Ta.Length_X = 0,  "(" & Id & ") Length_X /= 0");
         T.Assert (Ta.Length_Y = 0,  "(" & Id & ") Length_Y /= 0");

         declare
            I : Integer;
            pragma Unreferenced (I);
         begin
            I := Ta.Item (0, 0);
            T.Assert (False, "(" & Id & ") Item (0, 0) should raise " &
                             "Constraint_Error");
         exception
            when Constraint_Error =>
               null;
         end;

         Ta.Item (0, 0, E, Ok);

         T.Assert (not Ok, "(" & Id & ") Item (0, 0, E, Ok) is Ok");

      end Check_Empty;

   begin

      Check_Empty ("A", False);

      Ta.Put (0, 0, 5);
      Ta.Put (0, 1, 6);
      Ta.Put (1, 0, 7);
      Ta.Put (1, 2, 8);

      T.Assert (Ta.Length.X = 2,  "(B) Length.X /= 2 =" & Ta.Length.X'Img);
      T.Assert (Ta.Length.Y = 3,  "(B) Length.Y /= 3 =" & Ta.Length.Y'Img);
      T.Assert (not Ta.Is_Empty,  "(B) Is_Empty");
      T.Assert (Ta.Count    = 4,  "(B) Count /= 4");
      T.Assert (Ta.First_X  = 0,  "(B) First_X /= 0");
      T.Assert (Ta.First_Y  = 0,  "(B) First_X /= 0");
      T.Assert (Ta.Last_X   = 1,  "(B) Last_X /= 1");
      T.Assert (Ta.Last_Y   = 2,  "(B) Last_Y /= 2");
      T.Assert (Ta.Length_X = 2,  "(B) Length_X /= 2");
      T.Assert (Ta.Length_Y = 3,  "(B) Length_Y /= 3");

      T.Assert (Ta.Item (0, 0) = 5, "(B) Item (0, 0) /= 5");
      T.Assert (Ta.Item (0, 1) = 6, "(B) Item (0, 1) /= 6");
      T.Assert (Ta.Item (1, 0) = 7, "(B) Item (1, 0) /= 7");
      T.Assert (Ta.Item (1, 2) = 8, "(B) Item (1, 2) /= 8");

      Ta.Item (1, 1, E, Ok); T.Assert (not Ok, "(B) Item (1, 1, E, Ok) is Ok");
      Ta.Item (0, 2, E, Ok); T.Assert (not Ok, "(B) Item (0, 2, E, Ok) is Ok");

      Ta.Clear;

      Check_Empty ("C", False);

      T.Assert (Ta.Last_X = -1,  "(D) Last_X /= -1");
      T.Assert (Ta.Last_Y = -1,  "(D) Last_Y /= -1");

      Ta.First_X (1); Ta.First_Y (1);

      T.Assert (Ta.First_X = 1,  "(D) First_X /= 1");
      T.Assert (Ta.First_Y = 1,  "(D) First_Y /= 1");
      T.Assert (Ta.Last_X = 0,   "(D) Last_X /= 0");
      T.Assert (Ta.Last_Y = 0,   "(D) Last_Y /= 0");
      T.Assert (Ta.Length_X = 0, "(D) Length_X /= 0");
      T.Assert (Ta.Length_Y = 0, "(D) Length_Y /= 0");

      Ta.Add_X;
      Ta.Add_Y;

      T.Assert (Ta.First_X = 1,  "(E) First_X /= 1");
      T.Assert (Ta.First_Y = 1,  "(E) First_Y /= 1");
      T.Assert (Ta.Last_X = 1,   "(E) Last_X /= 1");
      T.Assert (Ta.Last_Y = 1,   "(E) Last_Y /= 1");
      T.Assert (Ta.Length_X = 1, "(E) Length_X /= 1");
      T.Assert (Ta.Length_Y = 1, "(E) Length_Y /= 1");

      Ta.Item (1, 1, E, Ok);

      T.Assert (not Ok, "(E) Item (1, 1, E, Ok) is Ok");

      Ta.Clear;
      T.Assert (Ta.First_X = 1,  "(F) First_X /= 1");
      T.Assert (Ta.First_Y = 1,  "(F) First_Y /= 1");

      Check_Empty ("G", True);

   end Run;


   --  Test_2  ----------------------------------------------------------------

   function  Name (T : in Test_2) return String is
      pragma Unreferenced (T);
   begin
      return "XReqLib.String_Tables";
   end Name;

   procedure Run (T : in out Test_2) is
      use XReqLib.String_Tables;
      use Ada.Strings.Fixed;
      use Ada.Strings;

      Ta : XReqLib.String_Tables.Table;
      I  : XReqLib.String_Tables.Cursor;

      procedure Check_Key (K : Key_Type; Elem : in String);
      procedure Check_Key (K : Key_Type; Elem : in String) is
         Should : constant String := "(" & Trim (K.X'Img, Left) & "," &
                  Trim (K.Y'Img, Left) & ")";
      begin
         T.Assert (Should = Elem, Elem & " should be " & Should);
      end Check_Key;
   begin

      Ta.Put (1, 1, "(1,1)");
      Ta.Put (1, 2, "(1,2)");
      Ta.Put (2, 1, "(2,1)");

      T.Assert (Ta.Item (1, 1)           = "(1,1)",  "Item(1, 1) not OK");
      T.Assert (Ta.Item (1, 2)           = "(1,2)",  "Item(1, 2) not OK");
      T.Assert (Ta.Item (2, 1, "<none>") = "(2,1)",  "Item(2, 1) not OK");
      T.Assert (Ta.Item (2, 2, "<none>") = "<none>", "Item(2, 2) not OK");

      I := First (Ta);
      T.Assert (Has_Element (I), "cursor should still have an element");
      Check_Key (Key (I), Element (I));

      Next (I);
      T.Assert (Has_Element (I), "cursor should still have an element");
      Check_Key (Key (I), Element (I));

      Next (I);
      T.Assert (Has_Element (I), "cursor should still have an element");
      Check_Key (Key (I), Element (I));

      Next (I);
      T.Assert (not Has_Element (I), "cursor should not have an element");

   end Run;

end Test_Suite.Lib.Tables;
