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

with Util.Smart;

package body Test_Suite.Smart is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return "Util.Smart";
   end Name;

   procedure Run (T : in out Test_1) is

      package Int is new Util.Smart (Integer, 0);

      P1, P2 : Int.Ptr;

   begin

      T.Assert (P1.Ref = 1,     "(1)");
      T.Assert (P1.Val = 0,     "(2)");
      T.Assert (not P1.Is_Null, "(3)");
      T.Assert (P1.Valid,       "(4)");

      P1.IncRef;

      T.Assert (P1.Ref = 2,     "(5)");
      T.Assert (P1.Is_Valid,    "(6)");
      T.Assert (not P1.Is_Null, "(7)");

      P1.DecRef;

      T.Assert (P1.Ref = 1,     "(8)");
      T.Assert (not P1.Is_Null, "(9)");

      P2 := P1;

      T.Assert (P1.Ref = 2,     "(10)");
      T.Assert (P2.Ref = 2,     "(11)");
      T.Assert (P1.Val = 0,     "(12)");
      T.Assert (P2.Val = 0,     "(13)");
      T.Assert (not P2.Is_Null, "(14)");

      P1.Set (8);

      T.Assert (P1.Val = 8,     "(15)");
      T.Assert (P2.Val = 8,     "(16)");

      P2.Set (9);

      T.Assert (P1.Val = 9,     "(17)");
      T.Assert (P2.Val = 9,     "(18)");

      P1.UnRef;

      T.Assert (P1.Is_Null,     "(19)");
      T.Assert (P1.Ref = 0,     "(20)");
      T.Assert (P2.Ref = 1,     "(21)");

   end Run;

end Test_Suite.Smart;

