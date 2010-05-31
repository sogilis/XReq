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

with Ada.Strings.Unbounded;
with Util.Strings.Pool;

use Ada.Strings.Unbounded;
use Util.Strings.Pool;

package body Test_Suite.Strings.Pool is

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
      return ("Util.Strings.Pool");
   end Name;

   procedure Run (T : in out Test_1) is
      Pool : String_Pool;
      Str  : Unbounded_String;
   begin

      Get_Unique_String (Pool, "Test1", Str);
      T.Assert (To_String (Str) = "Test1",
              "Could not insert 'Test1' in pool");

      Get_Unique_String (Pool, "Test1", Str);
      T.Assert (To_String (Str) /= "Test1",
              "Get_Unique_String do not return a unique string: " &
              To_String (Str));

      Get_Unique_String (Pool, "Test2", Str);
      T.Assert (To_String (Str) = "Test2",
              "Could not insert 'Test2' in pool");

      Get_Unique_String (Pool, "Test2", Str);
      T.Assert (To_String (Str) /= "Test2",
              "Get_Unique_String do not return a unique string: " &
              To_String (Str));

   end Run;

end Test_Suite.Strings.Pool;
