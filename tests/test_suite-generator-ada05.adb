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
with Ada.Directories;
with Util.IO;
with XReq.Job;
with XReq.Generator;

use Ada.Strings.Unbounded;
use Ada.Directories;
use Util.IO;
use XReq.Job;
use XReq.Generator;

package body Test_Suite.Generator.Ada05 is

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
      return ("XReq.Generator.Ada05");
   end Name;

   procedure Run (T : in out Test_1) is
      Env     : Job_Environment;
      Job     : Job_Type;
      Gen     : Generator_Ptr;
      Output  : Unbounded_String;
      Success : Boolean;
      Result  : Integer;
      Flags   : constant String
              := "-gnat05 -c -aI../step_definitions -aI../../../src/lib" &
                 "  -aI../../../src/common  -aI../../../src/lib/static";
      pragma Unreferenced (Gen);
   begin

      begin
         Delete_File ("tests/features/tests/simplest.adb");
         Delete_File ("tests/features/tests/simplest.ads");
      exception
         when others => null;
      end;

      Init (Env, Job, Std_Logger, "tests/features/simplest.feature");
      Run  (Job, Env, Std_Logger);
      Generate (Job, Env, Std_Logger);

      Append (Output, "gnatmake " & Flags & " feature_simplest.adb" &
              ASCII.LF);
      Spawn ("gnatmake", Flags & " feature_simplest.adb",
             Output, Success, Result, "tests/features/tests");

      T.Assert (Success, "gnatmake did not succeed" & ASCII.LF &
              To_String (Output));
      T.Assert (Result = 0, "gnatmake returned with error" & Result'Img &
              ASCII.LF & To_String (Output));

      Cleanup (Job);
      UnLoad (Env);

   end Run;

end Test_Suite.Generator.Ada05;
