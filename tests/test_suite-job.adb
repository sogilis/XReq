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

with XReq.Job;
with XReq.Environment;
with XReq.Environment.Handles;
with Util.IO;

use XReq.Job;
use XReq.Environment.Handles;
use Util.IO;

package body Test_Suite.Job is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_First_Step_Dir);
      Ret.Add_Test (new Test_Fill_Missing);
      Ret.Add_Test (new Test_Job_Environment);
      Ret.Add_Test (new Test_Run);
      Ret.Add_Test (new Test_Options);
   end Add_Tests;

   --  Describe  --------------------------------------------------------------

   function  Name (T : in Test_First_Step_Dir) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Job.First_Step_Dir");
   end Name;

   procedure Run (T : in out Test_First_Step_Dir) is
      Env  : Environment_Handle;
   begin

      declare
         procedure P;
         procedure P is begin
            T.Assert (Env.Ref.First_Step_Dir = "", "not OK");
         end P;
         procedure A is new Assert_Except (Test_First_Step_Dir, P);
      begin
         A (T,
            "Constraint_Error has not been raised in call to First_Step_Dir",
            Constraint_Error'Identity);
      end;
   end Run;


   --  Fill_Missing  ----------------------------------------------------------

   function  Name (T : in Test_Fill_Missing) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Job.Fill_Missing");
   end Name;

   procedure Run (T : in out Test_Fill_Missing) is
      Env  : constant Environment_Handle := Create;
   begin
      Env.Ref.Fill_Missing ("A/B/spec.feature");
      T.Assert (Env.Ref.First_Step_Dir = "A/B/step_definitions",
                "Incorrect step dir");
      T.Assert (Env.Ref.Out_Dir = "A/B/tests", "Incorrect out dir");
   end Run;

   --  Job_Environment  -------------------------------------------------------

   function  Name (T : in Test_Job_Environment) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Job.Job_Environment");
   end Name;

   procedure Run (T : in out Test_Job_Environment) is
      Env  : constant Environment_Handle := Create;
   begin

      T.Assert (not Env.Ref.Loaded, "Env should NOT be loaded");

      Env.Ref.Make ("steps", "out");

      T.Assert (not Env.Ref.Loaded, "Env should NOT be loaded");

      T.Assert (Env.Ref.First_Step_Dir = "steps", "Invalid step dir");
      T.Assert (Env.Ref.Out_Dir = "out", "Invalid out dir");

      Env.Ref.Make ("tests/features/step_definitions");

      T.Assert (not Env.Ref.Loaded, "Env should NOT be loaded");

      declare
         procedure P;
         procedure P is begin
            Env.Ref.Load (Std_Logger);
         end P;
         procedure A is new Assert_Except (Test_Job_Environment, P);
      begin
         A (T, "Invalid_Environment has not been raised in call to Load (1)",
            XReq.Environment.Invalid_Environment'Identity);
      end;

      T.Assert (not Env.Ref.Loaded, "Env should NOT be loaded");

      Env.Ref.Make (Out_Dir => "tests/features/tests");

      T.Assert (not Env.Ref.Loaded, "Env should NOT be loaded");

      declare
         procedure P;
         procedure P is begin
            Env.Ref.Load (Std_Logger);
         end P;
         procedure A is new Assert_Except (Test_Job_Environment, P);
      begin
         A (T, "Invalid_Environment has not been raised in call to Load (2)",
            XReq.Environment.Invalid_Environment'Identity);
      end;

      T.Assert (not Env.Ref.Loaded, "Env should NOT be loaded");

      Env.Ref.Make ("tests/features/step_definitions", "tests/features/tests");
      Env.Ref.Load (Std_Logger);

      T.Assert (Env.Ref.Loaded, "Env should be loaded");

   end Run;

   --  Run  -------------------------------------------------------------------

   function  Name (T : in Test_Run) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Job.Run");
   end Name;

   procedure Run (T : in out Test_Run) is
      Env  : Environment_Handle := Create;
      Job  : Job_Type;
   begin
      Job.Make ("tests/features/simplest.feature");
      Env.Ref.Make;
      Env.Ref.Fill_Missing (Feature_File (Job));

      declare
         procedure P;
         procedure P is begin
            Run (Job, Env, Std_Logger);
         end P;
         procedure A is new Assert_Except (Test_Run, P);
      begin
         A (T, "Invalid_Environment has not been raised in call to Run",
            XReq.Environment.Invalid_Environment'Identity);
      end;

      Env.Ref.Load (Std_Logger);

      T.Assert (Env.Ref.First_Step_Dir = "tests/features/step_definitions",
              "incorrect step dir");

      T.Assert (Env.Ref.Out_Dir = "tests/features/tests",
              "incorrect out dir");

      Run (Job, Env, Std_Logger);

   end Run;

   --  Test_Options  ----------------------------------------------------------

   function  Name (T : in Test_Options) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Job.Env_Options");
   end Name;

   procedure Run (T : in out Test_Options) is
      Env  : constant Environment_Handle := Create;
   begin

      Env.Ref.Set_Option ("a", "b");
      T.Assert (Env.Ref.Get_Option ("a") = "b",
                "Wrong option a (1)");

      Env.Ref.Set_Option ("a", "c");
      T.Assert (Env.Ref.Get_Option ("a") = "c",
                "Wrong option a (2)");
      T.Assert (Env.Ref.Get_Option ("a", "x") = "c",
                "Wrong option a (3)");

      T.Assert (Env.Ref.Has_Option ("a"), "Should have option a");
      T.Assert (not Env.Ref.Has_Option ("none"),
                "Should not have option none");

      T.Assert (Env.Ref.Get_Option ("none", "a") = "a",
                "Wrong default value Get_Option");
      declare
         procedure P;
         procedure P is begin
            T.Assert (Env.Ref.Get_Option ("none") = "",
                      "Exception not raised");
         end P;
         procedure A is new Assert_Except (Test_Options, P);
      begin
         A (T, "Invalid_Option has not been raised in call to Get_Option",
            XReq.Environment.Invalid_Option'Identity);
      end;

   end Run;

end Test_Suite.Job;
