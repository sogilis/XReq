--                         Copyright (C) 2010, Sogilis                       --

with XReq.Job;
with Util.IO;

use XReq.Job;
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
   end Add_Tests;

   --  Describe  --------------------------------------------------------------

   function  Name (T : in Test_First_Step_Dir) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Job.First_Step_Dir");
   end Name;

   procedure Run (T : in out Test_First_Step_Dir) is
      Env  : Job_Environment;
   begin

      declare
         procedure P;
         procedure P is begin
            T.Assert (First_Step_Dir (Env) = "", "not OK");
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
      Env  : Job_Environment;
   begin
      Fill_Missing (Env, "A/B/spec.feature");
      T.Assert (First_Step_Dir (Env) = "A/B/step_definitions",
                "Incorrect step dir");
      T.Assert (Out_Dir  (Env) = "A/B/tests", "Incorrect out dir");
   end Run;

   --  Job_Environment  -------------------------------------------------------

   function  Name (T : in Test_Job_Environment) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Job.Job_Environment");
   end Name;

   procedure Run (T : in out Test_Job_Environment) is
      Env  : Job_Environment;
   begin

      T.Assert (not Env.Loaded, "Env should NOT be loaded");

      Make (Env, "steps", "out");

      T.Assert (not Env.Loaded, "Env should NOT be loaded");

      T.Assert (First_Step_Dir (Env) = "steps", "Invalid step dir");
      T.Assert (Out_Dir  (Env) = "out", "Invalid out dir");

      Make (Env, "tests/features/step_definitions");

      T.Assert (not Env.Loaded, "Env should NOT be loaded");

      declare
         procedure P;
         procedure P is begin
            Load (Env, Std_Logger);
         end P;
         procedure A is new Assert_Except (Test_Job_Environment, P);
      begin
         A (T, "Invalid_Environment has not been raised in call to Load (1)",
            Invalid_Environment'Identity);
      end;

      T.Assert (not Env.Loaded, "Env should NOT be loaded");

      Make (Env, Out_Dir => "tests/features/tests");

      T.Assert (not Env.Loaded, "Env should NOT be loaded");

      declare
         procedure P;
         procedure P is begin
            Load (Env, Std_Logger);
         end P;
         procedure A is new Assert_Except (Test_Job_Environment, P);
      begin
         A (T, "Invalid_Environment has not been raised in call to Load (2)",
            Invalid_Environment'Identity);
      end;

      T.Assert (not Env.Loaded, "Env should NOT be loaded");

      Make (Env, "tests/features/step_definitions", "tests/features/tests");
      Load (Env, Std_Logger);

      T.Assert (Env.Loaded, "Env should be loaded");

      UnLoad (Env);

   end Run;

   --  Run  -------------------------------------------------------------------

   function  Name (T : in Test_Run) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Job.Run");
   end Name;

   procedure Run (T : in out Test_Run) is
      Env  : Job_Environment;
      Job  : Job_Type;
   begin
      Make (Job, "tests/features/simplest.feature");
      Fill_Missing (Env, Feature_File (Job));

      declare
         procedure P;
         procedure P is begin
            Run (Job, Env, Std_Logger);
         end P;
         procedure A is new Assert_Except (Test_Run, P);
      begin
         A (T, "Invalid_Environment has not been raised in call to Run",
            Invalid_Environment'Identity);
      end;

      Load (Env, Std_Logger);

      T.Assert (First_Step_Dir (Env) = "tests/features/step_definitions",
              "incorrect step dir");

      T.Assert (Out_Dir (Env) = "tests/features/tests",
              "incorrect out dir");

      Run (Job, Env, Std_Logger);

      Cleanup (Job);
      UnLoad (Env);
   end Run;

end Test_Suite.Job;
