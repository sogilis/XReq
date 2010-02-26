--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with AdaSpec.Job;

use Ada.Strings.Unbounded;
use AdaSpec.Job;

package body Test_Suite.Job is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_Describe);
      Ret.Add_Test (new Test_Fill_Missing);
      Ret.Add_Test (new Test_Job_Environment);
      Ret.Add_Test (new Test_Run);
   end Add_Tests;

   --  Describe  --------------------------------------------------------------

   function  Name (T : in Test_Describe) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Job.Describe");
   end Name;

   procedure Run (T : in out Test_Describe) is
      CRLF : constant String := ASCII.CR & ASCII.LF;
      Job  : Job_Type;
      Env  : Job_Environment;
      Expected_Result : constant String :=
         "Feature:     F" & CRLF &
         "Steps in:    S" & CRLF &
         "Generate in: O" & CRLF;
   begin
      Make (Env, "S", "O");
      Make (Job, "F");
      T.Assert (Describe (Job, Env) = Expected_Result,
                "Incorrect description");
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
      T.Assert (Env.Step_Dir = "A/B/step_definitions", "Incorrect step dir");
      T.Assert (Env.Out_Dir  = "A/B/tests", "Incorrect out dir");
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

      T.Assert (Env.Step_Dir = "steps", "Invalid step dir");
      T.Assert (Env.Out_Dir = "out", "Invalid out dir");

      Make (Env, "tests/features/step_definitions");

      T.Assert (not Env.Loaded, "Env should NOT be loaded");

      declare
         procedure P;
         procedure P is begin
            Load (Env);
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
            Load (Env);
         end P;
         procedure A is new Assert_Except (Test_Job_Environment, P);
      begin
         A (T, "Invalid_Environment has not been raised in call to Load (2)",
            Invalid_Environment'Identity);
      end;

      T.Assert (not Env.Loaded, "Env should NOT be loaded");

      Make (Env, "tests/features/step_definitions", "tests/features/tests");
      Load (Env);

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
            Run (Job, Env);
         end P;
         procedure A is new Assert_Except (Test_Run, P);
      begin
         A (T, "Invalid_Environment has not been raised in call to Run",
            Invalid_Environment'Identity);
      end;

      Load (Env);

      T.Assert (Step_Dir (Env) = "tests/features/step_definitions",
              "incorrect step dir");

      T.Assert (Out_Dir (Env) = "tests/features/tests",
              "incorrect out dir");

      Run (Job, Env);

      UnLoad (Env);
   end Run;

end Test_Suite.Job;
