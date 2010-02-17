--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with AUnit.Assertions;
with AdaSpec.Job;

use Ada.Strings.Unbounded;
use AUnit.Assertions;
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
      pragma Unreferenced (T);
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
      Assert (Describe (Job, Env) = Expected_Result, "Incorrect description");
   end Run;


   --  Fill_Missing  ----------------------------------------------------------

   function  Name (T : in Test_Fill_Missing) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Job.Fill_Missing");
   end Name;

   procedure Run (T : in out Test_Fill_Missing) is
      pragma Unreferenced (T);
      Env  : Job_Environment;
   begin
      Fill_Missing (Env, "A/B/spec.feature");
      Assert (Env.Step_Dir = "A/B/step_definitions", "Incorrect step dir");
      Assert (Env.Out_Dir  = "A/B/tests", "Incorrect out dir");
   end Run;

   --  Job_Environment  -------------------------------------------------------

   function  Name (T : in Test_Job_Environment) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Job.Job_Environment");
   end Name;

   procedure Run (T : in out Test_Job_Environment) is
      pragma Unreferenced (T);
      Env  : Job_Environment;
   begin

      Assert (not Env.Loaded, "Env should NOT be loaded");

      Make (Env, "steps", "out");

      Assert (not Env.Loaded, "Env should NOT be loaded");

      Assert (Env.Step_Dir = "steps", "Invalid step dir");
      Assert (Env.Out_Dir = "out", "Invalid out dir");

      Make (Env, "tests/features/step_definitions");

      Assert (not Env.Loaded, "Env should NOT be loaded");

      declare
         procedure P;
         procedure P is begin
            Load (Env);
         end P;
         procedure Assert_Exception_Raised is new Assert_Exception (P);
      begin
         Assert_Exception_Raised ("Invalid_Environment has not been raised " &
                                  "in call to Load (1)");
      end;

      Assert (not Env.Loaded, "Env should NOT be loaded");

      Make (Env, Out_Dir => "tests/features/tests");

      Assert (not Env.Loaded, "Env should NOT be loaded");

      declare
         procedure P;
         procedure P is begin
            Load (Env);
         end P;
         procedure Assert_Exception_Raised is new Assert_Exception (P);
      begin
         Assert_Exception_Raised ("Invalid_Environment has not been raised " &
                                  "in call to Load (2)");
      end;

      Assert (not Env.Loaded, "Env should NOT be loaded");

      Make (Env, "tests/features/step_definitions", "tests/features/tests");
      Load (Env);

      Assert (Env.Loaded, "Env should be loaded");

   end Run;

   --  Run  -------------------------------------------------------------------

   function  Name (T : in Test_Run) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Job.Run");
   end Name;

   procedure Run (T : in out Test_Run) is
      pragma Unreferenced (T);
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
         procedure Assert_Exception_Raised is new Assert_Exception (P);
      begin
         Assert_Exception_Raised ("Invalid_Environment has not been raised " &
                                  "in call to Run");
      end;

      Load (Env);

      Assert (Step_Dir (Env) = "tests/features/step_definitions",
              "incorrect step dir");

      Assert (Out_Dir (Env) = "tests/features/tests",
              "incorrect out dir");

      Run (Job, Env);
   end Run;

end Test_Suite.Job;
