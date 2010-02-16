--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with AUnit.Assertions;
with AdaSpec.Job;
with AdaSpec.Result;

use Ada.Strings.Unbounded;
use AUnit.Assertions;
use AdaSpec.Job;
use AdaSpec.Result;

package body Test_Suite.Job is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_Describe);
      Ret.Add_Test (new Test_Fill_Missing);
      Ret.Add_Test (new Test_Fill_Missing_2);
      Ret.Add_Test (new Test_Run);
   end Add_Tests;

   --  Describe  --------------------------------------------------------------

   function  Name (T : in Test_Describe) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("AsaSpec.Job.Describe");
   end Name;

   procedure Run_Test (T : in out Test_Describe) is
      pragma Unreferenced (T);
      CRLF : constant String := ASCII.CR & ASCII.LF;
      Job  : constant Job_Type := (
         Feature  => To_Unbounded_String ("F"),
         Step_Dir => To_Unbounded_String ("S"),
         Out_Dir  => To_Unbounded_String ("O"));
      Expected_Result : constant String :=
         "Feature:     F" & CRLF &
         "Steps in:    S" & CRLF &
         "Generate in: O" & CRLF;
   begin
      Assert (Describe (Job) = Expected_Result, "Incorrect description");
   end Run_Test;


   --  Fill_Missing  ----------------------------------------------------------

   function  Name (T : in Test_Fill_Missing) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("AsaSpec.Job.Fill_Missing");
   end Name;

   procedure Run_Test (T : in out Test_Fill_Missing) is
      pragma Unreferenced (T);
      Job  : Job_Type;
   begin
      Job.Feature := To_Unbounded_String ("A/B/spec.feature");
      Fill_Missing (Job);
      Assert (To_String (Job.Step_Dir) = "A/B/steps", "Incorrect step dir");
      Assert (To_String (Job.Out_Dir)  = "A/B/tests", "Incorrect out dir");
   end Run_Test;

   --  Fill_Missing (2) -------------------------------------------------------

   function  Name (T : in Test_Fill_Missing_2) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("AsaSpec.Job.Fill_Missing (invalid job)");
   end Name;

   procedure Run_Test (T : in out Test_Fill_Missing_2) is
      pragma Unreferenced (T);
      Job  : Job_Type;
   begin

      declare
         procedure P;
         procedure P is begin
            Fill_Missing (Job);
         end P;
         procedure Assert_Exception_Raised is new Assert_Exception (P);
      begin
         Assert_Exception_Raised ("Invalid Job");
      end;

   end Run_Test;

   --  Fill_Missing (2) -------------------------------------------------------

   function  Name (T : in Test_Run) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("AsaSpec.Job.Run");
   end Name;

   procedure Run_Test (T : in out Test_Run) is
      pragma Unreferenced (T);
      Job  : constant Job_Type := (
         Feature  => To_Unbounded_String ("tests/features/simplest.feature"),
         Step_Dir => To_Unbounded_String ("tests/features/step_definitions"),
         Out_Dir  => To_Unbounded_String ("tests/features/tests"));
      Res  : Result_Feature_Type;
      pragma Unreferenced (Res);
   begin
      Run (Job, Res);
   end Run_Test;

end Test_Suite.Job;
