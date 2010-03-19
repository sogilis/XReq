--                         Copyright (C) 2010, Sogilis                       --

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
with Ada.Finalization;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with AdaSpecLib.Args;
with AdaSpecLib.Report;

use  Ada.Exceptions;
use  AdaSpecLib.Args;
use  AdaSpecLib.Report;

package AdaSpecLib.Format is

   package New_Text_IO is

      type Local_File_Ptr is access Ada.Text_IO.File_Type;

      type File_Type is new Ada.Finalization.Controlled with
         record
            Output_Ownership : Boolean := False;
            Output_Ptr       : Local_File_Ptr
                             := new Ada.Text_IO.File_Type;
            Output_Access    : Ada.Text_IO.File_Access
                             := Ada.Text_IO.Current_Output;
         end record;

      subtype File_Mode is Ada.Text_IO.File_Mode;

      procedure Put      (File : in out File_Type;
                          Item : in     String);
      procedure Put      (File : in out File_Type;
                          Item : in     Character);
      procedure Put_Line (File : in out File_Type;
                          Item : in     String);
      procedure New_Line (File : in out File_Type);
      procedure Create   (File : in out File_Type;
                          Mode : in     File_Mode;
                          Name : in     String := "");
      procedure Close    (File : in out File_Type);
      overriding
      procedure Finalize (File : in out File_Type);

   end New_Text_IO;

   ----------------------------------------------------------------------------

   type Tag_Array_Type is array (Positive range <>)  --  GCOV_IGNORE
      of Ada.Strings.Unbounded.Unbounded_String;

   type Status_Type is (Status_Passed, Status_Skipped, Status_Failed,
                        Status_Outline);

   type Format_Type is abstract tagged
      record
         Output        : New_Text_IO.File_Type;
         Debug_Mode    : Boolean := False; --  Debug mode, extra information
         First_Feature : Boolean := True;  --  True for the first feature only
         In_Tests      : Boolean := False; --  True between Start/Stop_Tests
         In_Feature    : Boolean := False; --  True between Start/Stop_Feature
         In_Outline    : Boolean := False; --  True between Start/Stop_Outline
         In_Background : Boolean := False; --  True between Start/S_Background
         In_Scenario   : Boolean := False; --  True between Start/Stop_Scenario
         In_Step       : Boolean := False; --  True between Start/Stop_Step
         Feature_ID    : Natural := 0;     --  Feature number, start at 1
         Background_ID : Natural := 0;     --  Background number in feature
         Scenario_ID   : Natural := 0;     --  Scenario (outline) n. in feature
         Step_ID       : Natural := 0;     --  Step num. in scenario (outline)
      end record;
   type Format_Ptr  is access all Format_Type'Class;

   ----------------------------------------------------------------------------

   procedure Start_Tests    (Format      : in out Format_Type);
   procedure Put_Summary    (Format      : in out Format_Type;
                             Report      : in     Report_Type;
                             D           : in     Duration) is abstract;
   procedure Stop_Tests     (Format      : in out Format_Type);

   ----------------------------------------------------------------------------

   procedure Start_Feature  (Format      : in out Format_Type);
   procedure Put_Feature    (Format      : in out Format_Type;
                             Feature     : in     String;
                             Description : in     String;
                             Position    : in     String) is abstract;
   procedure Stop_Feature   (Format      : in out Format_Type);

   ----------------------------------------------------------------------------


   procedure Start_Background (Format     : in out Format_Type;
                               First      : in Boolean);
   procedure Put_Background   (Format     : in out Format_Type;
                               Background : in     String;
                               Position   : in     String;
                               Tags       : in     Tag_Array_Type) is abstract;
   procedure Stop_Background  (Format     : in out Format_Type;
                               First      : in Boolean);

   ----------------------------------------------------------------------------

   procedure Enter_Outline  (Format     : in out Format_Type);
   procedure Start_Outline  (Format     : in out Format_Type);
   procedure Put_Outline    (Format     : in out Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type) is abstract;
   procedure Put_Outline_Report
                            (Format     : in out Format_Type;
                             Table      : in     Table_Type) is abstract;
   procedure Stop_Outline   (Format     : in out Format_Type);

   ----------------------------------------------------------------------------

   procedure Enter_Scenario (Format     : in out Format_Type);
   procedure Start_Scenario (Format     : in out Format_Type);
   procedure Put_Scenario   (Format     : in out Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type) is abstract;
   procedure Stop_Scenario  (Format     : in out Format_Type);

   ----------------------------------------------------------------------------

   procedure Start_Step     (Format     : in out Format_Type);
   procedure Put_Step       (Format     : in out Format_Type;
                             Step       : in     Step_Type;
                             Name       : in     String;
                             Position   : in     String;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type)
                             is abstract;
   procedure Put_Error      (Format     : in out Format_Type;
                             Err        : in     Exception_Occurrence)
                             is abstract;
   procedure Stop_Step      (Format     : in out Format_Type);

   ----------------------------------------------------------------------------

   procedure Set_Output     (Format     : in out Format_Type;
                             Output     : in     String);
   procedure Set_Debug      (Format     : in out Format_Type;
                             Debug_Mode : in     Boolean);

   procedure List_Feature   (Format     : in out Format_Type;
                             Name       : in     String);

   procedure List_Scenario  (Format     : in out Format_Type;
                             Name       : in     String;
                             Filename   : in     String;
                             Num        : in     Positive);

   ----------------------------------------------------------------------------

   procedure Free is new Ada.Unchecked_Deallocation
      (Format_Type'Class, Format_Ptr);

   function Get_Formatter (Name : in String) return Format_Ptr; --  GCOV_IGNORE

   ---------------------
   --  Tag_Expr_Type  --
   ---------------------

   package String_Vectors is new Ada.Containers.Vectors (
      Natural,
      Ada.Strings.Unbounded.Unbounded_String,
      Ada.Strings.Unbounded."=");

   type Conditional_Type is tagged
      record
         Expr      : Ada.Strings.Unbounded.Unbounded_String;
         Scenarios : String_Vectors.Vector;
      end record;

   function  Create (Expr : in String) return Conditional_Type;

   function  Eval   (Tag_Expr : in Conditional_Type;
                     Tags     : in Tag_Array_Type) return Boolean;

   function  Eval   (Cond     : in Conditional_Type;
                     File     : in String;
                     Num      : in Integer) return Boolean;

   Null_Condition : constant Conditional_Type := (others => <>);

   function Get_Duration (D : in Duration) return String;
   --  Need this to be public in order to be tested

private

   --  type Format_Type should be private but Ada is absolutely awful

end AdaSpecLib.Format;
