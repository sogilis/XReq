--                         Copyright (C) 2010, Sogilis                       --

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
with AdaSpecLib.Report;
with Ada.Finalization;
with Ada.Text_IO;

use  Ada.Exceptions;
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

   type Status_Type is (Status_Passed, Status_Skipped, Status_Failed);

   type Format_Type is abstract tagged
      record
         Output : New_Text_IO.File_Type;
      end record;
   type Format_Ptr  is access all Format_Type'Class;

   procedure Put_Feature    (Format     : in out Format_Type;
                             Feature    : in     String)
                             is abstract;
   procedure Put_Background (Format     : in out Format_Type;
                             Background : in     String)
                             is abstract;
   procedure Put_Scenario   (Format     : in out Format_Type;
                             Scenario   : in     String)
                             is abstract;
   procedure Put_Step       (Format     : in out Format_Type;
                             Step       : in     Step_Type;
                             Name       : in     String;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type)
                             is abstract;
   procedure Put_Error      (Format     : in out Format_Type;
                             Err        : in     Exception_Occurrence)
                             is abstract;
   procedure Put_Summary    (Format     : in out Format_Type;
                             Report     : in     Report_Type)
                             is abstract;
   procedure Set_Output     (Format     : in out Format_Type;
                             Output     : in     String);

   procedure Start_Tests    (Format     : in out Format_Type) --  GCOV_IGNORE
                             is null;
   procedure Stop_Tests     (Format     : in out Format_Type) --  GCOV_IGNORE
                             is null;

   procedure Start_Feature  (Format     : in out Format_Type) --  GCOV_IGNORE
                             is null;
   procedure Stop_Feature   (Format     : in out Format_Type) --  GCOV_IGNORE
                             is null;

   procedure Start_Background (Format     : in out Format_Type; --  GCOV_IGNORE
                               First      : in Boolean)
                               is null;
   procedure Stop_Background  (Format     : in out Format_Type; --  GCOV_IGNORE
                               First      : in Boolean)
                               is null;

   procedure Enter_Scenario (Format     : in out Format_Type) --  GCOV_IGNORE
                             is null;
   procedure Start_Scenario (Format     : in out Format_Type) --  GCOV_IGNORE
                             is null;
   procedure Stop_Scenario  (Format     : in out Format_Type) --  GCOV_IGNORE
                             is null;

   procedure Start_Step     (Format     : in out Format_Type) --  GCOV_IGNORE
                             is null;
   procedure Stop_Step      (Format     : in out Format_Type) --  GCOV_IGNORE
                             is null;

   procedure Free is new Ada.Unchecked_Deallocation
      (Format_Type'Class, Format_Ptr);

   function Get_Formatter (Name : in String) return Format_Ptr;

private

   --  type Format_Type should be private but Ada is absolutely awful

end AdaSpecLib.Format;