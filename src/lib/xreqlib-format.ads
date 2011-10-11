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

with Ada.Exceptions;
with Ada.Finalization;
with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with XReqLib.Args;
with XReqLib.Report;

use  Ada.Exceptions;
use  Ada.Strings.Unbounded;
use  XReqLib.Args;
use  XReqLib.Report;

package XReqLib.Format is

   package New_Text_IO is

      type Local_File_Ptr is access Ada.Text_IO.File_Type;

      type File_Type is new Ada.Finalization.Controlled with
         record
            Output_Ownership : Boolean := False;
            Output_Ptr       : Local_File_Ptr
              := new Ada.Text_IO.File_Type;
            Output_Access    : Ada.Text_IO.File_Access
              := Ada.Text_IO.Current_Output;
            Buffering        : Boolean := False;
            Buffer           : Unbounded_String;
         end record;

      subtype File_Mode is Ada.Text_IO.File_Mode;


      procedure Buffer_Start   (File : in out File_Type);
      procedure Buffer_Commit  (File : in out File_Type);
      procedure Buffer_Discard (File : in out File_Type);
      function  Buffered       (File :        File_Type) return Boolean;
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

   package Tag_Array_Vector is new Ada.Containers.Vectors
     (Positive, Ada.Strings.Unbounded.Unbounded_String);

   function Convert (Tags : Tag_Array_Type) return Tag_Array_Vector.Vector;
   function Convert (Tags : Tag_Array_Vector.Vector) return Tag_Array_Type;

   type Status_Type is (Status_Passed, Status_Skipped, Status_Failed,
                        Status_Outline);

   type Format_Type is limited interface;
   type Format_Ptr  is access all Format_Type'Class;



   ----------------------------------------------------------------------------
   --
   --  Start_Tests
   --  Begin_Tests
   --    <features...>
   --  Put_Summary
   --  End_Tests
   --  Stop_Tests

   procedure Start_Tests    (Format      : in out Format_Type) is abstract;
   procedure Begin_Tests    (Format      : in out Format_Type) is abstract;
   procedure Put_Summary    (Format      : in out Format_Type;
                             Report      : in     Report_Type;
                             D           : in     Duration) is abstract;
   procedure End_Tests      (Format      : in out Format_Type) is abstract;
   procedure Stop_Tests     (Format      : in out Format_Type) is abstract;

   ----------------------------------------------------------------------------
   --
   --  Start_Feature
   --  Begin_Feature
   --  Put_Feature
   --    <scenarios and scenario outlines...>
   --  End_Feature
   --  Stop_Feature

   procedure Start_Feature  (Format      : in out Format_Type;
                             Feature     : in     String;
                             Description : in     String;
                             Position    : in     String) is abstract;
   procedure Begin_Feature  (Format      : in out Format_Type) is abstract;
   procedure Put_Feature    (Format      : in out Format_Type) is abstract;
   procedure End_Feature    (Format      : in out Format_Type) is abstract;
   procedure Stop_Feature   (Format      : in out Format_Type) is abstract;

   ----------------------------------------------------------------------------
   --
   --  Start_Outline
   --  Enter_Outline
   --    <steps with Success=Status_Outline...>
   --  Begin_Outline
   --    <scenarios...>
   --  Put_Outline_Report
   --  End_Outline
   --  Stop_Outline

   procedure Start_Outline  (Format     : in out Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type) is abstract;
   procedure Enter_Outline  (Format     : in out Format_Type) is abstract;
   procedure Begin_Outline  (Format     : in out Format_Type) is abstract;
   procedure Put_Outline_Report
                            (Format     : in out Format_Type;
                             Table      : in     Table_Type) is abstract;
   procedure End_Outline    (Format     : in out Format_Type) is abstract;
   procedure Stop_Outline   (Format     : in out Format_Type) is abstract;

   ----------------------------------------------------------------------------
   --
   --  Start_Scenario
   --  Enter_Scenario
   --    <background>
   --  Begin_Scenario
   --    <steps...>
   --  End_Scenario
   --  Stop_Scenario

   procedure Start_Scenario (Format     : in out Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type) is abstract;
   procedure Enter_Scenario (Format     : in out Format_Type) is abstract;
   procedure Begin_Scenario (Format     : in out Format_Type) is abstract;
   procedure End_Scenario   (Format     : in out Format_Type) is abstract;
   procedure Stop_Scenario  (Format     : in out Format_Type) is abstract;

   ----------------------------------------------------------------------------
   --
   --  Start_Background
   --    Begin_Background
   --    Put_Background
   --    <steps...>
   --    End_Background
   --  Stop_Background

   procedure Start_Background (Format     : in out Format_Type;
                               Background : in     String;
                               Position   : in     String) is abstract;
   procedure Begin_Background (Format     : in out Format_Type) is abstract;
   procedure Put_Background   (Format     : in out Format_Type) is abstract;
   procedure End_Background   (Format     : in out Format_Type) is abstract;
   procedure Stop_Background  (Format     : in out Format_Type) is abstract;

   ----------------------------------------------------------------------------
   --
   --  Start_Step
   --  Begin_Step
   --  Put_Step
   --  Put_Error ?
   --  End_Step
   --  Stop_Step

   procedure Start_Step     (Format     : in out Format_Type;
                             Step       : in     Step_Kind;
                             Name       : in     String;
                             Position   : in     String) is abstract;
   procedure Begin_Step     (Format     : in out Format_Type) is abstract;
   procedure Put_Step       (Format     : in out Format_Type;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type)
                             is abstract;
   procedure Put_Error      (Format     : in out Format_Type;
                             Err        : in     Exception_Occurrence)
                             is abstract;
   procedure End_Step       (Format     : in out Format_Type) is abstract;
   procedure Stop_Step      (Format     : in out Format_Type) is abstract;

   ----------------------------------------------------------------------------

   procedure Set_Output     (Format     : in out Format_Type;
                             Output     : in     String) is abstract;
   procedure Set_Debug      (Format     : in out Format_Type;
                             Debug_Mode : in     Boolean) is abstract;
   procedure Set_Num_Steps  (Format     : in out Format_Type;
                             Num_Steps  : in     Natural) is abstract;

   procedure List_Feature   (Format     : in out Format_Type;
                             Name       : in     String) is abstract;

   procedure List_Scenario  (Format     : in out Format_Type;
                             Name       : in     String;
                             Filename   : in     String;
                             Line       : in     Positive;
                             Num        : in     Positive) is abstract;

   ----------------------------------------------------------------------------

   procedure S_Feature  (F : in out Format_Type; S : in String) is abstract;
   procedure S_Scenario (F : in out Format_Type; S : in String) is abstract;
   procedure S_Outline  (F : in out Format_Type; S : in String) is abstract;

   ----------------------------------------------------------------------------

   procedure Free (Self : in out Format_Ptr);

   function Get_Formatter (Name : in String) return Format_Ptr; --  GCOV_IGNORE

   ---------------------
   --  Tag_Expr_Type  --
   ---------------------

   type Conditional_Type is tagged
      record
         Expr      : Ada.Strings.Unbounded.Unbounded_String;
         Scenarios : String_Vector;
      end record;

   function  Create (Expr : in String) return Conditional_Type;

   function  Eval   (Tag_Expr : in Conditional_Type;
                     Tags     : in Tag_Array_Type) return Boolean;

   function  Eval   (Cond     : in Conditional_Type;
                     File     : in String;
                     Line     : in Integer;
                     Num      : in Integer) return Boolean;

   Null_Condition : constant Conditional_Type := (others => <>);

   function Get_Duration (D : in Duration) return String;
   --  Need this to be public in order to be tested

end XReqLib.Format;
