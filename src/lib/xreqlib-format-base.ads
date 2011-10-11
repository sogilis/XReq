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

package XReqLib.Format.Base is

   type Sub_Element_Type is
      record
         Name        : Unbounded_String;
         Description : Unbounded_String;
         Position    : Unbounded_String;
         Tags        : Tag_Array_Vector.Vector;
      end record;

   type Sub_Step_Type is
      record
         Kind     : Step_Kind;
         Name     : Unbounded_String;
         Position : Unbounded_String;
      end record;

   type Base_Format_Type is abstract new Format_Type with
      record
         Output        : New_Text_IO.File_Type;
         Debug_Mode    : Boolean := False; --  Debug mode, extra information
         In_Tests      : Boolean := False; --  True between Start/Stop_Tests
         In_Feature    : Boolean := False; --  True between Start/Stop_Feature
         In_Outline    : Boolean := False; --  True between Start/Stop_Outline
         In_Scenario   : Boolean := False; --  True between Start/Stop_Scenario
         In_Background : Boolean := False; --  True between Start/S_Background
         In_Step       : Boolean := False; --  True between Start/Stop_Step
         Feature_ID    : Natural := 0;     --  Feature number, start at 1
         Scenario_ID   : Natural := 0;     --  Scenario num. in feature
         ScenOutl_ID   : Natural := 0;     --  Scenario outline num. in feature
         Example_ID    : Natural := 0;     --  Example num. in scenario outline
         Step_ID       : Natural := 0;     --  Step num. in scenario (outline)
         Num_Steps     : Natural := 0;     --  How many steps executed in total
         Exec_Steps    : Natural := 0;     --  Steps executed until now
         Str_Feature   : Unbounded_String := To_Unbounded_String ("Feature:");
         Str_Scenario  : Unbounded_String := To_Unbounded_String ("Scenario:");
         Str_Outline   : Unbounded_String := To_Unbounded_String
                                                         ("Scenario Outline:");
         Feature       : Sub_Element_Type;
         Outline       : Sub_Element_Type;
         Scenario      : Sub_Element_Type;
         Background    : Sub_Element_Type;
         Step          : Sub_Step_Type;
         Previous_Step_Type : Step_All_Kind := Step_Null;
      end record;

   type Base_Format_Ptr  is access all Base_Format_Type'Class;

   ----------------------------------------------------------------------------

   procedure Start_Tests    (Format      : in out Base_Format_Type);
   procedure Begin_Tests    (Format      : in out Base_Format_Type) is null;
   procedure Put_Summary    (Format      : in out Base_Format_Type;
                             Report      : in     Report_Type;
                             D           : in     Duration)         is null;
   procedure End_Tests      (Format      : in out Base_Format_Type) is null;
   procedure Stop_Tests     (Format      : in out Base_Format_Type);

   ----------------------------------------------------------------------------

   procedure Start_Feature  (Format      : in out Base_Format_Type;
                             Feature     : in     String;
                             Description : in     String;
                             Position    : in     String);
   procedure Begin_Feature  (Format      : in out Base_Format_Type) is null;
   procedure Put_Feature    (Format      : in out Base_Format_Type) is null;
   procedure End_Feature    (Format      : in out Base_Format_Type) is null;
   procedure Stop_Feature   (Format      : in out Base_Format_Type);

   ----------------------------------------------------------------------------

   procedure Start_Outline  (Format     : in out Base_Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type);
   procedure Enter_Outline  (Format     : in out Base_Format_Type) is null;
   procedure Begin_Outline  (Format     : in out Base_Format_Type) is null;
   procedure Put_Outline_Report
                            (Format     : in out Base_Format_Type;
                             Table      : in     Table_Type)       is null;
   procedure End_Outline    (Format     : in out Base_Format_Type) is null;
   procedure Stop_Outline   (Format     : in out Base_Format_Type);

   ----------------------------------------------------------------------------

   procedure Start_Scenario (Format     : in out Base_Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type);
   procedure Enter_Scenario (Format     : in out Base_Format_Type) is null;
   procedure Begin_Scenario (Format     : in out Base_Format_Type) is null;
   procedure End_Scenario   (Format     : in out Base_Format_Type) is null;
   procedure Stop_Scenario  (Format     : in out Base_Format_Type);

   ----------------------------------------------------------------------------
   --
   --  Start_Background
   --    Put_Background
   --    <steps...>
   --  Stop_Background

   procedure Start_Background (Format     : in out Base_Format_Type;
                               Background : in     String;
                               Position   : in     String);
   procedure Begin_Background (Format     : in out Base_Format_Type) is null;
   procedure Put_Background   (Format     : in out Base_Format_Type) is null;
   procedure End_Background   (Format     : in out Base_Format_Type) is null;
   procedure Stop_Background  (Format     : in out Base_Format_Type);

   ----------------------------------------------------------------------------
   --
   --  Start_Step
   --  Put_Step
   --  Put_Error ?
   --  Stop_Step

   procedure Start_Step     (Format     : in out Base_Format_Type;
                             Step       : in     Step_Kind;
                             Name       : in     String;
                             Position   : in     String);
   procedure Begin_Step     (Format     : in out Base_Format_Type) is null;
   procedure Put_Step       (Format     : in out Base_Format_Type;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type)      is null;
   procedure Put_Error      (Format     : in out Base_Format_Type;
                             Err        : in Exception_Occurrence) is null;
   procedure End_Step       (Format     : in out Base_Format_Type) is null;
   procedure Stop_Step      (Format     : in out Base_Format_Type);

   ----------------------------------------------------------------------------

   procedure Set_Output     (Format     : in out Base_Format_Type;
                             Output     : in     String);
   procedure Set_Debug      (Format     : in out Base_Format_Type;
                             Debug_Mode : in     Boolean);
   procedure Set_Num_Steps  (Format     : in out Base_Format_Type;
                             Num_Steps  : in     Natural);

   procedure List_Feature   (Format     : in out Base_Format_Type;
                             Name       : in     String);

   procedure List_Scenario  (Format     : in out Base_Format_Type;
                             Name       : in     String;
                             Filename   : in     String;
                             Line       : in     Positive;
                             Num        : in     Positive);

   ----------------------------------------------------------------------------

   procedure S_Feature  (F : in out Base_Format_Type; S : in String);
   procedure S_Scenario (F : in out Base_Format_Type; S : in String);
   procedure S_Outline  (F : in out Base_Format_Type; S : in String);

   function  S_Feature  (F : in Base_Format_Type) return String;
   function  S_Scenario (F : in Base_Format_Type) return String;
   function  S_Outline  (F : in Base_Format_Type) return String;

end XReqLib.Format.Base;
