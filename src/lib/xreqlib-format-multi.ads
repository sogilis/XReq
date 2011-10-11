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

with Ada.Containers.Vectors;
with Ada.Finalization;       use Ada.Finalization;

package XReqLib.Format.Multi is

   type Multi_Format_Type is new Limited_Controlled and Format_Type with
     private;
   type Multi_Format_Ptr  is access all Multi_Format_Type'Class;

   procedure Initialize (Object : in out Multi_Format_Type) is null;
   procedure Finalize   (Object : in out Multi_Format_Type);

   ----------------------------------------------------------------------------

   procedure Start_Tests    (Format      : in out Multi_Format_Type);
   procedure Begin_Tests    (Format      : in out Multi_Format_Type);
   procedure Put_Summary    (Format      : in out Multi_Format_Type;
                             Report      : in     Report_Type;
                             D           : in     Duration);
   procedure End_Tests      (Format      : in out Multi_Format_Type);
   procedure Stop_Tests     (Format      : in out Multi_Format_Type);

   ----------------------------------------------------------------------------

   procedure Start_Feature  (Format      : in out Multi_Format_Type;
                             Feature     : in     String;
                             Description : in     String;
                             Position    : in     String);
   procedure Begin_Feature  (Format      : in out Multi_Format_Type);
   procedure Put_Feature    (Format      : in out Multi_Format_Type);
   procedure End_Feature    (Format      : in out Multi_Format_Type);
   procedure Stop_Feature   (Format      : in out Multi_Format_Type);

   ----------------------------------------------------------------------------

   procedure Start_Outline  (Format     : in out Multi_Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type);
   procedure Enter_Outline  (Format     : in out Multi_Format_Type);
   procedure Begin_Outline  (Format     : in out Multi_Format_Type);
   procedure Put_Outline_Report
                            (Format     : in out Multi_Format_Type;
                             Table      : in     Table_Type);
   procedure End_Outline    (Format     : in out Multi_Format_Type);
   procedure Stop_Outline   (Format     : in out Multi_Format_Type);

   ----------------------------------------------------------------------------

   procedure Start_Scenario (Format     : in out Multi_Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type);
   procedure Enter_Scenario (Format     : in out Multi_Format_Type);
   procedure Begin_Scenario (Format     : in out Multi_Format_Type);
   procedure End_Scenario   (Format     : in out Multi_Format_Type);
   procedure Stop_Scenario  (Format     : in out Multi_Format_Type);

   ----------------------------------------------------------------------------

   procedure Start_Background (Format     : in out Multi_Format_Type;
                               Background : in     String;
                               Position   : in     String);
   procedure Begin_Background (Format     : in out Multi_Format_Type);
   procedure Put_Background   (Format     : in out Multi_Format_Type);
   procedure End_Background   (Format     : in out Multi_Format_Type);
   procedure Stop_Background  (Format     : in out Multi_Format_Type);

   ----------------------------------------------------------------------------

   procedure Start_Step     (Format     : in out Multi_Format_Type;
                             Step       : in     Step_Kind;
                             Name       : in     String;
                             Position   : in     String);
   procedure Begin_Step     (Format     : in out Multi_Format_Type);
   procedure Put_Step       (Format     : in out Multi_Format_Type;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type);
   procedure Put_Error      (Format     : in out Multi_Format_Type;
                             Err        : in     Exception_Occurrence);
   procedure End_Step       (Format     : in out Multi_Format_Type);
   procedure Stop_Step      (Format     : in out Multi_Format_Type);

   ----------------------------------------------------------------------------

   procedure Set_Output     (Format     : in out Multi_Format_Type;
                             Output     : in     String);
   procedure Set_Debug      (Format     : in out Multi_Format_Type;
                             Debug_Mode : in     Boolean);
   procedure Set_Num_Steps  (Format     : in out Multi_Format_Type;
                             Num_Steps  : in     Natural);

   procedure List_Feature   (Format     : in out Multi_Format_Type;
                             Name       : in     String);

   procedure List_Scenario  (Format     : in out Multi_Format_Type;
                             Name       : in     String;
                             Filename   : in     String;
                             Line       : in     Positive;
                             Num        : in     Positive);

   ----------------------------------------------------------------------------

   procedure S_Feature  (F : in out Multi_Format_Type; S : in String);
   procedure S_Scenario (F : in out Multi_Format_Type; S : in String);
   procedure S_Outline  (F : in out Multi_Format_Type; S : in String);

   ----------------------------------------------------------------------------

   procedure Add_Sub_Format (Format     : in out Multi_Format_Type;
                             Sub_Format : in     Format_Ptr);

   ----------------------------------------------------------------------------

   function New_Multi_Format return Multi_Format_Ptr;

private

   package Format_Vector_Package is new Ada.Containers.Vectors
     (Natural, Format_Ptr);

   type Multi_Format_Type is new Limited_Controlled and Format_Type with
      record
         Sub_Formats : Format_Vector_Package.Vector;
      end record;

end XReqLib.Format.Multi;
