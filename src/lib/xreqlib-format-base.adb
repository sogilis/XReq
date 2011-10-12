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

with Ada.Strings.Fixed;

package body XReqLib.Format.Base is

   ------------------------
   --  Start/Stop_Tests  --
   ------------------------

   procedure Start_Tests    (Format      : in out Base_Format_Type) is
   begin
      Format.In_Tests := True;
   end Start_Tests;

   procedure Stop_Tests     (Format      : in out Base_Format_Type) is
   begin
      Format.In_Tests := False;
   end Stop_Tests;

   --------------------------
   --  Start/Stop_Feature  --
   --------------------------

   procedure Start_Feature  (Format      : in out Base_Format_Type;
                             Feature     : in     String;
                             Description : in     String;
                             Position    : in     String) is
   begin
      Format.Feature_ID  := Format.Feature_ID + 1;
      Format.Scenario_ID := 0;
      Format.ScenOutl_ID := 0;
      Format.Step_ID     := 0;
      Format.In_Feature  := True;
      Format.Feature     := (Name        => To_Unbounded_String (Feature),
                             Description => To_Unbounded_String (Description),
                             Position    => To_Unbounded_String (Position),
                             others      => <>);
   end Start_Feature;

   procedure Stop_Feature   (Format      : in out Base_Format_Type) is
   begin
      Format.In_Feature    := False;
   end Stop_Feature;

   --------------------------
   --  Start/Stop_Outline  --
   --------------------------

   procedure Start_Outline  (Format     : in out Base_Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type) is
   begin
      Format.Previous_Step_Type := Step_Null;
      Format.ScenOutl_ID := Format.ScenOutl_ID + 1;
      Format.Scenario_ID := Format.Scenario_ID + 1;
      Format.Example_ID  := 0;
      Format.Step_ID     := 0;
      Format.In_Outline  := True;
      Format.Outline     := (Name     => To_Unbounded_String (Scenario),
                             Position => To_Unbounded_String (Position),
                             Tags     => Convert (Tags),
                             others   => <>);
   end Start_Outline;

   procedure Stop_Outline   (Format     : in out Base_Format_Type) is
   begin
      Format.In_Outline := False;
   end Stop_Outline;

   ---------------------------
   --  Start/Stop_Scenario  --
   ---------------------------

   procedure Start_Scenario (Format     : in out Base_Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type) is
   begin
      Format.Previous_Step_Type := Step_Null;
      Format.Scenario_ID := Format.Scenario_ID + 1;
      Format.Step_ID     := 0;
      Format.In_Scenario := True;
      Format.Scenario    := (Name     => To_Unbounded_String (Scenario),
                             Position => To_Unbounded_String (Position),
                             Tags     => Convert (Tags),
                             others   => <>);
      if Format.In_Outline then
         Format.Example_ID := Format.Example_ID + 1;
      else
         Format.ScenOutl_ID := Format.ScenOutl_ID + 1;
      end if;
   end Start_Scenario;

   procedure Stop_Scenario  (Format     : in out Base_Format_Type) is
   begin
      Format.In_Scenario := False;
   end Stop_Scenario;

   -----------------------------
   --  Start/Stop_Background  --
   -----------------------------

   procedure Start_Background (Format     : in out Base_Format_Type;
                               Background : in     String;
                               Position   : in     String) is
   begin
      Format.Previous_Step_Type := Step_Null;
      Format.In_Background := True;
      Format.Background    := (Name     => To_Unbounded_String (Background),
                               Position => To_Unbounded_String (Position),
                               others   => <>);
   end Start_Background;

   procedure Stop_Background  (Format     : in out Base_Format_Type) is
   begin
      Format.In_Background := False;
   end Stop_Background;

   -----------------------
   --  Start/Stop_Step  --
   -----------------------

   procedure Start_Step     (Format     : in out Base_Format_Type;
                             Step       : in     Step_Kind;
                             Name       : in     String;
                             Position   : in     String) is
   begin
      Format.Step_ID := Format.Step_ID + 1;
      Format.In_Step := True;
      Format.Step    := (Kind     => Step,
                         Name     => To_Unbounded_String (Name),
                         Position => To_Unbounded_String (Position));
   end Start_Step;

   procedure Stop_Step      (Format     : in out Base_Format_Type) is
   begin
      if Format.In_Scenario then
         Format.Exec_Steps := Format.Exec_Steps + 1;
      end if;
      Format.In_Step := False;
   end Stop_Step;

   ------------------
   --  Set_Output  --
   ------------------

   procedure Set_Output     (Format     : in out Base_Format_Type;
                             Output     : in     String)
   is
   begin
      Format.Output.Create (Ada.Text_IO.Out_File, Output);
   end Set_Output;

   -----------------
   --  Set_Debug  --
   -----------------

   procedure Set_Debug      (Format     : in out Base_Format_Type;
                             Debug_Mode : in     Boolean)
   is
   begin
      Format.Debug_Mode := Debug_Mode;
   end Set_Debug;

   ---------------------
   --  Set_Num_Steps  --
   ---------------------

   procedure Set_Num_Steps  (Format     : in out Base_Format_Type;
                             Num_Steps  : in     Natural)
   is
   begin
      Format.Num_Steps := Num_Steps;
   end Set_Num_Steps;

   --------------------
   --  List_Feature  --
   --------------------

   procedure List_Feature   (Format     : in out Base_Format_Type;
                             Name       : in     String) is
   begin
      if Format.Feature_ID > 0 then
         Format.Output.New_Line;
      end if;
      Format.Output.Put_Line ("Feature: " & Name);
      Format.Output.New_Line;
      Format.Feature_ID := Format.Feature_ID + 1;
   end List_Feature;

   ---------------------
   --  List_Scenario  --
   ---------------------

   procedure List_Scenario  (Format     : in out Base_Format_Type;
                             Name       : in     String;
                             Filename   : in     String;
                             Line       : in     Positive;
                             Num        : in     Positive)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      Format.Output.Put_Line ("  " & Filename & "#" & Trim (Num'Img, Left) &
                              ":" & Trim (Line'Img, Left) & " " & Name);
   end List_Scenario;

   pragma Style_Checks (Off);

   procedure S_Feature  (F : in out Base_Format_Type; S : in String) is begin F.Str_Feature  := To_Unbounded_String (S); end S_Feature;
   procedure S_Scenario (F : in out Base_Format_Type; S : in String) is begin F.Str_Scenario := To_Unbounded_String (S); end S_Scenario;
   procedure S_Outline  (F : in out Base_Format_Type; S : in String) is begin F.Str_Outline  := To_Unbounded_String (S); end S_Outline;

   function  S_Feature  (F : in Base_Format_Type) return String is begin return To_String (F.Str_Feature);  end S_Feature;
   function  S_Scenario (F : in Base_Format_Type) return String is begin return To_String (F.Str_Scenario); end S_Scenario;
   function  S_Outline  (F : in Base_Format_Type) return String is begin return To_String (F.Str_Outline);  end S_Outline;

   pragma Style_Checks (On);

end XReqLib.Format.Base;
