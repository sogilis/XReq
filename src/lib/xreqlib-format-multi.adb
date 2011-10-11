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

package body XReqLib.Format.Multi is

   use Format_Vector_Package;

   --------------
   -- Finalize --
   --------------

   procedure Finalize   (Object : in out Multi_Format_Type) is
      I : Cursor := First (Object.Sub_Formats);
      X : Format_Ptr;
   begin
      while Has_Element (I) loop
         X := Element (I);
         Free (X);
         Next (I);
      end loop;
   end Finalize;

   -----------------
   -- Start_Tests --
   -----------------

   procedure Start_Tests (Format      : in out Multi_Format_Type) is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Start_Tests;
         Next (I);
      end loop;
   end Start_Tests;

   -----------------
   -- Put_Summary --
   -----------------

   procedure Put_Summary
     (Format      : in out Multi_Format_Type;
      Report      : in     Report_Type;
      D           : in     Duration)
   is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Put_Summary (Report, D);
         Next (I);
      end loop;
   end Put_Summary;

   ----------------
   -- Stop_Tests --
   ----------------

   procedure Stop_Tests (Format      : in out Multi_Format_Type) is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Stop_Tests;
         Next (I);
      end loop;
   end Stop_Tests;

   -------------------
   -- Start_Feature --
   -------------------

   procedure Start_Feature
     (Format      : in out Multi_Format_Type;
      Feature     : in     String;
      Description : in     String;
      Position    : in     String)
   is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Start_Feature (Feature, Description, Position);
         Next (I);
      end loop;
   end Start_Feature;

   -----------------
   -- Put_Feature --
   -----------------

   procedure Put_Feature (Format      : in out Multi_Format_Type) is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Put_Feature;
         Next (I);
      end loop;
   end Put_Feature;

   ------------------
   -- Stop_Feature --
   ------------------

   procedure Stop_Feature (Format      : in out Multi_Format_Type) is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Stop_Feature;
         Next (I);
      end loop;
   end Stop_Feature;

   -------------------
   -- Start_Outline --
   -------------------

   procedure Start_Outline
     (Format     : in out Multi_Format_Type;
      Scenario   : in     String;
      Position   : in     String;
      Tags       : in     Tag_Array_Type)
   is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Start_Outline (Scenario, Position, Tags);
         Next (I);
      end loop;
   end Start_Outline;

   -------------------
   -- Enter_Outline --
   -------------------

   procedure Enter_Outline (Format     : in out Multi_Format_Type) is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Enter_Outline;
         Next (I);
      end loop;
   end Enter_Outline;

   -------------------
   -- Begin_Outline --
   -------------------

   procedure Begin_Outline (Format     : in out Multi_Format_Type) is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Begin_Outline;
         Next (I);
      end loop;
   end Begin_Outline;

   ------------------------
   -- Put_Outline_Report --
   ------------------------

   procedure Put_Outline_Report
     (Format     : in out Multi_Format_Type;
      Table      : in     Table_Type)
   is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Put_Outline_Report (Table);
         Next (I);
      end loop;
   end Put_Outline_Report;

   ------------------
   -- Stop_Outline --
   ------------------

   procedure Stop_Outline (Format     : in out Multi_Format_Type) is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Stop_Outline;
         Next (I);
      end loop;
   end Stop_Outline;

   --------------------
   -- Start_Scenario --
   --------------------

   procedure Start_Scenario
     (Format     : in out Multi_Format_Type;
      Scenario   : in     String;
      Position   : in     String;
      Tags       : in     Tag_Array_Type)
   is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Start_Scenario (Scenario, Position, Tags);
         Next (I);
      end loop;
   end Start_Scenario;

   --------------------
   -- Enter_Scenario --
   --------------------

   procedure Enter_Scenario (Format     : in out Multi_Format_Type) is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Enter_Scenario;
         Next (I);
      end loop;
   end Enter_Scenario;

   --------------------
   -- Begin_Scenario --
   --------------------

   procedure Begin_Scenario (Format     : in out Multi_Format_Type) is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Begin_Scenario;
         Next (I);
      end loop;
   end Begin_Scenario;

   -------------------
   -- Stop_Scenario --
   -------------------

   procedure Stop_Scenario (Format     : in out Multi_Format_Type) is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Stop_Scenario;
         Next (I);
      end loop;
   end Stop_Scenario;

   ----------------------
   -- Start_Background --
   ----------------------

   procedure Start_Background
     (Format     : in out Multi_Format_Type;
      Background : in     String;
      Position   : in     String)
   is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Start_Background (Background, Position);
         Next (I);
      end loop;
   end Start_Background;

   --------------------
   -- Put_Background --
   --------------------

   procedure Put_Background (Format     : in out Multi_Format_Type) is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Put_Background;
         Next (I);
      end loop;
   end Put_Background;

   ---------------------
   -- Stop_Background --
   ---------------------

   procedure Stop_Background (Format     : in out Multi_Format_Type) is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Stop_Background;
         Next (I);
      end loop;
   end Stop_Background;

   ----------------
   -- Start_Step --
   ----------------

   procedure Start_Step
     (Format     : in out Multi_Format_Type;
      Step       : in     Step_Kind;
      Name       : in     String;
      Position   : in     String)
   is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Start_Step (Step, Name, Position);
         Next (I);
      end loop;
   end Start_Step;

   --------------
   -- Put_Step --
   --------------

   procedure Put_Step
     (Format     : in out Multi_Format_Type;
      Args       : in     Arg_Type;
      Success    : in     Status_Type)
   is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Put_Step (Args, Success);
         Next (I);
      end loop;
   end Put_Step;

   ---------------
   -- Put_Error --
   ---------------

   procedure Put_Error
     (Format     : in out Multi_Format_Type;
      Err        : in     Exception_Occurrence)
   is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Put_Error (Err);
         Next (I);
      end loop;
   end Put_Error;

   ---------------
   -- Stop_Step --
   ---------------

   procedure Stop_Step (Format     : in out Multi_Format_Type) is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Stop_Step;
         Next (I);
      end loop;
   end Stop_Step;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output
     (Format     : in out Multi_Format_Type;
      Output     : in     String)
   is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Set_Output (Output);
         Next (I);
      end loop;
   end Set_Output;

   ---------------
   -- Set_Debug --
   ---------------

   procedure Set_Debug
     (Format     : in out Multi_Format_Type;
      Debug_Mode : in     Boolean)
   is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Set_Debug (Debug_Mode);
         Next (I);
      end loop;
   end Set_Debug;

   -------------------
   -- Set_Num_Steps --
   -------------------

   procedure Set_Num_Steps
     (Format     : in out Multi_Format_Type;
      Num_Steps  : in     Natural)
   is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).Set_Num_Steps (Num_Steps);
         Next (I);
      end loop;
   end Set_Num_Steps;

   ------------------
   -- List_Feature --
   ------------------

   procedure List_Feature
     (Format     : in out Multi_Format_Type;
      Name       : in     String)
   is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).List_Feature (Name);
         Next (I);
      end loop;
   end List_Feature;

   -------------------
   -- List_Scenario --
   -------------------

   procedure List_Scenario
     (Format     : in out Multi_Format_Type;
      Name       : in     String;
      Filename   : in     String;
      Line       : in     Positive;
      Num        : in     Positive)
   is
      I : Cursor := First (Format.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).List_Scenario (Name, Filename, Line, Num);
         Next (I);
      end loop;
   end List_Scenario;

   ---------------
   -- S_Feature --
   ---------------

   procedure S_Feature (F : in out Multi_Format_Type; S : in String) is
      I : Cursor := First (F.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).S_Feature (S);
         Next (I);
      end loop;
   end S_Feature;

   ----------------
   -- S_Scenario --
   ----------------

   procedure S_Scenario (F : in out Multi_Format_Type; S : in String) is
      I : Cursor := First (F.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).S_Scenario (S);
         Next (I);
      end loop;
   end S_Scenario;

   ---------------
   -- S_Outline --
   ---------------

   procedure S_Outline (F : in out Multi_Format_Type; S : in String) is
      I : Cursor := First (F.Sub_Formats);
   begin
      while Has_Element (I) loop
         Element (I).S_Outline (S);
         Next (I);
      end loop;
   end S_Outline;

   --------------------
   -- Add_Sub_Format --
   --------------------

   procedure Add_Sub_Format
     (Format     : in out Multi_Format_Type;
      Sub_Format : in     Format_Ptr)
   is
   begin
      Append (Format.Sub_Formats, Sub_Format);
   end Add_Sub_Format;

   ----------------------
   -- New_Multi_Format --
   ----------------------

   function New_Multi_Format return Multi_Format_Ptr is
   begin
      return new Multi_Format_Type;
   end New_Multi_Format;

end XReqLib.Format.Multi;
