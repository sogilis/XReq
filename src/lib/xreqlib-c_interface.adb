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

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Real_Time;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;
with XReqLib.CLI;


package body XReqLib.C_Interface is  --  GCOV_IGNORE_BEGIN

   function Value (Tags : in XReq_Tags)   return XReqLib.Format.Tag_Array_Type;
   function Value (Bool : in XReq_Bool)   return Boolean;
   function Value (Stat : in XReq_Status) return XReqLib.Format.Status_Type;
   function Value (Kind : in XReq_Kind)   return Step_All_Kind;
   function Convert (Bool : in Boolean)   return XReq_Bool;




   function Value (Tags : in XReq_Tags) return XReqLib.Format.Tag_Array_Type is
      use Ada.Containers;
      use XReqLib.Format;
      use String_Vectors;
      function Convert is new Ada.Unchecked_Conversion (XReq_Tags, chars_ptr);
      Vec : String_Vector;
      I   : size_t;
   begin
      if Convert (Tags) /= Null_Ptr then
         I := Tags'First;
         while Tags (I) /= Null_Ptr loop
            Append (Vec, To_Unbounded_String (Value (Tags (I))));
            I := I + 1;
         end loop;
      end if;
      return Arr : Tag_Array_Type (1 .. Integer (Length (Vec))) do
         for J in Arr'Range loop
            Arr (J) := Element (Vec, J - 1);
         end loop;
      end return;
   end Value;

   function Value (Bool : in XReq_Bool) return Boolean is
   begin
      return Bool /= 0;
   end Value;

   function Value (Stat : in XReq_Status) return XReqLib.Format.Status_Type is
      use XReqLib.Format;
   begin
      return Status_Type'Val (Integer (Stat));
   end Value;

   function Value (Kind : in XReq_Kind)   return Step_All_Kind is
   begin
      return Step_All_Kind'Val (Integer (Kind));
   end Value;

   function Convert (Bool : in Boolean)   return XReq_Bool is
   begin
      if Bool then
         return 1;
      else
         return 0;
      end if;
   end Convert;



   pragma Warnings (off);

   procedure XReq_Time_Start (Duration : in XReq_Duration_Ptr) is
      use Ada.Real_Time;
      function Convert is new Ada.Unchecked_Conversion (Time, XReq_Duration);
   begin
      Duration.all := Convert (Clock);
   end XReq_Time_Start;
   procedure XReq_Time_Stop  (Duration : in XReq_Duration_Ptr) is
      use Ada.Real_Time;
      function Convert is new Ada.Unchecked_Conversion (XReq_Duration, Time);
      Time_Start : constant Time := Convert (Duration.all);
      Time_Stop  : constant Time := Clock;
   begin
      Duration.all := XReq_Duration (To_Duration (Time_Stop - Time_Start));
   end XReq_Time_Stop;



   procedure XReq_Format_Set_Num_Steps (Format : in XReq_Format_Ptr;
                                        Num    : in long) is
   begin
      Format.all.Set_Num_Steps (Integer (Num));
   end XReq_Format_Set_Num_Steps;

   procedure XReq_Format_List_Feature  (Format : in XReq_Format_Ptr;
                                        A      : in XReq_Cstr) is
   begin
      Format.all.List_Feature (Value (A));
   end XReq_Format_List_Feature;

   procedure XReq_Format_List_Scenario (Format : in XReq_Format_Ptr;
                                        A, B   : in XReq_Cstr;
                                        C, D   : in long) is
   begin
      Format.all.List_Scenario (Value (A), Value (B),
                                Integer (C), Integer (D));
   end XReq_Format_List_Scenario;


   procedure XReq_Format_Free          (Format : in XReq_Format_Ptr) is
      Ptr : XReq_Format_Ptr := Format;
   begin
      XReqLib.Format.Free (Ptr);
   end XReq_Format_Free;



   procedure XReq_Format_Start_Tests   (Format : in XReq_Format_Ptr) is
   begin
      Format.all.Start_Tests;
   end XReq_Format_Start_Tests;

   procedure XReq_Format_Put_Summary   (Format : in XReq_Format_Ptr;
                                        Report : in XReq_Report_Ptr;
                                        Time   : in XReq_Duration) is
   begin
      Format.all.Put_Summary (Report.all, Duration (Time));
   end XReq_Format_Put_Summary;
   procedure XReq_Format_Stop_Tests    (Format : in XReq_Format_Ptr) is
   begin
      Format.all.Stop_Tests;
   end XReq_Format_Stop_Tests;



   procedure XReq_Format_Start_Feature (Format  : in XReq_Format_Ptr) is
   begin
      Format.all.Start_Feature;
   end XReq_Format_Start_Feature;

   procedure XReq_Format_Put_Feature   (Format  : in XReq_Format_Ptr;
                                        A, B, C : in XReq_Cstr)
   is
   begin
      Format.all.Put_Feature (Value (A), Value (B), Value (C));
   end XReq_Format_Put_Feature;
   procedure XReq_Format_Stop_Feature  (Format  : in XReq_Format_Ptr) is
   begin
      Format.all.Stop_Feature;
   end XReq_Format_Stop_Feature;



   procedure XReq_Format_Start_Background     (Format : in XReq_Format_Ptr;
                                               First  : in XReq_Bool) is
   begin
      Format.all.Start_Background (Value (First));
   end XReq_Format_Start_Background;

   procedure XReq_Format_Put_Background       (Format : in XReq_Format_Ptr;
                                               A, B   : in XReq_Cstr;
                                               Tags   : in XReq_Tags)
   is
   begin
      Format.all.Put_Background (Value (A), Value (B), Value (Tags));
   end XReq_Format_Put_Background;

   procedure XReq_Format_Stop_Background      (Format : in XReq_Format_Ptr;
                                               First  : in XReq_Bool) is
   begin
      Format.all.Stop_Background (Value (First));
   end XReq_Format_Stop_Background;



   procedure XReq_Format_Enter_Outline        (Format : in XReq_Format_Ptr) is
   begin
      Format.all.Enter_Outline;
   end XReq_Format_Enter_Outline;

   procedure XReq_Format_Start_Outline        (Format : in XReq_Format_Ptr) is
   begin
      Format.all.Start_Outline;
   end XReq_Format_Start_Outline;

   procedure XReq_Format_Put_Outline          (Format : in XReq_Format_Ptr;
                                               A, B   : in XReq_Cstr;
                                               Tags   : in XReq_Tags)
   is
   begin
      Format.all.Put_Outline (Value (A), Value (B), Value (Tags));
   end XReq_Format_Put_Outline;
   procedure XReq_Format_Put_Outline_Report   (Format : in XReq_Format_Ptr;
                                               Table  : in XReq_Table_Ptr)
   is
   begin
      Format.all.Put_Outline_Report (Table.all);
   end XReq_Format_Put_Outline_Report;
   procedure XReq_Format_Stop_Outline         (Format : in XReq_Format_Ptr) is
   begin
      Format.all.Stop_Outline;
   end XReq_Format_Stop_Outline;



   procedure XReq_Format_Enter_Scenario       (Format : in XReq_Format_Ptr) is
   begin
      Format.all.Enter_Scenario;
   end XReq_Format_Enter_Scenario;

   procedure XReq_Format_Start_Scenario       (Format : in XReq_Format_Ptr) is
   begin
      Format.all.Start_Scenario;
   end XReq_Format_Start_Scenario;

   procedure XReq_Format_Put_Scenario         (Format : in XReq_Format_Ptr;
                                               A, B   : in XReq_Cstr;
                                               Tags   : in XReq_Tags)
   is
   begin
      Format.all.Put_Scenario (Value (A), Value (B), Value (Tags));
   end XReq_Format_Put_Scenario;
   procedure XReq_Format_Put_Scenario_Outline (Format : in XReq_Format_Ptr;
                                               Num    : in long;
                                               A, B   : in XReq_Cstr;
                                               Tags   : in XReq_Tags)
   is
   begin
      Format.all.Put_Scenario_Outline (Integer (Num), Value (A), Value (B),
                                       Value (Tags));
   end XReq_Format_Put_Scenario_Outline;
   procedure XReq_Format_Stop_Scenario        (Format : in XReq_Format_Ptr) is
   begin
      Format.all.Stop_Scenario;
   end XReq_Format_Stop_Scenario;



   procedure XReq_Format_Start_Step (Format : in XReq_Format_Ptr) is
   begin
      Format.all.Start_Step;
   end XReq_Format_Start_Step;

   procedure XReq_Format_Put_Step   (Format : in XReq_Format_Ptr;
                                     Kind   : in XReq_Kind;
                                     A, B   : in XReq_Cstr;
                                     Args   : in XReq_Args_Ptr;
                                     Status : in XReq_Status)
   is
   begin
      Format.all.Put_Step (Value (Kind), Value (A), Value (B), Args.all,
                           Value (Status));
   end XReq_Format_Put_Step;

   procedure XReq_Format_Put_Error  (Format : in XReq_Format_Ptr;
                                     Error  : in XReq_Error_Ptr)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Message : Unbounded_String;
      Step_Error : exception;
   begin
      if Error.all.Error then
         Append (Message, Error.all.Message);
         if Error.all.File /= Null_Unbounded_String then
            Append (Message, ASCII.LF & "in: " & Error.all.File & ":" &
                             Trim (Error.all.Line'Img, Left));
         end if;
         raise Step_Error with To_String (Message);
      end if;
   exception
      when E : Step_Error =>
         Format.all.Put_Error (E);
   end XReq_Format_Put_Error;

   procedure XReq_Format_Stop_Step  (Format : in XReq_Format_Ptr) is
   begin
      Format.all.Stop_Step;
   end XReq_Format_Stop_Step;


   procedure XReq_Formet_STR_Feature  (Format : in XReq_Format_Ptr;
                                       S      : in XReq_Cstr) is
   begin
      Format.all.S_Feature (Value (S));
   end XReq_Formet_STR_Feature;
   procedure XReq_Formet_STR_Scenario (Format : in XReq_Format_Ptr;
                                       S      : in XReq_Cstr) is
   begin
      Format.all.S_Scenario (Value (S));
   end XReq_Formet_STR_Scenario;
   procedure XReq_Formet_STR_Outline  (Format : in XReq_Format_Ptr;
                                       S      : in XReq_Cstr) is
   begin
      Format.all.S_Outline  (Value (S));
   end XReq_Formet_STR_Outline;



   function  XReq_Conditional_New               return XReq_Conditional_Ptr is
   begin
      return new XReqLib.Format.Conditional_Type;
   end XReq_Conditional_New;

   procedure XReq_Conditional_Free          (Cond : in XReq_Conditional_Ptr) is
      procedure Dealloc is new Ada.Unchecked_Deallocation
         (XReq_Conditional, XReq_Conditional_Ptr);
      Ptr : XReq_Conditional_Ptr := Cond;
   begin
      Dealloc (Ptr);
   end XReq_Conditional_Free;

   function  XReq_Conditional_Eval_Tags     (Cond : in XReq_Conditional_Ptr;
                                             Tags : in XReq_Tags)
                                               return XReq_Bool
   is
   begin
      return Convert (Cond.all.Eval (Value (Tags)));
   end XReq_Conditional_Eval_Tags;
   function  XReq_Conditional_Eval_Position (Cond : in XReq_Conditional_Ptr;
                                             Pos  : in XReq_Cstr;
                                             N, M : in long)
                                                return XReq_Bool
   is
   begin
      return Convert (Cond.all.Eval (Value (Pos), Integer (N), Integer (M)));
   end XReq_Conditional_Eval_Position;


   function  XReq_Report_New                 return XReq_Report_Ptr is
   begin
      return new XReq_Report;
   end XReq_Report_New;

   procedure XReq_Report_step_Free     (Report : in XReq_Report_Ptr) is
      procedure Dealloc is new Ada.Unchecked_Deallocation
         (XReq_Report, XReq_Report_Ptr);
      Ptr : XReq_Report_Ptr := Report;
   begin
      Dealloc (Ptr);
   end XReq_Report_step_Free;

   procedure XReq_Report_step_skip     (Report : in XReq_Report_Ptr) is
   begin
      Report.all.Count_Steps_Skipped := Report.all.Count_Steps_Skipped + 1;
   end XReq_Report_step_skip;

   procedure XReq_Report_step_pass     (Report : in XReq_Report_Ptr) is
   begin
      Report.all.Count_Steps_Passed := Report.all.Count_Steps_Passed + 1;
   end XReq_Report_step_pass;

   procedure XReq_Report_step_fail     (Report : in XReq_Report_Ptr) is
   begin
      Report.all.Count_Steps_Failed := Report.all.Count_Steps_Failed + 1;
   end XReq_Report_step_fail;

   procedure XReq_Report_scenario_pass (Report : in XReq_Report_Ptr) is
   begin
      Report.all.Count_Scenario_Passed := Report.all.Count_Scenario_Passed + 1;
   end XReq_Report_scenario_pass;

   procedure XReq_Report_scenario_fail (Report : in XReq_Report_Ptr) is
   begin
      Report.all.Count_Scenario_Failed := Report.all.Count_Scenario_Failed + 1;
   end XReq_Report_scenario_fail;

   procedure XReq_Report_num_steps_inc (Report : in XReq_Report_Ptr;
                                        N      : in long)
   is
   begin
      Report.all.Num_Steps := Report.all.Num_Steps + Integer (N);
   end XReq_Report_num_steps_inc;

   function XReq_Report_get_num_steps (Report : in XReq_Report_Ptr) return long
   is
   begin
      return long (Report.all.Num_Steps);
   end XReq_Report_get_num_steps;

   function  XReq_Report_Status        (Report : in XReq_Report_Ptr)
                                             return XReq_Bool
   is
   begin
      return Convert (XReqLib.Report.Status (Report.all));
   end XReq_Report_Status;

   function  XReq_CLI_Parse_Arguments  (argc : long; argv : chars_ptr_array;
                                        Format     : access XReq_Format_Ptr;
                                        Continue   : access XReq_Bool;
                                        Cond       : in XReq_Conditional_Ptr;
                                        List_Mode  : access XReq_Bool;
                                        Name       : in     XReq_Cstr)
                                                     return XReq_Bool
   is
      use GNAT.OS_Lib;
      use XReqLib.Format;
      Args       : Argument_List_Access (1 .. Integer (argc - 1));
      Res_Continue, Res_List_Mode : Boolean;
      Res_Format : Format_Ptr;
      Res_Cond   : Conditional_Type;
      Success    : Boolean := True;
   begin
      Args := new Argument_List (1 .. Integer (argc - 1));
      for I in 1 .. Integer (argc - 1) loop
         Args.all (I) := new String'(Value (argv (size_t (I))));
      end loop;
      XReqLib.CLI.Parse_Arguments
        (Args      => Args,
         Format    => Res_Format,
         Continue  => Res_Continue,
         Cond      => Res_Cond,
         List_Mode => Res_List_Mode,
         Success   => Success,
         Name      => Value (Name));
      Format.all    := Res_Format;
      Continue.all  := Convert (Res_Continue);
      Cond.all      := Res_Cond;
      List_Mode.all := Convert (Res_List_Mode);
      return Convert (Success);
   end XReq_CLI_Parse_Arguments;

   function XReq_Args_New   return XReq_Args_Ptr is
   begin
      return new XReq_Args;
   end XReq_Args_New;

   function XReq_Table_New return XReq_Table_Ptr is
   begin
      return new XReq_Table;
   end XReq_Table_New;

   function XReq_Error_New return XReq_Error_Ptr is
   begin
      return new XReq_Error;
   end XReq_Error_New;


   procedure XReq_Args_Make      (Args : in XReq_Args_Ptr; S : in XReq_Cstr) is
   begin
      Args.Make (Value (S));
   end XReq_Args_Make;

   procedure XReq_Args_Add_Match (Args : in XReq_Args_Ptr; A, B : in long) is
   begin
      Args.Add_Match (Integer (A), Integer (B));
   end XReq_Args_Add_Match;

   procedure XReq_Args_Add_Sep   (Args : in XReq_Args_Ptr; A    : in long) is
   begin
      Args.Add_Sep (Integer (A));
   end XReq_Args_Add_Sep;


   procedure XReq_Args_Add_Para (Args : in XReq_Args_Ptr; A : in XReq_Cstr) is
   begin
      Args.Add_Para (Value (A));
   end XReq_Args_Add_Para;

   procedure XReq_Args_Add_Text (Args : in XReq_Args_Ptr; A : in XReq_Cstr) is
   begin
      Args.Add_Text (Value (A));
   end XReq_Args_Add_Text;

   procedure XReq_Args_Add_Table  (Args : in XReq_Args_Ptr;
                                   Tble : in XReq_Table_Ptr) is
   begin
      Args.Add_Table (Tble.all);
   end XReq_Args_Add_Table;

   function  XReq_Args_Match      (Args : in XReq_Args_Ptr; A    : in long)
                                      return XReq_Cstr is
   begin
      return New_String (Args.Match (Integer (A)));
   end XReq_Args_Match;

   function  XReq_Args_Text       (Args : in XReq_Args_Ptr; A    : in long)
                                      return XReq_Cstr is
   begin
      return New_String (Args.Text (Integer (A)));
   end XReq_Args_Text;


   function  XReq_Args_Table      (Args : in XReq_Args_Ptr; A    : in long)
                                      return XReq_Table_Ptr is
   begin
      return new XReq_Table'(Args.Table (Integer (A)));
   end XReq_Args_Table;

   procedure XReq_Args_Free      (Args : in XReq_Args_Ptr) is
      procedure Dealloc is new Ada.Unchecked_Deallocation
         (XReq_Args, XReq_Args_Ptr);
      Ptr : XReq_Args_Ptr := Args;
   begin
      Dealloc (Ptr);
   end XReq_Args_Free;



   procedure XReq_Table_Put      (Tble : in XReq_Table_Ptr;
                                  X, Y : in long;
                                  Str  : in XReq_Cstr) is
   begin
      Tble.Put (Integer (X), Integer (Y), Value (Str));
   end XReq_Table_Put;

   function  XReq_Table_Equals   (A, B : in XReq_Table_Ptr) return XReq_Bool is
      use XReqLib.String_Tables;
   begin
      return Convert (A.all = B.all);
   end XReq_Table_Equals;

   procedure XReq_Table_Free     (Tble : in XReq_Table_Ptr) is
      procedure Dealloc is new Ada.Unchecked_Deallocation
         (XReq_Table, XReq_Table_Ptr);
      Ptr : XReq_Table_Ptr := Tble;
   begin
      Dealloc (Ptr);
   end XReq_Table_Free;


   procedure XReq_Error_Clear    (Err : in XReq_Error_Ptr) is
   begin
      Err.all.Error := False;
   end XReq_Error_Clear;

   procedure XReq_Error_Make         (Err  : in XReq_Error_Ptr;
                                      A, B : in XReq_Cstr;
                                      Line : in long)
   is
   begin
      Err.all :=
        (Error   => True,
         Message => To_Unbounded_String (Value (A)),
         File    => To_Unbounded_String (Value (B)),
         Line    => Integer (Line));
   end XReq_Error_Make;

   function  XReq_Error_Is_Null      (Err : in XReq_Error_Ptr)
                                        return XReq_Bool is
   begin
      return Convert (not Err.all.Error);
   end XReq_Error_Is_Null;

   procedure XReq_Error_Free     (Err : in XReq_Error_Ptr) is
      procedure Dealloc is new Ada.Unchecked_Deallocation
         (XReq_Error, XReq_Error_Ptr);
      Ptr : XReq_Error_Ptr := Err;
   begin
      Dealloc (Ptr);
   end XReq_Error_Free;

   procedure XReq_String_Free        (Str : in XReq_Cstr) is
      Ptr : chars_ptr := chars_ptr (Str);
   begin
      Free (Ptr);
   end XReq_String_Free;


   pragma Warnings (On);

end XReqLib.C_Interface;
