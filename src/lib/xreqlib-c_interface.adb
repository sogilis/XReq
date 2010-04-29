--                         Copyright (C) 2010, Sogilis                       --

with Ada.Real_Time;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with GNAT.OS_Lib;
with XReqLib.CLI;


package body XReqLib.C_Interface is

   function Value (Tags : in XReq_Tags)   return XReqLib.Format.Tag_Array_Type;
   function Value (Bool : in XReq_Bool)   return Boolean;
   function Value (Stat : in XReq_Status) return XReqLib.Format.Status_Type;
   function Value (Kind : in XReq_Kind)   return Step_All_Kind;
   function Convert (Bool : in Boolean)   return XReq_Bool;



   function Value (Tags : in XReq_Tags) return XReqLib.Format.Tag_Array_Type is
      use Ada.Strings.Unbounded;
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
                                        C      : in long) is
   begin
      Format.all.List_Scenario (Value (A), Value (B), Integer (C));
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
   begin
      Format.all.Put_Error (Error.all);
   end XReq_Format_Put_Error;
   procedure XReq_Format_Stop_Step  (Format : in XReq_Format_Ptr) is
   begin
      Format.all.Stop_Step;
   end XReq_Format_Stop_Step;



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
                                             N    : in long)
                                                return XReq_Bool
   is
   begin
      return Convert (Cond.all.Eval (Value (Pos), Integer (N)));
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

   procedure XReq_CLI_Parse_Arguments  (argc : long; argv : chars_ptr_array;
                                        Format     : access XReq_Format_Ptr;
                                        Continue   : access XReq_Bool;
                                        Cond       : in XReq_Conditional_Ptr;
                                        List_Mode  : access XReq_Bool;
                                        Name       : in     XReq_Cstr)
   is
      use GNAT.OS_Lib;
      use XReqLib.Format;
      Args       : Argument_List_Access (1 .. Integer (argc - 1));
      Res_Continue, Res_List_Mode : Boolean;
      Res_Format : Format_Ptr;
      Res_Cond   : Conditional_Type;
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
         Name      => Value (Name));
      Format.all    := Res_Format;
      Continue.all  := Convert (Res_Continue);
      Cond.all      := Res_Cond;
      List_Mode.all := Convert (Res_List_Mode);
   end XReq_CLI_Parse_Arguments;

   pragma Warnings (On);

end XReqLib.C_Interface;
