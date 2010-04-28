--                         Copyright (C) 2010, Sogilis                       --

package body XReqLib.C_Interface is

   pragma Warnings (off);

   procedure XReq_Time_Start (Duration : in XReq_Duration_Ptr) is
   begin
      null;
   end XReq_Time_Start;
   procedure XReq_Time_Stop  (Duration : in XReq_Duration_Ptr) is
   begin
      null;  --  TODO
   end XReq_Time_Stop;



   procedure XReq_Format_Set_Num_Tests (Format : in XReq_Format_Ptr;
                                        Num    : in long) is
   begin
      null;  --  TODO
   end XReq_Format_Set_Num_Tests;
   procedure XReq_Format_List_Feature  (Format : in XReq_Format_Ptr;
                                        A      : in XReq_Cstr)
   is
   begin
      null;
   end XReq_Format_List_Feature;
   procedure XReq_Format_List_Scenario (Format : in XReq_Format_Ptr;
                                        A, B   : in XReq_Cstr;
                                        C      : in long)
   is
   begin
      null;
   end XReq_Format_List_Scenario;
   procedure XReq_Format_Free          (Format : in XReq_Format_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Free;



   procedure XReq_Format_Start_Tests   (Format : in XReq_Format_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Start_Tests;

   procedure XReq_Format_Put_Summary   (Format : in XReq_Format_Ptr;
                                        Report : in XReq_Report_Ptr;
                                        Time   : in XReq_Duration) is
   begin
      null;  --  TODO
   end XReq_Format_Put_Summary;
   procedure XReq_Format_Stop_Tests    (Format : in XReq_Format_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Stop_Tests;



   procedure XReq_Format_Start_Feature (Format  : in XReq_Format_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Start_Feature;

   procedure XReq_Format_Put_Feature   (Format  : in XReq_Format_Ptr;
                                        A, B, C : in XReq_Cstr) is
   begin
      null;  --  TODO
   end XReq_Format_Put_Feature;
   procedure XReq_Format_Stop_Feature  (Format  : in XReq_Format_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Stop_Feature;



   procedure XReq_Format_Start_Background     (Format : in XReq_Format_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Start_Background;

   procedure XReq_Format_Put_Background       (Format : in XReq_Format_Ptr;
                                               A, B   : in XReq_Cstr;
                                               Tags   : in XReq_Tags) is
   begin
      null;  --  TODO
   end XReq_Format_Put_Background;
   procedure XReq_Format_Stop_Background      (Format : in XReq_Format_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Stop_Background;



   procedure XReq_Format_Enter_Outline        (Format : in XReq_Format_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Enter_Outline;

   procedure XReq_Format_Start_Outline        (Format : in XReq_Format_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Start_Outline;

   procedure XReq_Format_Put_Outline          (Format : in XReq_Format_Ptr;
                                               A, B   : in XReq_Cstr;
                                               Tags   : in XReq_Tags) is
   begin
      null;  --  TODO
   end XReq_Format_Put_Outline;
   procedure XReq_Format_Put_Outline_Report   (Format : in XReq_Format_Ptr;
                                               Table  : in XReq_Table) is
   begin
      null;  --  TODO
   end XReq_Format_Put_Outline_Report;
   procedure XReq_Format_Stop_Outline         (Format : in XReq_Format_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Stop_Outline;



   procedure XReq_Format_Enter_Scenario       (Format : in XReq_Format_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Enter_Scenario;

   procedure XReq_Format_Start_Scenario       (Format : in XReq_Format_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Start_Scenario;

   procedure XReq_Format_Put_Scenario         (Format : in XReq_Format_Ptr;
                                               A, B   : in XReq_Cstr;
                                               Tags   : in XReq_Tags) is
   begin
      null;  --  TODO
   end XReq_Format_Put_Scenario;
   procedure XReq_Format_Put_Scenario_Outline (Format : in XReq_Format_Ptr;
                                               Num    : in long;
                                               A, B   : in XReq_Cstr;
                                               Tags   : in XReq_Tags) is
   begin
      null;  --  TODO
   end XReq_Format_Put_Scenario_Outline;
   procedure XReq_Format_Stop_Scenario        (Format : in XReq_Format_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Stop_Scenario;



   procedure XReq_Format_Start_Step (Format : in XReq_Format_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Start_Step;

   procedure XReq_Format_Put_Step   (Format : in XReq_Format_Ptr;
                                     A, B   : in XReq_Cstr;
                                     Args   : in XReq_Args_Ptr;
                                     Status : in XReq_Status) is
   begin
      null;  --  TODO
   end XReq_Format_Put_Step;
   procedure XReq_Format_Put_Error  (Format : in XReq_Format_Ptr;
                                     Error  : in XReq_Error_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Put_Error;
   procedure XReq_Format_Stop_Step  (Format : in XReq_Format_Ptr) is
   begin
      null;  --  TODO
   end XReq_Format_Stop_Step;



   function  XReq_Conditional_New               return XReq_Conditional_Ptr is
   begin
      return null;  --  TODO
   end XReq_Conditional_New;

   procedure XReq_Conditional_Free          (Cond : in XReq_Conditional_Ptr) is
   begin
      null;  --  TODO
   end XReq_Conditional_Free;

   function  XReq_Conditional_Eval_Tags     (Cond : in XReq_Conditional_Ptr;
                                             Tags : in XReq_Tags)
                                               return XReq_Bool is
   begin
      return 0;  --  TODO
   end XReq_Conditional_Eval_Tags;
   function  XReq_Conditional_Eval_Position (Cond : in XReq_Conditional_Ptr;
                                             Pos  : in XReq_Cstr)
                                                return XReq_Bool is
   begin
      return 0;  --  TODO
   end XReq_Conditional_Eval_Position;


   function  XReq_Report_New                 return XReq_Report_Ptr is
   begin
      return null;  --  TODO
   end XReq_Report_New;

   procedure XReq_Report_step_Free     (Report : in XReq_Report_Ptr) is
   begin
      null;  --  TODO
   end XReq_Report_step_Free;

   procedure XReq_Report_step_skip     (Report : in XReq_Report_Ptr) is
   begin
      null;  --  TODO
   end XReq_Report_step_skip;

   procedure XReq_Report_step_pass     (Report : in XReq_Report_Ptr) is
   begin
      null;  --  TODO
   end XReq_Report_step_pass;

   procedure XReq_Report_step_fail     (Report : in XReq_Report_Ptr) is
   begin
      null;  --  TODO
   end XReq_Report_step_fail;

   procedure XReq_Report_scenario_pass (Report : in XReq_Report_Ptr) is
   begin
      null;  --  TODO
   end XReq_Report_scenario_pass;

   procedure XReq_Report_scenario_fail (Report : in XReq_Report_Ptr) is
   begin
      null;  --  TODO
   end XReq_Report_scenario_fail;

   procedure XReq_Report_num_steps_inc (Report : in XReq_Report_Ptr;
                                        N      : in long) is
   begin
      null;  --  TODO
   end XReq_Report_num_steps_inc;
   procedure XReq_Report_get_num_steps (Report : in XReq_Report_Ptr) is
   begin
      null;  --  TODO
   end XReq_Report_get_num_steps;

   function  XReq_Report_Status        (Report : in XReq_Report_Ptr)
                                             return XReq_Bool is
   begin
      return 0;  --  TODO
   end XReq_Report_Status;

   procedure XReq_CLI_Parse_Arguments  (argc : long; argv : chars_ptr_array;
                                        Format     : access XReq_Format_Ptr;
                                        Continue   : access Boolean;
                                        Cond : access XReq_Conditional_Ptr;
                                        List_Mode  : access Boolean;
                                        Name       : in     String)
   is
   begin
      null;
   end XReq_CLI_Parse_Arguments;

   pragma Warnings (On);

end XReqLib.C_Interface;
