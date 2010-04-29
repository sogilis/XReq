--                         Copyright (C) 2010, Sogilis                       --

with Ada.Exceptions;
with Interfaces.C;
with Interfaces.C.Strings;
with XReqLib.Format;
with XReqLib.Report;
with XReqLib.Args;
with XReqLib.String_Tables;

use Interfaces.C;
use Interfaces.C.Strings;

package XReqLib.C_Interface is

   pragma Warnings (Off);

   ---  struct XReq_Format;
   ---  typedef struct XReq_Format XReq_Format;
   subtype XReq_Format     is XReqLib.Format.Format_Type;
   subtype XReq_Format_Ptr is XReqLib.Format.Format_Ptr;
   ---
   ---  struct XReq_Conditional;
   ---  typedef struct XReq_Conditional XReq_Conditional;
   subtype XReq_Conditional     is XReqLib.Format.Conditional_Type;
   type    XReq_Conditional_Ptr is access all XReq_Conditional;
   ---
   ---  struct XReq_Report;
   ---  typedef struct XReq_Report XReq_Report;
   subtype XReq_Report     is XReqLib.Report.Report_Type;
   type    XReq_Report_Ptr is access all XReq_Report;
   ---
   ---  struct XReq_Args;
   ---  typedef struct XReq_Args XReq_Args;
   subtype XReq_Args     is XReqLib.Args.Arg_Type;
   type    XReq_Args_Ptr is access all XReq_Args;
   ---
   ---  struct XReq_Table;
   ---  typedef struct XReq_Table XReq_Table;
   subtype XReq_Table     is XReqLib.String_Tables.Table;
   type    XReq_Table_Ptr is access all XReq_Table;
   ---
   ---  struct XReq_Error;
   ---  typedef struct XReq_Error XReq_Error;
   subtype XReq_Error     is Ada.Exceptions.Exception_Occurrence;
   type    XReq_Error_Ptr is access all XReq_Error;
   ---
   ---  typedef unsigned short XReq_Status;
   ---  typedef unsigned short XReq_Kind;
   ---  typedef unsigned long  XReq_Duration;
   ---  typedef const char*    XReq_Cstr;
   ---  typedef short          XReq_Bool;
   ---  typedef XReq_Cstr*     XReq_Tags;
   type XReq_Status       is new unsigned_short;
   type XReq_Kind         is new unsigned_short;
   type XReq_Duration     is new unsigned_long;
   type XReq_Duration_Ptr is access all XReq_Duration;
   type XReq_Cstr         is new chars_ptr;
   type XReq_Bool         is new short;
   type XReq_Tags         is new chars_ptr_array;
   ---
   ---  void XReq_Time_Start (XReq_Duration*);
   ---  void XReq_Time_Stop  (XReq_Duration*);
   procedure XReq_Time_Start (Duration : in XReq_Duration_Ptr);
   procedure XReq_Time_Stop  (Duration : in XReq_Duration_Ptr);
   ---
   ---  void XReq_Format_Set_Num_Steps (XReq_Format*, long);
   ---  void XReq_Format_List_Feature  (XReq_Format*, XReq_Cstr);
   ---  void XReq_Format_List_Scenario (XReq_Format*, XReq_Cstr, XReq_Cstr,
   ---                                  long);
   ---  void XReq_Format_Free          (XReq_Format*);
   procedure XReq_Format_Set_Num_Steps (Format : in XReq_Format_Ptr;
                                        Num    : in long);
   procedure XReq_Format_List_Feature  (Format : in XReq_Format_Ptr;
                                        A      : in XReq_Cstr);
   procedure XReq_Format_List_Scenario (Format : in XReq_Format_Ptr;
                                        A, B   : in XReq_Cstr;
                                        C      : in long);
   procedure XReq_Format_Free          (Format : in XReq_Format_Ptr);
   ---
   ---  void XReq_Format_Start_Tests   (XReq_Format*);
   ---  void XReq_Format_Put_Summary   (XReq_Format*, XReq_Report*,
   ---                                  XReq_Duration);
   ---  void XReq_Format_Stop_Tests    (XReq_Format*);
   procedure XReq_Format_Start_Tests   (Format : in XReq_Format_Ptr);
   procedure XReq_Format_Put_Summary   (Format : in XReq_Format_Ptr;
                                        Report : in XReq_Report_Ptr;
                                        Time   : in XReq_Duration);
   procedure XReq_Format_Stop_Tests    (Format : in XReq_Format_Ptr);
   ---
   ---  void XReq_Format_Start_Feature (XReq_Format*);
   ---  void XReq_Format_Put_Feature   (XReq_Format*, XReq_Cstr, XReq_Cstr,
   ---                                  XReq_Cstr);
   ---  void XReq_Format_Stop_Feature  (XReq_Format*);
   procedure XReq_Format_Start_Feature (Format  : in XReq_Format_Ptr);
   procedure XReq_Format_Put_Feature   (Format  : in XReq_Format_Ptr;
                                        A, B, C : in XReq_Cstr);
   procedure XReq_Format_Stop_Feature  (Format  : in XReq_Format_Ptr);
   ---
   ---  void XReq_Format_Start_Background     (XReq_Format*, XReq_Bool);
   ---  void XReq_Format_Put_Background       (XReq_Format*, XReq_Cstr,
   ---                                         XReq_Cstr, XReq_Tags);
   ---  void XReq_Format_Stop_Background      (XReq_Format*, XReq_Bool);
   procedure XReq_Format_Start_Background     (Format : in XReq_Format_Ptr;
                                               First  : in XReq_Bool);
   procedure XReq_Format_Put_Background       (Format : in XReq_Format_Ptr;
                                               A, B   : in XReq_Cstr;
                                               Tags   : in XReq_Tags);
   procedure XReq_Format_Stop_Background      (Format : in XReq_Format_Ptr;
                                               First  : in XReq_Bool);
   ---
   ---  void XReq_Format_Enter_Outline        (XReq_Format*);
   ---  void XReq_Format_Start_Outline        (XReq_Format*);
   ---  void XReq_Format_Put_Outline          (XReq_Format*, XReq_Cstr,
   ---                                         XReq_Cstr, XReq_Tags);
   ---  void XReq_Format_Put_Outline_Report   (XReq_Format*, XReq_Table*);
   ---  void XReq_Format_Stop_Outline         (XReq_Format*);
   procedure XReq_Format_Enter_Outline        (Format : in XReq_Format_Ptr);
   procedure XReq_Format_Start_Outline        (Format : in XReq_Format_Ptr);
   procedure XReq_Format_Put_Outline          (Format : in XReq_Format_Ptr;
                                               A, B   : in XReq_Cstr;
                                               Tags   : in XReq_Tags);
   procedure XReq_Format_Put_Outline_Report   (Format : in XReq_Format_Ptr;
                                               Table  : in XReq_Table_Ptr);
   procedure XReq_Format_Stop_Outline         (Format : in XReq_Format_Ptr);
   ---
   ---  void XReq_Format_Enter_Scenario       (XReq_Format*);
   ---  void XReq_Format_Start_Scenario       (XReq_Format*);
   ---  void XReq_Format_Put_Scenario         (XReq_Format*, XReq_Cstr,
   ---                                         XReq_Cstr, XReq_Tags);
   ---  void XReq_Format_Put_Scenario_Outline (XReq_Format*, long, XReq_Cstr,
   ---                                         XReq_Cstr, XReq_Tags);
   ---  void XReq_Format_Stop_Scenario        (XReq_Format*);
   procedure XReq_Format_Enter_Scenario       (Format : in XReq_Format_Ptr);
   procedure XReq_Format_Start_Scenario       (Format : in XReq_Format_Ptr);
   procedure XReq_Format_Put_Scenario         (Format : in XReq_Format_Ptr;
                                               A, B   : in XReq_Cstr;
                                               Tags   : in XReq_Tags);
   procedure XReq_Format_Put_Scenario_Outline (Format : in XReq_Format_Ptr;
                                               Num    : in long;
                                               A, B   : in XReq_Cstr;
                                               Tags   : in XReq_Tags);
   procedure XReq_Format_Stop_Scenario        (Format : in XReq_Format_Ptr);
   ---
   ---  void XReq_Format_Start_Step (XReq_Format*);
   ---  void XReq_Format_Put_Step   (XReq_Format*, XReq_Kind, XReq_Cstr,
   ---                               XReq_Cstr, XReq_Args*, XReq_Status);
   ---  void XReq_Format_Put_Error  (XReq_Format*, XReq_Error*);
   ---  void XReq_Format_Stop_Step  (XReq_Format*);
   procedure XReq_Format_Start_Step (Format : in XReq_Format_Ptr);
   procedure XReq_Format_Put_Step   (Format : in XReq_Format_Ptr;
                                     Kind   : in XReq_Kind;
                                     A, B   : in XReq_Cstr;
                                     Args   : in XReq_Args_Ptr;
                                     Status : in XReq_Status);
   procedure XReq_Format_Put_Error  (Format : in XReq_Format_Ptr;
                                     Error  : in XReq_Error_Ptr);
   procedure XReq_Format_Stop_Step  (Format : in XReq_Format_Ptr);
   ---
   ---  XReq_Conditional* XReq_Conditional_New  ();
   ---  void      XReq_Conditional_Free         (XReq_Conditional*);
   ---  XReq_Bool XReq_Conditional_Eval_Tags    (XReq_Conditional*, XReq_Tags);
   ---  XReq_Bool XReq_Conditional_Eval_Position(XReq_Conditional*, XReq_Cstr,
   ---                                           long);
   function  XReq_Conditional_New               return XReq_Conditional_Ptr;
   procedure XReq_Conditional_Free          (Cond : in XReq_Conditional_Ptr);
   function  XReq_Conditional_Eval_Tags     (Cond : in XReq_Conditional_Ptr;
                                             Tags : in XReq_Tags)
                                               return XReq_Bool;
   function  XReq_Conditional_Eval_Position (Cond : in XReq_Conditional_Ptr;
                                             Pos  : in XReq_Cstr;
                                             N    : in long)
                                                return XReq_Bool;
   ---
   ---  XReq_Report* XReq_Report_New   ();
   ---  void XReq_Report_step_Free     (XReq_Report*);
   ---  void XReq_Report_step_skip     (XReq_Report*);
   ---  void XReq_Report_step_pass     (XReq_Report*);
   ---  void XReq_Report_step_fail     (XReq_Report*);
   ---  void XReq_Report_scenario_pass (XReq_Report*);
   ---  void XReq_Report_scenario_fail (XReq_Report*);
   ---  void XReq_Report_num_steps_inc (XReq_Report*, long);
   ---  long XReq_Report_get_num_steps (XReq_Report*);
   ---  XReq_Bool XReq_Report_Status   (XReq_Report*);
   function  XReq_Report_New                 return XReq_Report_Ptr;
   procedure XReq_Report_step_Free     (Report : in XReq_Report_Ptr);
   procedure XReq_Report_step_skip     (Report : in XReq_Report_Ptr);
   procedure XReq_Report_step_pass     (Report : in XReq_Report_Ptr);
   procedure XReq_Report_step_fail     (Report : in XReq_Report_Ptr);
   procedure XReq_Report_scenario_pass (Report : in XReq_Report_Ptr);
   procedure XReq_Report_scenario_fail (Report : in XReq_Report_Ptr);
   procedure XReq_Report_num_steps_inc (Report : in XReq_Report_Ptr;
                                        N      : in long);
   function  XReq_Report_get_num_steps (Report : in XReq_Report_Ptr)
                                             return long;
   function  XReq_Report_Status        (Report : in XReq_Report_Ptr)
                                             return XReq_Bool;

   --  void XReq_CLI_Parse_Arguments   (long, char**, XReq_Format**,
   --                                   XReq_Bool*, XReq_Conditional**,
   --                                   XReq_Bool*, XReq_Cstr);
   procedure XReq_CLI_Parse_Arguments  (argc : long; argv : chars_ptr_array;
                                        Format     : access XReq_Format_Ptr;
                                        Continue   : access XReq_Bool;
                                        Cond       : in XReq_Conditional_Ptr;
                                        List_Mode  : access XReq_Bool;
                                        Name       : in     XReq_Cstr);



   pragma Export (C, XReq_Time_Start,            "XReq_Time_Start");
   pragma Export (C, XReq_Time_Stop,             "XReq_Time_Stop");
   pragma Export (C, XReq_Format_Set_Num_Steps,  "XReq_Format_Set_Num_Steps");
   pragma Export (C, XReq_Format_List_Feature,   "XReq_Format_List_Feature");
   pragma Export (C, XReq_Format_List_Scenario,  "XReq_Format_List_Scenario");
   pragma Export (C, XReq_Format_Free,           "XReq_Format_Free");
   pragma Export (C, XReq_Format_Start_Tests,    "XReq_Format_Start_Tests");
   pragma Export (C, XReq_Format_Put_Summary,    "XReq_Format_Put_Summary");
   pragma Export (C, XReq_Format_Stop_Tests,     "XReq_Format_Stop_Tests");
   pragma Export (C, XReq_Format_Start_Feature,  "XReq_Format_Start_Feature");
   pragma Export (C, XReq_Format_Put_Feature,    "XReq_Format_Put_Feature");
   pragma Export (C, XReq_Format_Stop_Feature,   "XReq_Format_Stop_Feature");
   pragma Export (C, XReq_Format_Start_Background,
                    "XReq_Format_Start_Background");
   pragma Export (C, XReq_Format_Put_Background,
                    "XReq_Format_Put_Background");
   pragma Export (C, XReq_Format_Stop_Background,
                    "XReq_Format_Stop_Background");
   pragma Export (C, XReq_Format_Enter_Outline,  "XReq_Format_Enter_Outline");
   pragma Export (C, XReq_Format_Start_Outline,  "XReq_Format_Start_Outline");
   pragma Export (C, XReq_Format_Put_Outline,    "XReq_Format_Put_Outline");
   pragma Export (C, XReq_Format_Put_Outline_Report,
                    "XReq_Format_Put_Outline_Report");
   pragma Export (C, XReq_Format_Stop_Outline,   "XReq_Format_Stop_Outline");
   pragma Export (C, XReq_Format_Enter_Scenario, "XReq_Format_Enter_Scenario");
   pragma Export (C, XReq_Format_Start_Scenario, "XReq_Format_Start_Scenario");
   pragma Export (C, XReq_Format_Put_Scenario,   "XReq_Format_Put_Scenario");
   pragma Export (C, XReq_Format_Put_Scenario_Outline,
                    "XReq_Format_Put_Scenario_Outline");
   pragma Export (C, XReq_Format_Stop_Scenario,  "XReq_Format_Stop_Scenario");
   pragma Export (C, XReq_Format_Start_Step,     "XReq_Format_Start_Step");
   pragma Export (C, XReq_Format_Put_Step,       "XReq_Format_Put_Step");
   pragma Export (C, XReq_Format_Put_Error,      "XReq_Format_Put_Error");
   pragma Export (C, XReq_Format_Stop_Step,      "XReq_Format_Stop_Step");
   pragma Export (C, XReq_Conditional_New,       "XReq_Conditional_New");
   pragma Export (C, XReq_Conditional_Free,      "XReq_Conditional_Free");
   pragma Export (C, XReq_Conditional_Eval_Tags, "XReq_Conditional_Eval_Tags");
   pragma Export (C, XReq_Conditional_Eval_Position,
                    "XReq_Conditional_Eval_Position");
   pragma Export (C, XReq_Report_New,            "XReq_Report_New");
   pragma Export (C, XReq_Report_step_Free,      "XReq_Report_step_Free");
   pragma Export (C, XReq_Report_step_skip,      "XReq_Report_step_skip");
   pragma Export (C, XReq_Report_step_pass,      "XReq_Report_step_pass");
   pragma Export (C, XReq_Report_step_fail,      "XReq_Report_step_fail");
   pragma Export (C, XReq_Report_scenario_pass,  "XReq_Report_scenario_pass");
   pragma Export (C, XReq_Report_scenario_fail,  "XReq_Report_scenario_fail");
   pragma Export (C, XReq_Report_num_steps_inc,  "XReq_Report_num_steps_inc");
   pragma Export (C, XReq_Report_get_num_steps,  "XReq_Report_get_num_steps");
   pragma Export (C, XReq_Report_Status,         "XReq_Report_Status");
   pragma Export (C, XReq_CLI_Parse_Arguments,   "XReq_CLI_Parse_Arguments");

   pragma Warnings (On);

end XReqLib.C_Interface;
