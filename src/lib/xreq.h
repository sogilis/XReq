#include <stdlib.h>

#ifndef XREQ_H
#define XREQ_H

struct XReq_Format;
typedef struct XReq_Format XReq_Format;

struct XReq_Conditional;
typedef struct XReq_Conditional XReq_Conditional;

struct XReq_Report;
typedef struct XReq_Report XReq_Report;

struct XReq_Args;
typedef struct XReq_Args XReq_Args;

struct XReq_Table;
typedef struct XReq_Table XReq_Table;

struct XReq_Error;
typedef struct XReq_Error XReq_Error;

typedef unsigned short XReq_Status;
typedef unsigned long  XReq_Duration;
typedef const char*    XReq_Cstr;
typedef short          XReq_Bool;
typedef XReq_Cstr*     XReq_Tags;

void XReq_Time_Start (XReq_Duration*);
void XReq_Time_Stop  (XReq_Duration*);

void XReq_Format_Set_Num_Tests (XReq_Format*, long);
void XReq_Format_List_Feature  (XReq_Format*, XReq_Cstr);
void XReq_Format_List_Scenario (XReq_Format*, XReq_Cstr, XReq_Cstr, long);
void XReq_Format_Free          (XReq_Format*);

void XReq_Format_Start_Tests   (XReq_Format*);
void XReq_Format_Put_Summary   (XReq_Format*, XReq_Report*, XReq_Duration);
void XReq_Format_Stop_Tests    (XReq_Format*);

void XReq_Format_Start_Feature (XReq_Format*);
void XReq_Format_Put_Feature   (XReq_Format*, XReq_Cstr, XReq_Cstr, XReq_Cstr);
void XReq_Format_Stop_Feature  (XReq_Format*);

void XReq_Format_Start_Background     (XReq_Format*, XReq_Bool);
void XReq_Format_Put_Background       (XReq_Format*, XReq_Cstr, XReq_Cstr, XReq_Tags);
void XReq_Format_Stop_Background      (XReq_Format*, XReq_Bool);

void XReq_Format_Enter_Outline        (XReq_Format*);
void XReq_Format_Start_Outline        (XReq_Format*);
void XReq_Format_Put_Outline          (XReq_Format*, XReq_Cstr, XReq_Cstr, XReq_Tags);
void XReq_Format_Put_Outline_Report   (XReq_Format*, XReq_Table*);
void XReq_Format_Stop_Outline         (XReq_Format*);

void XReq_Format_Enter_Scenario       (XReq_Format*);
void XReq_Format_Start_Scenario       (XReq_Format*);
void XReq_Format_Put_Scenario         (XReq_Format*, XReq_Cstr, XReq_Cstr, XReq_Tags);
void XReq_Format_Put_Scenario_Outline (XReq_Format*, long, XReq_Cstr, XReq_Cstr, XReq_Tags);
void XReq_Format_Stop_Scenario        (XReq_Format*);

void XReq_Format_Start_Step (XReq_Format*);
void XReq_Format_Put_Step   (XReq_Format*, XReq_Cstr, XReq_Cstr, XReq_Args*, XReq_Status);
void XReq_Format_Put_Error  (XReq_Format*, XReq_Error*);
void XReq_Format_Stop_Step  (XReq_Format*);

XReq_Conditional* XReq_Conditional_New  ();
void      XReq_Conditional_Free         (XReq_Conditional*);
XReq_Bool XReq_Conditional_Eval_Tags    (XReq_Conditional*, XReq_Tags);
XReq_Bool XReq_Conditional_Eval_Position(XReq_Conditional*, XReq_Cstr, long);

XReq_Report* XReq_Report_New   ();
void XReq_Report_step_Free     (XReq_Report*);
void XReq_Report_step_skip     (XReq_Report*);
void XReq_Report_step_pass     (XReq_Report*);
void XReq_Report_step_fail     (XReq_Report*);
void XReq_Report_scenario_pass (XReq_Report*);
void XReq_Report_scenario_fail (XReq_Report*);
void XReq_Report_num_steps_inc (XReq_Report*, long);
long XReq_Report_get_num_steps (XReq_Report*);
XReq_Bool XReq_Report_Status   (XReq_Report*);

void XReq_CLI_Parse_Arguments   (long, char**, XReq_Format**, XReq_Bool*, XReq_Conditional**, XReq_Bool*, XReq_Cstr);

#endif
