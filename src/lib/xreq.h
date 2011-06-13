#include <stdlib.h>

#ifndef XREQ_H
#define XREQ_H

#define XREQ_GIVEN(PAT)
#define XREQ_WHEN(PAT)
#define XREQ_THEN(PAT)
#define XREQ_STEP_TODO
#define XREQ_STEP(function_name) \
  void XReq_Step__##function_name (XReq_Args *__xreq_args, XReq_Error *__xreq_err)

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
typedef unsigned short XReq_Kind;
typedef unsigned long  XReq_Duration;
typedef char*          XReq_Cstr;
typedef short          XReq_Bool;
typedef XReq_Cstr*     XReq_Tags;

#define XReq_Status_Passed  0
#define XReq_Status_Skipped 1
#define XReq_Status_Failed  2
#define XReq_Status_Outline 3

#define XReq_Kind_Null  0
#define XReq_Kind_Given 1
#define XReq_Kind_When  2
#define XReq_Kind_Then  3

void XReq_Time_Start (XReq_Duration*);
void XReq_Time_Stop  (XReq_Duration*);

void XReq_Format_Set_Num_Steps (XReq_Format*, long);
void XReq_Format_List_Feature  (XReq_Format*, const XReq_Cstr);
void XReq_Format_List_Scenario (XReq_Format*, const XReq_Cstr, const XReq_Cstr, long, long);
void XReq_Format_Free          (XReq_Format*);

void XReq_Format_Start_Tests   (XReq_Format*);
void XReq_Format_Put_Summary   (XReq_Format*, XReq_Report*, XReq_Duration);
void XReq_Format_Stop_Tests    (XReq_Format*);

void XReq_Format_Start_Feature (XReq_Format*);
void XReq_Format_Put_Feature   (XReq_Format*, const XReq_Cstr, const XReq_Cstr, const XReq_Cstr);
void XReq_Format_Stop_Feature  (XReq_Format*);

void XReq_Format_Start_Background     (XReq_Format*, XReq_Bool);
void XReq_Format_Put_Background       (XReq_Format*, const XReq_Cstr, const XReq_Cstr, XReq_Tags);
void XReq_Format_Stop_Background      (XReq_Format*, XReq_Bool);

void XReq_Format_Enter_Outline        (XReq_Format*);
void XReq_Format_Start_Outline        (XReq_Format*);
void XReq_Format_Put_Outline          (XReq_Format*, const XReq_Cstr, const XReq_Cstr, XReq_Tags);
void XReq_Format_Put_Outline_Report   (XReq_Format*, XReq_Table*);
void XReq_Format_Stop_Outline         (XReq_Format*);

void XReq_Format_Enter_Scenario       (XReq_Format*);
void XReq_Format_Start_Scenario       (XReq_Format*);
void XReq_Format_Put_Scenario         (XReq_Format*, const XReq_Cstr, const XReq_Cstr, XReq_Tags);
void XReq_Format_Put_Scenario_Outline (XReq_Format*, long, const XReq_Cstr, const XReq_Cstr, XReq_Tags);
void XReq_Format_Stop_Scenario        (XReq_Format*);

void XReq_Format_Start_Step (XReq_Format*);
void XReq_Format_Put_Step   (XReq_Format*, XReq_Kind, const XReq_Cstr, const XReq_Cstr, XReq_Args*, XReq_Status);
void XReq_Format_Put_Error  (XReq_Format*, XReq_Error*);
void XReq_Format_Stop_Step  (XReq_Format*);

void XReq_Formet_STR_Feature  (XReq_Format*, const XReq_Cstr);
void XReq_Formet_STR_Scenario (XReq_Format*, const XReq_Cstr);
void XReq_Formet_STR_Outline  (XReq_Format*, const XReq_Cstr);

XReq_Conditional* XReq_Conditional_New  ();
void      XReq_Conditional_Free         (XReq_Conditional*);
XReq_Bool XReq_Conditional_Eval_Tags    (XReq_Conditional*, XReq_Tags);
XReq_Bool XReq_Conditional_Eval_Position(XReq_Conditional*, const XReq_Cstr, long, long);

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

XReq_Bool XReq_CLI_Parse_Arguments (long, char**, XReq_Format**, XReq_Bool*, XReq_Conditional*, XReq_Bool*, const XReq_Cstr);

XReq_Args*  XReq_Args_New  ();
XReq_Table* XReq_Table_New ();
XReq_Error* XReq_Error_New ();

void XReq_Args_Make          (XReq_Args*, const XReq_Cstr);
void XReq_Args_Add_Match     (XReq_Args*, long, long);
void XReq_Args_Add_Sep       (XReq_Args*, long);
void XReq_Args_Add_Para      (XReq_Args*, const XReq_Cstr);
void XReq_Args_Add_Text      (XReq_Args*, const XReq_Cstr);
void XReq_Args_Add_Table     (XReq_Args*, XReq_Table*);
XReq_Cstr   XReq_Args_Match  (XReq_Args*, long);
XReq_Cstr   XReq_Args_Text   (XReq_Args*, long);
XReq_Table* XReq_Args_Table  (XReq_Args*, long);
void        XReq_Args_Free   (XReq_Args*);

void XReq_Table_Put          (XReq_Table*, long, long, const XReq_Cstr);
XReq_Bool XReq_Table_Equals  (XReq_Table*, XReq_Table*);
void XReq_Table_Free         (XReq_Table*);

void XReq_Error_Clear        (XReq_Error*);
void XReq_Error_Make         (XReq_Error*, const XReq_Cstr, const XReq_Cstr, long);
XReq_Bool XReq_Error_Is_Null (XReq_Error*);
void XReq_Error_Free         (XReq_Error*);

void XReq_String_Free        (const char*);

#define XREQ_FAIL(message) XReq_Error_Make(__xreq_err, (message), __FILE__, __LINE__); return;
#define XREQ_FAIL_NULL XReq_Error_Make(__xreq_err, "", __FILE__, __LINE__); return;
#define XREQ_FAIL_GOTO(message,label) XReq_Error_Make(__xreq_err, (message), __FILE__, __LINE__); goto label;
#define XREQ_FAIL_NULL_GOTO(label) XReq_Error_Make(__xreq_err, "", __FILE__, __LINE__); goto label;

#define XREQ_ASSERT(condition,message) if(!(condition)){XREQ_FAIL(message)}
#define XREQ_ASSERT_GOTO(condition,label,message) if(!(condition)){XREQ_FAIL_GOTO(message,label)}

#define XREQ_ARG (__xreq_args)

#endif
