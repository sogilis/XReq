#include <stdio.h>
#include <string.h>
#include "simple_steps.h"

XREQ_STEP (Given_this_step_works)
{
}

XREQ_STEP (Given_this_step_works_with)
{
  const char* match_1 = XReq_Args_Match(XREQ_ARG, 1);
  size_t len = 1024 + strlen(match_1);
  char string[len];
  snprintf(string, len, "Debug text for working step %s", match_1);
  XReq_Args_Add_Para (XREQ_ARG, string);
  printf("This step works %s\n", match_1);
  XReq_String_Free (match_1);
}

XREQ_STEP (Given_this_step_doesn_t_work)
{
  XREQ_FAIL_NULL;
}
