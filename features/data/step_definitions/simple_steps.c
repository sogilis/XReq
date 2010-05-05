#include <stdio.h>
#include <string.h>
#include "simple_steps.h"

XREQ_STEP (Given_this_step_works)
{
}

XREQ_STEP(Given_this_step_works_loudly)
{
  XReq_Args_Add_Para (XREQ_ARG, "Debug text");
  printf("This step works\n");
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
  XREQ_ASSERT(0, "Assertion failed");
}

XREQ_STEP(Given_this_fails_periodically)
{
  static int state = 0;
  state = !state;
  char assert_message[1024];
  snprintf(assert_message, 1024, "State is %s (should be TRUE)", state ? "TRUE":"FALSE");
  XREQ_ASSERT (state, assert_message);
  printf ("State is %s OK\n", state ? "TRUE":"FALSE");
}