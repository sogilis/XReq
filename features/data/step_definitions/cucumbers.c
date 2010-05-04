#include "cucumbers.h"

static int cukes = 0;

XREQ_STEP(Given_there_are_n_cucumbers)
{
  const char* match = XReq_Args_Match(XREQ_ARG, 1);
  cukes = atoi(match);
  XReq_String_Free (match);
}

XREQ_STEP(When_i_eat_n_cucumbers)
{
  const char* match = XReq_Args_Match(XREQ_ARG, 1);
  cukes -= atoi(match);
  XReq_String_Free (match);
}

XREQ_STEP(Then_i_should_have_n_cucumbers)
{
  const char* match = XReq_Args_Match(XREQ_ARG, 1);
  XREQ_ASSERT_GOTO (cukes == atoi(match), finalize, "Assertion failed");
finalize:
  XReq_String_Free (match);
}
