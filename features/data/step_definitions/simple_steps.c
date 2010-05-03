#include "simple_steps.h"

XREQ_STEP (Given_this_step_works)
{
}

XREQ_STEP (Given_this_step_doesn_t_work)
{
  XREQ_FAIL ("Doesn't work");
}
