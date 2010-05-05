#include <xreq.h>

#ifndef SIMPLE_STEPS_H
#define SIMPLE_STEPS_H

// @given ^this( step)? is ambiguous$
// @given ^this( step)? is( too)? ambiguous$
// @given ^this( step)? is ambiguous too$

// @given ^this is ignored$
// @given ^I match \"([^\"]*)\" and \"([^\"]*)\"$
// @then ^do nothing$

// @given ^this step works$

XREQ_STEP (Given_this_step_works);


// @given ^this step works with (.*)$
XREQ_STEP (Given_this_step_works_with);


// @given ^this step works loudly$
XREQ_STEP(Given_this_step_works_loudly);

// @given ^this step doesn't work$
// @given ^it fails$
// @when  ^it fails$
XREQ_STEP (Given_this_step_doesn_t_work);


// @given ^this fails periodically$
XREQ_STEP(Given_this_fails_periodically);

#endif
