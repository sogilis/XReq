#include <xreq.h>

#ifndef SIMPLE_STEPS_H
#define SIMPLE_STEPS_H

XREQ_GIVEN("^this( step)? is ambiguous$")
XREQ_GIVEN("^this( step)? is( too)? ambiguous$")
XREQ_GIVEN("^this( step)? is ambiguous too$")

XREQ_GIVEN("^this is ignored$")
XREQ_GIVEN("^do nothing$")
XREQ_GIVEN("^I match \"([^\"]*)\" and \"([^\"]*)\"$")

XREQ_GIVEN("^this step works$")

XREQ_STEP (Given_this_step_works);



// @given ^this step works with (.*)$

XREQ_STEP (Given_this_step_works_with);



XREQ_GIVEN("^this step doesn't work$")
XREQ_GIVEN("^it fails$")
XREQ_WHEN ("^it fails$")
XREQ_STEP (Given_this_step_doesn_t_work);

#endif
