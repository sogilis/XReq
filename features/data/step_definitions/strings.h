#include <xreq.h>

// @when ^I concatenate "(.*)" and "(.*)"$
XREQ_STEP(When_I_concatenate_and);

// @then ^I get "(.*)"$
XREQ_STEP(Then_I_Get);

// @given ^the long string:$
XREQ_STEP(Given_the_long_string);

// @when ^I compare it with "(.*)"$
XREQ_STEP(When_I_compare_it_with);

// @then ^they are equal$
XREQ_STEP(Then_they_are_equal);
