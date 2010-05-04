#include <xreq.h>


// @given ^there are ([0-9]+) cucumbers$
XREQ_STEP(Given_there_are_n_cucumbers);

// @when ^I eat ([0-9]+) cucumbers$
XREQ_STEP(When_i_eat_n_cucumbers);

// @then ^I should have ([0-9]+) cucumbers$
XREQ_STEP(Then_i_should_have_n_cucumbers);
