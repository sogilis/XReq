
#include "tables.h"

static XReq_Table* tble = NULL;

XREQ_STEP(Given_a_table)
{
  if(tble) XReq_Table_Free (tble);
  tble = XReq_Args_Table (XREQ_ARG, 0);
}


XREQ_STEP(Then_the_table_should_be_equal_to)
{
  XREQ_ASSERT(XReq_Table_Equals(tble, XReq_Args_Table(XREQ_ARG, 0)),
              "Assertion failed");
}
