#include <string.h>
#include "strings.h"

static char* result = NULL;
static char* string1 = NULL;
static char* string2 = NULL;

XREQ_STEP(When_I_concatenate_and)
{
  const char* str1 = XReq_Args_Match (XREQ_ARG, 1);
  const char* str2 = XReq_Args_Match (XREQ_ARG, 2);
  if(result) free(result);
  size_t len = strlen(str1) + strlen(str2) + 1;
  result = malloc (len);
  strncpy(result, str1, len);
  strncat(result, str2, len);
  XReq_String_Free (str1);
  XReq_String_Free (str2);
}

XREQ_STEP(Then_I_Get)
{
  const char* str1 = XReq_Args_Match (XREQ_ARG, 1);
  XREQ_ASSERT_GOTO(!strcmp(result, str1), end,"Strings should be equal");
end:
  XReq_String_Free (str1);
}

XREQ_STEP(Given_the_long_string)
{
  if(string1) XReq_String_Free (string1);
  string1 = XReq_Args_Text (XREQ_ARG, 0);
}

XREQ_STEP(When_I_compare_it_with)
{
  const char* str1 = XReq_Args_Match (XREQ_ARG, 1);
  size_t len = strlen(str1);
  string2 = malloc (len+1);
  string2[len] = 0;
  int i = 0, j = 0;
  for (i = 0; i < len; ++i) {
    if (str1[i] == '\\' && i + 1 < len) {
      ++i;
      switch (str1[i]) {
        case 'n':
          string2[j++] = '\n';
          break;
        case '\\':
          string2[j++] = '\\';
          break;
        default:
          string2[j++] = '\\';
          string2[j++] = str1[i];
          break;
      }
    } else {
      string2[j++] = str1[i];
    }
  }
  string2[j] = 0;
  XReq_String_Free (str1);
}

XREQ_STEP(Then_they_are_equal)
{
  XREQ_ASSERT_GOTO(!strcmp(string1, string2), end,"Strings should be equal");
end:
  XReq_String_Free (string1); string1 = NULL;
  XReq_String_Free (string2); string2 = NULL;
}
