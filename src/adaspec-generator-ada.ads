--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Util.Strings.Pool;
with AdaSpec.Result;

use Ada.Strings.Unbounded;
use Util.Strings.Pool;
use AdaSpec.Result;

package AdaSpec.Generator.Ada is

   procedure Generate_Step     (Buffer   : in out Unbounded_String;
                                Pool     : in out String_Pool;
                                Step     : in Result_Step_Type;
                                Indent   : in String := "";
                                CRLF     : in String := ASCII.CR & ASCII.LF);

   procedure Generate_Scenario (Buffer   : in out Unbounded_String;
                                Pool     : in out String_Pool;
                                Scenario : in Result_Scenario_Type;
                                Indent   : in String := "";
                                CRLF     : in String := ASCII.CR & ASCII.LF);

   procedure Generate_Feature  (Buffer   : in out Unbounded_String;
                                Pool     : in out String_Pool;
                                Feature  : in Result_Feature_Type;
                                Indent   : in String := "";
                                CRLF     : in String := ASCII.CR & ASCII.LF);

end AdaSpec.Generator.Ada;
