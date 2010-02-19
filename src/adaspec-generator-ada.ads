--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Util.Strings;
with Util.Strings.Pool;
with AdaSpec.Result;

use Ada.Strings.Unbounded;
use Util.Strings;
use Util.Strings.Pool;
use AdaSpec.Result;

package AdaSpec.Generator.Ada is

   procedure Generate (Job : in Job_Type;
                       Env : in Job_Environment);

private

   type Generator_State is tagged
      record
         --  BEGIN_GCOV_IGNORE
         Feature    : Result_Feature_Type;
         Adb_Buf    : Unbounded_String;
         Ads_Buf    : Unbounded_String;
         Pool       : String_Pool;
         Ind_Ads    : Unbounded_String;
         Ind_Adb    : Unbounded_String;
         CRLF       : Unbounded_String := To_Unbounded_String ("" & ASCII.LF);
         Fn_Backgnd : Unbounded_String;
         Id_Pkgname : Unbounded_String;
         With_Pkg   : String_Set.Set;
         --  END_GCOV_IGNORE
      end record;

   procedure Adb_Line (State : in out Generator_State; Line : in String);
--    procedure Ads_Line (State : in out Generator_State; Line : in String);
--    procedure Adb (State : in out Generator_State; S : in String);
--    procedure Ads (State : in out Generator_State; S : in String);

   procedure Adb_Line (State : in out Generator_State;
                       Line  : in Unbounded_String);
   procedure Ads_Line (State : in out Generator_State;
                       Line  : in Unbounded_String);
--    procedure Adb (State : in out Generator_State; S : in Unbounded_String);
--    procedure Ads (State : in out Generator_State; S : in Unbounded_String);

   procedure Indent_Ads   (State : in out Generator_State;
                           N     : in Positive := 3);
   procedure Indent_Adb   (State : in out Generator_State;
                           N     : in Positive := 3);
   procedure Unindent_Ads (State : in out Generator_State;
                           N     : in Positive := 3);
   procedure Unindent_Adb (State : in out Generator_State;
                           N     : in Positive := 3);
   procedure Indent       (State : in out Generator_State;
                           N     : in Positive := 3);
   procedure Unindent     (State : in out Generator_State;
                           N     : in Positive := 3);

end AdaSpec.Generator.Ada;
