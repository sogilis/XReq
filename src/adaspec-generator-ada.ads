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

   type Ada_Generator_Type is new Generator_Type with private;
   type Ada_Generator_Ptr is access all Ada_Generator_Type'Class;

   procedure Make     (Gen : out    Ada_Generator_Type;
                       Job : in     Job_Type;
                       Env : in     Job_Environment);

   procedure Generate (Gen : in out Ada_Generator_Type);

private

   type Ada_Generator_Type is new Generator_Type with
      record
         Feature    : Result_Feature_Type;
         Ads_File   : Unbounded_String;
         Adb_File   : Unbounded_String;
         Adb_Buf    : Unbounded_String;
         Ads_Buf    : Unbounded_String;
         Pool       : String_Pool;
         Ind_Ads    : Unbounded_String;
         Ind_Adb    : Unbounded_String;
         CRLF       : Unbounded_String := To_Unbounded_String ("" & ASCII.LF);
         Fn_Backgnd : Unbounded_String;
         Id_Pkgname : Unbounded_String;
         With_Pkg   : String_Set.Set;
         Fn_Steps   : Util.Strings.Vectors.Vector;
      end record;

   procedure Adb_Line (State : in out Ada_Generator_Type; Line : in String);
   procedure Ads_Line (State : in out Ada_Generator_Type; Line : in String);
--    procedure Adb (State : in out Ada_Generator_Type;
--                  S      : in     String);
--    procedure Ads (State : in out Ada_Generator_Type;
--                   S     : in     String);

   procedure Adb_Line (State : in out Ada_Generator_Type;
                       Line  : in     Unbounded_String);
   procedure Ads_Line (State : in out Ada_Generator_Type;
                       Line  : in     Unbounded_String);
--    procedure Adb (State : in out Ada_Generator_Type;
--                   S     : in Unbounded_String);
--    procedure Ads (State : in out Ada_Generator_Type;
--                   S     : in Unbounded_String);

   procedure Indent_Ads   (State : in out Ada_Generator_Type;
                           N     : in     Positive := 3);
   procedure Indent_Adb   (State : in out Ada_Generator_Type;
                           N     : in     Positive := 3);
   procedure Unindent_Ads (State : in out Ada_Generator_Type;
                           N     : in     Positive := 3);
   procedure Unindent_Adb (State : in out Ada_Generator_Type;
                           N     : in     Positive := 3);
   procedure Indent       (State : in out Ada_Generator_Type;
                           N     : in     Positive := 3);
   procedure Unindent     (State : in out Ada_Generator_Type;
                           N     : in     Positive := 3);

end AdaSpec.Generator.Ada;
