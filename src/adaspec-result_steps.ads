--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with AdaSpecLib;
with Util.IO;
with AdaSpec.Steps;
with AdaSpec.Step_Definitions;

use Ada.Strings.Unbounded;
use AdaSpecLib;
use Util.IO;
use AdaSpec.Steps;
use AdaSpec.Step_Definitions;

package AdaSpec.Result_Steps is

   ------------------------
   --  Result_Step_Type  --
   ------------------------

   --  A procedure name of a step definition and its arguments

   type Result_Step_Type is new Step_Type with private;

   function New_Step     (Kind     : in Step_Kind;
                          Stanza   : in String;
                          Position : in Position_Type) return Result_Step_Type;
   function Stanza_Given (S        : in String;
                          File     : in String := "";
                          Line     : in Natural := 0)  return Result_Step_Type;
   function Stanza_When  (S        : in String;
                          File     : in String := "";
                          Line     : in Natural := 0)  return Result_Step_Type;
   function Stanza_Then  (S        : in String;
                          File     : in String := "";
                          Line     : in Natural := 0)  return Result_Step_Type;

   function New_Result_Step (Step           : in  Step_Type;
                             Procedure_Name : in  String := "";
                             Matches        : in  Match_Vectors.Vector
                                            := Match_Vectors.Empty_Vector)
                                              return Result_Step_Type;


   function Procedure_Name (S      : in Result_Step_Type) return String;
   function To_String      (S      : in Result_Step_Type;
                            Indent : in String := "") return String;
   function To_Code        (S      : in Result_Step_Type;
                            Indent : in String := "") return String
                            renames To_String;

   procedure Set_Procedure_Name (S   : in out Result_Step_Type;
                                 Prc : in     String);

   function Match_First   (S : in Result_Step_Type) return Natural;
   function Match_Last    (S : in Result_Step_Type) return Integer;
   function Match_Count   (S : in Result_Step_Type) return Natural;
   function Match_Element (S : in Result_Step_Type;
                           I : in Natural)          return Match_Location;

   procedure Process_Step     (Res      : out Result_Step_Type;
                               Stanza   : in  Step_Type;
                               Steps    : in  Step_Definitions_Type;
                               Log      : in  Logger_Ptr;
                               Errors   : out Boolean;
                               Step_Matching : in Boolean);

   function Equals (Left, Right : in Result_Step_Type) return Boolean;

private

   type Result_Step_Type is new Step_Type with
      record
         Procedure_Name : Unbounded_String;
         Matches        : Match_Vectors.Vector;
      end record;

end AdaSpec.Result_Steps;
