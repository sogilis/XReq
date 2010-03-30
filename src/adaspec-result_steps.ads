--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with AdaSpecLib;
with AdaSpec.Steps;
with AdaSpec.Step_Definitions;

use Ada.Strings.Unbounded;
use AdaSpecLib;
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

   function New_Result_Step (Procedure_Name : in  String;
                             Step           : in  Step_Type;
                             Matches        : in  Match_Vectors.Vector
                                            := Match_Vectors.Empty_Vector)
                                              return Result_Step_Type;


   function Procedure_Name (S      : in Result_Step_Type) return String;
   function To_String      (S      : in Result_Step_Type;
                            Indent : in String := "") return String;

private

   type Result_Step_Type is new Step_Type with
      record
         Procedure_Name : Unbounded_String;
         Matches        : Match_Vectors.Vector;
      end record;

end AdaSpec.Result_Steps;
