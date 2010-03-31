--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Util.IO;
with AdaSpec.Steps;
with AdaSpec.Step_Definitions;

use Ada.Strings.Unbounded;
use Util.IO;
use AdaSpec.Steps;
use AdaSpec.Step_Definitions;

package AdaSpec.Result_Steps is

   ------------------------
   --  Result_Step_Type  --
   ------------------------

   --  A procedure name of a step definition and its arguments

   type Result_Step_Type is new Step_Type with private;  --  GCOV_INGORE

   --  Creation  --------------------------------------------------------------

   function New_Result_Step (Step           : in  Step_Type;
                             Procedure_Name : in  String := "";
                             Matches        : in  Match_Vectors.Vector
                                            := Match_Vectors.Empty_Vector)
                             return Result_Step_Type;

   procedure Make           (Self           : out Result_Step_Type;
                             Step           : in  Step_Type;
                             Procedure_Name : in  String := "";
                             Matches        : in  Match_Vectors.Vector
                                            := Match_Vectors.Empty_Vector);

   --  Processing  ------------------------------------------------------------

   function  To_Code       (S             : in  Result_Step_Type;
                            Indent        : in  String := "") return String;

   procedure Process_Step  (Res           : out Result_Step_Type;
                            Stanza        : in  Step_Type;
                            Steps         : in  Step_Definitions_Type;
                            Log           : in  Logger_Ptr;
                            Errors        : out Boolean;
                            Step_Matching : in Boolean);

   --  Properties  ------------------------------------------------------------


   function  Procedure_Name     (S   : in     Result_Step_Type) return String;
   procedure Set_Procedure_Name (S   : in out Result_Step_Type;
                                 Prc : in     String);

   --  Collection: Matches  ---------------------------------------------------

   function Match_First   (S : in Result_Step_Type) return Natural;
   function Match_Last    (S : in Result_Step_Type) return Integer;
   function Match_Count   (S : in Result_Step_Type) return Natural;
   function Match_Element (S : in Result_Step_Type;
                           I : in Natural)          return Match_Location;

   ----------------------------------------------------------------------------

private

   type Result_Step_Type is new Step_Type with
      record
         Procedure_Name : Unbounded_String;
         Matches        : Match_Vectors.Vector;
      end record;

end AdaSpec.Result_Steps;
