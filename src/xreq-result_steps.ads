--                         Copyright (C) 2010, Sogilis                       --

with Util.IO;
with Util.Strings;
with XReq.Steps;
with XReq.Step_Definitions;

use Util.IO;
use Util.Strings;
use XReq.Steps;
use XReq.Step_Definitions;

package XReq.Result_Steps is

   ------------------------
   --  Result_Step_Type  --
   ------------------------

   --  A procedure name of a step definition and its arguments

   type Result_Step_Type is new Step_Type with private;  --  GCOV_IGNORE

   --  Creation  --------------------------------------------------------------

   function  New_Result_Step (Step           : in  Step_Type;
                              Match          : in  Step_Match_Type
                                             := Step_Match_Type'(others => <>))
                             return Result_Step_Type;

   procedure Make           (Self           : out Result_Step_Type;
                             Step           : in  Step_Type;
                             Match          : in  Step_Match_Type
                                            := Step_Match_Type'(others => <>));

   --  Processing  ------------------------------------------------------------

   function  To_Code       (S             : in     Result_Step_Type;
                            Indent        : in     String := "") return String;

   procedure Process_Step  (Res           : out    Result_Step_Type;
                            Stanza        : in     Step_Type;
                            Steps         : in     Step_Definitions_Type;
                            Log           : in     Logger_Ptr;
                            Errors        : out    Boolean;
                            Step_Matching : in     Boolean;
                            Missing_Steps : in out String_Set);

   --  Properties  ------------------------------------------------------------


   function  Procedure_Name     (S   : in     Result_Step_Type) return String;
   function  File_Name          (S   : in     Result_Step_Type) return String;
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
         Match : Step_Match_Type;
      end record;

end XReq.Result_Steps;
