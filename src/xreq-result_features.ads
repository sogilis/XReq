--                         Copyright (C) 2010, Sogilis                       --

with XReqLib;
with XReqLib.Generic_Features;
with Util.IO;
with Util.Strings;
with XReq.Result_Scenarios;
with XReq.Features;
with XReq.Step_Definitions;

use XReqLib;
use Util.IO;
use Util.Strings;
use XReq.Result_Scenarios;
use XReq.Features;
use XReq.Step_Definitions;


package XReq.Result_Features is

   ---------------------------
   --  Result_Feature_Type  --
   ---------------------------

   --  The Result_Feature_Type describes the result of the combinaison of the
   --  steps in the feature file and the step definitions. It contains the
   --  procedures of the step definitions to call in correct order and their
   --  parameters.

   package Features_Package is new XReqLib.Generic_Features
      (Result_Scenario_Type, "=");

   type Result_Feature_Type is new Features_Package.Feature_Type with private;

   --  Processing  ------------------------------------------------------------

   function  To_Code         (Res           : in     Result_Feature_Type;
                              Indent        : in     String := "")
                                              return String;
   procedure Process_Feature (Res           : out    Result_Feature_Type;
                              Feature       : in     Feature_Ptr;
                              Steps         : in     Step_Definitions_Type;
                              Log           : in     Logger_Ptr;
                              Missing_Steps : in out String_Set;
                              Step_Matching : in     Boolean := False);

   --  Properties  ------------------------------------------------------------

   function  Fail     (F    : in     Result_Feature_Type) return Boolean;
   procedure Set_Fail (F    : in out Result_Feature_Type;
                       Fail : in     Boolean := True);

   ----------------------------------------------------------------------------

private

   type Result_Feature_Type is new Features_Package.Feature_Type with
      record
         Fail        : Boolean := False;
      end record;

end XReq.Result_Features;
