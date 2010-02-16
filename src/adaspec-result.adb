--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpec.Result is

   ----------------------------------
   --  Result_Step_Type  --  Make  --
   ----------------------------------

   procedure Make (S              : out Result_Step_Type;
                   Procedure_Name : in  String)
   is
   begin
      S := (
         Procedure_Name => To_Unbounded_String (Procedure_Name));
   end Make;

   ------------------------------------
   --  Result_Step_Type  --  Create  --
   ------------------------------------

   function  Create (Procedure_Name : in  String) return Result_Step_Type is
      Res : Result_Step_Type;
   begin
      Make (Res, Procedure_Name);
      return Res;
   end Create;


   ----------------------------------
   --  Result_Step_Type  --  Name  --
   ----------------------------------

   function Procedure_Name (S : in Result_Step_Type) return String is
   begin
      return To_String (S.Procedure_Name);
   end Procedure_Name;


   --------------------------------------------------
   --  Result_Scenario_Type  --  Process_Scenario  --
   --------------------------------------------------

   procedure Process_Scenario (Res      : out Result_Scenario_Type;
                               Scenario : in  Scenario_Type;
                               Steps    : in  Steps_Type)
   is
      pragma Unreferenced (Scenario, Steps);
   begin
      Res := (others => <>);
   end Process_Scenario;

end AdaSpec.Result;
