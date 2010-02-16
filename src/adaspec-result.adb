--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with AdaSpec.Stanzas;

use AdaSpec.Stanzas;

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


   --------------------------------------------
   --  Result_Step_Type  --  Procedure_Name  --
   --------------------------------------------

   function Procedure_Name (S : in Result_Step_Type) return String is
   begin
      return To_String (S.Procedure_Name);
   end Procedure_Name;

   ---------------------------------------
   --  Result_Step_Type  --  To_String  --
   ---------------------------------------

   function To_String (S : in Result_Step_Type) return String is
   begin
      return Procedure_Name (S);
   end To_String;

   --------------------------------------------------
   --  Result_Scenario_Type  --  Process_Scenario  --
   --------------------------------------------------

   procedure Process_Scenario (Res      : out Result_Scenario_Type;
                               Scenario : in  Scenario_Type;
                               Steps    : in  Steps_Type;
                               Errors   : out Boolean)
   is
      use Ada.Text_IO;
      use Stanza_Container;
      use Result_Steps;
      I      : Stanza_Container.Cursor := First (Scenario.Stanzas);
      Stanza : Stanza_Type;
      Res_St : Result_Step_Type;
      StepsV : Result_Steps.Vector;
   begin
      Errors := False;
      while Has_Element (I) loop
         Stanza := Element (I);
         declare
            Proc_Name : constant String := Find (Steps, Stanza);
         begin
            if Proc_Name = "" then
               --  TODO: better error reporting
               Put_Line ("Error: Missing step for " & To_String (Stanza));
               Errors := True;
            else
               Make   (Res_St, Proc_Name);
               Append (StepsV, Res_St);
               Put_Line ("Add in step: " & Proc_Name);
            end if;
         end;
         Next (I);
      end loop;
      Res := (Steps => StepsV);
   end Process_Scenario;

end AdaSpec.Result;
