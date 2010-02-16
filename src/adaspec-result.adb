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

   function To_String (S : in Result_Step_Type;
                       Indent : in String := "") return String is
   begin
      return Indent & Procedure_Name (S);
   end To_String;

   ----------------------------------------
   --  Result_Scenario_Type  --  Append  --
   ----------------------------------------

   procedure Append           (Res      : in out Result_Scenario_Type;
                               Step     : in     Result_Step_Type)
   is
      use Result_Steps;
   begin
      Append (Res.Steps, Step);
   end Append;

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

   ------------------------------------------------
   --  Result_Feature_Type  --  Process_Feature  --
   ------------------------------------------------

   procedure Process_Feature (Res     : out Result_Feature_Type;
                              Feature : in  Feature_Ptr;
                              Steps   : in  Steps_Type)
   is
      use Scenario_Container;
      I      : Scenario_Container.Cursor := First (Feature.all.Scenarios);
      R_Scen : Result_Scenario_Type;
      Result : Result_Feature_Type;
      Errors : Boolean;
   begin
      if not Parsed (Feature.all) then
         raise Unparsed_Feature;
      end if;
      Process_Scenario (Result.Background, Feature.all.Background,
                        Steps, Errors);
      while Has_Element (I) loop
         Process_Scenario (R_Scen, Element (I), Steps, Errors);
         Append (Result, R_Scen);
         Next (I);
      end loop;
      Res := Result;
   end Process_Feature;

   ---------------------------------------
   --  Result_Feature_Type  --  Append  --
   ---------------------------------------

   procedure Append          (Res      : in out Result_Feature_Type;
                              Scenario : in     Result_Scenario_Type)
   is
      use Result_Scenarios;
   begin
      Append (Res.Scenarios, Scenario);
   end Append;

   -------------------------------------------
   --  Result_Scenario_Type  --  To_String  --
   -------------------------------------------

   function  To_String        (Res      : in     Result_Scenario_Type;
                               Indent   : in     String := "")
                                          return String
   is
      use Result_Steps;
      CRLF   : constant String := ASCII.CR & ASCII.LF;
      I      : Result_Steps.Cursor := First (Res.Steps);
      Buffer : Unbounded_String;
   begin
      while Has_Element (I) loop
         Append (Buffer, Indent & To_String (Element (I)) & CRLF);
         Next (I);
      end loop;
      return To_String (Buffer);
   end To_String;

   ------------------------------------------
   --  Result_Feature_Type  --  To_String  --
   ------------------------------------------

   function  To_String        (Res      : in     Result_Feature_Type;
                               Indent   : in     String := "")
                                          return String
   is
      use Result_Scenarios;
      CRLF   : constant String := ASCII.CR & ASCII.LF;
      I      : Result_Scenarios.Cursor := First (Res.Scenarios);
      Buffer : Unbounded_String;
   begin
      Append (Buffer, Indent & "Background" & CRLF);
      Append (Buffer, To_String (Res.Background, Indent & "   "));
      Append (Buffer, Indent & "End Background" & CRLF);
      while Has_Element (I) loop
         Append (Buffer, Indent & "Scenario" & CRLF);
         Append (Buffer, To_String (Element (I), Indent & "   "));
         Append (Buffer, Indent & "End Scenario" & CRLF);
         Next (I);
      end loop;
      return To_String (Buffer);
   end To_String;

end AdaSpec.Result;
