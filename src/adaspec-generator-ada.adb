--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpec.Generator.Ada is

   procedure Generate_Step     (Buffer   : in out Unbounded_String;
                                Pool     : in out String_Pool;
                                Step     : in Result_Step_Type;
                                Indent   : in String := "";
                                CRLF     : in String := ASCII.CR & ASCII.LF)
   is
      pragma Unreferenced (Pool);
   begin
      Append (Buffer, Indent & Procedure_Name (Step) & ";" & CRLF);
   end Generate_Step;

   procedure Generate_Scenario (Buffer   : in out Unbounded_String;
                                Pool     : in out String_Pool;
                                Scenario : in Result_Scenario_Type;
                                Indent   : in String := "";
                                CRLF     : in String := ASCII.CR & ASCII.LF)
   is
      use Result_Steps;
      I      : Result_Steps.Cursor := First (Scenario.Steps);
      S_Name : constant String := To_String (Scenario.Name);
   begin
      --  TODO: use strings pool
      Append (Buffer, Indent & "procedure " & S_Name & " is" & CRLF);
      Append (Buffer, Indent & "begin" & CRLF);
      while Has_Element (I) loop
         Generate_Step (Buffer, Pool, Element (I), Indent & "   ", CRLF);
         Next (I);
      end loop;
      Append (Buffer, Indent & "end " & S_Name & ";" & CRLF);
   end Generate_Scenario;

   procedure Generate_Feature  (Buffer   : in out Unbounded_String;
                                Pool     : in out String_Pool;
                                Feature  : in Result_Feature_Type;
                                Indent   : in String := "";
                                CRLF     : in String := ASCII.CR & ASCII.LF)
   is
      use Result_Scenarios;
      I      : Result_Scenarios.Cursor := First (Feature.Scenarios);
   begin
      --  TODO: use strings pool
      Append (Buffer, Indent & "package body ... is" & CRLF);
      while Has_Element (I) loop
         Generate_Scenario (Buffer, Pool, Element (I), Indent & "   ", CRLF);
         Next (I);
      end loop;
      Append (Buffer, Indent & "end ...;" & CRLF);
   end Generate_Feature;

end AdaSpec.Generator.Ada;
