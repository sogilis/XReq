--                         Copyright (C) 2010, Sogilis                       --


package body AdaSpec.Result_Features is

   ------------------------------------------------
   --  Result_Feature_Type  --  Process_Feature  --
   ------------------------------------------------

   procedure Process_Feature (Res     : out Result_Feature_Type;
                              Feature : in  Feature_Ptr;
                              Steps   : in  Step_Definitions_Type;
                              Log     : in  Logger_Ptr;
                              Step_Matching : in Boolean := False)
   is
      R_Scen : Result_Scenario_Type;
      Result : Result_Feature_Type;
      Errors : Boolean;
   begin
      if not Feature.Parsed then
         raise Unparsed_Feature;
      end if;
      Result := Result_Feature_Type'(
         Name        => To_Unbounded_String (Feature.Name),
         Description => To_Unbounded_String (Feature.Description),
         Pos         => Feature.Position,
         others      => <>);
      Process_Scenario (Result.Background, Feature.all.Background,
                        Steps, Log, Errors, Step_Matching);
      if Errors then
         Result.Fail := True;
      end if;
      for I in Feature.Scenario_First .. Feature.Scenario_Last loop
         Process_Scenario (R_Scen,
                           Feature.Scenario_Element (I),
                           Steps,
                           Log,
                           Errors,
                           Step_Matching);
         if Errors then
            Result.Fail := True;
         end if;
         Append (Result, R_Scen);
      end loop;
      if Result.Fail then
         Log.Put_Line ("AdaSpec can create the procedures for you if you " &
                       "use --fill-steps");
      end if;
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
      S      : constant String := To_String (Res.Background.Name);
   begin
      Append (Buffer, Indent & "Feature " & To_String (Res.Name) & CRLF);
      Append (Buffer, Indent & "   Background " & S & CRLF);
      Append (Buffer, To_String (Res.Background, Indent & "      "));
      Append (Buffer, Indent & "   End Background " & S & CRLF);
      while Has_Element (I) loop
         declare
            S2 : constant String := To_String (Element (I).Name);
         begin
            Append (Buffer, Indent & "   Scenario " & S2 & CRLF);
            Append (Buffer, To_String (Element (I), Indent & "      "));
            Append (Buffer, Indent & "   End Scenario " & S2 & CRLF);
         end;
         Next (I);
      end loop;
      Append (Buffer, Indent & "End Feature " & To_String (Res.Name) & CRLF);
      return To_String (Buffer);
   end To_String;

end AdaSpec.Result_Features;
