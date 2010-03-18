--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpec.Result is

   ----------------------------------
   --  Result_Step_Type  --  Make  --
   ----------------------------------

   procedure Make (S              : out Result_Step_Type;
                   Procedure_Name : in  String;
                   Step           : in  Stanza_Type;
                   Matches        : in  Match_Vectors.Vector
                                  := Match_Vectors.Empty_Vector)
   is
   begin
      S := (
         Procedure_Name => To_Unbounded_String (Procedure_Name),
         Step           => Step,
         Matches        => Matches);
   end Make;

   ------------------------------------
   --  Result_Step_Type  --  Create  --
   ------------------------------------

   function  Create (Procedure_Name : in  String;
                     Step           : in  Stanza_Type;
                     Matches        : in  Match_Vectors.Vector
                                    := Match_Vectors.Empty_Vector)
                                   return Result_Step_Type
   is
      Res : Result_Step_Type;
   begin
      Make (Res, Procedure_Name, Step, Matches);
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
                       Indent : in String := "") return String
   is
      use Match_Vectors;
      Buffer : Unbounded_String;
      I : Cursor := First (S.Matches);
   begin
      Append (Buffer, Indent & Procedure_Name (S) & " (");
      while Has_Element (I) loop
         Append (Buffer, "(" &
                 Ada_String (Slice (S.Step.Stanza,
                                    Element (I).First,
                                    Element (I).Last)) &
                 Element (I).First'Img &
                 Element (I).Last'Img & ")");
         Next (I);
      end loop;
      Append (Buffer, " );");
      return To_String (Buffer);
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
                               Log      : in  Logger_Ptr;
                               Errors   : out Boolean)
   is
      use Stanza_Container;
      use Result_Steps;
      use Result_Steps_Vectors2;
      I         : Stanza_Container.Cursor := First (Scenario.Stanzas);
      Stanza    : Stanza_Type;
      Res_St    : Result_Step_Type;
      StepsV    : Result_Steps.Vector;
      Steps_tmp : Result_Steps.Vector;
      Proc_Name : Unbounded_String;
      Matches   : Match_Vectors.Vector;
      Found     : Boolean;
      Scenarios : Result_Steps_Vectors2.Vector;
   begin
      Errors := False;
      while Has_Element (I) loop
         Stanza := Element (I);
         begin
            Find (Steps, Stanza, Proc_Name, Matches, Found);
         exception
            when Ambiguous_Match =>
               --  TODO: better error reporting
               Log.Put_Line (String'("Error: Ambiguous match in " &
                             Position (Stanza) & " for:"));
               Log.Put_Line ("  " & To_String (Stanza));
               Errors := True;
         end;
         if not Found then
            Log.Put_Line (String'("Error: Missing step definition in " &
                          Position (Stanza) & " for:"));
            Log.Put_Line ("  " & To_String (Stanza));
            Log.Put_Line ("You can implement this step by adding on your " &
                          "step definition file:");
            Log.Put_Line ("  --  " & To_Regexp (Stanza));
            Log.Put_Line ("  --  @todo");
            Log.New_Line;
            Errors := True;
         else
            Make   (Res_St, To_String (Proc_Name), Stanza, Matches);
            Append (StepsV, Res_St);
            --  Put_Line ("Add in step: " & Proc_Name);
         end if;
         Next (I);
      end loop;
      if not Scenario.Outline then
         Res := (Outline => False,
                 Name    => Scenario.Name,
                 Pos     => Scenario.Pos,
                 Tags    => Scenario.Tags,
                 Steps   => StepsV);
      else
         for Y in Scenario.Table.First_Y + 1 .. Scenario.Table.Last_Y loop
            Steps_tmp := StepsV;
            for X in Scenario.Table.First_X .. Scenario.Table.Last_X loop
               declare
                  J     : Result_Steps.Cursor := First (Steps_tmp);
                  Item  : constant String := Scenario.Table.Item (X, Y, "");
                  Label : constant String := "<" & Scenario.Table.Item
                                   (X, Scenario.Table.First_Y, "") & ">";
               begin
                  while Has_Element (J) loop
                     Res_St := Element (J);
                     Replace (Res_St.Step.Stanza, Label, Item);
                     Replace_Element (Steps_tmp, J, Res_St);
                     Next (J);
                  end loop;
               end;
            end loop;
            Append (Scenarios, Steps_tmp);
         end loop;
         Res := (Outline   => True,
                 Name      => Scenario.Name,
                 Pos       => Scenario.Pos,
                 Tags      => Scenario.Tags,
                 Steps     => StepsV,
                 Table     => Scenario.Table,
                 Scenarios => Scenarios);
      end if;
   end Process_Scenario;

   ------------------------------------------------
   --  Result_Feature_Type  --  Process_Feature  --
   ------------------------------------------------

   procedure Process_Feature (Res     : out Result_Feature_Type;
                              Feature : in  Feature_Ptr;
                              Steps   : in  Steps_Type;
                              Log     : in  Logger_Ptr)
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
      Result := Result_Feature_Type'(
         Name        => Feature.Name,
         Description => Feature.Description,
         Pos         => Feature.Pos,
         others      => <>);
      Process_Scenario (Result.Background, Feature.all.Background,
                        Steps, Log, Errors);
      if Errors then
         Result.Fail := True;
      end if;
      while Has_Element (I) loop
         Process_Scenario (R_Scen, Element (I), Steps, Log, Errors);
         if Errors then
            Result.Fail := True;
         end if;
         Append (Result, R_Scen);
         Next (I);
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

end AdaSpec.Result;
