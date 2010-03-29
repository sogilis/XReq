--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Containers;
with Ada.Characters.Handling;
with Util.IO;
with AdaSpecLib.String_Tables;
with AdaSpec.Steps;
with AdaSpec.Stanzas;

use Ada.Directories;
use Ada.Containers;
use Util.IO;
use AdaSpecLib;
use AdaSpec.Steps;
use AdaSpec.Stanzas;

package body AdaSpec.Generator.Ada05 is


   procedure Generate_Table    (S          : in out Ada_Generator_Type;
                                Name       : in     String;
                                T          : in     String_Tables.Table);
   procedure Generate_Step     (S          : in out Ada_Generator_Type;
                                Scenario   : in     Result_Scenario_Type;
                                Step       : in     Result_Step_Type;
                                Background : in     Boolean := False;
                                Fake       : in     Boolean := False;
                                Outline    : in     Boolean := False);
   procedure Generate_Scenario (S          : in out Ada_Generator_Type;
                                Scenario   : in     Result_Scenario_Type;
                                Name       : in     Unbounded_String;
                                Seq_Num    : in     Integer;
                                Background : in     Boolean := False);
   procedure Generate_Feature  (S          : in out Ada_Generator_Type);
   procedure Generate_With     (S          : in out Ada_Generator_Type);

   ------------
   --  Make  --
   ------------

   procedure Make     (Gen : out    Ada_Generator_Type;
                       Job : in     Job_Type;
                       Env : in     Job_Environment)
   is
      use Ada.Characters.Handling;
      Basename    : Unbounded_String;
      Pkgname     : constant String :=
                     "Feature_" & Base_Name (Feature_File (Job));
      Generator   : Ada_Generator_Type := (
                     Feature  => Job.Result,
                     others   => <>);
   begin
      Get_Unique_String (
         Gen.Pool, To_Identifier (Pkgname),      Generator.Id_Pkgname);
      Get_Unique_String (
         Gen.Pool, To_Identifier ("Background"), Generator.Fn_Backgnd);
      Basename := To_Unbounded_String (Compose (Out_Dir (Env),
                  To_Lower (To_String (Generator.Id_Pkgname))));
      Generator.Ads_File := Basename & ".ads";
      Generator.Adb_File := Basename & ".adb";
      Gen := Generator;
   end Make;

   ----------------------
   --  Generate_Table  --
   ----------------------

   procedure Generate_Table    (S          : in out Ada_Generator_Type;
                                Name       : in     String;
                                T          : in     String_Tables.Table)
   is
      use String_Tables;
      use Ada.Strings;
      use Ada.Strings.Fixed;
      I : String_Tables.Cursor;
      K : String_Tables.Key_Type;
   begin
      I := T.First;
      while Has_Element (I) loop
         K := Key (I);
         S.Adb.Put_Line (Name & ".Put (" &
            Trim (K.X'Img, Left)                 & ", " &
            Trim (K.Y'Img, Left)                 & ", " &
            Ada_String (To_String (Element (I))) & ");");
         Next (I);
      end loop;
   end Generate_Table;

   ---------------------
   --  Generate_Step  --
   ---------------------

   procedure Generate_Step     (S          : in out Ada_Generator_Type;
                                Scenario   : in     Result_Scenario_Type;
                                Step       : in     Result_Step_Type;
                                Background : in     Boolean := False;
                                Fake       : in     Boolean := False;
                                Outline    : in     Boolean := False)
   is
      pragma Unreferenced (Scenario);
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Argument_Vectors;
      use String_Set;
      use Match_Vectors;
      Procname : constant String := Procedure_Name (Step);
      Pkgname  : Unbounded_String;
      Copy     : Boolean := False;
      I        : Match_Vectors.Cursor := First (Step.Matches);
      E        : Match_Location;
      I2       : Argument_Vectors.Cursor := First (Step.Step.Args);
      E2       : Argument_Type;
   begin
      S.Adb.Put_Line ("--");
      S.Adb.Put_Line ("--  " & To_String (Step.Step));
      S.Adb.Put_Line ("--");
      S.Adb.Put_Line ("Format.Start_Step;");

      -------------------------------------------------------------------------
      --  Declare  ------------------------------------------------------------
      -------------------------------------------------------------------------

      --  Args   : Arguments (matches, text, tables ...)
      --  Prefix : (Given, When, Then)
      --  Stanza : Step sentance
      --  Pos    : String filename:line

      S.Adb.Put_Line ("declare");
      S.Adb.Indent;
      S.Adb.Put_Line ("Args   : Arg_Type;");
      S.Adb.Put_Indent;
      S.Adb.Put      ("Prefix : constant Step_Type := ");
      case Step.Step.Prefix is
         when Prefix_Given => S.Adb.Put ("Step_Given;");
         when Prefix_When  => S.Adb.Put ("Step_When;");
         when Prefix_Then  => S.Adb.Put ("Step_Then;");
      end case;
      S.Adb.New_Line;
      S.Adb.Put_Line ("Stanza : constant String    := " &
                      Ada_String (To_String (Step.Step.Stanza)) & ";");
      S.Adb.Put_Line ("Pos    : constant String    := " &
                      Ada_String (To_String (Step.Step.Pos)) & ";");
      S.Adb.UnIndent;

      -------------------------------------------------------------------------
      --  Begin  --------------------------------------------------------------
      -------------------------------------------------------------------------

      S.Adb.Put_Line ("begin");
      S.Adb.Indent;

      --------------------
      --  Fill in Args  --
      -------------------------------------------------------------------------
      S.Adb.Put_Line ("Make (Args, " &
                     Ada_String (To_String (Step.Step.Stanza)) & ");");
      while Has_Element (I) loop
         E := Element (I);
         S.Adb.Put_Line ("Add_Match (Args," & E.First'Img & "," &
                                     E.Last'Img & ");");
         Next (I);
      end loop;
      while Has_Element (I2) loop
         E2 := Element (I2);
         case E2.Typ is
            when Text =>
               S.Adb.Put_Line ("Add_Text  (Args, " &
                               Ada_String (To_String (E2.Text)) & ");");
            when Table =>
               S.Adb.Put_Line ("declare");
               S.Adb.Indent;
               S.Adb.Put_Line ("Tble : Table_Type;");
               S.Adb.UnIndent;
               S.Adb.Put_Line ("begin");
               S.Adb.Indent;
               Generate_Table (S, "Tble", E2.Table);
               S.Adb.Put_Line ("Args.Add_Table (Tble);");
               S.Adb.UnIndent;
               S.Adb.Put_Line ("end;");
            when others =>
               null;    --  GCOV_IGNORE (never happens)
         end case;
         Next (I2);
      end loop;
      S.Adb.Put_Line ("Add_Sep   (Args, 1);");
      --  Skip if failure
      S.Adb.Put_Line ("if Fail then");
      S.Adb.Indent;
      S.Adb.Put_Line ("Report.Count_Steps_Skipped := " &
                      "Report.Count_Steps_Skipped + 1;");
      if Background then
         S.Adb.Put_Line ("if not Stop then");
         S.Adb.Indent;
      end if;
      S.Adb.Put_Line ("Format.Put_Step  (Prefix, Stanza, Pos, Args, " &
                      "Status_Skipped);");
      if Background then
         S.Adb.UnIndent;
         S.Adb.Put_Line ("end if;");
      end if;
      S.Adb.UnIndent;
      S.Adb.Put_Line ("else");
      S.Adb.Indent;
      if Fake then
         S.Adb.Put_Line ("Format.Put_Step (Prefix, Stanza, Pos, Args, " &
                         "Status_Outline);");
      elsif Procname = "" then
         S.Adb.Put_Line ("raise AdaSpecLib.Not_Yet_Implemented");
         S.Adb.Put_Line ("   with ""The step definition cound not be " &
                         "found"";");
      else
         --  Generate with clause
         for K in reverse Procname'Range loop
            if Copy then
               Pkgname := Procname (K) & Pkgname;
            elsif Procname (K) = '.' then
               Copy := True;
            end if;
         end loop;
         if not Contains (S.With_Pkg, Pkgname) then
            Insert (S.With_Pkg, Pkgname);
         end if;
         --  Call to step
         S.Adb.Put_Line (Procname & " (Args);");
         --  Count step
         S.Adb.Put_Line ("Report.Count_Steps_Passed := " &
                         "Report.Count_Steps_Passed + 1;");
         --  Print the step
         if Background then
            S.Adb.Put_Line ("if First then");
            S.Adb.Indent;
         end if;
         S.Adb.Put_Line ("Format.Put_Step (Prefix, Stanza, Pos, Args, " &
                        "Status_Passed);");
         if Background then
            S.Adb.UnIndent;
            S.Adb.Put_Line ("end if;");
         end if;
      end if;
      --  End if skip
      S.Adb.UnIndent;
      S.Adb.Put_Line ("end if;");
      S.Adb.UnIndent;

      -------------------------------------------------------------------------
      --  Exception  ----------------------------------------------------------
      -------------------------------------------------------------------------

      if not Fake then
         S.Adb.Put_Line ("exception");
         S.Adb.Put_Line ("   when Err : others =>");
         S.Adb.Put_Line ("     Report.Count_Steps_Failed := " &
                              "Report.Count_Steps_Failed + 1;");
         S.Adb.Put_Line ("     Fail := True;");
         if Outline then
            S.Adb.Put_Line ("     Priv_Put_Scenario;");
         end if;
         S.Adb.Put_Line ("     Format.Put_Step  (Prefix, Stanza, Pos, " &
                              "Args, Status_Failed);");
         S.Adb.Put_Line ("     Format.Put_Error (Err);");
      end if;
      --  End block
      S.Adb.Put_Line ("end;");
      S.Adb.Put_Line ("Format.Stop_Step;");
   end Generate_Step;

   -------------------------
   --  Generate_Scenario  --
   -------------------------

   procedure Generate_Scenario (S          : in out Ada_Generator_Type;
                                Scenario   : in Result_Scenario_Type;
                                Name       : in Unbounded_String;
                                Seq_Num    : in Integer;
                                Background : in Boolean := False)
   is
      use Util.Strings.Vectors;
      use Result_Steps_Vectors2;
      use Result_Steps;
      I     : Result_Steps.Cursor;
      E     : Result_Steps.Vector;
      J     : Result_Steps_Vectors2.Cursor;
      N     : Integer;
      Proc  : constant String := "procedure " & To_String (Name) & " ";
      First : Boolean := True;
         --  True if it is the first scenario of the outline
   begin

      -------------------------------------------------------------------------
      --  Declaration  --------------------------------------------------------
      -------------------------------------------------------------------------

      --  ARGUMENTS:

      --  Format : Object that handle writing what happens
      --  Report : Counters of passed, skipped and faikled steps/scenarios
      --  First  : True if it is the first scenario of the feature
      --           Set to False if the scenario is executed
      --  Cond   : Condition from the command line to know if the scenario has
      --           to be executed (in form of tags or FEATURE:NUM)
      --  Stop   : Set to True when the background fails for the first scenario
      --           if it is True, skip all the steps in the feature.

      --  VARIABLES:

      --  Fail   : True if a step failed. In that case, all other steps are
      --           skipped
      --  Tags   : Array of tags

      S.Adb.New_Line;
      S.Ads.Put_Line (Proc & "(Format   : in out Format_Ptr;");
      S.Adb.Put_Line (Proc & "(Format   : in out Format_Ptr;");
      S.Ads.Put_Indent; S.Ads.Put (Proc'Length * " ");
      S.Adb.Put_Indent; S.Adb.Put (Proc'Length * " ");
      S.Ads.Put             (" Report   : in out Report_Type;");
      S.Adb.Put             (" Report   : in out Report_Type;");
      S.Ads.New_Line; S.Ads.Put_Indent; S.Ads.Put (Proc'Length * " ");
      S.Adb.New_Line; S.Adb.Put_Indent; S.Adb.Put (Proc'Length * " ");
      S.Ads.Put             (" First    : in out Boolean;");
      S.Adb.Put             (" First    : in out Boolean;");
      S.Ads.New_Line; S.Ads.Put_Indent; S.Ads.Put (Proc'Length * " ");
      S.Adb.New_Line; S.Adb.Put_Indent; S.Adb.Put (Proc'Length * " ");
      S.Ads.Put             (" Cond     : in     Conditional_Type;");
      S.Adb.Put             (" Cond     : in     Conditional_Type;");
      S.Ads.New_Line; S.Ads.Put_Indent; S.Ads.Put (Proc'Length * " ");
      S.Adb.New_Line; S.Adb.Put_Indent; S.Adb.Put (Proc'Length * " ");
      S.Ads.Put             (" Stop     : in out Boolean);");
      S.Adb.Put             (" Stop     : in out Boolean)");
      S.Ads.New_Line;
      S.Adb.New_Line;
      S.Adb.Put_Line ("is");
      S.Adb.Indent;
      if Scenario.Outline then
         S.Adb.Put_Line ("Outline_Table : Table_Type;");
      end if;
      S.Adb.Put_Line    ("Fail          : Boolean := Stop;");
      S.Adb.Put_Line    ("Tags          : constant " &
                     "AdaSpecLib.Format.Tag_Array_Type (1 .." &
                     String'(Length (Scenario.Tags)'Img) & ") :=");
      S.Adb.Put_Indent;
      S.Adb.Put ("      (");
      for I in 0 .. Integer (Length (Scenario.Tags)) - 1 loop
         if I > 0 then
            S.Adb.Put (", ");
            S.Adb.New_Line;
            S.Adb.Put_Indent;
            S.Adb.Put ("       ");
         end if;
         S.Adb.Put (String'(Integer'Image (I + 1)) &
                    " => Ada.Strings.Unbounded.To_Unbounded_String (" &
                    Ada_String (To_String (Element (Scenario.Tags, I))) &
                    ")");
      end loop;
      if Length (Scenario.Tags) = 0 then
         S.Adb.Put ("others => Ada.Strings.Unbounded.Null_Unbounded_String");
      end if;
      S.Adb.Put (");");
      S.Adb.New_Line;
      S.Adb.UnIndent;
      S.Adb.Put_Line ("begin");
      Indent (S.Adb);

      -------------------------------------------------------------------------
      --  Body  ---------------------------------------------------------------
      -------------------------------------------------------------------------

      if Background then
         S.Adb.Put_Line ("Format.Start_Background (First);");
      else
         S.Adb.Put_Line ("if Cond.Eval (Tags) and then Cond.Eval (" &
                         Ada_String (To_String (Scenario.Pos.File)) & "," &
                         Seq_Num'Img & ") then");
         S.Adb.Indent;
         S.Adb.Put_Line ("if First then");
         S.Adb.Put_Line ("   Priv_Feature (Format);");
         S.Adb.Put_Line ("end if;");
         if Scenario.Outline then
            Generate_Table (S, "Outline_Table", Scenario.Table);
            S.Adb.Put_Line ("Format.Enter_Outline;");
         else
            S.Adb.Put_Line ("Format.Enter_Scenario;");
         end if;
         S.Adb.Put_Line ("if not First then");
         S.Adb.Put_Line ("   --  Background has already been shown, " &
                                "show scenario");
         S.Adb.Put_Indent; S.Adb.Put ("  Format.");
         if Scenario.Outline then
            S.Adb.Put ("Put_Outline");
         else
            S.Adb.Put ("Put_Scenario");
         end if;
         S.Adb.Put (" (" & Ada_String (To_String (Scenario.Name)) & ", " &
                           Ada_String (To_String (Scenario.Pos)) & ", " &
                           "Tags);");
         S.Adb.New_Line;
         S.Adb.Put_Line ("end if;");
         S.Adb.Put_Line ("------------------");
         S.Adb.Put_Line ("--  Background  --");
         S.Adb.Put_Line ("------------------");
         S.Adb.Put_Line (S.Fn_Backgnd &
                         " (Format, Report, First, Cond, Fail);");
         S.Adb.Put_Line ("Stop := Stop or (First and Fail);");
         if Scenario.Outline then
            S.Adb.Put_Line ("Format.Start_Outline;");
         else
            S.Adb.Put_Line ("Format.Start_Scenario;");
         end if;
      end if;
      if not Background or else Length (Scenario.Steps) /= 0 then
         if Background then
            S.Adb.Put_Line ("------------------");
            S.Adb.Put_Line ("--  Background  --");
            S.Adb.Put_Line ("------------------");
         elsif Scenario.Outline then
            S.Adb.Put_Line ("--------------------------");
            S.Adb.Put_Line ("--   Scenario Outline   --");
            S.Adb.Put_Line ("--------------------------");
         else
            S.Adb.Put_Line ("----------------");
            S.Adb.Put_Line ("--  Scenario  --");
            S.Adb.Put_Line ("----------------");
         end if;
         S.Adb.Put_Line ("if First then");
         S.Adb.Indent;
         if Background then
            S.Adb.Put_Line ("Format.Put_Background (" &
                              Ada_String (To_String (Scenario.Name)) & ", " &
                              Ada_String (To_String (Scenario.Pos)) & ", " &
                              "Tags);");
         elsif Scenario.Outline then
            S.Adb.Put_Line ("Format.Put_Outline (" &
                              Ada_String (To_String (Scenario.Name)) & ", " &
                              Ada_String (To_String (Scenario.Pos)) & ", " &
                              "Tags);");
         else
            S.Adb.Put_Line ("Format.Put_Scenario (" &
                              Ada_String (To_String (Scenario.Name)) & ", " &
                              Ada_String (To_String (Scenario.Pos)) & ", " &
                              "Tags);");
         end if;
         S.Adb.UnIndent;
         S.Adb.Put_Line ("end if;");
         I := Result_Steps.First (Scenario.Steps);
         while Has_Element (I) loop
            Generate_Step (S, Scenario, Element (I),
                           Background, Scenario.Outline, Scenario.Outline);
            Next (I);
         end loop;
      end if;
      if Scenario.Outline then
         N := 0;
         J := Result_Steps_Vectors2.First (Scenario.Scenarios);
         while Has_Element (J) loop
            N := N + 1;
            E := Element (J);
            S.Adb.Put_Line ("--------------------------");
            S.Adb.Put_Line ("--  Generated Scenario  --");
            S.Adb.Put_Line ("--------------------------");
            S.Adb.Put_Line ("Format.Enter_Scenario;");
            S.Adb.Put_Line (S.Fn_Backgnd &
                           " (Format, Report, First, Cond, Fail);");
            S.Adb.Put_Line ("Stop := Stop or (First and Fail);");
            S.Adb.Put_Line ("Fail := Stop;");
            if not First then
               S.Adb.Put_Line (S.Fn_Backgnd &
                              " (Format, Report, First, Cond, Fail);");
               S.Adb.Put_Line ("Stop := Stop or (First and Fail);");
            end if;
            S.Adb.Put_Line ("Format.Start_Scenario;");
            S.Adb.Put_Line ("declare");
            S.Adb.Indent;
            S.Adb.Put_Line ("procedure Priv_Put_Scenario;");
            S.Adb.Put_Line ("procedure Priv_Put_Scenario is");
            S.Adb.Put_Line ("begin");
            S.Adb.Put_Line ("  Format.Put_Scenario_Outline (" &
                            Ada.Strings.Fixed.Trim
                              (N'Img, Ada.Strings.Left) & ", " &
                            Ada_String (To_String (Scenario.Name)) & ", " &
                            Ada_String (To_String (Scenario.Pos)) & ", " &
                            "Tags);");
            S.Adb.Put_Line ("end Priv_Put_Scenario;");
            S.Adb.UnIndent;
            S.Adb.Put_Line ("begin");
            S.Adb.Indent;
            I := Result_Steps.First (E);
            while Has_Element (I) loop
               Generate_Step (S, Scenario, Element (I),
                              Background, False, True);
               Next (I);
            end loop;
            S.Adb.UnIndent;
            S.Adb.Put_Line ("end;");
            S.Adb.Put_Line ("if Fail then");
            S.Adb.Put_Line ("   Report.Count_Scenario_Failed := " &
                              "Report.Count_Scenario_Failed + 1;");
            S.Adb.Put_Line ("else");
            S.Adb.Put_Line ("   Report.Count_Scenario_Passed := " &
                              "Report.Count_Scenario_Passed + 1;");
            S.Adb.Put_Line ("end if;");
            S.Adb.Put_Line ("Format.Stop_Scenario;");
            Next (J);
            First := False;
         end loop;
         S.Adb.Put_Line ("--------------------------------");
         S.Adb.Put_Line ("--  Scenario Outline Summary  --");
         S.Adb.Put_Line ("--------------------------------");
         S.Adb.Put_Line ("Format.Put_Outline_Report (Outline_Table);");
      end if;
      S.Adb.Put_Line ("--------------------");
      S.Adb.Put_Line ("--  Finalization  --");
      S.Adb.Put_Line ("--------------------");
      if Background then
         S.Adb.Put_Line ("Stop := Fail;");
         S.Adb.Put_Line ("Format.Stop_Background (First);");
      else
         if Scenario.Outline then
            S.Adb.Put_Line ("Format.Stop_Outline;");
         else
            S.Adb.Put_Line ("if Fail then");
            S.Adb.Put_Line ("   Report.Count_Scenario_Failed := " &
                              "Report.Count_Scenario_Failed + 1;");
            S.Adb.Put_Line ("else");
            S.Adb.Put_Line ("   Report.Count_Scenario_Passed := " &
                              "Report.Count_Scenario_Passed + 1;");
            S.Adb.Put_Line ("end if;");
            S.Adb.Put_Line ("Format.Stop_Scenario;");
         end if;
         S.Adb.Put_Line ("First := False;");
         S.Adb.UnIndent;
         S.Adb.Put_Line ("end if;");
      end if;
      S.Adb.UnIndent;
      S.Adb.Put_Line ("end " & Name & ";");
   end Generate_Scenario;

   ------------------------
   --  Generate_Feature  --
   ------------------------

   procedure Generate_Feature  (S : in out Ada_Generator_Type)
   is
      use Util.Strings.Vectors;
      use Result_Scenarios;
      I   : Result_Scenarios.Cursor := First (S.Feature.Scenarios);
      Str : Unbounded_String;
      N   : Positive := 1;
   begin
      Generate_Scenario (S, S.Feature.Background, S.Fn_Backgnd, 0, True);
      while Has_Element (I) loop
         Get_Unique_String (S.Pool,
            To_Identifier ("Scenario_" & To_String (Element (I).Name)),
            Str);
         Append (S.Fn_Steps, Str);
         Generate_Scenario (S, Element (I), Str, N);
         Next (I);
         N := N + 1;
      end loop;
   end Generate_Feature;

   ---------------------
   --  Generate_With  --
   ---------------------

   procedure Generate_With (S : in out Ada_Generator_Type)
   is
      use String_Set;
      J   : String_Set.Cursor := First (S.With_Pkg);
      Buf : Unbounded_String;
   begin
      while Has_Element (J) loop
         Append (Buf, "with " & Element (J) & ";" & S.Adb.CRLF);
         Next (J);
      end loop;
      S.Adb.Buffer := Buf & S.Adb.CRLF & S.Adb.Buffer;
   end Generate_With;

   -----------------
   --  Full_Name  --
   -----------------

   function  Full_Name (Gen : in Ada_Generator_Type) return String is
   begin
      return To_String (Gen.Id_Pkgname);
   end Full_Name;

   ----------------
   --  Generate  --
   ----------------

   procedure Generate (Gen : in out Ada_Generator_Type;
                       Log : in     Logger_Ptr) is
      use Util.Strings.Vectors;
      use Result_Scenarios;
      I   : Result_Scenarios.Cursor := First (Gen.Feature.Scenarios);
      Num : Positive := 1;
   begin
      Gen.Ads.Put_Line ("with Ada.Strings.Unbounded;");
      Gen.Ads.Put_Line ("with AdaSpecLib;");
      Gen.Ads.Put_Line ("with AdaSpecLib.Args;");
      Gen.Ads.Put_Line ("with AdaSpecLib.Report;");
      Gen.Ads.Put_Line ("with AdaSpecLib.Format;");
      Gen.Ads.Put_Line ("use  AdaSpecLib;");
      Gen.Ads.Put_Line ("use  AdaSpecLib.Args;");
      Gen.Ads.Put_Line ("use  AdaSpecLib.Report;");
      Gen.Ads.Put_Line ("use  AdaSpecLib.Format;");
      Gen.Ads.Put_Line ("package " & Gen.Id_Pkgname & " is");
      Gen.Adb.Put_Line ("package body " & Gen.Id_Pkgname & " is");
      Gen.Ads.Indent;
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("procedure Priv_Feature " &
                              "(Format : in out Format_Ptr);");
      Gen.Adb.New_Line;
      Gen.Adb.Put_Line ("procedure Priv_Feature " &
                              "(Format : in out Format_Ptr) is");
      Gen.Adb.Put_Line ("begin");
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("Format.Put_Feature (" &
                        Ada_String (To_String (Gen.Feature.Name)) & ", " &
                        Ada_String (Join (Gen.Feature.Description,
                                          "" & ASCII.LF)) & ", " &
                        Ada_String (To_String (Gen.Feature.Pos)) & ");");
      Gen.Adb.UnIndent;
      Gen.Adb.Put_Line ("end Priv_Feature;");
      Generate_Feature (Gen);
      Gen.Adb.New_Line;
      Gen.Ads.Put_Line ("procedure Run (Format    : in out Format_Ptr;");
      Gen.Ads.Put_Line ("               Cond      : in Conditional_Type;");
      Gen.Ads.Put_Line ("               Report    : in out Report_Type;");
      Gen.Ads.Put_Line ("               List_Mode : in Boolean := False);");
      Gen.Adb.Put_Line ("procedure Run (Format    : in out Format_Ptr;");
      Gen.Adb.Put_Line ("               Cond      : in Conditional_Type;");
      Gen.Adb.Put_Line ("               Report    : in out Report_Type;");
      Gen.Adb.Put_Line ("               List_Mode : in Boolean := False) is");
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("Stop  : Boolean  := False;");
      Gen.Adb.Put_Line ("First : Boolean  := True;");
      Gen.Adb.UnIndent;
      Gen.Adb.Put_Line ("begin");
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("if List_Mode then");
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("Format.List_Feature (" &
                        Ada_String (To_String (Gen.Feature.Name)) & ");");
      while Has_Element (I) loop
         Gen.Adb.Put_Line ("Format.List_Scenario (" &
                           Ada_String (To_String (Element (I).Name)) & ", " &
                           Ada_String (To_String (Element (I).Pos.File)) &
                           "," & String'(Num'Img) & ");");
         Next (I);
         Num := Num + 1;
      end loop;
      Gen.Adb.UnIndent;
      Gen.Adb.Put_Line ("else");
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("Format.Start_Feature;");
      for I in 0 .. Integer (Length (Gen.Fn_Steps)) - 1 loop
         Gen.Adb.Put_Line (Element (Gen.Fn_Steps, I) &
                           " (Format, Report, First, Cond, Stop);");
      end loop;
      Gen.Adb.Put_Line ("Format.Stop_Feature;");
      Gen.Adb.UnIndent;
      Gen.Adb.Put_Line ("end if;");
      Gen.Adb.UnIndent;
      Gen.Adb.Put_Line ("end Run;");
      Gen.Ads.UnIndent;
      Gen.Adb.UnIndent;
      Gen.Adb.New_Line;
      Gen.Ads.Put_Line ("end " & Gen.Id_Pkgname & ";");
      Gen.Adb.Put_Line ("end " & Gen.Id_Pkgname & ";");
      Generate_With (Gen);

      Set_File (To_String (Gen.Ads_File), To_String (Gen.Ads.Buffer));
      Set_File (To_String (Gen.Adb_File), To_String (Gen.Adb.Buffer));
      Log.Put_Line ("Generate: " & To_String (Gen.Ads_File));
      Log.Put_Line ("Generate: " & To_String (Gen.Adb_File));
   end Generate;

   ----------------------
   --  Generate_Suite  --
   ----------------------

   procedure Generate_Suite (Gens : in Generator_Vectors.Vector;
                             Name : in String;
                             Env  : in Job_Environment;
                             Log  : in  Logger_Ptr;
                             Make : in Boolean := False)
   is
      use Generator_Vectors;
      Filename : constant String := Out_Dir (Env) & "/" & Name & ".adb";
      Gpr_Name : constant String := Out_Dir (Env) & "/" & Name & ".gpr";
      With_B   : Buffer_Type;
      Body_B   : Buffer_Type;
      Gpr_B    : Buffer_Type;
      I        : Generator_Vectors.Cursor := First (Gens);
      E        : Ada_Generator_Ptr;
      Prc_Name : constant String := To_Identifier (Name);
      Step_D   : constant String :=
         Relative_Path (Reverse_Path (Out_Dir (Env)), Step_Dir (Env));
   begin
      With_B.Put_Line ("--  File: " & Filename);
      With_B.Put_Line ("with Ada.Command_Line;");
      With_B.Put_Line ("with Ada.Real_Time;");
      With_B.Put_Line ("with AdaSpecLib;");
      With_B.Put_Line ("with AdaSpecLib.CLI;");
      With_B.Put_Line ("with AdaSpecLib.Util;");
      With_B.Put_Line ("with AdaSpecLib.Report;");
      With_B.Put_Line ("with AdaSpecLib.Format;");
      With_B.Put_Line ("with AdaSpecLib.Format.Text;");
      With_B.Put_Line ("use  Ada.Real_Time;");
      With_B.Put_Line ("use  AdaSpecLib;");
      With_B.Put_Line ("use  AdaSpecLib.CLI;");
      With_B.Put_Line ("use  AdaSpecLib.Util;");
      With_B.Put_Line ("use  AdaSpecLib.Report;");
      With_B.Put_Line ("use  AdaSpecLib.Format;");
      With_B.Put_Line ("use  AdaSpecLib.Format.Text;");
      Body_B.Put_Line ("procedure " & Prc_Name & " is");
      Body_B.Indent;
      Body_B.Put_Line ("Self_Name : constant String := " &
                       Ada_String (Prc_Name) & ";");
      Body_B.Put_Line ("Continue   : Boolean;");
      Body_B.Put_Line ("Report     : Report_Type;");
      Body_B.Put_Line ("Format     : Format_Ptr;");
      Body_B.Put_Line ("Cond       : Conditional_Type;");
      Body_B.Put_Line ("List_Mode  : Boolean;");
      Body_B.Put_Line ("Time_Start : Time;");
      Body_B.Put_Line ("Time_Stop  : Time;");
      Body_B.Put_Line ("Time_Delta : Duration := Duration (0);");
      Body_B.UnIndent;
      Body_B.Put_Line ("begin");
      Body_B.Indent;
      Body_B.Put_Line ("Parse_Arguments (Format, Continue, Cond, " &
                                        "List_Mode, Self_Name);");
      Body_B.Put_Line ("if Continue then");
      Body_B.Indent;
      Body_B.Put_Line    ("Format.Start_Tests;");
      Body_B.Put_Line    ("Time_Start := Clock;");
      while Has_Element (I) loop
         E := Ada_Generator_Ptr (Element (I));
         With_B.Put_Line ("with " & To_String (E.Id_Pkgname) & ";");
         Body_B.Put_Line (E.Full_Name & ".Run (Format, Cond, Report, " &
                                              "List_Mode);");
         Next (I);
      end loop;
      Body_B.Put_Line    ("Time_Stop := Clock;");
      Body_B.Put_Line    ("Time_Delta := To_Duration " &
                                             "(Time_Stop - Time_Start);");
      Body_B.Put_Line    ("if not List_Mode then");
      Body_B.Indent;
      Body_B.Put_Line    ("Format.Put_Summary (Report, Time_Delta);");
      Body_B.Put_Line    ("if not Status (Report) then");
      Body_B.Indent;
      Body_B.Put_Line       ("Ada.Command_Line.Set_Exit_Status " &
                             "(Ada.Command_Line.Failure);");
      Body_B.UnIndent;
      Body_B.Put_Line    ("end if;");
      Body_B.Put_Line    ("Format.Stop_Tests;");
      Body_B.UnIndent;
      Body_B.Put_Line    ("end if;");
      Body_B.UnIndent;
      Body_B.Put_Line ("end if;");
      Body_B.Put_Line ("Free (Format);");
      Body_B.UnIndent;
      Body_B.Put_Line ("exception");
      Body_B.Indent;
      Body_B.Put_Line ("when E : others =>");
      Body_B.Indent;
      Body_B.Put_Line ("Put_Exception_Information (E);");
      Body_B.Put_Line ("Ada.Command_Line.Set_Exit_Status " &
                       "(Ada.Command_Line.Failure);");
      Body_B.UnIndent;
      Body_B.UnIndent;
      Body_B.Put_Line ("end " & Prc_Name & ";");

      Gpr_B.Put_Line ("with ""adaspeclib"";");
      Gpr_B.Put_Line ("project " & Prc_Name & " is");
      Gpr_B.Indent;
      Gpr_B.Put_Line ("for Main        use (""" & Prc_Name & """);");
      Gpr_B.Put_Line ("for Source_Dirs use (""."", """ & Step_D & """);");
      Gpr_B.Put_Line ("package Compiler is");
      Gpr_B.Indent;
      Gpr_B.Put_Line ("for Default_Switches (""Ada"") use " &
                      "(""-gnat05"", ""-g"");");
      Gpr_B.UnIndent;
      Gpr_B.Put_Line ("end Compiler;");
      Gpr_B.Put_Line ("package Binder is");
      Gpr_B.Indent;
      Gpr_B.Put_Line ("for Default_Switches (""Ada"") use (""-E"");");
      Gpr_B.UnIndent;
      Gpr_B.Put_Line ("end Binder;");
      Gpr_B.UnIndent;
      Gpr_B.Put_Line ("end " & Prc_Name & ";");

      Set_File    (Filename, To_String (With_B.Buffer));
      Append_File (Filename, To_String (Body_B.Buffer));
      Log.Put_Line ("Generate: " & Filename);
      Set_File    (Gpr_Name, To_String (Gpr_B.Buffer));
      Log.Put_Line ("Generate: " & Gpr_Name);
      if Make then
         declare
            Arg1 : aliased String := "-m";
            Arg2 : aliased String := "-P" & Gpr_Name;
            Args : constant Argument_List (1 .. 2)
                 := (Arg1'Unchecked_Access, Arg2'Unchecked_Access);
            Buffer  : Unbounded_String;
            Success : Boolean;
            Code    : Integer;
         begin
            Log.Put_Line ("Build: gnatmake -m -P" & Gpr_Name);
            Spawn ("gnatmake", Args, Buffer, Success, Code);
            Log.Put_Line (Buffer);
            if not Success then
               Log.Put_Line ("--> Failure");
            elsif Code = 0 then
               Log.Put_Line ("--> Success");
            else
               Log.Put_Line ("--> Failure:" & Code'Img);
            end if;
            if not Success and Code /= 0 then
               raise Generation_Error;
            end if;
         end;
      end if;
   end Generate_Suite;

end AdaSpec.Generator.Ada05;
