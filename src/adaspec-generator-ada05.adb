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


   procedure Generate_Step     (S          : in out Ada_Generator_Type;
                                Scenario   : in     Result_Scenario_Type;
                                Step       : in     Result_Step_Type;
                                Background : in     Boolean := False);
   procedure Generate_Scenario (S          : in out Ada_Generator_Type;
                                Scenario   : in     Result_Scenario_Type;
                                Name       : in     Unbounded_String;
                                Background : in Boolean := False);
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

   ----------------
   --  Generate  --
   ----------------

   procedure Generate (Gen : in out Ada_Generator_Type;
                       Log : in     Logger_Ptr) is
      use Util.Strings.Vectors;
      First : Boolean := True;
   begin
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
      Indent (Gen.Ads);
      Indent (Gen.Adb);
      Generate_Feature (Gen);
      Gen.Ads.Put_Line ("procedure Run (Format : in out Format_Ptr;");
      Gen.Ads.Put_Line ("               Report : in out Report_Type);");
      Gen.Adb.Put_Line ("procedure Run (Format : in out Format_Ptr;");
      Gen.Adb.Put_Line ("               Report : in out Report_Type) is");
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("Stop : Boolean := False;");
      Gen.Adb.UnIndent;
      Gen.Adb.Put_Line ("begin");
      Gen.Ads.Indent;
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("Format.Start_Feature;");
      Gen.Adb.Put_Line ("Format.Put_Feature (" &
                        Ada_String (To_String (Gen.Feature.Name)) & ");");
      for I in 0 .. Integer (Length (Gen.Fn_Steps)) - 1 loop
         if First then
            Gen.Adb.Put_Line (Element (Gen.Fn_Steps, I) &
                              " (Format, Report, True, Stop);");
            First := False;
         else
            Gen.Adb.Put_Line (Element (Gen.Fn_Steps, I) &
                              " (Format, Report, False, Stop);");
         end if;
      end loop;
      Gen.Adb.Put_Line ("Format.Stop_Feature;");
      Gen.Ads.UnIndent;
      Gen.Adb.UnIndent;
      Gen.Adb.Put_Line ("end Run;");
      Gen.Ads.UnIndent;
      Gen.Adb.UnIndent;
      Gen.Ads.Put_Line ("end " & Gen.Id_Pkgname & ";");
      Gen.Adb.Put_Line ("end " & Gen.Id_Pkgname & ";");
      Generate_With (Gen);

      Set_File (To_String (Gen.Ads_File), To_String (Gen.Ads.Buffer));
      Set_File (To_String (Gen.Adb_File), To_String (Gen.Adb.Buffer));
      Log.Put_Line ("Generate: " & To_String (Gen.Ads_File));
      Log.Put_Line ("Generate: " & To_String (Gen.Adb_File));
   end Generate;

   ---------------------
   --  Generate_Step  --
   ---------------------

   procedure Generate_Step     (S          : in out Ada_Generator_Type;
                                Scenario   : in     Result_Scenario_Type;
                                Step       : in     Result_Step_Type;
                                Background : in     Boolean := False)
   is
      pragma Unreferenced (Scenario);
      package T renames String_Tables;
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
      I3       : String_Tables.Cursor;
   begin
      S.Adb.Put_Line ("Format.Start_Step;");
      --  Declare
      S.Adb.Put_Line ("declare"); ---------------------------------------------
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
      S.Adb.UnIndent;
      S.Adb.Put_Line ("begin"); -----------------------------------------------
      S.Adb.Indent;
      --  Generate arguments
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
               I3 := T.First (E2.Table);
               while T.Has_Element (I3) loop
                  S.Adb.Put_Indent;
                  S.Adb.Put ("Tble.Put (");
                  S.Adb.Put (Trim (T.Key (I3).X'Img, Left) & ", ");
                  S.Adb.Put (Trim (T.Key (I3).Y'Img, Left) & ", ");
                  S.Adb.Put (Ada_String (To_String (T.Element (I3))) & ");");
                  S.Adb.New_Line;
                  T.Next (I3);
               end loop;
               S.Adb.Put_Line ("Args.Add_Table (Tble);");
               S.Adb.UnIndent;
               S.Adb.Put_Line ("end;");
            when others =>
               null;
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
      S.Adb.Put_Line ("Format.Put_Step  (Prefix, Stanza, Args, " &
                      "Status_Skipped);");
      if Background then
         S.Adb.UnIndent;
         S.Adb.Put_Line ("end if;");
      end if;
      S.Adb.UnIndent;
      S.Adb.Put_Line ("else");
      S.Adb.Indent;
      if Procname = "" then
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
         S.Adb.Put_Line ("Format.Put_Step (Prefix, Stanza, Args, " &
                        "Status_Passed);");
         if Background then
            S.Adb.UnIndent;
            S.Adb.Put_Line ("end if;");
         end if;
      end if;
      --  End if skip
      S.Adb.UnIndent;
      S.Adb.Put_Line ("end if;");
      --  Exception
      S.Adb.UnIndent;
      S.Adb.Put_Line ("exception"); -------------------------------------------
      S.Adb.Indent;
      S.Adb.Put_Line ("when Err : others =>");
      S.Adb.Indent;
      S.Adb.Put_Line ("Report.Count_Steps_Failed := " &
                      "Report.Count_Steps_Failed + 1;");
      S.Adb.Put_Line ("Fail := True;");
      S.Adb.Put_Line ("Format.Put_Step  (Prefix, Stanza, Args, " &
                      "Status_Failed);");
      S.Adb.Put_Line ("Format.Put_Error (Err);");
      S.Adb.UnIndent;
      --  End block
      S.Adb.UnIndent;
      S.Adb.Put_Line ("end;");
      S.Adb.Put_Line ("Format.Stop_Step;");
   end Generate_Step;

   -------------------------
   --  Generate_Scenario  --
   -------------------------

   procedure Generate_Scenario (S          : in out Ada_Generator_Type;
                                Scenario   : in Result_Scenario_Type;
                                Name       : in Unbounded_String;
                                Background : in Boolean := False)
   is
      use Result_Steps;
      I    : Result_Steps.Cursor := First (Scenario.Steps);
      Proc : constant String := "procedure " & To_String (Name) & " ";
   begin
      --  declaration
      S.Ads.Put_Line (Proc & "(Format : in out Format_Ptr;");
      S.Adb.Put_Line (Proc & "(Format : in out Format_Ptr;");
      S.Ads.Put_Indent; S.Ads.Put (Proc'Length * " ");
      S.Adb.Put_Indent; S.Adb.Put (Proc'Length * " ");
      S.Ads.Put             (" Report : in out Report_Type;");
      S.Adb.Put             (" Report : in out Report_Type;");
      S.Ads.New_Line; S.Ads.Put_Indent; S.Ads.Put (Proc'Length * " ");
      S.Adb.New_Line; S.Adb.Put_Indent; S.Adb.Put (Proc'Length * " ");
      S.Ads.Put             (" First  : in     Boolean;");
      S.Adb.Put             (" First  : in     Boolean;");
      S.Ads.New_Line; S.Ads.Put_Indent; S.Ads.Put (Proc'Length * " ");
      S.Adb.New_Line; S.Adb.Put_Indent; S.Adb.Put (Proc'Length * " ");
      S.Ads.Put             (" Stop   : in out Boolean);");
      S.Adb.Put             (" Stop   : in out Boolean)");
      S.Ads.New_Line;
      S.Adb.New_Line;
      S.Adb.Put_Line ("is");
      S.Adb.Indent;
      S.Adb.Put_Line ("Fail : Boolean := Stop;");
      S.Adb.UnIndent;
      S.Adb.Put_Line ("begin");
      Indent (S.Adb);
      --  body
      if Background then
         S.Adb.Put_Line ("Format.Start_Background (First);");
      else
         S.Adb.Put_Line ("Format.Enter_Scenario;");
         S.Adb.Put_Line ("if not First then");
         S.Adb.Indent;
            S.Adb.Put_Line ("Format.Put_Scenario (" &
                            Ada_String (To_String (Scenario.Name)) & ");");
         S.Adb.UnIndent;
         S.Adb.Put_Line ("end if;");
         S.Adb.Put_Line (S.Fn_Backgnd & " (Format, Report, First, Fail);");
         S.Adb.Put_Line ("Stop := Stop or (First and Fail);");
         S.Adb.Put_Line ("Format.Start_Scenario;");
      end if;
      if Length (Scenario.Steps) = 0 then
         S.Adb.Put_Line ("null;");
      else
         S.Adb.Put_Line ("if First then");
         S.Adb.Indent;
         if Background then
            S.Adb.Put_Line ("Format.Put_Background (" &
                            Ada_String (To_String (Scenario.Name)) & ");");
         else
            S.Adb.Put_Line ("Format.Put_Scenario (" &
                            Ada_String (To_String (Scenario.Name)) & ");");
         end if;
         S.Adb.UnIndent;
         S.Adb.Put_Line ("end if;");
         while Has_Element (I) loop
            Generate_Step (S, Scenario, Element (I), Background);
            Next (I);
         end loop;
      end if;
      --  Count
      if not Background then
         S.Adb.Put_Line ("if Fail then");
         S.Adb.Indent;
         S.Adb.Put_Line ("Report.Count_Scenario_Failed := " &
                         "Report.Count_Scenario_Failed + 1;");
         S.Adb.UnIndent;
         S.Adb.Put_Line ("else");
         S.Adb.Indent;
         S.Adb.Put_Line ("Report.Count_Scenario_Passed := " &
                         "Report.Count_Scenario_Passed + 1;");
         S.Adb.UnIndent;
         S.Adb.Put_Line ("end if;");
      end if;
      --  end
      if Background then
         S.Adb.Put_Line ("Stop := Fail;");
         S.Adb.Put_Line ("Format.Stop_Background (First);");
      else
         S.Adb.Put_Line ("Format.Stop_Scenario;");
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
   begin
      Generate_Scenario (S, S.Feature.Background, S.Fn_Backgnd, True);
      while Has_Element (I) loop
         Get_Unique_String (S.Pool,
            To_Identifier ("Scenario_" & To_String (Element (I).Name)),
            Str);
         Append (S.Fn_Steps, Str);
         Generate_Scenario (S, Element (I), Str);
         Next (I);
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

   ----------------------
   --  Generate_Suite  --
   ----------------------

   procedure Generate_Suite (Gens : in Generator_Vectors.Vector;
                             Name : in String;
                             Env  : in Job_Environment;
                             Log  : in  Logger_Ptr)
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
      With_B.Put_Line ("with AdaSpecLib;");
      With_B.Put_Line ("with AdaSpecLib.CLI;");
      With_B.Put_Line ("with AdaSpecLib.Util;");
      With_B.Put_Line ("with AdaSpecLib.Report;");
      With_B.Put_Line ("with AdaSpecLib.Format;");
      With_B.Put_Line ("with AdaSpecLib.Format.Text;");
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
      Body_B.Put_Line ("Continue  : Boolean;");
      Body_B.Put_Line ("Report    : Report_Type;");
      Body_B.Put_Line ("Format    : Format_Ptr;");
      Body_B.UnIndent;
      Body_B.Put_Line ("begin");
      Body_B.Indent;
      Body_B.Put_Line ("Parse_Arguments (Format, Continue, Self_Name);");
      Body_B.Put_Line ("if Continue then");
      Body_B.Indent;
      Body_B.Put_Line    ("Format.Start_Tests;");
      while Has_Element (I) loop
         E := Ada_Generator_Ptr (Element (I));
         With_B.Put_Line ("with " & To_String (E.Id_Pkgname) & ";");
         Body_B.Put_Line (E.Full_Name & ".Run (Format, Report);");
         Next (I);
      end loop;
      Body_B.Put_Line    ("Format.Put_Summary (Report);");
      Body_B.Put_Line    ("if not Status (Report) then");
      Body_B.Indent;
      Body_B.Put_Line       ("Ada.Command_Line.Set_Exit_Status " &
                             "(Ada.Command_Line.Failure);");
      Body_B.UnIndent;
      Body_B.Put_Line    ("end if;");
      Body_B.Put_Line    ("Format.Stop_Tests;");
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
   end Generate_Suite;

end AdaSpec.Generator.Ada05;
