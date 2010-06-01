-------------------------------------------------------------------------------
--  XReq  --  Behaviour Driven Developpement tool for compiled languages     --
--  Copyright (c) 2010, SOGILIS <http://sogilis.com>                         --
--                                                                           --
--  This program is free software: you can redistribute it and/or modify     --
--  it under the terms of the GNU Affero General Public License as           --
--  published by the Free Software Foundation, either version 3 of the       --
--  License, or (at your option) any later version.                          --
--                                                                           --
--  This program is distributed in the hope that it will be useful,          --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of           --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            --
--  GNU Affero General Public License for more details.                      --
--                                                                           --
--  You should have received a copy of the GNU Affero General Public License --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.    --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Characters.Handling;
with GNAT.OS_Lib;
with Util.IO;
with XReqLib.String_Tables;
with XReq.Step_Definitions;
with XReq.Steps;
with XReq.Args;
with XReq.Language;

use Ada.Directories;
use GNAT.OS_Lib;
use Util.IO;
use XReq.Step_Definitions;
use XReq.Steps;
use XReq.Args;
use XReq.Language;

package body XReq.Generator.Ada05 is


   procedure Generate_Table    (S          : in out Ada_Generator_Type;
                                Name       : in     String;
                                T          : in     String_Tables.Table);
   procedure Generate_Step     (S          : in out Ada_Generator_Type;
                                Scenario   : in     Result_Scenario_Type;
                                Step       : in     Result_Step_Type;
                                Num        : in     Natural;
                                Background : in     Boolean := False;
                                Fake       : in     Boolean := False;
                                Outline    : in     Boolean := False);
   procedure Generate_Scenario (S          : in out Ada_Generator_Type;
                                Scenario   : in     Result_Scenario_Type;
                                Name       : in     Unbounded_String;
                                Seq_Num    : in     Integer;
                                Num_Steps  : out    Natural;
                                Background : in     Boolean := False);
   procedure Generate_Feature  (S          : in out Ada_Generator_Type;
                                Num_Steps  : out    Natural);
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
                    Job.Result.Filetype & "_" & Base_Name (Feature_File (Job));
   begin
      Assert (Job.Result.Language.Val /= null);
      Gen.Feature := Job.Result;
      Get_Unique_String (
         Gen.Pool, To_Identifier (Pkgname),      Gen.Id_Pkgname);
      Get_Unique_String (
         Gen.Pool, To_Identifier ("Background"), Gen.Fn_Backgnd);
      Basename := To_Unbounded_String (Compose (Out_Dir (Env),
                  To_Lower (To_String (Gen.Id_Pkgname))));
      Gen.Ads_File := Basename & ".ads";
      Gen.Adb_File := Basename & ".adb";
      Assert (Gen.Feature.Language.Val /= null);
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
                                Num        : in     Natural;
                                Background : in     Boolean := False;
                                Fake       : in     Boolean := False;
                                Outline    : in     Boolean := False)
   is
      pragma Unreferenced (Scenario);
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use String_Sets;
      use Match_Vectors;
      Procname : constant String := Step.Procedure_Name;
      Pkgname  : Unbounded_String;
      Copy     : Boolean := False;
      E        : Match_Location;
      E2       : Argument_Type;
   begin
      S.Adb.Put_Line ("--");
      S.Adb.Put_Line ("-- " & Num'Img & ". " & Step_Type (Step).To_String);
      S.Adb.Put_Line ("--");
      S.Adb.Put_Line ("Num_Step :=" & Num'Img & ";");
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
      S.Adb.Put      ("Prefix : constant Step_Kind := ");
      case Step.Kind is
         when Step_Given => S.Adb.Put ("Step_Given;");
         when Step_When  => S.Adb.Put ("Step_When;");
         when Step_Then  => S.Adb.Put ("Step_Then;");
      end case;
      S.Adb.New_Line;
      S.Adb.Put_Line ("Stanza : constant String    := " &
                      Ada_String (Step.Stanza) & ";");
      S.Adb.Put_Line ("Pos    : constant String    := " &
                      Ada_String (To_String (Step.Position)) & ";");
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
                      Ada_String (Step.Stanza) & ");");
      for I in Step.Match_First .. Step.Match_Last loop
         E := Step.Match_Element (I);
         S.Adb.Put_Line ("Add_Match (Args," & E.First'Img & "," &
                                     E.Last'Img & ");");
      end loop;
      for I2 in Step.Arg_First .. Step.Arg_Last loop
         E2 := Step.Arg_Element (I2);
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
      end loop;
      S.Adb.Put_Line ("Add_Sep   (Args, 1);");
      --  Skip if failure
      S.Adb.Put_Line ("if Fail then");
      S.Adb.Indent;
      if not Fake then
         S.Adb.Put_Line ("Report.Count_Steps_Skipped := " &
                         "Report.Count_Steps_Skipped + 1;");
      end if;
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
         S.Adb.Put_Line ("raise XReqLib.Not_Yet_Implemented");
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
                                Scenario   : in     Result_Scenario_Type;
                                Name       : in     Unbounded_String;
                                Seq_Num    : in     Integer;
                                Num_Steps  : out    Natural;
                                Background : in     Boolean := False)
   is
      use String_Vectors;
      N, M        : Integer;
      Proc        : constant String := "procedure " & To_String (Name) & " ";
      First       : Boolean := True;
      Steps_Count : Natural := 0;
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
      S.Ads.Put_Line (Proc & "(Format     : in out Format_Ptr;");
      S.Adb.Put_Line (Proc & "(Format     : in out Format_Ptr;");
      S.Ads.Put_Indent; S.Ads.Put (Proc'Length * " ");
      S.Adb.Put_Indent; S.Adb.Put (Proc'Length * " ");
      S.Ads.Put             (" Report     : in out Report_Type;");
      S.Adb.Put             (" Report     : in out Report_Type;");
      S.Ads.New_Line; S.Ads.Put_Indent; S.Ads.Put (Proc'Length * " ");
      S.Adb.New_Line; S.Adb.Put_Indent; S.Adb.Put (Proc'Length * " ");
      S.Ads.Put             (" First      : in out Boolean;");
      S.Adb.Put             (" First      : in out Boolean;");
      S.Ads.New_Line; S.Ads.Put_Indent; S.Ads.Put (Proc'Length * " ");
      S.Adb.New_Line; S.Adb.Put_Indent; S.Adb.Put (Proc'Length * " ");
      S.Ads.Put             (" Cond       : in     Conditional_Type;");
      S.Adb.Put             (" Cond       : in     Conditional_Type;");
      S.Ads.New_Line; S.Ads.Put_Indent; S.Ads.Put (Proc'Length * " ");
      S.Adb.New_Line; S.Adb.Put_Indent; S.Adb.Put (Proc'Length * " ");
      S.Ads.Put             (" Stop       : in out Boolean;");
      S.Adb.Put             (" Stop       : in out Boolean;");
      S.Ads.New_Line; S.Ads.Put_Indent; S.Ads.Put (Proc'Length * " ");
      S.Adb.New_Line; S.Adb.Put_Indent; S.Adb.Put (Proc'Length * " ");
      S.Ads.Put             (" Count_Mode : in     Boolean := False);");
      S.Adb.Put             (" Count_Mode : in     Boolean := False)");
      S.Ads.New_Line;
      S.Adb.New_Line;
      S.Ads.New_Line;
      S.Adb.New_Line;
      S.Adb.Put_Line ("is");
      S.Adb.Indent;
      if Scenario.Outline then
         S.Adb.Put_Line ("Outline_Table : Table_Type;");
      end if;
      S.Adb.Put_Line    ("Num_Step      : Natural := 0;");
      S.Adb.Put_Line    ("Num_Scenario  : constant Natural :=" &
                                          Seq_Num'Img & ";");
      S.Adb.Put_Line    ("Fail          : Boolean := Stop;");
      S.Adb.Put_Line    ("Tags          : constant " &
                     "XReqLib.Format.Tag_Array_Type (1 .." &
                     String'(Scenario.Tag_Count'Img) & ") :=");
      S.Adb.Put_Indent;
      S.Adb.Put ("      (");
      for I in Scenario.Tag_First .. Scenario.Tag_Last loop
         if I > 0 then
            S.Adb.Put (", ");
            S.Adb.New_Line;
            S.Adb.Put_Indent;
            S.Adb.Put ("       ");
         end if;
         S.Adb.Put (String'(Integer'Image (I + 1)) &
                    " => Ada.Strings.Unbounded.To_Unbounded_String (" &
                    Ada_String (Scenario.Tag_Element (I)) & ")");
      end loop;
      if Scenario.Tag_Count = 0 then
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
         S.Adb.Put_Line ("if not Count_Mode then");
         S.Adb.Indent;
         S.Adb.Put_Line ("Format.Start_Background (First);");
      else
         S.Adb.Put_Line ("if Cond.Eval (Tags) and then Cond.Eval (" &
                         Ada_String (To_String (Scenario.Position.File)) &
                         ", Num_Scenario) then");
         S.Adb.Indent;
         S.Adb.Put_Line ("if not Count_Mode then");
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
         S.Adb.Put (" (" & Ada_String (Scenario.Name) & ", " &
                           Ada_String (To_String (Scenario.Position)) & ", " &
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
      M := 1;
      if not Background or else Scenario.Step_Count /= 0 then
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
                              Ada_String (Scenario.Name) & ", " &
                              Ada_String (To_String (Scenario.Position)) &
                              ", " & "Tags);");
         elsif Scenario.Outline then
            S.Adb.Put_Line ("Format.Put_Outline (" &
                              Ada_String (Scenario.Name) & ", " &
                              Ada_String (To_String (Scenario.Position)) &
                              ", " & "Tags);");
         else
            S.Adb.Put_Line ("Format.Put_Scenario (" &
                              Ada_String (Scenario.Name) & ", " &
                              Ada_String (To_String (Scenario.Position)) &
                              ", " & "Tags);");
         end if;
         S.Adb.UnIndent;
         S.Adb.Put_Line ("end if;");
         for I in Scenario.Step_First .. Scenario.Step_Last loop
            Generate_Step (S, Scenario, Scenario.Step_Element (I), M,
                           Background, Scenario.Outline, Scenario.Outline);
            M := M + 1;
            if not Scenario.Outline then
               Steps_Count := Steps_Count + 1;
            end if;
         end loop;
      end if;
      if Scenario.Outline then
         N := 0;
         for J in Scenario.Outline_First .. Scenario.Outline_Last loop
            N := N + 1;
            S.Adb.Put_Line ("--------------------------");
            S.Adb.Put_Line ("--  Generated Scenario  --");
            S.Adb.Put_Line ("--------------------------");
            S.Adb.Put_Line ("Format.Enter_Scenario;");
            if not First then
               S.Adb.Put_Line (S.Fn_Backgnd &
                              " (Format, Report, First, Cond, Fail);");
               S.Adb.Put_Line ("Stop := Stop or (First and Fail);");
               S.Adb.Put_Line ("Fail := Stop;");
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
                            Ada_String (Scenario.Name) & ", " &
                            Ada_String (To_String (Scenario.Position)) & ", " &
                            "Tags);");
            S.Adb.Put_Line ("end Priv_Put_Scenario;");
            S.Adb.UnIndent;
            S.Adb.Put_Line ("begin");
            S.Adb.Indent;
            for I in Scenario.Outline_Step_First (J) ..
                              Scenario.Outline_Step_Last (J)
            loop
               Generate_Step (S, Scenario,
                              Scenario.Outline_Step_Element (J, I), M,
                              Background, False, True);
               M := M + 1;
               Steps_Count := Steps_Count + 1;
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
         S.Adb.UnIndent;
         S.Adb.Put_Line ("else  --  Count_Mode");
         S.Adb.Put_Line ("   Report.Num_Steps := Report.Num_Steps +" &
                                                 Steps_Count'Img & ";");
         S.Adb.Put_Line ("end if;");
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
         S.Adb.Put_Line ("else  --  Count_Mode");
         S.Adb.Indent;
         if Scenario.Outline then
            for J in Scenario.Outline_First .. Scenario.Outline_Last loop
               S.Adb.Put_Line (S.Fn_Backgnd &
                               " (Format, Report, First, Cond, Fail, True);");
            end loop;
         else
            S.Adb.Put_Line (S.Fn_Backgnd &
                            " (Format, Report, First, Cond, Fail, True);");
         end if;
         S.Adb.Put_Line ("Report.Num_Steps := Report.Num_Steps +" &
                                              Steps_Count'Img & ";");
         S.Adb.UnIndent;
         S.Adb.Put_Line ("end if;");
         S.Adb.UnIndent;
         S.Adb.Put_Line ("end if;");
      end if;
      S.Adb.UnIndent;
      S.Adb.Put_Line ("end " & Name & ";");
      Num_Steps := Steps_Count;
   end Generate_Scenario;

   ------------------------
   --  Generate_Feature  --
   ------------------------

   procedure Generate_Feature  (S           : in out Ada_Generator_Type;
                                Num_Steps   : out    Natural)
   is
      use String_Vectors;
      Str : Unbounded_String;
      N   : Positive := 1;
      Num_Steps_Scenario   : Natural := 0;
      Num_Steps_Background : Natural := 0;
      Total_Steps          : Natural := 0;
   begin
      Generate_Scenario (S, S.Feature.Background, S.Fn_Backgnd, 0,
                         Num_Steps_Background, True);
      for I in S.Feature.Scenario_First .. S.Feature.Scenario_Last loop
         Get_Unique_String (S.Pool,
            To_Identifier ("Scenario_" & S.Feature.Scenario_Element (I).Name),
            Str);
         Append (S.Fn_Steps, Str);
         Generate_Scenario (S, S.Feature.Scenario_Element (I), Str, N,
                            Num_Steps_Scenario);
         N := N + 1;
         Total_Steps := Num_Steps_Background + Num_Steps_Scenario;
      end loop;
      Num_Steps := Total_Steps;
   end Generate_Feature;

   ---------------------
   --  Generate_With  --
   ---------------------

   procedure Generate_With (S : in out Ada_Generator_Type)
   is
      use String_Sets;
      J   : String_Sets.Cursor := First (S.With_Pkg);
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
      use String_Vectors;
      E           : Result_Scenario_Type;
      Num         : Positive := 1;
      Total_Steps : Natural := 0;
      Lang        : constant Language_Ptr := Gen.Feature.Language.Val;
   begin
      Assert (Gen.Feature.Language.Val /= null);
      Assert (Lang /= null);
      if Lang = null then
         raise Program_Error with "Lang = null";
      end if;
      Gen.Ads.Put_Line ("with Ada.Strings.Unbounded;");
      Gen.Ads.Put_Line ("with XReqLib;");
      Gen.Ads.Put_Line ("with XReqLib.Args;");
      Gen.Ads.Put_Line ("with XReqLib.Report;");
      Gen.Ads.Put_Line ("with XReqLib.Format;");
      Gen.Ads.Put_Line ("use  XReqLib;");
      Gen.Ads.Put_Line ("use  XReqLib.Args;");
      Gen.Ads.Put_Line ("use  XReqLib.Report;");
      Gen.Ads.Put_Line ("use  XReqLib.Format;");
      Gen.Ads.Put_Line ("package " & Gen.Id_Pkgname & " is");
      Gen.Adb.Put_Line ("package body " & Gen.Id_Pkgname & " is");
      Gen.Ads.Indent;
      Gen.Adb.Indent;
      Gen.Adb.New_Line;
      Gen.Adb.Put_Line ("Str_Feature    : constant String := " &
                                          Ada_String (Lang.Feature) & ";");
      Gen.Adb.Put_Line ("Str_Background : constant String := " &
                                          Ada_String (Lang.Background) & ";");
      Gen.Adb.Put_Line ("Str_Scenario   : constant String := " &
                                          Ada_String (Lang.Scenario) & ";");
      Gen.Adb.Put_Line ("Str_Outline    : constant String := " &
                                     Ada_String (Lang.Scenario_Outline) & ";");
      Gen.Adb.Put_Line ("Str_Examples   : constant String := " &
                                          Ada_String (Lang.Examples) & ";");
      Gen.Adb.Put_Line ("Str_Given      : constant String := " &
                                          Ada_String (Lang.Given) & ";");
      Gen.Adb.Put_Line ("Str_When       : constant String := " &
                                          Ada_String (Lang.When_K) & ";");
      Gen.Adb.Put_Line ("Str_Then       : constant String := " &
                                          Ada_String (Lang.Then_K) & ";");
      Gen.Adb.Put_Line ("Str_And        : constant String := " &
                                          Ada_String (Lang.And_K) & ";");
      Gen.Adb.New_Line;
      Gen.Adb.Put_Line ("procedure Priv_Feature " &
                              "(Format : in out Format_Ptr);");
      Gen.Adb.New_Line;
      Gen.Adb.Put_Line ("procedure Priv_Feature " &
                              "(Format : in out Format_Ptr) is");
      Gen.Adb.Put_Line ("begin");
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("Format.Put_Feature (" &
                        Ada_String (Gen.Feature.Name) & ", " &
                        Ada_String (Gen.Feature.Description) &
                        ", " &
                        Ada_String (To_String (Gen.Feature.Position)) & ");");
      Gen.Adb.UnIndent;
      Gen.Adb.Put_Line ("end Priv_Feature;");
      Generate_Feature (Gen, Total_Steps);
      Gen.Ads.New_Line;
      Gen.Ads.Put_Line ("Num_Steps : constant Natural :=" &
                                     Total_Steps'Img & ";");
      Gen.Ads.New_Line;
      Gen.Adb.New_Line;
      Gen.Ads.Put_Line ("procedure Run (Format     : in out Format_Ptr;");
      Gen.Ads.Put_Line ("               Cond       : in Conditional_Type;");
      Gen.Ads.Put_Line ("               Report     : in out Report_Type;");
      Gen.Ads.Put_Line ("               List_Mode  : in Boolean := False;");
      Gen.Ads.Put_Line ("               Count_Mode : in Boolean := False);");
      Gen.Adb.Put_Line ("procedure Run (Format     : in out Format_Ptr;");
      Gen.Adb.Put_Line ("               Cond       : in Conditional_Type;");
      Gen.Adb.Put_Line ("               Report     : in out Report_Type;");
      Gen.Adb.Put_Line ("               List_Mode  : in Boolean := False;");
      Gen.Adb.Put_Line ("               Count_Mode : in Boolean := False) is");
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("Stop  : Boolean  := False;");
      Gen.Adb.Put_Line ("First : Boolean  := True;");
      Gen.Adb.UnIndent;
      Gen.Adb.Put_Line ("begin");
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("if List_Mode then");
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("Format.List_Feature (" &
                        Ada_String (Gen.Feature.Name) & ");");
      for I in Gen.Feature.Scenario_First .. Gen.Feature.Scenario_Last loop
         E := Gen.Feature.Scenario_Element (I);
         Gen.Adb.Put_Line ("Format.List_Scenario (" &
                           Ada_String (E.Name) & ", " &
                           Ada_String (To_String (E.Position.File)) &
                           "," & String'(Num'Img) & ");");
         Num := Num + 1;
      end loop;
      Gen.Adb.UnIndent;
      Gen.Adb.Put_Line ("else");
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("if not Count_Mode then");
      Gen.Adb.Put_Line ("   Format.Start_Feature;");
      Gen.Adb.Put_Line ("end if;");
      for I in 0 .. Integer (Length (Gen.Fn_Steps)) - 1 loop
         Gen.Adb.Put_Line (Element (Gen.Fn_Steps, I) & " " &
                           "(Format, Report, First, Cond, Stop, Count_Mode);");
      end loop;
      Gen.Adb.Put_Line ("if not Count_Mode then");
      Gen.Adb.Put_Line ("   Format.Stop_Feature;");
      Gen.Adb.Put_Line ("end if;");
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
      procedure Put_GPR_With (Str : in String);
      procedure Put_GPR_Path (Str : in String);
      use Generator_Vectors;
      use String_Vectors;
      use Ada.Strings.Fixed;
      Filename : constant String := Out_Dir (Env) & "/" & Name & ".adb";
      Gpr_Name : constant String := Out_Dir (Env) & "/" & Name & ".gpr";
      With_B   : Buffer_Type;
      Body_B   : Buffer_Type;
      Gpr_B    : Buffer_Type;
      I        : Generator_Vectors.Cursor;
      E        : Ada_Generator_Ptr;
      Prc_Name : constant String := To_Identifier (Name);
      Splitter : Split_String_Type;

      procedure Put_GPR_With (Str : in String) is
      begin
         if Str /= "" then
            Gpr_B.Put_Line ("with " & Ada_String (Str) & ";");
         end if;
      end Put_GPR_With;

      procedure Put_GPR_Path (Str : in String) is
      begin
         if Str /= "" then
            Gpr_B.Put_Line (", " & Ada_String (Str));
         end if;
      end Put_GPR_Path;

   begin
      With_B.Put_Line ("--  File: " & Filename);
      With_B.Put_Line ("with Ada.Command_Line;");
      With_B.Put_Line ("with Ada.Real_Time;");
      With_B.Put_Line ("with XReqLib;");
      With_B.Put_Line ("with XReqLib.CLI;");
      With_B.Put_Line ("with XReqLib.Util;");
      With_B.Put_Line ("with XReqLib.Report;");
      With_B.Put_Line ("with XReqLib.Format;");
      With_B.Put_Line ("with XReqLib.Format.Text;");
      With_B.Put_Line ("use  Ada.Real_Time;");
      With_B.Put_Line ("use  XReqLib;");
      With_B.Put_Line ("use  XReqLib.CLI;");
      With_B.Put_Line ("use  XReqLib.Util;");
      With_B.Put_Line ("use  XReqLib.Report;");
      With_B.Put_Line ("use  XReqLib.Format;");
      With_B.Put_Line ("use  XReqLib.Format.Text;");
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
      Body_B.Put_Line    ("--  Count Steps");
      I := First (Gens);
      while Has_Element (I) loop
         E := Ada_Generator_Ptr (Element (I));
         With_B.Put_Line ("with " & To_String (E.Id_Pkgname) & ";");
         Body_B.Put_Line (E.Full_Name & ".Run (Format, Cond, Report, " &
                                              "Count_Mode => True);");
         Next (I);
      end loop;
      Body_B.Put_Line    ("Format.Set_Num_Steps (Report.Num_Steps);");
      Body_B.Put_Line    ("--  Run Steps");
      I := First (Gens);
      while Has_Element (I) loop
         E := Ada_Generator_Ptr (Element (I));
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

      Gpr_B.Put_Line ("with ""xreqlib"";");
      Split_String_Start (Splitter,
                          Get_Option (Env, "ada.gpr.with", ""), ",");
      while Split_String_Has_Next (Splitter) loop
         Put_GPR_With (Split_String_Current (Splitter));
         Split_String_Next (Splitter);
      end loop;
      Gpr_B.New_Line;
      Gpr_B.Put_Line ("project " & Prc_Name & " is");
      Gpr_B.Indent;
      Gpr_B.Put_Line ("for Languages   use (""Ada"");");
      Gpr_B.Put_Line ("for Main        use (""" & Prc_Name & """);");
      Gpr_B.Put_Indent;
      Gpr_B.Put      ("for Source_Dirs use ("".""");

      for I in First_Index (Env.Step_Dir) .. Last_Index (Env.Step_Dir) loop
         Gpr_B.Put (", """ & Goto_Path (Out_Dir (Env),
                                        To_String (Element (Env.Step_Dir, I)))
                    & """");
      end loop;
      Split_String_Start (Splitter,
                          Get_Option (Env, "ada.gpr.srcdir", ""), ",");
      while Split_String_Has_Next (Splitter) loop
         Put_GPR_Path (Split_String_Current (Splitter));
         Split_String_Next (Splitter);
      end loop;
      Gpr_B.Put      (");");
      Gpr_B.New_Line;
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
      declare
         Results   : Search_Type;
         Dir_Entry : Directory_Entry_Type;
         Generated : Boolean;
      begin
         Start_Search (Results,
                       Directory => Out_Dir (Env),
                       Pattern   => "*.ad[bs]",
                       Filter    => (Ordinary_File => True, others => False));
         while More_Entries (Results) loop
            Get_Next_Entry (Results, Dir_Entry);
            Generated := Simple_Name (Dir_Entry) = Name & ".adb";
            I := First (Gens);
            while not Generated and then Has_Element (I) loop
               E := Ada_Generator_Ptr (Element (I));
               Generated := Simple_Name (To_String (E.Ads_File)) =
                                         Simple_Name (Dir_Entry) or
                            Simple_Name (To_String (E.Adb_File)) =
                                         Simple_Name (Dir_Entry);
               Next (I);
            end loop;
            if not Generated then
               declare
                  File     : constant String :=
                             Compose (Out_Dir (Env), Simple_Name (Dir_Entry));
                  File_O   : constant String :=
                             File (File'First .. File'Last - 3) & "o";
                  File_ALI : constant String :=
                             File (File'First .. File'Last - 3) & "ali";
               begin
                  Log.Put_Line ("Delete: " & File);
                  Delete_File (File);
                  --  This is ugly as hell, but what can you do with Ada?
                  begin
                     Delete_File (File_O);
                     Log.Put_Line ("Delete: " & File_O);
                  exception
                     when Name_Error => null;
                  end;
                  begin
                     Delete_File (File_ALI);
                     Log.Put_Line ("Delete: " & File_ALI);
                  exception
                     when Name_Error => null;
                  end;
               end;
            end if;
         end loop;
         End_Search (Results);
      end;
      if Make then
         declare
            Arg1    : aliased String := "-m";
            Arg2    : aliased String := "-P" & Gpr_Name;
            Args    : constant Argument_List (1 .. 2)
                    := (Arg1'Unchecked_Access, Arg2'Unchecked_Access);
--                     := (Arg2'Unchecked_Access, others => <>);
            Args2   : Argument_List_Access :=
                      Argument_String_To_List (GetEnv ("GNAT_FLAGS", ""));
            Buffer  : Unbounded_String;
            Success : Boolean;
            Code    : Integer;
         begin
            Log.Put_Line ("Build: gnatmake -m -P" & Gpr_Name);
            Spawn ("gnatmake", Args & Args2.all, Buffer, Success, Code);
            Free (Args2);
            Log.Put_Line (Buffer);
            if not Success then
               Log.Put_Line ("--> Failure");  --  GCOV_IGNORE
               --  Should never happend but better be safe than sorry
            elsif Code = 0 then
               Log.Put_Line ("--> Success");
            else
               Log.Put_Line ("--> Failure:" & Code'Img);  --  GCOV_IGNORE
            end if;
            if not Success or Code /= 0 then    --  GCOV_IGNORE_BEGIN
               raise Generation_Error;          --  Same here
            end if;                             --  GCOV_IGNORE_END
         end;
      end if;
   end Generate_Suite;

end XReq.Generator.Ada05;
