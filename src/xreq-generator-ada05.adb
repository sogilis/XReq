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
with XReq.Steps.Result.Handles;
with XReq.Args;
with XReq.Language.Handles;
with XReq.Steps.Result;
with XReq.Scenarios.Result;
with XReq.Scenarios.Result.Handles;

use Ada.Directories;
use GNAT.OS_Lib;
use Util.IO;
use XReq.Step_Definitions;
use XReq.Steps.Result.Handles;
use XReq.Args;
use XReq.Language.Handles;
use XReq.Steps.Result;
use XReq.Scenarios.Result;
use XReq.Scenarios.Result.Handles;

package body XReq.Generator.Ada05 is

   pragma Style_Checks (Off);


   procedure Generate_Table      (S          : in out Ada_Generator_Type;
                                  Name       : in     String;
                                  T          : in     String_Tables.Table);
   procedure Generate_Step       (S          : in out Ada_Generator_Type;
                                  Scenario   : in     Result_Scenario_Handle;
                                  Step       : in     Result_Step_Handle;
                                  Fake       : in     Boolean := False);
   procedure Generate_Scenario   (S          : in out Ada_Generator_Type;
                                  Scenario   : in     Result_Scenario_Handle;
                                  Name       : in     Unbounded_String;
                                  Seq_Num    : in     Integer);
   procedure Generate_Background (S          : in out Ada_Generator_Type;
                                  Scenario   : in     Result_Scenario_Handle;
                                  Name       : in     Unbounded_String);

   procedure Generate_Feature    (S          : in out Ada_Generator_Type);
   --  Generate scenario procedures for the feature

   procedure Generate_With       (S          : in out Ada_Generator_Type);
   --  Generate with clauses

   ------------
   --  Make  --
   ------------

   procedure Make     (Gen : out    Ada_Generator_Type;
                       Job : in     Job_Type;
                       Env : in     Environment_Handle)
   is
      use Ada.Characters.Handling;
      Basename    : Unbounded_String;
      Pkgname     : constant String :=
                    Job.Result.R.Filetype & "_" & Base_Name (Job.Feature_File);
   begin
      Gen.Feature := Job.Result;
      Get_Unique_String (
         Gen.Pool, To_Identifier (Pkgname),      Gen.Id_Pkgname);
      Get_Unique_String (
         Gen.Pool, To_Identifier ("Background"), Gen.Fn_Backgnd);
      Basename := To_Unbounded_String (Compose (Env.Ref.Out_Dir,
                  To_Lower (To_String (Gen.Id_Pkgname))));
      Gen.Ads_File := Basename & ".ads";
      Gen.Adb_File := Basename & ".adb";
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
                                Scenario   : in     Result_Scenario_Handle;
                                Step       : in     Result_Step_Handle;
                                Fake       : in     Boolean := False)
   is
      pragma Unreferenced (Scenario);
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use String_Sets;
      use Match_Vectors;
      Procname : constant String := Step.R.Procedure_Name;
      Pkgname  : Unbounded_String;
      Copy     : Boolean := False;
      E        : Match_Location;
      E2       : Argument_Type;
   begin
      S.Adb.Put_Line ("--");
      S.Adb.Put_Line ("-- " & Step.R.To_String);
      S.Adb.Put_Line ("--");

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
      case Step.R.Kind is
         when Step_Given => S.Adb.Put ("Step_Given;");
         when Step_When  => S.Adb.Put ("Step_When;");
         when Step_Then  => S.Adb.Put ("Step_Then;");
      end case;
      S.Adb.New_Line;
      S.Adb.Put_Line ("Stanza : constant String    := " &
                      Ada_String (Step.R.Stanza) & ";");
      S.Adb.Put_Line ("Pos    : constant String    := " &
                      Ada_String (To_String (Step.R.Position)) & ";");
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
                      Ada_String (Step.R.Stanza) & ");");
      for I in Step.R.Match_First .. Step.R.Match_Last loop
         E := Step.R.Match_Element (I);
         S.Adb.Put_Line ("Add_Match (Args," & E.First'Img & "," &
                                     E.Last'Img & ");");
      end loop;
      for I2 in Step.R.Arg_First .. Step.R.Arg_Last loop
         E2 := Step.R.Arg_Element (I2);
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

      ----------------
      --  Run Step  --
      -------------------------------------------------------------------------
      S.Adb.Put_Line ("Format.Start_Step (Prefix, Stanza, Pos);");
      S.Adb.Put_Line ("Format.Begin_Step;");
      --  Skip if failure
      S.Adb.Put_Line ("if Fail then");
      S.Adb.Indent;
      if not Fake then
         S.Adb.Put_Line ("Report.Count_Steps_Skipped := " &
                         "Report.Count_Steps_Skipped + 1;");
      end if;
      S.Adb.Put_Line ("Format.Put_Step  (Args, Status_Skipped);");
      S.Adb.UnIndent;
      S.Adb.Put_Line ("else");
      S.Adb.Indent;
      if Fake then
         S.Adb.Put_Line ("Format.Put_Step (Args, Status_Outline);");
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
         S.Adb.Put_Line ("Call_Hook (Hook_Begin, Hook_Step);");
         S.Adb.Put_Line (Procname & " (Args);");
         S.Adb.Put_Line ("Call_Hook (Hook_End, Hook_Step);");
         --  Count step
         S.Adb.Put_Line ("Report.Count_Steps_Passed := " &
                         "Report.Count_Steps_Passed + 1;");
         --  Print the step
         S.Adb.Put_Line ("Format.Put_Step (Args, Status_Passed);");
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
         S.Adb.Put_Line ("     Call_Hook (Hook_End, Hook_Step);");
         S.Adb.Put_Line ("     Report.Count_Steps_Failed := " &
                                 "Report.Count_Steps_Failed + 1;");
         S.Adb.Put_Line ("     Fail := True;");
         S.Adb.Put_Line ("     Format.Put_Step  (Args, Status_Failed);");
         S.Adb.Put_Line ("     Format.Put_Error (Err);");
      end if;
      --  End block
      S.Adb.Put_Line ("end;");
      S.Adb.Put_Line ("Format.End_Step;");
      S.Adb.Put_Line ("Format.Stop_Step;");
   end Generate_Step;

   procedure Generate_Tag_Variable (S          : in out Ada_Generator_Type;
                                    Scenario   : in     Result_Scenario_Handle)
   is
      function Generate_Tag_List_Type
        (Scenario : Result_Scenario_Handle) return String
      is
      begin
         return "XReqLib.Format.Tag_Array_Type (1 .."
           & String'(Scenario.R.Tag_Count'Img) & ")";
      end Generate_Tag_List_Type;

      procedure Generate_Tag_List
        (S          : in out Ada_Generator_Type;
         Scenario   : in     Result_Scenario_Handle)
      is
      begin
         S.Adb.Put ("(");
         for I in Scenario.R.Tag_First .. Scenario.R.Tag_Last loop
            if I > 0 then
               S.Adb.Put (", ");
               S.Adb.New_Line;
               S.Adb.Put_Indent;
               S.Adb.Put ("       ");
            end if;
            S.Adb.Put (String'(Integer'Image (I + 1)) &
                       " => Ada.Strings.Unbounded.To_Unbounded_String (" &
                       Ada_String (Scenario.R.Tag_Element (I)) & ")");
         end loop;
         if Scenario.R.Tag_Count = 0 then
            S.Adb.Put
              ("others => Ada.Strings.Unbounded.Null_Unbounded_String");
         end if;
         S.Adb.Put (")");
      end Generate_Tag_List;
   begin
      S.Adb.Put_Line    ("Tags : constant " & Generate_Tag_List_Type (Scenario)
                         & " :=");
      S.Adb.Put_Indent;
      S.Adb.Put ("      ");
      Generate_Tag_List (S, Scenario);
      S.Adb.Put (";");
   end Generate_Tag_Variable;

   ---------------------------
   --  Generate_Background  --
   ---------------------------

   procedure Generate_Background (S          : in out Ada_Generator_Type;
                                  Scenario   : in     Result_Scenario_Handle;
                                  Name       : in     Unbounded_String)
   is
      use String_Vectors;
      Proc        : constant String := "procedure " & To_String (Name) & " ";
   begin
      pragma Assert (Scenario.Valid);

      -------------------------------------------------------------------------
      --  Declaration  --------------------------------------------------------
      -------------------------------------------------------------------------

      --  ARGUMENTS:

      --  Format     : Object that handle writing what happens
      --  Report     : Counters of passed, skipped and faikled steps/scenarios
      --  Stop       : Set to True if there is a step that fails.
      --  Count_Mode : Only return the number of steps in Report

      --  VARIABLES:

      --  Fail : True if a step failed. In that case, all other steps are
      --         skipped
      --  Tags : Array of tags

      S.Adb.New_Line;
      S.Ads.Put_Line (Proc & "(Format     : in out Format_Ptr;");
      S.Adb.Put_Line (Proc & "(Format     : in out Format_Ptr;");
      S.Ads.Put_Indent; S.Ads.Put (Proc'Length * " ");
      S.Adb.Put_Indent; S.Adb.Put (Proc'Length * " ");
      S.Ads.Put             (" Report     : in out Report_Type;");
      S.Adb.Put             (" Report     : in out Report_Type;");
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
      S.Adb.Put_Line    ("Fail : Boolean := Stop;");
      Generate_Tag_Variable (S, Scenario);
      S.Adb.New_Line;
      S.Adb.UnIndent;
      S.Adb.Put_Line ("begin");
      Indent (S.Adb);

      -------------------------------------------------------------------------
      --  Body  ---------------------------------------------------------------
      -------------------------------------------------------------------------

      S.Adb.Put_Line ("if Count_Mode then");
      S.Adb.Put_Line ("   Report.Num_Steps := Report.Num_Steps +" & Scenario.R.Step_Count'Img & ";");
      S.Adb.Put_Line ("else");
      S.Adb.Indent;
      S.Adb.Put_Line ("Format.Start_Background ("
                      & Ada_String (Scenario.R.Name) & ", "
                      & Ada_String (To_String (Scenario.R.Position)) & ");");
      if Scenario.R.Step_Count /= 0 then
         S.Adb.Put_Line ("Format.Begin_Background;");
         S.Adb.Put_Line ("Format.Put_Background;");
         for I in Scenario.R.Step_First .. Scenario.R.Step_Last loop
            Generate_Step (S, Scenario, Scenario.R.all.Step_Element (I));
         end loop;
         S.Adb.Put_Line ("Format.End_Background;");
      end if;
      S.Adb.Put_Line ("--------------------");
      S.Adb.Put_Line ("--  Finalization  --");
      S.Adb.Put_Line ("--------------------");
      S.Adb.Put_Line ("Stop := Fail;");
      S.Adb.Put_Line ("Format.Stop_Background;");
      S.Adb.UnIndent;
      S.Adb.Put_Line ("end if;");
      S.Adb.UnIndent;
      S.Adb.Put_Line ("end " & Name & ";");
   end Generate_Background;

   ------------------------------
   --  Generate_Scenario_Body  --
   ------------------------------

   procedure Generate_Scenario_Body
     (S          : in out Ada_Generator_Type;
      Scenario   : in     Result_Scenario_Handle;
      Outline_ID : in     Integer := -1)
   is
   begin
      S.Adb.Put_Line ("if Count_Mode then");
      S.Adb.Indent;
      S.Adb.Put_Line (S.Fn_Backgnd &
                      " (Format, Report, Stop, True);");
      S.Adb.Put_Line ("Report.Num_Steps := Report.Num_Steps +" &
                      Scenario.R.Step_Count'Img & ";");
      S.Adb.UnIndent;
      S.Adb.Put_Line ("else");
      S.Adb.Indent;

      --  Scenario Start
      -------------------
      S.Adb.Put_Line ("Format.Start_Scenario (" &
                      Ada_String (Scenario.R.Name) & ", " &
                      Ada_String (To_String (Scenario.R.Position)) &
                      ", " & "Tags);");
      S.Adb.Put_Line ("Format.Enter_Scenario;");
      S.Adb.Put_Line ("Call_Hook (Hook_Begin, Hook_Scenario);");

      --  Call Background
      ---------------------
      S.Adb.New_Line;
      S.Adb.Put_Line ("------------------");
      S.Adb.Put_Line ("--  Background  --");
      S.Adb.Put_Line ("------------------");
      S.Adb.New_Line;
      S.Adb.Put_Line (S.Fn_Backgnd &
                      " (Format, Report, Stop, Count_Mode);");
      S.Adb.Put_Line ("Fail := Stop;");

      --  Call Scenario Steps
      -------------------------
      S.Adb.New_Line;
      S.Adb.Put_Line ("----------------");
      S.Adb.Put_Line ("--  Scenario  --");
      S.Adb.Put_Line ("----------------");
      S.Adb.New_Line;
      S.Adb.Put_Line ("Format.Begin_Scenario;");

      if Outline_ID < 0 then
         for I in Scenario.R.Step_First .. Scenario.R.Step_Last loop
            Generate_Step (S, Scenario, Scenario.R.all.Step_Element (I));
         end loop;
      else
         for I in Scenario.R.Outline_Step_First (Outline_ID)
           .. Scenario.R.Outline_Step_Last (Outline_ID)
         loop
            Generate_Step
              (S, Scenario,
               Scenario.R.all.Outline_Step_Element (Outline_ID, I));
         end loop;
      end if;

      --  Finalize
      --------------
      S.Adb.New_Line;
      S.Adb.Put_Line ("--------------------");
      S.Adb.Put_Line ("--  Finalization  --");
      S.Adb.Put_Line ("--------------------");
      S.Adb.New_Line;
      S.Adb.Put_Line ("Call_Hook (Hook_End, Hook_Scenario);");
      S.Adb.Put_Line ("if Fail then");
      S.Adb.Put_Line ("   Report.Count_Scenario_Failed := " &
                         "Report.Count_Scenario_Failed + 1;");
      S.Adb.Put_Line ("else");
      S.Adb.Put_Line ("   Report.Count_Scenario_Passed := " &
                         "Report.Count_Scenario_Passed + 1;");
      S.Adb.Put_Line ("end if;");
      S.Adb.Put_Line ("Format.End_Scenario;");
      S.Adb.Put_Line ("Format.Stop_Scenario;");
      S.Adb.UnIndent;
      S.Adb.Put_Line ("end if;");
   end Generate_Scenario_Body;

   -------------------------
   --  Generate_Scenario  --
   -------------------------

   procedure Generate_Scenario (S          : in out Ada_Generator_Type;
                                Scenario   : in     Result_Scenario_Handle;
                                Name       : in     Unbounded_String;
                                Seq_Num    : in     Integer)
   is
      use String_Vectors;
      Proc        : constant String := "procedure " & To_String (Name) & " ";
   begin
      pragma Assert (Scenario.Valid);

      -------------------------------------------------------------------------
      --  Declaration  --------------------------------------------------------
      -------------------------------------------------------------------------

      --  ARGUMENTS:

      --  Format     : Object that handle writing what happens
      --  Report     : Counters of passed, skipped and faikled steps/scenarios
      --  Cond       : Condition from the command line to know if the scenario
      --               has to be executed (in form of tags or FEATURE:NUM)
      --  Stop       : Set to True if there is a background step that fails.
      --               if input value is True, all steps will be skipped
      --               if output value is true, skip all other scenarios of
      --               this feature
      --  Count_Mode : Only return the number of steps in Report

      --  VARIABLES:

      --  Fail : True if a step failed. In that case, all other steps are
      --         skipped
      --  Tags : Array of tags

      S.Adb.New_Line;
      S.Ads.Put_Line (Proc & "(Format     : in out Format_Ptr;");
      S.Adb.Put_Line (Proc & "(Format     : in out Format_Ptr;");
      S.Ads.Put_Indent; S.Ads.Put (Proc'Length * " ");
      S.Adb.Put_Indent; S.Adb.Put (Proc'Length * " ");
      S.Ads.Put             (" Report     : in out Report_Type;");
      S.Adb.Put             (" Report     : in out Report_Type;");
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
      if Scenario.R.Outline then
         S.Adb.Put_Line ("Outline_Table : Table_Type;");
      end if;
      S.Adb.Put_Line    ("Num_Scenario  : constant Natural :=" &
                                          Seq_Num'Img & ";");
      S.Adb.Put_Line    ("Fail : Boolean := Stop;");
      Generate_Tag_Variable (S, Scenario);
      S.Adb.New_Line;
      S.Adb.UnIndent;
      S.Adb.Put_Line ("begin");
      Indent (S.Adb);

      -------------------------------------------------------------------------
      --  Body  ---------------------------------------------------------------
      -------------------------------------------------------------------------

      S.Adb.Put_Line ("if Cond.Eval (Tags) and then Cond.Eval (" &
                      Ada_String (To_String (Scenario.R.Position.File)) &
                      "," & String'(Scenario.R.Position.Line'Img) &
                      ", Num_Scenario) then");
      S.Adb.Indent;

      if not Scenario.R.Outline then
         Generate_Scenario_Body (S, Scenario);
      else

         S.Adb.Put_Line ("if not Count_Mode then");
         S.Adb.Indent;
         Generate_Table (S, "Outline_Table", Scenario.R.Table);
         S.Adb.Put_Line ("Format.Start_Outline ("
                         & Ada_String (Scenario.R.Name) & ", "
                         & Ada_String (To_String (Scenario.R.Position))
                         & ", " & "Tags);");
         S.Adb.Put_Line ("Format.Enter_Outline;");
         for I in Scenario.R.Step_First .. Scenario.R.Step_Last loop
            Generate_Step (S, Scenario, Scenario.R.all.Step_Element (I),
                           Fake => True);
         end loop;
         S.Adb.Put_Line ("Format.Begin_Outline;");
         S.Adb.UnIndent;
         S.Adb.Put_Line ("end if;");

         for I in Scenario.R.Outline_First .. Scenario.R.Outline_Last loop
            Generate_Scenario_Body (S, Scenario, I);
         end loop;

         S.Adb.Put_Line ("if not Count_Mode then");
         S.Adb.Indent;
         S.Adb.Put_Line ("Format.Put_Outline_Report (Outline_Table);");
         S.Adb.Put_Line ("Format.End_Outline;");
         S.Adb.Put_Line ("Format.Stop_Outline;");
         S.Adb.UnIndent;
         S.Adb.Put_Line ("end if;");

      end if;

      S.Adb.UnIndent;
      S.Adb.Put_Line ("end if;");
      S.Adb.UnIndent;
      S.Adb.Put_Line ("end " & Name & ";");
   end Generate_Scenario;

   ------------------------
   --  Generate_Feature  --
   ------------------------

   procedure Generate_Feature  (S  : in out Ada_Generator_Type)
   is
      use String_Vectors;
      Str : Unbounded_String;
      N   : Positive := 1;
      Sce : Result_Scenario_Handle;
   begin
      Generate_Background (S, S.Feature.R.Background, S.Fn_Backgnd);
      for I in S.Feature.R.Scenario_First .. S.Feature.R.Scenario_Last loop
         Sce := S.Feature.R.all.Scenario_Element (I);
         Get_Unique_String (S.Pool,
            To_Identifier
              ("Scenario_" & Sce.R.Name), Str);
         Append (S.Fn_Steps, Str);
         Generate_Scenario (S, S.Feature.R.all.Scenario_Element (I), Str, N);
         N := N + 1;
      end loop;
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
      use Ada.Containers;
      E           : Result_Scenario_Handle;
      Num         : Positive := 1;
      Lang        : constant Language_Handle := Gen.Feature.R.Language;
   begin
      Gen.Adb.Put_Line ("pragma Style_Checks (Off);");
      Gen.Ads.Put_Line ("with Ada.Strings.Unbounded;");
      Gen.Ads.Put_Line ("with XReqLib;");
      Gen.Ads.Put_Line ("with XReqLib.Args;");
      Gen.Ads.Put_Line ("with XReqLib.Report;");
      Gen.Ads.Put_Line ("with XReqLib.Format;");
      Gen.Adb.Put_Line ("with XReqLib.Register;");
      Gen.Ads.Put_Line ("use  XReqLib;");
      Gen.Ads.Put_Line ("use  XReqLib.Args;");
      Gen.Ads.Put_Line ("use  XReqLib.Report;");
      Gen.Ads.Put_Line ("use  XReqLib.Format;");
      Gen.Adb.Put_Line ("use  XReqLib.Register;");
      Gen.Ads.Put_Line ("package " & Gen.Id_Pkgname & " is");
      Gen.Adb.Put_Line ("package body " & Gen.Id_Pkgname & " is");
      Gen.Ads.Indent;
      Gen.Adb.Indent;
      Gen.Adb.New_Line;
      Gen.Adb.Put_Line ("Str_Feature    : constant String := " &
                                          Ada_String (Lang.R.Feature) & ";");
      Gen.Adb.Put_Line ("Str_Background : constant String := " &
                                         Ada_String (Lang.R.Background) & ";");
      Gen.Adb.Put_Line ("Str_Scenario   : constant String := " &
                                          Ada_String (Lang.R.Scenario) & ";");
      Gen.Adb.Put_Line ("Str_Outline    : constant String := " &
                                   Ada_String (Lang.R.Scenario_Outline) & ";");
      Gen.Adb.Put_Line ("Str_Examples   : constant String := " &
                                          Ada_String (Lang.R.Examples) & ";");
      Gen.Adb.Put_Line ("Str_Given      : constant String := " &
                                          Ada_String (Lang.R.Given) & ";");
      Gen.Adb.Put_Line ("Str_When       : constant String := " &
                                          Ada_String (Lang.R.When_K) & ";");
      Gen.Adb.Put_Line ("Str_Then       : constant String := " &
                                          Ada_String (Lang.R.Then_K) & ";");
      Gen.Adb.Put_Line ("Str_And        : constant String := " &
                                          Ada_String (Lang.R.And_K) & ";");
      Generate_Feature (Gen);
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

      ---------------
      --  Feature  --
      ---------------

      Gen.Adb.Put_Line ("Format.S_Feature  (Str_Feature);");
      Gen.Adb.Put_Line ("Format.S_Scenario (Str_Scenario);");
      Gen.Adb.Put_Line ("Format.S_Outline  (Str_Outline);");

      --  List Mode
      ---------------
      Gen.Adb.Put_Line ("if List_Mode then");
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("Format.List_Feature (" &
                        Ada_String (Gen.Feature.R.Name) & ");");
      for I in Gen.Feature.R.Scenario_First .. Gen.Feature.R.Scenario_Last loop
         E := Gen.Feature.R.all.Scenario_Element (I);
         Gen.Adb.Put_Line ("Format.List_Scenario (" &
                           Ada_String (E.R.Name) & ", " &
                           Ada_String (To_String (E.R.Position.File)) & "," &
                           String'(E.R.Position.Line'Img) & "," &
                           String'(Num'Img) & ");");
         Num := Num + 1;
      end loop;
      Gen.Adb.UnIndent;

      --  Count Mode
      ----------------
      Gen.Adb.Put_Line ("elsif Count_Mode then");
      Gen.Adb.Indent;
      for I in 0 .. Integer (Length (Gen.Fn_Steps)) - 1 loop
         Gen.Adb.Put_Line (Element (Gen.Fn_Steps, I) & " " &
                           "(Format, Report, Cond, Stop, Count_Mode);");
      end loop;
      if Length (Gen.Fn_Steps) = 0 then
         Gen.Adb.Put_Line ("null;");
      end if;
      Gen.Adb.UnIndent;

      --  Run Mode
      --------------
      Gen.Adb.Put_Line ("else");
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("Format.Start_Feature (" &
                        Ada_String (Gen.Feature.R.Name) & ", " &
                        Ada_String (Gen.Feature.R.Description) &
                        ", " &
                        Ada_String (To_String (Gen.Feature.R.Position)) &
                        ");");
      Gen.Adb.Put_Line ("Format.Begin_Feature;");
      Gen.Adb.Put_Line ("Format.Put_Feature;");
      Gen.Adb.Put_Line ("Call_Hook (Hook_Begin, Hook_Feature);");
      for I in 0 .. Integer (Length (Gen.Fn_Steps)) - 1 loop
         Gen.Adb.Put_Line (Element (Gen.Fn_Steps, I) & " " &
                           "(Format, Report, Cond, Stop, Count_Mode);");
      end loop;
      Gen.Adb.Put_Line ("Call_Hook (Hook_End, Hook_Feature);");
      Gen.Adb.Put_Line ("Format.End_Feature;");
      Gen.Adb.Put_Line ("Format.Stop_Feature;");

      -------------------
      --  End Feature  --
      -------------------

      Gen.Adb.UnIndent;
      Gen.Adb.Put_Line ("end if;");
      Gen.Adb.UnIndent;
      Gen.Adb.Put_Line ("end Run;");
      Gen.Ads.UnIndent;
      Gen.Adb.UnIndent;
      Gen.Adb.New_Line;
      Gen.Adb.Put_Line ("begin");
      Gen.Adb.New_Line;
      Gen.Adb.Indent;
      Gen.Adb.Put_Line ("Register_Feature (Run'Access);");
      Gen.Adb.UnIndent;
      Gen.Adb.New_Line;
      Gen.Ads.Put_Line ("end " & Gen.Id_Pkgname & ";");
      Gen.Adb.Put_Line ("end " & Gen.Id_Pkgname & ";");
      Gen.Adb.Put_Line ("pragma Style_Checks (On);");
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
                             Env  : in Environment_Handle;
                             Log  : in Logger_Ptr;
                             Make : in Boolean := False)
   is
      procedure Put_GPR_With (Str : in String);
      procedure Put_GPR_Path (Str : in String);
      use Generator_Vectors;
      use String_Vectors;
      use Ada.Strings.Fixed;
      Filename : constant String := Env.Ref.Out_Dir & "/" & Name & ".adb";
      Gpr_Name : constant String := Env.Ref.Out_Dir & "/" & Name & ".gpr";
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
      I := First (Gens);
      while Has_Element (I) loop
         E := Ada_Generator_Ptr (Element (I));
         With_B.Put_Line ("with " & To_String (E.Id_Pkgname) & ";");
         Next (I);
      end loop;
      With_B.Put_Line ("pragma Style_Checks (Off);");
      With_B.New_Line;
      With_B.Put_Line ("with Ada.Command_Line;");
      With_B.Put_Line ("with Ada.Real_Time;");
      With_B.Put_Line ("with XReqLib;");
      With_B.Put_Line ("with XReqLib.CLI;");
      With_B.Put_Line ("with XReqLib.Util;");
      With_B.Put_Line ("with XReqLib.Report;");
      With_B.Put_Line ("with XReqLib.Format;");
      With_B.Put_Line ("with XReqLib.Format.Text;");
      With_B.Put_Line ("with XReqLib.Register;");
      With_B.Put_Line ("use  Ada.Real_Time;");
      With_B.Put_Line ("use  XReqLib;");
      With_B.Put_Line ("use  XReqLib.CLI;");
      With_B.Put_Line ("use  XReqLib.Util;");
      With_B.Put_Line ("use  XReqLib.Report;");
      With_B.Put_Line ("use  XReqLib.Format;");
      With_B.Put_Line ("use  XReqLib.Format.Text;");
      With_B.Put_Line ("use  XReqLib.Register;");
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
      Body_B.Put_Line    ("Format.Begin_Tests;");
      Body_B.Put_Line    ("Time_Start := Clock;");
      Body_B.Put_Line    ("--  Count Steps");
      Body_B.Put_Line    ("Call_Features (Format, Cond, Report, " &
                                         "Count_Mode => True);");
      Body_B.Put_Line    ("Format.Set_Num_Steps (Report.Num_Steps);");
      Body_B.Put_Line    ("--  Run Steps");
      Body_B.Put_Line    ("Call_Features (Format, Cond, Report, List_Mode);");
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
      Body_B.Put_Line    ("Format.End_Tests;");
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
      Body_B.New_Line;
      Body_B.Put_Line ("pragma Style_Checks (On);");

      Gpr_B.Put_Line ("with ""xreqlib"";");
      Split_String_Start (Splitter,
                          Env.Ref.Get_Option ("ada.gpr.with", ""), ",");
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

      for I in First_Index (Env.Ref.Step_Dir) .. Last_Index (Env.Ref.Step_Dir)
      loop
         Gpr_B.Put (", """ &
                    Goto_Path (Env.Ref.Out_Dir,
                               To_String (Element (Env.Ref.Step_Dir, I)))
                    & """");
      end loop;
      Split_String_Start (Splitter,
                          Env.Ref.Get_Option ("ada.gpr.srcdir", ""), ",");
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
                       Directory => Env.Ref.Out_Dir,
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
                             Compose (Env.Ref.Out_Dir,
                                      Simple_Name (Dir_Entry));
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

   pragma Style_Checks (On);

end XReq.Generator.Ada05;
