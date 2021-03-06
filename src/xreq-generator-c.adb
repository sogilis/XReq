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

with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Fixed;
with GNAT.OS_Lib;
with XReqLib.String_Tables;
with XReq.Step_Definitions;
with XReq.Steps.Result.Handles;
with XReq.Args;
with XReq.Language.Handles;
with XReq.Steps.Result;
with XReq.Scenarios.Result;
with XReq.Scenarios.Result.Handles;

use XReq.Step_Definitions;
use XReq.Steps.Result.Handles;
use XReq.Args;
use XReq.Language.Handles;
use XReq.Steps.Result;
use XReq.Scenarios.Result;
use XReq.Scenarios.Result.Handles;

package body XReq.Generator.C is


   procedure Generate_Table    (S          : in out C_Generator_Type;
                                Name       : in     String;
                                T          : in     String_Tables.Table);
   procedure Generate_Step     (S          : in out C_Generator_Type;
                                Scenario   : in     Result_Scenario_Handle;
                                Step       : in     Result_Step_Handle;
                                Num        : in     Natural;
                                Background : in     Boolean := False;
                                Fake       : in     Boolean := False;
                                Outline    : in     Boolean := False;
                                Num_Outlne : in     Natural := 1);
   procedure Generate_Scenario (S          : in out C_Generator_Type;
                                Scenario   : in     Result_Scenario_Handle;
                                Name       : in     Unbounded_String;
                                Seq_Num    : in     Integer;
                                Num_Steps  : out    Natural;
                                Background : in     Boolean := False);
   procedure Generate_Feature  (S          : in out C_Generator_Type;
                                Num_Steps  : out    Natural);
   procedure Generate_With     (S          : in out C_Generator_Type);

   ------------
   --  Make  --
   ------------

   procedure Make      (Gen : out    C_Generator_Type;
                        Job : in     Job_Type;
                        Env : in     Environment_Handle)
   is
      use Ada.Characters.Handling;
      use Ada.Directories;
      Basename    : Unbounded_String;
      Pkgname     : constant String :=
               Job.Result.R.Filetype & "_" & Base_Name (Feature_File (Job));
   begin
      Gen.Feature := Job.Result;
      Get_Unique_String (
         Gen.Pool, To_Identifier (Pkgname),      Gen.Header_Name);
      Get_Unique_String (
         Gen.Pool, To_Identifier ("background"), Gen.Fn_Backgnd);
      Basename := To_Unbounded_String (Compose (Env.Ref.Out_Dir,
                  To_Lower (To_String (Gen.Header_Name))));
      Gen.H_File := Basename & ".h";
      Gen.C_File := Basename & ".c";
   end Make;

   ----------------------
   --  Generate_Table  --
   ----------------------

   procedure Generate_Table    (S          : in out C_Generator_Type;
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
         S.C.Put_Line ("XReq_Table_Put (" & Name & ", " &
            Trim (K.X'Img, Left)                 & ", " &
            Trim (K.Y'Img, Left)                 & ", " &
            C_String (To_String (Element (I))) & ");");
         Next (I);
      end loop;
   end Generate_Table;

   ---------------------
   --  Generate_Step  --
   ---------------------

   procedure Generate_Step     (S          : in out C_Generator_Type;
                                Scenario   : in     Result_Scenario_Handle;
                                Step       : in     Result_Step_Handle;
                                Num        : in     Natural;
                                Background : in     Boolean := False;
                                Fake       : in     Boolean := False;
                                Outline    : in     Boolean := False;
                                Num_Outlne : in     Natural := 1)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use String_Sets;
      use Match_Vectors;
      Procname : constant String := Step.R.Procedure_Name;
      H_File   : constant String := Step.R.File_Name;
      E        : Match_Location;
      E2       : Argument_Type;
   begin
      S.C.Put_Line ("/*");
      S.C.Put_Line (" * " & Num'Img & ". " & Step.R.To_String);
      S.C.Put_Line (" */");
      S.C.Put_Line ("num_step =" & Num'Img & ";");
      S.C.Put_Line ("XReq_Format_Start_Step (format);");

      -------------------------------------------------------------------------
      --  Declare  ------------------------------------------------------------
      -------------------------------------------------------------------------

      --  Args   : Arguments (matches, text, tables ...)
      --  Prefix : (Given, When, Then)
      --  Stanza : Step sentance
      --  Pos    : String filename:line

      S.C.Put_Line ("{");
      S.C.Indent (2);
      S.C.Put_Indent;
      S.C.Put      ("#define prefix ");
      case Step.R.Kind is
         when Step_Given => S.C.Put ("XReq_Kind_Given");
         when Step_When  => S.C.Put ("XReq_Kind_When");
         when Step_Then  => S.C.Put ("XReq_Kind_Then");
      end case;
      S.C.New_Line;
      S.C.Put_Line ("#define stanza " & C_String (Step.R.Stanza));
      S.C.Put_Line ("#define pos    " & C_String
        (To_String (Step.R.Position)));
      S.C.Put_Line ("XReq_Args  *args = XReq_Args_New ();");
      S.C.Put_Line ("XReq_Table *tble = XReq_Table_New();");
      S.C.Put_Line ("XReq_Error *err  = XReq_Error_New();");

      -------------------------------------------------------------------------
      --  Begin  --------------------------------------------------------------
      -------------------------------------------------------------------------

      --------------------
      --  Fill in Args  --
      -------------------------------------------------------------------------
      S.C.Put_Line ("XReq_Args_Make      (args, stanza);");
      for I in Step.R.Match_First .. Step.R.Match_Last loop
         E := Step.R.Match_Element (I);
         S.C.Put_Line ("XReq_Args_Add_Match (args," & E.First'Img & "," &
                                     E.Last'Img & ");");
      end loop;
      for I2 in Step.R.Arg_First .. Step.R.Arg_Last loop
         E2 := Step.R.Arg_Element (I2);
         case E2.Typ is
            when Text =>
               S.C.Put_Line ("XReq_Args_Add_Text  (args, " &
                               C_String (To_String (E2.Text)) & ");");
            when Table =>
               Generate_Table (S, "tble", E2.Table);
               S.C.Put_Line ("XReq_Args_Add_Table (args, tble);");
            when others =>
               null;    --  GCOV_IGNORE (never happens)
         end case;
      end loop;
      S.C.Put_Line ("XReq_Args_Add_Sep   (args, 1);");
      --  Skip if failure
      S.C.Put_Line ("if (fail) {");
      S.C.Indent (2);
      if not Fake then
         S.C.Put_Line ("XReq_Report_step_skip (report);");
      end if;
      if Background then
         S.C.Put_Line ("if (!stop) {");
         S.C.Indent (2);
      end if;
      S.C.Put_Line ("XReq_Format_Put_Step  (format, prefix, stanza, pos, " &
                      "args, XReq_Status_Skipped);");
      if Background then
         S.C.UnIndent (2);
         S.C.Put_Line ("}");
      end if;
      S.C.UnIndent (2);
      S.C.Put_Line ("} else {");
      S.C.Indent (2);
      S.C.Put_Line ("XReq_Error_Clear (err);");
      if Fake then
         S.C.Put_Line ("XReq_Format_Put_Step (format, prefix, stanza, pos, " &
                         "args, XReq_Status_Outline);");
      elsif Procname = "" then
         S.C.Put_Line ("XReq_Error_Make (err, ""The step definition cound " &
                       "not be found"", """", 0);");
         S.C.Put_Line ("if (1) {");
      else
         --  Generate extern declaration
         Include (S.Headers, To_Unbounded_String (H_File));
         Include (S.C_Steps, To_Unbounded_String
            (H_File (H_File'First .. H_File'Last - 2) & ".c"));
         --  Call to step
         S.C.Put_Line ("XReq_Step__" & Procname & " (args, err);");
         S.C.Put_Line ("if (XReq_Error_Is_Null (err)) {");
         S.C.Indent (2);
         --  Count step
         S.C.Put_Line ("XReq_Report_step_pass (report);");
         --  Print the step
         if Background then
            S.C.Put_Line ("if (first) {");
            S.C.Indent (2);
         end if;
         S.C.Put_Line ("XReq_Format_Put_Step (format, prefix, stanza, pos, " &
                        "args, XReq_Status_Passed);");
         if Background then
            S.C.UnIndent (2);
            S.C.Put_Line ("}");
         end if;
         S.C.UnIndent (2);
         S.C.Put_Line ("} else {");
      end if;
      -------------------------------------------------------------------------
      --  Exception  ----------------------------------------------------------
      -------------------------------------------------------------------------
      if not Fake then
         S.C.Indent (2);
         S.C.Put_Line ("XReq_Report_step_fail (report);");
         S.C.Put_Line ("fail = 1;");
         if Outline then
            S.C.Put_Line ("XReq_Format_Put_Scenario_Outline (format" & ", " &
                          Trim (Num_Outlne'Img, Left)                & ", " &
                          C_String (Scenario.R.Name)                   & ", " &
                          C_String (To_String (Scenario.R.Position))   & ", " &
                          "tags);");
         end if;
         S.C.Put_Line ("XReq_Format_Put_Step  (format, prefix, stanza, " &
                              "pos, args, XReq_Status_Failed);");
         S.C.Put_Line ("XReq_Format_Put_Error (format, err);");
         S.C.UnIndent (2);
         S.C.Put_Line ("}");
      end if;

      --  End if skip
      S.C.UnIndent (2);
      S.C.Put_Line ("}");
      --  End block
      S.C.Put_Line ("XReq_Error_Free (err);");
      S.C.Put_Line ("XReq_Table_Free (tble);");
      S.C.Put_Line ("XReq_Args_Free  (args);");
      S.C.Put_Line ("#undef pos");
      S.C.Put_Line ("#undef stanza");
      S.C.Put_Line ("#undef prefix");
      S.C.UnIndent (2);
      S.C.Put_Line ("}");
      S.C.Put_Line ("XReq_Format_Stop_Step (format);");

      -------------------------------------------------------------------------
      --  Exception  ----------------------------------------------------------
      -------------------------------------------------------------------------


   end Generate_Step;

   -------------------------
   --  Generate_Scenario  --
   -------------------------

   procedure Generate_Scenario (S          : in out C_Generator_Type;
                                Scenario   : in     Result_Scenario_Handle;
                                Name       : in     Unbounded_String;
                                Seq_Num    : in     Integer;
                                Num_Steps  : out    Natural;
                                Background : in     Boolean := False)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use String_Vectors;
      N, M        : Integer;
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

      S.C.New_Line;
      S.C.Put_Line ("static void " & To_String (Name));
      S.C.Put_Line ("  (XReq_Format      *format,");
      S.C.Put_Line ("   XReq_Report      *report,");
      S.C.Put_Line ("   int              *is_first,");
      S.C.Put_Line ("   XReq_Conditional *cond,");
      S.C.Put_Line ("   int              *is_stop,");
      S.C.Put_Line ("   int               is_count_mode)");
      S.C.Put_Line ("{");
      S.C.Indent (2);
      S.C.Put_Line ("#define num_scenario  " & Seq_Num'Img);
      S.C.Put_Line ("#define num_tags      " & Scenario.R.Tag_Count'Img);
      S.C.Put_Line ("#define first          (*is_first)");
      S.C.Put_Line ("#define stop           (*is_stop)");
      S.C.Put_Line ("#define count_mode     (is_count_mode)");
      if Scenario.R.Outline then
         S.C.Put_Line ("XReq_Table* outline_table = XReq_Table_New();");
      end if;
      S.C.Put_Line ("int         num_step      = 0;");
      S.C.Put_Line ("int         fail          = stop;");
      S.C.Put_Line ("XReq_Cstr   tags[]        = {");
      S.C.Indent (2);
      S.C.Put_Indent;
      for I in Scenario.R.Tag_First .. Scenario.R.Tag_Last loop
         S.C.Put_Line (C_String (Scenario.R.Tag_Element (I)) & ",");
      end loop;
      S.C.Put_Line ("NULL};");
      S.C.UnIndent (2);
      S.C.New_Line;

      -------------------------------------------------------------------------
      --  Body  ---------------------------------------------------------------
      -------------------------------------------------------------------------

      if Background then
         S.C.Put_Line ("if (!count_mode) {");
         S.C.Indent (2);
         S.C.Put_Line ("XReq_Format_Start_Background (format, first);");
      else
         S.C.Put_Line ("if (XReq_Conditional_Eval_Tags (cond, tags) &&");
         S.C.Put_Line ("    XReq_Conditional_Eval_Position (cond, " &
                            C_String (To_String (Scenario.R.Position.File)) &
                            ", num_scenario))");
         S.C.Put_Line ("{");
         S.C.Indent (2);
         S.C.Put_Line ("if (!count_mode) {");
         S.C.Indent (2);
         S.C.Put_Line ("if (first) {");
         S.C.Put_Line ("  __feature (format);");
         S.C.Put_Line ("}");
         if Scenario.R.Outline then
            Generate_Table (S, "outline_table", Scenario.R.Table);
            S.C.Put_Line ("XReq_Format_Enter_Outline (format);");
         else
            S.C.Put_Line ("XReq_Format_Enter_Scenario (format);");
         end if;
         S.C.Put_Line ("if (!first) {");
         S.C.Put_Line ("  /* Background has already been shown, show " &
                            "scenario */");
         S.C.Put_Indent; S.C.Put ("  XReq_Format_");
         if Scenario.R.Outline then
            S.C.Put ("Put_Outline");
         else
            S.C.Put ("Put_Scenario");
         end if;
         S.C.Put (" (format, " & C_String (Scenario.R.Name) & ", " &
                         C_String (To_String (Scenario.R.Position)) & ", " &
                           "tags);");
         S.C.New_Line;
         S.C.Put_Line ("}");
         S.C.Put_Line ("/*****************");
         S.C.Put_Line ("**  Background  **");
         S.C.Put_Line ("*****************/");
         S.C.Put_Line (S.Fn_Backgnd &
                         " (format, report, is_first, cond, &fail, 0);");
         S.C.Put_Line ("stop = stop || (first && fail);");
         if Scenario.R.Outline then
            S.C.Put_Line ("XReq_Format_Start_Outline (format);");
         else
            S.C.Put_Line ("XReq_Format_Start_Scenario (format);");
         end if;
      end if;
      M := 1;
      if not Background or else Scenario.R.Step_Count /= 0 then
         if Background then
            S.C.Put_Line ("/*****************");
            S.C.Put_Line ("**  Background  **");
            S.C.Put_Line ("*****************/");
         elsif Scenario.R.Outline then
            S.C.Put_Line ("/*************************");
            S.C.Put_Line ("**   Scenario Outline   **");
            S.C.Put_Line ("*************************/");
         else
            S.C.Put_Line ("/***************");
            S.C.Put_Line ("**  Scenario  **");
            S.C.Put_Line ("***************/");
         end if;
         S.C.Put_Line ("if (first) {");
         S.C.Indent (2);
         if Background then
            S.C.Put_Line ("XReq_Format_Put_Background (format, " &
                              C_String (Scenario.R.Name) & ", " &
                              C_String (To_String (Scenario.R.Position)) &
                              ", " & "tags);");
         elsif Scenario.R.Outline then
            S.C.Put_Line ("XReq_Format_Put_Outline (format, " &
                              C_String (Scenario.R.Name) & ", " &
                              C_String (To_String (Scenario.R.Position)) &
                              ", " & "tags);");
         else
            S.C.Put_Line ("XReq_Format_Put_Scenario (format, " &
                              C_String (Scenario.R.Name) & ", " &
                              C_String (To_String (Scenario.R.Position)) &
                              ", " & "tags);");
         end if;
         S.C.UnIndent (2);
         S.C.Put_Line ("}");
         for I in Scenario.R.Step_First .. Scenario.R.Step_Last loop
            Generate_Step (S, Scenario, Scenario.R.all.Step_Element (I), M,
                           Background, Scenario.R.Outline, Scenario.R.Outline);
            M := M + 1;
            if not Scenario.R.Outline then
               Steps_Count := Steps_Count + 1;
            end if;
         end loop;
      end if;
      if Scenario.R.Outline then
         N := 0;
         for J in Scenario.R.Outline_First .. Scenario.R.Outline_Last loop
            N := N + 1;
            S.C.Put_Line ("/*************************");
            S.C.Put_Line ("**  Generated Scenario  **");
            S.C.Put_Line ("*************************/");
            S.C.Put_Line ("XReq_Format_Enter_Scenario (format);");
            if not First then
               S.C.Put_Line (S.Fn_Backgnd &
                              " (format, report, is_first, cond, &fail, 0);");
               S.C.Put_Line ("stop = stop || (first && fail);");
               S.C.Put_Line ("fail = stop;");
            end if;
            S.C.Put_Line ("XReq_Format_Start_Scenario (format);");
            for I in Scenario.R.Outline_Step_First (J) ..
                              Scenario.R.Outline_Step_Last (J)
            loop
               Generate_Step (S, Scenario,
                              Scenario.R.Outline_Step_Element (J, I), M,
                              Background, False, True, N);
               M := M + 1;
               Steps_Count := Steps_Count + 1;
            end loop;
            S.C.Put_Line ("if (fail) {");
            S.C.Put_Line ("  XReq_Report_scenario_fail (report);");
            S.C.Put_Line ("} else {");
            S.C.Put_Line ("  XReq_Report_scenario_pass (report);");
            S.C.Put_Line ("}");
            S.C.Put_Line ("XReq_Format_Stop_Scenario (format);");
            First := False;
         end loop;
         S.C.Put_Line ("/*******************************");
         S.C.Put_Line ("**  Scenario Outline Summary  **");
         S.C.Put_Line ("*******************************/");
         S.C.Put_Line ("XReq_Format_Put_Outline_Report " &
                                        "(format, outline_table);");
      end if;
      S.C.Put_Line ("/*******************");
      S.C.Put_Line ("**  Finalization  **");
      S.C.Put_Line ("*******************/");
      if Background then
         S.C.Put_Line ("stop = fail;");
         S.C.Put_Line ("XReq_Format_Stop_Background (format, first);");
         S.C.UnIndent (2);
         S.C.Put_Line ("} else { /* count_mode */");
         S.C.Put_Line ("  XReq_Report_num_steps_inc (report, " &
                          Trim (Steps_Count'Img, Left) & ");");
         S.C.Put_Line ("}");
      else
         if Scenario.R.Outline then
            S.C.Put_Line ("XReq_Format_Stop_Outline (format);");
         else
            S.C.Put_Line ("if (fail) {");
            S.C.Put_Line ("  XReq_Report_scenario_fail (report);");
            S.C.Put_Line ("} else {");
            S.C.Put_Line ("  XReq_Report_scenario_pass (report);");
            S.C.Put_Line ("}");
            S.C.Put_Line ("XReq_Format_Stop_Scenario (format);");
         end if;
         S.C.Put_Line ("first = 0;");
         S.C.UnIndent (2);
         S.C.Put_Line ("} else { /* count_mode */");
         S.C.Indent (2);
         if Scenario.R.Outline then
            for J in Scenario.R.Outline_First .. Scenario.R.Outline_Last loop
               S.C.Put_Line (S.Fn_Backgnd &
                             " (format, report, is_first, cond, &fail, 1);");
            end loop;
         else
            S.C.Put_Line (S.Fn_Backgnd &
                          " (format, report, is_first, cond, &fail, 1);");
         end if;
         S.C.Put_Line ("XReq_Report_num_steps_inc (report," &
                        Steps_Count'Img & ");");
         S.C.UnIndent (2);
         S.C.Put_Line ("}");
         S.C.UnIndent (2);
         S.C.Put_Line ("}");
      end if;
      if Scenario.R.Outline then
         S.C.Put_Line ("XReq_Table_Free (outline_table);");
      end if;
      S.C.Put_Line ("#undef count_mode");
      S.C.Put_Line ("#undef stop");
      S.C.Put_Line ("#undef first");
      S.C.Put_Line ("#undef num_tags");
      S.C.Put_Line ("#undef num_scenario");
      S.C.UnIndent (2);
      S.C.Put_Line ("}");
      Num_Steps := Steps_Count;
   end Generate_Scenario;

   ------------------------
   --  Generate_Feature  --
   ------------------------

   procedure Generate_Feature  (S          : in out C_Generator_Type;
                                Num_Steps  : out    Natural)
   is
      use String_Vectors;
      Str : Unbounded_String;
      N   : Positive := 1;
      Sce : Result_Scenario_Handle;
      Num_Steps_Scenario   : Natural := 0;
      Num_Steps_Background : Natural := 0;
      Total_Steps          : Natural := 0;
   begin
      Generate_Scenario (S, S.Feature.R.Background, S.Fn_Backgnd, 0,
                         Num_Steps_Background, True);
      for I in S.Feature.R.Scenario_First .. S.Feature.R.Scenario_Last loop
         Sce := S.Feature.R.all.Scenario_Element (I);
         Get_Unique_String
           (S.Pool, To_Identifier ("scenario_" & Sce.R.Name), Str);
         Append (S.Fn_Steps, Str);
         Generate_Scenario (S, S.Feature.R.all.Scenario_Element (I), Str, N,
                            Num_Steps_Scenario);
         N := N + 1;
         Total_Steps := Num_Steps_Background + Num_Steps_Scenario;
      end loop;
      Num_Steps := Total_Steps;
   end Generate_Feature;

   ---------------------
   --  Generate_With  --
   ---------------------

   procedure Generate_With     (S          : in out C_Generator_Type) is
      use Ada.Directories;
      use String_Sets;
      J   : String_Sets.Cursor := First (S.Headers);
      Buf : Unbounded_String;
   begin
      while Has_Element (J) loop
         Append (Buf, "#include """ &
                 Goto_Path (Containing_Directory (To_String (S.H_File)),
                            To_String (Element (J))) &
                 """" & S.C.CRLF);
         Next (J);
      end loop;
      S.C.Buffer := "#include <xreq.h>" & S.C.CRLF & S.C.CRLF &
                    Buf                 & S.C.CRLF &
                    S.C.Buffer;
   end Generate_With;

   ----------------
   --  Generate  --
   ----------------

   procedure Generate  (Gen : in out C_Generator_Type;
                        Log : in     Logger_Ptr)
   is
      use String_Vectors;
      E           : Result_Scenario_Handle;
      Num         : Positive := 1;
      Total_Steps : Natural := 0;
      Lang        : constant Language_Handle := Gen.Feature.R.Language;
   begin
      Gen.H.Put_Line ("#include <xreq.h>");
      Gen.H.New_Line;
      Gen.H.Put_Line ("#ifndef XREQ_FEATURE__" & Gen.Header_Name & "_H");
      Gen.H.Put_Line ("#define XREQ_FEATURE__" & Gen.Header_Name & "_H");
      Gen.H.New_Line;
      Gen.C.Put_Line ("#include """ & Gen.Header_Name & ".h""");
      Gen.C.New_Line;
      Gen.C.New_Line;
      Gen.C.Put_Line ("#define STR_Feature    " & C_String (Lang.R.Feature));
      Gen.C.Put_Line ("#define STR_Background " &
                                                 C_String (Lang.R.Background));
      Gen.C.Put_Line ("#define STR_Scenario   " & C_String (Lang.R.Scenario));
      Gen.C.Put_Line ("#define STR_Outline    " &
                                           C_String (Lang.R.Scenario_Outline));
      Gen.C.Put_Line ("#define STR_Examples   " & C_String (Lang.R.Examples));
      Gen.C.Put_Line ("#define STR_Given      " & C_String (Lang.R.Given));
      Gen.C.Put_Line ("#define STR_When       " & C_String (Lang.R.When_K));
      Gen.C.Put_Line ("#define STR_Then       " & C_String (Lang.R.Then_K));
      Gen.C.Put_Line ("#define STR_And        " & C_String (Lang.R.And_K));
      Gen.C.New_Line;
      Gen.C.Put_Line ("static void __feature (XReq_Format *format)");
      Gen.C.Put_Line ("{");
      Gen.C.Indent (2);
      Gen.C.Put_Line ("XReq_Format_Put_Feature (format, " &
                      C_String (Gen.Feature.R.Name) & ", " &
                      C_String (Gen.Feature.R.Description) & ", " &
                      C_String (To_String (Gen.Feature.R.Position)) & ");");
      Gen.C.UnIndent (2);
      Gen.C.Put_Line ("}");
      Gen.C.New_Line;
      Generate_Feature (Gen, Total_Steps);
      Gen.H.Put_Line ("#define " & Gen.Header_Name & "_num_steps" &
                                   Total_Steps'Img);
      Gen.H.New_Line;
      Gen.H.Put_Line ("void run_" & Gen.Header_Name);
      Gen.H.Put_Line ("  (XReq_Format      *format,");
      Gen.H.Put_Line ("   XReq_Conditional *cond,");
      Gen.H.Put_Line ("   XReq_Report      *report,");
      Gen.H.Put_Line ("   int               is_list_mode,");
      Gen.H.Put_Line ("   int               is_count_mode);");
      Gen.C.New_Line;
      Gen.C.Put_Line ("void run_" & Gen.Header_Name);
      Gen.C.Put_Line ("  (XReq_Format      *format,");
      Gen.C.Put_Line ("   XReq_Conditional *cond,");
      Gen.C.Put_Line ("   XReq_Report      *report,");
      Gen.C.Put_Line ("   int               is_list_mode,");
      Gen.C.Put_Line ("   int               is_count_mode)");
      Gen.C.Put_Line ("{");
      Gen.C.Indent (2);
      Gen.C.Put_Line ("int stop  = 0;");
      Gen.C.Put_Line ("int first = 1;");
      Gen.C.New_Line;
      Gen.C.Put_Line ("XReq_Formet_STR_Feature  (format, STR_Feature);");
      Gen.C.Put_Line ("XReq_Formet_STR_Scenario (format, STR_Scenario);");
      Gen.C.Put_Line ("XReq_Formet_STR_Outline  (format, STR_Outline);");
      Gen.C.Put_Line ("if (is_list_mode) {");
      Gen.C.Indent (2);
      Gen.C.Put_Line ("XReq_Format_List_Feature (format, " &
                      C_String (Gen.Feature.R.Name) & ");");

      for I in Gen.Feature.R.Scenario_First .. Gen.Feature.R.Scenario_Last loop
         E := Gen.Feature.R.all.Scenario_Element (I);
         Gen.C.Put_Line ("XReq_Format_List_Scenario (format, " &
                           C_String (E.R.Name) & ", " &
                           C_String (To_String (E.R.Position.File)) &
                           "," & Num'Img & ");");
         Num := Num + 1;
      end loop;

      Gen.C.UnIndent (2);
      Gen.C.Put_Line ("} else {");
      Gen.C.Indent (2);
      Gen.C.Put_Line ("if (!is_count_mode) {");
      Gen.C.Put_Line ("  XReq_Format_Start_Feature(format);");
      Gen.C.Put_Line ("}");

      for I in 0 .. Integer (Length (Gen.Fn_Steps)) - 1 loop
         Gen.C.Put_Line (Element (Gen.Fn_Steps, I) & " " &
                      "(format, report, &first, cond, &stop, is_count_mode);");
      end loop;

      Gen.C.Put_Line ("if (!is_count_mode) {");
      Gen.C.Put_Line ("   XReq_Format_Stop_Feature(format);");
      Gen.C.Put_Line ("}");
      Gen.C.UnIndent (2);
      Gen.C.Put_Line ("}");
      Gen.C.UnIndent (2);
      Gen.C.Put_Line ("}");
      Gen.C.New_Line;
      Generate_With (Gen);
      Gen.H.New_Line;
      Gen.H.Put_Line ("#endif");
      Gen.H.New_Line;

      Set_File (To_String (Gen.H_File), To_String (Gen.H.Buffer));
      Set_File (To_String (Gen.C_File), To_String (Gen.C.Buffer));
      Log.Put_Line ("Generate: " & To_String (Gen.H_File));
      Log.Put_Line ("Generate: " & To_String (Gen.C_File));
   end Generate;

   -----------------
   --  Full_Name  --
   -----------------

   function  Full_Name (Gen : in     C_Generator_Type) return String is
   begin
      return To_String (Gen.Header_Name);
   end Full_Name;

   ----------------------
   --  Generate_Suite  --
   ----------------------

   procedure Generate_Suite (Gens : in Generator_Vectors.Vector;
                             Name : in String;
                             Env  : in Environment_Handle;
                             Log  : in Logger_Ptr;
                             Make : in Boolean := False)
   is
      use Ada.Directories;
      use GNAT.OS_Lib;
      use Generator_Vectors;
      use String_Vectors;
      use String_Sets;
      Filename : constant String := Env.Ref.Out_Dir & "/" & Name & ".c";
      Mak_Name : constant String := Env.Ref.Out_Dir & "/" & Name & ".Makefile";
      With_B   : Buffer_Type;
      Body_B   : Buffer_Type;
      Mak_B    : Buffer_Type;
      I        : Generator_Vectors.Cursor;
      E        : C_Generator_Ptr;
      Prc_Name : constant String := To_Identifier (Name);
      Sources  : String_Sets.Set;
      J        : String_Sets.Cursor;
   begin
      With_B.Put_Line ("/* File: " & Filename & " */");
      With_B.Put_Line ("#include <xreq.h>");
      Body_B.Put_Line ("int main (int argc, char* argv[])");
      Body_B.Put_Line ("{");
      Body_B.Indent (2);
      Body_B.Put_Line ("#define           self_name    " &
                       C_String (Prc_Name));
      Body_B.Put_Line ("xreqinit();");
      Body_B.Put_Line ("XReq_Bool         cont       = 0;");
      Body_B.Put_Line ("XReq_Report      *report     = XReq_Report_New();");
      Body_B.Put_Line ("XReq_Format      *format     = NULL;");
      Body_B.Put_Line ("XReq_Conditional *cond       = XReq_Conditional_New();"
                      );
      Body_B.Put_Line ("XReq_Bool         list_mode  = 0;");
      Body_B.Put_Line ("XReq_Duration     time_delta = 0;");
      Body_B.Put_Line ("int               exit_code  = 0;");
      Body_B.New_Line;
      Body_B.Put_Line ("if(!XReq_CLI_Parse_Arguments (argc, argv," &
                           " &format, &cont, cond, &list_mode, self_name)) " &
                           "exit_code = 1;");
      Body_B.Put_Line ("if (cont) {");
      Body_B.Indent (2);
      Body_B.Put_Line    ("XReq_Format_Start_Tests (format);");
      Body_B.Put_Line    ("XReq_Time_Start (&time_delta);");
      Body_B.Put_Line    ("/* Count Steps */");
      I := First (Gens);
      while Has_Element (I) loop
         E := C_Generator_Ptr (Element (I));
         With_B.Put_Line ("#include """ & To_String (E.Header_Name) & ".h""");
         Body_B.Put_Line ("run_" & E.Full_Name &
                                   " (format, cond, report, 0, 1);");
         Next (I);
      end loop;
      Body_B.Put_Line    ("XReq_Format_Set_Num_Steps (format, " &
                          "XReq_Report_get_num_steps (report));");
      Body_B.Put_Line    ("/* Run Steps */");
      I := First (Gens);
      while Has_Element (I) loop
         E := C_Generator_Ptr (Element (I));
         Body_B.Put_Line ("run_" & E.Full_Name & " (format, cond, report, " &
                                              "list_mode, 0);");
         Next (I);
      end loop;
      Body_B.Put_Line    ("XReq_Time_Stop (&time_delta);");
      Body_B.Put_Line    ("if (!list_mode) {");
      Body_B.Indent (2);
      Body_B.Put_Line    ("XReq_Format_Put_Summary " &
                                       "(format, report, time_delta);");
      Body_B.Put_Line    ("if (!XReq_Report_Status (report)) {");
      Body_B.Indent (2);
      Body_B.Put_Line       ("exit_code = 1;");
      Body_B.UnIndent (2);
      Body_B.Put_Line    ("}");
      Body_B.Put_Line    ("XReq_Format_Stop_Tests (format);");
      Body_B.UnIndent (2);
      Body_B.Put_Line    ("}");
      Body_B.UnIndent (2);
      Body_B.Put_Line ("}");
      Body_B.Put_Line ("XReq_Format_Free (format);");
      Body_B.Put_Line ("XReq_Conditional_Free (cond);");
      Body_B.Put_Line ("XReq_Report_step_Free (report);");
      Body_B.Put_Line ("xreqfinal();");
      Body_B.Put_Line ("return exit_code;");
      Body_B.Put_Line ("#undef self_name");
      Body_B.UnIndent (2);
      Body_B.Put_Line ("}");

      Mak_B.Put_Line ("## Autogenerated XReq Makefile ##");
      Mak_B.New_Line;
      Mak_B.Put_Line ("all: " & Name);
      Mak_B.Put_Line (".PHONY: all");
      Mak_B.New_Line;
      Mak_B.Put_Line ("SOURCES_" & Name & " = " & Name & ".c \");
      I := First (Gens);
      while Has_Element (I) loop
         E := C_Generator_Ptr (Element (I));
         Include (Sources, To_Unbounded_String
           (Goto_Path (Env.Ref.Out_Dir, To_String (E.C_File))));
         J := First (E.C_Steps);
         while Has_Element (J) loop
            Include (Sources, To_Unbounded_String
              (Goto_Path (Env.Ref.Out_Dir, To_String (Element (J)))));
            Next (J);
         end loop;
         Next (I);
      end loop;
      J := First (Sources);
      while Has_Element (J) loop
         Mak_B.Put ("  " & Element (J));
         if Has_Element (J) then
            Mak_B.Put (" \");
         end if;
         Next (J);
         Mak_B.New_Line;
      end loop;
      Mak_B.New_Line;
      Mak_B.Put_Line ("OBJECTS_" & Name & " = $(SOURCES_" & Name & ":.c=.o)");
      Mak_B.New_Line;
      Mak_B.Put_Line ("CFLAGS += \");
      for I in First_Index (Env.Ref.Step_Dir) .. Last_Index (Env.Ref.Step_Dir)
      loop
         Mak_B.Put ("  -I" &
                    Goto_Path (Env.Ref.Out_Dir,
                               To_String (Element (Env.Ref.Step_Dir, I))));
         if I < Last_Index (Env.Ref.Step_Dir) then
            Mak_B.Put (" \");
         end if;
         Mak_B.New_Line;
      end loop;
      Mak_B.New_Line;
      Mak_B.Put_Line (Name & ": $(OBJECTS_" & Name & ")");
      Mak_B.Put_Line (Name & ": LDFLAGS += -lxreq");
      Mak_B.New_Line;
      Mak_B.Put_Line ("clean:");
      Mak_B.Put_Line (ASCII.HT & "-$(RM) " & Name);
      Mak_B.Put_Line (ASCII.HT & "-$(RM) $(OBJECTS_" & Name & ")");
      Mak_B.Put_Line (".PHONY: clean");
      Mak_B.New_Line;
      Mak_B.Put_Line ("%.d: %.c");
      Mak_B.Put_Line (ASCII.HT & "$(SHELL) -ec '$(CC) -M $(CPPFLAGS) $< | " &
                           "sed -r ""s/^(\S.*)\.o([ :])/\1.o \1.d\2/"" > $@'");
      Mak_B.New_Line;
      Mak_B.Put_Line ("depend: $(SOURCES_" & Name & ":.c=.d)");
      Mak_B.Put_Line ("clean-depend:");
      Mak_B.Put_Line (ASCII.HT & "-$(RM) $(SOURCES_" & Name & ":.c=.d)");
      Mak_B.Put_Line (".PHONY: depend clean-depend");
      Mak_B.New_Line;
      Mak_B.Put_Line ("include $(SOURCES_" & Name & ":.c=.d)");
      Mak_B.New_Line;

      Set_File    (Filename, To_String (With_B.Buffer));
      Append_File (Filename, To_String (Body_B.Buffer));
      Log.Put_Line ("Generate: " & Filename);
      Set_File    (Mak_Name, To_String (Mak_B.Buffer));
      Log.Put_Line ("Generate: " & Mak_Name);
      if Make then
         --  Using make(1) -b flag to force recompilation in case the .c file
         --  is newer than the .o file for less than a second.
         declare
            Arg1    : aliased String := "-C";
            Arg2    : aliased String := Env.Ref.Out_Dir;
            Arg3    : aliased String := "-f";
            Arg4    : aliased String := Name & ".Makefile";
            Arg5    : aliased String := "-B";
            Args    : constant Argument_List (1 .. 5)
                    := (Arg1'Unchecked_Access, Arg2'Unchecked_Access,
                        Arg3'Unchecked_Access, Arg4'Unchecked_Access,
                        Arg5'Unchecked_Access);
            Buffer  : Unbounded_String;
            Success : Boolean;
            Code    : Integer;
         begin
            Log.Put_Line ("Build: make -C " & Env.Ref.Out_Dir &
                                     " -f " & Name & ".Makefile -B");
            Spawn ("make", Args, Buffer, Success, Code);
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

end XReq.Generator.C;
