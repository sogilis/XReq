--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings;
with Ada.Strings.Fixed;
--  with Ada.Exceptions.Traceback;
with GNAT.Traceback.Symbolic;
with AdaSpecLib.Format_HTML_Template;

use Ada.Strings;
use Ada.Strings.Fixed;

package body AdaSpecLib.Format.HTML is

   package Tmpl is new AdaSpecLib.Format_HTML_Template
      (New_Text_IO.File_Type, New_Text_IO.Put);

   function To_String (N : in Integer) return String;
   function Status_Class (Success : in Status_Type) return String;
   function HTML_Text (S : in String) return String;

   use Menu_Vectors_2;
   use Menu_Vectors;

   function To_String (N : in Integer) return String is
   begin
      return Trim (N'Img, Left);
   end To_String;

   function Status_Class (Success : in Status_Type) return String is
   begin
      case Success is
         when Status_Passed  => return "pass";
         when Status_Skipped => return "skip";
         when Status_Failed  => return "fail";
      end case;
   end Status_Class;

   function HTML_Text (S : in String) return String is
      Buffer : Unbounded_String;
   begin
      for I in S'Range loop
         case S (I) is
            when '&' =>
               Append (Buffer, "&amp;");
            when '<' =>
               Append (Buffer, "&lt;");
            when '>' =>
               Append (Buffer, "&gt;");
            when others =>
               Append (Buffer, S (I));
         end case;
      end loop;
      return To_String (Buffer);
   end HTML_Text;

   -------------------
   --  Start_Tests  --
   -------------------

   procedure Start_Tests    (Format     : in out HTML_Format_Type) is
   begin
      Tmpl.page_begin (Format.Output,
         Param_title => "Test Results");  --  TODO
   end Start_Tests;

   ---------------------
   --  Start_Feature  --
   ---------------------

   procedure Start_Feature  (Format     : in out HTML_Format_Type) is
   begin
      Format.Feature_ID    := Format.Feature_ID + 1;
      Format.Background_ID := 0;
      Format.Scenario_ID   := 0;
      Format.Curr_Feature  := (
         Name          => To_Unbounded_String ("Feature"),
         Feature_ID    => Format.Feature_ID,
         others        => <>);
   end Start_Feature;

   -------------------
   --  Put_Feature  --
   -------------------

   procedure Put_Feature (Format  : in out HTML_Format_Type;
                          Feature : in String;
                          Description : in String;
                          Position    : in String)
   is
   begin
      Format.Curr_Feature.Name := To_Unbounded_String (Feature);
      Tmpl.feature_begin (Format.Output,
         Param_id          => To_String (Format.Feature_ID),
         Param_position    => Position,
         Param_name        => HTML_Text (Feature),
         Param_description => HTML_Text (Description));
      Format.Skip_Scenarios := False;
   end Put_Feature;

   ----------------------
   --  Enter_Scenario  --
   ----------------------

   procedure Enter_Scenario (Format     : in out HTML_Format_Type)
   is
   begin
      Format.Scenario_ID := Format.Scenario_ID + 1;
      Format.Inline_Backgrnd := False;
   end Enter_Scenario;

   ------------------------
   --  Start_Background  --
   ------------------------

   procedure Start_Background (Format   : in out HTML_Format_Type;
                               First    : in Boolean)
   is
      pragma Unreferenced (First);
   begin
      Format.Background_ID := Format.Background_ID + 1;
      Format.In_Background := True;
      Format.Curr_Scenario := (
         Is_Background => True,
         Name          => Format.Curr_Scenario.Name,
         Feature_ID    => Format.Feature_ID,
         Scenario_ID   => Format.Background_ID,
         others        => <>);
   end Start_Background;

   ----------------------
   --  Put_Background  --
   ----------------------

   procedure Put_Background (Format     : in out HTML_Format_Type;
                             Background : in String;
                             Position   : in String)
   is
   begin
      if Background /= "" then
         Format.Curr_Scenario.Name := To_Unbounded_String
            ("Background: " & Background);
      else
         Format.Curr_Scenario.Name := To_Unbounded_String ("Background");
      end if;
      Format.Have_Background := True;
      Tmpl.background_begin (Format.Output,
         Param_feature_id => To_String (Format.Feature_ID),
         Param_num        => To_String (Format.Background_ID),
         Param_position   => Position,
         Param_title      => HTML_Text (Background));
   end Put_Background;

   -----------------------
   --  Stop_Background  --
   -----------------------

   procedure Stop_Background  (Format   : in out HTML_Format_Type;
                               First    : in Boolean)
   is
      pragma Unreferenced (First);
   begin
      if Format.Have_Background then
         Format.Skip_Scenarios := Format.Curr_Scenario.Status = Status_Failed;
         Tmpl.background_end (Format.Output,
            Param_feature_id => To_String (Format.Feature_ID),
            Param_num        => To_String (Format.Background_ID));
         Append (Format.Curr_Feature.Sub_Menu, Format.Curr_Scenario);
      end if;
      Format.Have_Background := False;
      Format.In_Background   := False;
   end Stop_Background;

   ----------------------
   --  Start_Scenario  --
   ----------------------

   procedure Start_Scenario (Format     : in out HTML_Format_Type)
   is
   begin
      Format.Step_ID := 0;
      Format.Curr_Scenario := (
         Name          => Format.Curr_Scenario.Name,
         Status        => Format.Curr_Scenario.Status,
         Is_Background => False,
         Feature_ID    => Format.Feature_ID,
         Scenario_ID   => Format.Scenario_ID,
         others        => <>);
      if Format.Skip_Scenarios then
         Format.Curr_Scenario.Status := Status_Skipped;
      end if;
      if Format.Inline_Backgrnd then
         Tmpl.scenario_label (Format.Output,
            Param_label => "Scenario:");
      end if;
   end Start_Scenario;

   --------------------
   --  Put_Scenario  --
   --------------------

   procedure Put_Scenario (Format   : in out HTML_Format_Type;
                           Scenario : in String;
                           Position : in String)
   is
   begin
      Tmpl.scenario_begin (Format.Output,
         Param_feature_id => To_String (Format.Feature_ID),
         Param_num        => To_String (Format.Scenario_ID),
         Param_position   => Position,
         Param_title      => HTML_Text (Scenario));
      if Scenario /= "" then
         Format.Curr_Scenario.Name := To_Unbounded_String (Scenario);
      else
         Format.Curr_Scenario.Name := To_Unbounded_String ("Scenario");
      end if;
   end Put_Scenario;

   ------------------
   --  Start_Step  --
   ------------------

   procedure Start_Step     (Format     : in out HTML_Format_Type) is
   begin
      Format.Close_Step := False;
      Format.Step_ID    := Format.Step_ID + 1;
   end Start_Step;

   ----------------
   --  Put_Step  --
   ----------------

   procedure Put_Step       (Format     : in out HTML_Format_Type;
                             Step       : in     Step_Type;
                             Name       : in     String;
                             Position   : in     String;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type)
   is
      use Table_Pkg;
      procedure Put_Table (T : in Table_Type);

      Inserts     : array (1 .. Name'Last + 1) of Unbounded_String;
      Stanza      : Unbounded_String;
      Left, Right : Natural; --  Left and right boundaries of matches


      procedure Put_Table (T : in Table_Type) is
         Cell    : Unbounded_String;
         Cell_Ok : Boolean;
      begin
         Tmpl.step_table_begin (Format.Output);
         for Y in T.First_Y .. T.Last_Y loop
            Tmpl.step_table_row_begin (Format.Output);
            for X in T.First_X .. T.Last_X loop
               T.Item (X, Y, Cell, Cell_Ok);
               if Cell_Ok then
                  Tmpl.step_table_cell (Format.Output,
                     Param_string => HTML_Text (To_String (Cell)));
               else
                  Tmpl.step_table_cell (Format.Output,
                     Param_string => "");
               end if;
            end loop;
            Tmpl.step_table_row_end (Format.Output);
         end loop;
         Tmpl.step_table_end (Format.Output);
      end Put_Table;

   begin
      if Format.In_Background and
         (not Format.Have_Background) and
         (not Format.Inline_Backgrnd)
      then
         Tmpl.scenario_label (Format.Output,
            Param_label => "Background:");
         Format.Inline_Backgrnd := True;
      end if;
      if Success = Status_Failed then
         Format.Curr_Feature.Status := Status_Failed;
         Format.Curr_Scenario.Status := Status_Failed;
      end if;
      Format.Close_Step := True;
      case Step is
         when Step_Given => Append (Stanza, "Given ");
         when Step_When  => Append (Stanza, "When ");
         when Step_Then  => Append (Stanza, "Then ");
      end case;
      for I in 1 .. Args.Last_Match loop
         Args.Match (I, Left, Right);
         Append (Inserts (Left), "<span class=""capture"">");
         Insert (Inserts (Right + 1), 1, "</span>");
      end loop;
      for I in Name'Range loop
         Append (Stanza, Inserts (I));
         Append (Stanza, HTML_Text ("" & Name (I)));
      end loop;
      Append (Stanza, Inserts (Inserts'Last));
      Tmpl.step_begin (Format.Output,
            Param_status   => Status_Class (Success),
            Param_position => Position,
            Param_stanza   => To_String (Stanza));

      Loop_Args :
      for I in Args.First .. Args.Last loop
         case Args.Elem_Type (I) is
            when Arg_Text =>
               Tmpl.step_string (Format.Output,
                  Param_string => HTML_Text (Args.Text (Args.Elem_Idx (I))));
            when Arg_Table =>
               Put_Table (Args.Table (Args.Elem_Idx (I)));
            when Arg_Separator =>
               if Success /= Status_Failed and not Format.Debug_Mode then
                  exit Loop_Args;
               end if;
               if I < Args.Last then
                  Tmpl.step_separator (Format.Output);
               end if;
            when Arg_Paragraph =>
               Tmpl.step_paragraph (Format.Output,
                  Param_string => HTML_Text (Args.Para (Args.Elem_Idx (I))));
         end case;
      end loop Loop_Args;
   end Put_Step;

   -----------------
   --  Put_Error  --
   -----------------

   procedure Put_Error      (Format     : in out HTML_Format_Type;
                             Err        : in Exception_Occurrence)
   is
      --  use Ada.Exceptions.Traceback;
      use GNAT.Traceback.Symbolic;
      --  Trace : constant Tracebacks_Array := Tracebacks (Err);
      Error : constant String :=
         Exception_Name (Err) & ": " & Exception_Message (Err) & ASCII.LF;
   begin
      if Format.Have_Background then
         Tmpl.step_error_background (Format.Output,
            Param_error      => HTML_Text (Error),
            Param_trace      => HTML_Text (Symbolic_Traceback (Err)),
            Param_feature_id => To_String (Format.Feature_ID),
            Param_num        => To_String (Format.Background_ID));
      else
         Tmpl.step_error_scenario (Format.Output,
            Param_error      => HTML_Text (Error),
            Param_trace      => HTML_Text (Symbolic_Traceback (Err)),
            Param_feature_id => To_String (Format.Feature_ID),
            Param_num        => To_String (Format.Scenario_ID));
      end if;
   end Put_Error;

   -----------------
   --  Stop_Step  --
   -----------------

   procedure Stop_Step      (Format     : in out HTML_Format_Type) is
   begin
      if Format.Close_Step then
         Tmpl.step_end (Format.Output);
      end if;
   end Stop_Step;

   ---------------------
   --  Stop_Scenario  --
   ---------------------

   procedure Stop_Scenario  (Format     : in out HTML_Format_Type)
   is
   begin
      Tmpl.scenario_end (Format.Output,
         Param_feature_id => To_String (Format.Feature_ID),
         Param_num        => To_String (Format.Scenario_ID));
      Append (Format.Curr_Feature.Sub_Menu, Format.Curr_Scenario);
   end Stop_Scenario;

   --------------------
   --  Stop_Feature  --
   --------------------

   procedure Stop_Feature  (Format     : in out HTML_Format_Type) is
   begin
      Tmpl.feature_end (Format.Output,
         Param_feature_id => To_String (Format.Feature_ID));
      Append (Format.Menu, Format.Curr_Feature);
   end Stop_Feature;

   ------------------
   --  Stop_Tests  --
   ------------------

   procedure Stop_Tests     (Format     : in out HTML_Format_Type) is
   begin
      Tmpl.page_end (Format.Output);
   end Stop_Tests;

   -------------------
   --  Put_Summary  --
   -------------------

   procedure Put_Summary    (Format     : in out HTML_Format_Type;
                             Report     : in Report_Type)
   is
      use Ada.Containers;

      Count_Scenarios : constant Natural := Report.Count_Scenario_Failed +
                                            Report.Count_Scenario_Passed;
      Count_Steps     : constant Natural := Report.Count_Steps_Failed +
                                            Report.Count_Steps_Skipped +
                                            Report.Count_Steps_Passed;
      Status : Status_Type;
      E1     : Menu_Item_1;
      E2     : Menu_Item_2;
   begin

      if (Report.Count_Scenario_Failed +
          Report.Count_Steps_Skipped +
          Report.Count_Steps_Failed) /= 0
      then
         Status := Status_Failed;
      else
         Status := Status_Passed;
      end if;

      Tmpl.report_begin (Format.Output,
         Param_status             => Status_Class (Status),
         Param_num_scenarios      => To_String (Count_Scenarios),
         Param_num_scenarios_fail => To_String (Report.Count_Scenario_Failed),
         Param_num_scenarios_pass => To_String (Report.Count_Scenario_Passed),
         Param_num_steps          => To_String (Count_Steps),
         Param_num_steps_fail     => To_String (Report.Count_Steps_Failed),
         Param_num_steps_skip     => To_String (Report.Count_Steps_Skipped),
         Param_num_steps_pass     => To_String (Report.Count_Steps_Passed));
      Tmpl.report_menu_begin (Format.Output);
      for I in 1 .. Integer (Length (Format.Menu)) loop
         E1 := Element (Format.Menu, I);
         Tmpl.report_menu_feature_begin (Format.Output,
            Param_status     => Status_Class (E1.Status),
            Param_feature_id => To_String    (E1.Feature_ID),
            Param_name       => HTML_Text (To_String (E1.Name)));
         if Length (E1.Sub_Menu) > 0 then
            Tmpl.report_menu_scenarios_begin (Format.Output);
            for J in 1 .. Integer (Length (E1.Sub_Menu)) loop
               E2 := Element (E1.Sub_Menu, J);
               if E2.Is_Background then
                  Tmpl.report_menu_background (Format.Output,
                     Param_status     => Status_Class (E2.Status),
                     Param_feature_id => To_String    (E2.Feature_ID),
                     Param_num        => To_String    (E2.Scenario_ID),
                     Param_name       => HTML_Text (To_String (E2.Name)));
               else
                  Tmpl.report_menu_scenario (Format.Output,
                     Param_status     => Status_Class (E2.Status),
                     Param_feature_id => To_String    (E2.Feature_ID),
                     Param_num        => To_String    (E2.Scenario_ID),
                     Param_name       => HTML_Text (To_String (E2.Name)));
               end if;
            end loop;
            Tmpl.report_menu_scenarios_end (Format.Output);
         end if;
         Tmpl.report_menu_feature_end (Format.Output);
      end loop;
      Tmpl.report_menu_end (Format.Output);
      Tmpl.report_end (Format.Output);
   end Put_Summary;

   -----------------------
   --  New_HTML_Format  --
   -----------------------

   function New_HTML_Format return HTML_Format_Ptr is
   begin
      return new HTML_Format_Type;
   end New_HTML_Format;

end AdaSpecLib.Format.HTML;
