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
   procedure Start_Scenario_Or_Outline (Format : in out HTML_Format_Type;
                                        Name   : in     String);
   procedure Put_Table (Format : in out HTML_Format_Type;
                        T      : in     Table_Type);

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
         when Status_Outline => return "outline";
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

   procedure Put_Table (Format : in out HTML_Format_Type;
                        T      : in     Table_Type) is
      use Table_Pkg;
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

   -------------------
   --  Start_Tests  --
   -------------------

   procedure Start_Tests    (Format     : in out HTML_Format_Type) is
   begin
      Format_Type (Format).Start_Tests;  --  resend
      Tmpl.page_begin (Format.Output,
         Param_title => "Test Results");  --  TODO
   end Start_Tests;

   ---------------------
   --  Start_Feature  --
   ---------------------

   procedure Start_Feature  (Format     : in out HTML_Format_Type) is
   begin
      Format_Type (Format).Start_Feature;  --  resend
      Format.Run_Feature   := False;
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
      Format.Run_Feature    := True;
   end Put_Feature;

   ----------------------
   --  Enter_Scenario  --
   ----------------------

   procedure Enter_Scenario (Format     : in out HTML_Format_Type)
   is
   begin
      Format_Type (Format).Enter_Scenario;  --  resend
      Format.Inline_Backgrnd := False;
   end Enter_Scenario;

   ---------------------
   --  Enter_Outline  --
   ---------------------

   procedure Enter_Outline  (Format     : in out HTML_Format_Type)
   is
   begin
      Format_Type (Format).Enter_Outline;  --  resend
      Format.Inline_Backgrnd := False;
   end Enter_Outline;

   ------------------------
   --  Start_Background  --
   ------------------------

   procedure Start_Background (Format   : in out HTML_Format_Type;
                               First    : in Boolean)
   is
   begin
      Format_Type (Format).Start_Background (First);  --  resend
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
                             Position   : in String;
                             Tags       : in     Tag_Array_Type)
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
      Tmpl.scenario_tags_begin (Format.Output);
      for I in Tags'Range loop
         Tmpl.scenario_tags_item (Format.Output,
            Param_tag => HTML_Text (To_String (Tags (I))));
      end loop;
      Tmpl.scenario_tags_end (Format.Output);
   end Put_Background;

   -----------------------
   --  Stop_Background  --
   -----------------------

   procedure Stop_Background  (Format   : in out HTML_Format_Type;
                               First    : in Boolean)
   is
   begin
      if Format.Have_Background then
         Format.Skip_Scenarios := Format.Curr_Scenario.Status = Status_Failed;
         Tmpl.background_end (Format.Output,
            Param_feature_id => To_String (Format.Feature_ID),
            Param_num        => To_String (Format.Background_ID));
         Append (Format.Curr_Feature.Sub_Menu, Format.Curr_Scenario);
      end if;
      Format.Have_Background := False;
      Format_Type (Format).Stop_Background (First);  --  resend
   end Stop_Background;

   ---------------------------------
   --  Start_Scenario_Or_Outline  --
   ---------------------------------

   procedure Start_Scenario_Or_Outline (Format : in out HTML_Format_Type;
                                        Name   : in     String) is
   begin
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
            Param_label => Name & ":");
      end if;
   end Start_Scenario_Or_Outline;

   ----------------------
   --  Start_Scenario  --
   ----------------------

   procedure Start_Scenario (Format     : in out HTML_Format_Type)
   is
   begin
      Format_Type (Format).Start_Scenario;  --  resend
      if not Format.In_Outline then
         Start_Scenario_Or_Outline (Format, "Scenario");
      end if;
   end Start_Scenario;

   ---------------------
   --  Start_Outline  --
   ---------------------

   procedure Start_Outline (Format     : in out HTML_Format_Type)
   is
   begin
      Format_Type (Format).Start_Outline;  --  resend
      Start_Scenario_Or_Outline (Format, "Scenario Outline");
   end Start_Outline;

   -------------------
   --  Put_Outline  --
   -------------------

   procedure Put_Outline  (Format   : in out HTML_Format_Type;
                           Scenario : in String;
                           Position : in String;
                           Tags     : in     Tag_Array_Type)
   is
   begin
      Tmpl.outline_begin (Format.Output,
         Param_feature_id => To_String (Format.Feature_ID),
         Param_num        => To_String (Format.Scenario_ID),
         Param_position   => Position,
         Param_title      => HTML_Text (Scenario));
      if Scenario /= "" then
         Format.Curr_Scenario.Name := To_Unbounded_String (Scenario);
      else
         Format.Curr_Scenario.Name := To_Unbounded_String ("Scenario Outline");
      end if;
      Tmpl.scenario_tags_begin (Format.Output);
      for I in Tags'Range loop
         Tmpl.scenario_tags_item (Format.Output,
            Param_tag => HTML_Text (To_String (Tags (I))));
      end loop;
      Tmpl.scenario_tags_end (Format.Output);
   end Put_Outline;

   --------------------
   --  Put_Scenario  --
   --------------------

   procedure Put_Scenario (Format   : in out HTML_Format_Type;
                           Scenario : in String;
                           Position : in String;
                           Tags     : in     Tag_Array_Type)
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
      Tmpl.scenario_tags_begin (Format.Output);
      for I in Tags'Range loop
         Tmpl.scenario_tags_item (Format.Output,
            Param_tag => HTML_Text (To_String (Tags (I))));
      end loop;
      Tmpl.scenario_tags_end (Format.Output);
   end Put_Scenario;

   ----------------------------
   --  Put_Scenario_Outline  --
   ----------------------------

   procedure Put_Scenario_Outline
                            (Format     : in out HTML_Format_Type;
                             Num        : in     Natural;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type)
   is
      pragma Unreferenced (Position, Tags);
      Title : Unbounded_String;
   begin
      Append (Title, "Scenario");
      if Num > 0 then
         Append (Title, Num'Img);
      end if;
      Append (Title, ": " & Scenario);
      Tmpl.outline_scenario (Format.Output,
         Param_title      => HTML_Text (To_String (Title)));
   end Put_Scenario_Outline;

   ------------------
   --  Start_Step  --
   ------------------

   procedure Start_Step     (Format     : in out HTML_Format_Type) is
   begin
      Format_Type (Format).Start_Step;  --  resend
      Format.Close_Step := False;
   end Start_Step;

   ----------------
   --  Put_Step  --
   ----------------

   procedure Put_Step       (Format     : in out HTML_Format_Type;
                             Step       : in     Step_Kind;
                             Name       : in     String;
                             Position   : in     String;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type)
   is
      use Table_Pkg;

      Inserts     : array (1 .. Name'Last + 1) of Unbounded_String;
      Stanza      : Unbounded_String;
      Left, Right : Natural; --  Left and right boundaries of matches

   begin
      Format.Has_Debug := False;
      if not (Format.In_Outline  and
              Format.In_Scenario and
              Success = Status_Passed)
      then
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
                     Param_string => HTML_Text (
                        Args.Text (Args.Elem_Idx (I))));
               when Arg_Table =>
                  Put_Table (Format, Args.Table (Args.Elem_Idx (I)));
               when Arg_Separator =>
                  if Success /= Status_Failed and not Format.Debug_Mode then
                     exit Loop_Args;
                  elsif not Format.Has_Debug and I < Args.Last then
                     Tmpl.step_debug (Format.Output);
                     Format.Has_Debug := True;
                  end if;
                  if I < Args.Last then
                     Tmpl.step_separator (Format.Output);
                  end if;
               when Arg_Paragraph =>
                  Tmpl.step_paragraph (Format.Output,
                     Param_string => HTML_Text (
                        Args.Para (Args.Elem_Idx (I))));
            end case;
         end loop Loop_Args;
      end if;
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
         if Format.Has_Debug then
            Tmpl.step_debug_end (Format.Output);
         end if;
         Tmpl.step_end (Format.Output,
            Param_num   => To_String (Format.Exec_Steps),
            Param_total => To_String (Format.Num_Steps));
      end if;
      Format_Type (Format).Stop_Step;  --  resend
   end Stop_Step;

   ---------------------
   --  Stop_Scenario  --
   ---------------------

   procedure Stop_Scenario  (Format     : in out HTML_Format_Type)
   is
   begin
      if not Format.In_Outline then
         Tmpl.scenario_end (Format.Output,
            Param_feature_id => To_String (Format.Feature_ID),
            Param_num        => To_String (Format.Scenario_ID));
         Append (Format.Curr_Feature.Sub_Menu, Format.Curr_Scenario);
         Format_Type (Format).Stop_Scenario;  --  resend
      end if;
   end Stop_Scenario;

   --------------------------
   --  Put_Outline_Report  --
   --------------------------

   procedure Put_Outline_Report
                            (Format     : in out HTML_Format_Type;
                             Table      : in     Table_Type)
   is
   begin
      Tmpl.outline_examples_begin (Format.Output);
      Put_Table (Format, Table);
      Tmpl.outline_examples_end (Format.Output);
   end Put_Outline_Report;

   --------------------
   --  Stop_Outline  --
   --------------------

   procedure Stop_Outline   (Format     : in out HTML_Format_Type)
   is
   begin
      Tmpl.outline_end (Format.Output,
         Param_feature_id => To_String (Format.Feature_ID),
         Param_num        => To_String (Format.Scenario_ID));
      Append (Format.Curr_Feature.Sub_Menu, Format.Curr_Scenario);
      Format_Type (Format).Stop_Outline;  --  resend
   end Stop_Outline;

   --------------------
   --  Stop_Feature  --
   --------------------

   procedure Stop_Feature  (Format     : in out HTML_Format_Type) is
   begin
      if Format.Run_Feature then
         Tmpl.feature_end (Format.Output,
            Param_feature_id => To_String (Format.Feature_ID));
         Append (Format.Menu, Format.Curr_Feature);
      end if;
      Format_Type (Format).Stop_Feature;  --  resend
   end Stop_Feature;

   ------------------
   --  Stop_Tests  --
   ------------------

   procedure Stop_Tests     (Format     : in out HTML_Format_Type) is
   begin
      Tmpl.page_end (Format.Output);
      Format_Type (Format).Stop_Tests;  --  resend
   end Stop_Tests;

   -------------------
   --  Put_Summary  --
   -------------------

   procedure Put_Summary    (Format     : in out HTML_Format_Type;
                             Report     : in     Report_Type;
                             D          : in     Duration)
   is
      use Ada.Containers;

      Count_Scenarios : constant Natural := Report.Count_Scenario_Failed +
                                            Report.Count_Scenario_Passed;
      Count_Steps     : constant Natural := Report.Num_Steps;
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
         Param_num_steps_pass     => To_String (Report.Count_Steps_Passed),
         Param_duration           => Get_Duration (D));
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
