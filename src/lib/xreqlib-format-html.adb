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
--  with Ada.Exceptions.Traceback;
with XReqLib.Error_Handling;
with XReqLib.Format_HTML_Template;

use Ada.Strings;
use Ada.Strings.Fixed;

package body XReqLib.Format.HTML is

   package Tmpl is new XReqLib.Format_HTML_Template
      (New_Text_IO.File_Type, New_Text_IO.Put);

   function To_String (N : in Integer) return String;
   function Status_Class (Success : in Status_Type) return String;
   function HTML_Text (S : in String) return String;
   function HTML_Text (S : in Unbounded_String) return String;
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

   function HTML_Text (S : in Unbounded_String) return String is
   begin
      return HTML_Text (To_String (S));
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

   procedure Put_Tags    (Format     : in out HTML_Format_Type;
                          Tags       : in     Tag_Array_Type);

   ----------------
   --  Put_Tags  --
   ----------------

   procedure Put_Tags    (Format     : in out HTML_Format_Type;
                          Tags       : in     Tag_Array_Type) is
   begin
      Tmpl.scenario_tags_begin (Format.Output);
      for I in Tags'Range loop
         Tmpl.scenario_tags_item (Format.Output,
            Param_tag => HTML_Text (To_String (Tags (I))));
      end loop;
      Tmpl.scenario_tags_end (Format.Output);
   end Put_Tags;

   -------------------
   --  Begin_Tests  --
   -------------------

   procedure Begin_Tests    (Format     : in out HTML_Format_Type) is
   begin
      Tmpl.page_begin (Format.Output,
         Param_title => "Test Results");  --  TODO
   end Begin_Tests;

   -----------------
   --  End_Tests  --
   -----------------

   procedure End_Tests     (Format     : in out HTML_Format_Type) is
   begin
      Tmpl.page_end (Format.Output);
   end End_Tests;

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

   -------------------
   --  Put_Feature  --
   -------------------

   procedure Put_Feature (Format      : in out HTML_Format_Type)
   is
   begin
      Format.Output.Buffer_Start;
      Tmpl.feature_begin (Format.Output,
         Param_id          => To_String (Format.Feature_ID),
         Param_position    => HTML_Text (Format.Feature.Position),
         Param_STR_Feature => Format.S_Feature,
         Param_name        => HTML_Text (Format.Feature.Name),
         Param_description => HTML_Text (Format.Feature.Description));
      Format.Curr_Feature := Menu_Item_1'
        (Name       => Format.Feature.Name,
         Status     => Status_Skipped,
         Feature_ID => Format.Feature_ID,
         others     => <>);
   end Put_Feature;

   --------------------
   --  End_Feature  --
   --------------------

   procedure End_Feature  (Format     : in out HTML_Format_Type) is
   begin
      if not Format.Output.Buffered then
         Tmpl.feature_end (Format.Output,
            Param_feature_id => To_String (Format.Feature_ID));
         Append (Format.Menu, Format.Curr_Feature);
      else
         Format.Output.Buffer_Discard;
      end if;
   end End_Feature;

   ---------------------
   --  Enter_Outline  --
   ---------------------

   procedure Enter_Outline  (Format     : in out HTML_Format_Type)
   is
   begin
      Tmpl.outline_begin
        (Format.Output,
         Param_feature_id  => To_String (Format.Feature_ID),
         Param_num         => To_String (Format.Scenario_ID),
         Param_position    => HTML_Text (Format.Outline.Position),
         Param_STR_Outline => Format.S_Outline,
         Param_title       => HTML_Text (Format.Outline.Name));

      Format.Curr_Scenario :=
        Menu_Item_2'(Name          => Format.Outline.Name,
                     Status        => Status_Skipped,
                     Is_Background => False,
                     Feature_ID    => Format.Feature_ID,
                     Scenario_ID   => Format.Scenario_ID);
      if Format.Curr_Scenario.Name = "" then
         Format.Curr_Scenario.Name := To_Unbounded_String
           ("Scenario Outline #" & To_String (Format.Scenario_ID));
      end if;

      Put_Tags (Format, Convert (Format.Outline.Tags));
   end Enter_Outline;

   ---------------------
   --  Begin_Outline  --
   ---------------------

   procedure Begin_Outline  (Format     : in out HTML_Format_Type)
   is
   begin
      Tmpl.outline_examples_begin (Format.Output);
   end Begin_Outline;


   --------------------------
   --  Put_Outline_Report  --
   --------------------------

   procedure Put_Outline_Report
                            (Format     : in out HTML_Format_Type;
                             Table      : in     Table_Type)
   is
   begin
      Put_Table (Format, Table);
      Tmpl.outline_examples_end (Format.Output);
      Tmpl.outline_end
        (Format.Output,
         Param_feature_id => To_String (Format.Feature_ID),
         Param_num        => To_String (Format.Scenario_ID));
      Append (Format.Curr_Feature.Sub_Menu, Format.Curr_Scenario);
   end Put_Outline_Report;


   ----------------------
   --  Enter_Scenario  --
   ----------------------

   procedure Enter_Scenario (Format     : in out HTML_Format_Type)
   is
   begin
      if not Format.In_Outline then
         Format.Curr_Scenario :=
           (Name          => Format.Scenario.Name,
            Status        => Status_Skipped,
            Is_Background => False,
            Feature_ID    => Format.Feature_ID,
            Scenario_ID   => Format.Scenario_ID);
         if Format.Curr_Scenario.Name = "" then
            Format.Curr_Scenario.Name := To_Unbounded_String
              ("Scenario #" & To_String (Format.Scenario_ID));
         end if;
      end if;
      Tmpl.scenario_begin
        (Format.Output,
         Param_feature_id   => To_String (Format.Feature_ID),
         Param_num          => To_String (Format.Scenario_ID),
         Param_position     => HTML_Text (Format.Scenario.Position),
         Param_STR_Scenario => Format.S_Scenario,
         Param_title        => HTML_Text (Format.Scenario.Name));
      if not Format.In_Outline then
         Put_Tags (Format, Convert (Format.Scenario.Tags));
      end if;
   end Enter_Scenario;

   ----------------------
   --  Begin_Scenario  --
   ----------------------

   procedure Begin_Scenario (Format   : in out HTML_Format_Type)
   is
   begin
      null;
   end Begin_Scenario;

   --------------------
   --  End_Scenario  --
   --------------------

   procedure End_Scenario  (Format     : in out HTML_Format_Type)
   is
   begin
      Tmpl.scenario_end
        (Format.Output,
         Param_feature_id => To_String (Format.Feature_ID),
         Param_num        => To_String (Format.Scenario_ID));
      if not Format.In_Outline then
         Append (Format.Curr_Feature.Sub_Menu, Format.Curr_Scenario);
      end if;
   end End_Scenario;

   ------------------------
   --  Begin_Background  --
   ------------------------

   procedure Begin_Background (Format   : in out HTML_Format_Type)
   is
   begin
      Tmpl.background_begin
        (Format.Output,
         Param_feature_id => To_String (Format.Feature_ID),
         Param_num        => To_String (Format.Scenario_ID),
         Param_position   => HTML_Text (Format.Background.Position),
         Param_title      => HTML_Text (Format.Background.Name));
   end Begin_Background;

   ----------------------
   --  End_Background  --
   ----------------------

   procedure End_Background  (Format   : in out HTML_Format_Type)
   is
   begin
      Tmpl.background_end (Format.Output);
   end End_Background;

   ----------------
   --  Put_Step  --
   ----------------

   procedure Put_Step       (Format     : in out HTML_Format_Type;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type)
   is
      use Table_Pkg;

      Name        : constant String := To_String (Format.Step.Name);
      Inserts     : array (1 .. Name'Last + 1) of Unbounded_String;
      Stanza      : Unbounded_String;
      Left, Right : Natural; --  Left and right boundaries of matches
      Has_Debug   : Boolean renames Format.Has_Debug;

   begin
      Format.Output.Buffer_Commit;
      Has_Debug := False;

      --  Propagate step status to scenario and feature

      if Success = Status_Failed then
         Format.Curr_Feature.Status  := Status_Failed;
         Format.Curr_Scenario.Status := Status_Failed;
      elsif Success = Status_Passed then
         if Format.Curr_Feature.Status = Status_Skipped then
            Format.Curr_Feature.Status := Status_Passed;
         end if;
         if Format.Curr_Scenario.Status = Status_Skipped then
            Format.Curr_Scenario.Status := Status_Passed;
         end if;
         Tmpl.step_success
           (Format.Output,
            Param_feature_id => To_String (Format.Feature_ID),
            Param_num        => To_String (Format.Scenario_ID));
      end if;

      --  Prefix step stanza

      case Format.Step.Kind is
         when Step_Given => Append (Stanza, "Given ");
         when Step_When  => Append (Stanza, "When ");
         when Step_Then  => Append (Stanza, "Then ");
      end case;

      --  Mark the stanza captures

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

      --  Template

      if Format.In_Background then
         Tmpl.step_begin
           (Format.Output,
            Param_status     => Status_Class (Success),
            Param_position   => HTML_Text (Format.Step.Position),
            Param_stanza     => To_String (Stanza),
            Param_background => "background ");
      else
         Tmpl.step_begin
           (Format.Output,
            Param_status     => Status_Class (Success),
            Param_position   => HTML_Text (Format.Step.Position),
            Param_stanza     => To_String (Stanza),
            Param_background => "");
      end if;

      --  Arguments

      Loop_Args :
      for I in Args.First .. Args.Last loop
         case Args.Elem_Type (I) is

         when Arg_Text =>
            Tmpl.step_string
              (Format.Output,
               Param_string => HTML_Text (Args.Text (Args.Elem_Idx (I))));

         when Arg_Table =>
            Put_Table (Format, Args.Table (Args.Elem_Idx (I)));

         when Arg_Separator =>
            if not Has_Debug and I < Args.Last then
               Tmpl.step_debug (Format.Output);
               Has_Debug := True;
            end if;
            if I < Args.Last then
               Tmpl.step_separator (Format.Output);
            end if;

         when Arg_Paragraph =>
            Tmpl.step_paragraph
              (Format.Output,
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
      use XReqLib.Error_Handling;
      Error : constant String := Exception_To_String (Err);
   begin
      Tmpl.step_error
        (Format.Output,
         Param_error      => HTML_Text (Error),
         Param_trace      => HTML_Text (Symbolic_Traceback (Err)),
         Param_feature_id => To_String (Format.Feature_ID),
         Param_num        => To_String (Format.Scenario_ID));
   end Put_Error;

   -----------------
   --  Stop_Step  --
   -----------------

   procedure End_Step      (Format     : in out HTML_Format_Type) is
   begin
      if Format.Has_Debug then
         Tmpl.step_debug_end (Format.Output);
      end if;
      Tmpl.step_end
        (Format.Output,
         Param_num   => To_String (Format.Exec_Steps),
         Param_total => To_String (Format.Num_Steps));
      Format.Previous_Step_Type := Format.Step.Kind;
   end End_Step;
   -----------------------
   --  New_HTML_Format  --
   -----------------------

   function New_HTML_Format return HTML_Format_Ptr is
   begin
      return new HTML_Format_Type;
   end New_HTML_Format;

end XReqLib.Format.HTML;
