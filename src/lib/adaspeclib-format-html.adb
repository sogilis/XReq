--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings;
with Ada.Strings.Fixed;
with AdaSpecLib.Format_HTML_Template;

use Ada.Strings;
use Ada.Strings.Fixed;

package body AdaSpecLib.Format.HTML is

   package Tmpl is new AdaSpecLib.Format_HTML_Template
      (New_Text_IO.File_Type, New_Text_IO.Put);

   function To_String (N : in Integer) return String;
   function Status_Class (Success : in Status_Type) return String;

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



   -------------------
   --  Start_Tests  --
   -------------------

   procedure Start_Tests    (Format     : in out HTML_Format_Type) is
   begin
      Tmpl.page_begin (Format.Output);
   end Start_Tests;

   ---------------------
   --  Start_Feature  --
   ---------------------

   procedure Start_Feature  (Format     : in out HTML_Format_Type) is
   begin
      Format.Feature_ID  := Format.Feature_ID + 1;
      Format.Background_ID := 0;
      Format.Scenario_ID   := 0;
   end Start_Feature;

   -------------------
   --  Put_Feature  --
   -------------------

   procedure Put_Feature (Format  : in out HTML_Format_Type;
                          Feature : in String)
   is
   begin
      Tmpl.feature_begin (Format.Output,
         Param_id          => To_String (Format.Feature_ID),
         Param_name        => Feature,
         Param_description => "");
   end Put_Feature;

   ----------------------
   --  Enter_Scenario  --
   ----------------------

   procedure Enter_Scenario (Format     : in out HTML_Format_Type)
   is
   begin
      Format.Background_ID := Format.Background_ID + 1;
      Format.Scenario_ID   := Format.Scenario_ID   + 1;
   end Enter_Scenario;

   ------------------------
   --  Start_Background  --
   ------------------------

   procedure Start_Background (Format   : in out HTML_Format_Type;
                               First    : in Boolean)
   is
      pragma Unreferenced (First);
   begin
      Format.In_Background := True;
   end Start_Background;

   ----------------------
   --  Put_Background  --
   ----------------------

   procedure Put_Background (Format     : in out HTML_Format_Type;
                             Background : in String)
   is
   begin
      Format.Have_Background := True;
      Tmpl.background_begin (Format.Output,
         Param_feature_id => To_String (Format.Feature_ID),
         Param_num        => To_String (Format.Background_ID),
         Param_title      => Background);
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
         Tmpl.background_end (Format.Output,
            Param_feature_id => To_String (Format.Feature_ID),
            Param_num        => To_String (Format.Background_ID));
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
   end Start_Scenario;

   --------------------
   --  Put_Scenario  --
   --------------------

   procedure Put_Scenario (Format   : in out HTML_Format_Type;
                           Scenario : in String)
   is
   begin
      Tmpl.scenario_begin (Format.Output,
         Param_feature_id => To_String (Format.Feature_ID),
         Param_num        => To_String (Format.Scenario_ID),
         Param_title      => Scenario);
   end Put_Scenario;

   ------------------
   --  Start_Step  --
   ------------------

   procedure Start_Step     (Format     : in out HTML_Format_Type) is
   begin
      Format.Step_ID := Format.Step_ID + 1;
   end Start_Step;

   ----------------
   --  Put_Step  --
   ----------------

   procedure Put_Step       (Format     : in out HTML_Format_Type;
                             Step       : in     Step_Type;
                             Name       : in     String;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type)
   is
      Stanza : Unbounded_String;
   begin
      case Step is
         when Step_Given => Append (Stanza, "Given ");
         when Step_When  => Append (Stanza, "When ");
         when Step_Then  => Append (Stanza, "Then ");
      end case;
      Append (Stanza, Name);
      Tmpl.step_begin (Format.Output,
            Param_status => Status_Class (Success),
            Param_stanza => To_String (Stanza));
      for I in Args.First_Text .. Args.Last_Text loop
         Tmpl.step_string (Format.Output,
            Param_string => Args.Text (I));
      end loop;
   end Put_Step;

   -----------------
   --  Put_Error  --
   -----------------

   procedure Put_Error      (Format     : in out HTML_Format_Type;
                             Err        : in Exception_Occurrence)
   is
      Error : constant String :=
         Exception_Name (Err) & ": " & Exception_Message (Err) & ASCII.LF &
         Exception_Information (Err);
   begin
      if Format.In_Background then
         Tmpl.step_error_background (Format.Output,
            Param_error      => Error,
            Param_feature_id => To_String (Format.Feature_ID),
            Param_num        => To_String (Format.Background_ID));
      else
         Tmpl.step_error_scenario (Format.Output,
            Param_error      => Error,
            Param_feature_id => To_String (Format.Feature_ID),
            Param_num        => To_String (Format.Scenario_ID));
      end if;
   end Put_Error;

   -----------------
   --  Stop_Step  --
   -----------------

   procedure Stop_Step      (Format     : in out HTML_Format_Type) is
   begin
      if not Format.In_Background or Format.Have_Background then
         Tmpl.step_end (Format.Output);
      end if;
   end Stop_Step;

   ---------------------
   --  Stop_Scenario  --
   ---------------------

   procedure Stop_Scenario  (Format     : in out HTML_Format_Type)
   is
   begin
      if not Format.In_Background or Format.Have_Background then
         Tmpl.scenario_end (Format.Output,
            Param_feature_id => To_String (Format.Feature_ID),
            Param_num        => To_String (Format.Scenario_ID));
      end if;
   end Stop_Scenario;

   --------------------
   --  Stop_Feature  --
   --------------------

   procedure Stop_Feature  (Format     : in out HTML_Format_Type) is
   begin
      Tmpl.feature_end (Format.Output,
         Param_feature_id => To_String (Format.Feature_ID));
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
      Count_Scenarios : constant Natural := Report.Count_Scenario_Failed +
                                            Report.Count_Scenario_Passed;
      Count_Steps     : constant Natural := Report.Count_Steps_Failed +
                                            Report.Count_Steps_Skipped +
                                            Report.Count_Steps_Passed;
      Status : Status_Type;
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
