--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings;
with Ada.Strings.Fixed;
with AdaSpecLib.Format_HTML_Template;

use Ada.Strings;
use Ada.Strings.Fixed;

package body AdaSpecLib.Format.HTML is

   package Tmpl is new AdaSpecLib.Format_HTML_Template
      (New_Text_IO.File_Type, New_Text_IO.Put);

   -------------------
   --  Start_Tests  --
   -------------------

   procedure Start_Tests    (Format     : in out HTML_Format_Type) is
   begin
      pragma Style_Checks (Off);
--       Format.Output.Put_Line ("<?xml version=""1.0"" encoding=""UTF-8""?>");
--       Format.Output.Put_Line ("<!DOCTYPE html PUBLIC ""-//W3C//DTD XHTML 1.0 Strict//EN"" ""http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"">");
--       Format.Output.Put_Line ("<html xmlns=""http://www.w3.org/1999/xhtml"" xml:lang=""en"" lang=""en"">");
--       Format.Output.Put_Line (" <body>");
--       Format.Output.Put_Line ("  <pre>");
      pragma Style_Checks (On);
      Tmpl.page_begin (Format.Output);
   end Start_Tests;

   ------------------
   --  Stop_Tests  --
   ------------------

   procedure Stop_Tests     (Format     : in out HTML_Format_Type) is
   begin
--       Format.Output.Put_Line ("  </pre>");
--       Format.Output.Put_Line (" </body>");
--       Format.Output.Put_Line ("</html>");
      Tmpl.page_end (Format.Output);
   end Stop_Tests;

   -------------------
   --  Put_Feature  --
   -------------------

   procedure Put_Feature (Format  : in out HTML_Format_Type;
                          Feature : in String)
   is
   begin
      Format.Output.Put_Line ("Feature: " & Feature);
   end Put_Feature;

   procedure Put_Background (Format     : in out HTML_Format_Type;
                             Background : in String)
   is
   begin
      Format.Output.New_Line;
      Format.Output.Put ("  Background:");
      if Background /= "" then
         Format.Output.Put (" " & Background);
      end if;
      Format.Output.New_Line;
   end Put_Background;

   procedure Put_Scenario (Format   : in out HTML_Format_Type;
                           Scenario : in String)
   is
   begin
      Format.Output.New_Line;
      Format.Output.Put ("  Scenario:");
      if Scenario /= "" then
         Format.Output.Put (" " & Scenario);
      end if;
      Format.Output.New_Line;
   end Put_Scenario;

   ----------------
   --  Put_Step  --
   ----------------

   procedure Put_Step       (Format     : in out HTML_Format_Type;
                             Step       : in     Step_Type;
                             Name       : in     String;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type)
   is
      pragma Unreferenced (Success);
   begin
      Format.Output.Put ("    ");
      case Step is
         when Step_Given => Format.Output.Put ("Given ");
         when Step_When  => Format.Output.Put ("When ");
         when Step_Then  => Format.Output.Put ("Then ");
      end case;
      Format.Output.Put (Name);
      Format.Output.New_Line;
      for I in Args.First_Text .. Args.Last_Text loop
         Format.Output.Put_Line ("      """"""");
         Format.Output.Put      ("      ");
         declare
            Text : constant String := Args.Text (I);
            J    : Natural := Text'First;
         begin
            while J <= Text'Last loop
               Format.Output.Put (Text (J));
               if Text (J) = ASCII.LF then
                  Format.Output.Put ("      ");
               end if;
               J := J + 1;
            end loop;
         end;
         Format.Output.New_Line;
         Format.Output.Put_Line ("      """"""");
      end loop;
   end Put_Step;

   -----------------
   --  Put_Error  --
   -----------------

   procedure Put_Error      (Format     : in out HTML_Format_Type;
                             Err        : in Exception_Occurrence)
   is
--       Info : constant String := Exception_Information (Err);
--       Line : Positive := 1;
   begin
      Format.Output.Put_Line ("      " & Exception_Name (Err) &
                ": " & Exception_Message (Err));
--       Format.Output.Put ("      ");
--       for I in Info'Range loop
--          if Line > 1 then
--             Format.Output.Put (Info (I));
--          end if;
--          if Info (I) = ASCII.LF and I /= info'Last then
--             Format.Output.Put ("      ");
--             Line := Line + 1;
--          end if;
--       end loop;
   end Put_Error;

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
      Need_Comma : Boolean;
   begin
      Format.Output.New_Line;
      if Count_Scenarios > 1 then
         Format.Output.Put
            (Trim (Count_Scenarios'Img, Left) & " scenarios (");
      else
         Format.Output.Put
            (Trim (Count_Scenarios'Img, Left) & " scenario (");
      end if;
      Need_Comma := False;
      if Report.Count_Scenario_Failed /= 0 then
         Format.Output.Put
            (Trim (Report.Count_Scenario_Failed'Img, Left) & " failed");
         Need_Comma := True;
      end if;
      if Report.Count_Scenario_Passed /= 0 then
         if Need_Comma then Format.Output.Put (", "); end if;
         Format.Output.Put
            (Trim (Report.Count_Scenario_Passed'Img, Left) & " passed");
         Need_Comma := True;
      end if;
      Format.Output.Put (")");
      Format.Output.New_Line;
      if Count_Steps > 1 then
         Format.Output.Put
            (Trim (Count_Steps'Img, Left) & " steps (");
      else
         Format.Output.Put
            (Trim (Count_Steps'Img, Left) & " step (");
      end if;
      Need_Comma := False;
      if Report.Count_Steps_Failed /= 0 then
         Format.Output.Put
            (Trim (Report.Count_Steps_Failed'Img, Left) & " failed");
         Need_Comma := True;
      end if;
      if Report.Count_Steps_Skipped /= 0 then
         if Need_Comma then Format.Output.Put (", "); end if;
         Format.Output.Put
            (Trim (Report.Count_Steps_Skipped'Img, Left) & " skipped");
         Need_Comma := True;
      end if;
      if Report.Count_Steps_Passed /= 0 then
         if Need_Comma then Format.Output.Put (", "); end if;
         Format.Output.Put
            (Trim (Report.Count_Steps_Passed'Img, Left) & " passed");
         Need_Comma := True;
      end if;
      Format.Output.Put (")");
      Format.Output.New_Line;
   end Put_Summary;

   -----------------------
   --  New_HTML_Format  --
   -----------------------

   function New_HTML_Format return HTML_Format_Ptr is
   begin
      return new HTML_Format_Type;
   end New_HTML_Format;

end AdaSpecLib.Format.HTML;
