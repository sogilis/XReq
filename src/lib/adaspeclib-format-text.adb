--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;

use Ada.Text_IO;
use Ada.Strings;
use Ada.Strings.Fixed;

package body AdaSpecLib.Format.Text is

   procedure Put_Feature (Format     : in out Text_Format_Type;
                          Feature : in String)
   is
      pragma Unreferenced (Format);
   begin
      Put_Line ("Feature: " & Feature);
   end Put_Feature;

   procedure Put_Background (Format     : in out Text_Format_Type;
                             Background : in String)
   is
      pragma Unreferenced (Format);
   begin
      New_Line;
      Put ("  Background:");
      if Background /= "" then
         Put (" " & Background);
      end if;
      New_Line;
   end Put_Background;

   procedure Put_Scenario (Format     : in out Text_Format_Type;
                           Scenario : in String)
   is
      pragma Unreferenced (Format);
   begin
      New_Line;
      Put ("  Scenario:");
      if Scenario /= "" then
         Put (" " & Scenario);
      end if;
      New_Line;
   end Put_Scenario;

   procedure Put_Step       (Format     : in out Text_Format_Type;
                             Step       : in     Step_Type;
                             Name       : in     String;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type)
   is
      pragma Unreferenced (Format, Success);
   begin
      Put ("    ");
      case Step is
         when Step_Given => Put ("Given ");
         when Step_When  => Put ("When ");
         when Step_Then  => Put ("Then ");
      end case;
      Put (Name);
      New_Line;
      for I in Args.First_Text .. Args.Last_Text loop
         Put_Line ("      """"""");
         Put ("      ");
         declare
            Text : constant String := Args.Text (I);
            J    : Natural := Text'First;
         begin
            while J <= Text'Last loop
               Put (Text (J));
               if Text (J) = ASCII.LF then
                  Put ("      ");
               end if;
               J := J + 1;
            end loop;
         end;
         New_Line;
         Put_Line ("      """"""");
      end loop;
   end Put_Step;

   procedure Put_Error      (Format     : in out Text_Format_Type;
                             Err        : in Exception_Occurrence)
   is
      pragma Unreferenced (Format);
--       Info : constant String := Exception_Information (Err);
--       Line : Positive := 1;
   begin
      Put_Line ("      " & Exception_Name (Err) &
                ": " & Exception_Message (Err));
--       Put ("      ");
--       for I in Info'Range loop
--          if Line > 1 then
--             Put (Info (I));
--          end if;
--          if Info (I) = ASCII.LF and I /= info'Last then
--             Put ("      ");
--             Line := Line + 1;
--          end if;
--       end loop;
   end Put_Error;

   procedure Put_Summary    (Format     : in out Text_Format_Type;
                             Report     : in Report_Type)
   is
      pragma Unreferenced (Format);
      Count_Scenarios : constant Natural := Report.Count_Scenario_Failed +
                                            Report.Count_Scenario_Passed;
      Count_Steps     : constant Natural := Report.Count_Steps_Failed +
                                            Report.Count_Steps_Skipped +
                                            Report.Count_Steps_Passed;
      Need_Comma : Boolean;
   begin
      New_Line;
      Put (Trim (Count_Scenarios'Img, Left) & " scenarios (");
      Need_Comma := False;
      if Report.Count_Scenario_Failed /= 0 then
         Put (Trim (Report.Count_Scenario_Failed'Img, Left) & " failed");
         Need_Comma := True;
      end if;
      if Report.Count_Scenario_Passed /= 0 then
         if Need_Comma then Put (", "); end if;
         Put (Trim (Report.Count_Scenario_Passed'Img, Left) & " passed");
         Need_Comma := True;
      end if;
      Put (")");
      New_Line;
      Put (Trim (Count_Steps'Img, Left) & " steps (");
      Need_Comma := False;
      if Report.Count_Steps_Failed /= 0 then
         Put (Trim (Report.Count_Steps_Failed'Img, Left) & " failed");
         Need_Comma := True;
      end if;
      if Report.Count_Steps_Skipped /= 0 then
         if Need_Comma then Put (", "); end if;
         Put (Trim (Report.Count_Steps_Skipped'Img, Left) & " skipped");
         Need_Comma := True;
      end if;
      if Report.Count_Steps_Passed /= 0 then
         if Need_Comma then Put (", "); end if;
         Put (Trim (Report.Count_Steps_Passed'Img, Left) & " passed");
         Need_Comma := True;
      end if;
      Put (")");
      New_Line;
   end Put_Summary;

   -----------------------
   --  New_Text_Format  --
   -----------------------

   function New_Text_Format return Text_Format_Ptr is
   begin
      return new Text_Format_Type;
   end New_Text_Format;

end AdaSpecLib.Format.Text;
