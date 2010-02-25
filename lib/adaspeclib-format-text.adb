--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;

use Ada.Text_IO;

package body AdaSpecLib.Format.Text is

   procedure Put_Feature (Feature : in String) is
   begin
      Put_Line ("Feature: " & Feature);
   end Put_Feature;

   procedure Put_Background (Background : in String) is
   begin
      New_line;
      Put ("  Background:");
      if Background /= "" then
         Put (" " & Background);
      end if;
      New_line;
   end Put_Background;

   procedure Put_Scenario (Scenario : in String) is
   begin
      New_line;
      Put ("  Scenario:");
      if Scenario /= "" then
         Put (" " & Scenario);
      end if;
      New_line;
   end Put_Scenario;

   procedure Put_Step       (Step       : in Step_Type;
                             Name       : in String) is
   begin
      Put ("    ");
      case Step is
         when Step_Given => Put ("Given ");
         when Step_When  => Put ("When ");
         when Step_Then  => Put ("Then ");
      end case;
      Put (Name);
      New_Line;
   end Put_Step;

   procedure Put_Error      (Err        : in Exception_Occurrence) is
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

end AdaSpecLib.Format.Text;
