--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

use Ada.Strings;
use Ada.Strings.Fixed;

package body AdaSpecLib.Format.Text is

--    procedure Free is
--       new Ada.Unchecked_Deallocation (File_Type, File_Ptr);

   -------------------
   --  Put_Feature  --
   -------------------

   procedure Put_Feature (Format  : in out Text_Format_Type;
                          Feature : in String)
   is
   begin
      Format.Output.Put_Line ("Feature: " & Feature);
   end Put_Feature;

   procedure Put_Background (Format     : in out Text_Format_Type;
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

   procedure Put_Scenario (Format   : in out Text_Format_Type;
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

   procedure Put_Step       (Format     : in out Text_Format_Type;
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

   procedure Put_Error      (Format     : in out Text_Format_Type;
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

   procedure Put_Summary    (Format     : in out Text_Format_Type;
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

   ------------------
   --  Set_Output  --
   ------------------

   procedure Set_Output     (Format     : in out Text_Format_Type;
                             Output     : in     String)
   is
   begin
      Format.Output.Create (Ada.Text_IO.Out_File, Output);
   end Set_Output;

   -----------------------
   --  New_Text_Format  --
   -----------------------

   function New_Text_Format return Text_Format_Ptr is
   begin
      return new Text_Format_Type;
   end New_Text_Format;

   -------------------
   --  New_Text_IO  --
   -------------------

   package body New_Text_IO is

      procedure Put (File : in out File_Type;
                     Item : in     String)
      is
      begin
         if File.Output_Ownership then
            Ada.Text_IO.Put (File.Output_Ptr.all, Item);
         else
            Ada.Text_IO.Put (File.Output_Access.all, Item);
         end if;
      end Put;

      procedure Put (File : in out File_Type;
                     Item : in     Character)
      is
      begin
         File.Put ("" & Item);
      end Put;

      procedure Put_Line (File : in out File_Type;
                          Item : in     String)
      is
      begin
         if File.Output_Ownership then
            Ada.Text_IO.Put_Line (File.Output_Ptr.all, Item);
         else
            Ada.Text_IO.Put_Line (File.Output_Access.all, Item);
         end if;
      end Put_Line;

      procedure New_Line (File : in out File_Type)
      is
      begin
         if File.Output_Ownership then
            Ada.Text_IO.New_Line (File.Output_Ptr.all);
         else
            Ada.Text_IO.New_Line (File.Output_Access.all);
         end if;
      end New_Line;

      procedure Create (File : in out File_Type;
                          Mode : in     File_Mode;
                          Name : in     String := "")
      is
      begin
         File.Close;
         File.Output_Ownership := True;
         Ada.Text_IO.Create (File.Output_Ptr.all, Mode, Name);
      end Create;

      procedure Close (File : in out File_Type) is
      begin
         if Ada.Text_IO.Is_Open (File.Output_Ptr.all) then
            Ada.Text_IO.Close (File.Output_Ptr.all);
         end if;
      end Close;

      procedure Finalize (File : in out File_Type) is
         procedure Free is new
            Ada.Unchecked_Deallocation (Ada.Text_IO.File_Type, Local_File_Ptr);
      begin
         File.Close;
         Free (File.Output_Ptr);
      end Finalize;

   end New_Text_IO;

end AdaSpecLib.Format.Text;
