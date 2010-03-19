--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

use Ada.Strings;
use Ada.Strings.Fixed;
use Ada.Strings.Unbounded;

package body AdaSpecLib.Format.Text is

--    procedure Free is
--       new Ada.Unchecked_Deallocation (File_Type, File_Ptr);

   procedure Put_Table      (Format     : in out Text_Format_Type;
                             T          : in     Table_Type;
                             Indent     : in     String);
   procedure Put_Table      (Format     : in out Text_Format_Type;
                             T          : in     Table_Type;
                             Indent     : in     String)
   is
      function Is_Num (S : in String) return Boolean;
      function Is_Num (S : in String) return Boolean is
         I : Integer;
      begin
         I := Integer'Value (S);
         return I = 0 or I /= 0 or True;
      exception
         when others =>
            return False;
      end Is_Num;

      Cell    : Unbounded_String;
      Cell_Ok : Boolean;
      Num     : Boolean;
      Pad     : Integer;
      Width   : array (T.First_X .. T.Last_X) of Natural;
   begin
      for X in Width'Range loop
         Width (X) := T.Width (X);
      end loop;
      for Y in T.First_Y .. T.Last_Y loop
         Format.Output.Put (Indent & "|");
         for X in T.First_X .. T.Last_X loop
            T.Item (X, Y, Cell, Cell_Ok);
            if Cell_Ok then
               Num := Is_Num (To_String (Cell));
               Format.Output.Put (" ");
               Pad := Width (X) - Length (Cell);
               if Num and Pad > 0 then
                  Format.Output.Put (Pad * " ");
               end if;
               Format.Output.Put (To_String (Cell));
               if not Num and Pad > 0 then
                  Format.Output.Put (Pad * " ");
               end if;
            end if;
            Format.Output.Put (" |");
         end loop;
         Format.Output.New_Line;
      end loop;
   end Put_Table;

   -------------------
   --  Put_Feature  --
   -------------------

   procedure Put_Feature (Format      : in out Text_Format_Type;
                          Feature     : in     String;
                          Description : in     String;
                          Position    : in     String)
   is
      pragma Unreferenced (Position);
      Has_Description : Boolean := False;
   begin
      if Format.First_Feature then
         Format.First_Feature := False;
      else
         Format.Output.New_Line;
      end if;
      Format.Output.Put_Line ("Feature: " & Feature);
      for I in Description'Range loop
         if Description (I) = ASCII.LF then
            if I < Description'Last then
               Format.Output.New_Line;
               Format.Output.Put ("  ");
            end if;
         else
            Has_Description := True;
            if I = Description'First then
               Format.Output.Put ("  ");
            end if;
            Format.Output.Put (Description (I));
         end if;
      end loop;
      if Has_Description then
         Format.Output.New_Line;
      end if;
   end Put_Feature;

   procedure Put_Background (Format     : in out Text_Format_Type;
                             Background : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type)
   is
      pragma Unreferenced (Position);
   begin
      Format.Output.New_Line;
      for I in Tags'Range loop
         Format.Output.Put_Line ("  " & To_String (Tags (I)));
      end loop;
      Format.Output.Put ("  Background:");
      if Background /= "" then
         Format.Output.Put (" " & Background);
      end if;
      Format.Output.New_Line;
      Format.Has_Previous_Step := False;
   end Put_Background;

   procedure Put_Outline    (Format     : in out Text_Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type)
   is
      pragma Unreferenced (Position);
   begin
      Format.Output.New_Line;
      for I in Tags'Range loop
         Format.Output.Put_Line ("  " & To_String (Tags (I)));
      end loop;
      Format.Output.Put ("  Scenario Outline:");
      if Scenario /= "" then
         Format.Output.Put (" " & Scenario);
      end if;
      Format.Output.New_Line;
      Format.Has_Previous_Step := False;
   end Put_Outline;

   procedure Put_Outline_Report
                            (Format     : in out Text_Format_Type;
                             Table      : in     Table_Type)
   is
   begin
      Format.Output.New_Line;
      Format.Output.Put_Line ("    Examples:");
      Put_Table (Format, Table, "      ");
   end Put_Outline_Report;

   procedure Put_Scenario (Format   : in out Text_Format_Type;
                           Scenario : in     String;
                           Position : in     String;
                           Tags     : in     Tag_Array_Type)
   is
      pragma Unreferenced (Position);
   begin
      if not Format.In_Outline then
         Format.Output.New_Line;
         for I in Tags'Range loop
            Format.Output.Put_Line ("  " & To_String (Tags (I)));
         end loop;
         Format.Output.Put ("  Scenario:");
         if Scenario /= "" then
            Format.Output.Put (" " & Scenario);
         end if;
         Format.Output.New_Line;
      end if;
      Format.Has_Previous_Step := False;
   end Put_Scenario;

   ----------------
   --  Put_Step  --
   ----------------

   procedure Put_Step       (Format     : in out Text_Format_Type;
                             Step       : in     Step_Type;
                             Name       : in     String;
                             Position   : in     String;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type)
   is
      pragma Unreferenced (Position);
      procedure Put_Long_String (Text : in String);

      procedure Put_Long_String (Text : in String) is
         J    : Natural := Text'First;
      begin
         Format.Output.Put_Line ("      """"""");
         Format.Output.Put      ("      ");
         while J <= Text'Last loop
            Format.Output.Put (Text (J));
            if Text (J) = ASCII.LF then
               Format.Output.Put ("      ");
            end if;
            J := J + 1;
         end loop;
         Format.Output.New_Line;
         Format.Output.Put_Line ("      """"""");
      end Put_Long_String;

      Indent : Integer := 2;

   begin
      if not (Format.In_Outline  and
              Format.In_Scenario and
              Success = Status_Passed)
      then
         if Format.In_Outline and Format.In_Scenario then
            Indent := Indent + 1;
         end if;
         Format.Output.Put (Indent * "  ");
         if Format.Has_Previous_Step and Format.Previous_Step_Type = Step then
            Format.Output.Put ("And ");
         else
            case Step is
               when Step_Given => Format.Output.Put ("Given ");
               when Step_When  => Format.Output.Put ("When ");
               when Step_Then  => Format.Output.Put ("Then ");
            end case;
         end if;
         Format.Output.Put (Name);
         Format.Output.New_Line;
         Indent := Indent + 1;
         Loop_Args :
         for I in Args.First .. Args.Last loop
            case Args.Elem_Type (I) is
               when Arg_Text =>
                  Put_Long_String (Args.Text (Args.Elem_Idx (I)));
               when Arg_Table =>
                  Put_Table (Format,
                             Args.Table (Args.Elem_Idx (I)),
                             Indent * "  ");
               when others => null;
            end case;
         end loop Loop_Args;
         Format.Has_Previous_Step  := True;
         Format.Previous_Step_Type := Step;
      end if;
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
                             Report     : in Report_Type;
                             D          : in     Duration)
   is
      Count_Scenarios : constant Natural := Report.Count_Scenario_Failed +
                                            Report.Count_Scenario_Passed;
      Count_Steps     : constant Natural := Report.Count_Steps_Failed +
                                            Report.Count_Steps_Skipped +
                                            Report.Count_Steps_Passed;
      Need_Comma : Boolean;
   begin
      if not Format.First_Feature then
         Format.Output.New_Line;
      end if;
      if Count_Scenarios > 1 then
         Format.Output.Put
            (Trim (Count_Scenarios'Img, Left) & " scenarios (");
      elsif Count_Scenarios = 1 then
         Format.Output.Put
            (Trim (Count_Scenarios'Img, Left) & " scenario (");
      else
         Format.Output.Put
            (Trim (Count_Scenarios'Img, Left) & " scenarios");
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
      if Count_Scenarios > 0 then
         Format.Output.Put (")");
      end if;
      Format.Output.New_Line;
      if Count_Steps > 1 then
         Format.Output.Put
            (Trim (Count_Steps'Img, Left) & " steps (");
      elsif Count_Steps = 1 then
         Format.Output.Put
            (Trim (Count_Steps'Img, Left) & " step (");
      else
         Format.Output.Put
            (Trim (Count_Steps'Img, Left) & " steps");
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
      if Count_Steps > 0 then
         Format.Output.Put (")");
      end if;
      Format.Output.New_Line;
      Format.Output.Put_Line ("Finished in " & Get_Duration (D));
   end Put_Summary;

   -----------------------
   --  New_Text_Format  --
   -----------------------

   function New_Text_Format return Text_Format_Ptr is
   begin
      return new Text_Format_Type;
   end New_Text_Format;

end AdaSpecLib.Format.Text;
