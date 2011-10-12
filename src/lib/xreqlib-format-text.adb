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

with Ada.Command_Line;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with XReqLib.ANSI;
with XReqLib.Error_Handling;

use Ada.Strings;
use Ada.Strings.Fixed;
use Ada.Strings.Unbounded;

package body XReqLib.Format.Text is

   procedure Put_Tags    (Format     : in out Text_Format_Type;
                          Tags       : in     Tag_Array_Type);

   procedure Put_Scenario (Format   : in out Text_Format_Type);

   procedure Put_Table      (Format     : in out Text_Format_Type;
                             T          : in     Table_Type;
                             Indent     : in     String);

   function ANSI (Sequence : in String) return String
      renames XReqLib.ANSI.ANSI;
   function ANSI_G return String;
   function ANSI_R return String;
   function ANSI_C return String;
   function ANSI_X return String;

   function Color (Success : in Status_Type) return String;

   -------------
   --  Color  --
   -------------

   function Color (Success : in Status_Type) return String is
   begin
      case Success is
         when Status_Passed =>
            return ANSI_G;
         when Status_Skipped =>
            return ANSI_C;
         when Status_Failed =>
            return ANSI_R;
         when others =>
            return ANSI_X;
      end case;
   end Color;

   ------------
   --  ANSI  --
   ------------

   function ANSI_G return String is
   begin
      return ANSI ("32m");
   end ANSI_G;

   function ANSI_R return String is
   begin
      return ANSI ("31m");
   end ANSI_R;

   function ANSI_C return String is
   begin
      return ANSI ("36m");
   end ANSI_C;

   function ANSI_X return String is
   begin
      return ANSI ("m");
   end ANSI_X;

   -----------------
   --  Put_Table  --
   -----------------

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
               Format.Output.Put (" ");
            else
               Format.Output.Put ((Width (X) + 2) * "-");
            end if;
            Format.Output.Put ("|");
         end loop;
         Format.Output.New_Line;
      end loop;
   end Put_Table;

   -------------------
   --  Put_Feature  --
   -------------------

   procedure Put_Feature (Format      : in out Text_Format_Type) is
      Has_Description : Boolean := False;
      Description     : constant String
        := To_String (Format.Feature.Description);
   begin
      Format.Output.Buffer_Start;
      if Format.Feature_ID > 1 then
         Format.Output.New_Line;
      end if;
      Format.Output.Put_Line
        (To_String (Format.S_Feature & " " & Format.Feature.Name));
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

   -------------------
   --  End_Feature  --
   -------------------

   procedure End_Feature (Format      : in out Text_Format_Type) is
   begin
      Format.Output.Buffer_Discard;
   end End_Feature;

   ----------------
   --  Put_Tags  --
   ----------------

   procedure Put_Tags    (Format     : in out Text_Format_Type;
                          Tags       : in     Tag_Array_Type) is
   begin
      for I in Tags'Range loop
         Format.Output.Put_Line
            ("  " & ANSI_C & To_String (Tags (I)) & ANSI_X);
      end loop;
   end Put_Tags;

   ---------------------
   --  Enter_Outline  --
   ---------------------

   procedure Enter_Outline    (Format     : in out Text_Format_Type) is
   begin
      Format.Output.New_Line;
      Put_Tags (Format, Convert (Format.Outline.Tags));
      Format.Output.Put ("  " & Format.S_Outline);
      if Format.Outline.Name /= "" then
         Format.Output.Put (" " & To_String (Format.Outline.Name));
      end if;
      Format.Output.New_Line;
   end Enter_Outline;

   ---------------------
   --  Begin_Outline  --
   ---------------------

   procedure Begin_Outline    (Format     : in out Text_Format_Type) is
   begin
      null;
   end Begin_Outline;

   ----------------------
   --  Enter_Scenario  --
   ----------------------

   procedure Enter_Scenario (Format   : in out Text_Format_Type) is
   begin
      Format.Background_Failed := False;
      Format.Output.Buffer_Commit;
      if Format.Scenario_ID > 1 then
         Put_Scenario (Format);
      end if;
   end Enter_Scenario;

   ----------------------
   --  Put_Background  --
   ----------------------

   procedure Put_Background (Format     : in out Text_Format_Type) is
   begin
      if Format.Scenario_ID = 1 then
         Format.Output.New_Line;
         Format.Output.Put ("  Background:");
         if Format.Background.Name /= "" then
            Format.Output.Put (" " & To_String (Format.Background.Name));
         end if;
         Format.Output.New_Line;
      end if;
   end Put_Background;

   ----------------------
   --  Begin_Scenario  --
   ----------------------

   procedure Begin_Scenario (Format   : in out Text_Format_Type) is
   begin
      if Format.Scenario_ID = 1 then
         Format.Previous_Step_Type := Step_Null;
         Put_Scenario (Format);
      end if;
   end Begin_Scenario;

   --------------------
   --  Put_Scenario  --
   --------------------

   procedure Put_Scenario (Format   : in out Text_Format_Type) is
      Indent : Integer := 1;
      Scen : constant String := Format.S_Scenario;
   begin
      if Format.In_Outline then
         Indent := Indent + 1;
      end if;
      Format.Output.New_Line;
      if Format.In_Outline then
         Format.Output.Put ((Indent * "  ")
                            & Scen (Scen'First .. Scen'Last - 1)
                            & Format.Scenario_ID'Img & ":");
      else
         Put_Tags (Format, Convert (Format.Scenario.Tags));
         Format.Output.Put ((Indent * "  ") & Scen);
      end if;
      if Format.Scenario.Name /= "" then
         Format.Output.Put (" " & To_String (Format.Scenario.Name));
      end if;
      Format.Output.New_Line;
   end Put_Scenario;

   ----------------
   --  Put_Step  --
   ----------------

   procedure Put_Step       (Format     : in out Text_Format_Type;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type)
   is
      procedure Put_Long_String (Indent, Text : in String);

      procedure Put_Long_String (Indent, Text : in String) is
         J    : Natural := Text'First;
      begin
         Format.Output.Put_Line (Indent & """""""");
         Format.Output.Put      (Indent);
         while J <= Text'Last loop
            Format.Output.Put (Text (J));
            if Text (J) = ASCII.LF then
               Format.Output.Put (Indent);
            end if;
            J := J + 1;
         end loop;
         Format.Output.New_Line;
         Format.Output.Put_Line (Indent & """""""");
      end Put_Long_String;

      Name    : constant String := To_String (Format.Step.Name);
      Indent  : Integer := 2;
      Inserts : array (1 .. Name'Last + 1) of Unbounded_String;
      Left    : Integer;
      Right   : Integer;

   begin
      --  Update status
      if Success = Status_Failed and Format.In_Background then
         Format.Background_Failed := True;
      end if;

      --  Append scenario to the list of failed scenarios
      if Success = Status_Failed
        or (Success = Status_Skipped and Format.Step_ID = 1)
      then
         Append (Format.Failed_Step_List, Ada.Command_Line.Command_Name
                 & " " & Format.Scenario.Position & ASCII.LF);
      end if;

      if Format.Debug_Mode           --  Display every step in debug mode
        or not Format.In_Background  --  Display steps not in background
        or Format.Scenario_ID = 1    --  Display steps in first background
        or Success = Status_Failed   --  Display failed steps
        or Format.Background_Failed  --  Display steps after a failed step in a
      --  scenario. This is not equivalent to skipped tests because some
      --  scenarios are entirely skipped, and we don't want to display all
      --  the skipped background steps.
      then
         if Format.In_Outline and Format.In_Scenario then
            Indent := Indent + 1;
         end if;

         --  Put Step
         --------------

         Format.Output.Put (Indent * "  ");
         Format.Output.Put (Color (Success));
         if Format.Step_ID > 1
           and then Format.Previous_Step_Type = Format.Step.Kind
         then
            Format.Output.Put ("And ");
         else
            case Format.Step.Kind is
               when Step_Given => Format.Output.Put ("Given ");
               when Step_When  => Format.Output.Put ("When ");
               when Step_Then  => Format.Output.Put ("Then ");
            end case;
         end if;
         for I in 1 .. Args.Last_Match loop
            Args.Match (I, Left, Right);
            Append (Inserts (Left), ANSI ("1m"));
            Insert (Inserts (Right + 1), 1, ANSI ("22m"));
         end loop;
         for I in Name'Range loop
            Format.Output.Put (To_String (Inserts (I)));
            Format.Output.Put ("" & Name (I));
         end loop;
         Format.Output.Put (To_String (Inserts (Inserts'Last)));
         Format.Output.Put (ANSI_X);
         if Format.In_Background and Format.Scenario_ID > 1 then
            Format.Output.Put (" (background)");
         end if;
         Format.Output.New_Line;
         Indent := Indent + 1;

         --  Put Args
         --------------

         Loop_Args :
         for I in Args.First .. Args.Last loop
            case Args.Elem_Type (I) is
               when Arg_Text =>
                  Format.Output.Put (Color (Success));
                  Put_Long_String (Indent * "  ",
                                   Args.Text (Args.Elem_Idx (I)));
                  Format.Output.Put (ANSI_X);
               when Arg_Table =>
                  Put_Table (Format,
                             Args.Table (Args.Elem_Idx (I)),
                             Indent * "  ");
               when Arg_Separator =>
                  if Success /= Status_Failed and not Format.Debug_Mode then
                     exit Loop_Args;
                  end if;
                  if I < Args.Last then
                     Format.Output.Put (Indent * "  ");
                     Format.Output.Put (Color (Success));
                     Format.Output.Put ((79 - 2 * Indent) * "-");
                     Format.Output.Put (ANSI_X);
                     Format.Output.New_Line;
                  end if;
               when Arg_Paragraph =>
                  Format.Output.Put (Indent * "  ");
                  Format.Output.Put (Color (Success));
                  Format.Output.Put (String'(Args.Para (Args.Elem_Idx (I))));
                  Format.Output.Put (ANSI_X);
                  Format.Output.New_Line;
            end case;
         end loop Loop_Args;

         Format.Previous_Step_Type := Format.Step.Kind;
      end if;
   end Put_Step;

   -----------------
   --  Put_Error  --
   -----------------

   procedure Put_Error      (Format     : in out Text_Format_Type;
                             Err        : in Exception_Occurrence)
   is
      use XReqLib.Error_Handling;
      Error  : constant String := Exception_To_String (Err);
      Indent : Integer := 3;
   begin
      if Format.In_Outline then
         Indent := Indent + 1;
      end if;
      Format.Output.Put (Indent * "  ");
      for I in Error'Range loop
         Format.Output.Put (Error (I));
         if Error (I) = ASCII.LF and I /= Error'Last then
            Format.Output.Put (Indent * "  ");
         end if;
      end loop;
   end Put_Error;

   --------------------------
   --  Put_Outline_Report  --
   --------------------------

   procedure Put_Outline_Report
                            (Format     : in out Text_Format_Type;
                             Table      : in     Table_Type)
   is
   begin
      Format.Output.New_Line;
      Format.Output.Put_Line ("    Examples:");
      Put_Table (Format, Table, "      ");
   end Put_Outline_Report;

   -------------------
   --  Put_Summary  --
   -------------------

   procedure Put_Summary    (Format     : in out Text_Format_Type;
                             Report     : in Report_Type;
                             D          : in     Duration)
   is
      Count_Scenarios : constant Natural := Report.Count_Scenario_Failed +
                                            Report.Count_Scenario_Passed;
      Count_Steps     : constant Natural := Report.Num_Steps;
      Need_Comma : Boolean;
   begin
      if Count_Scenarios > 0 then
         Format.Output.New_Line;
      end if;
      if Format.Failed_Step_List /= Null_Unbounded_String then
         Format.Output.Put (To_String (Format.Failed_Step_List));
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
            (ANSI_R &
             Trim (Report.Count_Scenario_Failed'Img, Left) & " failed" &
             ANSI_X);
         Need_Comma := True;
      end if;
      if Report.Count_Scenario_Passed /= 0 then
         if Need_Comma then Format.Output.Put (", "); end if;
         Format.Output.Put
            (ANSI_G &
             Trim (Report.Count_Scenario_Passed'Img, Left) & " passed" &
             ANSI_X);
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
            (ANSI_R &
             Trim (Report.Count_Steps_Failed'Img, Left) & " failed" &
             ANSI_X);
         Need_Comma := True;
      end if;
      if Report.Count_Steps_Skipped /= 0 then
         if Need_Comma then Format.Output.Put (", "); end if;
         Format.Output.Put
            (ANSI_C &
             Trim (Report.Count_Steps_Skipped'Img, Left) & " skipped" &
             ANSI_X);
         Need_Comma := True;
      end if;
      if Report.Count_Steps_Passed /= 0 then
         if Need_Comma then Format.Output.Put (", "); end if;
         Format.Output.Put
            (ANSI_G &
             Trim (Report.Count_Steps_Passed'Img, Left) & " passed" &
             ANSI_X);
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

end XReqLib.Format.Text;
