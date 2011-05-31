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
with Ada.Strings.Unbounded;
with XReqLib.ANSI;
with XReqLib.Error_Handling;

use Ada.Strings;
use Ada.Strings.Fixed;
use Ada.Strings.Unbounded;

package body XReqLib.Format.Text is

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
      Format.Output.Put_Line (Format.S_Feature & " " & Feature);
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

   ----------------------
   --  Put_Background  --
   ----------------------

   procedure Put_Background (Format     : in out Text_Format_Type;
                             Background : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type)
   is
      pragma Unreferenced (Position);
   begin
      Format.Output.New_Line;
      for I in Tags'Range loop
         Format.Output.Put_Line
            ("  " & ANSI_C & To_String (Tags (I)) & ANSI_X);
      end loop;
      Format.Output.Put ("  Background:");
      if Background /= "" then
         Format.Output.Put (" " & Background);
      end if;
      Format.Output.New_Line;
      Format.Has_Previous_Step := False;
   end Put_Background;

   -------------------
   --  Put_Outline  --
   -------------------

   procedure Put_Outline    (Format     : in out Text_Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type)
   is
      pragma Unreferenced (Position);
   begin
      Format.Output.New_Line;
      for I in Tags'Range loop
         Format.Output.Put_Line
            ("  " & ANSI_C & To_String (Tags (I)) & ANSI_X);
      end loop;
      Format.Output.Put ("  " & Format.S_Outline);
      if Scenario /= "" then
         Format.Output.Put (" " & Scenario);
      end if;
      Format.Output.New_Line;
      Format.Has_Previous_Step := False;
   end Put_Outline;

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

   --------------------
   --  Put_Scenario  --
   --------------------

   procedure Put_Scenario (Format   : in out Text_Format_Type;
                           Scenario : in     String;
                           Position : in     String;
                           Tags     : in     Tag_Array_Type)
   is
   begin
      Put_Scenario_Outline (Format, 0, Scenario, Position, Tags);
   end Put_Scenario;

   ----------------------------
   --  Put_Scenario_Outline  --
   ----------------------------

   procedure Put_Scenario_Outline
                            (Format     : in out Text_Format_Type;
                             Num        : in     Natural;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type)
   is
      pragma Unreferenced (Position);
      Indent : Integer := 2;
      Scen : constant String := Format.S_Scenario;
   begin
      if Format.In_Outline then
         Indent := Indent + 2;
      end if;
      Format.Output.New_Line;
      if not Format.In_Outline then
         for I in Tags'Range loop
            Format.Output.Put_Line
               ((Indent * " ") & ANSI_C & To_String (Tags (I)) & ANSI_X);
         end loop;
      end if;
      Format.Output.Put ((Indent * " ") & Scen (Scen'First .. Scen'Last - 1));
      if Num > 0 then
         Format.Output.Put (Num'Img);
      end if;
      Format.Output.Put (":");
      if Scenario /= "" then
         Format.Output.Put (" " & Scenario);
      end if;
      Format.Output.New_Line;
      Format.Has_Previous_Step := False;
   end Put_Scenario_Outline;

   ----------------
   --  Put_Step  --
   ----------------

   procedure Put_Step       (Format     : in out Text_Format_Type;
                             Step       : in     Step_Kind;
                             Name       : in     String;
                             Position   : in     String;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type)
   is
      pragma Unreferenced (Position);
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

      Indent  : Integer := 2;
      Inserts : array (1 .. Name'Last + 1) of Unbounded_String;
      Left    : Integer;
      Right   : Integer;

   begin
      if not (Format.In_Outline  and
              Format.In_Scenario and
              Success = Status_Passed)
      then
         if Format.In_Outline and Format.In_Scenario then
            Indent := Indent + 1;
         end if;
         Format.Output.Put (Indent * "  ");
         Format.Output.Put (Color (Success));
         if Format.Has_Previous_Step and then
            Format.Previous_Step_Type = Step
         then
            Format.Output.Put ("And ");
         else
            case Step is
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
         Format.Output.New_Line;
         Indent := Indent + 1;

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
      use XReqLib.Error_Handling;
      function Indent_String return String;
      Error : constant String := Exception_To_String (Err);

      function Indent_String return String is
      begin
         if Format.In_Outline then
            return "        ";
         else
            return "      ";
         end if;
      end Indent_String;
   begin
      Format.Output.Put (Indent_String);
      for I in Error'Range loop
         Format.Output.Put (Error (I));
         if Error (I) = ASCII.LF and I /= Error'Last then
            Format.Output.Put (Indent_String);
         end if;
      end loop;
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
      Count_Steps     : constant Natural := Report.Num_Steps;
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
