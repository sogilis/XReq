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

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with XReqLib.Format.Text;
with XReqLib.Format.HTML;

package body XReqLib.Format is

   ------------------------
   --  Start/Stop_Tests  --
   ------------------------

   procedure Start_Tests    (Format      : in out Format_Type) is
   begin
      Format.In_Tests := True;
   end Start_Tests;

   procedure Stop_Tests     (Format      : in out Format_Type) is
   begin
      Format.In_Tests := False;
   end Stop_Tests;

   --------------------------
   --  Start/Stop_Feature  --
   --------------------------

   procedure Start_Feature  (Format      : in out Format_Type) is
   begin
      Format.Feature_ID    := Format.Feature_ID + 1;
      Format.Background_ID := 0;
      Format.Scenario_ID   := 0;
      Format.In_Feature    := True;
   end Start_Feature;

   procedure Stop_Feature   (Format      : in out Format_Type) is
   begin
      Format.In_Feature    := False;
   end Stop_Feature;

   --------------------------
   --  Start/Stop_Outline  --
   --------------------------

   procedure Enter_Outline  (Format     : in out Format_Type) is
   begin
      Format.Scenario_ID := Format.Scenario_ID + 1;
   end Enter_Outline;

   procedure Start_Outline  (Format     : in out Format_Type) is
   begin
      Format.In_Outline := True;
   end Start_Outline;

   procedure Stop_Outline   (Format     : in out Format_Type) is
   begin
      Format.In_Outline := False;
   end Stop_Outline;

   ---------------------------
   --  Start/Stop_Scenario  --
   ---------------------------

   procedure Enter_Scenario (Format     : in out Format_Type) is
   begin
      if not Format.In_Outline then
         Format.Scenario_ID := Format.Scenario_ID + 1;
      end if;
   end Enter_Scenario;

   procedure Start_Scenario (Format     : in out Format_Type) is
   begin
      if not Format.In_Outline then
         Format.Step_ID  := 0;
      end if;
      Format.In_Scenario := True;
   end Start_Scenario;

   procedure Stop_Scenario  (Format     : in out Format_Type) is
   begin
      Format.In_Scenario := False;
   end Stop_Scenario;

   -----------------------------
   --  Start/Stop_Background  --
   -----------------------------

   procedure Start_Background (Format     : in out Format_Type;
                               First      : in Boolean) is
      pragma Unreferenced (First);
   begin
      Format.Background_ID := Format.Background_ID + 1;
      Format.In_Background := True;
   end Start_Background;

   procedure Stop_Background  (Format     : in out Format_Type;
                               First      : in Boolean) is
      pragma Unreferenced (First);
   begin
      Format.In_Background := False;
   end Stop_Background;

   -----------------------
   --  Start/Stop_Step  --
   -----------------------

   procedure Start_Step     (Format     : in out Format_Type) is
   begin
      Format.Step_ID := Format.Step_ID + 1;
      Format.In_Step := True;
      if not Format.In_Outline or Format.In_Scenario then
         Format.Exec_Steps := Format.Exec_Steps + 1;
      end if;
   end Start_Step;

   procedure Stop_Step      (Format     : in out Format_Type) is
   begin
      Format.In_Step := False;
   end Stop_Step;

   ---------------------
   --  Get_Formatter  --
   ---------------------

   function Get_Formatter (Name : in String) return Format_Ptr is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;
      N : constant String := Translate (Name, Lower_Case_Map);
   begin
      if N = "text" then
         return Format_Ptr (XReqLib.Format.Text.New_Text_Format);
      elsif N = "html" then
         return Format_Ptr (XReqLib.Format.HTML.New_HTML_Format);
      else
         return null;
      end if;
   end Get_Formatter;  --  GCOV_IGNORE

   ------------------
   --  Set_Output  --
   ------------------

   procedure Set_Output     (Format     : in out Format_Type;
                             Output     : in     String)
   is
   begin
      Format.Output.Create (Ada.Text_IO.Out_File, Output);
   end Set_Output;

   -----------------
   --  Set_Debug  --
   -----------------

   procedure Set_Debug      (Format     : in out Format_Type;
                             Debug_Mode : in     Boolean)
   is
   begin
      Format.Debug_Mode := Debug_Mode;
   end Set_Debug;

   ---------------------
   --  Set_Num_Steps  --
   ---------------------

   procedure Set_Num_Steps  (Format     : in out Format_Type;
                             Num_Steps  : in     Natural)
   is
   begin
      Format.Num_Steps := Num_Steps;
   end Set_Num_Steps;

   --------------------
   --  List_Feature  --
   --------------------

   procedure List_Feature   (Format     : in out Format_Type;
                             Name       : in     String) is
   begin
      if Format.First_Feature then
         Format.First_Feature := False;
      else
         Format.Output.New_Line;
      end if;
      Format.Output.Put_Line ("Feature: " & Name);
      Format.Output.New_Line;
   end List_Feature;

   ---------------------
   --  List_Scenario  --
   ---------------------

   procedure List_Scenario  (Format     : in out Format_Type;
                             Name       : in     String;
                             Filename   : in     String;
                             Line       : in     Positive;
                             Num        : in     Positive)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      Format.Output.Put_Line ("  " & Filename & "#" & Trim (Num'Img, Left) &
                              ":" & Trim (Line'Img, Left) & " " & Name);
   end List_Scenario;

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
            Ada.Unchecked_Deallocation (Ada.Text_IO.File_Type,
                                        Local_File_Ptr);
      begin
         if File.Output_Ptr /= null then
            File.Close;
            Free (File.Output_Ptr);
         end if;
      end Finalize;

   end New_Text_IO;

   ------------------------------------
   --  Conditional_Type  --  Create  --
   ------------------------------------

   function  Create (Expr : in String) return Conditional_Type is
   begin
      return Conditional_Type'(Expr   => To_Unbounded_String (Expr),
                               others => <>);
   end Create;

   ----------------------------------
   --  Conditional_Type  --  Eval  --
   ----------------------------------

   function  Eval   (Tag_Expr : in Conditional_Type;
                     Tags     : in Tag_Array_Type) return Boolean
   is

      function Eval_Simple (Expr : in String) return Boolean;
      function Eval_Not    (Expr : in String) return Boolean;
      function Eval_And    (Expr : in String) return Boolean;
      function Eval_Or     (Expr : in String) return Boolean;

      function Eval_Simple (Expr : in String) return Boolean is
      begin
         if Expr'Length = 0 then
            return True;
         else
            for I in Tags'Range loop
               if Expr = To_String (Tags (I)) then
                  return True;
               end if;
            end loop;
            return False;
         end if;
      end Eval_Simple;

      function Eval_Not    (Expr : in String) return Boolean is
      begin
         if Expr'Length >= 1 and then Expr (Expr'First) = '~' then
            return not Eval_Simple (Expr (Expr'First + 1 .. Expr'Last));
         else
            return Eval_Simple (Expr);
         end if;
      end Eval_Not;

      function Eval_And    (Expr : in String) return Boolean is
         Buffer : Unbounded_String;
      begin
         for I in Expr'Range loop
            case Expr (I) is
               when '+' =>
                  if not Eval_Not (To_String (Buffer)) then
                     return False;
                  end if;
                  Buffer := Null_Unbounded_String;
               when others =>
                  Append (Buffer, Expr (I));
            end case;
         end loop;
         return Eval_Not (To_String (Buffer));
      end Eval_And;  --  GCOV_IGNORE

      function Eval_Or     (Expr : in String) return Boolean is
         Buffer : Unbounded_String;
      begin
         for I in Expr'Range loop
            case Expr (I) is
               when ',' =>
                  if Eval_And (To_String (Buffer)) then
                     return True;
                  end if;
                  Buffer := Null_Unbounded_String;
               when others =>
                  Append (Buffer, Expr (I));
            end case;
         end loop;
         return Eval_And (To_String (Buffer));
      end Eval_Or;  --  GCOV_IGNORE

   begin
      return Eval_Or (To_String (Tag_Expr.Expr));
   end Eval;

   ----------------------------------
   --  Conditional_Type  --  Eval  --
   ----------------------------------

   function  Eval   (Cond     : in Conditional_Type;
                     File     : in String;
                     Line     : in Integer;
                     Num      : in Integer) return Boolean
   is
      use String_Vectors;
      use Ada.Strings.Fixed;
      use Ada.Strings;
      procedure Decompose (C : String; Line, Pos, First, Last : out Integer);
      function Basename (S : String) return String;
      I   : String_Vectors.Cursor;

      procedure Decompose (C : String; Line, Pos, First, Last : out Integer) is
      begin
         First := C'First;
         Last := C'Last;
         Line := -1;
         Pos  := -1;
         for I in reverse C'Range loop
            case C (I) is
               when ':' =>
                  begin
                     Line := Integer'Value (C (I + 1 .. Last));
                  exception
                     when Constraint_Error =>
                        raise Constraint_Error with C & Integer'Image (I + 1) &
                           Last'Img;
                  end;
                  Last := I - 1;
               when '#' =>
                  begin
                     Pos  := Integer'Value (C (I + 1 .. Last));
                  exception
                     when Constraint_Error =>
                        raise Constraint_Error with C & Integer'Image (I + 1) &
                           Last'Img;
                  end;
                  Last := I - 1;
               when others =>
                  null;
            end case;
         end loop;
      end Decompose;

      function Basename (S : String) return String is
         First : Integer := S'First;
      begin
         for I in reverse S'Range loop
            if S (I) = '/' then
               First := I + 1;
               exit;
            end if;
         end loop;
         return S (First .. S'Last);
      end Basename;
   begin
      if Integer (Length (Cond.Scenarios)) = 0 then
         return True;
      end if;
      I := First (Cond.Scenarios);
      while Has_Element (I) loop
         declare
            C : constant String := To_String (Element (I));
            Request_Line, Request_Pos, First, Last : Integer;
            Has_Line, Has_Pos : Boolean;
            Is_Line, Is_Pos : Boolean;
            Is_File : Boolean;
         begin
            Decompose (C, Request_Line, Request_Pos, First, Last);
            Has_Line := Request_Line > 0;
            Has_Pos  := Request_Pos > 0;
            Is_Line  := not Has_Line or Line = Request_Line;
            Is_Pos   := not Has_Pos  or Num  = Request_Pos;
            Is_File  := C (First .. Last) = File or else
                        C (First .. Last) = Basename (File);
            if Is_File and Is_Line and Is_Pos then
               return True;
            end if;
         end;
         Next (I);
      end loop;
      return False;
   end Eval;  --  GCOV_IGNORE

   --------------------
   --  Get_Duration  --
   --------------------

   function Get_Duration (D : in Duration) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Min : Natural;
      Sec : Natural;
      Buf : Unbounded_String;
   begin
      Min := Integer (D) / 60;
      Sec := Integer (D) mod 60;
      if Min > 0 then
         Append (Buf, Trim (Min'Img, Left) & "m ");
      end if;
      Append (Buf, Trim (Sec'Img, Left) & "s");
      --  Append (Buf, " (" & Trim (D'Img, Left) & "s)");
      return To_String (Buf);
   end Get_Duration;

   ----------------------------------------------------------------------------

   pragma Style_Checks (Off);

   procedure S_Feature  (F : in out Format_Type; S : in String) is begin F.Str_Feature  := To_Unbounded_String (S); end S_Feature;
   procedure S_Scenario (F : in out Format_Type; S : in String) is begin F.Str_Scenario := To_Unbounded_String (S); end S_Scenario;
   procedure S_Outline  (F : in out Format_Type; S : in String) is begin F.Str_Outline  := To_Unbounded_String (S); end S_Outline;

   function  S_Feature  (F : in Format_Type) return String is begin return To_String (F.Str_Feature);  end S_Feature;
   function  S_Scenario (F : in Format_Type) return String is begin return To_String (F.Str_Scenario); end S_Scenario;
   function  S_Outline  (F : in Format_Type) return String is begin return To_String (F.Str_Outline);  end S_Outline;

   pragma Style_Checks (On);

end XReqLib.Format;
