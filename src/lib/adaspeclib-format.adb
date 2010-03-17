--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants;
with AdaSpecLib.Format.Text;
with AdaSpecLib.Format.HTML;

use Ada.Strings.Unbounded;

package body AdaSpecLib.Format is

   ---------------------
   --  Get_Formatter  --
   ---------------------

   function Get_Formatter (Name : in String) return Format_Ptr is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;
      N : constant String := Translate (Name, Lower_Case_Map);
   begin
      if N = "text" then
         return Format_Ptr (AdaSpecLib.Format.Text.New_Text_Format);
      elsif N = "html" then
         return Format_Ptr (AdaSpecLib.Format.HTML.New_HTML_Format);
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
                             Num        : in     Positive)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      Format.Output.Put_Line ("  " & Filename & ":" & Trim (Num'Img, Left) &
                                     " " & Name);
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
            Ada.Unchecked_Deallocation (Ada.Text_IO.File_Type, Local_File_Ptr);
      begin
         File.Close;
         Free (File.Output_Ptr);
      end Finalize;

   end New_Text_IO;

   ---------------------------------
   --  Tag_Expr_Type  --  Create  --
   ---------------------------------

   function  Create (Expr : in String) return Tag_Expr_Type is
   begin
      return Tag_Expr_Type'(Expr => To_Unbounded_String (Expr));
   end Create;

   -------------------------------
   --  Tag_Expr_Type  --  Eval  --
   -------------------------------

   function  Eval   (Tag_Expr : in Tag_Expr_Type;
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
      end Eval_And;

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
      end Eval_Or;

   begin
      return Eval_Or (To_String (Tag_Expr.Expr));
   end Eval;

end AdaSpecLib.Format;
