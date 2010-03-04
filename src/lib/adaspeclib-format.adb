--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with AdaSpecLib.Format.Text;
with AdaSpecLib.Format.HTML;

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

end AdaSpecLib.Format;