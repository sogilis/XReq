with Ada.Text_IO;
with Ada.Exceptions;
with Gnat.Traceback.Symbolic;
with Language_Custom;

use Language_Custom;

package body GPR_Custom is

   procedure Tracebackinfo(E : Ada.Exceptions.Exception_Occurrence) is
      use Ada.Text_IO;
      use Ada.Exceptions;
      Last_Exception_Name     : constant String := Exception_Name(E);
      Last_Exception_Messsage : constant String:= Exception_Message(E);
      Last_Exception_Info     : constant String:= Exception_Information(E);
   begin
      Put_Line("Exception raised : " & Last_Exception_Name);
      New_Line;
      Put_Line("Message : " & Last_Exception_Messsage);
      Put_Line(Last_Exception_Info);
      Put_Line("...................................................");
      New_Line;
      Put_Line("Hex Subprogram name and file");
      Put_Line("----- ------------------------");
      Put_Line(Gnat.Traceback.Symbolic.Symbolic_Traceback(E));
      Put_Line("-------------------------------------------------");
   end Tracebackinfo;

   ------------------
   -- Comment_Line --
   ------------------

   function Comment_Line
     (Line : String; Comment : Boolean; Reserved : Integer) return chars_ptr
   is
   begin
      if Comment then
         declare
            S : String (1 .. Line'Length + 2);
         begin
            S (1 .. 2) := "# ";
            S (3 .. Line'Length) := Line;
            return New_String (S);
         end;
      else
         if Line'Length > 2 and then
            Line (Line'First .. Line'First + 1) = "# "
         then
            return New_String (Line (Line'First + 2 .. Line'Last));
         elsif
            Line'Length > 1 and then
            Line (Line'First) = '#'
         then
            return New_String (Line (Line'First + 1 .. Line'Last));
         else
            return New_String (Line);
         end if;
      end if;
   exception
      when E : others =>
         Tracebackinfo (E);
         return Null_Ptr;
   end Comment_Line;

end GPR_Custom;
