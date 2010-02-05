--                         Copyright (C) 2010, Sogilis                       --

package body Util.IO is

   --  Thanks to WikiBooks
   --  <http://en.wikibooks.org/wiki/Ada_Programming/Libraries/Ada.Text_IO>
   function Get_Whole_Line (File : File_Type) return Unbounded_String is
      Retval : Unbounded_String := Null_Unbounded_String;
      Item   : String (1 .. BufferSize);
      Last   : Natural;
   begin
      Get_Whole_Line : loop

         Get_Line (File, Item, Last);
         Append   (Retval, Item (1 .. Last));

         exit Get_Whole_Line when Last < Item'Last or End_Of_File (File);

      end loop Get_Whole_Line;
      return Retval;
   end Get_Whole_Line;

   function Get_Whole_Line (File : File_Type) return String is
   begin
      return To_String (Get_Whole_Line (File));
   end Get_Whole_Line;


end Util.IO;
