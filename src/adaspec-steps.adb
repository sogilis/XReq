--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpec.Steps is

   function  File_Name (S : in Step_File_Type) return String is
   begin
      return To_String (S.File_Name);
   end File_Name;

end AdaSpec.Steps;
