--                         Copyright (C) 2010, Sogilis                       --

with AdaSpec.Steps.Ada;

package body AdaSpec.Steps is

   function  File_Name (S : in Step_File_Type) return String is
   begin
      return To_String (S.File_Name);
   end File_Name;

   procedure Load (Steps     : in out Steps_Type;
                   Directory : in     String) is
   begin
      AdaSpec.Steps.Ada.Parse_Directory (Steps, Directory);
   end Load;


   function  Contains  (Steps     : in Steps_Type;
                        Stanza    : in Stanza_Type) return Boolean
   is
   begin
      return Find (Steps, Stanza) /= "";
   end Contains;

   function  Find      (Steps     : in Steps_Type;
                        Stanza    : in Stanza_Type) return String
   is
      use Step_Vectors;
      I    : Step_Vectors.Cursor := First (Steps);
      Step : Step_File_Ptr;
   begin
      while Has_Element (I) loop
         Step := Element (I);
         declare
            Result : constant String := Find (Step.all, Stanza);
         begin
            if Result /= "" then
               return Result;
            end if;
         end;
         Next (I);
      end loop;
      return "";
   end Find;

end AdaSpec.Steps;
