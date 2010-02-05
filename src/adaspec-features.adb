--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;

use Ada.Text_IO;

package body AdaSpec.Features is

   procedure Make (S         : in out Feature_File_Type;
                   File_Name : in String) is
   begin
      S.File_Name := To_Unbounded_String (File_Name);
      S.Parsed    := False;
   end Make;

   function  Parsed (S : in Feature_File_Type) return Boolean is
   begin
      return S.Parsed;
   end Parsed;

   procedure Parse (S : in out Feature_File_Type) is
      File     : File_Type;
   begin
      Open (File, In_File, To_String (S.File_Name));
      Close (File);
      S.Parsed := True;
   end Parse;

   function  File_Name (S : in Feature_File_Type) return String is
   begin
      return To_String (S.File_Name);
   end File_Name;

end AdaSpec.Features;
