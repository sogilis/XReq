--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package body AdaSpec.Steps.Ada is

   procedure Make (S         : in out Ada_Step_Type;
                   File_Name : in String) is
   begin
      S.File_Name := To_Unbounded_String (File_Name);
      S.Parsed    := False;
   end Make;

   function  Parsed (S : in Ada_Step_Type) return Boolean is
   begin
      return S.Parsed;
   end Parsed;

   procedure Parse (S : in out Ada_Step_Type) is
   begin
      S.Parsed := True;
   end Parse;


   function  Contains  (S      : in Ada_Step_Type;
                        Prefix : in Prefix_Type;
                        Phrase : in String) return Boolean
   is
      pragma Unreferenced (Prefix);
      pragma Unreferenced (Phrase);
   begin
      if not S.Parsed then
         raise Unparsed_Step;
      end if;
      return False;
   end Contains;

end AdaSpec.Steps.Ada;
