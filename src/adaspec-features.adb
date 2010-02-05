--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Util.IO;
with Util.Strings;

use Ada.Text_IO;
use Util.IO;
use Util.Strings;

package body AdaSpec.Features is

   procedure Make (F         : in out Feature_File_Type;
                   File_Name : in String) is
   begin
      F.File_Name := To_Unbounded_String (File_Name);
      F.Parsed    := False;
   end Make;

   function  Parsed (F : in Feature_File_Type) return Boolean is
   begin
      return F.Parsed;
   end Parsed;

   procedure Parse (F : in out Feature_File_Type) is
      type Mode_Type is (M_Begin, M_Feature, M_Background, M_Scenario);
      K_Feature    : constant String := "Feature:";
--       K_Background : constant String := "Background:";
--       K_Scenario   : constant String := "Scenario:";
--       K_Given      : constant String := "Given";
--       K_When       : constant String := "When";
--       K_Then       : constant String := "Then";
--       K_And        : constant String := "And";
      State     : Mode_Type := M_Begin;
      File      : File_Type;
      Line_S    : Unbounded_String;
      Idx_Start : Natural;
   begin
      Open (File, In_File, To_String (F.File_Name));
      while not End_Of_File (File) loop
         --
         --  Read Line
         --
         Line_S    := Get_Whole_Line (File);
         Idx_Start := Index_Non_Blank (Line_S);
         --
         --  State Machine
         --
         case State is
            when M_Begin =>
               if Starts_With (To_String (Line_S), K_Feature, Idx_Start) then
                  State := M_Feature;
                  Idx_Start := Index_Non_Blank (Line_S,
                                                Idx_Start + K_Feature'Length);
                  F.Name := Unbounded_Slice (Line_S,
                                             Idx_Start, Length (Line_S));
               end if;
               null;
            when M_Feature =>
               null;
            when M_Background | M_Scenario =>
               null;
         end case;
      end loop;
      Close (File);
      F.Parsed := True;
   end Parse;

   function  File_Name (F : in Feature_File_Type) return String is
   begin
      return To_String (F.File_Name);
   end File_Name;

end AdaSpec.Features;
