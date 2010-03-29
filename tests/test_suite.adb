--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;

with Test_Suite.IO;
with Test_Suite.Strings;
with Test_Suite.Strings.Pool;

with Test_Suite.Main;
with Test_Suite.Lang;
with Test_Suite.CLI;
with Test_Suite.Job;
with Test_Suite.Result;
with Test_Suite.Steps;
with Test_Suite.Step_Definitions;
with Test_Suite.Step_Definitions.Ada05;
with Test_Suite.Features;
with Test_Suite.Generator;
with Test_Suite.Generator.Ada05;

with Test_Suite.Lib;
with Test_Suite.Lib.Util;
with Test_Suite.Lib.Args;
with Test_Suite.Lib.Report;
with Test_Suite.Lib.Format;
with Test_Suite.Lib.Format.Text;
with Test_Suite.Lib.Tables;
with Test_Suite.Lib.Asserts;

package body Test_Suite is

   -------------
   --  Suite  --
   -------------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Ret : constant AUnit.Test_Suites.Access_Test_Suite :=
            new AUnit.Test_Suites.Test_Suite;
   begin

      Test_Suite.IO             .Add_Tests (Ret);
      Test_Suite.Strings        .Add_Tests (Ret);
      Test_Suite.Strings.Pool   .Add_Tests (Ret);

      Test_Suite.Main           .Add_Tests (Ret);
      Test_Suite.Lang           .Add_Tests (Ret);
      Test_Suite.CLI            .Add_Tests (Ret);
      Test_Suite.Job            .Add_Tests (Ret);
      Test_Suite.Steps        .Add_Tests (Ret);
      Test_Suite.Result         .Add_Tests (Ret);
      Test_Suite.Step_Definitions          .Add_Tests (Ret);
      Test_Suite.Step_Definitions.Ada05    .Add_Tests (Ret);
      Test_Suite.Features       .Add_Tests (Ret);
      Test_Suite.Generator      .Add_Tests (Ret);
      Test_Suite.Generator.Ada05.Add_Tests (Ret);

      Test_Suite.Lib            .Add_Tests (Ret);
      Test_Suite.Lib.Util       .Add_Tests (Ret);
      Test_Suite.Lib.Args       .Add_Tests (Ret);
      Test_Suite.Lib.Report     .Add_Tests (Ret);
      Test_Suite.Lib.Format     .Add_Tests (Ret);
      Test_Suite.Lib.Format.Text.Add_Tests (Ret);
      Test_Suite.Lib.Tables     .Add_Tests (Ret);
      Test_Suite.Lib.Asserts    .Add_Tests (Ret);

      return Ret;

   end Suite;

   ---------------
   --  Banners  --
   ---------------

   procedure Title (Text : in String) is
      use Ada.Text_IO;
      Line1 : constant String (1 .. 80) := (others => '*');
      Line2 : constant String (1 .. 80) := (1 | 2 => '*',
                                            others => '-');
   begin
      New_Line;
      New_Line;
      Put_Line (Line1);
      if Text'Length > 80 then
         T_Put_Line (Text);
      else
         Put ("** ");
         for I in 1 .. (80 - Text'Length) / 2 - 3 loop
            Put (' ');
         end loop;
         Put_Line (Text);
      end if;
      Put_Line (Line2);
   end Title;

   procedure End_Test is
      Line : constant String (1 .. 80) := (others => '*');
      use Ada.Text_IO;
   begin
      T_New_Line;
      Put_Line (Line);
      New_Line;
   end End_Test;

   --------------
   --  Output  --
   --------------

   procedure T_Put (Item : in String) is
      use Ada.Text_IO;
   begin
      if Col (Current_Output) = 1 then
         Put (Current_Output, "** ");
      end if;
      for I in Item'Range loop
         Put (Current_Output, Item (I));
         if Item (I) = ASCII.LF then
            Put (Current_Output, "** ");
         end if;
      end loop;
   end T_Put;

   procedure T_Put_Line (Item : in String) is
      use Ada.Text_IO;
   begin
      T_Put (Item);
      T_New_Line;
   end T_Put_Line;

   procedure T_New_Line is
      use Ada.Text_IO;
   begin
      if Col (Current_Output) = 1 then
         Put (Current_Output, "** ");
      end if;
      New_Line (Current_Output);
   end T_New_Line;

   ------------------------
   --  Custom_Test_Case  --
   ------------------------

   function  Name (T : in Test_Case_Type) return AUnit.Message_String is
      Self : constant access constant Test_Case_Type'Class := T'Access;
   begin
      return AUnit.Format (String'(Name (Self.all)));
   end Name;

   procedure Run_Test (T : in out Test_Case_Type) is
      use Ada.Text_IO;
      Self : constant access Test_Case_Type'Class := T'Access;
      Pref : constant String  := "** Exception: ";
      Line : String (1 .. 80) := (others => '_');
   begin
      Line (Pref'Range) := Pref;
      Title (Self.Name);
      Self.Run;
      End_Test;
   exception
      when Error : others =>
         T_New_Line;
         Put_Line (Line);
         T_New_Line;
         T_Put (Exception_Information (Error));
         End_Test;
         Reraise_Occurrence (Error);
   end Run_Test;

   procedure Assert_Except (T       : in Test_Case_Generic_Type;
                            Message : in String;
                            Err     : in Exception_Id := Null_Id)
   is
   begin
      Proc;
      Assert (T, False, "Exception " & Exception_Name (Err) &
              " should have been raised: " & Message);
   exception
      when E : others =>
         if Err /= Null_Id and Exception_Identity (E) /= Err then
            Reraise_Occurrence (E);
         end if;
   end Assert_Except;

end Test_Suite;
