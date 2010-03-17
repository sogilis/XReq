--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package AdaSpecLib.Format.HTML is

   type HTML_Format_Type is new Format_Type with private;
   type HTML_Format_Ptr  is access all HTML_Format_Type;

   overriding
   procedure Start_Tests    (Format     : in out HTML_Format_Type);
   overriding
   procedure Stop_Tests     (Format     : in out HTML_Format_Type);

   overriding
   procedure Start_Feature  (Format     : in out HTML_Format_Type);
   overriding
   procedure Stop_Feature   (Format     : in out HTML_Format_Type);

   overriding
   procedure Start_Background (Format   : in out HTML_Format_Type;
                               First    : in Boolean);
   overriding
   procedure Stop_Background  (Format   : in out HTML_Format_Type;
                               First    : in Boolean);

   overriding
   procedure Start_Step     (Format     : in out HTML_Format_Type);
   overriding
   procedure Stop_Step      (Format     : in out HTML_Format_Type);


   overriding
   procedure Enter_Scenario (Format     : in out HTML_Format_Type);
   overriding
   procedure Start_Scenario (Format     : in out HTML_Format_Type);
   overriding
   procedure Stop_Scenario  (Format     : in out HTML_Format_Type);

   overriding
   procedure Put_Feature    (Format     : in out HTML_Format_Type;
                             Feature    : in     String;
                             Description : in    String;
                             Position   : in     String);
   overriding
   procedure Put_Background (Format     : in out HTML_Format_Type;
                             Background : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type);
   overriding
   procedure Put_Scenario   (Format     : in out HTML_Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type);
   overriding
   procedure Put_Step       (Format     : in out HTML_Format_Type;
                             Step       : in     Step_Type;
                             Name       : in     String;
                             Position   : in     String;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type);
   overriding
   procedure Put_Error      (Format     : in out HTML_Format_Type;
                             Err        : in     Exception_Occurrence);
   overriding
   procedure Put_Summary    (Format     : in out HTML_Format_Type;
                             Report     : in     Report_Type);

   function  New_HTML_Format return HTML_Format_Ptr;

private

   type Menu_Item_2 is
      record
         Name            : Unbounded_String;
         Status          : Status_Type := Status_Passed;
         Is_Background   : Boolean;
         Feature_ID      : Natural := 0;
         Scenario_ID     : Natural := 0;
      end record;

   package Menu_Vectors_2 is
      new Ada.Containers.Vectors (Positive, Menu_Item_2, "=");

   type Menu_Item_1 is
      record
         Name            : Unbounded_String;
         Status          : Status_Type := Status_Passed;
         Feature_ID      : Natural := 0;
         Sub_Menu        : Menu_Vectors_2.Vector;
      end record;

   package Menu_Vectors is
      new Ada.Containers.Vectors (Positive, Menu_Item_1, "=");

   type HTML_Format_Type is new Format_Type with
      record
         Close_Step      : Boolean := False;
         In_Background   : Boolean := False;
         Have_Background : Boolean := False;
         Skip_Scenarios  : Boolean := False;
         Inline_Backgrnd : Boolean := False;
         Run_Feature     : Boolean := False;
         Feature_ID      : Natural := 0;
         Background_ID   : Natural := 0;
         Scenario_ID     : Natural := 0;
         Step_ID         : Natural := 0;
         Curr_Scenario   : Menu_Item_2;
         Curr_Feature    : Menu_Item_1;
         Menu            : Menu_Vectors.Vector;
      end record;


end AdaSpecLib.Format.HTML;
