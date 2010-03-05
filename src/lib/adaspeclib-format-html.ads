--                         Copyright (C) 2010, Sogilis                       --

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
                             Feature    : in     String);
   overriding
   procedure Put_Background (Format     : in out HTML_Format_Type;
                             Background : in     String);
   overriding
   procedure Put_Scenario   (Format     : in out HTML_Format_Type;
                             Scenario   : in     String);
   overriding
   procedure Put_Step       (Format     : in out HTML_Format_Type;
                             Step       : in     Step_Type;
                             Name       : in     String;
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

   type HTML_Format_Type is new Format_Type with
      record
         In_Background   : Boolean := False;
         Have_Background : Boolean := False;
         Feature_ID      : Natural := 0;
         Background_ID   : Natural := 0;
         Scenario_ID     : Natural := 0;
         Step_ID         : Natural := 0;
      end record;


end AdaSpecLib.Format.HTML;
