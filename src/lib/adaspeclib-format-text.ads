--                         Copyright (C) 2010, Sogilis                       --

package AdaSpecLib.Format.Text is

   type Text_Format_Type is new Format_Type with null record;
   type Text_Format_Ptr  is access all Text_Format_Type;

   overriding
   procedure Put_Feature    (Format     : in out Text_Format_Type;
                             Feature    : in     String);
   overriding
   procedure Put_Background (Format     : in out Text_Format_Type;
                             Background : in     String);
   overriding
   procedure Put_Scenario   (Format     : in out Text_Format_Type;
                             Scenario   : in     String);
   overriding
   procedure Put_Step       (Format     : in out Text_Format_Type;
                             Step       : in     Step_Type;
                             Name       : in     String;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type);
   overriding
   procedure Put_Error      (Format     : in out Text_Format_Type;
                             Err        : in     Exception_Occurrence);
   overriding
   procedure Put_Summary    (Format     : in out Text_Format_Type;
                             Report     : in     Report_Type);

   function  New_Text_Format return Text_Format_Ptr;


end AdaSpecLib.Format.Text;
