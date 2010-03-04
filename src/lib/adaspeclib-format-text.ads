--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Finalization;

package AdaSpecLib.Format.Text is

   type Text_Format_Type is new Format_Type with private;
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
   overriding
   procedure Set_Output     (Format     : in out Text_Format_Type;
                             Output     : in     String);

   function  New_Text_Format return Text_Format_Ptr;

   package New_Text_IO is

      type Local_File_Ptr is access Ada.Text_IO.File_Type;

      type File_Type is new Ada.Finalization.Controlled with
         record
            Output_Ownership : Boolean := False;
            Output_Ptr       : Local_File_Ptr
                             := new Ada.Text_IO.File_Type;
            Output_Access    : Ada.Text_IO.File_Access
                             := Ada.Text_IO.Current_Output;
         end record;

      subtype File_Mode is Ada.Text_IO.File_Mode;

      procedure Put      (File : in out File_Type;
                          Item : in     String);
      procedure Put      (File : in out File_Type;
                          Item : in     Character);
      procedure Put_Line (File : in out File_Type;
                          Item : in     String);
      procedure New_Line (File : in out File_Type);
      procedure Create   (File : in out File_Type;
                          Mode : in     File_Mode;
                          Name : in     String := "");
      procedure Close    (File : in out File_Type);
      overriding
      procedure Finalize (File : in out File_Type);

   end New_Text_IO;

private

   type Text_Format_Type is new Format_Type with
      record
         Output : New_Text_IO.File_Type;
      end record;

end AdaSpecLib.Format.Text;
