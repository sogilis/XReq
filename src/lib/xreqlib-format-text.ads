--                         Copyright (C) 2010, Sogilis                       --

package XReqLib.Format.Text is

   type Text_Format_Type is new Format_Type with private;
   type Text_Format_Ptr  is access all Text_Format_Type;

   overriding
   procedure Put_Feature    (Format     : in out Text_Format_Type;
                             Feature    : in     String;
                             Description : in    String;
                             Position   : in     String);
   overriding
   procedure Put_Background (Format     : in out Text_Format_Type;
                             Background : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type);
   overriding
   procedure Put_Outline    (Format     : in out Text_Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type);
   overriding
   procedure Put_Outline_Report
                            (Format     : in out Text_Format_Type;
                             Table      : in     Table_Type);
   overriding
   procedure Put_Scenario   (Format     : in out Text_Format_Type;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type);
   overriding
   procedure Put_Scenario_Outline
                            (Format     : in out Text_Format_Type;
                             Num        : in     Natural;
                             Scenario   : in     String;
                             Position   : in     String;
                             Tags       : in     Tag_Array_Type);
   overriding
   procedure Put_Step       (Format     : in out Text_Format_Type;
                             Step       : in     Step_Kind;
                             Name       : in     String;
                             Position   : in     String;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type);
   overriding
   procedure Put_Error      (Format     : in out Text_Format_Type;
                             Err        : in     Exception_Occurrence);
   overriding
   procedure Put_Summary    (Format     : in out Text_Format_Type;
                             Report     : in     Report_Type;
                             D          : in     Duration);

   function  New_Text_Format return Text_Format_Ptr;

private

   type Text_Format_Type is new Format_Type with
      record
         Has_Previous_Step  : Boolean := False;
         Previous_Step_Type : Step_Kind;
      end record;

end XReqLib.Format.Text;
