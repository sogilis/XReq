--                         Copyright (C) 2010, Sogilis                       --

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
with AdaSpecLib.Report;

use  Ada.Exceptions;
use  AdaSpecLib.Report;

package AdaSpecLib.Format is

   type Status_Type is (Status_Passed, Status_Skipped, Status_Failed);

   type Format_Type is interface;
   type Format_Ptr  is access all Format_Type'Class;

   procedure Put_Feature    (Format     : in out Format_Type;
                             Feature    : in     String)
                             is abstract;
   procedure Put_Background (Format     : in out Format_Type;
                             Background : in     String)
                             is abstract;
   procedure Put_Scenario   (Format     : in out Format_Type;
                             Scenario   : in     String)
                             is abstract;
   procedure Put_Step       (Format     : in out Format_Type;
                             Step       : in     Step_Type;
                             Name       : in     String;
                             Args       : in     Arg_Type;
                             Success    : in     Status_Type)
                             is abstract;
   procedure Put_Error      (Format     : in out Format_Type;
                             Err        : in     Exception_Occurrence)
                             is abstract;
   procedure Put_Summary    (Format     : in out Format_Type;
                             Report     : in     Report_Type)
                             is abstract;
   procedure Set_Output     (Format     : in out Format_Type;  --  GCOV_IGNORE
                             Output     : in     String) is null;

   procedure Free is new Ada.Unchecked_Deallocation
      (Format_Type'Class, Format_Ptr);

   function Get_Formatter (Name : in String) return Format_Ptr;

end AdaSpecLib.Format;