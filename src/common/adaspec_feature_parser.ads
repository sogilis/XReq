--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

generic

   type Feature_Type  is private;
   type Scenario_Type is private;
   type Step_Type     is private;
   type Table_Type    is private;

   with function  Read_Line return String;
   with function  End_Of_File return Boolean;
   with procedure Log_Line (Line : in String);

   type String_Array is array (Natural range <>) of Unbounded_String;

   ----------------------------------
   --  Operations on Feature_Type  --
   ----------------------------------

   with procedure Set_Name        (Feature     : in out Feature_Type;
                                   Name        : in     String);
   with procedure Set_Position    (Feature     : in out Feature_Type;
                                   Line_Number : in     Positive;
                                   File_Name   : in     String);
   with procedure Set_Description (Feature     : in out Feature_Type;
                                   Description : in     String_Array);
   with procedure Append_Scenario (Feature     : in out Feature_Type;
                                   Scenario    : in     Scenario_Type);

   -----------------------------------
   --  Operations on Scenario_Type  --
   -----------------------------------

   with function  Get_Scenario    (Outline     : in     Boolean;
                                   Name        : in     String;
                                   Line_Number : in     Positive;
                                   File_Name   : in     String;
                                   Tags        : in     String_Array)
                                                 return Scenario_Type;
   with procedure Create          (Scenario    : out    Scenario_Type);
   with procedure Append_Step     (Scenario    : in out Scenario_Type;
                                   Step        : in     Step_Type);

   -------------------------------
   --  Operations on Step_Type  --
   -------------------------------

   with procedure Create_Given    (Step        : out    Step_Type);
   with procedure Create_When     (Step        : out    Step_Type);
   with procedure Create_Then     (Step        : out    Step_Type);
   with procedure Set_Stanza      (Step        : in out Step_Type;
                                   Stanza      : in     String);
   with procedure Set_Position    (Step        : in out Step_Type;
                                   Line_Number : in     Positive;
                                   File_Name   : in     String);

   --------------------------------
   --  Operations on Table_Type  --
   --------------------------------

package AdaSpec_Feature_Parser is

   procedure Parse (Feature   : in out Feature_Type;
                    File_Name : in     String);

end AdaSpec_Feature_Parser;
