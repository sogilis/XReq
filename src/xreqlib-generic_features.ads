--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with XReqLib.Interface_Scenarios;

use Ada.Strings.Unbounded;
use XReqLib.Interface_Scenarios;

generic

   type Scenario_Type is new Scenario_Interface with private;
   with function "=" (Left, Right : in Scenario_Type) return Boolean;

package XReqLib.Generic_Features is

   -------------------
   -- Feature_Type  --
   -------------------

   type Feature_Type is tagged private;
   type Feature_Ptr  is access all Feature_Type'Class;

   procedure Free            (F      : in out Feature_Ptr);

   --  Creation  --------------------------------------------------------------

   procedure Make            (F      : out    Feature_Type;
                              Name   : in     String := "");

   --  Process  ---------------------------------------------------------------

   function  To_String   (F : in Feature_Type) return String;

   --  Properties: Read  ------------------------------------------------------

   function  Parsed      (F : in Feature_Type) return Boolean;
   function  Name        (F : in Feature_Type) return String;
   function  Position    (F : in Feature_Type) return Position_Type;
   function  Background  (F : in Feature_Type) return Scenario_Type;
   function  Description (F : in Feature_Type) return String;

   --  Properties: Write  -----------------------------------------------------

   procedure Set_Name           (F      : in out Feature_Type;
                                 Name   : in     String);
   procedure Set_Position       (F      : in out Feature_Type;
                                 Pos    : in     Position_Type);
   procedure Set_Background     (F      : in out Feature_Type;
                                 Bg     : in     Scenario_Type);
   procedure Set_Description    (F      : in out Feature_Type;
                                 Desc   : in     String);
   procedure Append_Description (F      : in out Feature_Type;
                                 Desc   : in     String);

   --  Collection: Scenario  --------------------------------------------------

   function  Scenario_First     (F : in Feature_Type) return Natural;
   function  Scenario_Last      (F : in Feature_Type) return Integer;
   function  Scenario_Count     (F : in Feature_Type) return Natural;
   function  Scenario_Element   (F : in Feature_Type;
                                 I : in Natural)      return Scenario_Type;
   procedure Scenario_Append    (F : in out Feature_Type;
                                 S : in     Scenario_Type);

   ----------------------------------------------------------------------------

   Null_Feature     : constant Feature_Type;
   Unparsed_Feature : exception;

private

   package Scenario_Container is
      new Ada.Containers.Vectors (Natural, Scenario_Type, "=");

   type Feature_Type is tagged
      record
         Name        : Unbounded_String;
         Description : Unbounded_String;
         Pos         : Position_Type;
         Background  : Scenario_Type;
         Scenarios   : Scenario_Container.Vector;
      end record;

   Null_Feature : constant Feature_Type := (others => <>);

end XReqLib.Generic_Features;
