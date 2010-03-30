--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with AdaSpecLib.String_Tables;

use Ada.Strings.Unbounded;

generic

   type Step_Type is private;
   with function "=" (Left, Right : in Step_Type) return Boolean;

package AdaSpecLib.Generic_Scenarios is

   subtype Table_Type is AdaSpecLib.String_Tables.Table;

   ---------------------
   --  Scenario_Type  --
   ---------------------

   type Scenario_Type is tagged private;
   type Scenario_Ptr is access all Scenario_Type'Class;

   function  New_Scenario (Name     : in     String;
                           Position : in     Position_Type := Null_Position;
                           Outline  : in     Boolean := False;
                           Tags     : in     String_Vector :=
                                             String_Vectors.Empty_Vector)
                                      return Scenario_Type;

   procedure Step_Append  (Scenario : in out Scenario_Type;
                           Stanza   : in     Step_Type);
   function  Step_First   (Scenario : in     Scenario_Type) return Natural;
   function  Step_Last    (Scenario : in     Scenario_Type) return Integer;
   function  Step_Count   (Scenario : in     Scenario_Type) return Natural;
   function  Step_Element (Scenario : in     Scenario_Type;
                           Index    : in     Natural)       return Step_Type;

   function  Outline      (S : in Scenario_Type) return Boolean;
   function  Name         (S : in Scenario_Type) return String;
   function  Position     (S : in Scenario_Type) return Position_Type;
   function  Tag_Vector   (S : in Scenario_Type) return String_Vector;
   function  Table        (S : in Scenario_Type)
                           return AdaSpecLib.String_Tables.Table;

   procedure Set_Table    (S     : in out Scenario_Type;
                           Table : in AdaSpecLib.String_Tables.Table);

   Null_Scenario         : constant Scenario_Type;
   Null_Scenario_Outline : constant Scenario_Type;

private

   package Step_Vectors is new Ada.Containers.Vectors
     (Natural, Step_Type, "=");

   type Scenario_Record (Outline : Boolean := False) is
      record
         Name          : Unbounded_String;
         Pos           : Position_Type;
         Tags          : String_Vector;
         Steps         : Step_Vectors.Vector;
         case Outline is
            when True =>
               Table   : AdaSpecLib.String_Tables.Table;
            when False =>
               null;
         end case;
      end record;

   type Scenario_Type is tagged
      record
         D : Scenario_Record;
      end record;

   Null_Scenario         : constant Scenario_Type := (others => <>);
   Null_Scenario_Outline : constant Scenario_Type :=
     (D => Scenario_Record'(Outline => True, others => <>));

end AdaSpecLib.Generic_Scenarios;
