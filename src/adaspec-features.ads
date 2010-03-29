--                         Copyright (C) 2010, Sogilis                       --

with Ada.Unchecked_Deallocation;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Strings;
with Util.IO;
with AdaSpec.Steps;
with AdaSpecLib.String_Tables;

use Ada.Strings.Unbounded;
use Util.Strings;
use Util.IO;
use AdaSpec.Steps;

package AdaSpec.Features is

   Parse_Error : exception;

   ---------------------
   --  Scenario_Type  --
   ---------------------

   type Scenario_Type (Outline : Boolean := False) is
      record
         Name          : Unbounded_String;
         Pos           : Position_Type;
         Tags          : String_Vector;
         Stanzas       : Stanza_Container.Vector;
         case Outline is
            when True =>
               Table   : AdaSpecLib.String_Tables.Table;
            when False =>
               null;
         end case;
      end record;
   type Scenario_Ptr is access all Scenario_Type;
   Null_Scenario : Scenario_Type;

   package Scenario_Container is
      new Ada.Containers.Vectors (Natural, Scenario_Type, "=");

   procedure Make   (Scenario : out    Scenario_Type;
                     Name     : in     String := "");
   procedure Append (Scenario : in out Scenario_Type;
                     Stanza   : in     Stanza_Type);

   -------------------
   -- Feature_Type  --
   -------------------

   type Feature_Type is tagged
      record
         Name        : Unbounded_String;
         Description : String_Vector;
         Pos         : Position_Type;
         Background  : Scenario_Type;
         Scenarios   : Scenario_Container.Vector;
      end record;
   type Feature_Ptr  is access all Feature_Type'Class;

   Unparsed_Feature : exception;

   procedure Make           (F      : out    Feature_Type;
                             Name   : in     String := "");
   function  Parsed         (F      : in     Feature_Type) return Boolean;
   function  Name           (F      : in     Feature_Type) return String;
   function  To_String      (F      : in     Feature_Type) return String;
   procedure Append         (F      : in out Feature_Type;
                             S      : in     Scenario_Type);
   procedure Set_Background (F      : in out Feature_Type;
                             Bg     : in     Scenario_Type);

   procedure Free is new Ada.Unchecked_Deallocation
      (Feature_Type'Class, Feature_Ptr);

   Null_Feature : constant Feature_Type := (others => <>);

   ------------------------
   -- Feature_File_Type  --
   ------------------------

   type Feature_File_Type is new Feature_Type with private;
   type Feature_File_Ptr  is access all Feature_File_Type'Class;

   function  Null_Feature_File return Feature_File_Type;

   procedure Make      (F         : out    Feature_File_Type;
                        File_Name : in     String);
   function  Create    (File_Name : in     String) return Feature_File_Type;
   function  File_Name (F         : in     Feature_File_Type) return String;
   procedure Parse     (F         : in out Feature_File_Type;
                        Log       : in     Logger_Ptr);

   overriding
   function  Parsed    (F         : in     Feature_File_Type) return Boolean;
   overriding
   function  To_String (F         : in     Feature_File_Type) return String;






private  ----------------------------------------------------------------------

   type Feature_File_Type is new Feature_Type with
      record
         Parsed      : Boolean := False;
         File_Name   : Unbounded_String;
      end record;

end AdaSpec.Features;
