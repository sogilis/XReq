--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Strings;

use Ada.Strings.Unbounded;

package AdaSpec.Features is

   -------------------
   --  Stanza_Type  --
   -------------------

   type Stanza_Type is
      record
         Prefix : Prefix_Type;
         Stanza : Unbounded_String;
      end record;

   package Stanza_Container is
      new Ada.Containers.Vectors (Natural, Stanza_Type);

   ---------------------
   --  Scenario_Type  --
   ---------------------

   type Scenario_Type is
      record
         Name    : Unbounded_String;
         Stanzas : Stanza_Container.Vector;
      end record;

   package Scenario_Container is
      new Ada.Containers.Vectors (Natural, Scenario_Type);

   -------------------
   -- Feature_Type  --
   -------------------

   type Feature_Type is tagged
      record
         Name        : Unbounded_String;
         Description : Util.Strings.Vectors.Vector;
         Background  : Scenario_Type;
         Scenarios   : Scenario_Container.Vector;
      end record;
   type Feature_Ptr  is access all Feature_Type'Class;

   Unparsed_Feature : exception;

   function  Parsed      (F : in Feature_Type) return Boolean;
   function  Name        (F : in Feature_Type) return String;
   function  To_String   (F : in Feature_Type) return String;

   ------------------------
   -- Feature_File_Type  --
   ------------------------

   type Feature_File_Type is new Feature_Type with private;

   procedure Make      (F         : in out Feature_File_Type;
                        File_Name : in String);
   function  File_Name (F         : in Feature_File_Type) return String;
   procedure Parse     (F         : in out Feature_File_Type);

   overriding
   function  Parsed    (F         : in Feature_File_Type) return Boolean;
   overriding
   function  To_String (F         : in Feature_File_Type) return String;






private  ----------------------------------------------------------------------

   type Feature_File_Type is new Feature_Type with
      record
         Parsed      : Boolean := False;
         File_Name   : Unbounded_String;
      end record;

end AdaSpec.Features;
