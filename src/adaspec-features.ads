--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Strings;

use Ada.Containers;
use Ada.Strings.Unbounded;

package AdaSpec.Features is

   type Stanza_Type is
      record
         Prefix : Prefix_Type;
         Stanza : Unbounded_String;
      end record;

   package Stanza_Container is new Vectors (Natural, Stanza_Type);
   use Stanza_Container;

   type Scenario_Type is
      record
         Name    : Unbounded_String;
         Stanzas : Stanza_Container.Vector;
      end record;

   package Scenario_Container is new Vectors (Natural, Scenario_Type);
   use Scenario_Container;

   type Feature_File_Type is tagged
      record
         File_Name   : Unbounded_String;
         Parsed      : Boolean := False;
         Name        : Unbounded_String;
         Description : Util.Strings.Vectors.Vector;
         Background  : Scenario_Type;
         Scenarios   : Scenario_Container.Vector;
      end record;
   type Feature_File_Ptr  is access all Feature_File_Type'Class;

   Unparsed_Feature : exception;

   procedure Make (S         : in out Feature_File_Type;
                   File_Name : in String);

   function  File_Name (S : in Feature_File_Type) return String;
   function  Parsed    (S : in Feature_File_Type) return Boolean;
   procedure Parse     (S : in out Feature_File_Type);

end AdaSpec.Features;