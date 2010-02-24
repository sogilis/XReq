--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with GNAT.Regpat;

use Ada.Containers;

package AdaSpec.Steps.Ada is


   --  Called in AdaSpec.Steps.Load
   procedure Parse_Directory (Steps     : in out Steps_Type;
                              Directory : in     String);


   type Ada_Step_File_Type is new Step_File_Type with private;
   type Ada_Step_File_Ptr  is access all Ada_Step_File_Type'Class;

   procedure Make (S         : out Ada_Step_File_Type;
                   File_Name : in  String);

   overriding function  Parsed    (S       : in     Ada_Step_File_Type)
                                            return Boolean;
   overriding procedure Parse     (S      : in out Ada_Step_File_Type);

   overriding function  Contains  (S      : in     Ada_Step_File_Type;
                                   Stanza : in     Stanza_Type)
                                            return Boolean;

   overriding function  Find      (S      : in     Ada_Step_File_Type;
                                   Stanza : in     Stanza_Type)
                                            return String;

private

   type Step_Type is
      record
         Prefix    : Prefix_Type;
         Pattern_R : access GNAT.Regpat.Pattern_Matcher;
         Pattern_S : Unbounded_String;
         Proc_Name : Unbounded_String;
      end record;

   package Step_Container is new Vectors (Natural, Step_Type);

   type Ada_Step_File_Type is new Step_File_Type with
      record
         Parsed : Boolean := False;
         Steps  : Step_Container.Vector;
      end record;

end AdaSpec.Steps.Ada;
