--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with GNAT.Regpat;

package AdaSpec.Steps.Ada05 is


   --  Called in AdaSpec.Steps.Load
   procedure Parse_Directory (Steps     : in out Steps_Type;
                              Directory : in     String);
   --  IMPORTANT: deallocate Steps_Type


   type Ada_Step_File_Type is new Step_File_Type with private;
   type Ada_Step_File_Ptr  is access all Ada_Step_File_Type'Class;

   procedure Make (S         : out Ada_Step_File_Type;
                   File_Name : in  String);

   overriding function  Parsed    (S       : in     Ada_Step_File_Type)
                                             return Boolean;
   overriding procedure Parse     (S       : in out Ada_Step_File_Type);

   overriding procedure Find      (S       : in     Ada_Step_File_Type;
                                   Stanza  : in     Stanza_Type;
                                   Proc    : out    Unbounded_String;
                                   Matches : out    Match_Vectors.Vector;
                                   Found   : out    Boolean);

   overriding procedure Finalize  (S       : in out Ada_Step_File_Type);

private

   type Pattern_Matcher_Ptr is access all GNAT.Regpat.Pattern_Matcher;

   procedure Free is new Ada.Unchecked_Deallocation
      (GNAT.Regpat.Pattern_Matcher, Pattern_Matcher_Ptr);

   type Step_Type is
      record
         Prefix    : Prefix_Type;
         Pattern_R : Pattern_Matcher_Ptr;
         Pattern_S : Unbounded_String;
         Proc_Name : Unbounded_String;
      end record;

   package Step_Container is new
      Ada.Containers.Vectors (Natural, Step_Type);

   type Ada_Step_File_Type is new Step_File_Type with
      record
         Parsed : Boolean := False;
         Steps  : Step_Container.Vector;
      end record;

end AdaSpec.Steps.Ada05;
