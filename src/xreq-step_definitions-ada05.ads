--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with GNAT.Regpat;
with Util.Strings.Pool;


use Util.Strings.Pool;

package XReq.Step_Definitions.Ada05 is


   --  Called in XReq.Steps.Load
   procedure Parse_Directory (Steps      : in out Step_Definitions_Type;
                              Logger     : in     Logger_Ptr;
                              Directory  : in     String;
                              Fill_Steps : in     Boolean := False);
   --  IMPORTANT: deallocate Steps_Type

   --  Called in XReq.Steps.Add_Steps
   procedure Add_Steps       (Steps      : in out Step_Definitions_Type;
                              New_Steps  : in     String_Set;
                              Step_Pkg   : in     String;
                              Directory  : in     String;
                              Logger     : in     Logger_Ptr);
   --  IMPORTANT: deallocate Steps_Type


   type Ada_Step_File_Type is new Step_File_Type with private;
   type Ada_Step_File_Ptr  is access all Ada_Step_File_Type'Class;

   procedure Make (S          : out Ada_Step_File_Type;
                   File_Name  : in  String;
                   Fill_Steps : in  Boolean := False);

   overriding function  Parsed    (S          : in     Ada_Step_File_Type)
                                                return Boolean;
   overriding procedure Parse     (S          : in out Ada_Step_File_Type;
                                   Logger     : in     Logger_Ptr);

   overriding function  Find      (S       : in     Ada_Step_File_Type;
                                   Stanza  : in     Step_Type)
                                             return Step_Match_Type;

   overriding procedure Finalize  (S       : in out Ada_Step_File_Type);

private

   type Pattern_Matcher_Ptr is access all GNAT.Regpat.Pattern_Matcher;

   procedure Free is new Ada.Unchecked_Deallocation
      (GNAT.Regpat.Pattern_Matcher, Pattern_Matcher_Ptr);

   type Step_Definition_Type is
      record
         Prefix    : Step_Kind;
         Pattern_R : Pattern_Matcher_Ptr;
         Pattern_S : Unbounded_String;
         Proc_Name : Unbounded_String;
         Position  : Position_Type;
      end record;

   package Step_Container is new
      Ada.Containers.Vectors (Natural, Step_Definition_Type);

   type Ada_Step_File_Type is new Step_File_Type with
      record
         Parsed     : Boolean := False;
         Steps      : Step_Container.Vector;
         Fill_Steps : Boolean := False;
         Procedures : String_Pool;
      end record;

end XReq.Step_Definitions.Ada05;