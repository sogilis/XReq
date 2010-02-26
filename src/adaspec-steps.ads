--                         Copyright (C) 2010, Sogilis                       --

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with AdaSpec.Stanzas;

use Ada.Strings.Unbounded;
use AdaSpec.Stanzas;

package AdaSpec.Steps is

   type Match_Location is  --  GCOV_IGNORE
      record
         First : Natural;
         Last  : Natural;
      end record;

   package Match_Vectors is
      new Ada.Containers.Vectors (Natural, Match_Location, "=");

   ----------------------
   --  Step_File_Type  --
   ----------------------



   type Step_File_Type is abstract tagged
      record
         File_Name : Unbounded_String;
      end record;
      --  I would like this private but I can't
      --  premature use of type with private component
   type Step_File_Ptr  is access all Step_File_Type'Class;

   package Step_Vectors is
      new Ada.Containers.Vectors (Natural, Step_File_Ptr, "=");

   Unparsed_Step : exception;

   function  File_Name (S : in Step_File_Type) return String;
   function  Parsed    (S : in Step_File_Type) return Boolean is abstract;
   procedure Parse     (S : in out Step_File_Type) is abstract;

   function  Contains  (S       : in  Step_File_Type;
                        Stanza  : in  Stanza_Type) return Boolean is abstract;

   function  Find      (S       : in  Step_File_Type;
                        Stanza  : in  Stanza_Type) return String is abstract;
   procedure Find      (S       : in  Step_File_Type;
                        Stanza  : in  Stanza_Type;
                        Proc    : out Unbounded_String;
                        Matches : out Match_Vectors.Vector;
                        Found   : out Boolean) is abstract;

   procedure Free is new Ada.Unchecked_Deallocation
      (Step_File_Type'Class, Step_File_Ptr);


   ------------------
   --  Steps_Type  --
   ------------------

   subtype Steps_Type is Step_Vectors.Vector;

   function  Load      (Directory : in     String) return Steps_Type;
   --  IMPORTANT: deallocate Steps_Type

   procedure Load      (Steps     : in out Steps_Type;
                        Directory : in     String);
   --  IMPORTANT: deallocate Steps_Type

   function  Contains  (Steps     : in  Steps_Type;
                        Stanza    : in  Stanza_Type) return Boolean;

   function  Find      (Steps     : in  Steps_Type;
                        Stanza    : in  Stanza_Type) return String;
   procedure Find      (Steps     : in  Steps_Type;
                        Stanza    : in  Stanza_Type;
                        Proc      : out Unbounded_String;
                        Matches   : out Match_Vectors.Vector;
                        Found     : out Boolean);

   procedure Free      (Steps     : in out Steps_Type);

end AdaSpec.Steps;
