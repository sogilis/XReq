--                         Copyright (C) 2010, Sogilis                       --

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with GNAT.Regpat;
with Util.IO;
with Util.Strings;
with XReqLib;
with XReq.Lang;
with XReq.Steps;

use Ada.Strings.Unbounded;
use Util.IO;
use Util.Strings;
use XReqLib;
use XReq.Lang;
use XReq.Steps;

package XReq.Step_Definitions is

   Ambiguous_Match : exception;

   type Match_Location is  --  GCOV_IGNORE
      record
         First : Natural;
         Last  : Natural;
      end record;

   package Match_Vectors is
      new Ada.Containers.Vectors (Natural, Match_Location, "=");

   type Step_Match_Type is
      record
         Match     : Boolean := False;
         Proc_Name : Unbounded_String;
         Matches   : Match_Vectors.Vector;
         Position  : Position_Type;
      end record;

   ----------------------
   --  Step_File_Type  --
   ----------------------

   type Step_File_Type is abstract tagged private;
   type Step_File_Ptr  is access all Step_File_Type'Class;

   package Step_Definition_Vectors is
      new Ada.Containers.Vectors (Natural, Step_File_Ptr, "=");

   Unparsed_Step : exception;

   function  File_Name (S       : in     Step_File_Type) return String;
   function  Parsed    (S       : in     Step_File_Type) return Boolean;
   procedure Parse     (S       : in out Step_File_Type;
                        Logger  : in     Logger_Ptr) is abstract;

   function  Contains  (S       : in  Step_File_Type;
                        Stanza  : in  Step_Type) return Boolean;

   function  Find      (S       : in  Step_File_Type;
                        Stanza  : in  Step_Type) return String;
   function  Find      (S       : in  Step_File_Type;
                        Stanza  : in  Step_Type)
                        return Step_Match_Type;
   procedure Find      (S       : in  Step_File_Type;
                        Stanza  : in  Step_Type;
                        Proc    : out Unbounded_String;
                        Matches : out Match_Vectors.Vector;
                        Found   : out Boolean);
   procedure Finalize  (S       : in out Step_File_Type);

   procedure Free      (S : in out Step_File_Ptr);


   ------------------
   --  Steps_Type  --
   ------------------

   subtype Step_Definitions_Type is     --  GCOV_IGNORE
      Step_Definition_Vectors.Vector;   --  GCOV_IGNORE

   function  Load      (Directory  : in     String;
                        Language   : in     Language_Type)
                                     return Step_Definitions_Type;
   --  IMPORTANT: deallocate Steps_Type

   procedure Load      (Steps      : in out Step_Definitions_Type;
                        Logger     : in     Logger_Ptr;
                        Directory  : in     String;
                        Language   : in     Language_Type;
                        Fill_Steps : in     Boolean := False);
   --  IMPORTANT: deallocate Steps_Type

   procedure Add_Steps (Steps      : in out Step_Definitions_Type;
                        New_Steps  : in     String_Set;
                        Step_Pkg   : in     String;
                        Directory  : in     String;
                        Language   : in     Language_Type;
                        Logger     : in     Logger_Ptr);

   function  Contains  (Steps      : in  Step_Definitions_Type;
                        Stanza     : in  Step_Type) return Boolean;
   function  Find      (Steps      : in  Step_Definitions_Type;
                        Stanza     : in  Step_Type) return String;
   function  Find      (Steps      : in  Step_Definitions_Type;
                        Stanza     : in  Step_Type) return Step_Match_Type;
   procedure Find      (Steps      : in  Step_Definitions_Type;
                        Stanza     : in  Step_Type;
                        Proc       : out Unbounded_String;
                        Matches    : out Match_Vectors.Vector;
                        Found      : out Boolean);

   procedure Free      (Steps      : in out Step_Definitions_Type);

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

   procedure Finalize (Steps : in out Step_Container.Vector);

   type Step_File_Type is abstract tagged
      record
         Parsed     : Boolean := False;
         File_Name  : Unbounded_String;
         Steps      : Step_Container.Vector;
      end record;

end XReq.Step_Definitions;
