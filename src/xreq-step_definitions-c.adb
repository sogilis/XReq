
with Ada.Directories;

use Ada.Directories;

package body XReq.Step_definitions.C is

   -----------------------
   --  Parse_Directory  --
   -----------------------

   procedure Parse_Directory (Steps      : in out Step_Definitions_Type;
                              Logger     : in     Logger_Ptr;
                              Directory  : in     String;
                              Fill_Steps : in     Boolean := False)
   is
      use Step_Definition_Vectors;
      Search  : Search_Type;
      Element : Directory_Entry_Type;
      Step    : C_Step_File_Ptr;
   begin
      Start_Search (Search, Directory, "*.h",
                    (Ordinary_File => True, others => False));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Element);
         Step := new C_Step_File_Type;
         Step.Make  (Compose (Directory, Simple_Name (Element)), Fill_Steps);
         Step.Parse (Logger);
         Step_Definition_Vectors.Append (Steps, Step_File_Ptr (Step));
      end loop;
      End_Search (Search);
   end Parse_Directory;

   ------------
   --  Make  --
   ------------

   procedure Make (S          : out C_Step_File_Type;
                   File_Name  : in  String;
                   Fill_Steps : in  Boolean := False) is
   begin
      S := (File_Name  => To_Unbounded_String (File_Name),
            Parsed     => False,
            Fill_Steps => Fill_Steps,
            others     => <>);
   end Make;

   -------------
   --  Parse  --
   -------------

   procedure Parse     (S          : in out C_Step_File_Type;
                        Logger     : in     Logger_Ptr)
   is
      pragma Unreferenced (Logger);
   begin
      S.Parsed := True;
      raise Not_Yet_Implemented;
   end Parse;


end XReq.Step_Definitions.C;