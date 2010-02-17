--                         Copyright (C) 2010, Sogilis                       --

with AUnit.Test_Suites;
with AUnit.Simple_Test_Cases;

package Test_Suite is

   -------------
   --  Suite  --
   -------------

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   ---------------
   --  Banners  --
   ---------------

   procedure Title (Text : in String);
   procedure Begin_Test (Text : in String) renames Title;
   procedure End_Test;

   --------------
   --  Output  --
   --------------

   procedure T_Put      (Item : in String);
   procedure T_Put_Line (Item : in String);
   procedure T_New_Line;

   package Text_IO is

      procedure Put      (Item : in String) renames T_Put;
      procedure Put_Line (Item : in String) renames T_Put_Line;
      procedure New_Line                    renames T_New_Line;

   end Text_IO;

   ------------------------
   --  Custom_Test_Case  --
   ------------------------

   type Test_Case_Type is abstract
      new AUnit.Simple_Test_Cases.Test_Case with null record;
   type Test_Case_Ptr is access all Test_Case_Type;

   function  Name     (T : in     Test_Case_Type) return String is abstract;
   procedure Run      (T : in out Test_Case_Type) is abstract;
   function  Name     (T : in     Test_Case_Type) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_Case_Type);

end Test_Suite;
