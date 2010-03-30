--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpecLib.Generic_Scenarios is


   function New_Scenario (Outline  : in Boolean;
                          Name     : in String;
                          Position : in Position_Type) return Scenario_Type
   is
   begin
      if Outline then
         return Scenario_Type'( D =>
           (Outline => True,
            Name    => To_Unbounded_String (Name),
            Pos     => Position));
      else
         return Scenario_Type'( D =>
           (Outline => False,
            Name    => To_Unbounded_String (Name),
            Pos     => Position));
      end if;
   end New_Scenario;

end AdaSpecLib.Generic_Scenarios;
