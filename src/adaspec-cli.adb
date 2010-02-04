--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO; use Ada.Text_IO;

package body AdaSpec.CLI is

   procedure Help is
   begin

      Put_Line ("adaspec - generate tests from specification");
      Put_Line ("");
      Put_Line ("SYNOPSIS");
      Put_Line ("");
      Put_Line ("    adaspec [OPTIONS] FEATURE ...");
      Put_Line ("");
      Put_Line ("DESCRIPTION");
      Put_Line ("");
      Put_Line ("    adaspec takes as input feature files describing");
      Put_Line ("    specifications and generate tests for these features in");
      Put_Line ("    languages such as Ada, C, C++ depending on the template");
      Put_Line ("    used.");
      Put_Line ("");
      Put_Line ("    Specifications are described in a formalized natural");
      Put_Line ("    language (see cucumber documentation) and the sentences");
      Put_Line ("    are translated in code using steps. These steps define");
      Put_Line ("    a relation between the regular expression matching the");
      Put_Line ("    sentance and a function toperform what the sentance");
      Put_Line ("    describes.");
      Put_Line ("");
      Put_Line ("OPTIONS");
      Put_Line ("");
      Put_Line ("    FEATURE (NOT IMPLEMENTED: more than one features file)");
      Put_Line ("        The .feature file containing cucumber-like");
      Put_Line ("        specification. It is possible to specify multiple");
      Put_Line ("        features on the command-line all of them will be");
      Put_Line ("        processed");
      Put_Line ("");
      Put_Line ("    -h, -help, --help");
      Put_Line ("        Help message");
      Put_Line ("");
      Put_Line ("    -s, --step STEPDIR");
      Put_Line ("        Specify a step directory. By default, the step/");
      Put_Line ("        directory relative to each feature file specified");
      Put_Line ("        is searched.");
      Put_Line ("");
      Put_Line ("    -o, --output DIR");
      Put_Line ("        Choose in which directory to generate the files");
      Put_Line ("        generated. They have the same name as the feature");
      Put_Line ("        files.");
      Put_Line ("        By default, they are generated in the tests/");
      Put_Line ("        directory relative to the feature file.");
      Put_Line ("");
      Put_Line ("    -l, --lang LANG (NOT IMPLEMENTED)");
      Put_Line ("        Choose wich language to generate.");
      Put_Line ("        Available values: Ada");
      Put_Line ("");
      Put_Line ("-- ");
      Put_Line ("Copyright (c) 2010 SOGILIS");

   end Help;

end AdaSpec.CLI;
