--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO; use Ada.Text_IO;

package body AdaSpec.CLI is

   procedure Help is
   begin

      pragma Style_Checks (Off);

      --         0         10        20        30        40        50        60        70
      --         ---------------------------------------------------------------------------XXX

      Put_Line ("adaspec - generate tests from specification");
      Put_Line ("");
      Put_Line ("SYNOPSIS");
      Put_Line ("");
      Put_Line ("    adaspec [OPTIONS] FEATURE ...");
      Put_Line ("");
      Put_Line ("DESCRIPTION");
      Put_Line ("");
      Put_Line ("    AdaSpec takes as input feature files describing specifications and");
      Put_Line ("    generate tests for these features in languages such as Ada, C, C++.");
      Put_Line ("");
      Put_Line ("    Specifications are described in a formalized natural language (see");
      Put_Line ("    cucumber documentation) and the sentences are translated in code using");
      Put_Line ("    using steps. These steps define a relation between the regular");
      Put_Line ("    expression matching the sentance and a function to perform what the");
      Put_Line ("    sentance describes.");
      Put_Line ("");
      Put_Line ("OPTIONS");
      Put_Line ("");
      Put_Line ("    FEATURE");
      Put_Line ("        The .feature file containing cucumber-like specification. It is");
      Put_Line ("        possible to specify multiple features on the command-line all of");
      Put_Line ("        them will be processed.");
      Put_Line ("");
      Put_Line ("    -h, -help, --help");
      Put_Line ("        Help message");
      Put_Line ("");
      Put_Line ("    -s, --step STEPDIR");
      Put_Line ("        Specify a step directory.  By default, the step/ directory relative");
      Put_Line ("        to each feature file specified is searched.");
      Put_Line ("");
      Put_Line ("    -k, --keep-going");
      Put_Line ("        Keep going if there are errors for a feature and compile the next");
      Put_Line ("        feature.");
      Put_Line ("");
      Put_Line ("    --fill-steps");
      Put_Line ("        Replace @todo tags with a function skeleton that implement the");
      Put_Line ("        step.  With this switch, you can omit the features files but you");
      Put_Line ("        have to specify a step definition directory using --step.");
      Put_Line ("");
      Put_Line ("    -x, --executable TEST_NAME");
      Put_Line ("        Create a test program called TEST_NAME that will be able to run all");
      Put_Line ("        the specified features.  If not present, only test packages are");
      Put_Line ("        created.  The TEST_NAME is relative to the output directory");
      Put_Line ("        specified with the -o (--output-dir) option.");
      Put_Line ("");
      Put_Line ("    -o, --output DIR");
      Put_Line ("        Choose in which directory the files will be generated. The test");
      Put_Line ("        packages have the same name as the features.  By default the output");
      Put_Line ("        directory is the tests/ directory relative to the feature file.");
      Put_Line ("");
      Put_Line ("    -l, --lang LANG");
      Put_Line ("        Choose wich language to generate.");
      Put_Line ("        Available languages: Ada");
      Put_Line ("");
      Put_Line ("    --progress");
      Put_Line ("        Show progress.");
      Put_Line ("");
      Put_Line ("    --partial");
      Put_Line ("        Don't generate anything.");
      Put_Line ("");
      Put_Line ("-- ");
      Put_Line ("Copyright (c) 2010 SOGILIS");

      --         ---------------------------------------------------------------------------XXX
      --         0         10        20        30        40        50        60        70

      pragma Style_Checks (On);

   end Help;

end AdaSpec.CLI;
