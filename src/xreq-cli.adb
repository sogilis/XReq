-------------------------------------------------------------------------------
--  XReq  --  Behaviour Driven Developpement tool for compiled languages     --
--  Copyright (c) 2010, SOGILIS <http://sogilis.com>                         --
--                                                                           --
--  This program is free software: you can redistribute it and/or modify     --
--  it under the terms of the GNU Affero General Public License as           --
--  published by the Free Software Foundation, either version 3 of the       --
--  License, or (at your option) any later version.                          --
--                                                                           --
--  This program is distributed in the hope that it will be useful,          --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of           --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            --
--  GNU Affero General Public License for more details.                      --
--                                                                           --
--  You should have received a copy of the GNU Affero General Public License --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.    --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

package body XReq.CLI is

   procedure Help is
   begin

      pragma Style_Checks (Off);

      --         0         10        20        30        40        50        60        70
      --         ---------------------------------------------------------------------------XXX

      Put_Line ("xreq - generate tests from specification");
      Put_Line ("");
      Put_Line ("SYNOPSIS");
      Put_Line ("");
      Put_Line ("    xreq [OPTIONS] FEATURE ...");
      Put_Line ("");
      Put_Line ("DESCRIPTION");
      Put_Line ("");
      Put_Line ("    XReq takes as input feature files describing specifications and");
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
      Put_Line ("    -V, --version");
      Put_Line ("        Show version");
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
      Put_Line ("    --fill-steps-in PACKAGE");
      Put_Line ("        Same as --fill-steps except that steps that were not found in the");
      Put_Line ("        feature files will be inserted in the step package PACKAGE and");
      Put_Line ("        filled in with a procedure skeleton.");
      Put_Line ("");
      Put_Line ("    -x, --executable TEST_NAME");
      Put_Line ("        Create a test program called TEST_NAME that will be able to run all");
      Put_Line ("        the specified features.  If not present, only test packages are");
      Put_Line ("        created.  The TEST_NAME is relative to the output directory");
      Put_Line ("        specified with the -o (--output-dir) option.");
      Put_Line ("        .");
      Put_Line ("        ATTENTION: For the Ada language, this option will remove every");
      Put_Line ("                   other .adb and .ads file in the output directory. This");
      Put_Line ("                   is necessary for the GPR project file to work.");
      Put_Line ("        .");
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
      Put_Line ("    -m, --make");
      Put_Line ("        Build generated files (using gnatmake for Ada language).");
      Put_Line ("        Require option -x, --executable.");
      Put_Line ("");
      Put_Line ("    -q, --quiet");
      Put_Line ("        Display minimum output only.");
      Put_Line ("");
      Put_Line ("    -c NAME=VALUE");
      Put_Line ("        Specify an additional configuration option NAME with the value");
      Put_Line ("        VALUE. See below for the documentation of those options. If the");
      Put_Line ("        same option is specified twice, the first value is discarded.");
      Put_Line ("");
      Put_Line ("    --progress");
      Put_Line ("        Show progress.");
      Put_Line ("");
      Put_Line ("    --partial");
      Put_Line ("        Don't generate anything.");
      Put_Line ("");
      Put_Line ("    --step-matching");
      Put_Line ("        Show matches with step definitions");
      Put_Line ("");
      Put_Line ("CONFIGURATION OPTIONS");
      Put_Line ("");
      Put_Line ("    These configurations options are specified on the command line using");
      Put_Line ("    the scitch -c with an argument of the form NAME=VALUE. If an option is");
      Put_Line ("    not recognized, it is silently ignored. Possible options are:");
      Put_Line ("");
      Put_Line ("    ada.gpr.with");
      Put_Line ("        These option specify an additional library to include in the GPR");
      Put_Line ("        project file. Separate the libraries by commas ','.");
      Put_Line ("");
      Put_Line ("    ada.gpr.srcdir");
      Put_Line ("        These option specify an additional source path in the GPR project.");
      Put_Line ("        Separate the directories by commas ','.");
      Put_Line ("");
      Put_Line ("ENVIRONMENT VARIABLES");
      Put_Line ("");
      Put_Line ("    GNAT_FLAGS");
      Put_Line ("        Additional flags to pass to gnatmake");
      Put_Line ("");
      Put_Line ("-- ");
      Put_Line ("Copyright (c) 2010 SOGILIS");

      --         ---------------------------------------------------------------------------XXX
      --         0         10        20        30        40        50        60        70

      pragma Style_Checks (On);

   end Help;

   procedure Version is
   begin
      Put_Line ("XReq version: " & Constants.Version);
   end Version;

end XReq.CLI;
