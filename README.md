# SolverViz
Intermediate language representing geometric constructions, which compiles to
different graphical software file formats. Written in Prolog.

This "compiler" accepts the XML file format. The available tags are listed
in the `src/commands.pl` file.

## Requirements

In order for SolverViz to run, you need to have a prolog interpreter installed.
SWI-Prolog is recommended, because the compiler was written for, and tested with
SWI-Prolog.

## Usage

With the `swipl` command, the entry-point file of the program is `src/run.pl`,
so simply run: `swipl src/run.pl [OPTIONS] <input_file>`

Or simply use the wrapper script: `./run [OPTIONS] <input_file>`

## Available options

Run `./run --help` to have a list of the available options

## Implementing a translator

A translator in this context is a Prolog module which translates
the input XML file into a target format, that can be read by a specific
graphic software.

To implement such a translator, a directory needs to be created named after
the target backend, in the `src/translators` directory.

A file with the same name as the directory must be created inside the directory
(with the `.pl` extension added of course). This file serves as the entry
point for the translator, and defines a
[Prolog module](https://www.swi-prolog.org/pldoc/man?section=modules). Other
files in this directory aren't important.

The Prolog module needs to export the `translate/2` and `export/1` predicates.
The `translate/2` predicate translates the original XML structure into
anything that will then be exported as an output file by the `export/1`
predicate.

You can check the geogebra backend for a translator example.
