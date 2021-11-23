:- module(load, [parse_command_line/2, assert_options/1, option/1, version/1]).
:- use_module(library(optparse)).

% version predicate
version('0.2.2').

% option(Option)
:- dynamic option/1.

% List of available options
optspecs(
  [
    [
      opt(translator),
      type(atom),
      shortflags([t]),
      longflags([translator]),
      default(geogebra),
      help('Select the translator (backend) to be used')
    ],
    [
      opt(output),
      type(atom),
      shortflags([o]),
      longflags([output]),
      default(output),
      help('Specify output file')
    ],
    [
      opt(version),
      type(boolean),
      shortflags([v]),
      longflags([version]),
      default(false),
      help('Print version number and exit')
    ],
    [
      opt(help),
      type(boolean),
      shortflags([h]),
      longflags([help]),
      default(false),
      help('Print this help text and exit')
    ]
  ]
).

% parse_command_line(Opts, PositionalArgs).
% Parse the command line to get a list of options and positional arguments
parse_command_line(Opts, PositionalArgs) :-
  % Fetch optspects
  optspecs(OptSpec),
  current_prolog_flag(argv, Argv),
  opt_parse(OptSpec, Argv, Opts, PositionalArgs).

% assert_options(Opts).
% This predicate asserts all options with the dynamic predicate 'option(Option)'

assert_options([]).
assert_options([OptHead | OptTail]) :-
  assertz(option(OptHead)),
  assert_options(OptTail).
