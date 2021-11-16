:- use_module(library(readutil)).
:- use_module(library(porter_stem)).

:- use_module(load).
:- use_module(parser).

main :-
  parse_command_line(Opts, PositionalArgs),
  assert_options(Opts),

  % Load optspecs
  load:optspecs(OptSpecs),
  % Handle help flag
  (
    option(help(false))
  ;
    opt_help(OptSpecs, Help),
    write(Help),
    halt
  ),
  (
    option(version(false))
  ;
    % Fetch and display version
    load:version(Version),
    write('SolverViz version '), write(Version), nl,
    halt
  ),

  % Check number of positional arguments
  (
    length(PositionalArgs, 1)
  ;
    print_usage_and_halt
  ), !,

  % Read input file
  PositionalArgs = [Filename],
  read_file_to_string(Filename, InputString, []),
  % Tokenize input string
  tokenize_atom(InputString, TokenList),
  % Call translator
  (
    phrase(parse(AffList), TokenList)
  ;
    write('Compilation error. Aborting'), nl, fail
  ),
  write(AffList), nl,
  halt.

print_usage_and_halt :-
  write('Usage: solverviz [options] <input_file>'), nl, halt.

:- main.
