:- module(geogebra, [translate/2]).

translate([], "").
translate([AffHead | AffTail], Translation) :-
  % Deconstruct the affectation
  AffHead = aff(Output, cons(ConsName, Input)),
  % Translate the constructor name
  translate_command_name(ConsName, GeoName),
  % Translate the individual command
  export_command(GeoName, Input, Output, TranslationHead),
  % Recursive call
  translate(AffTail, TranslationTail),
  string_concat(TranslationHead, TranslationTail, Translation).

% export_command(+CommandName, +Input, +Output, -Translation)
% Helper predicate to export a single command
export_command(CommandName, Input, Output, Translation) :-
  length(Input, InputLength),
  export_input(Input, InputLength, InputTranslation),
  format(
    string(Translation),
    '<command name="~w">~n\c
    <input~n\c
    ~w\c
    />~n\c
    <output a0="~w"/>~n\c
    </command>~n',
    [CommandName, InputTranslation, Output]
  ).

% export_input(+Input, +InputLength, -Translation)
% Helper predicate to export an input
export_input([], _, "").
export_input([InputHead | InputTail], InputLength, Translation) :-
  length(InputTail, RemainingLength),
  Index is InputLength - RemainingLength - 1,
  format(
    string(TranslationHead),
    'a~w="~w"~n',
    [Index, InputHead]
  ),
  export_input(InputTail, InputLength, TranslationTail),
  string_concat(TranslationHead, TranslationTail, Translation).

% translate_command_name(+ConstructorName, -GeogebraName)
% Predicate to translate a SolverViz constructor name to
% a geogebra command name.
translate_command_name(line, 'Line').
translate_command_name(point, 'Point').
translate_command_name(inter, 'Intersect').
