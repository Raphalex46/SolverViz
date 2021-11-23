:- module(geogebra, [translate/2, export/1]).
:- use_module(library(unix)).

% export(+Translation)
% Will export the translation to a readable geogebra file.
export(Translation) :-
  format(
    string(FinalTranslation),
    '<geogebra format="4.0">~n\c
     <construction>~n\c
     ~w~n\c
     </construction>~n\c
     </geogebra>~n\c
    ',
    [Translation]
  ),
  % Fetch output filename
  load:option(output(Filename)),
  % Add the GGB extension to the final file
  atom_concat(Filename, '.ggb', FilenameGGB),
  % Open and write to the XML file
  open('geogebra.xml', write, Stream),
  write(Stream, FinalTranslation),
  close(Stream),
  % Finally, zip the XML to get the final file readable by Geogebra
  fork(PidZIP),
  (
    PidZIP = child -> exec(zip(FilenameGGB, 'geogebra.xml'))
  ;
    % Don't care about the status of the child for now, we just want to
    % wait.
    wait(PidZIP, _)
  ),
  % Finally, remove the temporary XML file
  fork(PidRM),
  (
    PidRM = child -> exec(rm('geogebra.xml'))
  ;
    wait(PidRM, _)
  ).




translate([], "").
translate([AffHead | AffTail], Translation) :-
  % Deconstruct the affectation
  AffHead = element(ConsName, [name=Output], Input),
  % Translate the constructor name
  translate_command_name(ConsName, GeoName),
  % If it is a point, translate into an element
  (
    GeoName = 'Point'
  ->
    export_element(point, Input, Output, TranslationHead)
  ;
    % Translate the individual command
    export_command(GeoName, Input, Output, TranslationHead)
  ),
  % Recursive call
  translate(AffTail, TranslationTail),
  string_concat(TranslationHead, TranslationTail, Translation).

% export_element(+ElementType, +Coords, +ElementName, -Translation)
% Helper predicate to export an element (mostly points)
export_element(ElementType, [X, Y], ElementName, Translation) :-
  % Deconstruct the element
  X = element(literal, [value=XVal], []),
  Y = element(literal, [value=YVal], []),
  format(
    string(Translation),
    '<element type="~w" label="~w">~n\c
    <show object="true" label="true"/>~n\c
    <coords x="~w" y="~w" z="1"/>~n\c
    </element>~n',
    [ElementType, ElementName, XVal, YVal]
  ).


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
  % Deconstruct the input
  InputHead = element(_, [value=InputValue], []),
  length(InputTail, RemainingLength),
  Index is InputLength - RemainingLength - 1,
  format(
    string(TranslationHead),
    'a~w="~w"~n',
    [Index, InputValue]
  ),
  export_input(InputTail, InputLength, TranslationTail),
  string_concat(TranslationHead, TranslationTail, Translation).

% translate_command_name(+ConstructorName, -GeogebraName)
% Predicate to translate a SolverViz constructor name to
% a geogebra command name.
translate_command_name(line, 'Line').
translate_command_name(point, 'Point').
translate_command_name(inter, 'Intersect').
