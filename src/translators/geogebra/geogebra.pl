:- module(geogebra, [translate/2, export/1]).
:- use_module(library(unix)).

% export(+Translation)
% Will export the translation to a readable geogebra file.
export(Translation) :-
  FinalTranslation = element(
    geogebra, [format="4.0"], [
      element(
        construction, [], Translation
      )
      ]
    ),
  % Fetch output filename
  load:option(output(Filename)),
  % Add the GGB extension to the final file
  atom_concat(Filename, '.ggb', FilenameGGB),
  % Open and write to the XML file
  open('geogebra.xml', write, Stream),
  xml_write(Stream, FinalTranslation, []),
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




translate([], []).
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
  Translation = [TranslationHead | TranslationTail].

% export_element(+ElementType, +Coords, +ElementName, -Translation)
% Helper predicate to export an element (mostly points)
export_element(ElementType, [X, Y], ElementName, Translation) :-
  % Deconstruct the element
  X = element(literal, [value=XVal], []),
  Y = element(literal, [value=YVal], []),
  Translation =
    element(element, [type=ElementType, label=ElementName],
            [
              element(show, [object=true, label=true], []),
              element(coords, [x=XVal, y=YVal, z=1], [])
            ]).

% export_command(+CommandName, +Input, +Output, -Translation)
% Helper predicate to export a single command
export_command(CommandName, Input, Output, Translation) :-
  length(Input, InputLength),
  export_input(Input, InputLength, InputTranslation),
  Translation =
    element(command, [name=CommandName], 
            [
             element(input, InputTranslation, []),
             element(output, [a0=Output], [])
            ]).

% export_input(+Input, +InputLength, -Translation)
% Helper predicate to export an input
export_input([], _, []).
export_input([InputHead | InputTail], InputLength, Translation) :-
  % Deconstruct the input
  InputHead = element(_, [value=InputValue], []),
  length(InputTail, RemainingLength),
  Index is InputLength - RemainingLength - 1,
  atom_concat(a, Index, Arg),
  export_input(InputTail, InputLength, TranslationTail),
  Translation = [Arg=InputValue | TranslationTail].

% translate_command_name(+ConstructorName, -GeogebraName)
% Predicate to translate a SolverViz constructor name to
% a geogebra command name.
translate_command_name(line, 'Line').
translate_command_name(point, 'Point').
translate_command_name(inter, 'Intersect').
