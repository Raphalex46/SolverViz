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
  % If it is a point, translate into an element
  translate_command_name(ConsName, GeoName),
  (
    GeoName = 'Point'
  ->
    export_element(point, Input, Output, TranslationHead)
  ;
    % Translate the individual command
    export_command(ConsName, Input, Output, TranslationHead)
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

% export_command(+OgCommandName, +Input, +Output, -Translation)
% Helper predicate to export a single command
export_command(OgCommandName, Input, Output, Translation) :-
  % Translate the command name
  translate_command_name(OgCommandName, CommandName),
  length(Input, InputLength),
  export_input(Input, InputLength, InputTranslation),
  % Check output arity and translate the output section
  command_output_arity(OgCommandName, OutputArity),
  export_output(Output, OutputArity, OutputTranslation),
  Translation =
    element(command, [name=CommandName], 
            [
             element(input, InputTranslation, []),
             element(output, OutputTranslation, [])
            ]).

% export_output(+Output, +OutputArity, -OutputTranslation)
% This predicate generate a list for multi-output commands
export_output(Output, 1, [a0=Output]).
export_output(Output, OutputArity, [OutLabel=OutMod | TransTail]) :-
  Index is OutputArity - 1,
  atom_concat(a, Index, OutLabel),
  atom_concat(Output, Index, OutMod),
  export_output(Output, Index, TransTail).


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
translate_command_name(inter_line_line, 'Intersect').
translate_command_name(inter_circle_line, 'Intersect').
translate_command_name(line_vector, 'Direction').
translate_command_name(circle_center_point, 'Circle').
translate_command_name(radius, 'Radius').
translate_command_name(line_point_vector, 'Line').
translate_command_name(line_perpendicular_vector, 'PerpendicularVector').
translate_command_name(Com, Com).

% command_output_arity(+CommandName, -Arity)
% Gives the number of output arguments of a command in Geogebra.
% defaults to 1
command_output_arity(inter_circle_line, 2).

% Default case
command_output_arity(_, 1).
