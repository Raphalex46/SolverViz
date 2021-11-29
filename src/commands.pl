:- module(commands, [command/3]).
 
% command(?CommandName, ?InputArity, ?OutputArity)
% Command predicate that specifies all commands and their input / output arity

% Basic constructors
command(line, 2, 1).
command(point, 2, 1).
command(circle_center_point, 2, 1).
command(circle_center_radius, 2, 1).
command(line_point_vector, 2, 1).
command(vector_perpendicular_line, 1, 1).
command(line_vector, 1, 1).

% Intersection
command(inter_line_line, 2, 1).
command(inter_circle_line, 2, 2).

% "Getters"
command(line_direction, 1, 1).
command(circle_center, 1, 1).
command(circle_radius, 1, 1).
