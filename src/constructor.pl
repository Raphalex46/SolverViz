:- module(constructor, [cons_name/3, cons_arity/2]).

% Define constructor names (TODO: adapt with corpuses)
cons_name(point) --> [point].
cons_name(line) --> [line].
cons_name(inter) --> [inter].

% Define constructor arity
cons_arity(point, 2).
cons_arity(line, 2).
cons_arity(inter, 2).
