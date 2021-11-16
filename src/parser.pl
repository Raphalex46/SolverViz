:- module(parser, [parse/3]).

:- use_module(constructor).

parse(AffList) --> aff_list(AffList).

% List of affectations
aff_list([]) --> [].
aff_list([AffHead | AffTail]) --> affect(AffHead), ['.'], aff_list(AffTail).

% List of parameters
param_list([]) --> [].
param_list([Param]) --> param(Param).
param_list([ParamHead | ParamTail]) -->
  param(ParamHead), [','], param_list(ParamTail).

% Affectation
affect(aff(Name, Cons)) --> [Name, '='], cons(Cons).

% Constructor call
cons(cons(Name, Params)) --> cons_name(Name), ['('], param_list(Params), [')'].

% Single parameters
param(Param) --> [Param].
