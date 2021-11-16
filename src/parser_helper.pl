:- module(parser_helper, [choice/3]).

choice([]) --> {fail}.
choice([ChoiceHead | _]) --> [ChoiceHead].
choice([_ | ChoiceTail]) --> choice(ChoiceTail).
