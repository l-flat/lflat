:- object(evenCFG,
	instantiates(cfg)).

	start_symbol('S').

	rules([
		('S'->['a','S']),
		('S'->['b','S','b']),
		('S'->['S','S']),
		('S'->[])
	]).

:- end_object.
