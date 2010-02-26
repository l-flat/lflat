:- object(anbnplusCFG,
	instantiates(cfg)).

	start_symbol('S').

	rules([
		('S'->['T']),
		('S'->['A']),
		('S'->['B']),
		('T'->[a,b]),
		('T'->[a,'T',b]),
		('A'->[]),
		('A'->[a, 'A']),
		('B'->[]),
		('B'->[b, 'B'])
	]).  
:- end_object.

