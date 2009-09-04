
% Slide 159: finding all nonterminals that generate lambda in a grammar

:- object(cfg159,
	instantiates(cfg)).

	:- initialization((
		write('*** Context free grammar of slide 159 ***'), nl,
		::show,
		::diagnostics,
		write('The nonterminals that generate lambda: '),
		::symbols_generating_lambda(L), writeq(L), nl
	)).

	start_symbol('S').

	rules([
		('S'->[a,'A',b]),
		('S'->[b,'A',a]),
		('A'->['B','C']),
		('A'->[a,'B']),
		('A'->['C',b]),
		('B'->[]),
		('B'->[a,'B']),
		('C'->[]),
		('C'->['C',b])
	]).

:- end_object.
