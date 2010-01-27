
% This file shows how some of the examples used in the course at the
% Universidade Nova de Lisboa can be be encoded in L-FLAT. Examples are taken
% from Luís Monteiro, Formal Languages and Automata, 298 slides in Portuguese,
% 2003 which are available from "http://ctp.di.fct.unl.pt/~lm/lfa/".

:- initialization(
	run_examples([
		% regular expressions and finite automata
		re,
		re_fa0,
		re_fa,
		% finite automata	  
		s80,
		% context free grammars
		s131,
		s159,
		% pushdown automata
		s207,
		s213,
		s214,
		s218,
		s220,
		s223,
		% turing machines
		s311,
		s312,
		s315,
		% other examples		  
		alphabets,
		predicates,
		words,
		simple
	])
).
