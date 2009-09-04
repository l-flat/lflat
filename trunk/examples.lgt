
% This file shows how some of the examples used in the course at the
% Universidade Nova de Lisboa can be be encoded in P-FLAT. Examples are taken
% from Luís Monteiro, Formal Languages and Automata, 298 slides in Portuguese,
% 2003 which are available from "http://ctp.di.fct.unl.pt/~lm/lfa/".

:- initialization(
	logtalk_load([
	    s80,    % finite automata
	    s131,   % context free grammars
	    s159,
	    s207,   % pushdown automata
	    s213,
	    s214,
	    s218,
	    s220,
	    s311,   % turing machines
	    s312,
	    s315,
		alphabets,
		predicates
	], [hook(hook)])).
