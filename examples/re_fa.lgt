
% Several manipulations involving REs and FAa

% The starting point of these examples is the second RE of slide 100,
%  but the actual manipulations performed in this file are not in the slides.

:- object(p100L,
	instantiates(language)).

	:- initialization((
		::show,
		::diagnostics
	)).

	alphabet([a,b]).

	positive([]).
	positive([a]).
	positive([a,a]).
	positive([b,b]).
	positive([b,b,b,b]).
	positive([b,a,b]).
	positive([b,a,a,b]).
	positive([b,a,a,a,b]).
	positive([b,a,a,a,b,a]).

	negative([b]).
	negative([b,a]).
	negative([a,b]).
	negative([a,a,b]).
	negative([b,b,b]).

:- end_object.



:- object(p100RE,
	instantiates(re)).

	:- initialization((
		write('*** Second regular expression of slide 100 ***'), nl,
		::show,
		::diagnostics,
		p100L::test_mechanism(p100RE),

		write('*** Simplify RE ***'), nl,
		::simplify(SRE),
		SRE::show,
		SRE::diagnostics,
		p100L::test_mechanism(SRE),

		write('*** Convert RE to non-deterministic finite automata ***'), nl,
		::fa(P100FA),
		P100FA::show,
%		P100FA::diagnostics,
		p100L::test_mechanism(P100FA),

		write('*** Convert finite automata into a deterministic one ***'), nl,
		P100FA::determine(P100FAD),
		P100FAD::show,
%		P100FAD::diagnostics,
		p100L::test_mechanism(P100FAD),

		write('*** Rename the states of a finite automata ***'), nl,
		P100FAD::rename(P100FADR),
		P100FADR::show,
%		P100FAD::diagnostics,
		p100L::test_mechanism(P100FADR),

		write('*** Minimise a finite automata ***'), nl,
		P100FADR::minimise(P100FAM),
		P100FAM::show,
%		P100FAD::diagnostics,
		p100L::test_mechanism(P100FAM),

		write('*** Rename again the states of a finite automata ***'), nl,
		P100FAM::rename(P100FAMR),
		P100FAMR::show,
%		P100FAMR::diagnostics,
		p100L::test_mechanism(P100FAMR),

		write('*** Convert FA  back to RE. The result is complex ***'), nl,
		P100FAMR::re(P100RE2),
		P100RE2::show,
%		P100RE2::diagnostics,
		p100L::test_mechanism(P100RE2)

/*
		write('*** Convert RE to composite language ***'), nl,
		::language(P100L),
		writeln(P100L),
		writeln('Composite languages are not supported in L-FLAT. Will introduce a Mix (new mechanism) in the future')
*/ 
   
	)).

	expression((a + a + b * a^* * b)^*).

:- end_object.
