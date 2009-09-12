
:- object(evenRE,
    instantiates(re)).

	expression((a + b * a^* * b)^*).

:- end_object.



:- object(evenL,
    instantiates(language)).

    :- initialization(( 
		contests::setup,
		contests::diagnostics(evenL),
		contests::check_definition(re, evenRE),
		contests::diagnostics(evenRE),
		contests::test_mechanism(evenL,evenRE),
		contests::finish_checking
	)).

	alphabet([a,b]).

	positive([]).
	positive([a,a,a]).
	positive([b,b]).
	positive([a,b,b]).
	positive([b,b,a,a]).
	positive([b,a,a,a,b,b,a,b]).

	negative([b]).
	negative([b,b,b]).
	negative([b,a,a]).
	negative([a,a,b]).
	negative([b,a,a,b,b,a,a]).

:- end_object.
