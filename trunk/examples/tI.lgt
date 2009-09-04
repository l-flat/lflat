
:- object(sistemaRE,
	instantiates(re)).

	expression((a*a + b + b*a + []) * (a*b*a*a + a*b*b + a*b*b*a + a*b + a*b*a)^*).

:- end_object.



:- object(sistemaL,
	instantiates(language)).

	:- initialization(( 
		contests::setup,
		contests::diagnostics(sistemaL),
		contests::check_definition(re, sistemaRE),
		contests::diagnostics(sistemaRE),
		contests::test_mechanism(sistemaL, sistemaRE),
		contests::finish_checking
	)).

	alphabet([a,b]).

	positive([], true).
	positive([a,a], true).
	positive([b], true).
	positive([b,a], true).
	positive([a,a,a,b,a,a,a,b,b,a,b,b,a,a,b,a,b,a,a], true).
	positive([b,a,a,b,b,a,b,a,b,a,b], true).

	negative([a,a,a,b,a,b,a,b,a,b,a,b], true).
	negative([a], false).
	negative([b,b,b], false).
	negative([b,a,a], false).
	negative([a,a,b], false).
	negative([b,a,a,b,b,a,a], false).

:- end_object.
