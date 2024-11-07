
:- object(p100L1,
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



:- object(aRE1,
	instantiates(re)).

	:- initialization((
		write('*** RE a + a ***'), nl,
		::show,
		::diagnostics
	)).

	expression(a + a).

:- end_object.



:- object(p100RE1,
	instantiates(re)).

	:- initialization((
		::valid,
		write('*** Second regular expression of slide 100 ***'), nl,
		::show,
		::diagnostics,
		p100L1::test_mechanism(p100RE1)
	)).

	expression((aRE1 + b * a^* * b)^*).

:- end_object.
