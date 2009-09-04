
:- object(bits,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression([0,1]).

:- end_object.



:- object(decimal,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression(bits + [2,3,4,5,6,7,8,9]).

:- end_object.



:- object(hex,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression(decimal + [a,b,c,d,e,f]).

:- end_object.



:- object(letters,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression(hex - decimal).

:- end_object.
