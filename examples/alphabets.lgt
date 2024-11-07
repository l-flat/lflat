
:- object(bits1,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression([0,1]).

:- end_object.



:- object(decimal1,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression(bits1 + [2,3,4,5,6,7,8,9]).

:- end_object.



:- object(hex1,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression(decimal1 + [a,b,c,d,e,f]).

:- end_object.



:- object(letters1,
	instantiates(alphabet)).

	:- initialization((
		::show,
		::diagnostics
	)).

	expression(hex1 - decimal1).

:- end_object.
