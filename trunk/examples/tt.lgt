
% Mooshak contest with 10 problems

:- initialization(
      run_examples([
          tA,
          tB,
          tC,
          tD,
          tE,
          tF,
          tG,
          tH,
          tI,
          tJ
      ])
).



:- object(evenRE,						% A solution
	instantiates(re)).

	expression((a + b * a^* * b)^*).

:- end_object.



:- object(evenFA,						% B solution
	instantiates(fa)).

	initial(1).

	transitions([1/a/1, 1/b/2, 2/a/2, 2/b/1]).

	finals([1]).

:- end_object.



:- object(evenCFG,						% C solution
	instantiates(cfg)).

	start_symbol('S').

	rules([
		('S'->['a','S']),
		('S'->['b','S','b']),
		('S'->['S','S']),
		('S'->[])
	]).

:- end_object.



:- object(evenPDA,						% D solution
	instantiates(pda)).

	initial(p).

	initial_stack_symbol(z).

	transitions([
		p/z/a/p/[z],
		p/z/b/q/[z],
		q/z/a/q/[z],
		q/z/b/p/[z]
	]).

	finals([p]).

:- end_object.



:- object(evenTM,						% E solution
	instantiates(tm)).

	initial(q0).

	transitions([
		q0/'B'/'B'/'R'/q1,
		q1/a/a/'R'/q1,     q1/b/b/'R'/q2,    q1/'B'/'B'/'R'/q3,
		q2/a/a/'R'/q2,     q2/b/b/'R'/q1,    q2/'B'/'B'/'R'/q4
	]).

	finals([q3]).

:- end_object.



:- object(paratembFA,					% F solution
	instantiates(fa)).

	initial(1).

	transitions([
		1/c/1, 1/a/2, 1/b/3,
		2/c/2, 2/a/1, 2/b/4,
		3/c/3, 3/b/3, 3/a/4,
		4/c/4, 4/b/4, 4/a/3
	]).

	finals([3]).

:- end_object.



:- object(aibjambnPDA,					% G solution
	instantiates(pda)).

	initial(p1).

	initial_stack_symbol(z).

	transitions([
		p1/z/a/p2/[a,z],
		p2/a/a/p2/[a,a], p2/a/b/p3/[],
		p3/a/b/p3/[], p3/z/b/p4/[b,z], p3/z/a/p6/[a,z],
		p4/b/b/p4/[b,b], p4/b/a/p5/[],
		p5/b/a/p5/[], p5/z/a/p6/[a,z],
		p6/a/a/p6/[a,a], p6/a/b/p7/[],
		p7/a/b/p7/[], p7/z/[]/p8/[z]
	]).

	finals([p8]).

:- end_object.



:- object(priANS,						% H solution
	instantiates(ans)).

	answer([12, 1, false, true, true]).

:- end_object.



:- object(sistemaRE,					% I solution
	instantiates(re)).

	expression((a*a + b + b*a + []) * (a*b*a*a + a*b*b + a*b*b*a + a*b + a*b*a)^*).

:- end_object.



:- object(anbnplusCFG,					% J solution
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
