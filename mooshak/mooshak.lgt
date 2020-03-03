

:- object(ans,
	instantiates(class),
	specializes(mechanism)).

	:- info([
		version is 2:0:1,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Answer to direct questions.']).

	:- public([message/4,
				fail_with_warning/3, fail_with_error/3, halt_with_error/3]).

	:- public([answer/1, test_mechanism/1]).

	:- uses(list, [valid/1::is_list/1]).

	valid :-
		::answer(Answer),
		is_list(Answer).

	diagnostic(Diagnostic) :-
		::answer(Answer),
		\+ is_list(Answer),
		::build_error(Answer, 'is not a list', Diagnostic).

	test_mechanism(Mechanism) :-
		self(Self),
		write('Starting tests of '), write(Mechanism),
		write(' against '), write(Self), write(' ...'), nl,
		fail.
	test_mechanism(Attempt) :-
		::answer(Answer),
		Attempt::answer(Answer2),
		Answer \== Answer2,
		::fail_with_error(Attempt, 'wrong answer', '').
	test_mechanism(_) :-
		write('... tests finished'), nl.

:- end_object.



:- object(contests,
	imports(messages)).

	:- info([
		version is 2:0:1,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Automated contests.']).

	:- public([	setup/0,
				check_definition/2,
				diagnostics/1,
				test_mechanism/2,
				check_deterministic/1,
				compile/2,
				execute/2,
				finish_checking/0
			]).

	setup :-
		modes::set_warning_mode(low),
		modes::set_conceal_mode(on),
		modes::set_error_mode(off).

	check_definition(MechanismType, Mechanism) :-
	% check existence
		(	current_object(Mechanism) ->
			true
		;	::halt_with_error(Mechanism, 'missing definition', '')
		),
	% check type
		( instantiates_class(Mechanism, MechanismType) ->
			write(Mechanism), write(' is well defined'), nl
		;	::halt_with_error(Mechanism, 'should have been defined as a', MechanismType)
		).

	diagnostics(Mechanism) :-
		Mechanism::diagnostics,
		(	modes::current_error_mode(on) ->
			halt
		;	true
		).

	test_mechanism(Language, Mechanism) :-
		Language::test_mechanism(Mechanism).

	check_deterministic(Mechanism) :-
		(	Mechanism::current_predicate(deterministic/0) ->
			(	Mechanism::deterministic ->
				write(Mechanism), write(' is deterministic'), nl
			;	::fail_with_error(Mechanism, 'ought to be deterministic', '')
			)
		;	::fail_with_warning(Mechanism, 'does not have a deterministic property', '')
		).

	compile(Submission, _) :-
		::setup,
		logtalk_load(Submission, [hook(hook)]), % just for syntax checking
		halt.

	execute(Submission, Test) :-
		::setup,
		logtalk_load(Submission, [hook(hook)]), % loads the submission
		logtalk_load(Test),				% and runs one input test over it
		halt.

	finish_checking :-
		write('Finished checking'), nl.

:- end_object.


