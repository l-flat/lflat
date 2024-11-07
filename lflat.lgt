%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                               %
% lflat.lgt - the Logtalk Formal Language and Automata Toolkit	%
%                                                               %
%  - Artur Miguel Dias                                          %
%	(CITI, Depart. de Informatica, Univ. Nova de Lisboa)        %
%  - Paulo Moura                                                %
%	(CRACS, INESC Porto)                                        %
%  - Michel Wermelinger                                         %
%	(Computing Department, The Open University)                 %
%                                                               %
% L-FLAT, version 2.1.1                                         %
% 23/May/2022                                                   %
%                                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (TABSTOPS = 4)

/*
Changelog (sumary):

	* Fix loading of the gensym library.
	* Fix name conflicts when loading all the examples.

23/May/2022 -- L-FLAT version 2.1.1 (Paulo Moura, A. Miguel Dias)
	* Fix linter warning.

11/Oct/2021 -- L-FLAT version 2.1.0 (Paulo Moura, A. Miguel Dias)
	* Don't use a settings file to simplify making L-FLAT avaialble as a pack.

05/Feb/2021 -- L-FLAT version 2.0.2 (Paulo Moura, A. Miguel Dias)
	* Fix linter warnings.

03/Mar/2020 -- L-FLAT version 2.0.1 (Paulo Moura, A. Miguel Dias)
	* Update for Logtalk 3.36.0, which is now the minimum version required.

15/Aug/2009 -- L-FLAT version 2.0 (Paulo Moura, A. Miguel Dias)
	* The entire application was rewritten in Logtalk. Virtually all the
	  same functionality has been kept. Now making the aplication evolve
	  will be easier due to the advantages of Logtalk.

25/Mar/2007 -- P-FLAT version 1.7 (A. Miguel Dias)
	* Made the application more conformant to the ISO Prolog standard.

05/Jul/2006 -- P-FLAT version 1.6 (A. Miguel Dias)
	* Introduced support for automated contexts using Mooshak, the programming
	  contests management system.

10/Mar/2006 -- P-FLAT version 1.5 (A. Miguel Dias)
	* Introduced "overloaded operations", operations that are universal,
	  each kind of entity supplying its own implementation. As now
	  the overloaded operations are:
	    alpha/2, show/1, accept/2, accept/3, word/2, tracing/2.
	* Introduced "generic predicates" that implement generic algorithms
	  applicable to all kinds of entities. Some types of entities use
	  the generic predicates, some others provide more efficient
	  customised implementations. As now the generic predicates are:
	    generic_accept/3, generic_accept/2, generic_tracing/2,
	    generic_word/2.
	* Tracing was completely revised and is now available to all the
	  mechanisms: PREDs, REs, FAs, CFGs, PDAs and TMs
	* accept/2 and accept/3 can now handle non-deterministic mechanisms,
	  with the help of the breath-first strategy. Loop detection is
	  also implemented to allow some endless executions paths to be
	  pruned: this helps when trying to reject words and also improves
	  eficiency. accept/2 and accept/3 are semi-algorithms.
	* Now show/1 presents more information, e.g. whether a mechanism is
	  deterministic.
	* Introduced the auxiliary predicate cfg_valid_config/2 to help
	  reducing the combinatorial explosion in the breath-first
	  generation of new configurations. This concerns CFGs only.
	* The predicates enter/0, execute/1 and execute/3 were promoted
	  from the file "tutorial.pl" to the main source code file of
	  P-FLAT. They are very useful to write P-FLAT scripts, and from
	  now on, the "script" will be considered an important concept
	  in P-FLAT.

30/Jun/2005 -- P-FLAT version 1.0 (Michel Wermelinger, A. Miguel Dias)
	* Initial release, corresponding to the ITiCSE'2005 paper.
*/


%%%%%%%%%%%%
% CONCEPTS %
%%%%%%%%%%%%
/*
A LANGUAGE is partially defined using "unit tests" that is using a set of
positive and a set of negative test cases.

A MECHANISM is an attempt to implement a particular language, either using
a generator or a recognizer. The supported mechanisms are: PREDICATE, RE,
FA, CFG, PDA, TM.

AUTOMATED CONTESTS are implemented for the Mooshak automatic judging manager.
Mooshak is a WEB application that suports automatic judging of submitted programs.

The entities that can have a composite form are the following: ALPHABET, WORD,
RE, MIX. MIX is yet to be implemented.
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Back-end Prolog compiler configuration %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Allows long lists and large terms to be completely displayed in SWI-Prolog
% (no "..." abbreviations will be used)

:- if(current_logtalk_flag(prolog_dialect, swi)).

	% bypass the compiler to avoid a portability warning on the flag
	{:- set_prolog_flag(toplevel_print_options, [max_depth(200)])}.

:- endif.


% Allows missing/not imported predicates to be discovered

:- if(\+ current_logtalk_flag(prolog_dialect, qp)).

	:- set_prolog_flag(unknown, error).

:- endif.


:- op(300, xf, ^*).
:- op(300, xf, ^+).



:- object(modes).

	:- info([
		version is 2:1:1,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2021-05-23,
		comment is 'Flags defining application modes for warning and error reporting.']).

	:- public(set_warning_mode/1).
	:- mode(set_warning_mode(+atom), one).
	:- info(set_warning_mode/1, [
		comment is 'Sets warning mode.',
		argnames is ['Mode']]).

	:- public(current_warning_mode/1).
	:- mode(current_warning_mode(?atom), one).
	:- info(current_warning_mode/1, [
		comment is '.',
		argnames is ['Mode']]).

	:- public(set_conceal_mode/1).
	:- mode(set_conceal_mode(+atom), one).
	:- info(set_conceal_mode/1, [
		comment is 'Sets warnings mode.',
		argnames is ['Mode']]).

	:- public(current_conceal_mode/1).
	:- mode(current_conceal_mode(?atom), one).
	:- info(current_conceal_mode/1, [
		comment is '.',
		argnames is ['Mode']]).

	:- public(set_error_mode/1).
	:- mode(set_error_mode(+atom), one).
	:- info(set_error_mode/1, [
		comment is 'Sets error mode.',
		argnames is ['Mode']]).

	:- public(current_error_mode/1).
	:- mode(current_error_mode(?atom), one).
	:- info(current_error_mode/1, [
		comment is '.',
		argnames is ['Mode']]).

	:- private(warning_mode_/1).	% default is high
	:- dynamic(warning_mode_/1).
	warning_mode_(high).

	set_warning_mode(Mode) :-
		once((Mode == off ; Mode == low ; Mode == high)),
		retractall(warning_mode_(_)),
		assertz(warning_mode_(Mode)).

	current_warning_mode(Mode) :-
		warning_mode_(Mode).

	:- private(conceal_mode_/1).	% default is off
	:- dynamic(conceal_mode_/1).
	conceal_mode_(off).

	set_conceal_mode(Mode) :-
		once((Mode == off ; Mode == on)),
		retractall(conceal_mode_(_)),
		assertz(conceal_mode_(Mode)).

	current_conceal_mode(Mode) :-
		conceal_mode_(Mode).

	:- private(error_mode_/1).		% default is off
	:- dynamic(error_mode_/1).
	error_mode_(off).

	set_error_mode(Mode) :-
		once((Mode == off ; Mode == on)),
		retractall(error_mode_(_)),
		assertz(error_mode_(Mode)).

	current_error_mode(Mode) :-
		error_mode_(Mode).

:- end_object.



:- object(interaction).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Predicates for command-line user interaction.']).

	:- public(banner/0).
	:- mode(banner, one).
	:- info(banner/0, [
		comment is 'Prints the L-FLAT banner.']).

	:- public(run_example/1).
	:- mode(run_example(+atom), one).
	:- info(run_example/1, [
		comment is 'Loads and runs an example.',
		argnames is ['Example']]).

	:- public(run_examples/1).
	:- mode(run_examples(+list(atom)), one).
	:- info(run_examples/1, [
		comment is 'Loads and runs a list of examples.',
		argnames is ['Examples']]).

	banner :-
		write('L-FLAT 2.1.0 - the Logtalk Formal Language and Automata Toolkit'), nl,
		write('Copyright (c) 2005-2021 Artur Miguel Dias, Paulo Moura, Michel Wermelinger'), nl, nl.

	run_example(Example) :-
		logtalk_load(lflat_examples(Example), [hook(hook)]).

	run_examples([]).
	run_examples([X|Xs]) :-
		run_example(X),
		separator,
		enter,
		nl, nl,
		separator,
		run_examples(Xs).

	enter :-
		nl, write('Press ENTER to continue'), nl, get_code(_).

	separator :-
		write('------------------------------------'), nl.

:- end_object.



:- category(messages).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Utility predicates for composing and writing messages.']).

	:- public([
		message/4,
		fail_with_warning/3,
		fail_with_error/3,
		halt_with_error/3
	]).

	:- public(execute/1).
	:- meta_predicate(execute(0)).
	:- mode(execute(+callable), one).
	:- info(execute/1, [
		comment is 'Calls a goal, printing "Yes" or "No".',
		argnames is ['Goal']]).

	:- public(execute/3).
	:- meta_predicate(execute(0, *, *)).
	:- mode(execute(+callable, +atom, +callable), one).
	:- info(execute/3, [
		comment is 'Calls a goal Goal, writing result Result until Goal fails or stop condition Stop succeeds.',
		argnames is ['Goal', 'Result', 'Stop']]).

	:- public(write_symbol_list/2).
	:- mode(write_symbol_list(+list, +atom), one).
	:- info(write_symbol_list/2, [
		comment is 'Writes a sequence of symbols (starting with a prefix).',
		argnames is ['Symbols', 'Prefix']]).

	:- uses(list, [member/2]).

	execute(G, R, S) :-
		write('?- '), write(G), write('.'), nl, call(G),
		(	R == none ->
			true
		;	write(R), write(' ;'), nl
		),
		call(S), write('Yes'), nl, !.
	execute(_, _, _) :-
		write('No'), nl.

	execute(Goal) :-
		execute(Goal, none, true).

	write_symbol_list([], _).
	write_symbol_list([Symbol| Symbols], Prefix) :-
		write(Prefix), write(Symbol),
		write_symbol_list(Symbols).

	write_symbol_list([]).
	write_symbol_list([Symbol| Symbols]) :-
		write(' '), write(Symbol),
		write_symbol_list(Symbols).

	% warnings and errors

	writerr(Term) :-
		write(user_error, Term).
	nlerr :-
		nl(user_error).

	message_body(Name, Msg1, Msg2) :-
		writerr(Name),
		writerr(': '),
		writerr(Msg1),
		writerr(' '),
		writerr(Msg2),
		nlerr.

	message(weak_warning, Name, Msg1, Msg2) :-
		(	modes::current_warning_mode(high) ->
			writerr('Warning in '),
			message_body(Name, Msg1, Msg2)
		;	true
		).
	message(warning, Name, Msg1, Msg2) :-
		(	modes::current_warning_mode(off) ->
			true
		;	writerr('Warning in '),
			message_body(Name, Msg1, Msg2)
		).
	message(error, Name, Msg1, Msg2) :-
		modes::set_error_mode(on),
		writerr('Error in '),
		message_body(Name, Msg1, Msg2).

	fail_with_warning(Name, Msg1, Msg2) :-
		message(warning, Name, Msg1, Msg2),
		fail.

	fail_with_error(Name, Msg1, Msg2) :-
		message(error, Name, Msg1, Msg2),
		fail.

	halt_with_error(Name, Msg1, Msg2) :-
		message(error, Name, Msg1, Msg2),
		halt.

:- end_category.



:- object(term_classification).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Classification of terms as valid elements for sets, valid names for declarations, etc.']).

	:- public(is_element/1).
	:- mode(is_element(@term), zero_or_one).
	:- info(is_element/1, [
		comment is 'Term is a valid element for a set.',
		argnames is ['Term']]).

	:- public(is_symbol/1).
	:- mode(is_symbol(@term), zero_or_one).
	:- info(is_symbol/1, [
		comment is 'Symbol is a symbol that can be used in an alphabet.',
		argnames is ['Symbol']]).

	:- public(is_lambda/1).
	:- mode(is_lambda(@term), zero_or_one).
	:- info(is_lambda/1, [
		comment is 'Lambda is used in the representation of lambda transitions.',
		argnames is ['Lambda']]).

	:- public(is_state/1).
	:- mode(is_state(@term), zero_or_one).
	:- info(is_state/1, [
		comment is 'State is a valid name for a state.',
		argnames is ['State']]).

	:- uses(term, [ground/1]).

	is_element(Element) :-
		ground(Element).

	is_symbol(Symbol) :-
		atomic(Symbol),
		Symbol \== [],
		Symbol \== {},
		(	atom(Symbol) ->
			\+ current_object(Symbol)
		;	true
		).

	is_lambda(Lambda) :-
		Lambda == [].

	is_empty_re(EmptyRE) :-
		EmptyRE == {}.

	is_state(State) :-
		ground(State).

:- end_object.



:- object(entity,
	imports(messages),
	instantiates(abstract_class),
	specializes(object)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Predicates common to all entities.']).

	:- public(show/0).
	:- mode(show, one).
	:- info(show/0, [
		comment is 'Prints a description of the declaration.']).

	:- public(valid/0).
	:- mode(valid, zero_or_one).
	:- info(valid/0, [
		comment is 'Declaration is valid.']).

	:- public(diagnostics/0).
	:- mode(diagnostics, one).
	:- info(diagnostics/0, [
		comment is 'Prints all diagnostics.']).

	:- public(diagnostic/1).
	:- mode(diagnostic(-compound), zero_or_more).
	:- info(diagnostic/1, [
		comment is 'Diagnostic represents a problem diagnostic.',
		argnames is ['Diagnostic']]).

	:- protected(build_error/3).
	:- protected(build_warning/3).
	:- protected(build_weak_warning/3).

	build_error(Part1, Part2, error(Declaration, Part1, Part2)) :-
		self(Declaration).

	build_warning(Part1, Part2, warning(Declaration, Part1, Part2)) :-
		self(Declaration).

	build_weak_warning(Part1, Part2, weak_warning(Declaration, Part1, Part2)) :-
		self(Declaration).

	diagnostics :-
		self(Self),
		write('Starting diagnostics of '), write(Self), write(' ...'), nl,
		::diagnostic(Diagnostic),
		Diagnostic =.. [Kind, Name, Msg1, Msg2],
		::message(Kind, Name, Msg1, Msg2),
		fail.
	diagnostics :-
		write('... diagnostics finished'), nl.

:- end_object.



:- object(meta_alphabet,
	specializes(class)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Metaclass of alphabet.']).

	:- public(equal/2).
	:- mode(equal(@alphabet, @alphabet), zero_or_one).
	:- info(equal/2, [
		comment is 'The two alphabet expressions define the same set of symbols.',
		argnames is ['Alphabet1', 'Alphabet2']]).

	:- public(contains/2).
	:- mode(contains(@alphabet, @alphabet), zero_or_one).
	:- info(contains/2, [
		comment is 'The first alphabet contains the symbols of the second alphabet.',
		argnames is ['Alphabet1', 'Alphabet2']]).

	:- public(new/2).
	:- mode(new(?object_identifier, +nonvar), one).
	:- info(new/2, [
		comment is 'Creates a new alphabet.',
		argnames is ['Id', 'Expression']]).

	equal(Alphabet1, Alphabet2) :-
		alphabet(Alphabet1)::symbols(Symbols),
		alphabet(Alphabet2)::symbols(Symbols).

	contains(Alphabet1, Alphabet2) :-
		alphabet(Alphabet1)::symbols(Symbols1),
		alphabet(Alphabet2)::symbols(Symbols2),
		set::subset(Symbols2, Symbols1).

	new(Id, Expression) :-
		self(Self),
		create_object(Id, [instantiates(Self)], [], [expression(Expression)]).

:- end_object.



:- object(alphabet,
	instantiates(meta_alphabet),
	specializes(entity)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Alphabets.']).

	:- public(expression/1).
	:- mode(expression(-nonvar), one).
	:- info(expression/1, [
		comment is 'Alphabet expression.',
		argnames is ['Expression']]).

	:- public(symbols/1).
	:- mode(symbols(-list), one).
	:- info(symbols/1, [
		comment is 'Alphabet evaluates to symbol set Symbols.',
		argnames is ['Symbols']]).

	:- uses(list, [valid/1::is_list/1, permutation/2, sort/2]).
	:- uses(term_classification, [is_symbol/1]).
	:- uses(set, [valid/1::is_set/1, member/2, union/3, intersection/3, symdiff/3]).

	show :-
		write('ALPHABET: '),
		self(Self),
		writeq(Self), nl,
		write('  Expression: '),
		::expression(Expression),
		writeq(Expression), nl,
		write('  Symbols: '),
		::symbols(Symbols),
		writeq(Symbols), nl.

	valid :-
		::expression(Expression),
		valid(Expression).

	valid((-)) :-
		!,
		fail.
	valid([]) :-
		!.
	valid([Symbol| Symbols]) :-
		!,
		\+ (member(Element, [Symbol| Symbols]), \+ is_symbol(Element)),
		is_set([Symbol| Symbols]).
	valid(Alphabet1 + Alphabet2) :-
		!,
		valid(Alphabet1),
		valid(Alphabet2).
	valid(Alphabet1 - Alphabet2) :-
		!,
		valid(Alphabet1),
		valid(Alphabet2).
	valid(Alphabet1 /\ Alphabet2) :-
		!,
		valid(Alphabet1),
		valid(Alphabet2).
	valid(Alphabet) :-
		Alphabet::expression(Expression),
		valid(Expression).

	diagnostic(Diagnostic) :-
		::expression(Expression),
		\+ valid(Expression),
		::build_error(Expression, 'is not an alphabet', Diagnostic).
	diagnostic(Diagnostic) :-
		::symbols([]),
		::build_warning('empty alphabet', '', Diagnostic).

	symbols(Symbols) :-
		::expression(Expression),
		symbols(Expression, Symbols).

	symbols(List, Symbols) :-
		is_list(List),
		!,
		sort(List, Symbols).
	symbols(Alphabet1 + Alphabet2, Symbols) :-
		!,
		symbols(Alphabet1, Symbols1),
		symbols(Alphabet2, Symbols2),
		union(Symbols1, Symbols2, Symbols).
	symbols(Alphabet1 - Alphabet2, Symbols) :-
		!,
		symbols(Alphabet1, Symbols1),
		symbols(Alphabet2, Symbols2),
		symdiff(Symbols1, Symbols2, Symbols).
	symbols(Alphabet1 /\ Alphabet2, Symbols) :-
		!,
		symbols(Alphabet1, Symbols1),
		symbols(Alphabet2, Symbols2),
		intersection(Symbols1, Symbols2, Symbols).
	symbols(Alphabet, Symbols) :-
		Alphabet::symbols(Symbols).

:- end_object.



:- object(meta_order,
	specializes(class)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Metaclass of order.']).

	:- public(new/3).
	:- mode(new(?object_identifier, +nonvar, +list), one).
	:- info(new/3, [
		comment is 'Creates a new order over an alphabet.',
		argnames is ['Id', 'Alphabet', 'Sequence']]).

	new(Id, Expression, Sequence) :-
		self(Self),
		create_object(Id, [instantiates(Self)], [], [alphabet(Expression), sequence(Sequence)]).

:- end_object.



:- object(order,
	implements(comparingp),
	instantiates(meta_order),
	specializes(entity)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Total order over an alphabet.']).

	:- public(alphabet/1).
	:- mode(alphabet(-nonvar), one).
	:- info(alphabet/1, [
		comment is 'Alphabet expression.',
		argnames is ['Expression']]).

	:- public(sequence/1).
	:- mode(sequence(-list), one).
	:- info(sequence/1, [
		comment is 'The default total alphabet order is specified by Sequence.',
		argnames is ['Sequence']]).

	:- uses(list, [nth0/3]).
	:- uses(set, [intersection/3, symdiff/3]).

	Symbol1 < Symbol2 :-
		::sequence(Sequence),
		nth0(N1, Sequence, Symbol1),
		nth0(N2, Sequence, Symbol2),
		{N1 < N2}.

	Symbol1 =< Symbol2 :-
		::sequence(Sequence),
		nth0(N1, Sequence, Symbol1),
		nth0(N2, Sequence, Symbol2),
		{N1 =< N2}.

	Symbol1 > Symbol2 :-
		::sequence(Sequence),
		nth0(N1, Sequence, Symbol1),
		nth0(N2, Sequence, Symbol2),
		{N1 > N2}.

	Symbol1 >= Symbol2 :-
		::sequence(Sequence),
		nth0(N1, Sequence, Symbol1),
		nth0(N2, Sequence, Symbol2),
		{N1 >= N2}.

	Symbol1 =:= Symbol2 :-
		Symbol1 == Symbol2.

	Symbol1 =\= Symbol2 :-
		Symbol1 \== Symbol2.

	show :-
		write('ORDER: '), self(Self), writeq(Self), nl,
		write('  Alphabet: '), ::alphabet(Expression), writeq(Expression), nl,
		write('  Sequence:'), nl,
		forall(::sequence(Sequence), (write('    '), writeq(Sequence), nl)).

	valid :-
		::alphabet(Expression),
		alphabet(Expression)::valid,
		alphabet(Expression)::symbols(Symbols),
		::sequence(Sequence),
		intersection(Symbols, Sequence, []).

	diagnostic(Diagnostic) :-
		::alphabet(Expression),
		\+ alphabet(Expression)::valid,
		::build_error(Expression, 'is not an alphabet', Diagnostic).
	diagnostic(Diagnostic) :-
		::alphabet(Expression),
		alphabet(Expression)::symbols(Symbols),
		::sequence(Sequence),
		\+ symdiff(Symbols, Sequence, []),
		::build_error(Sequence, 'is not a permutation of the alphabet symbols', Diagnostic).

:- end_object.



:- object(word).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Words are finite sequences of symbols.']).

	:- public(valid/1).
	:- mode(valid(@term), zero_or_one).
	:- info(valid/1, [
		comment is 'Term is a valid word (a list of symbols).',
		argnames is ['Term']]).

	:- public(compute_word/2).
	:- mode(compute_word(+nonvar, -list), zero_or_one).
	:- info(compute_word/2, [
		comment is 'Expression evaluates to word Word.',
		argnames is ['Expression', 'Word']]).

	:- public(prefix/2).
	:- mode(prefix(?list, +list), zero_or_more).
	:- info(prefix/2, [
		comment is 'Word1 is a prefix of Word2.',
		argnames is ['Word1', 'Word2']]).

	:- public(suffix/2).
	:- mode(suffix(?list, +list), zero_or_more).
	:- info(suffix/2, [
		comment is 'Word1 is a suffix of Word2.',
		argnames is ['Word1', 'Word2']]).

	:- public(subword/2).
	:- mode(subword(?list, +list), zero_or_one).
	:- info(subword/2, [
		comment is 'Word1 is contained in Word2.',
		argnames is ['Word1', 'Word2']]).

	:- public(occurs/3).
	:- mode(occurs(+nonvar, +list, -integer), zero_or_one).
	:- info(occurs/3, [
		comment is 'Symbol occurs N times in Word.',
		argnames is ['Symbol', 'Word', 'N']]).

	:- public(word_alphabet/2).
	:- mode(word_alphabet(?list, @object_identifier), zero_or_more).
	:- mode(word_alphabet(?list, @list), zero_or_more).
	:- mode(word_alphabet(@list, -nonvar), one).
	:- info(word_alphabet/2, [
		comment is 'Word is a word over Alphabet.',
		argnames is ['Word', 'Alphabet']]).

	:- public(lexically_ordered/3).
	:- mode(lexically_ordered(?list, +list, +object_identifier), zero_or_one).
	:- mode(lexically_ordered(+list, ?list, +object_identifier), zero_or_one).
	:- info(lexically_ordered/3, [
		comment is 'Word1 < Word2 according to the lexical order based on the total alphabetic order.',
		argnames is ['Word1', 'Word2', 'Order']]).

	:- public(mixed_ordered/3).
	:- mode(mixed_ordered(?list, +list, +object_identifier), zero_or_one).
	:- mode(mixed_ordered(+list, ?list, +object_identifier), zero_or_one).
	:- info(mixed_ordered/3, [
		comment is 'Word1 =< Word2 and are lexically ordered according to the total alphabetic order.',
		argnames is ['Word1', 'Word2', 'Alphabet']]).

	:- public(next_word/3).
	:- mode(next_word(+list, -list, +object_identifier), zero_or_one).
	:- info(next_word/3, [
		comment is 'Word2 is the successor of Word1 according to the mixed order based on alphabetic order.',
		argnames is ['Word1', 'Word2', 'Alphabet']]).

	:- uses(term_classification, [is_symbol/1]).
	:- uses(list, [member/2, length/2, append/3, reverse/2, nth0/3, valid/1::is_list/1]).
	:- uses(set, [valid/1::is_set/1]).

	valid((-)) :-
		!,
		fail.
	valid([]).
	valid([Symbol| Symbols]) :-
		is_symbol(Symbol),
		valid(Symbols).
	valid(W1 * W2) :- valid(W1), valid(W2).
	valid(W ^ N) :- N >= -1, valid(W).

	% compute_word(+Expression, ?Word)
	% Expression denotes the list of symbols Word
	% pre: valid(Expression)

	compute_word(Word, Word) :-
		is_list(Word),
		!.
	compute_word(Expression1 * Expression2, Word) :-
		compute_word(Expression1, Word1),
		compute_word(Expression2, Word2),
		append(Word1, Word2, Word).
	compute_word(_ ^ 0, []).
	compute_word(E ^ N, Wn) :-
		N > 0, M is N-1, compute_word(E, W), compute_word(W * W^M, Wn).
	compute_word(E ^ N, Wr) :-
		-1 is N, compute_word(E, W), reverse(W, Wr).

	prefix(Word1, Word2) :-
		append(Word1, _, Word2).

	suffix(Word1, Word2) :-
		append(_, Word1, Word2).

	subword(Word1, Word2) :-
		prefix(Prefix, Word2),
		suffix(Word1, Prefix).

	occurs(_, [], 0) :-
		!.
	occurs(Symbol, [Symbol| Symbols], N) :-
		!,
		occurs(Symbol, Symbols, M),
		N is M + 1.
	occurs(Symbol, [_| Symbols], N) :-
		occurs(Symbol, Symbols, N).

	word_alphabet(Expression, Alphabet) :-
		(	var(Expression) ->
			Word = Expression
		;	compute_word(Expression, Word)
		),
		(	var(Alphabet) ->
			Alphabet = Symbols
		;	alphabet(Alphabet)::symbols(Symbols)
		),
		word_symbols(Word, Symbols).

	% word_symbols(?/+Word, +/-Symbols)
	% pre: (alphabet::valid(Symbols), Symbols = {_}) ; valid(Word)

	word_symbols(Word, Symbols) :-
		var(Symbols),
		!,
		setof(Symbol, member(Symbol, Word), Symbols).
	word_symbols([], _).
	word_symbols([Symbol| Word], Symbols) :-
		word_symbols(Word, Symbols),
		member(Symbol, Symbols).

	lexically_ordered([], [_| _], _).
	lexically_ordered([Symbol| Word1], [Symbol| Word2], Order) :-
		lexically_ordered(Word1, Word2, Order).
	lexically_ordered([Symbol1| _], [Symbol2| _], Order) :-
		Order::(Symbol1 < Symbol2).

	mixed_ordered(Word1, Word2, Order) :-
		length(Word1, N1),
		length(Word2, N2),
		(	N1 < N2
		;	N1 = N2,
			lexically_ordered(Word1, Word2, Order)
		;	N1 > N2,
			!,
			fail
		).

	next_word([], [First], Order) :-
		!,
		Order::sequence([First| _]).
	next_word(Word1, Word2, Order) :-
		append(Prefix, [Last], Word1),
		Order::sequence(Sequence),
		subword([Last, Next], Sequence),
		append(Prefix, [Next], Word2).
	next_word(Word1, Word2, Order) :-
		Order::sequence(Sequence),
		suffix([Last], Sequence),
		prefix([First], Sequence),
		append(Prefix1, [Last], Word1),
		next_word(Prefix1, Prefix2, Order),
		append(Prefix2, [First], Word2).

:- end_object.



:- object(meta_language,
	specializes(class)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Metaclass of language.']).

	:- public(new/4).
	:- mode(new(?object_identifier, +nonvar, +list, +list), one).
	:- info(new/4, [
		comment is 'Creates a new language.',
		argnames is ['Id', 'Alphabet', 'Positives', 'Negatives']]).

	:- uses(list, [append/3, member/2]).

	new(Id, Alphabet, Positives, Negatives) :-
		self(Self),
		findall(positive(Positive), member(Positive, Positives), Positives),
		findall(negative(Negative), member(Negative, Negatives), Negatives),
		append(Positives, Negatives, Clauses),
		create_object(Id, [instantiates(Self)], [], [alphabet(Alphabet)| Clauses]).

:- end_object.



:- object(language,
	instantiates(meta_language),
	specializes(entity)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Languages defined using unit tests.']).

	:- public(alphabet/1).
	:- mode(alphabet(-nonvar), zero_or_one).
	:- info(alphabet/1, [
		comment is 'Returns the expression that specifies the language alphapet.',
		argnames is ['Expression']]).

	:- public(positive/1).
	:- mode(positive(?list), zero_or_more).
	:- info(positive/1, [
		comment is 'Positive example of a language word.',
		argnames is ['Word']]).

	:- public(negative/1).
	:- mode(negative(?list), zero_or_more).
	:- info(negative/1, [
		comment is 'Negative example of a language word.',
		argnames is ['Word']]).

	:- public(test_mechanism/1).
	:- mode(test_mechanism(+object_identifier), zero_or_one).
	:- info(test_mechanism/1, [
		comment is 'Tests the definition of Mechanism using the language examples.',
		argnames is ['Mechanism']]).

	:- uses(word, [valid/1::is_word/1, word_alphabet/2]).

	valid :-
		::alphabet(Expression),
		alphabet(Expression)::valid,
		\+ (::positive(Word), \+ is_word(Word)),
		\+ (::negative(Word), \+ is_word(Word)),
		\+ (::positive(Word), \+ word_alphabet(Word, Expression)),
		\+ (::negative(Word), \+ word_alphabet(Word, Expression)).

	diagnostic(Diagnostic) :-
		\+ ::alphabet(_),
		::build_error('alphabet not declared', '', Diagnostic).
	diagnostic(Diagnostic) :-
		::alphabet(Expression),
		\+ alphabet(Expression)::valid,
		::build_error(Expression, 'is not a valid alphabet', Diagnostic).
	diagnostic(Diagnostic) :-
		\+ ::positive(_),
		\+ ::negative(_),
		::build_warning('no test cases', '', Diagnostic).
	diagnostic(Diagnostic) :-
		::positive(Word),
		\+ is_word(Word),
		::build_error(Word, 'invalid word in positive example', Diagnostic).
	diagnostic(Diagnostic) :-
		::negative(Word),
		\+ is_word(Word),
		::build_error(Word, 'invalid word in negative example', Diagnostic).
	diagnostic(Diagnostic) :-
		\+ ::positive([]),
		\+ ::negative([]),
		::build_weak_warning('no test case for the empty word', '', Diagnostic).
	diagnostic(Diagnostic) :-
		::alphabet(Expression),
		::positive(Word),
		\+ word_alphabet(Word, Expression),
		::build_error(Word, 'is not over given alphabet', Diagnostic).
	diagnostic(Diagnostic) :-
		::alphabet(Expression),
		::negative(Word),
		\+ word_alphabet(Word, Expression),
		::build_error(Word, 'is not over given alphabet', Diagnostic).
	diagnostic(Diagnostic) :-
		::positive(Word),
		::negative(Word),
		::build_error(Word, 'word cannot be both a positive and a negative example', Diagnostic).

	test_mechanism(Mechanism) :-
		self(Self),
		write('Starting tests of '), write(Mechanism),
		write(' against '), write(Self), write(' ...'), nl,
		fail.
	test_mechanism(Mechanism) :-
		self(Language),
		::alphabet(LanguageAlphabet),
		Mechanism::alphabet(MechanismAlphabet),
		\+ alphabet::contains(LanguageAlphabet, MechanismAlphabet),
		::fail_with_error(Mechanism, Language, 'has a different alphabet').
	test_mechanism(Mechanism) :-
		::positive(Word),
		\+ Mechanism::accept(Word),
		(	modes::current_conceal_mode(off) ->
			::fail_with_error(Mechanism, Word, 'should be accepted')
		;	::fail_with_error(Mechanism, 'does not define the intended language', '')
		).
	test_mechanism(Mechanism) :-
		(	::negative(Word),
			Mechanism::accept(Word) ->
			(	modes::current_conceal_mode(off) ->
				::fail_with_error(Mechanism, Word, 'should be rejected')
			;	::fail_with_error(Mechanism, 'does not define the intended language', '')
			)
		;	fail
		).
	test_mechanism(_) :-
		write('... tests finished'), nl.

	show :-
		write('LANGUAGE: '), self(Self), writeq(Self), nl,
		write('  Alphabet: '), ::alphabet(Expression), writeq(Expression), nl,
		write('  Positive examples:'), nl,
		forall(::positive(Word), (write('    '), writeq(Word), nl)),
		write('  Negative examples:'), nl,
		forall(::negative(Word), (write('    '), writeq(Word), nl)).

:- end_object.



:- protocol(deterministicp).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Protocol for querying mechanisms determinacy. Only meaningful for mechanisms whose primary purpose is parsing.']).

	:- public(deterministic/0).
	:- mode(deterministic, zero_or_one).
	:- info(deterministic/0, [
		comment is 'Checks if the mechanism is deterministic.']).

:- end_protocol.



:- object(mechanism,
	instantiates(abstract_class),
	specializes(entity)).

	:- info([
		version is 2:1:1,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Common predicates for all mechanisms.']).

	:- public(accept/2).
	:- mode(accept(+list, -list), zero_or_one).
	:- info(accept/2, [
		comment is 'Entity accepts word Word with execution path Path.',
		argnames is ['Word', 'Path']]).

	:- public(accept/1).
	:- mode(accept(+list), zero_or_one).
	:- info(accept/1, [
		comment is 'This is a much faster and economical version of accept/2.',
		argnames is ['Word']]).

	:- public(word/1).
	:- mode(word(-list), zero_or_more).
	:- info(word/1, [
		comment is 'Mechanism generates word. This predicate produces the solutions according to the mixed increasing order, that is shorter words first.',
		argnames is ['Word']]).

	:- public(alphabet/1).
	:- mode(alphabet(-nonvar), one).
	:- info(alphabet/1, [
		comment is 'Mechanism alphabet expression.',
		argnames is ['Alphabet']]).

	:- public(action/2).
	:- mode(action(+nonvar, -nonvar), zero_or_more).
	:- info(action/2, [
		comment is '.',
		argnames is ['Config', 'NextConfig']]).

	:- public(tracing/1).
	:- mode(tracing(-list), zero_or_more).
	:- info(tracing/1, [
		comment is 'Trace mechanism accepting Word.',
		argnames is ['Word']]).

	:- public(initial_config/2).
	:- mode(initial_config(+list, -list), one).
	:- info(initial_config/2, [
		comment is '.',
		argnames is ['Word', 'Path']]).

	:- public(success_config/1).
	:- mode(success_config(+nonvar), zero_or_one).
	:- info(success_config/1, [
		comment is '.',
		argnames is ['Config']]).

	:- public(write_config/1).
	:- mode(write_config(+nonvar), zero_or_one).
	:- info(write_config/1, [
		comment is '.',
		argnames is ['Config']]).

	:- protected(mechanism_description/1).
	:- mode(mechanism_description(-atom), one).
	:- info(mechanism_description/1, [
		comment is 'Mechanism textual description.',
		argnames is ['Description']]).

	:- public((export)/1).
	:- mode(export(+atom), one).
	:- info((export)/1, [
		comment is 'Exports the mechanism definition to a XML file.',
		argnames is ['File']]).

	:- protected(export_definition/1).
	:- mode(export_definition(+atom), one).
	:- info(export_definition/1, [
		comment is 'Writes mechanism definition.',
		argnames is ['File']]).

	:- uses(list, [member/2, reverse/2]).
	:- uses(set, [valid/1::is_set/1, union/3]).
	:- uses(word, [word_alphabet/2]).

	% actions(+CurrentPaths, -FinalPaths, +VisitedConfigs)
	% Breath-first search of an acceptance execution path.
	% Progressively and in parallel, builds and keeps track all the execution paths
	% that may lead to success (called viable execution paths). On backtracking,
	% each of the current viable execution paths is extended with one extra
	% configuration, representing the next execution step for that path. If a
	% path can be extended in several ways, then this path gives rise to multiple
	% new execution paths. Early looping detection is implemented with the help
	% of the VisitedConfigs argument: loops are pruned once detected.
	%
	actions(FinalPaths, FinalPaths, _).
	actions(CurrentPaths, FinalPaths, VisitedConfigs) :-
		(	setof(C, Cs^member([C| Cs], CurrentPaths), VisitedConfigs2) ->
			true
		;	VisitedConfigs2 = []
		),
		union(VisitedConfigs, VisitedConfigs2, NewVisitedConfigs),
		(	setof([N,C| Cs], (member([C| Cs], CurrentPaths), ::action(C, N), \+ member(N, NewVisitedConfigs)), NewCurrentPaths) ->
			true
		;	NewCurrentPaths = []
		),
		actions(NewCurrentPaths, FinalPaths, NewVisitedConfigs).

	% actions_fast(+CurrentConfigs, -FinalConfigs, +VisitedConfigs)
	% Breath-first search of an acceptance configuration.
	% This is a much faster and economical version of actions/3, to be used
	% when explicitly building actual execution paths is too much. Instead of
	% building and keeping track of all viable execution paths, this predicate
	% only builds and keeps track a "front wave" of parallel viable configurations.
	% Loops are detected and pruned.
	%
	actions_fast(FinalConfigs, FinalConfigs, _).
	actions_fast(CurrentConfigs, FinalConfigs, VisitedConfigs) :-
		% write('-----'), nl, write(CurrentConfigs), nl,
		union(VisitedConfigs, CurrentConfigs, NewVisitedConfigs),
		(	setof(N, C^(member(C, CurrentConfigs), ::action(C, N), \+ member(N, NewVisitedConfigs)), NewCurrentConfigs) ->
			true
		;	NewCurrentConfigs = []
		),
		% write(NewCurrentConfigs),	nl,
		actions_fast(NewCurrentConfigs, FinalConfigs, NewVisitedConfigs).

	% accept(+W, -Path)
	% Entity E accepts word W with execution path Path.
	% Success happens if there is at least one execution path leading to success.
	% If any such execution path exists, it will be discovered (time and memory
	% allows).
	% Failure happens if all the possible execution paths could be proved
	% unable to reach success. This means that those execution paths that
	% stopped did that at a non-final state and that all the remaining active
	% execution paths were looping, always revisiting the same configurations.
	% Non-termination happens if none of the execution paths reach success
	% and at least one of them never stops and keeps generating (infinitely many)
	% new configurations.
	%
	accept(Word, Path) :-				% implements semi-algorithm
		nonvar(Word),
		::accept(Word),					% may turn semi-algorithm into algorithm
		::initial_config(Word, InitialConfig),
		catch(actions([[InitialConfig]], Paths, []), _, Paths = []), % out of memory?
		(	Paths == [], !, fail		% all paths unable to reach success
		;	member([Config| Configs], Paths),
			::success_config(Config),	% found path that reached success
			!, reverse([Config| Configs], Path)
		).

	accept(Word) :-						% implements semi-algorithm
		var(Word),
		throw(instantiation_error).

	accept(Word) :-						% implements semi-algorithm
		::initial_config(Word, InitialConfig),
		catch(actions_fast([InitialConfig], Configs, []), _, Configs = []), % out of memory?
		(	Configs == [] ->
			!, fail						% no success configuration attained
		;	member(Config, Configs),
			::success_config(Config),	% success configuration reached
			!
		).

	tracing(Word) :-
		nonvar(Word),
		tracing_write_head(Word),
		(	::accept(Word, Path) ->
			tracing_write_accepted,
			tracing_write_steps(Path)
		;	tracing_write_rejected
		).

	tracing_write_head(Word) :-
		self(Self),
		::mechanism_description(Type),
		write('TRACING '), write(Type), write(':'), nl,
		write('  Name: '), write(Self), nl,
		write('  Traced word: '),
		write(Word), nl.

	tracing_write_rejected :-
		write('  No execution path stopped at an acceptance state. Word rejected.'), nl.

	tracing_write_accepted :-
		write('  At least one execution path stops at an acceptance state. Word accepted.'), nl.

	tracing_write_steps(Path) :-
		write('  Traced steps:'), nl,
		forall(
			member(Config, Path),
			(write(' '), ::write_config(Config))
		).

	word(Word) :-
		::alphabet(Expression),
		word_alphabet(Word, Expression),
		::accept(Word).

	initial_config(Word, InitialConfig) :-
		throw(existence_error(initial_config/2, initial_config(Word, InitialConfig))).

	success_config(Config) :-
		throw(existence_error(success_config/1, success_config(Config))).

	export(File) :-
		open(File, write, Stream),
		write(Stream, '<?xml version=\"1.0\"?>\n'),
		write(Stream, '<!--Created with lflat-->\n'),
		write(Stream, '<structure>\n'),
		::export_definition(Stream),
		write(Stream, '</structure>\n'),
		close(Stream).

	export_definition(_) :-
		throw(subclass_responsability(export_definition/1)).

:- end_object.



:- object(predicate,
	instantiates(class),
	specializes(mechanism)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Specify a language using a Prolog predicate.']).

	:- uses(word, [word_alphabet/2]).

	mechanism_description('PREDICATE').

	initial_config(Word, [Word]).

	success_config(Word, [Word]).

	action(_, _).

	write_config(Word) :-
		write('   '), write(Word), nl.

	valid :-
		::alphabet(Alphabet),
		alphabet(Alphabet)::valid.

	diagnostic(Diagnostic) :-
		::alphabet(Alphabet),
		\+ alphabet(Alphabet)::valid,
		::build_error(Alphabet, 'is not an alphabet', Diagnostic).

	show :-
		write('PREDICATE: '), self(Self), writeq(Self), nl,
		write('  Alphabet: '), ::alphabet(Alphabet), write(Alphabet), nl.
		% write('  Definition:'), nl, listing(PRED). user term_expansion/2 num hook para guardar o predicado accept/2

	accept(_) :-
		throw(instance_responsability(accept/1)).

	accept(Word, [Word]) :-	% no path for predicates
		::accept(Word).

:- end_object.



:- object(meta_re,
	specializes(class)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Metaclass of re.']).

	:- public(new/2).
	:- mode(new(?object_identifier, +nonvar), one).
	:- info(new/2, [
		comment is 'Creates a new regular expression.',
		argnames is ['Id', 'Expression']]).

	new(Id, Expression) :-
		self(Self),
		create_object(Id, [instantiates(Self)], [], [expression(Expression)]).

:- end_object.



:- object(re,
	instantiates(meta_re),
	specializes(mechanism)).

	:- info([
		version is 2:1:2,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2021-02-05,
		comment is 'Regular expressions.']).

	:- public(expression/1).
	:- mode(expression(-nonvar), one).
	:- info(expression/1, [
		comment is 'Expression is the expression corresponding to RE',
		argnames is ['Expression']]).

	:- public(fa/1).
	:- mode(fa(-nonvar), one).
	:- info(fa/1, [
		comment is 'Translates the regular expression into finite automata FA.',
		argnames is ['FA']]).

	:- public(simplify/1).
	:- mode(simplify(-nonvar), one).
	:- info(simplify/1, [
		comment is 'RE2 is a simpler equivalent of RE',
		argnames is ['RE2']]).


	:- uses(term_classification, [is_symbol/1]).
	:- uses(gensym, [gensym/2]).
	:- uses(list, [append/3, member/2, reverse/2]).
	:- uses(set, [symdiff/3, union/3]).
	:- uses(word, [valid/1::is_word/1]).
	:- uses(fa, [re/1]).

	/*
	re(+D, +RE)			Declare D to be the regular expression RE
	expression(?RE, -E)
	re_accept(?RE, +W)		RE matches W
	re_word(?RE, -W)		RE generates W
	re_tracing(?RE, +W)		Trace RE matching W
	*/

	% REGULAR EXPRESSIONS can be defined using terms of the form
	%
	%	re(Expression)
	%
	% where Expression is a regular expression where the concatenation is denoted
	% by "*", union is denoted by "+" and closure is denoted by "^*". The empty
	% word is denoted by [] an the empty regular expression is denoted by {}.
	% Here is an example:
	%
	%	re(([]^* * 0^* * 1 * 0^* * 1)^* * 0^*)
	%
	% In the predicate accept/3, RE-CONFIGURATIONS are represented by
	% terms of the form
	%
	%	config(CurrentStack, ProcessedInput, RemainingInput)
	%
	% where CurrentStack is a list representing a current stack of regular expressions,
	% ProcessedInput is a list representing the input already processed
	% and RemainingInput is a list representing the remaining input, not
	% yet processed.
	% The transitions between configurations defined in action/2, specify a
	% pushdown automata that recognizes the language generated by the re.
	%

	mechanism_description('REGULAR EXPRESSION').

	valid :-
		::expression(Expression),
		valid(Expression).

	valid({}) :- !.
	valid([]) :- !.
	valid(S) :-
		is_symbol(S), !.
	valid(RE) :-
		callable(RE),
		instantiates_class(RE, re), !,
		RE::expression(Expression),
		valid(Expression).
	valid(RE1 + RE2) :- !,
		valid(RE1),
		valid(RE2).
	valid(RE1 * RE2) :- !,
		valid(RE1),
		valid(RE2).
	valid(RE^*) :- !,
		valid(RE).

	diagnostic(Diagnostic) :-
		::expression(Expression),
		\+ valid(Expression),
		::build_error('ill-defined regular expression', Expression, Diagnostic).

	alphabet(Alphabet) :-
		::expression(Expression),
		alphabet(Expression, Alphabet).

	alphabet({}, []).
	alphabet([], []).
	alphabet(S, [S]) :-
		is_symbol(S), !.
	alphabet(RE, Alphabet) :-
		callable(RE),
		instantiates_class(RE, re), !,
		RE::expression(Expression),
		alphabet(Expression, Alphabet).
	alphabet(RE1 + RE2, A) :-
		alphabet(RE1, A1),
		alphabet(RE2, A2),
		union(A1, A2, A).
	alphabet(RE1 * RE2, A) :-
		alphabet(RE1, A1),
		alphabet(RE2, A2),
		union(A1, A2, A).
	alphabet(RE^*, A) :-
		alphabet(RE, A).

	show :-
		write('REGULAR EXPRESSION: '), self(Self), writeq(Self), nl,
		write('  Definition: '), ::expression(Expression), write(Expression), nl.

	initial_config(Word, config([_], [], Word)).

	success_config(config([], _, [])).

	write_config(config(Stack, Done, Input)) :-
		reverse(Done, DoneR),
		write('   '), ::write_symbol_list(DoneR, ' '),
		write('>'), ::write_symbol_list(Input, ''),
		( Input == [] -> true; write(' ')),
		write('  '), ::write_symbol_list(DoneR, ' '),
		forall(member(X, Stack), (write(' ('), write(X), write(')'))),
		nl.

	% action(+RE, +CurrentConfig, -NewConfig)
	% Apply to CurrentConfig any compatible transition, giving NewConfig.
	% If there is no compatible transition available the predicate fails,
	% signaling the machine should stop. Used in accept/2.
	%

	action(config([[]| Stack], Done, Input), config(Stack, Done, Input)) :-
		!.
	action(config([S| Stack], Done, [S| Input]), config(Stack, [S| Done], Input)) :-
		is_symbol(S),
		!.
	action(config([RE| Stack], Done, Input), config([Expression| Stack], Done, Input)) :-
		callable(RE),
		instantiates_class(RE, re),
		!,
		RE::expression(Expression).
	action(config([(RE1 + _)| Stack], Done, Input), config([RE1| Stack], Done, Input)).
	action(config([(_ + RE2)| Stack], Done, Input), config([RE2| Stack], Done, Input)).
	action(config([(RE1 * RE2)| Stack], Done, Input), config([RE1, RE2| Stack], Done, Input)).
	action(config([(_^*)| Stack], Done, Input), config(Stack, Done, Input)).
	action(config([(RE1^*)| Stack], Done, Input), config([RE1, (RE1^*)| Stack], Done, Input)).

	accept(Word) :-			% implements algorithm
		nonvar(Word),
		::expression(Expression),
		matched(Expression, Word).

	% matched(+RE, +W)
	% Word W is matched by regular expression RE.
	% Must be deterministic predicate, so the cuts are really needed.
	%
	matched([], []).
	matched(S, [S]) :-
		is_symbol(S),
		!.
	matched(RE, W) :-
		callable(RE),
		instantiates_class(RE, re),
		!,
		RE::expression(Expression),
		matched(Expression, W).
	matched(RE1 + RE2, W) :-
		(matched(RE1, W) ; matched(RE2, W)),
		!.
	matched(RE1 * RE2, W) :-
		append(W1, W2, W),
		matched(RE1, W1),
		matched(RE2, W2),
		!.
	matched(_^*, []) :-
		!.
	matched(RE^*, W) :-
		append(W1, W2, W),
		W1 \== [],
		matched(RE, W1),
		matched(RE^*, W2),
		!.

	fa(FA) :-
		::expression(Expression),
		fae(Expression, FAE),
		fae_compute(FAE, FA).

	% fae(+RE, -FAE)
	% Translates regular expression RE into expression FAE over finite automata.
	%
	fae({}, fa(I, [], [])) :-
		gensym(s, I).
	fae([], fa(I, [], [I])) :-
		gensym(s, I).
	fae(S, fa(I, [I/S/F], [F])) :-
		is_symbol(S),
		!,
		gensym(s, I),
		gensym(s, F).
	fae(RE, FA) :-
		callable(RE),
		instantiates_class(RE, re),
		!,
		RE::expression(Expression),
		fae(Expression, FA).
	fae(RE1 + RE2, FA1 + FA2) :-
		fae(RE1, FA1),
		fae(RE2, FA2).
	fae(RE1 * RE2, FA1 * FA2) :-
		fae(RE1, FA1),
		fae(RE2, FA2).
	fae(RE^*, FA^*) :-
		fae(RE, FA).

	% fae_compute(+FAE, -FA)
	% Evaluate expression FAE over finite automata, producing FA
	%
	fae_compute((-), _) :-		% catch variables
		!,
		fail.
	fae_compute(FA1 + FA2, fa(I, T, F)) :-
		!,
		fae_compute(FA1, fa(I1, T1, F1)),
		fae_compute(FA2, fa(I2, T2, F2)),
		union(F1, F2, F),
		gensym(s, I),
		union(T1, T2, T12),
		union(T12, [I/[]/I1], T22),
		union(T22, [I/[]/I2], T).
	fae_compute(FA1 * FA2, fa(I1, T, F2)) :-
		!,
		fae_compute(FA1, fa(I1, T1, F1)),
		fae_compute(FA2, fa(I2, T2, F2)),
		union(T1, T2, T12),
		(	setof(F/[]/I2, member(F, F1), T3) ->
			union(T12, T3, T)
		;	T = T12
		).
	fae_compute(FA1^*, fa(I, T, F)) :-
		!,
		fae_compute(FA1, fa(I1, T1, F1)),
		gensym(s, I),
		union([I], F1, F),
		union(T1, [I/[]/I1], T12),
		(	setof(Fs/[]/I1, member(Fs, F1), T3) ->
			union(T12, T3, T)
		;	T = T12
		).
	fae_compute(-FA, fa(I, T, F)) :-
		!,
		fae_compute(det(FA), fa(I, T, A)),
		fa(I, T, A)::states(S), symdiff(S, A, F).
	fae_compute(FA1 /\ FA2, FA) :-
		!,
		fae_compute(-(-FA1 + (-FA2)), FA).
	fae_compute(det(FA1), FA2) :-
		!,
		fae_compute(FA1, FA),
		(	FA::deterministic ->
			FA2 = FA
		;	FA::determine(FA2)
		).
	fae_compute(min(FA1), FA2) :-
		!,
		fae_compute(det(FA1), FA), FA::minimize(FA2).
	fae_compute(FA, fa(I, T, F)) :-
		FA::initial(I),
		FA::transitions(T),
		FA::finals(F).

	simplify(re(RE)) :-
		::expression(Expression),
		simplify(Expression, RE).

	% simplify(-RE)
	% RE is provably equivalent to self and RE is usually simpler.
	% This is an ad hoc solution that only handles a bunch of cases.
	% There is no method to systematically rewrite a RE to some
	% normal form (this is called the "star height problem").
	%	
	simplify(E + E, F) :-
		simplify(E, F).
	simplify(E + {}, F) :-
		simplify(E, F).
	simplify({} + E, F) :-
		simplify(E, F).
	simplify(E^* + [], F) :-
		simplify(E^*, F).
	simplify([] + E^*, F) :-
		simplify(E^*, F).
	simplify(E1 * (E2 + E3), G) :-
		simplify(E1 * E2, F1),
		simplify(E1 * E3, F2),
		simplify(F1 + F2, G).
	simplify(E^* + E, F) :-
		simplify(E^*, F).
	simplify(E * E^* + [], F) :-
		simplify(E^*,F).
	simplify([] + E * E^*, F) :-
		simplify(E^*,F).
	simplify(E1 + E2, G) :-
		simplify(E1,F1),
		simplify(E2,F2),
		(E1 \= F1 ; E2 \= F2), simplify(F1 + F2, G).
	simplify(_ * {}, {}).
	simplify({} * _, {}).
	simplify(E * [], F) :-
		simplify(E, F).
	simplify([] * E, F) :-
		simplify(E, F).
	simplify(E1 * E2, G) :-
		simplify(E1, F1),
		simplify(E2, F2),
		(E1 \= F1 ; E2 \= F2), simplify(F1 * F2, G).
	simplify({}^*, []).
	simplify([]^*, []).
	simplify((E^*)^*, F) :-
		simplify(E^*,F).
	simplify((E + [])^*, F) :-
		simplify(E^*, F).
	simplify(([] + E)^*, F) :-
		simplify(E^*, F).
	simplify(E^*, G) :-
		simplify(E,F),
		E \= F,
		simplify(F^*, G).
	simplify(E * ((F * E)^*), G) :-
		simplify((E * F)^* * E, G).
	simplify(E, E).

:- end_object.



:- object(meta_fa,
	specializes(class)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Metaclass of fa.']).

	:- public(new/4).
	:- mode(new(?object_identifier, +nonvar, +list, +list), one).
	:- info(new/4, [
		comment is 'Creates a new finite automaton.',
		argnames is ['Id', 'Initial', 'Transitions', 'Finals']]).

	:- uses(list, [sort/2]).

	new(Id, Initial, Transitions, Finals) :-
		self(Self),
		sort(Transitions, OrderedTransitions),
		sort(Finals, OrderedFinals),
		create_object(Id, [instantiates(Self)], [], [initial(Initial), transitions(OrderedTransitions), finals(OrderedFinals)]).

:- end_object.



:- object(fa,
	implements(deterministicp),
	instantiates(meta_fa),
	specializes(mechanism)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Finite Automata.']).

	:- public(initial/1).
	:- mode(initial(-nonvar), one).
	:- info(initial/1, [
		comment is 'Finite automaton initial state.',
		argnames is ['Initial']]).

	:- public(transition/3).
	:- mode(transition(-nonvar, -nonvar, -nonvar), zero_or_more).
	:- info(transition/3, [
		comment is 'Transition with source state From, target state To, consuming Symbol (which may be the empty word []).',
		argnames is ['From', 'Symbol', 'To'],
		redefinition is never]).

	:- public(transitions/1).
	:- mode(transitions(-list), one).
	:- info(transitions/1, [
		comment is 'List of the finite automaton transitions.',
		argnames is ['Transitions']]).

	:- public(final/1).
	:- mode(final(-nonvar), zero_or_more).
	:- info(final/1, [
		comment is 'State is a final state of the finite automaton.',
		argnames is ['State'],
		redefinition is never]).

	:- public(finals/1).
	:- mode(finals(-list), one).
	:- info(finals/1, [
		comment is 'List of the finite automaton final states.',
		argnames is ['Finals']]).

	:- public(state/1).
	:- mode(state(-nonvar), zero_or_more).
	:- info(state/1, [
		comment is 'State is a state of the finite automaton. Generates thorugh backtracking each state exactly once.',
		argnames is ['State']]).

	:- public(states/1).
	:- mode(states(-nonvar), one).
	:- info(states/1, [
		comment is 'The states of the finite automaton are States.',
		argnames is ['States']]).

	:- public(reachable/4).
	:- mode(reachable(+nonvar, ?nonvar, ?nonvar, +list), zero_or_more).
	:- info(reachable/4, [
		comment is 'State2 is reachable from State1 by consuming Word without repeating states (including those in Path). Assumes member(State1, Path).',
		argnames is ['State1', 'State2', 'Word', 'Path']]).

	:- public(symbol/1).
	:- mode(symbol(-nonvar), zero_or_more).
	:- info(symbol/1, [
		comment is 'Symbol is a symbol of FA.',
		argnames is ['Symbol'],
		redefinition is never]).

	:- public(symbols/1).
	:- mode(symbols(-list), one).
	:- info(symbols/1, [
		comment is 'The symbols of FA are Symbols.',
		argnames is ['Symbols'],
		redefinition is never]).

	:- public(re/1).
	:- mode(re(-nonvar), one).
	:- info(re/1, [
		comment is 'Translates the deterministic finite automaton into a regular expression RegExp',
		argnames is ['RegExp']]).

	:- public(determine/1).
	:- mode(determine(-nonvar), one).
	:- info(determine/1, [
		comment is 'Transform non-deterministic FA into deterministic FA',
		argnames is ['FADeterm']]).

	:- public(rename/1).
	:- mode(rename(-nonvar), one).
	:- info(rename/1, [
		comment is 'Rename all the stated with new ids',
		argnames is ['FARenamed']]).

	:- public(minimize/1).
	:- mode(minimize(-nonvar), one).
	:- info(minimize/1, [
		comment is 'Minimize finita automata',
		argnames is ['FAMinimized']]).

	:- uses(term_classification, [is_symbol/1, is_lambda/1, is_state/1]).
	:- uses(gensym, [gensym/2]).
	:- uses(list, [length/2, member/2, reverse/2]).
	:- uses(set, [valid/1::is_set/1, union/3, intersection/3, symdiff/3, subtract/3]).
	:- uses(re, [simplify/2]).

	% FINITE AUTOMATA can be defined using terms of the form
	%
	%	fa(InitialState, Transitions, FinalStates)
	%
	% where InitialState is a state,
	% Transitions is a set of transitions, each one of the form
	%	FromState/Symbol/ToState,
	% and FinalStates is a set of states (possibly empty).
	% Here is an example:
	%
	%	fa(	1,
	%		[ 1/a/1, 1/a/2, 1/b/2,
	%		  2/b/2, 2/b/1
	%		],
	%		[2]
	%	).
	%
	% In the predicate generic_accept/3, FA-CONFIGURATIONS are represented by
	% terms of the form
	%
	%	config(CurrentState, ProcessedInput, RemainingInput)
	%
	% where CurrentState is a state,
	% ProcessedInput is a list representing the input already processed
	% and RemainingInput is a list representing the remaining input, not
	% yet processed.
	%

	mechanism_description('FINITE AUTOMATON').

	valid :-
		write('Not yet implemented'),
		fail.

	diagnostic(Diagnostic) :-
		::initial(Initial),
		\+ is_state(Initial),
		::build_error('invalid initial state', Initial, Diagnostic).
	diagnostic(Diagnostic) :-
		::transitions(Transitions),
		\+ is_set(Transitions),
		::build_error('transitions are not a set', '', Diagnostic).
	diagnostic(Diagnostic) :-
		::transitions(Transitions),
		member(Transition, Transitions),
		Transition \= _/_/_,
		::build_error('syntactically wrong transition', Transition, Diagnostic).
	diagnostic(Diagnostic) :-
		::finals(Finals),
		\+ is_set(Finals),
		::build_error('final states are not a set', '', Diagnostic).
	diagnostic(Diagnostic) :-
		::finals([]),
		::build_weak_warning('no final states', '', Diagnostic).
	diagnostic(Diagnostic) :-
		::initial(Initial),
		\+ transition(Initial, _, _),
		::build_error('initial transition undefined', '', Diagnostic).
	diagnostic(Diagnostic) :-
		::state(From),
		::symbol(Symbol),
		\+ transition(From, Symbol, _),
		::build_weak_warning('undefined transition for state/symbol', From/Symbol, Diagnostic).
	diagnostic(Diagnostic) :-
		::initial(Initial),
		::states(States),
		(	setof(Reachable, Word^(::reachable(Initial, Reachable, Word, [Initial])), ReachableStates) ->
			subtract(States, ReachableStates, UnreachableStates)
		;	UnreachableStates = States
		),
		UnreachableStates \== [],
		::build_weak_warning('unreachable states', UnreachableStates, Diagnostic).

	final(Final) :-
		::finals(Finals),
		member(Final, Finals).

	transition(From, Symbol, To) :-
		::transitions(Transitions),
		member(From/Symbol/To, Transitions).

	states(States) :-
		::initial(Initial),
		::finals(Finals),
		union([Initial], Finals, InitialFinals),
		(	setof(From, Symbol^To^(transition(From, Symbol, To)), FromStates) ->
			true
		;	FromStates = []
		),
		(	setof(To, From^Symbol^(transition(From, Symbol, To)), ToStates) ->
			true
		;	ToStates = []
		),
		union(FromStates, ToStates, FromToStates),
		union(InitialFinals, FromToStates, States).

	state(State) :-
		states(States),
		member(State, States).

	symbols(Symbols) :-
		(	setof(Symbol, From^To^(transition(From, Symbol, To), Symbol \== []), Symbols) ->
			true
		;	Symbols = []
		).

	symbol(Symbol) :-
		symbols(Symbols),
		member(Symbol, Symbols).

	alphabet(Alphabet) :-
		symbols(Alphabet).

	show :-
		write('FINITE AUTOMATON: '), self(Self), writeq(Self), nl,
		write('  Initial state: '), ::initial(Initial), write(Initial), nl,
		write('  Transitions:'), nl,
		::transitions(Transitions),
		forall(
			member(From/Symbol/To, Transitions),
			(write('    '), write(From), write(' - '), write(Symbol), write(' -> '), write(To), nl )
		),
		write('  Final states: '), ::finals(Finals), write(Finals), nl,
		write('  Deterministic: '),
		(	deterministic ->
			write('yes'), nl
		;	write('no'), nl
		).

	initial_config(Word, config(Initial, [], Word)) :-
		::initial(Initial).

	success_config(config(Final, _, [])) :-
		final(Final).

	write_config(config(State, Done, Input)) :-
		reverse(Done, DoneR),
		write('   '), ::write_symbol_list(DoneR, ' '),
		write('>'), ::write_symbol_list(Input, ''),
		(	Input == [] ->
			true
		;	write(' ')
		),
		write('   '), write(State),
		nl.

	% action(+CurrentConfig, -NewConfig)
	% Apply to CurrentConfig any compatible transition, giving NewConfig.
	% If there is no compatible transition available the predicate fails,
	% signaling the machine should stop. Used in accept/2.
	%
	action(config(OldState, Done, [Symbol| Input]), config(NewState, [Symbol| Done], Input)) :-
		transition(OldState, Symbol, NewState).
	action(config(OldState, Done, Input), config(NewState, Done, Input)) :-
		transition(OldState, [], NewState).

	deterministic :-
		\+ transition(_, [], _),
		\+ (transition(From, State, To1), transition(From, State, To2), To1 \= To2).

	% reachable(+State1, ?State2, ?Word, +Path)
	% State2 is reachable from State1 by consuming Word without repeating
	% states (including those in Path)
	% pre: member(State1, Path)
	%
	reachable(State, State, [], _) :-
		state(State).
	reachable(S1, S2, W, Path) :-
		transition(S1, Lambda, S),
		is_lambda(Lambda),
		\+ member(S, Path),
		reachable(S, S2, W, [S| Path]).
	reachable(S1, S2, [H| T], Path) :-
		transition(S1, H, S),
		is_symbol(H),
		\+ member(S, Path),
		reachable(S, S2, T, [S| Path]).

	% consume(?State1, +Word, ?State2)
	% FA consumes Word when going from State1 to State2.
	%
	consume(State, [], State) :-
		state(State).
	consume(S1, W, S2) :-
		transition(S1, Lambda, S),
		is_lambda(Lambda),
		consume(S, W, S2).
	consume(S1, [H| T], S2) :-
		transition(S1, H, S),
		is_symbol(H),
		consume(S, T, S2).

	% re(-RE)
	% Translates self into a regular expression RE.
	%
	re(re(R)) :-
		::initial(Initial),
		states(States),
		(	setof(E, Final^(final(Final), re(Initial, Final, States, E)), Es) ->
			true
		;	Es = []
		),
		re_set(R, Es).

	re_set({}, []).
	re_set(E2 + E1, [E1|T]) :-
		re_set(E2, T).

	re(S, S, [], E) :-
		(	setof(Symbol, transition(S, Symbol, S), Ts) ->
			union(Ts, [[]], Es)
		;	Es = [[]]
		),
		re_set(RE, Es), re(RE)::simplify(E), !.
	re(S1, S2, [], E) :-
		(	setof(Symbol, transition(S1, Symbol, S2), Es) ->
			true
		;	Es = []
		),
		re_set(RE, Es), re(RE)::simplify(E), !.
	re(S1, S2, [S|R], E) :-
		re(S1, S, R, E1), re(S, S, R, E2),
		re(S, S2, R, E3), re(S1, S2, R, E4),
		re(E1 * E2^* * E3 + E4)::simplify(E), !.

	% det_next_state(+CurrentState, +Symbol, +NextState)
	% NextState is the set of states reachable from the set CurrentState
	% by consuming the Symbol in the non-deterministic finite automaton NDFA
	%
	det_next_state(CS, S, NS) :-
		(	setof(N, C^(member(C, CS), consume(C, [S], N)), NS) ->
			true
		;	NS = []
		).

	% det_trans(+S, +CS, +CT, -NT)
	% Builds the deterministic transitions NT for symbols S, starting with
	% the current state CS and the current transitions CT.
	%
	det_trans([], _, CT, CT).
	det_trans([S| R], CS, CT, NT) :-
		det_next_state(CS, S, NS),
		(	(NS = [] ; member(CS/S/NS, CT)) ->
			T2 = CT
		;	symbols(FAS), union([CS/S/NS], CT, T1),
			det_trans(FAS, NS, T1, T2)
		),
		det_trans(R, CS, T2, NT).

	% det_final(?Final, +Transitions, +OriginalFinals)
	% Final is a final state of the deterministic automaton with Transitions
	% OriginalFinals are the final states of the non-deterministic automaton.

	det_final(Final, Transitions, OriginalFinals) :-
		(	member(Final/_/_, Transitions)
		;	member(_/_/Final, Transitions)
		),
		intersection(Final, OriginalFinals, Intersection),
		Intersection \== [].

	% determine(-FA)
	% Transforms non-deterministic self into deterministic FA.
	%
	determine(fa(I2, T2, F2)) :-
		::initial(I1),
		symbols(S1),
		::finals(F1),
		(	setof(S, consume(I1, [], S), I2) ->
			true
		;	I2 = []
		),
		det_trans(S1, I2, [], T2),
		(	setof(F, det_final(F, T2, F1), F2) ->
			true
		;	F2 = []
		).

	% rename(-FA)
	% FA is as self, but with all states renamed with new identifiers.
	%
	rename(FA) :-
		states(S1), rename_states(S1, [], _, R),
		rename(R, FA).

	% rename(+Renaming, -FA)
	% The names FA's states are obtained by applying Renaming to self's states.
	% States can be merged if Renaming is not a bijection
	% pre: Renaming is a total function given as a set of pairs State1/State2
	%
	rename(R, fa(I2, T2, F2)) :-
		::initial(I1),
		::transitions(T1),
		::finals(F1),
		rename_states(F1, R, F2, R),
		rename_states([I1], R, [I2], R),
		rename_transitions(T1, R, T2).

	% rename_states(+States1, +Renaming1, +States2, ?Renaming2)
	% Renames States1 into States2 as an application of Renaming2 where
	% Renaming1 is a given start set of State1/State2 pairs to use:
	% State1 is renamed into State2 if State1/State2 in Renaming1,
	% otherwise State2 is a fresh name and State1/State2 is added to
	% Renaming2
	%
	rename_states([], R, [], R).
	rename_states([H1| T1], R1, S2, R2) :-
		member(H1/H2, R1),
		rename_states(T1, R1, T2, R2), union([H2], T2, S2).
	rename_states([H1| T1], R1, S2, R2) :-
		\+ member(H1/_, R1),
		gensym(s, H2), union([H1/H2], R1, R),
		rename_states(T1, R, T2, R2), union([H2], T2, S2).

	% rename_transitions(+Transitions1, +Renaming, +Transitions2)
	% Renames (and possibly merges) Transitions1 into Transitions2 by
	% applying the Renaming of states
	% pre: as for rename
	%
	rename_transitions([], _, []).
	rename_transitions([F1/S/T1], R, [F2/S/T2]) :-
		member(F1/F2, R),
		member(T1/T2, R).
	rename_transitions([T1| S1], R, T) :-
		rename_transitions([T1], R, T2),
		rename_transitions(S1, R, S2),
		union(T2, S2, T).

	% equivalent_states(+State1, +State2, +N)
	% State1 and State2 of FA are equivalent after N iterations
	%
	equivalent_states(State1, State2, 0) :-
		final(State1),
		final(State2).
	equivalent_states(State1, State2, 0) :-
		\+ final(State1),
		\+ final(State2).
	equivalent_states(State1, State2, N) :-
		N > 0,
		M is N-1,
		equivalent_states(State1, State2, M),
		symbols(Symbols),
		forall(
			member(Symbol, Symbols),
			(	transition(State1, Symbol, S11) ->
				transition(State2, Symbol, S21),
				equivalent_states(S11, S21, M)
			;	\+ transition(State2, Symbol, _)
			)
		).

	% equivalence_class(+State, ?Class)
	% Class is the set of states of self equivalent to State
	%
	equivalence_class(State, Class) :-
		states(States), length(States, N), Max is N-2,
		(	setof(Equivalent, (member(Equivalent, States), equivalent_states(State, Equivalent, Max)), Class) ->
			true
		;	Class = []
		).

	% minimize(-FA)
	% The minimisation of self is FA.
	% pre: self is deterministic
	%
	minimize(FA) :-
		states(States),
		(	setof(State/Class, (member(State, States), equivalence_class(State, Class)), R) ->
			true
		;	R = []
		),
		rename(R, FA).

	% COMENTARY:
	%
	% Why is it necessary to use the breath-first strategy and loop detection
	% in fa_accept/2?
	%
	% Take this finite automata that recognizes the one word language {a}:
	%
	%	fa(1, [1/a/2], [2]).
	%
	% This is a deterministic machine without loops. Therefore a simple depth-first
	% strategy is enough to accept the word "a" and reject all the other words.
	%
	% Not let us insert a dummy transition (at the first position). This
	% transition causes a loop in the automata.
	%
	%	fa(1, [1/[]/1, 1/a/2], [2]).
	%
	% This automata recognizes the same language. But if the machine is implemented
	% using the depth-first strategy then it enters an endless loop when trying
	% to accept "a". Using the breath-first strategy solves the problem.
	% Even if there are endless execution paths, it is enough to find an
	% execution path that stops at a final state to accept the word.
	%
	% Now, to reject the word "aa" the implementation of the machines must discover
	% that all the execution paths that stop, will stop at non-final states.
	% Looping detection allows the implementation to discover that some execution
	% paths will never stop and can be pruned. So looping detection helps when
	% trying to reject words. Pruning execution paths also improves eficiency.
	%
	% There are no more complications regarding finite automata. All the endless
	% execution paths will start repeating themselves at some point, so they will
	% be pruned and the word will be accepted or rejected at some point.
	%
	
	export_definition(Stream) :-
		write(Stream, '<type>fa</type>\n'),
		write(Stream, '<automaton>\n'),
		::states(States),
		::initial(Initial),   %%in L-Flat - it's only one initial state -its not a list .!!
		::finals(Finals),
		wStates(Stream, States, [Initial], Finals, 0),
		wTrans(Stream, States),
		write(Stream, '</automaton>\n').

	wStates(_, [],_,_,_).
	wStates(Stream, [H|T],I,F,Id) :-
		write(Stream, '<state id=\"'),
		write(Stream, Id),
		write(Stream, '" name=\"'),
		write(Stream, H),
		write(Stream, '\">\n'),
		write(Stream, '<label>'),     
		write(Stream, H) ,
		write(Stream, '</label>\n'),
		( list::member(H,I) -> write(Stream, '<initial />\n'); true),
		( list::member(H,F) -> write(Stream, '<final />\n');   true),
		write(Stream, '</state>\n'),
		ID is Id+1,
		wStates(Stream, T,I,F,ID).

	%predicate to lookup an element of a list and return its position in the list -1 on fail
	%on fail with -1 the conversion will just blindly carry on

	lookup(E,L,N) :-
		d_lookup(E, L, 0, N).

	d_lookup(_,[],_, N) :- 
		N is (-1).

	d_lookup(E,[E|_],N,N).

	d_lookup(E,[_|T],ID,N):- 
		ID1 is ID+1,  
		d_lookup(E, T, ID1, N).

	%print out the list of transitions
	wTrans(Stream, S) :- 
		forall( ::transition(F,A,T), wT(Stream, F,A,T,S) ).

	wT(Stream, F, A, T, S):-
		write(Stream, '<transition>\n'),
		lookup(F,S,N1),
		write(Stream, '<from>'),write(Stream, N1),	write(Stream, '</from>'),
		lookup(T,S,N2),
		write(Stream, '<to>'),  write(Stream, N2),	write(Stream, '</to>')  ,
		write(Stream, '<read>'),	
		(	is_lambda(A) ->
			true
		;	write(Stream, A)
		),
		write(Stream, '</read>\n'),
		write(Stream, '</transition>\n').

:- end_object.



:- object(meta_cfg,
	specializes(class)).

	:- info([
		version is 2:1:1,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Metaclass of cfg.']).

	:- public(new/3).
	:- mode(new(?object_identifier, +nonvar, +list), one).
	:- info(new/3, [
		comment is 'Creates a new context free grammar.',
		argnames is ['Id', 'StartSymbol', 'Rules']]).

	:- uses(list, [sort/2]).

	new(Id, StartSymbol, Rules) :-
		self(Self),
		sort(Rules, OrderedRules),
		create_object(Id, [instantiates(Self)], [], [start_symbol(StartSymbol), rules(OrderedRules)]).

:- end_object.



:- object(cfg,
	instantiates(meta_cfg),
	specializes(mechanism)).

	:- info([
		version is 2:1:1,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Context Free Grammars.']).

	:- public(start_symbol/1).
	:- mode(start_symbol(-nonvar), one).
	:- info(start_symbol/1, [
		comment is 'The start symbol of the context free grammar is Symbol.',
		argnames is ['Symbol']]).

	:- public(rule/1).
	:- mode(rule(-nonvar), zero_or_more).
	:- info(rule/1, [
		comment is 'Rule is a rule of the context free grammar.',
		argnames is ['Rule']]).

	:- public(rule/2).
	:- mode(rule(-nonvar, -nonvar), zero_or_more).
	:- info(rule/2, [
		comment is 'The context free grammar has rule Head->Body.',
		argnames is ['Head', 'Body']]).

	:- public(rules/1).
	:- mode(rules(-list), one).
	:- info(rules/1, [
		comment is 'The rules of the context free grammar are Rules',
		argnames is ['Rules']]).

	:- public(chain_free/1).
	:- mode(chain_free(-object_identifier), one).
	:- info(chain_free/1, [
		comment is 'Returns a chain-free CFG, cfg(NS, Qs), equivalent to the context free grammar.',
		argnames is ['CFG']]).

	:- public(normalize/1).
	:- mode(normalize(-object_identifier), one).
	:- info(normalize/1, [
		comment is 'Returns a quasi-lambda-free and chain-free equivalent grammar.',
		argnames is ['CFG']]).

	:- public(terminal/1).
	:- mode(terminal(-nonvar), one).
	:- info(terminal/1, [
		comment is 'Symbol is a terminal symbol of the context free grammar.',
		argnames is ['Symbol']]).

	:- public(terminal_symbols/1).
	:- mode(terminal_symbols(-list), one).
	:- info(terminal_symbols/1, [
		comment is 'The terminal symbols of the context free grammar are Symbols.',
		argnames is ['Symbols']]).

	:- public(non_terminal/1).
	:- mode(non_terminal(-nonvar), zero_or_more).
	:- info(non_terminal/1, [
		comment is 'Symbol is a non-terminal symbol of the context free grammar.',
		argnames is ['Symbol']]).

	:- public(non_terminal_symbols/1).
	:- mode(non_terminal_symbols(-list), one).
	:- info(non_terminal_symbols/1, [
		comment is 'The non-terminal symbols of the context free grammar are Symbols.',
		argnames is ['Symbols']]).

	:- public(symbols_generating_lambda/1).
	:- mode(symbols_generating_lambda(-list), one).
	:- info(symbols_generating_lambda/1, [
		comment is 'Finds the set of all the symbols that generate lambda.',
		argnames is ['Symbols']]).

	:- uses(term_classification, [is_symbol/1]).
	:- uses(list, [member/2, length/2, append/3, reverse/2, select/3, sort/2, valid/1::is_list/1]).
	:- uses(set, [valid/1::is_set/1, union/3, difference/3, subtract/3]).
	:- uses(gensym, [gensym/2]).

	% CONTEXT FREE GRAMMARS can be defined using terms of the form
	%
	%	cfg(StartSymbol, Rules)
	%
	% where StartSymbol is a non_terminal symbol, called the start symbol of the
	% grammar
	% and Rules is the set of rules of the grammar. Each rule has the form
	%	Head->Body
	% where Head is a non_terminal symbol and Body is a list of symbols (possibly
	% empty).
	% Here is an example:
	%
	%	cfg('E',
	%		[ ('E'->['E',+,'T']),
	%		  ('E'->['T']),
	%		  ('T'->['T',*,'F']),
	%		  ('T'->['F']),
	%		  ('F'->['(','E',')']),
	%		  ('F'->[a])
	%		]
	%	).
	%
	% In the predicate generic_accept/3, CFG-CONFIGURATIONS are represented by
	% terms of the form
	%
	%	config(CurrentStack, ProcessedInput, RemainingInput)
	%
	% where CurrentStack is a list representing the current stack,
	% ProcessedInput is a list representing the input already processed
	% and RemainingInput is a list representing the remaining input, not
	% yet processed.
	% The transitions between configurations defined in action/2, specify a
	% pushdown automata that recognizes the language generated by the cfg.
	%

	mechanism_description('CONTEXT FREE GRAMMAR').

	terminal(Symbol) :-
		is_symbol(Symbol),
		\+ non_terminal(Symbol).

	non_terminal(Symbol) :-
		atom(Symbol),
		atom_chars(Symbol, [C| _]),
		'A' @=< C, C @=< 'Z'.

	valid :-
		write('Not yet implemented'),
		fail.

	diagnostic(Diagnostic) :-
		::start_symbol(Initial),
		\+ non_terminal(Initial),
		::build_error(Initial, 'invalid start symbol', Diagnostic).
	diagnostic(Diagnostic) :-
		::rules(Rules),
		\+ is_set(Rules),
		::build_error('rules are not a set', "", Diagnostic).
	diagnostic(Diagnostic) :-
		rule(Rule),
		\+ (Rule = (_->B), is_list(B)),
		::build_error(Rule, 'syntactically wrong rule', Diagnostic).
	diagnostic(Diagnostic) :-
		rule(Symbol, _),
		\+ non_terminal(Symbol),
		::build_error(Symbol, 'invalid head of rule', Diagnostic).
	diagnostic(Diagnostic) :-
		rule(_, Symbols),
		member(Symbol, Symbols),
		\+ terminal(Symbol),
		\+ non_terminal(Symbol),
		::build_error(Symbol, 'invalid symbol in body of rule', Diagnostic).
	diagnostic(Diagnostic) :-
		::start_symbol(Initial),
		\+ rule(Initial, _),
		::build_warning('no rules for the start symbol', '', Diagnostic).
	diagnostic(Diagnostic) :-
		non_terminal_symbols(Symbols),
		member(Symbol, Symbols), \+ ::rule(Symbol, _), \+ ::start_symbol(Symbol),
		::build_warning('no rules for the non_terminal symbol', Symbol, Diagnostic).
	diagnostic(Diagnostic) :-
		terminal_symbols([]),
		::build_warning( 'there are no terminal symbols in the rules', '', Diagnostic).

	rule(Rule) :-
		::rules(Rules),
		member(Rule, Rules).

	rule(H, B) :-
		rule((H->B)).

	terminal_symbols(Symbols) :-
		(	setof(T, H^B^(rule(H, B), member(T, B), terminal(T)), Ts) ->
			Symbols = Ts
		;	Symbols = []
		).

	non_terminal_symbols(Ns) :-
		(	setof(H, B^(rule(H,B)), Ns1) ->
			true
		;	Ns1 = []
		),
		(	setof(N, H^B^(rule(H,B), member(N,B), non_terminal(N)), Ns2),
			true
		;	Ns2 = []
		),
		union(Ns1, Ns2, Ns).

	alphabet(Alphabet) :-
		terminal_symbols(Alphabet).

	show :-
		write('CONTEXT FREE GRAMMAR: '), self(Self), writeq(Self), nl,
		write('  Start symbol: '), ::start_symbol(Initial), writeq(Initial), nl,
		write('  Rules:'), nl,
		forall(
			rule(R),
			(write('    '), writeq(R), nl)).

	initial_config(Word, config([Initial], [], Word)) :-
		::start_symbol(Initial).

	success_config(config([], _, [])).

	% valid_config(+Stack, +Input)
	% Check if Stack and Input belong to a valid configuration. The number
	% of terminal symbols in Stack must be smaller or equal to the number of
	% symbols in Input. This predicate helps in reducing the combinatorial
	% explosion in action/3.
	%
	valid_config([], _).
	valid_config([Symbol| Stack], Input) :-
		\+ terminal(Symbol), !,
		valid_config(Stack, Input).
	valid_config([_| Stack], [_| Input]) :-
		valid_config(Stack, Input).

	write_config(config(Stack, Done, Input)) :-
		reverse(Done, DoneR),
		write('   '), ::write_symbol_list(DoneR, ' '),
		write('>'), ::write_symbol_list(Input, ''),
		(	Input == [] ->
			true
		;	write(' ')
		),
		write('   '),
		::write_symbol_list(DoneR, ' '),
		::write_symbol_list(Stack, ' '),
		nl.

	% action(+CurrentConfig, -NewConfig)
	% Apply to CurrentConfig any compatible transition, giving NewConfig.
	% If there is no compatible transition available the predicate fails,
	% signaling the machine should stop. Used in accept/2.
	%
	action(config([Symbol| Stack], Done, [Symbol| Input]), config(Stack, [Symbol| Done], Input)) :- !.
	action(config([Top| Stack], Done, Input), config(NewStack, Done, Input)) :-
		rule(Top, Push),
		append(Push, Stack, NewStack),
		valid_config(NewStack, Input).

	accept(Word) :-		% implements algorithm
		word_fast(Word).

	word(Word) :-
		word(_, Word).

	% left_derivation(+Start, ?NextRewrite, +Rules, +Missing)
	% This is a recursive predicate that uses bounded-depth backtracking to
	% derive a word from a context free grammar. It is assumed that Rules
	% belongs to a CFG that is quasi-lambda-free and chain-free.
	% Meaning of the parameters:
	%	- CurrentRewrite is the current rewrite (in the first invocation it
	%	should contain only the start symbol of the grammar);
	%	- NextRewrite is the next rewrite and, at the end, also the final result;
	%	- Rules are the rules of the grammar;
	%	- Missing is the number of symbols still missing in the current
	%	rewrite to achieve the required length.
	% The procedure stops as soon as these 2 conditions are meet:
	%	1 - Missing == 0;
	%	2 - the current rewrite no longer contains non_terminal symbols.
	% Assuming the CFG quasi-lambda-free and chain-free, at each derivation step
	% (except the first) or the current rewrite grows in length, or some
	% non_terminal symbol (the leftmost) is replaced by a terminal symbol.
	% As there is a bound to the length of the current rewrite, termination
	% is ensured.
	% This algorithm has exponential complexity, so beware.

	left_derivation([], [], _, 0).
	left_derivation([S| Ss], [S| Ss1], Rs, M) :-
		terminal(S),
		!,
		left_derivation(Ss, Ss1, Rs, M).
	left_derivation([H| Ss], W, Rs, M) :-
		member((H->B), Rs), append(B, Ss, Ss1),
		length(B, L), M2 is M + 1 - L, M2 >= 0,
		left_derivation(Ss1, W, Rs, M2).

	% word_fast(?Length, ?W)
	% W is a word of length Length that can be derived from CFG.
	% On backtracking, this predicate may generate duplicate solutions.
	% To ensure that the derivation procedure terminates, the given CFG is
	% converted to a equivalent quasi-lambda-free and chain-free CFG.
	%
	word_fast(L, W) :-
		::normalize(cfg(S, Rs)),
		length(W, L),		% generate lengths from 0 upwards
		M is L - 1,
		left_derivation([S], W, Rs, M).

	% word_fast(?W)
	% W is a word that can be derived from CFG.
	% On backtracking, this predicate may generate duplicate solutions.
	% The solutions generated using backtracking are produced in nondecreasing
	% length order.
	%
	word_fast(Word) :-
		word_fast(_, Word).

	% words(?Length, -Ws)
	% Ws is the set of all words of length Length that can be derived from CFG.
	% There are not duplicates in the returned set.
	%
	words(L, Ws) :-
		::normalize(cfg(S, Rs)),
		length(W, L),		% generate lengths from 0 upwards
		M is L - 1,
		(	setof(W, left_derivation([S], W, Rs, M), Ws) ->
			true
		;	Ws = []
		).

	% word(?Length, -W)
	% W is a word of length Length that can be derived from CFG.
	% Using backtracking, this predicate produces the solutions in increasing order.
	%
	word(L, W) :-
		var(W),			% if nonvar(W) it's better to use accept/2
		words(L, Ws),
		sort(Ws, Ws2),
		member(W, Ws2).

	% lambda_heads(+Rs, -Hs)
	% Hs is the set of heads of the rules in Rs which have an empty body.
	%
	lambda_heads(Rs, Hs) :-
		(	setof(H, member((H->[]), Rs), Hs) ->
			true
		;	Hs = []
		).

	% clear_lambda_rules(+Rs, -Qs)
	% Remove from the set of rules Rs all the rules with an empty body, giving Qs.
	%
	clear_lambda_rules(Rs, Qs) :-
		(	setof((H->[]), member((H->[]), Rs), Es) ->
			subtract(Rs, Es, Qs)
		;	Qs = Rs
		).

	% clear_body(+R, +Ss, -Q)
	% Remove all symbols contained in Ss from the body of rule R, giving rule Q.
	%
	clear_body((H->B), Ss, (H->B1)) :-
		findall(S, (member(S, B), \+ member(S, Ss)), B1).

	% clear_bodies(+Rs, +Ss, -Qs)
	% Remove all symbols in Ss from the bodies of rules in Rs, giving rules Qs.
	%
	clear_bodies(Rs, Ss, Qs) :-
		(	setof(Q, R^(member(R, Rs), clear_body(R, Ss, Q)), Qs) ->
			true
		;	Qs = []
		).

	symbols_generating_lambda(Symbols) :-
		::rules(Rules),
		symbols_generating_lambda(Rules, Symbols).

	% symbols_generating_lambda(+Rs, -Ss)
	% Find all the symbols Ss that generate lambda in the set of rules Rs.
	% Example:
	% symbols_generating_lambda([('S'->[]),('A'->['S','S']),('R'->['S','A']),('R'->[a])], X).
	% X = ['S', 'A']
	%
	symbols_generating_lambda(Rules, Symbols) :-
		lambda_heads(Rules, Symbols1),
		(	Symbols1 == [] ->
			Symbols = []
		;	clear_lambda_rules(Rules, Rules1),
			clear_bodies(Rules1, Symbols1, Rules2),
			symbols_generating_lambda(Rules2, Symbols2),
			union(Symbols1, Symbols2, Symbols)
		).

	% rule_variants(+R, +Ss, -Vs)
	% Erase one or more occurrences of one or more symbols belonging to Ss
	% from the body of rule R, giving the set of rules Vs. Furthermore, rules
	% of the form H->[] are not allowed in the result Vs.
	% If no symbol can be erased from the body of R then Vs={}.
	% Example:
	% rule_variants(('S'->['A','C','A','B']), {'A','B'}, X).
	% X = [ ('S'->['C','A','B']), ('S'->['A','C','B']),
	%	('S'->['A','C','A']), ('S'->['C','B']),
	%	('S'->['C','A']), ('S'->['A','C']), ('S'->['C'])]).
	%
	rule_variants((H->B), Ss, Vs) :-
		(	setof((H->B1), X^(select(X,B,B1), B1 \== [], member(X,Ss)), Vs1) ->
			true
		;	Vs1 = []
		),
		(	setof(Q, R^Qs^(member(R, Vs1), rule_variants(R, Ss, Qs), member(Q, Qs)), Vs2) ->
			true
		;	Vs2 = []
		),
		union(Vs1, Vs2, Vs).

	% quasi_lambda_free(-cfg(NS, Qs))
	% cfg(NS, Qs) in a quasi-lambda-free CFG equivalent to CFG.
	% A CGF is quasi-lambda-free if none of its rules has an empty body
	% except, possibly, for the special rule S->[], where S stands for the
	% start symbol and S does not occur in the body of any rule.
	% Example:	(in this example, the input CFG is not quasi-lambda-free)
	% quasi_lambda_free(cfg('S', {('S'->[]),('S'->[a,'S',b]),('S'->['S','S'])}), X).
	% X = cfg('$S1', {('$S1'->[]),('$S1'->['S']),('S'->[a,'S',b]),('S'->['S','S']),
	%		('S'->['S']),('S'->[a,b])})
	%
	quasi_lambda_free(cfg(NS, Qs)) :-
		::start_symbol(S),
		::rules(Rs),
		symbols_generating_lambda(Rs, Ss),
		clear_lambda_rules(Rs, Rs1),
		(	setof(V, R^Vs^(member(R, Rs1), rule_variants(R, Ss, Vs), member(V, Vs)), Rs12) ->
			union(Rs1, Rs12, Rs2)
		;	Rs2 = Rs1
		),
		(	member(S, Ss) ->
			gensym('S$', NS),
			union([(NS->[]), (NS->[S])], Rs2, Qs)
		;	NS = S,
			Qs = Rs2
		).

	% cfg_chains(+Rs, -Qs)
	% Qs are all the chains belonging to Rs. A chain is a rule whose body
	% consists of a single non_terminal.
	%
	get_chains(Rs, Cs) :-
		(	setof((H->[N]), (member((H->[N]), Rs), non_terminal(N)), Cs) ->
			true
		;	Cs = []
		).

	% chain_closure(+Cs, +Ns, -Ss)
	% Ss is the set of all non_terminals that can be reached by transversing
	% the chains in Cs, starting with non_terminals in Ns.
	% Example:
	% chain_closure({('A'->['B']),('A'->['C']),('B'->['C']), ('C'->['D']), ('A'->['A'])}, {'D'}, X).
	% X = {'D', 'C', 'A', 'B'} ;
	%
	chain_closure(Cs, Ns, Ss) :-
		(	setof(H, N^(member((H->[N]), Cs), member(N, Ns), \+ member(H, Ns)), Ns1) ->
			union(Ns, Ns1, Ns2),
			chain_closure(Cs, Ns2, Ss)
		;	Ss = Ns
		).

	% rule_chain_variants(+R, Cs, Vs)
	% Vs contains versions of the rule R, all with the same body but with the
	% head replaced in different ways. The new heads are the non_terminals in
	% the chain closure corresponding to the head of R and the chain set Cs.
	%
	rule_chain_variants((H->B), Cs, Vs) :-
		chain_closure(Cs, [H], Ns),
		(	setof((N->B), member(N, Ns), Vs) ->
			true
		;	Vs = []
		).

	% chain_free(-cfg(NS, Qs))
	% cfg(NS, Qs) in a chain-free CFG equivalent to CFG.
	% Example:
	% chain_free(cfg('A', {('A'->['B']),('B'->['C']),('C'->['B',d]),('C'->[c])}), X).
	% X =cfg('A', { ('A'->['B',d]), ('A'->[c]), ('B'->['B',d]), ('B'->[c]),
	%		('C'->['B',d]),('C'->[c])}) ;
	%
	chain_free(cfg(S, Qs)) :-
		::start_symbol(S),
		::rules(Rs),
		get_chains(Rs, Cs),
		subtract(Rs, Cs, Rs1),
		(	setof(V, R^Vs^(member(R, Rs1), rule_chain_variants(R,Cs,Vs), member(V, Vs)), Qs) ->
			true
		;	Qs = []
		).

	% normalize(-cfg(S, Qs)) :-
	% cfg(NS, Qs) in a quasi-lambda-free and chain-free CFG equivalent to CFG.
	% Such a grammar is almost in the Chomsky normal form. In a normalized CFG
	% it is easy to create a procedure to systematically derive all the
	% word from the grammar.
	% Example:		(warning - the result in not meant to be pretty)
	% normalize(cfg('S', {('S'->[]),('S'->[a,'S',b]),('S'->['S','S'])}), X).
	% X = cfg('S', { ('$S1'->[]), ('$S1'->['S','S']), ('$S1'->[a,'S', b]),
	%		('$S1'->[a, b]), ('S'->['S','S']), ('S'->[a,'S', b]),
	%		('S'->[a, b])})
	%
	normalize(cfg(NS, Qs)) :-
		quasi_lambda_free(CFG),
		CFG::chain_free(cfg(NS, Qs)).

:- end_object.



:- object(meta_pda,
	specializes(class)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Metaclass of pda.']).

	:- public(new/5).
	:- mode(new(?object_identifier, +nonvar, +nonvar, +list, +list), one).
	:- info(new/5, [
		comment is 'Creates a new pushdown automaton.',
		argnames is ['Id', 'Initial', 'InitialStackSymbol', 'Transitions', 'Finals']]).

	:- uses(list, [sort/2]).

	new(Id, Initial, InitialStackSymbol, Transitions, Finals) :-
		self(Self),
		sort(Transitions, OrderedTransitions),
		sort(Finals, OrderedFinals),
		create_object(Id, [instantiates(Self)], [], [initial(Initial), initial_stack_symbol(InitialStackSymbol), transitions(OrderedTransitions), finals(OrderedFinals)]).

:- end_object.



:- object(pda,
	implements(deterministicp),
	instantiates(meta_pda),
	specializes(mechanism)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Pushdown automata.']).

	:- public(initial/1).
	:- mode(initial(-nonvar), one).
	:- info(initial/1, [
		comment is 'The initial state of the pushdown automata is Initial.',
		argnames is ['Initial']]).

	:- public(initial_stack_symbol/1).
	:- mode(initial_stack_symbol(-nonvar), one).
	:- info(initial_stack_symbol/1, [
		comment is 'Initial is the initial stack symbol of the pushdown automata.',
		argnames is ['Initial']]).

	:- public(transition/5).
	:- mode(transition(-nonvar, -nonvar, -nonvar, -nonvar, -nonvar), zero_or_more).
	:- info(transition/5, [
		comment is 'From/Pop/In/To/Push is a transition of the pushdown automata.',
		argnames is ['From', 'Pop', 'In', 'To', 'Push']]).

	:- public(transitions/1).
	:- mode(transitions(-list), one).
	:- info(transitions/1, [
		comment is 'The transitions of the pushdown automata are Transitions.',
		argnames is ['Transitions']]).

	:- public(finals/1).
	:- mode(finals(-list), one).
	:- info(finals/1, [
		comment is 'The final states of the pushdown automata are States.',
		argnames is ['States']]).

	:- public(state/1).
	:- mode(state(-nonvar), zero_or_more).
	:- info(state/1, [
		comment is 'State is a state of the pushdown automata.',
		argnames is ['State']]).

	:- public(states/1).
	:- mode(states(-list), one).
	:- info(states/1, [
		comment is 'The states of the pushdown automata are States.',
		argnames is ['States']]).

	:- public(input_symbol/1).
	:- mode(input_symbol(-nonvar), zero_or_more).
	:- info(input_symbol/1, [
		comment is 'Symbol is a input symbol of the pushdown automata.',
		argnames is ['Symbol']]).

	:- public(input_symbols/1).
	:- mode(input_symbols(-list), one).
	:- info(input_symbols/1, [
		comment is 'The input symbols of the pushdown automata are Symbols.',
		argnames is ['Symbols']]).

	:- public(stack_symbol/1).
	:- mode(stack_symbol(-nonvar), zero_or_more).
	:- info(stack_symbol/1, [
		comment is 'Symbol is a stack symbol of the pushdown automata.',
		argnames is ['Symbol']]).

	:- public(stack_symbols/1).
	:- mode(stack_symbols(-list), one).
	:- info(stack_symbols/1, [
		comment is 'The stack symbols of the pushdown automata are Symbols.',
		argnames is ['Symbols']]).

	:- public(change_criterion/1).
	:- mode(change_criterion(-nonvar), one).
	:- info(change_criterion/1, [
		comment is 'Convert between the two acceptance criteria.',
		argnames is ['PDA2']]).

	:- uses(term_classification, [is_symbol/1, is_state/1]).
	:- uses(gensym, [gensym/2]).
	:- uses(list, [member/2, append/3, reverse/2, valid/1::is_list/1]).
	:- uses(set, [valid/1::is_set/1, union/3]).

	/*
	transition(?PDA, ?T)	T is a transition of PDA
	final(?PDA, ?F)		F is a final state of PDA

	change_criterion(?PDA1, -PDA2) Convert between the two acceptance criteria
	*/

	% PUSHDOWN AUTOMATA can be defined using terms of the form
	%
	%	pda(InitialState, InitialStackSymbol, Transitions, FinalStates)
	%
	% where InitialState is a state,
	% InitialStackSymbol is a symbol,
	% Transitions is a set with elements of the form
	%	SourceState/PoppedSymbol/InputSymbol/TargetState/PushedSymbols
	% and FinalStates is a set of states (possibly empty).
	% Here is an example:
	%
	%	pda(p, z,
	%		[ p/z/a/p/[a,z], p/a/a/p/[a,a], % read and push a's
	%		  p/a/b/q/[],			% the first b, start popping a's
	%		  q/a/b/q/[],			% pop one a for each read b
	%		  q/z/[]/t/[]			% no more b's, go to final state
	%		],
	%		[t]
	%	).
	%
	% In the predicate generic_accept/3, PDA-CONFIGURATIONS are represented by
	% terms of the form
	%
	%	config(CurrentState, CurrentStack, ProcessedInput, RemainingInput)
	%
	% where CurrentState is a state,
	% CurrentStack is a list representing the current stack
	% ProcessedInput is a list representing the input already processed
	% and RemainingInput is a list representing the remaining input, not yet
	% processed.
	%
	% If FinalStates is not an empty set, aceptance is based on the "final states
	% criteria". If FinalStates is an empty set, aceptance is based on the
	% "empty stack criterion". The predicate change_criterion/2 allows a pda
	% based on the "final states criterion" to be converted to the a pda based on
	% the "empty stack criteria", and vice-versa.
	%

	mechanism_description('PUSHDOWN AUTOMATON').

	valid :-
		write('Not yet implemented'),
		fail.

	diagnostic(Diagnostic) :-
		::transitions(Transitions),
		\+ is_set(Transitions),
		::build_error('transitions are not a set', '', Diagnostic).
	diagnostic(Diagnostic) :-
		::finals(Finals),
		\+ is_set(Finals),
		::build_error('final states are not a set', '', Diagnostic).
	diagnostic(Diagnostic) :-
		transition(Transition),
		(	Transition \= _/_/_/_/_
		;	Transition = _/_/_/_/_/_
		;	Transition = _/_/_/_/Push, \+ is_list(Push)
		),
		::build_error('syntactically wrong transition', Transition, Diagnostic).
	diagnostic(Diagnostic) :-
		state(State),
		\+ is_state(State),
		::build_error('invalid state', State, Diagnostic).
	diagnostic(Diagnostic) :-
		::input_symbol(Symbol),
		\+ is_symbol(Symbol),
		::build_error('invalid input symbol', Symbol, Diagnostic).
	diagnostic(Diagnostic) :-
		::stack_symbol(Symbol),
		\+ is_symbol(Symbol),
		::build_error('invalid stack symbol', Symbol, Diagnostic).
	diagnostic(Diagnostic) :-
		::initial(Initial),
		::initial_stack_symbol(Symbol),
		\+ transition(Initial, Symbol, _, _, _),
		::build_warning('initial transition undefined', '', Diagnostic).
	diagnostic(Diagnostic) :-
		state(From),
		stack_symbol(Symbol),
		\+ transition(From, Symbol, _, _, _),
		::build_weak_warning('undefined transition for state/symbol', From/Symbol, Diagnostic).

	transition(Transition) :-
		::transitions(Transitions),
		member(Transition, Transitions).

	transition(From, Pop, In, To, Push) :-
		transition(From/Pop/In/To/Push).

	final(Final) :-
		::finals(Finals),
		member(Final, Finals).

	states(States) :-
		(	setof(State, states_element(State), States) ->
			true
		;	States = []
		).

	states_element(State) :-
		transition(State,_,_,_,_).
	states_element(State) :-
		transition(_,_,_,State,_).
	states_element(State) :-
		::initial(State).
	states_element(State) :-
		final(State).

	state(State) :-
		states(States),
		member(State, States).

	input_symbols(Symbols) :-
		(	setof(Symbol, (input_symbols_element(Symbol), Symbol \== []), Symbols) ->
			true
		;	Symbols = []
		).

	input_symbols_element(Symbol) :-
		transition(_, _, Symbol, _, _).

	input_symbol(Input) :-
		input_symbols(Inputs),
		member(Input, Inputs).

	stack_symbols(Symbols) :-
		(	setof(Symbol, stack_symbols_element(Symbol), Symbols) ->
			true
		;	Symbols = []
		).

	stack_symbols_element(Symbol) :-
		transition(_, Symbol, _, _, _).
	stack_symbols_element(Symbol) :-
		transition(_, _, _, _, Push),
		member(Symbol, Push).
	stack_symbols_element(Symbol) :-
		::initial_stack_symbol(Symbol).

	stack_symbol(Symbol) :-
		stack_symbols(Symbols),
		member(Symbol, Symbols).

	alphabet(Alphabet) :-
		input_symbols(Alphabet).

	show :-
		write('PUSHDOWN AUTOMATON: '), self(Self), writeq(Self), nl,
		write('  Initial state: '), ::initial(Initial), write(Initial), nl,
		write('  Initial stack: ['),
		::initial_stack_symbol(Symbol), write(Symbol), write(']'), nl,
		write('  Transitions:'), nl,
		::transitions(Transitions),
		forall(
			member(From/Pop/In/To/Push, Transitions),
			(write('    '), write(From/Pop), write(' - '), write(In), write(' -> '), write(To/Push), nl)),
		write('  Final states: '), ::finals(Finals), write(Finals), nl,
		write('  Deterministic: '),
		(	deterministic ->
			write('yes'), nl
		;	write('no'), nl
		).

	initial_config(Word, config(Initial, [Symbol], [], Word)) :-
		::initial(Initial),
		::initial_stack_symbol(Symbol).

	success_config(config(_, [], _, [])) :-
		::finals([]),	% empty stack criterion
		!.
	success_config(config(Final, _, _, [])) :-
		final(Final).	% final states criterion

	write_config(config(State, Stack, Done, Input)) :-
		reverse(Done, DoneR),
		write('   '), ::write_symbol_list(DoneR, ' '),
		write('>'), ::write_symbol_list(Input, ''),
		( Input == [] -> true; write(' ')),
		write('   '), write(State),
		write('/['), ::write_symbol_list(Stack, ''), write(']'),
		nl.

	% action(+CurrentConfig, -NewConfig)
	% Apply to CurrentConfig any compatible transition, giving NewConfig.
	% If there is no compatible transition available the predicate fails,
	% signaling the machine should stop. Used in accept/2.
	%
	action(config(OldState, [Top| Stack], Done, [Symbol| Input]), config(NewState, NewStack, [Symbol| Done], Input)) :-
		transition(OldState, Top, Symbol, NewState, Push),
		append(Push, Stack, NewStack).
	action(config(OldState, [Top| Stack], Done, Input), config(NewState, NewStack, Done, Input)) :-
		transition(OldState, Top, [], NewState, Push),
		append(Push, Stack, NewStack).

	deterministic :-
		transition(From, Pop, In, To1, Push1),
		transition(From, Pop, In, To2, Push2),
		(	To1 \= To2
		;	Push1 \= Push2
		),
		!, fail.
	deterministic :-
		transition(From, Pop, [], _, _),
		transition(From, Pop, In, _, _), In \== [],
		!, fail.
	deterministic.

	% change_criterion(?PDA1, -PDA2)
	% If PDA1 accepts a language using the empty stack criterion then PDA2 accepts
	% the same language using the final states criterion.
	% If PDA1 accepts a language using the final states criterion then PDA2 accepts
	% the same language using the empty stack criterion.
	%

	change_criterion(pda(I2, S2, T2, [F2])) :-
		::finals([]),
		!,
		::initial(I1),
		::initial_stack_symbol(S1),
		::transitions(T1),
		gensym(s, I2), gensym(ss, S2), gensym(s, F2),
		union(T1, [I2/S2/[]/I1/[S1,S2]], T12),
		(	setof(State/S2/[]/F2/[S2], ::state(State), Ts) ->
			union(T12, Ts, T2)
		;	T2 = T12
		).
	change_criterion(pda(I2, S2, T2, [])) :-
		gensym(s, I2), gensym(ss, S2), gensym(s, V),
		::initial(I1),
		::initial_stack_symbol(S1),
		::transitions(T1),
		stack_symbols(SS1),
		union(SS1, [S2], SS2),
		union(T1, [I2/S2/[]/I1/[S1,S2]], T12),
		(	setof(F/SS/[]/V/[SS], F^SS^(final(F), member(SS, SS2)), Ts1) ->
			union(T12, Ts1, Ts12)
		;	Ts12 = T12
		),
		(	setof(V/SS/[]/V/[], member(SS, SS2), Ts2) ->
			union(Ts12, Ts2, T2)
		;	T2 = Ts12
		).

	%
	% COMENTARY:
	%
	% Why is it necessary to use the breath-first strategy and loop detection
	% in pda_accept/2?
	%
	% Take this pda that recognizes the one word language {a}:
	%	pda(i, z, [ i/z/a/t/[z] ], [t]).
	%
	% This is a deterministic pda without loops. Therefore a simple depth-first
	% strategy is enough to accept the word "a" and reject all the other words.
	%
	% Not let us insert a dummy transition (at the first position). This
	% transition causes a loop in the machine.
	%
	%	pda(i, z, [ i/z/[]/i/[z], i/z/a/t/[z] ], [t]).
	%
	% This pda recognizes the same language. But if the pda is implemented
	% using the depth-first strategy then it enters an endless loop when trying
	% to accept "a". Using the breath-first strategy solves the problem.
	% Even if there are endless execution paths, it is enough to find an
	% execution path that stops at a final state to accept the word.
	%
	% Now, to reject the word "aa" the implementation of the pdas must discover
	% that all the execution paths that stop, will stop at non-final states.
	% Looping detection allows the implementation to discover that some execution
	% paths will never stop and can be pruned. So looping detection helps when
	% trying to reject words. Pruning execution paths also improves eficiency.
	%
	% Still, some pdas can generate endless execution paths that never
	% repeat themselves. Such execution paths cannot be discovered. Therefore any
	% generic implementation of pdas may enter an endless loop when
	% trying to reject words. For example, this happens in the case of the
	% following pda with one dummy transition inserted at the first position,
	% when trying to reject the empty word.
	%
	%	pda(i, z, [ i/z/[]/i/[z,z], i/z/a/t/[z] ], {t}).
	%

:- end_object.



:- object(meta_tm,
	specializes(class)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Metaclass of tm.']).

	:- public(new/4).
	:- mode(new(?object_identifier, +nonvar, +list, +list), one).
	:- info(new/4, [
		comment is 'Creates a new Turing machine.',
		argnames is ['Id', 'Initial', 'Transitions', 'Finals']]).

	:- uses(list, [sort/2]).

	new(Id, Initial, Transitions, Finals) :-
		self(Self),
		sort(Transitions, OrderedTransitions),
		sort(Finals, OrderedFinals),
		create_object(Id, [instantiates(Self)], [], [initial(Initial), transitions(OrderedTransitions), finals(OrderedFinals)]).

:- end_object.



:- object(tm,
	implements(deterministicp),
	instantiates(meta_tm),
	specializes(mechanism)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-03,
		comment is 'Turing machines.']).

	:- public(initial/1).
	:- mode(initial(-nonvar), one).
	:- info(initial/1, [
		comment is 'Initial state of the Turing machine.',
		argnames is ['Initial']]).

	:- public(final/1).
	:- mode(final(-nonvar), zero_or_more).
	:- info(final/1, [
		comment is 'Final is a final state of TM.',
		argnames is ['Final']]).

	:- public(finals/1).
	:- mode(finals(-list), one).
	:- info(finals/1, [
		comment is 'List of the Turing machine final states.',
		argnames is ['Finals']]).

	:- public(transitions/1).
	:- mode(transitions(-list), one).
	:- info(transitions/1, [
		comment is 'List of the Turing machine transitions.',
		argnames is ['Transitions']]).

	:- public(transition/1).
	:- mode(transition(-nonvar), zero_or_more).
	:- info(transition/1, [
		comment is 'Turing machine transition.',
		argnames is ['Transition']]).

	:- public(transition/5).
	:- mode(transition(-nonvar, -nonvar, -nonvar, -nonvar, -nonvar), zero_or_more).
	:- info(transition/5, [
		comment is 'Turing machine has transition From/Read/Write/Move/To.',
		argnames is ['From', 'Read', 'Write', 'Move', 'To']]).

	:- public(state/1).
	:- mode(state(-nonvar), zero_or_more).
	:- info(state/1, [
		comment is 'State is a state of the Turing machine',
		argnames is ['State']]).

	:- public(states/1).
	:- mode(states(-list), one).
	:- info(states/1, [
		comment is 'The states of TM are States.',
		argnames is ['States']]).

	:- public(tape_symbol/1).
	:- mode(tape_symbol(-nonvar), zero_or_more).
	:- info(tape_symbol/1, [
		comment is 'Symbol is a tape symbol of the Turing machine.',
		argnames is ['Symbol']]).

	:- public(tape_symbols/1).
	:- mode(tape_symbols(-list), one).
	:- info(tape_symbols/1, [
		comment is 'The tape symbols of TM are Symbols.',
		argnames is ['Symbols']]).

	:- public(move_symbol/1).
	:- mode(move_symbol(-nonvar), zero_or_more).
	:- info(move_symbol/1, [
		comment is 'Symbol is a move symbol of the Turing machine.',
		argnames is ['Symbol']]).

	:- public(move_symbols/1).
	:- mode(move_symbols(-list), one).
	:- info(move_symbols/1, [
		comment is 'The move symbols of TM are Symbols.',
		argnames is ['Symbols']]).

	:- uses(term_classification, [is_symbol/1, is_state/1]).
	:- uses(list, [append/3, member/2, reverse/2]).
	:- uses(set, [valid/1::is_set/1, subtract/3]).

	% TURING MACHINES can be defined using terms of the form
	%
	%	tm(InitialState, Transitions, FinalStates)
	%
	% where InitialState is a state,
	% Transitions is a set of transitions, each one of the form
	%	SourceState/ReadSymbol/WrittenSymbol/MoveSymbol/TargetState,
	% and FinalStates is a set of states (possibly empty).
	% Here is an example:
	%
	%	tm(	q0,
	%		[ q0/'B'/'B'/'R'/q1,
	%		  q1/a/b/'R'/q1,	q1/b/a/'R'/q1,	q1/'B'/'B'/'L'/q2,
	%		  q2/a/a/'L'/q2,	q2/b/b/'L'/q2
	%		],
	%		[]
	%	).
	%
	% In the predicate generic_accept/3, TM CONFIGURATIONS are represented by
	% terms of the form
	%
	%	config(CurrentState, CellsToTheLeft, CurrentCell, CellsToTheRight)
	%
	% where CurrentState is a state, and the cells of the infinite tape are split in
	% three parts: CellsToTheLeft, CurrentCell, CellsToTheRight.
	%
	% If FinalStates is not an empty set, aceptance is based on the "final states
	% criteria". If FinalStates is an empty set, aceptance is based on the
	% "stop criteria".
	%

	mechanism_description('TURING MACHINE').

	valid :-
		write('Not yet implemented'),
		fail.

	diagnostic(Diagnostic) :-
		::transitions(Transitions),
		\+ is_set(Transitions),
		::build_error('transitions are not a set', '', Diagnostic).
	diagnostic(Diagnostic) :-
		::finals(Finals),
		\+ is_set(Finals),
		::build_error('final states are not a set', '', Diagnostic).
	diagnostic(Diagnostic) :-
		transition(Transition),
		(	Transition \= _/_/_/_/_
		;	Transition = _/_/_/_/_/_
		),
		::build_error('syntactically wrong transition', Transition, Diagnostic).
	diagnostic(Diagnostic) :-
		state(State),
		\+ is_state(State),
		::build_error('invalid state', State, Diagnostic).
	diagnostic(Diagnostic) :-
		tape_symbol(Symbol),
		\+ is_symbol(Symbol),
		::build_error(Symbol, 'invalid tape symbol', Diagnostic).
	diagnostic(Diagnostic) :-
		move_symbol(Symbol),
		\+ member(Symbol, ['L', 'R']),
		::build_error(Symbol, 'invalid move symbol (allowed are "L" or "R")', Diagnostic).
	diagnostic(Diagnostic) :-
		::initial(Initial),
		\+ transition(Initial, 'B', _, 'R', _),
		::build_warning('initial transition undefined', '', Diagnostic).
	diagnostic(Diagnostic) :-
		state(From),
		tape_symbol(Read),
		\+ transition(From, Read, _, _, _),
		::build_weak_warning('undefined transition for state/symbol', From/Read, Diagnostic).

	transition(Transition) :-
		::transitions(Transitions),
		member(Transition, Transitions).

	transition(From, Read, Write, Move, To) :-
		transition(From/Read/Write/Move/To).

	final(Final) :-
		::finals(Finals),
		member(Final, Finals).

	states(States) :-
		(	setof(State, states_element(State), States) ->
			true
		;	States = []
		).

	states_element(From) :-
		transition(From, _, _, _, _).
	states_element(To) :-
		transition(_, _, _, _, To).
	states_element(Initial) :-
		::initial(Initial).
	states_element(Final) :-
		final(Final).

	state(State) :-
		states(States),
		member(State, States).

	tape_symbols(Symbols) :-
		(	setof(Symbol, (tape_symbols_element(Symbol), Symbol \==[], Symbol \== 'B'), Symbols) ->
			true
		;	Symbols = []
		).

	tape_symbols_element(Symbol) :-
		transition(_, Symbol, _, _, _).
	tape_symbols_element(Symbol) :-
		transition(_, _, Symbol, _, _).

	tape_symbol(Symbol) :-
		tape_symbols(Symbols),
		member(Symbol, Symbols).

	move_symbols(Symbols) :-
		(	setof(Symbol, move_symbols_element(Symbol), Symbols) ->
			true
		;	Symbols = []
		).

	move_symbols_element(Symbol) :-
		transition(_, _, _, Symbol, _).

	move_symbol(Symbol) :-
		move_symbols(Symbols),
		member(Symbol, Symbols).

	alphabet(Alphabet) :-
		tape_symbols(Symbols),
		subtract(Symbols, ['A','B','C','D','E','F','G','H','I','J','K',
				'L','M','N','O','P','Q','R','S','T','U','V',
				'W','X','Y','Z'], Alphabet).

	show :-
		write('TURING MACHINE: '), self(Self), writeq(Self), nl,
		write('  Initial state: '), ::initial(Initial), write(Initial), nl,
		write('  Transitions:'), nl,
		::transitions(Transitions),
		forall(
			member(From/Read/Write/Move/To, Transitions),
			( write('    '), write(From),
			  write(' - ('), write(Read),
			  write('/'), write(Write),
			  write(' '), write(Move),
			  write(') -> '), write(To), nl )
		),
		write('  Final states: '), ::finals(Finals), write(Finals), nl,
		write('  Deterministic: '),
		(	deterministic ->
			write('yes'), nl
		;	write('no'), nl
		).

	% move(+MoveSymbol, +CurrentConfig, -NewConfig)
	% Moves the machine head one cell to the left or to the right, depending
	% on MoveSymbol. Produces NewConfig from CurrentConfig.
	%
	move('R', config(S, Ls, C, [R| Rs]), config(S, [C| Ls], R, Rs)) :- !.
	move('R', config(S, Ls, C, []), config(S, [C| Ls], 'B', [])) :- !.
	move('L', config(S, [L| Ls], C, Rs), config(S, Ls, L, [C| Rs])) :- !.

	initial_config(Word, config(Initial, [], 'B', Tape)) :-
		::initial(Initial),
		append(Word, ['B'], Tape).

	success_config(Config) :-
		Config = config(Final, _, _, _),
		\+ action(Config, _),		% stopped
		(	::finals([])
		;	final(Final)
		).	% accept

	write_config(config(State, Left, Current, Right)) :-
		reverse(Left, LeftR),
		write('   '), ::write_symbol_list(LeftR, ' '),
		write('>'), write(Current),
		::write_symbol_list(Right, ' '),
		write('    '), write(State),
		nl.

	% action(+CurrentConfig, -NewConfig)
	% Apply to CurrentConfig any compatible transition, giving NewConfig.
	% If there is no compatible transition available the predicate fails,
	% signaling the machine should stop. Used in accept/2.
	%
	action(config(State, Left, Current, Right), NewConfig) :-
		transition(State, Current, NewCurrent, Move, NewState),
		move(Move, config(NewState, Left, NewCurrent, Right), NewConfig).

	deterministic :-
		transition(From, Read, Write1, Move1, To1),
		transition(From, Read, Write2, Move2, To2),
		(	Write1 \= Write2
		;	Move1 \= Move2
		;	To1 \= To2
		),
		!, fail.
	deterministic.

	%
	% COMENTARY:
	%
	% Why is it necessary to use the breath-first strategy and loop detection
	% in tm_accept/2?
	%
	% Take this Turing machine that recognizes the one word language {a}:
	%	tm(	q0,
	%		[q0/'B'/'B'/'R'/q1, q1/a/a/'R'/q2, q2/'B'/'B'/'L'/q3],
	%		[q3]
	%	).
	%
	% This is a deterministic machine without loops. Therefore a simple depth-first
	% strategy is enough to accept the word "a" and reject all the other words.
	%
	% Not let us insert a dummy transition (at the second position). This
	% transition causes a loop in the machine.
	%
	%	tm(	q0,
	%		[q0/'B'/'B'/'R'/q1, q1/a/a/'L'/q0, q1/a/a/'R'/q2, q2/'B'/'B'/'L'/q3],
	%		[q3]
	%	).
	%
	% This machine recognizes the same language. But if the machine is implemented
	% using the depth-first strategy then it enters an endless loop when trying
	% to accept "a". Using the breath-first strategy solves the problem.
	% Even if there is are endless execution paths, it is enough to find a
	% execution path that stops at a final state to accept the word.
	%
	% Now, to reject the word "aa" the implementation of the machines must discover
	% that all the execution paths that stop, will stop at non-final states.
	% Looping detection allows the implementation to discover that some execution
	% paths will never stop and can be pruned. So looping detection helps when
	% trying to reject words. Pruning execution paths also improves eficiency.
	%
	% Still, some machines can generate endless execution paths that never
	% repeat themselves. Such execution paths cannot be discovered. Therefore any
	% generic implementation of Turing machines may enter an endless loop when
	% trying to reject words. For example, this happens in the following machine
	% with two dummy transitions, when trying to reject the empty word.
	%
	%	tm(	q0,
	%		[	q0/'B'/'B'/'R'/q1, q1/a/a/'R'/q2, q1/a/a/'R'/q1,
	%			q1/'B'/a/'R'/q1, q2/'B'/'B'/'L'/q3],
	%		[q3]
	%	).
	%

:- end_object.


% PARAMETRIC OBJECTS

:- object(alphabet(_Expression_),
	instantiates(alphabet)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Alphabet represented as a parametric object.',
		parnames is ['Expression']]).

	expression(_Expression_).

:- end_object.



:- object(alphabet(_Id, _Expression_),
	instantiates(alphabet)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Alphabet represented as a parametric object.',
		parnames is ['Id', 'Expression']]).

	expression(_Expression_).

:- end_object.



:- object(order(_Alphabet_, _Sequence_),
	instantiates(order)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Total order over an alphabet represented as a parametric object.',
		parnames is ['Alphabet', 'Sequence']]).

	alphabet(_Alphabet_).

	sequence(_Sequence_).

:- end_object.



:- object(order(_Id, _Alphabet_, _Sequence_),
	instantiates(order)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Total order over an alphabet represented as a parametric object.',
		parnames is ['Id', 'Alphabet', 'Sequence']]).

	alphabet(_Alphabet_).

	sequence(_Sequence_).

:- end_object.



:- object(language(_Alphabet_, _Positives_, _Negatives_),
	instantiates(language)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Language represented as a parametric object.',
		parnames is ['Alphabet', 'Positives', 'Negatives']]).

	:- uses(list, [member/2]).

	alphabet(_Alphabet_).

	positive(Positive) :-
		member(Positive, _Positives_).

	negative(Negative) :-
		member(Negative, _Negatives_).

:- end_object.



:- object(language(_Id, _Alphabet_, _Positives_, _Negatives_),
	instantiates(language)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Language represented as a parametric object.',
		parnames is ['Id', 'Alphabet', 'Positives', 'Negatives']]).

	:- uses(list, [member/2]).

	alphabet(_Alphabet_).

	positive(Positive) :-
		member(Positive, _Positives_).

	negative(Negative) :-
		member(Negative, _Negatives_).

:- end_object.



:- object(re(_Expression_),
	instantiates(re)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Regular expression represented as a parametric object.',
		parnames is ['Expression']]).

	expression(_Expression_).

:- end_object.



:- object(re(_Id, _Expression_),
	instantiates(re)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Regular expression represented as a parametric object.',
		parnames is ['Id', 'Expression']]).

	expression(_Expression_).

:- end_object.



:- object(fa(_Initial_, _Transitions_, _Finals_),
	instantiates(fa)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Finite automata represented as a parametric object.',
		parnames is ['Initial', 'Transitions', 'Finals']]).

	initial(_Initial_).

	transitions(_Transitions_).

	finals(_Finals_).

:- end_object.



:- object(fa(_Id, _Initial_, _Transitions_, _Finals_),
	instantiates(fa)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Finite automata represented as a parametric object.',
		parnames is ['Id', 'Initial', 'Transitions', 'Finals']]).

	initial(_Initial_).

	transitions(_Transitions_).

	finals(_Finals_).

:- end_object.



:- object(cfg(_StartSymbol_, _Rules_),
	instantiates(cfg)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Context-free grammar represented as a parametric object.',
		parnames is ['StartSymbol', 'Rules']]).

	start_symbol(_StartSymbol_).

	rules(_Rules_).

:- end_object.



:- object(cfg(_Id, _StartSymbol_, _Rules_),
	instantiates(cfg)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Context-free grammar represented as a parametric object.',
		parnames is ['Id', 'StartSymbol', 'Rules']]).

	start_symbol(_StartSymbol_).

	rules(_Rules_).

:- end_object.



:- object(pda(_Initial_, _InitialStackSymbol_, _Transitions_, _Finals_),
	instantiates(pda)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Pushdown automaton represented as a parametric object.',
		parnames is ['Initial', 'InitialStackSymbol', 'Transitions', 'Finals']]).

	initial(_Initial_).

	initial_stack_symbol(_InitialStackSymbol_).

	transitions(_Transitions_).

	finals(_Finals_).

:- end_object.



:- object(pda(_Id, _Initial_, _InitialStackSymbol_, _Transitions_, _Finals_),
	instantiates(pda)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Pushdown automaton represented as a parametric object.',
		parnames is ['Id', 'Initial', 'InitialStackSymbol', 'Transitions', 'Finals']]).

	initial(_Initial_).

	initial_stack_symbol(_InitialStackSymbol_).

	transitions(_Transitions_).

	finals(_Finals_).

:- end_object.



:- object(tm(_Initial_, _Transitions_, _Finals_),
	instantiates(tm)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Turing machine represented as a parametric object.',
		parnames is ['Initial', 'Transitions', 'Finals']]).

	initial(_Initial_).

	transitions(_Transitions_).

	finals(_Finals_).

:- end_object.



:- object(tm(_Id, _Initial_, _Transitions_, _Finals_),
	instantiates(tm)).

	:- info([
		version is 2:1:0,
		author is 'Artur Miguel Dias, Paulo Moura, and Michel Wermelinger',
		date is 2020-03-04,
		comment is 'Turing machine represented as a parametric object.',
		parnames is ['Id', 'Initial', 'Transitions', 'Finals']]).

	initial(_Initial_).

	transitions(_Transitions_).

	finals(_Finals_).

:- end_object.
