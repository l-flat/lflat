
:- object(hook,
	implements(expanding)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2008/9/30,
		comment is 'Hook predicates for converting example sets into ordered sets.']).

	:- uses(list, [sort/2]).

	term_expansion(alphabet([H| T]), [alphabet(OrderedExpression)]) :-
		sort([H| T], OrderedExpression).
	term_expansion(expression([H| T]), [expression(OrderedExpression)]) :-
		sort([H| T], OrderedExpression).
	term_expansion(finals(Finals), [finals(OrderedFinals)]) :-
		sort(Finals, OrderedFinals).
	term_expansion(rules(Rules), [rules(OrderedRules)]) :-
		sort(Rules, OrderedRules).
	term_expansion(transitions(Transitions), [transitions(OrderedTransitions)]) :-
		sort(Transitions, OrderedTransitions).

:- end_object.


% the following expansions are only necessary when using object proxies:
% (due to the lack of Prolog portability of the predicate_property/2, we
% cannot apply the obvious simplification to this code in order to avoid
% the duplicating the definitions of term_expansion/2!)

:- if(predicate_property(term_expansion(_, _), multifile)).

	:- multifile(term_expansion/2).
	:- dynamic(term_expansion/2).

	term_expansion(fa(Initial, Transitions, Finals), [fa(Initial, OrderedTransitions, OrderedFinals)]) :-
		sort(Transitions, OrderedTransitions),
		sort(Finals, OrderedFinals).

	term_expansion(cfg(StartSymbol, Rules), [cfg(StartSymbol, OrderedRules)]) :-
		sort(Rules, OrderedRules).

	term_expansion(pda(Initial, InitialStackSymbol, Transitions, Finals), [pda(Initial, InitialStackSymbol, OrderedTransitions, OrderedFinals)]) :-
		sort(Transitions, OrderedTransitions),
		sort(Finals, OrderedFinals).

	term_expansion(tm(Initial, Transitions, Finals), [tm(Initial, OrderedTransitions, OrderedFinals)]) :-
		sort(Transitions, OrderedTransitions),
		sort(Finals, OrderedFinals).

:- elif(current_logtalk_flag(prolog_dialect, qp)).

	:- multifile(term_expansion/2).
	:- dynamic(term_expansion/2).

	term_expansion(fa(Initial, Transitions, Finals), [fa(Initial, OrderedTransitions, OrderedFinals)]) :-
		sort(Transitions, OrderedTransitions),
		sort(Finals, OrderedFinals).

	term_expansion(cfg(StartSymbol, Rules), [cfg(StartSymbol, OrderedRules)]) :-
		sort(Rules, OrderedRules).

	term_expansion(pda(Initial, InitialStackSymbol, Transitions, Finals), [pda(Initial, InitialStackSymbol, OrderedTransitions, OrderedFinals)]) :-
		sort(Transitions, OrderedTransitions),
		sort(Finals, OrderedFinals).

	term_expansion(tm(Initial, Transitions, Finals), [tm(Initial, OrderedTransitions, OrderedFinals)]) :-
		sort(Transitions, OrderedTransitions),
		sort(Finals, OrderedFinals).

:- else.

	:- initialization((
		nl,
		write('************************************* WARNING **************************************'), nl,
		write('Your back-end Prolog compiler does not seem to support the term expansion mechanism.'), nl,
		write('Representing languages and automata using object proxies is therefore not supported.'), nl,
		write('************************************************************************************'), nl, nl
	)).

:- endif.
