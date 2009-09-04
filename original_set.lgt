
:- object(set). 

	:- info([
		version is 1.0,
		author is 'Artur Miguel Dias, Michel Wermelinger, and Paulo Moura',
		date is 2007/06/21,
		comment is 'Set predicates.']).

    :- public(is_a_set/1).
	:- mode(is_a_set(-atom), one).
	:- info(is_a_set/1, [
		comment is 'Set is a set.',
		argnames is ['Set']]).

    :- public(set_split/3).
	:- mode(set_split(+nonvar, +list, -integer), zero_or_one).
	:- info(set_split/3, [
		comment is 'Set has "head" F and "tail" R.',
		argnames is ['Set', 'Head', 'Tail']]).

    :- public(set_list/2).
	:- mode(set_list(+set, ?list), zero_or_one).
	:- mode(set_list(-set, +list), one).
	:- info(set_list/2, [
		comment is 'Convert between set Set and list List.',
		argnames is ['Set', 'List']]).

    :- public(set_member/2).
	:- mode(set_member(+atom, -atom), one).
	:- info(set_member/2, [
		comment is 'Element is an element of set Set.',
		argnames is ['Element', 'Set']]).

    :- public(set_union/3).
	:- mode(set_union(+nonvar, +list, -integer), zero_or_one).
	:- info(set_union/3, [
		comment is 'Union is the union of Set1 and Set2.',
		argnames is ['Set1', 'Set2', 'Union']]).

    :- public(set_intersection/3).
	:- mode(set_intersection(+nonvar, +list, -integer), zero_or_one).
	:- info(set_intersection/3, [
		comment is 'Intersection is the intersection of Set1 and Set2.',
		argnames is ['Set1', 'Set2', 'Intersection']]).

    :- public(set_difference/3).
	:- mode(set_difference(+nonvar, +list, -integer), zero_or_one).
	:- info(set_difference/3, [
		comment is 'D is the difference between S1 and S2.',
		argnames is ['Set1', 'Set2', 'Difference']]).

    :- public(set_included/2).
	:- mode(set_included(+atom, -atom), one).
	:- info(set_included/2, [
		comment is 'S1 is a subset of S2.',
		argnames is ['', '']]).

    :- public(set_equal/2).
	:- mode(set_equal(+atom, -atom), one).
	:- info(set_equal/2, [
		comment is 'S1 and S2 have the same elements.',
		argnames is ['Set1', 'Set2']]).

    :- public(set_size/2).
	:- mode(set_size(+atom, -atom), one).
	:- info(set_size/2, [
		comment is 'Set has N elements.',
		argnames is ['Set', 'Size']]).

    :- public(compute_set/2).
	:- mode(compute_set(+atom, -atom), one).
	:- info(compute_set/2, [
		comment is 'Set is the value of the Expression.',
		argnames is ['Expression', 'Set']]).

	:- uses(term_classification, [is_element/1]).
	:- uses(list, [length/2]).

/*
Sets are represented by terms of the form {E1, E2, ...}
where Ei are unique valid terms. The empty set is {}.
*/

    is_a_set({}) :-
        !.
    is_a_set(S) :-
        set_split(S, F, R),
        is_element(F),
        is_a_set(R),
        \+ set_member(F, R).

% set_split(+/-S, ?/+F, ?/+R)
% Set is a term with First element and the (possibly empty) Rest
% this predicate corresponds to the [Head| Tail] notation for lists

    set_split({H, R}, H, {R}).
    set_split({H}, H, {}) :-
        H \= (_, _).

% set_list(+/-Set, ?/+List)
% List has the same elements as Set, in the same order
% pre: is_a_set(List)

    set_list({}, []).
    set_list(S, [F| L]) :-
        set_split(S, F, R),
        set_list(R, L).

    set_member(E, S) :-
        set_split(S, F, R),
        (   F = E
        ;   set_member(E, R)
        ).

    set_union({}, S, S) :-
        !.
    set_union(S1, S2, U) :-
	    set_split(S1, F, R),
	    set_union(R, S2, S3),
        (   set_member(F, S2) ->
            U = S3
        ;   set_split(U, F, S3)
        ).

    set_intersection({}, _, {}) :-
        !.
    set_intersection(S1, S2, I) :-
	    set_split(S1, F, R),
	    set_intersection(R, S2, S3),
	    (   set_member(F, S2) ->
	        set_split(I, F, S3)
	    ;   I = S3
	    ).

% set_difference(+Set1, +Set2, -Difference)
% Difference contains the elements that are in Set1 but not in Set2

    set_difference({}, _, {}) :-
        !.
    set_difference(S1, S2, D) :-
	    set_split(S1, F, R),
	    set_difference(R, S2, S3),
	    (   set_member(F, S2) ->
	        D = S3
	    ;   set_split(D, F, S3)
	    ).

% set_included(+Set1, +Set2)
% Set1 is a subset of Set2

    set_included({}, _) :-
        !.
    set_included(S1, S2) :-
	    set_split(S1, F, R),
	    set_member(F, S2),
	    set_included(R, S2).

    set_equal(S1, S2) :-
        set_included(S1, S2),
        set_included(S2, S1).

    set_size(S, N) :-
        set_list(S, L),
        length(L, N).

    compute_set(S, S) :-
        is_a_set(S),
        !.
    compute_set(S1+S2, S) :-
	    compute_set(S1, R1),
	    compute_set(S2, R2),
	    set_union(R1, R2, S).
    compute_set(S1-S2, S) :-
	    compute_set(S1, R1),
	    compute_set(S2, R2),
	    set_difference(R1, R2, S).
    compute_set(S1/\S2, S) :-
	    compute_set(S1, R1),
	    compute_set(S2, R2),
	    set_intersection(R1, R2, S).
    compute_set(setof(E, P), S) :-
	    (   setof(E, P, L) ->
	        set_list(S, L)
	    ;   S = {}
        ).

% write_set(+Set)
% writes one element of Set per line

    write_set({}) :-
        !, nl.
    write_set(S) :-
        set_split(S, H, T),
        write(H), nl,
        write_set(T).

:- end_object.

