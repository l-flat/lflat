L-FLAT - Logtalk Formal Language and Automata Toolkit
=====================================================

L-FLAT is a toolkit for teaching Formal Languages and Automata Theory.

L-FLAT supports the definition of alphabets, the definition of orders
over alphabet symbols, the partial definition of languages using unit
tests, and the definition of mechanisms, which implement language
generators or language recognizers. Supported mechanisms include
predicates, regular expressions, finite automata, context-free grammars,
Turing machines, and push-down automata. The use of L-FLAT in an
educational environment is enhanced by supporting Mooshak, a web
application that features automatic grading of submitted programs.

Authors
-------

- Artur Miguel Dias (CITI, Depart. de Informatica, Univ. Nova de Lisboa)
- Paulo Moura (CRACS, INESC Porto)
- Michel Wermelinger (Computing Department, The Open University)

Running L-FLAT
--------------

1. Install Logtalk (https://logtalk.org/)

2. Start Logtalk from the L-FLAT directory and type the query:

	| ?- {loader}.


Running L-FLAT Examples
-----------------------

To run all the examples provided with L-FLAT, start L-FLAT and
type the query:

	| ?- {lflat_examples(all)}.

To run a specific example, e.g. s131.lgt, type the query:

	| ?- {lflat_examples(s131)}.


L-FLAT documentation
--------------------

See the `lflat_entity_diagram.pdf` file for an overview of the L-Flat
architecture.

See the paper "L-FLAT: Logtalk Toolkit for Formal Languages and Automata
Theory" by Paulo Moura and Artur Miguel Dias for a detailed description:

http://arxiv.org/abs/1112.3783




