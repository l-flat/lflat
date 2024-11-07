
:- initialization((
	logtalk_load_context(directory, Directory),
	assertz(logtalk_library_path(lflat, Directory)),
	assertz(logtalk_library_path(lflat_examples, lflat('examples/'))),
	% load Logtalk libraries used by L-FLAT:
	logtalk_load(types(loader)),
	logtalk_load(sets(loader)),
	logtalk_load(gensym(loader)),
	logtalk_load(roots(loader)),
	% load L-FLAT itself:
	logtalk_load(lflat, [unknown_entities(silent), optimize(on)]),
	logtalk_load(hooks, [optimize(on)]),
	% print the L-FLAT banner:
	interaction::banner
)).


% some handy shortcuts to run the L-FLAT provided examples:
% (examples are located in the $HOME/lflat/examples directory)

run_example(Example) :-
	interaction::run_example(Example).

run_examples(List) :-
	interaction::run_examples(List).

all :-
	run_example(all).
