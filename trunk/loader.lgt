
:- initialization((
	% load Logtalk libraries used by L-FLAT:
	logtalk_load(library(types_loader), [portability(silent), reload(skip)]),
	logtalk_load(library(gensym), [portability(silent), reload(skip)]),
	logtalk_load(roots(loader), [portability(silent), reload(skip)]),
	% load L-FLAT itself:
	logtalk_load(lflat_home(lflat), [portability(warning), reload(skip)]),
	logtalk_load(lflat_home(hooks), [portability(warning), reload(skip)]),
	% print the L-FLAT banner:
	interaction::banner
)).


% some handy shortcuts to run the L-FLAT provided examples:

run_example(Example) :-
	interaction::run_example(Example).

run_examples(List) :-
	interaction::run_examples(List).

all :-
	run_example(all).
