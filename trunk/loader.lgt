
:- if(current_logtalk_flag(version, version(3,_,_))).

	:- initialization((
		% load Logtalk libraries used by L-FLAT:
		logtalk_load(library(types_loader), [portability(silent)]),
		logtalk_load(library(gensym), [portability(silent)]),
		logtalk_load(roots(loader), [portability(silent)]),
		% load L-FLAT itself:
		logtalk_load(lflat_home(lflat), [unknown_entities(silent), optimize(on)]),
		logtalk_load(lflat_home(hooks), [optimize(on)]),
		% print the L-FLAT banner:
		interaction::banner
	)).

:- else.

	:- initialization((
		% load Logtalk libraries used by L-FLAT:
		logtalk_load(library(gensym), [portability(silent), reload(skip)]),
		logtalk_load(roots(loader), [portability(silent), reload(skip)]),
		% load L-FLAT itself:
		logtalk_load(lflat_home(lflat), [unknown_entities(silent), optimize(on), reload(skip)]),
		logtalk_load(lflat_home(hooks), [optimize(on), reload(skip)]),
		% print the L-FLAT banner:
		interaction::banner
	)).

:- endif.

% some handy shortcuts to run the L-FLAT provided examples:
% (examples are located in the $HOME/lflat/examples directory)

run_example(Example) :-
	interaction::run_example(Example).

run_examples(List) :-
	interaction::run_examples(List).

all :-
	run_example(all).
