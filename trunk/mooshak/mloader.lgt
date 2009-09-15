
:- initialization((
    logtalk_load(loader, [portability(silent), reload(skip)]),
	logtalk_load(mooshak, [portability(silent), reload(skip)])
)).

