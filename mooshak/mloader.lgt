
:- initialization((
	logtalk_load(lflat(loader), [portability(silent), reload(skip)]),
	logtalk_load(lflat('mooshak/mooshak'), [portability(silent), reload(skip)])
)).

