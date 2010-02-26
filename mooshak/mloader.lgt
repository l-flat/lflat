
:- initialization((
	logtalk_load(lflat_home(loader), [portability(silent), reload(skip)]),
	logtalk_load(lflat_home('mooshak/mooshak'), [portability(silent), reload(skip)])
)).

