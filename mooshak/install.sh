#!/bin/bash
. ${0%${0##*/}}defs.sh

# Prepares all the files required for MOOSHAK to run L-FLAT
# Requires superuser privileges

ln -sf $(dirname $(pwd)) /usr/local/
sudo -H -u ${MOOSHAK_USER} swilgt < /dev/null > /dev/null 2>&1
sudo chmod -R 666 ${LFLAT_HOME}*.pl
sudo chmod -R 777 ${MOOSHAK_HOME}logtalk
sudo chmod 777 ${MOOSHAK_LFLAT_HOME}
chmod 744 ${MOOSHAK_LFLAT_HOME}*.sh


cat > ${MOOSHAK_HOME}logtalk/settings.lgt << EOF
:- initialization((
	assertz(logtalk_library_path(lflat, '${LFLAT_HOME}')),
	set_logtalk_flag(startup_message, banner),
	set_logtalk_flag(report, off),
	set_logtalk_flag(altdirs, off),
	set_logtalk_flag(xmldocs, off)
)).
EOF

# Lang:
# Name lflat
# Extension lgt
# Compiler Logtalk
# Version 2.37.3
# Compile /bin/bash /usr/local/lflat/mooshak/lflat.sh compile $name
# Execute /bin/bash /usr/local/lflat/mooshak/lflat.sh execute $name
# MaxCompFork 10
# MaxExecFork 10
# MaxCore 10485760
# MaxData 20971520
# MaxStack 10485760
# MaxProg 102400
# RealTimeout 60
# CompTimeout 30
# ExecTimeout 30
# MinUID 30000
# MaxUID 60000 
