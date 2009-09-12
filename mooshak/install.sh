#!/bin/bash
. ${0%${0##*/}}defs.sh

# Prepares all the files required for MOOSHAK to run L-FLAT
# Requires superuser privileges

sudo -H -u ${MOOSHAK_USER} swilgt < /dev/null > /dev/null 2>&1
sudo chmod -R 777 ${MOOSHAK_HOME}logtalk
sudo chmod 777 ${MOOSHAK_LFLAT_HOME}
chmod 744 ${MOOSHAK_LFLAT_HOME}*.sh

cat > ${MOOSHAK_HOME}logtalk/settings.lgt  << EOF
:- initialization((
    assertz(logtalk_library_path(lflat_home, '${MOOSHAK_LFLAT_HOME}')),
    assertz(logtalk_library_path(lflat_examples, lflat_home('examples/'))),
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
# Fork 10
