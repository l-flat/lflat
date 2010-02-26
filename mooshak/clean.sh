#!/bin/bash
. ${0%${0##*/}}defs.sh

# Cleans up the lflat/mooshak directory

sudo rm -rf ${MOOSHAK_HOME}logtalk
rm -rf ${MOOSHAK_LFLAT_HOME}.lgt_tmp ${MOOSHAK_LFLAT_HOME}*.pl
chmod 755 ${MOOSHAK_LFLAT_HOME}
