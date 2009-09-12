#!/bin/bash
. ${0%${0##*/}}defs.sh

# Cleans up the lflat/mooshak directory

rm -f ${MOOSHAK_LFLAT_HOME}*.pl ${MOOSHAK_LFLAT_HOME}*.lgt ${MOOSHAK_LFLAT_HOME}examples/*.pl
rm -rf ${MOOSHAK_LFLAT_HOME}.lgt_tmp ${MOOSHAK_LFLAT_HOME}examples/.lgt_tmp
chmod 755 ${MOOSHAK_LFLAT_HOME}
