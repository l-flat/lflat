#!/bin/bash
. ${0%${0##*/}}defs.sh

# This script tests the lflat.sh script using problems from a contest
# Usage:   test.sh <contest_name> <problem>
# Example: test.sh lflat-demo A

SUBMISSION=${MOOSHAK_LFLAT_HOME}contests/$1/problems/$2/Program
TEST=${MOOSHAK_LFLAT_HOME}contests/$1/problems/$2/tests/T1/input

echo '-------------------------------------------------------------'
cat /dev/null | /bin/bash ${MOOSHAK_LFLAT_HOME}lflat.sh compile ${SUBMISSION}
echo '-------------------------------------------------------------'
cat ${TEST} | /bin/bash ${MOOSHAK_LFLAT_HOME}lflat.sh execute ${SUBMISSION}
echo '-------------------------------------------------------------'
