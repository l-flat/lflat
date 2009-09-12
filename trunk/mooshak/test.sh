#!/bin/bash
. ${0%${0##*/}}defs.sh

# This script tests the lflat.sh script.


# This is a sample submission file

cat > /tmp/submission.lgt  << EOF
:- object(evenRE,
    instantiates(re)).
    
    expression((a + b * a^* * b)^*).
    
:- end_object.
EOF


# This is a sample test file

cat > /tmp/checks.lgt  << EOF

:- object(evenL,
    instantiates(language)).

    :- initialization((
		contests::setup,
		contests::diagnostics(evenL),
		contests::check_definition(re, evenRE),
		contests::diagnostics(evenRE),
		contests::test_mechanism(evenL,evenRE),
		contests::finish_checking
    )).

    alphabet([a,b]).

    positive([]).
    positive([a,a,a]).
    positive([b,b]).
    positive([a,b,b]).
    positive([b,b,a,a]).
    positive([b,a,a,a,b,b,a,b]).

    negative([b]).
    negative([b,b,b]).
    negative([b,a,a]).
    negative([a,a,b]).
    negative([b,a,a,b,b,a,a]).
:- end_object.
EOF


# Reproduce the sequence of commands issued by Mooshak for the two previous files

echo '-------------------------------------------------------------'
cat /dev/null | /bin/bash ${MOOSHAK_LFLAT_HOME}lflat.sh compile "/tmp/submission"
echo '-------------------------------------------------------------'
cat /tmp/checks.lgt | /bin/bash ${MOOSHAK_LFLAT_HOME}lflat.sh execute "/tmp/submission"
echo '-------------------------------------------------------------'
rm -f /tmp/submission.lgt /tmp/checks.lgt


