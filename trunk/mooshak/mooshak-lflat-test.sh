#!/bin/sh

# This script tests the mooshak-lflat.sh script.


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
        ::diagnostics,
        contests::check_definition(re, evenRE),
        evenRE::diagnostics,
        ::test_mechanism(evenRE)
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


#:- evenL::diagnostics.
#:- ac_check_declared(re, evenRE).
#:- ac_check_declaration(evenRE).
#:- ac_test_definition(evenRE, evenL).



# Reproduce the sequence of commands issued by Mooshak for the two previous files

APP_DIR=${0%${0##*/}}

echo $APP_DIR
echo '-------------------------------------------------------------'
cat /dev/null | /bin/bash ${APP_DIR}mooshak-lflat.sh compile "/tmp/submission"
echo '-------------------------------------------------------------'
cat /tmp/checks.lgt | /bin/bash ${APP_DIR}mooshak-lflat.sh execute "/tmp/submission"
echo '-------------------------------------------------------------'
rm -f /tmp/submission.lgt /tmp/checks.lgt

