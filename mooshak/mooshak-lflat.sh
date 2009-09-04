#/bin/bash

export LOGTALKHOME=/usr/share/logtalk
export LOGTALKUSER=/home/mooshak/logtalk

cat > input_test.lgt

echo -e "{'/usr/local/lflat/loader'}, contests::$1('$2', input_test)." | /usr/bin/swilgt | tail --lines=+4

rm -f input_test.lgt 
