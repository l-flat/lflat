export LFLATUSER=/usr/local/lflat

rm -rf $LFLATUSER
mkdir $LFLATUSER
chmod 777 $LFLATUSER
cp -a *lgt  $LFLATUSER
cp -a mooshak* $LFLATUSER
rm -f examples/*pl
cp -a examples $LFLATUSER
