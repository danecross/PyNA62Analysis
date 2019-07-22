#!/bin/sh

if [ -z "$ROOTSYS" ]; then
	echo "Source NA62 environment first"
	exit 1
fi

if [ $# -eq 0 ]; then
	echo "Please provide the full command that you want to test (including all the arguments)"
	exit 1
fi

LCG_PATH=`echo $ROOTSYS | sed -e 's,\(.*LCG_[0-9]*\).*,\1,'`
GCC_VER=`echo $ROOTSYS | sed -e 's,.*\(x86_.*\),\1,'`

VALGRIND_VER=`ls $LCG_PATH/valgrind/ | head -n 1`

source $LCG_PATH/valgrind/$VALGRIND_VER/$GCC_VER/valgrind-env.sh

echo " ########  Ready to run valgrind"
echo " ########    - Executing \"$@\""
echo ""

valgrind --log-file=log.log --leak-check=full --track-origins=yes --suppressions=$ROOTSYS/etc/valgrind-root.supp $@

echo ""
echo ""
echo " ########  Done "
