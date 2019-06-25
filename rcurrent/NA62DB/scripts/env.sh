GCC=/afs/.cern.ch/sw/lcg/external/gcc/4.6/x86_64-slc6

#Libs
BOOST=/afs/.cern.ch/sw/lcg/external/Boost/1.53.0_python2.7/x86_64-slc6-gcc46-opt
XERCESC=/afs/.cern.ch/sw/lcg/external/XercesC/3.1.1p2/x86_64-slc6-gcc46-opt
FRONTIER=/afs/.cern.ch/work/s/spadolsk/public
PACPARSER=/afs/.cern.ch/work/s/spadolsk/public

export CORAL=/afs/.cern.ch/sw/lcg/app/releases/CORAL/CORAL_2_3_28/x86_64-slc6-gcc46-opt
export PATH=$GCC/bin:$PATH
export LD_LIBRARY_PATH=$GCC{/lib,/lib64}:$BOOST/lib:$CORAL/lib:$FRONTIER/lib:$PACPARSER/lib:$XERCESC/lib:$LD_LIBRARY_PATH

# Config.
export PROJECT_SOURCE_DIR=$( dirname $( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd ) )

export CORAL_DBLOOKUP_PATH=${PROJECT_SOURCE_DIR}
export CORAL_AUTH_PATH=${PROJECT_SOURCE_DIR}

# For the CMakeList general file
export BOOST_DIR=$BOOST
export Boost_INCLUDE_DIRS=$BOOST/include/boost-1_53
export Boost_LIBRARY_DIRS=$BOOST/lib
export CC=`which gcc`
export CXX=`which g++`
