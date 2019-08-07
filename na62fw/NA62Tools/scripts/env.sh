#!/bin/sh

# FW Directory
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DIR=$(dirname $(dirname ${DIR}))

source ${DIR}/NA62Tools/scripts/functions.sh

if [ -z "$NA62TOOLSSOURCE" ]; then
    export NA62TOOLSSOURCE=${DIR}/NA62Tools
else
    return # Already sourced. Exit to avoid multiplying the same information in the variables
fi

# Env for relocatable ROOT dictionaries
if [ -z "$ROOT_INCLUDE_PATH" ]; then
    for PERS_INC_DIR in `find $NA62TOOLSSOURCE/Persistency/SlimReco -maxdepth 1 -type d `;do
        ROOT_INCLUDE_PATH+="$PERS_INC_DIR/include:"
    done
    for PERS_INC_DIR in `find $NA62TOOLSSOURCE/Persistency/FullReco  -maxdepth 1 -type d`;do
        ROOT_INCLUDE_PATH+="$PERS_INC_DIR/include:"
    done 
    export ROOT_INCLUDE_PATH=${ROOT_INCLUDE_PATH%?}
fi

if [ -z "$LCG96" ]; then
    LCG96=1
fi

BUILDMODE=opt

CC7=`grep " 7\." /etc/redhat-release | wc -l`
SLC6=`grep " 6\." /etc/redhat-release | wc -l`
if [ "$CC7" -gt 0 ]; then
    #echo "Congratulations! You are using na62fw with Centos7!"
    if [ $LCG96 -gt 0 ]; then
        ###### LCG 96
        source /cvmfs/sft.cern.ch/lcg/contrib/gcc/8/x86_64-centos7/setup.sh
        source /cvmfs/sft.cern.ch/lcg/releases/LCG_96_NA62_b/Geant4/10.05.p01.1/x86_64-centos7-gcc8-${BUILDMODE}/bin/geant4.sh
        source /cvmfs/sft.cern.ch/lcg/releases/LCG_96_NA62_b/ROOT/6.16.00/x86_64-centos7-gcc8-${BUILDMODE}/bin/thisroot.sh
        export LCGDIR=/cvmfs/sft.cern.ch/lcg/views/LCG_96_NA62_b/x86_64-centos7-gcc8-${BUILDMODE}
        export BOOST=/cvmfs/sft.cern.ch/lcg/releases/LCG_96_NA62_b/Boost/1.70.0/x86_64-centos7-gcc8-${BUILDMODE}
        export BOOSTCOMP=""
        export BOOSTVER=""
        export SQLITE=/cvmfs/sft.cern.ch/lcg/releases/LCG_96_NA62_b/sqlite/3280000/x86_64-centos7-gcc8-${BUILDMODE}
        export XROOTD=/cvmfs/sft.cern.ch/lcg/releases/LCG_96_NA62_b/xrootd/4.9.1/x86_64-centos7-gcc8-${BUILDMODE}
        export FFTW3=/cvmfs/sft.cern.ch/lcg/releases/LCG_96_NA62_b/fftw3/3.3.8/x86_64-centos7-gcc8-${BUILDMODE}
        export TBB=/cvmfs/sft.cern.ch/lcg/releases/LCG_96_NA62_b/tbb/2019_U7/x86_64-centos7-gcc8-${BUILDMODE}
        export CMAKE=/cvmfs/sft.cern.ch/lcg/releases/LCG_96_NA62_b/CMake/3.14.3/x86_64-centos7-gcc8-${BUILDMODE}
        export GSL=/cvmfs/sft.cern.ch/lcg/releases/LCG_96_NA62_b/GSL/2.5/x86_64-centos7-gcc8-${BUILDMODE}
        export CLHEP=/cvmfs/sft.cern.ch/lcg/releases/LCG_96_NA62_b/clhep/2.4.1.0/x86_64-centos7-gcc8-${BUILDMODE}
        export QTDIR=/cvmfs/sft.cern.ch/lcg/releases/LCG_96_NA62_b/qt5/5.12.1/x86_64-centos7-gcc8-${BUILDMODE}
    else
        ###### LCG 95
        source /cvmfs/sft.cern.ch/lcg/contrib/gcc/7/x86_64-centos7/setup.sh
        source /cvmfs/sft.cern.ch/lcg/releases/LCG_95/Geant4/10.05/x86_64-centos7-gcc7-${BUILDMODE}/bin/geant4.sh
        source /cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-${BUILDMODE}/bin/thisroot.sh
        export LCGDIR=/cvmfs/sft.cern.ch/lcg/views/LCG_95/x86_64-centos7-gcc7-${BUILDMODE}
        export BOOST=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Boost/1.69.0/x86_64-centos7-gcc7-${BUILDMODE}
        export BOOSTCOMP=""
        export BOOSTVER=""
        export SQLITE=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/sqlite/3210000/x86_64-centos7-gcc7-${BUILDMODE}
        export XROOTD=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/xrootd/4.8.4/x86_64-centos7-gcc7-${BUILDMODE}
        export FFTW3=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/fftw3/3.3.4/x86_64-centos7-gcc7-${BUILDMODE}
        export TBB=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/tbb/2019_U1/x86_64-centos7-gcc7-${BUILDMODE}
        export CMAKE=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/CMake/3.11.1/x86_64-centos7-gcc7-${BUILDMODE}
        export GSL=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/GSL/2.5/x86_64-centos7-gcc7-${BUILDMODE}
        export CLHEP=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/clhep/2.4.1.0/x86_64-centos7-gcc7-${BUILDMODE}
        export QTDIR=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/qt5/5.11.1/x86_64-centos7-gcc7-${BUILDMODE}
    fi
    export SYSTEMINSTALL=cc7
    export QTINC=${QTDIR}/include
    export QTLIB=${QTDIR}/lib
    export QT_PLUGIN_PATH=${QTDIR}/plugins
    export FONTCONFIG_PATH=/etc/fonts    
elif [ $SLC6 -gt 0 ]; then
    ### slc6
    source /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/setup.sh ""
    source /cvmfs/sft.cern.ch/lcg/releases/LCG_86/Geant4/10.01.p02/x86_64-slc6-gcc49-${BUILDMODE}/share/Geant4-10.1.2/geant4make/geant4make.sh
    source /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-${BUILDMODE}/bin/thisroot.sh
    export LCGDIR=/cvmfs/sft.cern.ch/lcg/views/LCG_86/x86_64-slc6-gcc49-${BUILDMODE}
    export BOOST=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-${BUILDMODE}
    export BOOSTCOMP="-gcc49-mt"
    export BOOSTVER="-1_62"
    export SQLITE=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/sqlite/3110100/x86_64-slc6-gcc49-${BUILDMODE}
    export XROOTD=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/xrootd/4.4.1/x86_64-slc6-gcc49-${BUILDMODE}
    export FFTW3=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/fftw3/3.3.4/x86_64-slc6-gcc49-${BUILDMODE}
    export TBB=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/tbb/44_20160413/x86_64-slc6-gcc49-${BUILDMODE}
    export CMAKE=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/CMake/3.5.2/x86_64-slc6-gcc49-${BUILDMODE}
    export GSL=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/GSL/2.1/x86_64-slc6-gcc49-${BUILDMODE}
    export CLHEP=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/clhep/2.3.1.1/x86_64-slc6-gcc49-${BUILDMODE}
    export SYSTEMINSTALL=slc6
else
    echo "[NA62FW] ERROR: This OS is not supported!"
    cat /etc/redhat-release
    return
fi

# add tools lib directory to LD_LIBRARY_PATH
pathappend LD_LIBRARY_PATH ${NA62TOOLSSOURCE}/lib-${SYSTEMINSTALL}/SlimPersistency ${NA62TOOLSSOURCE}/lib-${SYSTEMINSTALL}/Persistency ${NA62TOOLSSOURCE}/lib-${SYSTEMINSTALL}
## add needed libraries to library path
pathappend LD_LIBRARY_PATH ${LCGDIR}/lib ${LCGDIR}/lib64 ${XROOTD}/lib64 $TBB/lib
pathappend LD_LIBRARY_PATH $BOOST/lib $SQLITE/lib $FFTW3/lib $GSL/lib
pathappend PATH ${CMAKE}/bin ${SQLITE}/bin ${XROOTD}/bin
pathappend ROOT_INCLUDE_PATH ${ROOTSYS}/include

#use LCG python unless a custom one is already specified
if [ `which python` == "/usr/bin/python" ] || [ `which python` == "/bin/python" ]; then
  PYTHON_VER=`ls -l ${LCGDIR}/bin | awk '{ print $9 }' | grep "^python[0-9]\.[0-9]\+$" | head -1`
  pathappend PYTHONPATH ${LCGDIR}/lib ${LCGDIR}/lib/${PYTHON_VER}/site-packages
  export PYTHONHOME=$(dirname $(dirname $(readlink $LCGDIR/bin/python)))
  pathappend PATH ${PYTHONHOME}/bin
fi

gccPath=`which gcc`
export NA62FW_STDLIBSPATH="$(dirname $(dirname "$gccPath"))"
export CC=`which gcc`
export CXX=`which g++`
pathappend CMAKE_PREFIX_PATH ${NA62TOOLSSOURCE}/config ${CLHEP}
