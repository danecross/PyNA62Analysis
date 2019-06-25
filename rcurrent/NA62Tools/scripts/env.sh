#!/bin/sh

if [ -z $NA62TOOLSSOURCE ]; then
    DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    export NA62TOOLSSOURCE=$(dirname $(dirname $DIR))/NA62Tools
fi

# Env for relocatable ROOT dictionaries
for PERS_INC_DIR in `find $NA62TOOLSSOURCE/Persistency/SlimReco -maxdepth 1 -type d `;do
        ROOT_INCLUDE_PATH+="$PERS_INC_DIR/include:"
done
for PERS_INC_DIR in `find $NA62TOOLSSOURCE/Persistency/FullReco  -maxdepth 1 -type d`;do
        ROOT_INCLUDE_PATH+="$PERS_INC_DIR/include:"
done 
export ROOT_INCLUDE_PATH=${ROOT_INCLUDE_PATH%?}

LCG95=1

CC7=`grep " 7\." /etc/redhat-release | wc -l`
SLC6=`grep " 6\." /etc/redhat-release | wc -l`
if [ "$CC7" -gt 0 ]; then
    #echo "Congratulations! You are using na62fw with Centos7!"
    if [ $LCG95 -gt 0 ]; then
        ###### LCG 95
        source /cvmfs/sft.cern.ch/lcg/contrib/gcc/7/x86_64-centos7/setup.sh
        source /cvmfs/sft.cern.ch/lcg/releases/LCG_95/Geant4/10.05/x86_64-centos7-gcc7-opt/bin/geant4.sh
        source /cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/bin/thisroot.sh
        export LCGDIR=/cvmfs/sft.cern.ch/lcg/views/LCG_95/x86_64-centos7-gcc7-opt
        export BOOST=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Boost/1.69.0/x86_64-centos7-gcc7-opt
        export BOOSTCOMP=""
        export BOOSTVER=""
        export SQLITE=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/sqlite/3210000/x86_64-centos7-gcc7-opt
        export XROOTD=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/xrootd/4.8.4/x86_64-centos7-gcc7-opt
        export FFTW3=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/fftw3/3.3.4/x86_64-centos7-gcc7-opt
        export TBB=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/tbb/2019_U1/x86_64-centos7-gcc7-opt
        export CMAKE=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/CMake/3.11.1/x86_64-centos7-gcc7-opt
        export GSL=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/GSL/2.5/x86_64-centos7-gcc7-opt
        export CLHEP=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/clhep/2.4.1.0/x86_64-centos7-gcc7-opt
	export QTDIR=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/qt5/5.11.1/x86_64-centos7-gcc7-opt
    else
        ###### LCG 93
        source /cvmfs/sft.cern.ch/lcg/contrib/gcc/7/x86_64-centos7/setup.sh
        source /cvmfs/sft.cern.ch/lcg/releases/LCG_93/Geant4/10.04/x86_64-centos7-gcc7-opt/bin/geant4.sh
        source /cvmfs/sft.cern.ch/lcg/releases/LCG_93/ROOT/6.12.06/x86_64-centos7-gcc7-opt/bin/thisroot.sh
        export LCGDIR=/cvmfs/sft.cern.ch/lcg/views/LCG_93/x86_64-centos7-gcc7-opt
        export BOOST=/cvmfs/sft.cern.ch/lcg/releases/LCG_93/Boost/1.66.0/x86_64-centos7-gcc7-opt
        export BOOSTCOMP=""
        export BOOSTVER=""
        export SQLITE=/cvmfs/sft.cern.ch/lcg/releases/LCG_93/sqlite/3210000/x86_64-centos7-gcc7-opt
        export XROOTD=/cvmfs/sft.cern.ch/lcg/releases/LCG_93/xrootd/4.7.0/x86_64-centos7-gcc7-opt
        export FFTW3=/cvmfs/sft.cern.ch/lcg/releases/LCG_93/fftw3/3.3.4/x86_64-centos7-gcc7-opt
        export TBB=/cvmfs/sft.cern.ch/lcg/releases/LCG_93/tbb/2018_U1/x86_64-centos7-gcc7-opt
        export CMAKE=/cvmfs/sft.cern.ch/lcg/contrib/CMake/3.8.1/Linux-x86_64
        export GSL=/cvmfs/sft.cern.ch/lcg/releases/LCG_93/GSL/2.1/x86_64-centos7-gcc7-opt
        export CLHEP=/cvmfs/sft.cern.ch/lcg/releases/LCG_93/clhep/2.4.0.1/x86_64-centos7-gcc7-opt
	export QTDIR=/cvmfs/sft.cern.ch/lcg/releases/LCG_93/qt5/5.9.2/x86_64-centos7-gcc7-opt
    fi
    export SYSTEMINSTALL=cc7
    export QTINC=${QTDIR}/include
    export QTLIB=${QTDIR}/lib
    export QT_PLUGIN_PATH=${QTDIR}/plugins
    export FONTCONFIG_PATH=/etc/fonts    
elif [ $SLC6 -gt 0 ]; then
    ### slc6
    source /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/setup.sh ""
    source /cvmfs/sft.cern.ch/lcg/releases/LCG_86/Geant4/10.01.p02/x86_64-slc6-gcc49-opt/share/Geant4-10.1.2/geant4make/geant4make.sh
    source /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/bin/thisroot.sh
    export LCGDIR=/cvmfs/sft.cern.ch/lcg/views/LCG_86/x86_64-slc6-gcc49-opt
    export BOOST=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-opt
    export BOOSTCOMP="-gcc49-mt"
    export BOOSTVER="-1_62"
    export SQLITE=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/sqlite/3110100/x86_64-slc6-gcc49-opt
    export XROOTD=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/xrootd/4.4.1/x86_64-slc6-gcc49-opt
    export FFTW3=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/fftw3/3.3.4/x86_64-slc6-gcc49-opt
    export TBB=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/tbb/44_20160413/x86_64-slc6-gcc49-opt
    export CMAKE=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/CMake/3.5.2/x86_64-slc6-gcc49-opt
    export GSL=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/GSL/2.1/x86_64-slc6-gcc49-opt
    export CLHEP=/cvmfs/sft.cern.ch/lcg/releases/LCG_86/clhep/2.3.1.1/x86_64-slc6-gcc49-opt
    export SYSTEMINSTALL=slc6
else
    echo "[NA62FW] ERROR: This OS is not supported!"
    cat /etc/redhat-release
    return
fi

# add tools lib directory to LD_LIBRARY_PATH
export LD_LIBRARY_PATH=${NA62TOOLSSOURCE}/lib-${SYSTEMINSTALL}/:${NA62TOOLSSOURCE}/lib-${SYSTEMINSTALL}/Persistency:${NA62TOOLSSOURCE}/lib-${SYSTEMINSTALL}/SlimPersistency:$LD_LIBRARY_PATH
## add needed libraries to library path
export LD_LIBRARY_PATH=${LCGDIR}/lib:${LCGDIR}/lib64:${XROOTD}/lib64:$TBB/lib:$XERCESCROOT/lib:$QTHOME/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$BOOST/lib:$SQLITE/lib:$FFTW3/lib:$GSL/lib:$LD_LIBRARY_PATH
export PATH=${CMAKE}/bin:${SQLITE}/bin:${XROOTD}/bin:${PATH}

#use LCG python unless a custom one is already specified
if [ `which python` == "/usr/bin/python" ] || [ `which python` == "/bin/python" ]; then
  PYTHON_VER=`ls -l ${LCGDIR}/bin | awk '{ print $9 }' | grep "^python[0-9]\.[0-9]\+$" | head -1`
  export PYTHONPATH=${LCGDIR}/lib:${LCGDIR}/lib/${PYTHON_VER}/site-packages:${PYTHONPATH}
  export PYTHONHOME=$(dirname $(dirname $(readlink $LCGDIR/bin/python)))
  export PATH=${PYTHONHOME}/bin:${PATH}
fi

gccPath=`which gcc`
export NA62FW_STDLIBSPATH="$(dirname $(dirname "$gccPath"))"
export CC=`which gcc`
export CXX=`which g++`
export CMAKE_PREFIX_PATH=${NA62TOOLSSOURCE}/config:${CLHEP}:${CMAKE_PREFIX_PATH}
