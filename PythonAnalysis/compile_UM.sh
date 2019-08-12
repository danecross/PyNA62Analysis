

unset PYTHONHOME
unset PYTHONPATH

#rm -r build/

export LIBRARY_PATH="/usr/local/lib"
export PYTHONPATH="/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Analysis/lib-cc7/:/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Tools/lib-cc7/Persistency/:/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Tools/lib-cc7/SlimPersistency/:/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Tools/lib-cc7/:/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/lib-cc7/:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/lib/"

python3 setup.py install --user  

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH
