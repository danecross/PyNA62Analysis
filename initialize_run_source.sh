
#source /cvmfs/sft.cern.ch/lcg/views/LCG_93python3/x86_64-slc6-gcc7-opt/setup.sh

cd ~/SummerProject/
#git stash
#git pull
#git stash pop

#remake Tools and Reconstruction and Analysis
cd na62fw/
#git stash
#git pull
#git stash pop

source NA62Reconstruction/scripts/env.sh
cd NA62Tools
source scripts/env.sh
cd ../NA62Reconstruction
source scripts/env.sh
cd ../NA62MC
source scripts/env.sh
cd ../NA62Analysis
source scripts/env.sh

cd ~/Analysis
source scripts/env.sh

cd ~/SummerProject/na62fw/NA62Analysis/PythonAnalysis/

source /cvmfs/sft.cern.ch/lcg/views/LCG_93python3/x86_64-slc6-gcc7-opt/setup.sh

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Analysis/lib-cc7/:$ROOTSYS:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Boost/1.69.0/x86_64-slc6-gcc7-opt/lib/

