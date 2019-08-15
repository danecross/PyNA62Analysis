
echo "Pulling from PyNA62Analysis"
cd ~/PyNA62Analysis
git pull

cp changes/BaseAnalysis.cc ../na62fw/NA62Analysis/src/
cp changes/BaseAnalysis.hh ../na62fw/NA62Analysis/include/

#remake Tools and Reconstruction and Analysis
cd ../na62fw/

source NA62Reconstruction/scripts/env.sh
#source /cvmfs/sft.cern.ch/lcg/views/LCG_93python3/x86_64-slc6-gcc7-opt/setup.sh

cd NA62Tools
make clean
make 
cd ../NA62MC
make clean
make
cd ../NA62Reconstruction
make clean
make

rm -r ~/Analysis
cd ../NA62Analysis
source scripts/env.sh
./NA62AnalysisBuilder.py prepare ~/Analysis --build-user-only
cd ~/Analysis
source scripts/env.sh 

NA62AnalysisBuilder.py cleanall
NA62AnalysisBuilder.py config --shared

cd ~/PyNA62Analysis/PythonAnalysis/



