
cd ~/SummerProject/
git pull

#remake Tools and Reconstruction and Analysis
cd na62fw/
git pull

source NA62Reconstruction/scripts/env.sh
cd NA62Tools
source scripts/env.sh
cd ../NA62Reconstruction
source scripts/env.sh
cd ../NA62MC
source scripts/env.sh
cd ../NA62Analysis
source scripts/env.sh


#prepare the user directory
#~/SummerProject/na62fw/NA62Analysis/NA62AnalysisBuilder.py -v rcurrent -w ~/Analysis
#cd ~/Analysis
#source envAnalysis.sh 
#NA62AnalysisBuilder.py prepare MyAnalysis
#cd MyAnalysis
#source scripts/env.sh


#cd ~/Analysis
#source scripts/env.sh

#cd ../SummerProject/

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Analysis/lib-cc7/

