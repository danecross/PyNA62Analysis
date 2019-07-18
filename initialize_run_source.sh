
cd ~/SummerProject/

#remake Tools and Reconstruction and Analysis
cd rcurrent/
source NA62Reconstruction/scripts/env.sh
cd NA62Tools
source scripts/env.sh
cd ../NA62Reconstruction
source scripts/env.sh
cd ../NA62MC
source scripts/env.sh

#prepare the user directory
~/SummerProject/rcurrent/NA62FWBuildRunTree.py -v rcurrent -w ~/Analysis
cd ~/Analysis
source envAnalysis.sh 
NA62AnalysisBuilder.py prepare MyAnalysis
cd MyAnalysis
source scripts/env.sh

cd ../../SummerProject/



