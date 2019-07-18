
#remake Tools and Reconstruction and Analysis
cd rcurrent/
source NA62Reconstruction/scripts/env.sh
cd NA62Tools
make clean
make
cd ../NA62Reconstruction
make clean
make

~/SummerProject/rcurrent/NA62FWBuildRunTree.py -v rcurrent -w ~/Analysis
cd ~/Analysis
source envAnalysis.sh
NA62AnalysisBuilder.py prepare MyAnalysis
cd MyAnalysis
source scripts/env.sh

NA62AnalysisBuilder.py cleanall
NA62AnalysisBuilder.py build ~/Analysis/config --shared

cd ../../SummerProject/



