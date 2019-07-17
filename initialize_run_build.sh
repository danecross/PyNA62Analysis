
#remake Tools and Reconstruction and Analysis
cd rcurrent/
source NA62Reconstruction/scripts/env.sh
cd NA62Tools
make clean
make 
cd ../NA62Reconstruction
make clean
make
cd ../NA62Analysis
make clean
make
source scripts/env.sh

source initialize_run_source.sh

#prepare the user directory
#~/SummerProject/rcurrent/NA62FWBuildRunTree.py -v rcurrent -w ~/Analysis
#cd ~/Analysis
#source envAnalysis.sh 
#NA62AnalysisBuilder.py prepare MyAnalysis
#cd MyAnalysis
#source scripts/env.sh

#make a new *python* analyzer
###NA62AnalysisBuilder.py new analyzer-Name.py


#cd ../../SummerProject/



