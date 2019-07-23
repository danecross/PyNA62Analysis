
echo "\n\nPulling from SummerProject\n\n"
cd ~/SummerProject
git pull

#remake Tools and Reconstruction and Analysis
cd na62fw/
echo "\n\nPulling from na62fw\n\n"
git pull

source NA62Reconstruction/scripts/env.sh
cd NA62Tools
make clean
make 
cd ../NA62MC
make clean
make
cd ../NA62Reconstruction
make clean
make

#~/SummerProject/na62fw/NA62FWBuildRunTree.py -v rcurrent -w ~/Analysis
#cd ~/Analysis
#source envAnalysis.sh
#NA62AnalysisBuilder.py prepare MyAnalysis
#cd MyAnalysis
#source scripts/env.s

rm -r ~/Analysis
cd ../NA62Analysis
source scripts/env.sh


NA62AnalysisBuilder.py cleanall
NA62AnalysisBuilder.py config --shared

cd ../../SummerProject/



