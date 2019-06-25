# SummerProject

### To Build the "Template" script:

rcurrent/NA62FWBuildRunTree.py -v rcurrent -w /chosen/path/Analysis
cd /chosen/path/Analysis
source envAnalysis.sh #This will spit so many errors but it's fine for what we're doing
NA62AnalysisBuilder.py prepare MyAnalysis
cd MyAnalysis
source scripts/env.sh
NA62AnalysisBuilder.py new MyAnalyzer.py 
##### Note: the .py at the end is important because I will make an if statement that says if there is a .py at the end make the python file not the C++ file

### Once that's done, in order to actually build and test the template code: (this is from an email that Dr. Rubin sent, but I have not run it yet)
NA62AnalysisBuilder.py build config
Choose a list of files (reconstructed or MC, depending on your choice of analyzer(s)) from MC productions or processed data
./YourExecutable -l ChosenList -o Test.root
the -h option shows more options
