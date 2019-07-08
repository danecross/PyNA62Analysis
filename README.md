# SummerProject


I made scripts so we don't have to keep typing the commands listed below. When you first enter the environment, run:

	source initialize_run.sh

To run update the Analyzer with the python template:

	source build_analyzer.sh

### Once that's done, in order to actually build and test the template code: 

if it's the first time running the code after a `git pull`:

	source initialize_run_build.sh
	source create_analyzer.sh
	
if you just need to set env variables:
	
	source initialize_run_source.sh
	source create analyzer.sh
	
Once you have everything how you want it (ie you have rewritten the templateAnalyzer code and are ready to compile):
	
	source build_analyzer.sh
	
take a close look at what is inside of build_analyzer.sh. If you are having problems with your results take a look at what you're compiling and where it goes. 
