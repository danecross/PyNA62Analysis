# SummerProject


I made scripts so we don't have to keep typing the commands listed below. When you first enter the environment, run:

	source initialize_run.sh

To run update the Analyzer with the python template:

	source build_analyzer.sh

### Once that's done, in order to actually build and test the template code: 
##### (this is from an email that Dr. Rubin sent, but I have not run it yet)
	NA62AnalysisBuilder.py build config
	
Choose a list of files (reconstructed or MC, depending on your choice of analyzer(s)) from MC productions or processed data

	./YourExecutable -l ChosenList -o Test.root
	the -h option shows more options
