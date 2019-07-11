#
# template analyzer written in Python
# Authors: Amanda Hoebel and Dane Cross
#

# 6 steps:
#
# 	1. import statements 
# 	2. configuration
# 	3. initialize analyzers
# 	4. write and run analyzers
# 	5. do post-analysis processing
# 	6. plot products

#import statements
import PyNA62Analysis.Configure as config
import PyNA62Analysis.UserMethods as um
#import ROOT

# configuration
# this forgoes the need for a config file
def configure():

	input_files = ["example.txt"] # write the path to the desired input files here
	config.set_input_files(input_files)

	
	preanalyzersList = [] #list of preanalyzers
	analyzersList = [] #list of the analyzers you will create
	
	extraLibs = []
	extralibsdirs = []
	extraincludedirs = []
	
	parameters = []

	# set boolean flags 
	# booleans you can set:
	# 	UseLogFile, graphicMode, UseDownscaling, UsePrimitiveFile, FastStart, 
	# 	SkipIsFatal, ContinuousReading, Filter, SpecialOnly, HistoMode
	# default value is False 

	args = () # put aruments here. e.g: (UseLogFile = True, FastStart = True)
	
	# other stuff
	# TODO: CoreVerbosity, AnVerbosity, 
	
	burstsToIgnore = [] # string list of bursts to ignore
	eventsToIgnore = [] # string list of events to ignore

	# mandatory call that configures our BaseAnalysis object
	config.configure() 


# initialize analyzers
# this replaces the constructor, InitOutput, InitHist, DefineMCSimple, StartOfRunUser, 
# and StartOfBurstUser
#
# here we initialize analyzers, request data, request histograms, add particles to 
# MC simulator, add parameters, etc.
# 
def initializeAnalyzer():
	print("initialize analyzer")	

# write analyzers
# this replaces the Process method
# 
# here we do the bulk processing of the data and get it ready to be put into histograms
def defineAnalyzer():
	print("define analyzer")

# perform post-analysis processing
# this replaces the PostProcess, EndOfBurstUser, and EndOfRunUser methods
#
# here we do any last-minute processing before we plot the data
def postProcess():
	print("post process")

# plot products
# this replaces EndOfJobUser and DrawPlot()
# 
# here we use ROOT to plot our prepared data. 
def plots():
	print("plots")


configure()
initializeAnalyzer()
defineAnalyzer()
#UserMethods.runAnalyzer() # this should be defined by us not the user
plots()










