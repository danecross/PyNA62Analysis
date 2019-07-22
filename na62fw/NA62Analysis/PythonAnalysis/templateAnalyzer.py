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
import PyNA62Analysis.UserMethods as um
import PyNA62Analysis.PyBaseAnalysis as BaseAnalysis
#import ROOT

# configuration
# this forgoes the need for a config file
def configure():

	ban = BaseAnalysis.PyBaseAnalysis() #initialize base analysis object

	input_files = ["example.txt"] # write the path to the desired input files here
	setattr(ban, "input_files", input_files)

	log_file = ""
	outputFile = ""
	primitiveFile = ""
	
#	preanalyzersList = [] #list of preanalyzers
	#setattr(ban, "pre_analyzers", preanalyzersList)
#	analyzersList = [] #list of the analyzers you will create
	#setattr(ban, "analyzers", analyzersList)

	extraLibs = []
	#setattr...
	extralibsdirs = []
	#setattr...
	extraincludedirs = []
	#setattr...
	
	parameters = ""
	#setattr...
	
	coreVerbosity = "" ; anVerbosity = ""
	#setattr...

	# set boolean flags 
	# booleans you can set: 
	# 	ban.graphicMode        ban.useDownscaling     
	# 	ban.fastStart          ban.histoMode          ban.skipIsFatal        ban.useLogFile         
	# 	ban.continuousReading  ban.filter             ban.specialOnly        ban.usePrimitiveFile
	# default value is False 
	# set these values with setattr, like above. For example:
	
	setattr(ban, "histoMode", True)
	
	# other stuff
	
#	burstsToIgnore = [] # string list of bursts to ignore
#	eventsToIgnore = [] # string list of events to ignore

	# mandatory call that configures our BaseAnalysis object
	ban.configure()


	#create and add analyzers to base analysis object. 
	an1 = Analyzer()
	an2 = Analyzer()
	 
	ban.addAnalyzer(an1)
	ban.addAnalyzer(an2)

	return ban


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










