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

#import os
#print("\n\nlibrary path: ", os.environ.get("LD_LIBRARY_PATH"))
#print("\n\npython home: ", os.environ.get("PYTHONHOME"))
#print("\n\npython path: ", os.environ.get("PYTHONPATH"))

#import statements
from PyNA62Analysis.PyBaseAnalysis import PyBaseAnalysis as BaseAnalysis
from PyNA62Analysis.PyAnalyzer import PyAnalysis as Analyzer
#import ROOT

# configuration
# this forgoes the need for a config file
# to set an attribute to the base analysis instance, you must call setattr(ban, "parameterName", value)
def configure():

	ban = BaseAnalysis.PyBaseAnalysis() #initialize base analysis object

	input_files = [] # write the path to a file containing paths to data files
	setattr(ban, "input_files", input_files)

	log_file = ""
	outputFile = "templateOutput"
	primitiveFile = ""
	#setattr...
	
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

	print("\n\nattributes in python set \n\n")

	# mandatory call that configures our BaseAnalysis object
	ban.configure()

	print("configure done")

	return ban


# initialize analyzers
# this replaces the constructor, InitOutput, InitHist, DefineMCSimple, StartOfRunUser, 
# and StartOfBurstUser
#
# here we initialize analyzers, request data, request histograms, add particles to 
# MC simulator, add parameters, etc.
# 
def initializeAnalyzer(ban):
	#create and add analyzers to base analysis object. 
    	an1 = Analyzer()
       	an2 = Analyzer()

      	ban.addAnalyzer(an1)
 	ban.addAnalyzer(an2)
	
	print("analyzers initialized")
	
	return ban

# write analyzers
# this replaces the Process method
# 
# here we do the bulk processing of the data and get it ready to be put into histograms
def runAnalyzer():
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


baseAn = configure()
initializeAnalyzer()
runAnalyzer()
plots()










