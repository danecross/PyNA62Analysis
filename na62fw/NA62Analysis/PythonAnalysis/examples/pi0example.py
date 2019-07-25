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
import PyNA62Analysis.UserMethods as um
from PyNA62Analysis.PyBaseAnalysis import PyBaseAnalysis as BaseAnalysis
from PyNA62Analysis.PyAnalyzer import PyAnalysis as Analyzer
#import ROOT

# configuration
# this forgoes the need for a config file
def configure():

	ban = BaseAnalysis() #initialize base analysis object

#	input_files = ["/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Analysis/PythonAnalysis/examples/example.txt"] # write the path to the desired input files here
#	setattr(ban, "input_files", input_files)

	log_file = ""
#	outputFile = "examplePi0"
	#setattr(ban, "outputFile", outputFile)
	primitiveFile = ""
	
	extraLibs = []
	#setattr...
	extralibsdirs = []
	#setattr...
	extraincludedirs = []
	#setattr...
	
#	parameters = "f"
	#setattr...
	
	coreVerbosity = "extended" ; anVerbosity = "always"
#	setattr(ban, "coreVerbosity", coreVerbosity)

	# set boolean flags 
	# booleans you can set: 
	# 	ban.graphicMode        ban.useDownscaling     
	# 	ban.fastStart          ban.histoMode          ban.skipIsFatal        ban.useLogFile         
	# 	ban.continuousReading  ban.filter             ban.specialOnly        ban.usePrimitiveFile
	# default value is False 
	# set these values with setattr, like above. For example:
	
#	setattr(ban, "histoMode", True)
	
	# other stuff

	#create and add analyzers to base analysis object. 
	an1 = Analyzer()
	an2 = Analyzer()

#	setattr(ban, "analyzers", [an1, an2])

	# mandatory call that configures our BaseAnalysis object
	ban.configure()

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


baseAn = configure()
initializeAnalyzer()
defineAnalyzer()
#UserMethods.runAnalyzer() # this should be defined by us not the user
plots()










