#
# template analyzer written in Python
# Authors: Amanda Hoebel and Dane Cross
#

# 6 steps:
#
# 	1. import statements 
# 	2. configuration for BaseAnalysis
# 	3. configuration for Analyzers
# 	4. operate on Analyzers
# 	5. do post-analysis processing
# 	6. plot products

#import statements
from PyNA62Analysis.PyBaseAnalysis import PyBaseAnalysis as BaseAnalysis
from PyNA62Analysis.PyAnalyzer import PyAnalysis as Analyzer
#import ROOT

import os

# configuration
# this forgoes the need for a config file
# to set an attribute to the base analysis instance, you must call setattr(ban, "parameterName", value)
def configure():

	ban = BaseAnalysis() #initialize base analysis object

	input_files = [] # write the path to a file containing paths to data files
	#setattr(ban, "input_files", input_files)

	log_file = ""
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
	
	coreVerbosity = "normal" ; anVerbosity = "normal"
	setattr(ban, "coreVerbosity", coreVerbosity)
	setattr(ban, "anVerbosity", anVerbosity)

	# set boolean flags 
	# booleans you can set: 
	# 	ban.graphicMode        ban.useDownscaling     
	# 	ban.fastStart          ban.histoMode          ban.skipIsFatal
	# 	ban.continuousReading  ban.filter             ban.specialOnly
	# DEFAULT VALUE IS False 
	# set these values with setattr, like above. For example:
	
	#setattr(ban, "histoMode", True)
	
	# other stuff
	
	# burst and event checking: set all to true, just use True, to set specific 
        # trackers, use a list:

        #setattr(ban, "noCheckBadBurst", ["GigaTracker", "SAV"])
        #setattr(ban, "noCheckDetectors", True)

	#create and add analyzers to base analysis object. 
	an1 = Analyzer("Analyzer1")
	an2 = Analyzer("Analyzer2")

	ban.addAnalyzer(an1);
	ban.addAnalyzer(an2);

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
def configureAnalyzer(ban):
	analyzers = ban.analyzers

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
configureAnalyzer(baseAn)
runAnalyzer()
plots()










