#
# template analyzer written in Python
# Authors: Amanda Hoebel and Dane Cross
#

# 6 steps:
#
# 	1. import statements 
# 	2. configuration
# 	3. configure analyzers
# 	4. write and run analyzers
# 	5. do post-analysis processing
# 	6. plot products

#import os

#import statements
from PyNA62Analysis.PyBaseAnalysis import PyBaseAnalysis as BaseAnalysis
from PyNA62Analysis.PyAnalyzer import PyAnalysis as Analyzer
#import ROOT

# configuration
# this forgoes the need for a config file
def configure(currentPath):

	ban = BaseAnalysis() #initialize base analysis object
	setattr(ban, "currentPath", currentPath) #mandatory path setting

	# write the path to files which contain paths to your input files. 
	input_files = [currentPath + "/examples/example.txt"] 
	setattr(ban, "input_files", input_files)

	log_file = ""
	primitiveFile = ""
	
	extraLibs = []
	#setattr...
	extralibsdirs = []
	#setattr...
	extraincludedirs = []
	#setattr...
	
	coreVerbosity = "extended" ; anVerbosity = "normal"
	setattr(ban, "coreVerbosity", coreVerbosity)
	setattr(ban, "anVerbosity", anVerbosity)

	# set boolean flags 
	# booleans you can set: 
	# 	ban.graphicMode        ban.useDownscaling     
	# 	ban.fastStart          ban.histoMode          ban.skipIsFatal
	# 	ban.continuousReading  ban.filter             ban.specialOnly
	# default value is False 
	# set these values with setattr, like above. For example:
	
	#setattr(ban, "histoMode", True)

	# burst and event checking: set all to true, just use True, to set specific 
	# trackers, use a list:

	#setattr(ban, "noCheckBadBurst", ["GigaTracker", "SAV"])
	#setattr(ban, "noCheckDetectors", True)
		
	# other stuff

	#create and add analyzers to base analysis object. 
	an1 = Analyzer("Pi0Reconstruction")
	an2 = Analyzer("VertexCDA")
	
	ban.addAnalyzer(an1);
	ban.addAnalyzer(an2);

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


import os
path = os.getcwd()


baseAn = configure(path)
initializeAnalyzer()
defineAnalyzer()
#UserMethods.runAnalyzer() # this should be defined by us not the user
plots()










