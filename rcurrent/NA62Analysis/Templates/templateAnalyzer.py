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
import UserMethods
import ROOT

# configuration
# this forgoes the need for a config file
def configure():

	input_files = [] #write the path to the desired input files here
	
	# set boolean flags (ie UseColors, UsePrimitiveFile, etc) 
	# default value is False 

	

	# set integer info (ie NProcessFiles, etc.) 
	# default value is 0



	# set strings 
	# default value is ""



	#other stuff



# initialize analyzers
# this replaces the constructor, InitOutput, InitHist, DefineMCSimple, StartOfRunUser, 
# and StartOfBurstUser
#
# here we initialize analyzers, request data, request histograms, add particles to 
# MC simulator, add parameters, etc.
# 
def initializeAnalyzer():
	

# write analyzers
# this replaces the Process method
# 
# here we do the bulk processing of the data and get it ready to be put into histograms
def defineAnalyzer():


# perform post-analysis processing
# this replaces the PostProcess, EndOfBurstUser, and EndOfRunUser methods
#
# here we do any last-minute processing before we plot the data
def postProcess():
	

# plot products
# this replaces EndOfJobUser and DrawPlot()
# 
# here we use ROOT to plot our prepared data. 
def plots():


def runAll():

	configure()
	initializeAnalyzer()
	defineAnalyzer()
	UserMethods.runAnalyzer() # this should be defined by us not the user
	plots()










