#Python Analyzer
#


import UserMethods

class templateAnalyzer:

	def __init__ ():
		# Specify the trees you want to use and the event class corresponding
		# Don't try to load MCTruth tree (RUN_0 or Event). Use the MCTruthEvent in Process function instead. Problems when opening twice the same tree.
		# Example with RecoEvent:
		#UserMethods.RequestTree(new TRecoGigaTrackerEvent)
		#UserMethods.RequestTree("GigaTracker", new TRecoGigaTrackerEvent, "Reco")
		#UserMethods.RequestTree("GigaTracker", new TRecoGigaTrackerEvent, "Digis")

		# The first form can be used if the detector name is present in the class name.
		# Example with MC Event:
		#UserMethods.RequestTree(new TGigaTrackerEvent)       			TODO: import TGigaTrackerEvent
		#UserMethods.RequestTree("GigaTracker", new TGigaTrackerEvent)

		# Example with generic tree:
		#UserMethods.RequestTree<MyClass>("MyTree", "BranchName", "MyClass", new MyClass)
		#UserMethods.RequestTree("MyTree", "BranchName", "MyClass", new MyClass)		TODO: what is a MyClass..how do they use this
		#UserMethods.RequestTree("MyTree", "BranchName", "int", new int)

		# Requesting Trigger data:
		#UserMethods.RequestL0Data()
		#UserMethods.RequestL1Data()
		#UserMethods.RequestL2Data()

		# Call one of:
		#UserMethods.AddParam("paramName", variableName, defaultValue)
		# for each parameter of the analyzer. These parameters can be set when starting the FW from the command line with the -p option.
		# paramName is the name of the parameter in the command line
		# variableName is the name of the variable that should be declared in the definition of the class
		# defaultValue is the default value if not specified in the command line
		# The allowed types for parameters are the following: bool, int, long, float, double, char, string, TString
		
		# A primitive file can be read in parallel to the event file. You can request to read primitives for a sub-detector with
		#UserMethods.AddPrimitiveReader("detName", True/False)
		# The boolean flag indicates if the primitives should be time sorted or kept in the original order they arrived from
		# the detector
		
		# You can set the L0 matching window with
		#UserMethods.SetL0MatchingWindowWidth("detName", val)
		#UserMethods.SetL0MatchingWindowWidth("detName", timestamp, finetime)
		# where val is a value in nanoseconds.

	
	def InitOutput():
		# Register the output variables of the analyzer.

		# Call:
		#UserMethods.RegisterOutput("outputName", variableName)
		# for each variable that should be in the output of the Analyzer
		# The name of the analyzer will be prepended to the outputName (to avoid collisions with other analyzers)
		# variableName should be the name of a variable declared in the definition of the class
		
		# To create a new TTree in the output file, call:
		#OpenNewTree("TTreeName", "TTreeTitle")						#TODO: figure this out
		# TTreeName is the name of the TTree (will be used to refer to this TTree later)
		# TTreeTitle is the title of the TTree

		# To add a branch to the newly created TTree, call:
		#void AddBranch<VariableType>("TTreeName", "BranchName", &pointer);
		# VariableType is the type of the variable for this branch (fundamental data type or class)
		# TTreeName is the name of the TTree to add this branch
		# BranchName is the name of the branch
		# pointer is a pointer to the variable (should be declared in the header file) TODO: pointer?

		# To create a standard TTree containing KineParts (for candidates):
		#UserMethods.CreateStandardTree("TTreeName", "TTreeTitle")


	def InitHist():
		# Book and Initialize histograms in this function.

		# Same function to Book TH1, TH2, TGraph and TGraphAsymmErrors (anything derived from TH1 or TGraph)
		#UserMethods.BookHisto(histogram)
		# If isAutotUpdate is true, this histogram will be drawn and updated regularly during the processing (default=false).
		
		# The refresh interval can be set with (default=10):
		#UserMethods.SetUpdateInterval(interval)
		# Defining plots as AutoUpdate and setting the interval can also be done at runtime with a configuration file.

		# Example of booking an histogram:
		#UserMethods.BookHisto(new TH2I("PartEnergy", "Energy as a function of particle", 0, 0, 0, Bins, MinEnergy, MaxEnergy)); #TODO: TH2I? what is this.
		# The histogram can be saved in a subdirectory of the output files in several ways:
		#UserMethods.BookHisto(new TH2I("PartEnergy", "Energy as a function of particle", 0, 0, 0, Bins, MinEnergy, MaxEnergy), refresh, "dir1/dir2")
		#UserMethods.BookHisto(new TH2I("dir1/dir2/PartEnergy", "Energy as a function of particle", 0, 0, 0, Bins, MinEnergy, MaxEnergy))
		#UserMethods.BookHisto("dir1/dir2/PartEnergy", new TH2I("hPartEnergy", "Energy as a function of particle", 0, 0, 0, Bins, MinEnergy, MaxEnergy))

		
	
	def DefineMCSimple():


	def ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType):


	def Process(int iEvent):
                

	def PostProcess():
                

	def StartOfBurstUser():
        

	def EndOfBurstUser():
                

	def StartOfRunUser():
                

	def EndOfRunUser():
        

	def EndOfJobUser():
                

	def DrawPlot():


