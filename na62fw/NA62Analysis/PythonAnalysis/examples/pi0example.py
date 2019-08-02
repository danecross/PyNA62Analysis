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

#import statements
from PyNA62Analysis.PyBaseAnalysis import PyBaseAnalysis as BaseAnalysis
from PyNA62Analysis.PyAnalyzer import PyAnalysis as Analyzer
from PyNA62Analysis.WrapperObject import WrapperObject as WO
#import ROOT

import os

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
	
	coreVerbosity = "trace" ; anVerbosity = "normal"
	setattr(ban, "coreVerbosity", coreVerbosity)
	setattr(ban, "anVerbosity", anVerbosity)

	# set boolean flags 
	# booleans you can set: 
	# 	ban.graphicMode        ban.useDownscaling     
	# 	ban.fastStart          ban.histoMode          ban.skipIsFatal
	# 	ban.continuousReading  ban.filter             ban.specialOnly
	# default value is False 
	# set these values with setattr, like above. For example:
	
#	setattr(ban, "histoMode", True)

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
def initializePi0Analyzer(ban):

	print("PI0 RECONSTRUCTION INITIALIZATION: ")
	
	analyzers = ban.analyzers
	Pi0 = analyzers[0]

	Pi0.requestTree("LKr", "TRecoLKrEvent")
	
	Pi0.bookHisto("TH1I", "g1Energy", "Energy of g1", 100, 0, 75000)
	Pi0.bookHisto("TH1I", "g2Energy", "Energy of g2", 100, 0, 75000)
	Pi0.bookHisto("TH2I", "g1Reco", "g1 Reco vs. Real", 100, 0, 75000, 100, 0, 75000)
	Pi0.bookHisto("TH2I", "g2Reco", "g2 Reco vs. Real", 100, 0, 75000, 100, 0, 75000)
	Pi0.bookHisto("TH2I", "g1px", "g1 px Reco vs. Real", 200, 0, 2000, 200, 0, 2000)
	Pi0.bookHisto("TH2I", "g2px", "g2 px Reco vs. Real", 200, 0, 2000, 200, 0, 2000)
	Pi0.bookHisto("TH2I", "g1py", "g1 py Reco vs. Real", 200, 0, 2000, 200, 0, 2000)
	Pi0.bookHisto("TH2I", "g2py", "g2 py Reco vs. Real", 200, 0, 2000, 200, 0, 2000)
	Pi0.bookHisto("TH2I", "g1pz", "g1 pz Reco vs. Real", 10, 240000, 250000, 10, 240000, 250000)
	Pi0.bookHisto("TH2I", "g2pz", "g2 pz Reco vs. Real", 10, 240000, 250000, 10, 240000, 250000)
	Pi0.bookHisto("TH1I", "pi0Energy", "Energy of pi0", 100, 0, 75000)
	Pi0.bookHisto("TH1I", "pi0Mass", "Reconstructed mass of pi0", 200, 0, 200)
	Pi0.bookHisto("TH2I", "clusterPosition", "Cluster position on LKr", 500, -2000, 2000, 500, -2000, 2000)
	Pi0.bookHisto("TH1I", "photonsNbr", "Photons multiplicity/event", 10, 0, 10)
	Pi0.bookHisto("TH1I", "g1EnergyFraction", "Fraction between real energy and reco energy", 1000, 0, 100)
	Pi0.bookHisto("TH1I", "g2EnergyFraction", "Fraction between real energy and reco energy", 1000, 0, 100)

	Pi0.bookHisto("TH1I", "gPairSelected", "Pair of gamma selected for Pi0", 10, 0, 10)
	Pi0.bookHisto("TH1I", "g1FirstVol", "First touched volume for g1", 15, 0, 15)
	Pi0.bookHisto("TH1I", "g2FirstVol", "First touched volume for g2", 15, 0, 15)

	Pi0.bookHisto("TH1I", "pdgID", "Non complete events : pdgID", 0, 0, 0)

	Pi0.bookHisto("TGraph")

	Pi0.registerOutput("pi0", "KinePart")

	return ban

def initializeVertexCDAAnalyzer(ban):
	
	print("VERTEX CDA INITIALIZATION:")
	
	analyzers = ban.analyzers
	VCDA = analyzers[1]
	
	VCDA.requestTree("GigaTracker", "TRecoGigaTrackerEvent")
	VCDA.requestTree("Spectrometer", "TRecoSpectrometerEvent")

	VCDA.bookHisto("TH1I", "VertexX", "Reconstructed vertex X position; vtx_{x}^{reco}", 250, -250, 250)
	VCDA.bookHisto("TH1I", "VertexY", "Reconstructed vertex Y position; vtx_{y}^{reco}", 150, -150, 150)
	VCDA.bookHisto("TH1I", "VertexZ", "Reconstructed vertex Z position; vtx_{z}^{reco}", 100, 0, 300000)

	VCDA.bookHisto("TH1I", "DiffVertexX", "X difference between reco and real vertex; vtx_{x}^{reco}-vtx_{x}", 200, -50, 50)
	VCDA.bookHisto("TH1I", "DiffVertexY", "Y difference between reco and real vertex; vtx_{y}^{reco}-vtx_{y}", 200, -50, 50)
	VCDA.bookHisto("TH1I", "DiffVertexZ", "Z difference between reco and real vertex; vtx_{z}^{reco}-vtx_{z}", 200, -10000, 10000)

	VCDA.bookHisto("TH2I", "VertexRecoRealX", "Reconstructed vs. Real (X)", 250, -250, 250, 250, -250, 250)
	VCDA.bookHisto("TH2I", "VertexRecoRealY", "Reconstructed vs. Real (Y)", 150, -150, 150, 150, -150, 150)
	VCDA.bookHisto("TH2I", "VertexRecoRealZ", "Reconstructed vs. Real (Z)", 200, 0, 300000, 200, 0, 300000)

	VCDA.bookHisto("TH1I", "GTKMultiplicity", "Multiplicity in GTK", 11, -0.5, 10.5)
	VCDA.bookHisto("TH1I", "StrawMultiplicity", "Multiplicity in Straw", 11, -0.5, 10.5)
	
	VCDA.bookHistoArray("TH2I", "BeamXY", "BeamXY", 100, -100, 100, 100, -100, 100, 20)
	
	#counters and other things
	VCDA.bookCounter("Total_Events")
	VCDA.bookCounter("Good_GTK_Mult")
	VCDA.bookCounter("Good_Straw_Mult")

	VCDA.registerOutput("Vertex", "TVector3")

	return ban


def VCDAMonteCarlo(ban):

	print("VCDA MONTE CARLO INITIALIZATION: " )

	analyzers = ban.analyzers
	VCDA = analyzers[1]

	kaonID = VCDA.MC_addParticle(0, 321)
	VCDA.MC_addParticle(kaonID, 211)
	
	return ban

def Pi0MonteCarlo(ban):

	print("Pi0 MONTE CARLO INITIALIZATION: " )
	
	analyzers = ban.analyzers

	Pi0 = analyzers[0]

	kaonID = Pi0.MC_addParticle(0, 321) 	# beam kaon
	Pi0.MC_addParticle(kaonID, 211) 	# positive pion from beam kaon
	pipID = Pi0.MC_addParticle(kaonID, 111)	# positive pion from initial kaon decay
	Pi0.MC_addParticle(pipID, 22)		# positive pion from kaon decay
	Pi0.MC_addParticle(pipID, 22)		# positive pion from kaon decay

	return ban

# write analyzers
# this replaces the Process method
# 
# here we do the bulk processing of the data and get it ready to be put into histograms
def runVertexCDA(ban):
	print("RUNNING VERTEX CDA RECONSTRUCTION ANALYZER:")
	analyzers = ban.analyzers
	VCDA = analyzers[1]
	
	badEvent = False
	withMC = True

	if VCDA.MCstatus() != "complete":
		withMC = False

	print("HERE1")

	GTKEvent = VCDA.getEvent("TRecoGigaTrackerEvent")
	print("HERE2")

	SpectrometerEvent = VCDA.getEvent("TRecoSpectrometerEvent")

	VCDA.incrementCounter("Total_Events")
	#VCDA.fillHisto("GTKMultiplicity", GTKEvent->GetNCandidates())
	print("HERE")
	GTKEvent.getNCandidates()
	
	return ban



def runPi0Reconstruction(ban):
	print("RUNNING PI0 RECONSTRUCTION ANALYZER:")
	analyzers = ban.analyzers
	Pi0 = analyzers[0]
	VertexCDA = analyzers[1]
	
	LKrStartPos = 240413

	LKrEvent = Pi0.getEvent("TRecoLKrEvent")

	calibMult = 0.9744
	calibConst = -366.5

	withMC = True
	print("MCstatus:", Pi0.MCstatus())
	if Pi0.MCstatus()=="missing":
		event = Pi0.MC_getEvent()
		print("number of MC particles is:", Pi0.MC_numParticles())
#		for i in range(Pi0.MC_numParticles):
			#graph MC events 
		withMC = False
	elif Pi0.MCstatus() == "empty": 
		withMC = False

	vertex, state = VertexCDA.getOutput("Vertex")
	
	return ban

# perform post-analysis processing
# this replaces the PostProcess, EndOfBurstUser, and EndOfRunUser methods
#
# here we do any last-minute processing before we plot the data
def postProcess(ban):
	print("post process")
	return ban

# plot products
# this replaces EndOfJobUser and DrawPlot()
# 
# here we use ROOT to plot our prepared data. 
def plots(ban):
	print("plots")
	return ban



path = os.getcwd()


baseAn = configure(path)
baseAn = initializePi0Analyzer(baseAn)
baseAn = initializeVertexCDAAnalyzer(baseAn)	
baseAn = Pi0MonteCarlo(baseAn)
baseAn = VCDAMonteCarlo(baseAn)
baseAn = runVertexCDA(baseAn)
baseAn = runPi0Reconstruction(baseAn)
plots(baseAn)










