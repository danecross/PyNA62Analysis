/*
 * UserMethods.cc
 *
 *  Created on: 21 Jan 2014
 *      Author: ncl
 */

#include "UserMethods.hh"

#include "BaseAnalysis.hh"
#include "CanvasOrganizer.hh"
#include "IOPrimitive.hh"
#include "NA62Exceptions.hh"
#include "PrimitiveReader.hh"
#include "Persistency.hh"
#include "SlimPersistency.hh"

namespace NA62Analysis {

UserMethods::UserMethods(Core::BaseAnalysis *ba) :
		Verbose("UserMethods"), fDisableSave(false), fAnalyzerName(""),fParent(ba) {
	/// \MemberDescr
	/// \param ba : Pointer to the parent BaseAnalysis instance
	///
	/// Constructor
	/// \EndMemberDescr
}

UserMethods::UserMethods(Core::BaseAnalysis *ba, const std::string &name) :
		Verbose(name), fDisableSave(false), fAnalyzerName(name), fParent(ba) {
	/// \MemberDescr
	/// \param ba : Pointer to the parent BaseAnalysis instance
	/// \param name : Display name
	///
	/// Constructor with name
	/// \EndMemberDescr
}

UserMethods::UserMethods(const UserMethods &c) :
		Verbose(c), fDisableSave(false), fAnalyzerName(""), fParent(c.fParent) {
	/// \MemberDescr
	/// \param c : Reference of the object to copy
	///
	/// Constructor
	/// \EndMemberDescr
}

UserMethods::~UserMethods() {
	/// \MemberDescr
	/// Destructor
	/// \EndMemberDescr
}

void UserMethods::ExportAllPlot(std::map<TString, TTree*> &trees,
		std::map<TString, void*> &branches) {
	/// \MemberDescr
	/// \param trees : pointer to the list of TTrees
	/// \param branches : point to the list of branches
	///
	/// Export all booked histograms into the output file histograms trees
	/// \EndMemberDescr

	fHisto.ExportAllPlot(trees, branches);
}

void UserMethods::DrawAllPlots() {
	/// \MemberDescr
	/// Draw all booked histograms on the screen
	/// \EndMemberDescr

	fHisto.DrawAllPlots(fAnalyzerName);
}

void UserMethods::UpdatePlots(Long64_t evtNbr) {
	/// \MemberDescr
	/// \param evtNbr : Current event number
	///
	/// Update all plots with refresh
	/// \EndMemberDescr

	fHisto.UpdatePlots(evtNbr);
}

void UserMethods::SaveAllPlots() {
	/// \MemberDescr
	/// Write all the booked histograms into the output file
	/// \EndMemberDescr

	if (!fDisableSave)
		fHisto.SaveAllPlots(fAnalyzerName);
}

void UserMethods::SaveNonEmptyPlots() {
	/// \MemberDescr
	/// Write all the booked histograms with at least one entry into the output file
	/// \EndMemberDescr

	if (!fDisableSave) {
		fHisto.SaveAllPlots(fAnalyzerName, false);
    }
}

void UserMethods::SetUpdateInterval(int interval) {
	/// \MemberDescr
	/// \param interval : Events interval at which the plots should be updated
	//
	/// Set the update interval for the plots
	/// \EndMemberDescr

	std::cout << extended() << "Setting plot update interval to " << interval << std::endl;
	fHisto.SetUpdateInterval(interval);
}

void UserMethods::BookCounter(TString cName) {
	/// \MemberDescr
	/// \param cName : Name of the Counter
	///
	/// Book a new counter
	/// \EndMemberDescr

	if (!fParent) return;
	fParent->GetCounterHandler()->BookCounter(
			fAnalyzerName + TString(".") + cName);
}

void UserMethods::AddCounterToEventFraction(TString efName, TString cName) {
	/// \MemberDescr
	/// \param efName : Name of the EventFraction instance
	/// \param cName : Name of the Counter
	///
	/// Add a counter in the specified EventFraction table
	/// \EndMemberDescr

	if (!fParent) return;
	fParent->GetCounterHandler()->AddCounterToEventFraction(efName,
			fAnalyzerName + TString(".") + cName);
}
void UserMethods::NewEventFraction(TString name) {
	/// \MemberDescr
	/// \param name : Name of the eventFraction table
	///
	/// Create a new EventFraction table
	/// \EndMemberDescr

	if (!fParent) return;
	fParent->GetCounterHandler()->NewEventFraction(name);
}
void UserMethods::DefineSampleSizeCounter(TString efName, TString cName) {
	/// \MemberDescr
	/// \param efName : Name of the EventFraction instance
	/// \param cName : Name of the Counter
	///
	/// Define counter as SampleSize in the specified EventFraction table
	/// \EndMemberDescr

	if (!fParent) return;
	fParent->GetCounterHandler()->DefineSampleSizeCounter(efName,
			fAnalyzerName + TString(".") + cName);
}
void UserMethods::SetSignificantDigits(TString efName, int v) {
	/// \MemberDescr
	/// \param efName : Name of the EventFraction instance
	/// \param v : Number of significant digits
	///
	/// Set the number of significant digits for the specified EventFraction table
	/// \EndMemberDescr

	if (!fParent) return;
	fParent->GetCounterHandler()->SetSignificantDigits(efName, v);
}

void UserMethods::SetCounterValue(TString cName, int v) {
	/// \MemberDescr
	/// \param cName : Name of the counter
	/// \param v : value
	///
	/// Set the value of a previously booked counter
	/// \EndMemberDescr

	if (!fParent) return;
	fParent->GetCounterHandler()->SetCounterValue(
			fAnalyzerName + TString(".") + cName, v);
}
void UserMethods::IncrementCounter(TString cName, int delta) {
	/// \MemberDescr
	/// \param cName : Name of the counter
	/// \param delta : value
	///
	/// Increment a previously booked counter by delta
	/// \EndMemberDescr

	if (!fParent) return;
	fParent->GetCounterHandler()->IncrementCounter(
			fAnalyzerName + TString(".") + cName, delta);
}
void UserMethods::DecrementCounter(TString cName, int delta) {
	/// \MemberDescr
	/// \param cName : Name of the counter
	/// \param delta : value
	///
	/// Decrement a previously booked counter by delta
	/// \EndMemberDescr

	if (!fParent) return;
	fParent->GetCounterHandler()->DecrementCounter(
			fAnalyzerName + TString(".") + cName, delta);
}
void UserMethods::IncrementCounter(TString cName) {
	/// \MemberDescr
	/// \param cName : Name of the counter
	///
	/// Increment a previously booked counter by 1
	/// \EndMemberDescr

	if (!fParent) return;
	fParent->GetCounterHandler()->IncrementCounter(
			fAnalyzerName + TString(".") + cName);
}
void UserMethods::DecrementCounter(TString cName) {
	/// \MemberDescr
	/// \param cName : Name of the counter
	///
	/// Decrement a previously booked counter by 1
	/// \EndMemberDescr

	if (!fParent) return;
	fParent->GetCounterHandler()->DecrementCounter(
			fAnalyzerName + TString(".") + cName);
}
int UserMethods::GetCounterValue(TString cName) const {
	/// \MemberDescr
	/// \param cName : Name of the counter
	/// \return Value of the requested counter
	///
	/// Get counter value
	/// \EndMemberDescr

	if (!fParent) return -1;
	return fParent->GetCounterHandler()->GetCounterValue(
			fAnalyzerName + TString(".") + cName);
}

void UserMethods::RegisterOutput(TString name, const void* const address) {
	/// \MemberDescr
	/// \param name : name of the output
	/// \param address : pointer to the variable that is registered
	///
	/// Register a variable as output of the analyzer
	/// \EndMemberDescr

	if (!fParent) return;
	fParent->RegisterOutput(fAnalyzerName + TString(".") + name, address);
}

void UserMethods::SetOutputState(TString name, OutputState state) {
	/// \MemberDescr
	/// \param name : name of the output
	/// \param state : state to be set
	///
	/// Set the state of the output variable
	/// \EndMemberDescr

	if (!fParent) return;
	fParent->SetOutputState(fAnalyzerName + TString(".") + name, state);
}

const void *UserMethods::GetOutput(TString name, OutputState &state) const {
	/// \MemberDescr
	/// \param name : name of the output
	/// \param state : is filled with the current state of the output
	/// \return Output variable and the corresponding state
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	return fParent->GetOutput(name, state);
}

const void *UserMethods::GetOutput(TString name) const {
	/// \MemberDescr
	/// \param name : name of the output
	/// \return Output variable and the corresponding state
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	OutputState state;
	return fParent->GetOutput(name, state);
}

const void* UserMethods::GetOutputVoid(TString name, OutputState &state) const {
	/// \MemberDescr
	/// \param name: Name of the output variable to get
	/// \param state: Reference to the outputState of the output variable
	///
	/// Internal interface to BaseAnalysis for GetOutput method
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	return fParent->GetOutput(name, state);
}

void UserMethods::RequestBeamData() {
	/// \MemberDescr
	/// Request the beam data branch
	/// \EndMemberDescr

	// #NOQA_VERBOSE
	if (!fParent) return;
	if (fParent->IsTreeType())
		fParent->GetIOTree()->RequestTree("Reco", "Beam", "BeamData", new BeamData);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
}

void UserMethods::RequestL0Data() {
	/// \MemberDescr
	/// Request the L0 data branch
	/// \EndMemberDescr

	if (!fParent) return;
	if (fParent->IsTreeType())
		fParent->GetIOTree()->RequestTree("Reco", "L0TP", "L0TPData", new L0TPData);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
}


void UserMethods::RequestHLTData() {
	/// \MemberDescr
	/// Request the HLT data branch
	/// \EndMemberDescr

	if (!fParent) {
        return;
    }
	if (fParent->IsTreeType()) {
		fParent->GetIOTree()->RequestTree("Reco", "HLT", "HLTEvent", new HLTEvent);
	} else {
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
    }
}




void UserMethods::RequestL1Data() {
	/// \MemberDescr
	/// Request the L1 data branch
	/// \EndMemberDescr

	if (!fParent) return;
	if (fParent->IsTreeType())
		fParent->GetIOTree()->RequestTree("Reco", "L1TP", "L1TPData", new L1TPData);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
}

void UserMethods::RequestL2Data() {
	/// \MemberDescr
	/// Request the L2 data branch
	/// \EndMemberDescr

	if (!fParent) return;
	if (fParent->IsTreeType())
		fParent->GetIOTree()->RequestTree("Reco", "L2EB", "L2EBData", new L2EBData);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
}

void UserMethods::RequestL0SpecialTrigger() {
	/// \MemberDescr
	/// Request the L0 data branch
	/// \EndMemberDescr

	if (!fParent) return;
	if (fParent->IsTreeType())
		fParent->GetIOTree()->RequestTree("SpecialTrigger", "L0TP", "L0TPSpecialTrigger", new L0TPSpecialTrigger);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
}

void UserMethods::RequestL1SpecialTrigger() {
	/// \MemberDescr
	/// Request the L1 data branch
	/// \EndMemberDescr

	if (!fParent) return;
	if (fParent->IsTreeType())
		fParent->GetIOTree()->RequestTree("SpecialTrigger", "L1TP", "L1TPSpecialTrigger", new L1TPSpecialTrigger);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
}

void UserMethods::RequestL2SpecialTrigger() {
	/// \MemberDescr
	/// Request the L2 data branch
	/// \EndMemberDescr

	if (!fParent) return;
	if (fParent->IsTreeType())
		fParent->GetIOTree()->RequestTree("SpecialTrigger", "L2EB", "L2EBSpecialTrigger", new L2EBSpecialTrigger);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
}

void UserMethods::RequestBeamSpecialTrigger() {
	/// \MemberDescr
	/// Request the Beam data branch
	/// \EndMemberDescr

	if (!fParent) return;
	if (fParent->IsTreeType())
		fParent->GetIOTree()->RequestTree("SpecialTrigger", "Beam", "BeamSpecialTrigger", new BeamSpecialTrigger);
	else
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
}

void UserMethods::RequestTree(TString detectorName, TDetectorVEvent *evt, TString outputStage) {
	/// \MemberDescr
	/// \param detectorName : Name of the Detector branch to open
	/// \param evt : Pointer to an instance of a detector event (MC or Reco)
	/// \param outputStage : Name of the tree to request (outputStage = Reco, Digis, MC)
	///
	/// Request a branch in a tree in the input file. If the tree has already
	/// been requested before, only add the new branch.
	/// If outputStage is not specified, the branch "Reco" or "Digis" or "MC"
	/// will be used (depending on the TDetectorVEvent class instance).
	/// \EndMemberDescr

	if (!fParent) return;
	if (fParent->IsTreeType())
		fParent->GetIOTree()->RequestTree(detectorName, evt, outputStage);
	else {
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
		delete evt;
	}
}

void UserMethods::RequestTree(TDetectorVEvent* evt, TString outputStage) {
	/// \MemberDescr
	/// \param evt : Pointer to an instance of a detector event (MC or Reco)
	/// \param outputStage : Name of the tree to request (outputStage = Reco, Digis, MC)
	///
	/// Request a branch in a tree in the input file. The branch name is inferred
	/// from the event class if possible. It deduction fails, abort execution.
	/// If the tree has already been requested before, only add the new branch.
	/// If outputStage is not specified, the branch "Reco" or "Digis" or
	/// "MC" will be used (depending on the
	/// TDetectorVEvent class instance).
	/// \EndMemberDescr

	if (!fParent) return;
	if (!fParent->IsTreeType()) {
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
		delete evt;
		return;
	}

	//Try to deduce branch name from the class pointer.
	TString className = evt->ClassName();
	TString branchName;

	std::cout << debug() << "Trying to deduce branch name from class name"
			<< className << std::endl;

	if (className.Contains("Cedar"))
		branchName = "Cedar";
	else if (className.Contains("CHANTI"))
		branchName = "CHANTI";
	else if (className.Contains("NewCHOD"))
		branchName = "NewCHOD";
	else if (className.Contains("CHOD"))
		branchName = "CHOD";
	else if (className.Contains("GigaTracker"))
		branchName = "GigaTracker";
	else if (className.Contains("HAC"))
		branchName = "HAC";
	else if (className.Contains("IRC"))
		branchName = "IRC";
	else if (className.Contains("LAV"))
		branchName = "LAV";
	else if (className.Contains("LKr"))
		branchName = "LKr";
	else if (className.Contains("MUV0"))
		branchName = "MUV0";
	else if (className.Contains("MUV1"))
		branchName = "MUV1";
	else if (className.Contains("MUV2"))
		branchName = "MUV2";
	else if (className.Contains("MUV3"))
		branchName = "MUV3";
	else if (className.Contains("RICH"))
		branchName = "RICH";
	else if (className.Contains("SAC"))
		branchName = "SAC";
	else if (className.Contains("SAV"))
		branchName = "SAV";
	else if (className.Contains("Spectrometer"))
		branchName = "Spectrometer";
	else {
		std::cout << normal() << "[ERROR] Unable to determine branch name from class name: "
				<< className << std::endl;
		throw LogicException();
	}

	fParent->GetIOTree()->RequestTree(branchName, evt, outputStage);
}

void UserMethods::RequestTree(TString detectorName, TSlimRecoVEvent *evt) {
	/// \MemberDescr
	/// \param detectorName : Name of the Detector branch to open
	/// \param evt : Pointer to an instance of a detector event (SlimReco)
	///
	/// Request a branch in a tree in the input file. If the tree has already
	/// been requested before, only add the new branch.
	/// \EndMemberDescr

	if (!fParent) return;
	if (fParent->IsTreeType())
		fParent->GetIOTree()->RequestTree("SlimReco", detectorName, evt->ClassName(), evt);
	else {
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
		delete evt;
	}
}

void UserMethods::RequestTree(TSlimRecoVEvent* evt) {
	/// \MemberDescr
	/// \param evt : Pointer to an instance of a detector event (SlimReco)
	///
	/// Request a branch in a tree in the input file. The branch name is inferred
	/// from the event class if possible. It deduction fails, abort execution.
	/// If the tree has already been requested before, only add the new branch.
	/// \EndMemberDescr

	if (!fParent) return;
	if (!fParent->IsTreeType()) {
		std::cout << user() << "[WARNING] Not reading TTrees" << std::endl; // #NOQA_VERBOSE
		delete evt;
		return;
	}

	//Try to deduce branch name from the class pointer.
	TString className = evt->ClassName();
	TString branchName;

	std::cout << debug() << "Trying to deduce branch name from class name"
			<< className << std::endl;

	if (className.Contains("Cedar"))
		branchName = "Cedar";
	else if (className.Contains("CHANTI"))
		branchName = "CHANTI";
	else if (className.Contains("NewCHOD"))
		branchName = "NewCHOD";
	else if (className.Contains("CHOD"))
		branchName = "CHOD";
	else if (className.Contains("GigaTracker"))
		branchName = "GigaTracker";
	else if (className.Contains("HAC"))
		branchName = "HAC";
	else if (className.Contains("IRC"))
		branchName = "IRC";
	else if (className.Contains("LAV"))
		branchName = "LAV";
	else if (className.Contains("LKr"))
		branchName = "LKr";
	else if (className.Contains("MUV0"))
		branchName = "MUV0";
	else if (className.Contains("MUV1"))
		branchName = "MUV1";
	else if (className.Contains("MUV2"))
		branchName = "MUV2";
	else if (className.Contains("MUV3"))
		branchName = "MUV3";
	else if (className.Contains("RICH"))
		branchName = "RICH";
	else if (className.Contains("SAC"))
		branchName = "SAC";
	else if (className.Contains("SAV"))
		branchName = "SAV";
	else if (className.Contains("Spectrometer"))
		branchName = "Spectrometer";
	else {
		std::cout << normal() << "[ERROR] Unable to determine branch name from class name: "
				<< className << std::endl;
		throw LogicException();
	}

	fParent->GetIOTree()->RequestTree("SlimReco", branchName, evt->ClassName(), evt);
}

void NA62Analysis::UserMethods::RequestAllMCTrees() {
	/// \MemberDescr
	/// Call RequestTree() for all known MC trees and branches
	/// \EndMemberDescr
        if (!GetWithMC()) return;
	RequestTree(new TCedarEvent);
	RequestTree(new TCHANTIEvent);
	RequestTree(new TGigaTrackerEvent);
	RequestTree(new TLAVEvent);
	RequestTree(new TLKrEvent);
	RequestTree(new TMUV0Event);
	RequestTree(new TMUV1Event);
	RequestTree(new TMUV2Event);
	RequestTree(new TMUV3Event);
	RequestTree(new TRICHEvent);
	RequestTree(new TSpectrometerEvent);
	RequestTree(new TSACEvent);
	RequestTree(new THACEvent);
	RequestTree(new TCHODEvent);
	RequestTree(new TNewCHODEvent);
	RequestTree(new TIRCEvent);
}

void NA62Analysis::UserMethods::RequestAllRecoTrees(bool RequestSpecialTriggers) {
	/// \MemberDescr
	/// Call RequestTree() for all known Reco trees and branches
	/// \EndMemberDescr
	RequestTree(new TRecoCedarEvent);
	RequestTree(new TRecoCHANTIEvent);
	RequestTree(new TRecoGigaTrackerEvent);
	RequestTree(new TRecoLAVEvent);
	RequestTree(new TRecoLKrEvent);
	RequestTree(new TRecoMUV0Event);
	RequestTree(new TRecoMUV1Event);
	RequestTree(new TRecoMUV2Event);
	RequestTree(new TRecoMUV3Event);
	RequestTree(new TRecoRICHEvent);
	RequestTree(new TRecoSpectrometerEvent);
	RequestTree(new TRecoSACEvent);
	RequestTree(new TRecoHACEvent);
	RequestTree(new TRecoCHODEvent);
	RequestTree(new TRecoNewCHODEvent);
	RequestTree(new TRecoIRCEvent);
	RequestTree(new TRecoSAVEvent);

	RequestL0Data();
	RequestL1Data();
	RequestL2Data();
	RequestHLTData();

	RequestBeamData();
	RequestBeamSpecialTrigger();

	if (!RequestSpecialTriggers) return;

	RequestL0SpecialTrigger();
	RequestL1SpecialTrigger();
	RequestL2SpecialTrigger();

	RequestTree("Cedar", new TCedarSpecialTriggerEvent, "SpecialTrigger");
	RequestTree("CHANTI", new TSpecialTriggerEvent, "SpecialTrigger");
#ifdef OLD_SPECIALTRIGGER
	RequestTree("GigaTracker", new TSpecialTriggerEvent, "SpecialTrigger");
#else
	RequestTree("GigaTracker", new TGigaTrackerSpecialTriggerEvent, "SpecialTrigger");
#endif
	RequestTree("LAV", new TSpecialTriggerEvent, "SpecialTrigger");
	RequestTree("LKr", new TSpecialTriggerEvent, "SpecialTrigger");
	RequestTree("MUV0", new TSpecialTriggerEvent, "SpecialTrigger");
	RequestTree("MUV1", new TSpecialTriggerEvent, "SpecialTrigger");
	RequestTree("MUV2", new TSpecialTriggerEvent, "SpecialTrigger");
	RequestTree("MUV3", new TSpecialTriggerEvent, "SpecialTrigger");
	RequestTree("RICH", new TSpecialTriggerEvent, "SpecialTrigger");
	RequestTree("Spectrometer", new TSpecialTriggerEvent, "SpecialTrigger");
	RequestTree("SAC", new TSpecialTriggerEvent, "SpecialTrigger");
	RequestTree("HAC", new TSpecialTriggerEvent, "SpecialTrigger");
	RequestTree("CHOD", new TSpecialTriggerEvent, "SpecialTrigger");
	RequestTree("NewCHOD", new TSpecialTriggerEvent, "SpecialTrigger");
	RequestTree("IRC", new TSpecialTriggerEvent, "SpecialTrigger");
	RequestTree("SAV", new TSpecialTriggerEvent, "SpecialTrigger");
}

L0TPData* UserMethods::GetL0Data() {
	/// \MemberDescr
	/// \return L0TPData object
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (fParent->IsTreeType())
		return static_cast<L0TPData*>(fParent->GetIOTree()->GetObject("Reco",
				"L0TP"));
	else
		std::cout << extended() << "[WARNING] Not reading TTrees" << std::endl;
	return nullptr;
}

HLTEvent* UserMethods::GetHLTData() {
	/// \MemberDescr
	/// \return HLTData object
	/// \EndMemberDescr
	if (!fParent) {
		return nullptr;
	}
	if (fParent->IsTreeType()) {
		return static_cast<HLTEvent*>(fParent->GetIOTree()->GetObject("Reco",
				"HLT"));
	} else {
		std::cout << extended() << "[WARNING] Not reading TTrees" << std::endl;
	}
	return nullptr;
}

L1TPData* UserMethods::GetL1Data() {
	/// \MemberDescr
	/// \return L1TPData object
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (fParent->IsTreeType())
		return static_cast<L1TPData*>(fParent->GetIOTree()->GetObject("Reco",
				"L1TP"));
	else
		std::cout << extended() << "[WARNING] Not reading TTrees" << std::endl;
	return nullptr;
}

L2EBData* UserMethods::GetL2Data() {
	/// \MemberDescr
	/// \return L2EBData object
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (fParent->IsTreeType())
		return static_cast<L2EBData*>(fParent->GetIOTree()->GetObject("Reco",
				"L2EB"));
	else
		std::cout << extended() << "[WARNING] Not reading TTrees" << std::endl;
	return nullptr;
}

BeamData* UserMethods::GetBeamData() {
	/// \MemberDescr
	/// \return BeamData object
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (fParent->IsTreeType())
		return static_cast<BeamData*>(fParent->GetIOTree()->GetObject("Reco",
				"Beam"));
	else
		std::cout << extended() << "[WARNING] Not reading TTrees" << std::endl;
	return nullptr;
}

L0TPSpecialTrigger* UserMethods::GetL0SpecialTrigger() {
	/// \MemberDescr
	/// \return L0TPSpecialTrigger object
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (fParent->IsTreeType())
		return static_cast<L0TPSpecialTrigger*>(fParent->GetIOTree()->GetObject("SpecialTrigger",
				"L0TP"));
	else
		std::cout << extended() << "[WARNING] Not reading TTrees" << std::endl;
	return nullptr;
}

L1TPSpecialTrigger* UserMethods::GetL1SpecialTrigger() {
	/// \MemberDescr
	/// \return L0TPSpecialTrigger object
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (fParent->IsTreeType())
		return static_cast<L1TPSpecialTrigger*>(fParent->GetIOTree()->GetObject("SpecialTrigger",
				"L1TP"));
	else
		std::cout << extended() << "[WARNING] Not reading TTrees" << std::endl;
	return nullptr;
}

L2EBSpecialTrigger* UserMethods::GetL2SpecialTrigger() {
	/// \MemberDescr
	/// \return L0TPSpecialTrigger object
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (fParent->IsTreeType())
		return static_cast<L2EBSpecialTrigger*>(fParent->GetIOTree()->GetObject("SpecialTrigger",
				"L2EB"));
	else
		std::cout << extended() << "[WARNING] Not reading TTrees" << std::endl;
	return nullptr;
}

BeamSpecialTrigger* UserMethods::GetBeamSpecialTrigger() {
	/// \MemberDescr
	/// \return BeamSpecialTrigger object
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (fParent->IsTreeType())
          return static_cast<BeamSpecialTrigger*>(fParent->GetIOTree()->GetObject("SpecialTrigger",
				"Beam"));
	else
		std::cout << extended() << "[WARNING] Not reading TTrees" << std::endl;
	return nullptr;
}

TDetectorVEvent *UserMethods::GetEvent(TString detName, TString outputName) {
    /// \MemberDescr
    /// \param detName : Name of the detector from which the event is read
    /// \param outputName : Name of the output stage (Reco, Digis, MC)
    /// \return the pointer to the event corresponding to the given tree and the given branch.
    ///
    /// If outputName is left empty and there is only 1 tree requested for this detector, this
    /// single tree is returned. If there is more than 1 tree requested for this detector,
    /// return either the "Reco" or the "Hits" tree (the first one found - undefined behaviour
    /// if both "Reco" and "Hits" trees have been requested).
    /// If outputName is specified, try to return the specified tree.
    /// \EndMemberDescr

    if (!fParent) return nullptr;
    if (fParent->IsTreeType())
        return fParent->GetIOTree()->GetEvent(detName, outputName);
    else
        std::cout << extended() << "[WARNING] Not reading TTrees" << std::endl;
    return nullptr;
}

TSlimRecoVEvent *UserMethods::GetSlimEvent(TString detName) {
    /// \MemberDescr
    /// \param detName : Name of the detector from which the event is read
    /// \return the pointer to the event corresponding to the SlimReco tree and the given branch.
    /// \EndMemberDescr

    return GetObject<TSlimRecoVEvent>("SlimReco", detName);
}

template<> TCedarEvent* UserMethods::GetEvent<TCedarEvent>(TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TCedarEvent*>(GetEvent("Cedar", outputName));
}
template<> TCHANTIEvent* UserMethods::GetEvent<TCHANTIEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TCHANTIEvent*>(GetEvent("CHANTI", outputName));
}
template<> TCHODEvent* UserMethods::GetEvent<TCHODEvent>(TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TCHODEvent*>(GetEvent("CHOD", outputName));
}
template<> TGigaTrackerEvent* UserMethods::GetEvent<TGigaTrackerEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TGigaTrackerEvent*>(GetEvent("GigaTracker", outputName));
}
template<> THACEvent* UserMethods::GetEvent<THACEvent>(TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<THACEvent*>(GetEvent("HAC", outputName));
}
template<> TIRCEvent* UserMethods::GetEvent<TIRCEvent>(TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TIRCEvent*>(GetEvent("IRC", outputName));
}
template<> TLAVEvent* UserMethods::GetEvent<TLAVEvent>(TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TLAVEvent*>(GetEvent("LAV", outputName));
}
template<> TLKrEvent* UserMethods::GetEvent<TLKrEvent>(TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TLKrEvent*>(GetEvent("LKr", outputName));
}
template<> TMUV0Event* UserMethods::GetEvent<TMUV0Event>(TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TMUV0Event*>(GetEvent("MUV0", outputName));
}
template<> TMUV1Event* UserMethods::GetEvent<TMUV1Event>(TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TMUV1Event*>(GetEvent("MUV1", outputName));
}
template<> TMUV2Event* UserMethods::GetEvent<TMUV2Event>(TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TMUV2Event*>(GetEvent("MUV2", outputName));
}
template<> TMUV3Event* UserMethods::GetEvent<TMUV3Event>(TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TMUV3Event*>(GetEvent("MUV3", outputName));
}
template<> TNewCHODEvent* UserMethods::GetEvent<TNewCHODEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TNewCHODEvent*>(GetEvent("NewCHOD", outputName));
}
template<> TRICHEvent* UserMethods::GetEvent<TRICHEvent>(TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRICHEvent*>(GetEvent("RICH", outputName));
}
template<> TSACEvent* UserMethods::GetEvent<TSACEvent>(TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSACEvent*>(GetEvent("SAC", outputName));
}
template<> TSpectrometerEvent* UserMethods::GetEvent<TSpectrometerEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSpectrometerEvent*>(GetEvent("Spectrometer", outputName));
}

template<> TRecoCedarEvent* UserMethods::GetEvent<TRecoCedarEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoCedarEvent*>(GetEvent("Cedar", outputName));
}
template<> TRecoCHANTIEvent* UserMethods::GetEvent<TRecoCHANTIEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoCHANTIEvent*>(GetEvent("CHANTI", outputName));
}
template<> TRecoCHODEvent* UserMethods::GetEvent<TRecoCHODEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoCHODEvent*>(GetEvent("CHOD", outputName));
}
template<> TRecoGigaTrackerEvent* UserMethods::GetEvent<TRecoGigaTrackerEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoGigaTrackerEvent*>(GetEvent("GigaTracker",
			outputName));
}
template<> TRecoHACEvent* UserMethods::GetEvent<TRecoHACEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoHACEvent*>(GetEvent("HAC", outputName));
}
template<> TRecoIRCEvent* UserMethods::GetEvent<TRecoIRCEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoIRCEvent*>(GetEvent("IRC", outputName));
}
template<> TRecoLAVEvent* UserMethods::GetEvent<TRecoLAVEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoLAVEvent*>(GetEvent("LAV", outputName));
}
template<> TRecoLKrEvent* UserMethods::GetEvent<TRecoLKrEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoLKrEvent*>(GetEvent("LKr", outputName));
}
template<> TRecoMUV0Event* UserMethods::GetEvent<TRecoMUV0Event>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoMUV0Event*>(GetEvent("MUV0", outputName));
}
template<> TRecoMUV1Event* UserMethods::GetEvent<TRecoMUV1Event>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoMUV1Event*>(GetEvent("MUV1", outputName));
}
template<> TRecoMUV2Event* UserMethods::GetEvent<TRecoMUV2Event>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoMUV2Event*>(GetEvent("MUV2", outputName));
}
template<> TRecoMUV3Event* UserMethods::GetEvent<TRecoMUV3Event>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoMUV3Event*>(GetEvent("MUV3", outputName));
}
template<> TRecoNewCHODEvent* UserMethods::GetEvent<TRecoNewCHODEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoNewCHODEvent*>(GetEvent("NewCHOD", outputName));
}
template<> TRecoRICHEvent* UserMethods::GetEvent<TRecoRICHEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoRICHEvent*>(GetEvent("RICH", outputName));
}
template<> TRecoSACEvent* UserMethods::GetEvent<TRecoSACEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoSACEvent*>(GetEvent("SAC", outputName));
}
template<> TRecoSAVEvent* UserMethods::GetEvent<TRecoSAVEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoSAVEvent*>(GetEvent("SAV", outputName));
}
template<> TRecoSpectrometerEvent* UserMethods::GetEvent<TRecoSpectrometerEvent>(
		TString outputName) {
	/// \MemberDescr
	/// \param outputName : Name of the output stage (Reco, Digis, MC)
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TRecoSpectrometerEvent*>(GetEvent("Spectrometer",
			outputName));
}

template<> TSlimRecoCedarEvent* UserMethods::GetEvent<TSlimRecoCedarEvent>(
		TString ) {
	/// \MemberDescr
	/// \param outputName : Unused
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSlimRecoCedarEvent*>(GetSlimEvent("Cedar"));
}

template<> TSlimRecoCHANTIEvent* UserMethods::GetEvent<TSlimRecoCHANTIEvent>(
		TString ) {
	/// \MemberDescr
	/// \param outputName : Unused
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSlimRecoCHANTIEvent*>(GetSlimEvent("CHANTI"));
}

  template<> TSlimRecoCHODEvent* UserMethods::GetEvent<TSlimRecoCHODEvent>(
		  TString ) {
    /// \MemberDescr
    /// \param outputName : Unused
    /// \return the pointer to the event corresponding to the given output stage
    ///
    /// Specialisation of GetEvent. DetectorName not needed.
    /// \EndMemberDescr
    return static_cast<TSlimRecoCHODEvent*>(GetSlimEvent("CHOD"));
  }

template<> TSlimRecoGigaTrackerEvent* UserMethods::GetEvent<TSlimRecoGigaTrackerEvent>(
		TString ) {
	/// \MemberDescr
	/// \param outputName : Unused
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSlimRecoGigaTrackerEvent*>(GetSlimEvent("GigaTracker"));
}

template<> TSlimRecoHACEvent* UserMethods::GetEvent<TSlimRecoHACEvent>(
		TString ) {
	/// \MemberDescr
	/// \param outputName : Unused
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSlimRecoHACEvent*>(GetSlimEvent("HAC"));
}

template<> TSlimRecoIRCEvent* UserMethods::GetEvent<TSlimRecoIRCEvent>(
		TString ) {
	/// \MemberDescr
	/// \param outputName : Unused
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSlimRecoIRCEvent*>(GetSlimEvent("IRC"));
}

template<> TSlimRecoLAVEvent* UserMethods::GetEvent<TSlimRecoLAVEvent>(
		TString ) {
	/// \MemberDescr
	/// \param outputName : Unused
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSlimRecoLAVEvent*>(GetSlimEvent("LAV"));
}

template<> TSlimRecoLKrEvent* UserMethods::GetEvent<TSlimRecoLKrEvent>(
		TString ) {
	/// \MemberDescr
	/// \param outputName : Unused
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSlimRecoLKrEvent*>(GetSlimEvent("LKr"));
}

template<> TSlimRecoMUV0Event* UserMethods::GetEvent<TSlimRecoMUV0Event>(
		TString ) {
	/// \MemberDescr
	/// \param outputName : Unused
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSlimRecoMUV0Event*>(GetSlimEvent("MUV0"));
}

template<> TSlimRecoMUV1Event* UserMethods::GetEvent<TSlimRecoMUV1Event>(
		TString ) {
	/// \MemberDescr
	/// \param outputName : Unused
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSlimRecoMUV1Event*>(GetSlimEvent("MUV1"));
}

template<> TSlimRecoMUV2Event* UserMethods::GetEvent<TSlimRecoMUV2Event>(
		TString ) {
	/// \MemberDescr
	/// \param outputName : Unused
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSlimRecoMUV2Event*>(GetSlimEvent("MUV2"));
}

template<> TSlimRecoMUV3Event* UserMethods::GetEvent<TSlimRecoMUV3Event>(
		TString ) {
	/// \MemberDescr
	/// \param outputName : Unused
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSlimRecoMUV3Event*>(GetSlimEvent("MUV3"));
}

template<> TSlimRecoNewCHODEvent* UserMethods::GetEvent<TSlimRecoNewCHODEvent>(
		TString ) {
	/// \MemberDescr
	/// \param outputName : Unused
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSlimRecoNewCHODEvent*>(GetSlimEvent("NewCHOD"));
}

template<> TSlimRecoSACEvent* UserMethods::GetEvent<TSlimRecoSACEvent>(
		TString ) {
	/// \MemberDescr
	/// \param outputName : Unused
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSlimRecoSACEvent*>(GetSlimEvent("SAC"));
}

template<> TSlimRecoSAVEvent* UserMethods::GetEvent<TSlimRecoSAVEvent>(
		TString ) {
	/// \MemberDescr
	/// \param outputName : Unused
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSlimRecoSAVEvent*>(GetSlimEvent("SAV"));
}

template<> TSlimRecoSpectrometerEvent* UserMethods::GetEvent<TSlimRecoSpectrometerEvent>(
		TString ) {
	/// \MemberDescr
	/// \param outputName : Unused
	/// \return the pointer to the event corresponding to the given output stage
	///
	/// Specialisation of GetEvent. DetectorName not needed.
	/// \EndMemberDescr
	return static_cast<TSlimRecoSpectrometerEvent*>(GetSlimEvent("Spectrometer"));
}

template<> TSlimRecoRICHEvent* UserMethods::GetEvent<TSlimRecoRICHEvent>(
		TString ) {
        /// \MemberDescr
        /// \param outputName : Unused
        /// \return the pointer to the event corresponding to the given output stage
        ///
        /// Specialisation of GetEvent. DetectorName not needed.
        /// \EndMemberDescr
       return static_cast<TSlimRecoRICHEvent*>(GetSlimEvent("RICH"));
}

Event* UserMethods::GetMCEvent() {
	/// \MemberDescr
	/// \return Pointer to the MC event.
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (fParent->IsTreeType())
		return fParent->GetIOTree()->GetMCTruthEvent();
	else
		std::cout << extended() << "[WARNING] Not reading TTrees" << std::endl;
	return nullptr;
}

EventHeader* UserMethods::GetEventHeader(TString outputName) {
	/// \MemberDescr
	/// \return Pointer to the EventHeader.
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (fParent->IsTreeType())
		return fParent->GetIOTree()->GetEventHeaderEvent(outputName);
	else
		std::cout << extended() << "[WARNING] Not reading TTrees" << std::endl;
	return nullptr;
}

TH1* UserMethods::RequestHistogram(TString directory, TString name, bool appendOnNewFile, TString saveDirectory) {
	/// \MemberDescr
	/// \param directory : Directory in the input ROOT file where this histogram will be searched
	/// \param name : Name of the searched histogram (becomes the ID if the histogram in the framework (for calls to FillHisto, ...)
	/// \param appendOnNewFile : <br>
	///  - If set to true : When a new file is opened by the TChain the value of the new histogram extracted from this file will be appended to the existing histogram.
	///  - If set to false : When a new file is opened by the TChain the current histogram will be replaced by the new one.
	/// \return A pointer to the requested histogram if it was found, else a null pointer.
	///
	/// Request histograms from input file.
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (!fParent->IsHistoType()) {
		std::cout << extended() << "[WARNING] Not reading Histos" << std::endl;
		return nullptr;
	}
	TH1* histo = fParent->GetIOHisto()->GetInputHistogram(directory, name, appendOnNewFile);

	if (!histo)
		std::cout << extended() << "Requested input histogram not found "
			  << directory << "/" << name << std::endl;
	else if(!fHisto.Exists(name)){
		if(TString(histo->ClassName()).Contains("TGraph"))
			fHisto.BookHisto(name, reinterpret_cast<TGraph*>(histo), fAnalyzerName, 0, saveDirectory, false);
		else if(TString(histo->ClassName()).Contains("TEfficiency"))
			fHisto.BookHisto(name, reinterpret_cast<TEfficiency*>(histo), fAnalyzerName, 0, saveDirectory, false);
		else if(TString(histo->ClassName()).Contains("TH2"))
			fHisto.BookHisto(name, static_cast<TH2*>(histo), fAnalyzerName, 0, saveDirectory, false);
		else if(TString(histo->ClassName()).Contains("TH3"))
			fHisto.BookHisto(name, static_cast<TH3*>(histo), fAnalyzerName, 0, saveDirectory, false);
		else
			fHisto.BookHisto(name, histo, fAnalyzerName, 0, saveDirectory, false);
	}
	return histo;
}

bool UserMethods::ImportAllInputHistogram(TString directory, bool appendOnNewFile, TString saveDirectory){
	/// \MemberDescr
	/// \param directory : Directory in the input ROOT file where the histograms will be searched
	/// \param appendOnNewFile : <br>
	///  - If set to true : When a new file is opened by the TChain the value of the new histogram extracted from this file will be appended to the existing histogram.
	///  - If set to false : When a new file is opened by the TChain the current histogram will be replaced by the new one.
	/// \return true if successfully read the file, else false
	///
	/// Request all histograms from a given directory in input file.
	/// \EndMemberDescr

	if (!fParent) return false;
	if (!fParent->IsHistoType()) {
		std::cout << extended() << "[WARNING] Not reading Histos" << std::endl;
		return false;
	}
	std::vector<TString> histoList = GetListOfHistos(directory);
	for(auto histoName : histoList){
		TH1* histo = fParent->GetIOHisto()->GetInputHistogram(directory, histoName, appendOnNewFile);
		if (!histo)
			std::cout << extended() << "Requested input histogram not found "
				  << directory << "/" << histoName << std::endl;
		else if(!fHisto.Exists(histoName)) {
			if(TString(histo->ClassName()).Contains("TGraph"))
				fHisto.BookHisto(histoName, reinterpret_cast<TGraph*>(histo), fAnalyzerName, 0, saveDirectory, false);
			else if(TString(histo->ClassName()).Contains("TEfficiency"))
				fHisto.BookHisto(histoName, reinterpret_cast<TEfficiency*>(histo), fAnalyzerName, 0, saveDirectory, false);
			else if(TString(histo->ClassName()).Contains("TH2"))
				fHisto.BookHisto(histoName, static_cast<TH2*>(histo), fAnalyzerName, 0, saveDirectory, false);
			else if(TString(histo->ClassName()).Contains("TH3"))
				fHisto.BookHisto(histoName, static_cast<TH3*>(histo), fAnalyzerName, 0, saveDirectory, false);
			else
				fHisto.BookHisto(histoName, histo, fAnalyzerName, 0, saveDirectory, false);
                }
	}
	return true;
}

Core::HistoHandler::IteratorTH1 UserMethods::GetIteratorTH1() {
	/// \MemberDescr
	/// \return Iterator to TH1
	///
	/// Create a TH1Iterator over all the TH1 stored in this instance of HistoHandler.
	/// \EndMemberDescr

	return fHisto.GetIteratorTH1();
}

Core::HistoHandler::IteratorTH1 UserMethods::GetIteratorTH1(TString baseName) {
	/// \MemberDescr
	/// \param baseName: BaseName of the histograms to iterate over.
	/// \return Iterator to TH1
	///
	/// Create a TH1Iterator over all the TH1 whose name is starting with baseName and stored in this instance of HistoHandler.
	/// \EndMemberDescr

	return fHisto.GetIteratorTH1(baseName);
}

Core::HistoHandler::IteratorTH2 UserMethods::GetIteratorTH2() {
	/// \MemberDescr
	/// \return Iterator to TH2
	///
	/// Create a TH2Iterator over all the TH2 stored in this instance of HistoHandler.
	/// \EndMemberDescr

	return fHisto.GetIteratorTH2();
}

Core::HistoHandler::IteratorTH2 UserMethods::GetIteratorTH2(TString baseName) {
	/// \MemberDescr
	/// \param baseName: BaseName of the histograms to iterate over.
	/// \return Iterator to TH2
	///
	/// Create a TH2Iterator over all the TH2 whose name is starting with baseName and stored in this instance of HistoHandler.
	/// \EndMemberDescr

	return fHisto.GetIteratorTH2(baseName);
}

Core::HistoHandler::IteratorTH3 UserMethods::GetIteratorTH3() {
	/// \MemberDescr
	/// \return Iterator to TH3
	///
	/// Create a TH3Iterator over all the TH3 stored in this instance of HistoHandler.
	/// \EndMemberDescr

	return fHisto.GetIteratorTH3();
}

Core::HistoHandler::IteratorTH3 UserMethods::GetIteratorTH3(TString baseName) {
	/// \MemberDescr
	/// \param baseName: BaseName of the histograms to iterate over.
	/// \return Iterator to TH3
	///
	/// Create a TH3Iterator over all the TH3 whose name is starting with baseName and stored in this instance of HistoHandler.
	/// \EndMemberDescr

	return fHisto.GetIteratorTH3(baseName);
}

Core::HistoHandler::IteratorTGraph UserMethods::GetIteratorTGraph() {
	/// \MemberDescr
	/// \return Iterator to TGraph
	///
	/// Create a TGraphIterator over all the TGraph stored in this instance of HistoHandler.
	/// \EndMemberDescr

	return fHisto.GetIteratorTGraph();
}

Core::HistoHandler::IteratorTGraph UserMethods::GetIteratorTGraph(
		TString baseName) {
	/// \MemberDescr
	/// \param baseName: BaseName of the histograms to iterate over.
	/// \return Iterator to TGraph
	///
	/// Create a TGraphIterator over all the TGraph whose name is starting with baseName and stored in this instance of HistoHandler.
	/// \EndMemberDescr

	return fHisto.GetIteratorTGraph(baseName);
}

Core::HistoHandler::IteratorTEfficiency UserMethods::GetIteratorTEfficiency() {
	/// \MemberDescr
	/// \return Iterator to TEfficiency
	///
	/// Create a TEfficiencyIterator over all the TEfficiency stored in this instance of HistoHandler.
	/// \EndMemberDescr

	return fHisto.GetIteratorTEfficiency();
}

Core::HistoHandler::IteratorTEfficiency UserMethods::GetIteratorTEfficiency(
		TString baseName) {
	/// \MemberDescr
	/// \param baseName: BaseName of the histograms to iterate over.
	/// \return Iterator to TEfficiency
	///
	/// Create a TEfficiencyIterator over all the TEfficiency whose name is starting with baseName and stored in this instance of HistoHandler.
	/// \EndMemberDescr

	return fHisto.GetIteratorTEfficiency(baseName);
}

Core::HistoHandler::IteratorCanvas UserMethods::GetIteratorCanvas() {
	/// \MemberDescr
	/// \return Iterator to CanvasOrganizer
	///
	/// Create a CanvasIterator over all the CanvasOrganizer in this instance of HistoHandler.
	/// \EndMemberDescr

	return fHisto.GetIteratorCanvas();
}

TChain* UserMethods::GetTree(TString name) {
	/// \MemberDescr
	/// \param name: Name of the TTree
	/// \return  Pointer to the specified TChain
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	return fParent->GetTree(name);
}

void* UserMethods::GetObjectVoid(TString name, TString branchName) {
	/// \MemberDescr
	/// \param name: Name of the object
	/// Internal interface to BaseAnalysis for GetObject method
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (fParent->IsTreeType())
		return fParent->GetIOTree()->GetObject(name, branchName);
	else
		std::cout << extended() << "[WARNING] Not reading TTrees" << std::endl;

	return nullptr;
}

bool UserMethods::RequestTreeVoid(TString name, TString branchName,
		TString className, void* obj) {
	/// \MemberDescr
	/// \param name: Name of the tree
	/// \param branchName: Name of the branch in the requested tree
	/// \param className: Name of the class in the requested branch
	/// \param obj: Pointer to an object of class className
	/// \return True if successful
	///
	/// Internal interface to BaseAnalysis for RequestTree method
	/// \EndMemberDescr

	if (!fParent) return false;
	if (fParent->IsTreeType())
		return fParent->GetIOTree()->RequestTree(name, branchName, className,
				obj);
	else
		std::cout << extended() << "[WARNING] Not reading TTrees" << std::endl;

	return false;
}

TH1* UserMethods::GetReferenceTH1(TString name, TString directory) {
	/// \MemberDescr
	/// \param name : Name of the reference plot
	/// \param directory : TDirectory where the histogram should be searched for.
	/// \return Pointer to the specified reference histogram. If not found, return NULL.
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (fParent->IsHistoType())
		return fParent->GetIOHisto()->GetReferenceTH1(name, directory);
	else
		std::cout << extended() << "[WARNING] Not reading Histos" << std::endl;

	return nullptr;
}

TH2* UserMethods::GetReferenceTH2(TString name, TString directory) {
	/// \MemberDescr
	/// \param name : Name of the reference plot
	/// \param directory : TDirectory where the histogram should be searched for.
	/// \return Pointer to the specified reference histogram. If not found, return NULL.
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (fParent->IsHistoType())
		return fParent->GetIOHisto()->GetReferenceTH2(name, directory);
	else
		std::cout << extended() << "[WARNING] Not reading Histos" << std::endl;

	return nullptr;
}

TGraph* UserMethods::GetReferenceTGraph(TString name, TString directory) {
	/// \MemberDescr
	/// \param name : Name of the reference plot
	/// \param directory : TDirectory where the histogram should be searched for.
	/// \return Pointer to the specified reference histogram. If not found, return NULL.
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (fParent->IsHistoType())
		return fParent->GetIOHisto()->GetReferenceTGraph(name, directory);
	else
		std::cout << extended() << "[WARNING] Not reading Histos" << std::endl;

	return nullptr;
}

Long64_t UserMethods::GetNEvents() {
	/// \MemberDescr
	/// \return Total number of events loaded from input trees.
	/// \EndMemberDescr

	if (!fParent) return -1;
	return fParent->GetNEvents();
}

bool UserMethods::GetWithMC() const {
	/// \MemberDescr
	/// \return true if the input file contains MC events
	/// \EndMemberDescr
        
	if (!fParent) return false;
	return fParent->GetIOHandler()->GetWithMC();
}

bool UserMethods::GetWithEventHeader() {
	/// \MemberDescr
	/// \return true if the input file contains EventHeader
	/// \EndMemberDescr

	if (!fParent) return false;
	if (fParent->IsTreeType())
		return fParent->GetIOTree()->GetWithEventHeader();
	else
		std::cout << normal() << "[WARNING] Not reading TTrees" << std::endl;

	return false;
}

bool UserMethods::GetIsTree() {
	/// \MemberDescr
	/// \return True if the IO Handler is able to read TTrees
	/// \EndMemberDescr

	if (!fParent) return false;
	return fParent->IsTreeType();
}

bool UserMethods::GetIsHisto() {
	/// \MemberDescr
	/// \return True if the IO Handler is only able to read histograms (--histo flag)
	/// \EndMemberDescr

	if (!fParent) return false;
	return fParent->IsHistoType();
}

TH1* UserMethods::GetInputHistogram(TString directory, TString name) {
	/// \MemberDescr
	/// \param directory : Directory in the input ROOT file where this histogram will be searched
	/// \param name : Name of the searched histogram
	/// \return A pointer to the requested histogram if it was found, else a null pointer.
	///
	/// Request histograms from input file.
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (!fParent->IsHistoType()) {
		std::cout << extended() << "[WARNING] Not reading Histos" << std::endl;
		return nullptr;
	}
	TH1* histo = fParent->GetIOHisto()->GetInputHistogram(directory, name,
			false);

	if (!histo)
		std::cout << extended() << "Requested input histogram not found "
			  << directory << "/" << name << std::endl;
	return histo;
}

std::vector<Core::InputHandler::keyPair> UserMethods::GetListOfKeys(
		TString directory) {
	/// \MemberDescr
	/// \param directory : Directory in the input ROOT file
	/// \return A vector of keys available in the given directory. The key contains
	/// the name of the object (key.name) and the className of the object (key.className)
	///
	/// Request list of keys in the input file.
	/// \EndMemberDescr

	if (!fParent) return std::vector<Core::InputHandler::keyPair>();
	return fParent->GetIOHandler()->GetListOfKeys(directory);
}

std::vector<TString> UserMethods::GetListOfDirs(TString directory) {
	/// \MemberDescr
	/// \param directory : Directory in the input ROOT file
	/// \return A vector of directories available in the given directory
	///
	/// Request list of directories in the input file.
	/// \EndMemberDescr

	if (!fParent) return std::vector<TString>();
	return fParent->GetIOHandler()->GetListOfDirs(directory);
}

std::vector<TString> UserMethods::GetListOfTH1(TString directory) {
	/// \MemberDescr
	/// \param directory : Directory in the input ROOT file
	/// \return A vector of TH1 type histograms available in the given directory
	///
	/// Request list of TH1 type histograms in the input file.
	/// \EndMemberDescr

	if (!fParent) return std::vector<TString>();
	return fParent->GetIOHandler()->GetListOfTH1(directory);
}

std::vector<TString> UserMethods::GetListOfTH2(TString directory) {
	/// \MemberDescr
	/// \param directory : Directory in the input ROOT file
	/// \return A vector of TH2 type histograms available in the given directory
	///
	/// Request list of TH2 type histograms in the input file.
	/// \EndMemberDescr

	if (!fParent) return std::vector<TString>();
	return fParent->GetIOHandler()->GetListOfTH2(directory);
}

std::vector<TString> UserMethods::GetListOfTGraph(TString directory) {
	/// \MemberDescr
	/// \param directory : Directory in the input ROOT file
	/// \return A vector of TGraph type histograms available in the given directory
	///
	/// Request list of TGraph type histograms in the input file.
	/// \EndMemberDescr

	if (!fParent) return std::vector<TString>();
	return fParent->GetIOHandler()->GetListOfTGraph(directory);
}

std::vector<TString> UserMethods::GetListOfTEfficiency(TString directory) {
	/// \MemberDescr
	/// \param directory : Directory in the input ROOT file
	/// \return A vector of TGraph type histograms available in the given directory
	///
	/// Request list of TGraph type histograms in the input file.
	/// \EndMemberDescr

	if (!fParent) return std::vector<TString>();
	return fParent->GetIOHandler()->GetListOfTEfficiency(directory);
}

std::vector<TString> UserMethods::GetListOfHistos(TString directory) {
	/// \MemberDescr
	/// \param directory : Directory in the input ROOT file
	/// \return A vector of histograms available in the given directory
	///
	/// Request list of histograms in the input file.
	/// \EndMemberDescr

	if (!fParent) return std::vector<TString>();
	return fParent->GetIOHandler()->GetListOfHistos(directory);
}

NA62Analysis::NA62Map<TString, Core::CanvasOrganizer*>::type UserMethods::GetCanvases() {
	/// \MemberDescr
	/// \return Map containing the list of CanvasOrganizer for this analyzer
	/// \EndMemberDescr

	if (!fParent) return NA62Analysis::NA62Map<TString, Core::CanvasOrganizer*>::type();
	return fHisto.GetCanvases();
}

int UserMethods::GetUpdateInterval() const {
	/// \MemberDescr
	/// \return Update interval
	/// \EndMemberDescr

	if (!fParent) return -1;
	return fHisto.GetUpdateInterval();
}

void UserMethods::CreateCanvas(TString name, int width, int height) {
	/// \MemberDescr
	/// \param name: Name of the canvas
	/// \param width: width of the canvas (default=0=automatic)
	/// \param height: height of the canvas (default=0=automatic)
	///
	/// Create a new named canvas in the analyzer
	/// \EndMemberDescr

	fHisto.CreateCanvas(name, width, height);
}

bool UserMethods::PlacePlotOnCanvas(TString histoName, TString canvasName,
		int row, int col) {
	/// \MemberDescr
	/// \param histoName: Name of the plot
	/// \param canvasName: Name of the canvas
	/// \param row: Row position on the canvas
	/// \param col: Column position on the canvas
	/// \return True if canvas and histograms were found
	///
	/// Add a plot to the list of Plots managed by the specified CanvasOrganizer
	/// \EndMemberDescr

	return fHisto.PlacePlotOnCanvas(histoName, canvasName, row, col);
}

bool UserMethods::PlacePlotOnCanvas(std::initializer_list<TString> histoNames, TString canvasName,
		int row, int col) {
	/// \MemberDescr
	/// \param histoNames: List of plots names
	/// \param canvasName: Name of the canvas
	/// \param row: Row position on the canvas
	/// \param col: Column position on the canvas
	/// \return True if canvas and histograms were found
	///
	/// Add a list of plots to the list of Plots managed by the specified CanvasOrganizer
	/// \EndMemberDescr

	return fHisto.PlacePlotOnCanvas(histoNames, canvasName, row, col);
}

bool UserMethods::SetCanvasAutoUpdate(TString canvasName) {
	/// \MemberDescr
	/// \param canvasName: Name of the canvas
	/// \return True if canvas was found
	///
	/// Mark a canvas as AutoUpdate (will be redrawn every fAutoUpdateInterval events)
	/// \EndMemberDescr

	return fHisto.SetCanvasAutoUpdate(canvasName);
}

bool UserMethods::UpdateCanvas(TString canvasName) {
	/// \MemberDescr
	/// \param canvasName: Name of the canvas
	/// \return True if canvas was found
	///
	/// Force the update of a canvas
	/// \EndMemberDescr

	return fHisto.UpdateCanvas(canvasName);
}

void UserMethods::SetCanvasReference(TString canvasName, TString histo,
		TH1* refPtr) {
	/// \MemberDescr
	/// \param canvasName Name of the canvas that contains the histogram to which the reference will be added
	/// \param histo Name of the histogram to which the reference will be added
	/// \param refPtr Pointer to the reference histogram to link to histo.
	///
	/// Add a reference histogram to the specified histogram in the specified canvas
	/// \EndMemberDescr

	fHisto.SetCanvasReference(canvasName, histo, refPtr);
}

void UserMethods::SetCanvasReference(TString canvasName, TString histo,
		TGraph* refPtr) {
	/// \MemberDescr
	/// \param canvasName Name of the canvas that contains the histogram to which the reference will be added
	/// \param histo Name of the histogram to which the reference will be added
	/// \param refPtr Pointer to the reference histogram to link to histo.
	///
	/// Add a reference histogram to the specified histogram in the specified canvas
	/// \EndMemberDescr

	fHisto.SetCanvasReference(canvasName, histo, refPtr);
}

void UserMethods::CallReconfigureAnalyzer(TString analyzerName,
		TString parameterName, TString parameterValue) {
	/// \MemberDescr
	/// \param analyzerName : Analyzer to reconfigure
	/// \param parameterName : Parameter to change
	/// \param parameterValue : New value for the parameter
	///
	/// Reconfigure an analyzer at processing time (parameters).
	/// \EndMemberDescr

	if (!fParent) return;
	fParent->ReconfigureAnalyzer(analyzerName, parameterName, parameterValue);
}

void UserMethods::AddPrimitiveReader(TString detName, bool sorted) {
	/// \MemberDescr
	/// \param detName: Detector name for which the PrimitiveReader should be created
	/// \param sorted: Flag to turn on/off the sorting of primitives (default: off)
	///
	/// Creates a new PrimitiveReader for the requested detector.
	/// \EndMemberDescr

	if (!fParent) return;
	if (fParent->GetIOPrimitive())
		fParent->GetIOPrimitive()->AddReader(detName, sorted);
	else
		std::cout << normal()
				<< "Trying to use primitives but no primitive file provided."
				<< std::endl;
}

TPrimitive* UserMethods::FindMatchingPrimitive(TString detName) {
	/// \MemberDescr
	/// \param detName: Name of the detector to get the primitive from.
	/// \return Pointer to the primitive corresponding to the event if found, else nullptr
	///
	/// Read the primitives and return the primitive closest to the event time.
	/// If the primitive found is too far away from the event (outside of the
	/// L0MatchingWindow around the event), the primitive is discarded.
	/// \EndMemberDescr

	if (!fParent) return nullptr;
	if (!fParent->GetIOPrimitive()) {
		std::cout << normal()
				<< "Trying to use primitives but no primitive file provided."
				<< std::endl;
		return nullptr;
	}
	return fParent->GetIOPrimitive()->GetReader(detName)->FindMatchingPrimitive(
			GetEventHeader()->GetTimeStamp(), GetEventHeader()->GetFineTime());
}

std::vector<TPrimitive> UserMethods::FindAllPrimitiveInMatchingWindow(
		TString detName) {
	/// \MemberDescr
	/// \param detName: name of the detector to get the primitives from.
	/// \return Vector of all the Primitives found close to the Event time
	///
	/// Read the primitives and return the list of primitives within a time
	/// window of L0MatchingWindow around the event time.
	/// \EndMemberDescr

	if (!fParent) return std::vector<TPrimitive>();
	if (!fParent->GetIOPrimitive()) {
		std::cout << normal()
				<< "Trying to use primitives but no primitive file provided."
				<< std::endl;
		return std::vector<TPrimitive>();
	}
	return fParent->GetIOPrimitive()->GetReader(detName)->FindAllPrimitiveInMatchingWindow(
			GetEventHeader()->GetTimeStamp(), GetEventHeader()->GetFineTime());
}

void UserMethods::SetL0MatchingWindowWidth(TString detName, float ns) {
	/// \MemberDescr
	/// \param detName: detector for which the matching window should be set
	/// \param ns: L0MatchingWindow to set in ns
	///
	/// Set the L0Matching window. The window starts from ns ns before the event time
	/// and ends ns ns after the event time.
	/// \EndMemberDescr

	if (!fParent) return;
	if (fParent->GetIOPrimitive())
		fParent->GetIOPrimitive()->GetReader(detName)->SetL0MatchingWindowWidth(
				ns);
	else
		std::cout << normal()
				<< "Trying to use primitives but no primitive file provided."
				<< std::endl;
}

void UserMethods::SetL0MatchingWindowWidth(TString detName, int timeStamp,
		short fineTime) {
	/// \MemberDescr
	/// \param detName: detector for which the matching window should be set
	/// \param timeStamp: L0MatchingWindow to set in timeStamp units
	/// \param fineTime: L0MatchingWindow to set in FineTime units
	///
	/// Set the L0Matching window. The window starts from (timeStamp+fineTime/256.) timestamps
	/// units before the event time and ends (timeStamp+fineTime/256.) timestamps units
	/// after the event time.
	/// \EndMemberDescr

	if (!fParent) return;
	if (fParent->GetIOPrimitive())
		fParent->GetIOPrimitive()->GetReader(detName)->SetL0MatchingWindowWidth(
				timeStamp, fineTime);
	else
		std::cout << normal()
				<< "Trying to use primitives but no primitive file provided."
				<< std::endl;
}

TFile * UserMethods::GetCurrentFile(){
	/// \MemberDescr
	/// \return Pointer to the currently opened TFile
	/// \EndMemberDescr

	return fParent->GetCurrentFile();
}

Stream * UserMethods::GetStreamInfo(){
	return fParent->GetIOHandler()->GetStreamInfo();
}

bool UserMethods::IsFiltering(){
	return fParent->IsFiltering();
}

std::vector<Core::AnalyzerIdentifier> UserMethods::GetProcessingHistory() const {
	/// \MemberDescr
	/// \return vector containing sorted history of analyzers from input file
	/// \EndMemberDescr
	if (!fParent->GetIOHandler()) {
		std::cout << normal()
				<< "No IOHandler available to retrieve processing history."
				<< std::endl;
		return std::vector<Core::AnalyzerIdentifier>();
	}
	return fParent->GetIOHandler()->GetAnalyzerList();
}

Core::AnalyzerIdentifier UserMethods::GetAnalyzerFromBit(int bitNumber) const {
	/// \MemberDescr
	/// \param bitNumber : Bit position (start at 0)
	/// \return Identifier of the analyzer corresponding to the given bit number
	/// \EndMemberDescr

	if (!fParent->GetIOHandler()) {
		std::cout << normal()
				<< "No IOHandler available to retrieve processing history."
				<< std::endl;
		return ANIDNone;
	}
	return fParent->GetIOHandler()->GetAnalyzerFromBit(bitNumber);
}

int UserMethods::GetBitFromAnalyzer(TString analyzerID) const {
	/// \MemberDescr
	/// \param analyzerID : identification string of the analyzer (analyzer name)
	/// \return Bit position corresponding the the given analyzer
	/// \EndMemberDescr

	if (!fParent->GetIOHandler()) {
		std::cout << normal()
				<< "No IOHandler available to retrieve processing history."
				<< std::endl;
		return -1;
	}
	return fParent->GetIOHandler()->GetBitFromAnalyzer(analyzerID);
}

bool UserMethods::GetAcceptFromBit(unsigned int bitNumber) const {
	/// \MemberDescr
	/// \param bitNumber : Position of the checked bit (starts at 0)
	/// \return Accept decision from the analyzer corresponding to the given bit
	/// \EndMemberDescr

	if (!fParent->GetIOTree()) {
		std::cout << normal()
				<< "No IOTree available to retrieve filtering history."
				<< std::endl;
		return false;
	}
	return fParent->GetIOTree()->GetAcceptFromBit(bitNumber);
}

bool UserMethods::GetAcceptFromAnalyzer(TString analyzerID) const {
	/// \MemberDescr
	/// \param analyzerID : Identification string of the analyzer (analyzer name)
	/// \return Accept decision from the analyzer
	/// \EndMemberDescr

	if (!fParent->GetIOTree()) {
		std::cout << normal()
				<< "No IOTree available to retrieve filtering history."
				<< std::endl;
		return false;
	}
	return fParent->GetIOTree()->GetAcceptFromAnalyzer(analyzerID);
}

int64_t UserMethods::GetFilterWord() const {
	/// \MemberDescr
	/// \return Filter word retrieved from input file
	/// \EndMemberDescr

	if (!fParent->GetIOTree()) {
		std::cout << normal()
				<< "No IOTree available to retrieve filtering history."
				<< std::endl;
		return false;
	}
	return fParent->GetIOTree()->GetFilterWord();
}

bool UserMethods::IsAnalyzerInHistory(TString analyzerID) const {
	/// \MemberDescr
	/// \param analyzerID : identifier of the analyzer
	/// \return true if the specified analyzer is found in the history of the
	/// input file.
	/// \EndMemberDescr

	std::vector<Core::AnalyzerIdentifier> list = GetProcessingHistory();
	if(std::any_of(list.begin(), list.end(), [&analyzerID](AnalyzerIdentifier &anID){ return anID == analyzerID; }))
		return true;
	return false;
}

bool UserMethods::AnalyzerRanOnEvent(TString analyzerID) const {
	bool otherRun = IsAnalyzerInHistory(analyzerID);
	bool thisRun = fParent->AnalyzerRanOnEvent(fAnalyzerName, analyzerID);
	return otherRun || thisRun;
}

void UserMethods::RedrawAnalyzerOM(){
	fParent->UpdateOMWindowForAnalyzer(fAnalyzerName);
}

int UserMethods::GetRunID() {
	return fParent->GetIOHandler()->GetRunID();
}

int UserMethods::GetBurstID() {
	return fParent->GetIOHandler()->GetBurstID();
}

int UserMethods::GetBurstTime() {
	return fParent->GetIOHandler()->GetBurstTime();
}

TString UserMethods::GetRevision() {
	return fParent->GetIOHandler()->GetRevision();
}

MCInfo* UserMethods::GetMCInfo() {
	return fParent->GetIOHandler()->GetMCInfo();
}

RecoInfo* UserMethods::GetRecoInfo() {
	return fParent->GetIOHandler()->GetRecoInfo();
}

bool UserMethods::BadQualityMask(EventHeader* header, DetectorID detector) {
	/// \MemberDescr
	/// \param header: Pointer to EventHeader
	/// \param det: DetectorID of the detector to check
	/// \return True if the quality bit for the specified detector is bad.
	/// \EndMemberDescr
	return EventChecker::BadQualityMask(header, detector);
}

} /* namespace NA62Analysis */

