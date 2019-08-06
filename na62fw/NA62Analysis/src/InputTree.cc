/*
 * IOTree.cc
 *
 *  Created on: Mar 9, 2015
 *      Author: ncl
 */

#include "InputTree.hh"

#include <signal.h>
#include <iostream>
#include <sstream>

#include <TChain.h>
#include <TKey.h>
#include <TTreeCache.h>
#include <TBranchElement.h>
#include <TChainElement.h>

#include "StringBalancedTable.hh"
#include "NA62Exceptions.hh"
#include "TermManip.hh"
#include "BadBursts.hh"
#include "Indexer.hh"
#include "ConfigSettings.hh"
#include "NA62ConditionsService.hh"
#include "PersistencyChanger.hh"

namespace NA62Analysis {
namespace Core {

InputTree::InputTree() :
		InputHisto("IOTree"), fMCTruthTree(0), fEventHeaderRecoTree(0), fEventHeaderSpecialTree(0), fReferenceTree(0),
                fWithMC(false), fWithEventHeader(false),
                fAllowNonExisting(false), fFilterWord(0), fUseFilterBranch(false), fNextBurstIndex(-1), fNextSpecialBurstIndex(-1) {
	/// \MemberDescr
	/// Constructor
	/// \EndMemberDescr
	fIOType = IOHandlerType::kTREE;
}

InputTree::InputTree(const InputTree &c) :
		InputHisto(c), fTree(c.fTree), fEvent(c.fEvent), fObject(c.fObject), fMCTruthTree(
				c.fMCTruthTree), fEventHeaderRecoTree(c.fEventHeaderRecoTree), fEventHeaderSpecialTree(c.fEventHeaderSpecialTree), fReferenceTree(
				c.fReferenceTree), fWithMC(c.fWithMC), fWithEventHeader(c.fWithEventHeader),
				fAllowNonExisting(false), fFilterWord(0), fUseFilterBranch(false), fNextBurstIndex(-1), fNextSpecialBurstIndex(-1) {
	/// \MemberDescr
	/// \param c: Reference to the object to copy
	///
	/// Copy constructor
	/// \EndMemberDescr

	fIOType = c.GetIOType();
}

InputTree::~InputTree() {
	/// \MemberDescr
	/// Destructor
	/// \EndMemberDescr

	treeIterator itChain;
	objectTreeIterator itTreeObject;
	objectIterator itObject;
	eventDetIterator itTreeEvent;
	eventIterator itEvent;

	while (fTree.size() > 0) {
		itChain = fTree.begin();
		delete itChain->second;
		fTree.erase(itChain);
	}
	while (fEvent.size() > 0) {
		itTreeEvent = fEvent.begin();
		while(itTreeEvent->second.size()>0){
			itEvent = itTreeEvent->second.begin();
			delete itEvent->second;
			itTreeEvent->second.erase(itEvent);
		}
		fEvent.erase(itTreeEvent);
	}
	while (fObject.size() > 0) {
		itTreeObject = fObject.begin();
		while(itTreeObject->second.size()>0){
			itObject = itTreeObject->second.begin();
			delete itObject->second;
			itTreeObject->second.erase(itObject);
		}
		fObject.erase(itTreeObject);
	}
}

void InputTree::RequestTree(TString detectorName, TDetectorVEvent * const evt, TString outputStage) {
	/// \MemberDescr
	/// \param detectorName : Name of the requested Detector
	/// \param evt : Pointer to an instance of detector event (MC or Reco)
	/// \param outputStage : Name of the output type to request (Reco, MC, Digis, ...)
	///
	/// Request a branch in a tree in the input file. If the tree has already been requested before,
	/// only add the new branch.
	/// If outputName is not specified, the tree "Reco" or "Hits" will be used (depending on the
	/// TDetectorVEvent class instance). If you need a different tree, please specify the name of the tree
	/// (e.g. Digis)
	/// \EndMemberDescr

	std::cout << extended() << "Requesting Detector branch " << detectorName
			<< std::endl;
	eventDetIterator itTree;
	eventIterator it;

	//Which branch are we dealing with?
	if (outputStage.CompareTo("") == 0) {
		if (strstr(evt->ClassName(), "Reco") != NULL)
			outputStage = "Reco";
		else if (strstr(evt->ClassName(), "Digi") != NULL)
			outputStage = "Digis";
		else if (strstr(evt->ClassName(), "SpecialTrigger") != NULL)
			outputStage = "SpecialTrigger";
		else
			outputStage = "MC";
	}

	//Create the tree if not yet requested

	if (fTree.count(outputStage) == 0) {
		std::cout << debug() << "First request... Creating TTree" << std::endl;
		fTree.insert(std::make_pair(outputStage, new ChainObject(outputStage)));
	}

	std::cout << extended() << "TTree name set to " << outputStage << std::endl;

	//Is this branch of this tree already requested?
	//If yes delete evt and return (we already have the branching object instance)
	itTree = fEvent.find(detectorName);
	//Loop over detectors with this name
	if (itTree != fEvent.end()) {
		it = itTree->second.find(outputStage);
		//Does it point to this TTree?
		if (it != itTree->second.end()) {
			if (TString(it->second->fEvent->ClassName()).CompareTo(
					evt->ClassName()) == 0) {
				std::cout << debug() << "Branch already requested... Continue"
						<< std::endl;
				delete evt;
				return;
			} else {
				std::cout << normal()
						<< "[ERROR] Branch already requested with different class type: "
						<< it->second->fEvent->ClassName() << " (!= "
						<< evt->ClassName() << ")" << std::endl;
				delete evt;
				throw LogicException();
			}
		}
	}
	else
		fEvent.insert(std::make_pair(detectorName, eventMap::type()));

	fEvent[detectorName].insert(std::make_pair(outputStage, new EventTriplet(outputStage, evt)));
}

bool InputTree::RequestTree(TString treeName, TString branchName, TString className, void* const obj) {
	/// \MemberDescr
	/// \param treeName : Name of the requested TTree
	/// \param branchName : Name of the Branch to retrieve
	/// \param className : Name of the class type in this branch
	/// \param obj : Pointer to an instance of any class
	/// \return False if the branch is already requested. The pointer given here should be deleted
	///
	/// Request a tree in the input file. If already requested before, do nothing.
	/// \EndMemberDescr

	std::cout << extended() << "Requesting branch " << branchName
			<< " of generic TTree " << treeName << std::endl;
	std::cout << extended() << "Object class is expected to be " << className
			<< std::endl;
	objectTreeIterator itTree;
	objectIterator it;

	//Create the tree if not yet requested
	if (fTree.count(treeName) == 0) {
		std::cout << debug() << "First request... Creating TTree" << std::endl;
		fTree.insert(std::make_pair(treeName, new ChainObject(treeName)));
	}

	//Is this branch of this tree already requested?
	//If yes signal the caller that obj shoud be deleted and return (we already have the branching object instance)
	itTree = fObject.find(treeName);
	//Loop over all objects with this name
	if(itTree != fObject.end()){
		it = itTree->second.find(branchName);
		//Does it point to the same branch?
		if (it != itTree->second.end()) {
			std::cout << debug() << "Branch already requested... Continue"
					<< std::endl;
			return false;
		}
	}
	else
		fObject.insert(std::make_pair(treeName, objectMap::type()));

	fObject[treeName].insert(std::make_pair(branchName, new ObjectTriplet(className, branchName, obj)));
	return true;
}

Long64_t InputTree::BranchTrees(Long64_t eventNb) {
	/// \MemberDescr
	///	\param eventNb : Number of events that should be read in the tree
	/// \return Number of events found in the Tree
	///
	/// Effectively read all the requested trees in the input file and branch them
	/// \EndMemberDescr

	objectTreeIterator itTreeObj;

	if (!fReferenceTree && fTree.size() > 0)
		fReferenceTree = fTree.begin()->second->GetChain();

	for (auto itTree : fTree) {
		TObjArray * arr = itTree.second->GetChain()->GetListOfBranches();
		if (arr == nullptr) {
			if (IsKnownTree(itTree.first)) {
				std::cout << extended() << "Tree " << itTree.first
						<< " not found in input file. Disabling..."
						<< std::endl;
				itTree.second->Disable();
				continue;
			}
			std::cout << normal() << "[Error] Requested tree not found in input file: "
					<< itTree.first << std::endl;
			throw LogicException();
		}
		for (int i = 0; i < arr->GetEntries(); ++i) {
			TBranch *b = static_cast<TBranch*>(arr->At(i));
			std::cout << extended() << "Disabling branch " << b->GetName()
					<< " from tree " << itTree.second->GetChain()->GetName()
					<< std::endl;
			b->ResetAddress();
			b->SetStatus(0);
		}
	}
	//Loop over all detector branches and branch them
	for (auto itDetEvts : fEvent) {
		for(auto itEvts : itDetEvts.second){
			treeIterator itTree = fTree.find(itEvts.first);
			if (!itTree->second->IsEnabled())
				continue;
			itEvts.second->fEnabled = FindAndBranchTree(itTree->second->GetChain(), itDetEvts.first,
				itEvts.second->fEvent->ClassName(), &(itEvts.second->fEvent));
		}
	}

	//Loop over all generic branches and branch them
	for (itTreeObj = fObject.begin(); itTreeObj != fObject.end(); ++itTreeObj) {
		treeIterator itTree = fTree.find(itTreeObj->first);
		if (!itTree->second->IsEnabled())
			continue;
		for(auto itObj : itTreeObj->second)
			itObj.second->fEnabled = FindAndBranchTree(itTree->second->GetChain(), itObj.second->fBranchName,
				itObj.second->fClassName, &(itObj.second->fObject));
	}

	if (eventNb == -1)
		eventNb = GetNEvents();

	for (auto itTree : fTree) {
		itTree.second->GetChain()->SetCacheSize(400000000);
		itTree.second->GetChain()->SetCacheLearnEntries(2);

	}

	treeIterator itTree;
	if (fReferenceTree && (itTree = fTree.find(fReferenceTree->GetName())) != fTree.end())
		BranchFilterBranch(itTree->second->GetChain());

	return eventNb;
}

TDetectorVEvent *InputTree::GetEvent(TString detName, TString outputName) {
	/// \MemberDescr
	/// \param detName : Name of the detector from which the event is read (branch)
	/// \param outputName : Name of the output stage (Reco, Digis, MC) (TTree)
	/// \return Pointer to the event corresponding to the given tree and the given branch.
	///
	/// If outputName is left empty and there is only 1 tree requested for this detector, this
	/// single tree is returned. If there is more than 1 tree requested for this detector,
	/// return either the "Reco" or the "Hits" tree (the first one found - undefined behaviour
	/// if both "Reco" and "Hits" trees have been requested).
	/// If outputName is specified, try to return the specified tree.
	/// \EndMemberDescr

	eventDetIterator itDetEvt;

	itDetEvt = fEvent.find(detName);
	if (itDetEvt == fEvent.end()) { //No such event found
		std::cout << normal() << "[Error] Unable to find event in branch "
				<< detName << " of tree " << outputName << std::endl;
		return nullptr;
	} else {
		TString mainTree(outputName);
		if (outputName.CompareTo("") == 0) {
			mainTree = DetermineMainTree(itDetEvt->second);
			std::cout << trace() << "No TTree specified... Using " << mainTree << std::endl;
		}

		auto itEvts = itDetEvt->second.find(mainTree);
		if (itEvts != itDetEvt->second.end()) {
			std::cout << trace() << "Using branch " << mainTree << std::endl;
			return itEvts->second->fEvent;
		}
		std::cout << normal() << "[Error] Unable to find event in branch "
				<< detName << " of tree " << mainTree << std::endl;
	}
	return nullptr;
}

void *InputTree::GetObject(TString name, TString branchName, bool silent) {
	/// \MemberDescr
	/// \param name : Name of the TTree from which the object is read
	/// \param branchName : Name of the branch
	///
	///
	/// Return the pointer to the event corresponding to the given tree and the given branch.
	/// If branchName is left empty and there is only 1 branch requested on this tree, this
	/// single branch is returned. If there is more than 1 branch requested on this tree,
	/// return the first one.
	/// If branchName is specified, try to return the specified branch.
	/// \EndMemberDescr

	objectTreeIterator itTreeObj;

	itTreeObj = fObject.find(name);
	if (itTreeObj == fObject.end()) { //No such object found
		std::cout << normal() << "[Error] Unable to find object in branch "
				<< branchName << " of tree " << name << std::endl;
		return nullptr;
	} else {
		// If the branch is not specified return the first entry if available
		if(branchName.CompareTo("") == 0 && itTreeObj->second.size()>0){
			std::cout << trace() << "Using branch "
					<< itTreeObj->second.begin()->second->fBranchName << std::endl;
			return itTreeObj->second.begin()->second->fObject;
		}
		auto itObj = itTreeObj->second.find(branchName);
		if(itObj != itTreeObj->second.end()){
			// If the requested branch is found, return it
			std::cout << trace() << "Using branch "
						<< itObj->second->fBranchName << std::endl;
				return itObj->second->fObject;
		}
	}
	if(!silent)
		std::cout << normal() << "[Error] Unable to find object in branch " << branchName
			<< " of tree " << name << std::endl;
	return nullptr;
}

bool InputTree::LoadEvent(Long64_t &iEvent) {
	/// \MemberDescr
	/// \param iEvent : Index of the event
	/// \return true
	///
	/// Load the event from the TTrees
	/// \EndMemberDescr

	treeIterator it;

	fPreviousEvent = fCurrentEvent;
	if (fSkipBadBurst && !fWithMC) {
		Long64_t goodEvent = SkipBadBurst(iEvent);
		if (goodEvent == -1) {
		  //No more good bursts, stop processing
		  iEvent = GetNEvents();
		  return false;
		}
		if (goodEvent != iEvent) {
		  std::cout << normal() << "Skipping bad burst from event " << iEvent
			    << " to event " << goodEvent << std::endl;
		  iEvent = goodEvent;
		}
	}
	std::cout << debug() << "Loading event " << iEvent << "... " << std::endl;

	if (fGraphicalMutex->Lock() == 0) {
		//Loop over all our trees
		for (auto itTree : fTree) {
			if (!itTree.second->IsEnabled() || itTree.first.CompareTo("SpecialTrigger")==0)
				continue;

			Long64_t localEntry = itTree.second->GetChain()->LoadTree(iEvent);

			if (itTree.second->GetChain() == fReferenceTree && fUseFilterBranch) {
				fIOTimeCount.Start();
				fReferenceTree->GetBranch("FilterWord")->GetEntry(localEntry);
				fIOTimeCount.Stop();
			}

			//Loop over all event and object branch and load the corresponding entry for each of them
			eventMap::type tempEvtMap = GetEventsForTree(itTree.first);
			objectMap::type tempObjMap = GetObjectsForTree(itTree.first);

			for (auto itEvt : tempEvtMap) {
			    if(!itEvt.second->fEnabled) continue;
				TBranch *evtBranch = itTree.second->GetChain()->GetBranch(itEvt.first);
				if (evtBranch) {
					std::cout << trace() << "Getting entry " << iEvent
							<< " for " << itEvt.first << std::endl;
					fIOTimeCount.Start();
					evtBranch->GetEntry(localEntry);
					fIOTimeCount.Stop();
				}
				else{
				    std::cout << normal() << "Cannot get branch " << itEvt.first << ". The current file might be corrupted." << std::endl;
				    throw LogicException();
				}
			}

			for (auto itObj : tempObjMap) {
			    if(!itObj.second->fEnabled) continue;
				TBranch *evtBranch = itTree.second->GetChain()->GetBranch(itObj.second->fBranchName);
				if (evtBranch) {
					std::cout << trace() << "Getting entry " << iEvent
							<< " for " << itObj.second->fBranchName << std::endl;
					fIOTimeCount.Start();
					evtBranch->GetEntry(localEntry);
					fIOTimeCount.Stop();
				}
                else{
                    std::cout << normal() << "Cannot get branch " << itObj.first <<
                            ". The current file (" << GetCurrentFile()->GetName() << ") might be corrupted." << std::endl;
                    throw LogicException();
                }
			}
		}

		for(auto slimRecoPair : fPersAutoTransform) // Automatically transform all the Slim to Reco if needed
		    slimRecoPair.first->ToReco(slimRecoPair.second);

		fCurrentEvent = iEvent;
		fGraphicalMutex->UnLock();
	}
	return true;
}

bool InputTree::Rewind() {
	/// \MemberDescr
	/// \return true if the file was loaded successfully, else false
	///
	/// Load the previous good event without changing any of the other internal state variables.
	/// \EndMemberDescr

	if(fPreviousEvent==-1) return false;

	if (fGraphicalMutex->Lock() == 0) {
		//Loop over all our trees
		for (auto it : fTree) {
			if (!it.second->IsEnabled() || it.first.CompareTo("SpecialTrigger")==0)
				continue;

			Long64_t localEntry = it.second->GetChain()->LoadTree(fPreviousEvent);

			if (it.second->GetChain() == fReferenceTree && fUseFilterBranch) {
				fIOTimeCount.Start();
				fReferenceTree->GetBranch("FilterWord")->GetEntry(localEntry);
				fIOTimeCount.Stop();
			}

			//Loop over all event and object branch and load the corresponding entry for each of them
			eventMap::type tempEvtMap = GetEventsForTree(it.first);
			objectMap::type tempObjMap = GetObjectsForTree(it.first);

			for (auto itEvt : tempEvtMap) {
				TBranch *evtBranch = it.second->GetChain()->GetBranch(itEvt.first);
				if (evtBranch) {
					fIOTimeCount.Start();
					evtBranch->GetEntry(localEntry);
					fIOTimeCount.Stop();
				}
			}

			for (auto itObj : tempObjMap) {
				TBranch *evtBranch = it.second->GetChain()->GetBranch(itObj.second->fBranchName);
				if (evtBranch) {
					fIOTimeCount.Start();
					evtBranch->GetEntry(localEntry);
					fIOTimeCount.Stop();
				}
			}
		}

        for(auto slimRecoPair : fPersAutoTransform) // Automatically transform all the Slim to Reco if needed
            slimRecoPair.first->ToReco(slimRecoPair.second);

		if(fCurrentFileNumber != fReferenceTree->GetTreeNumber()){
			fCurrentFile = fReferenceTree->GetCurrentFile();
			LoadMCStream(fReferenceTree->GetTreeNumber());
		}
		fGraphicalMutex->UnLock();
	}
	return true;
}

bool InputTree::ReloadLatest() {
	/// \MemberDescr
	/// \return true if the file was loaded successfully, else false
	///
	/// Load the current good event without changin any of the other internal state variables.
	/// \EndMemberDescr

	if(fCurrentEvent==-1) return false;

	if (fGraphicalMutex->Lock() == 0) {
		//Loop over all our trees
		bool hasToReloadStream = fCurrentFileNumber != fReferenceTree->GetTreeNumber();

		for (auto it : fTree) {
			if (!it.second->IsEnabled() || it.first.CompareTo("SpecialTrigger")==0)
				continue;

			Long64_t localEntry = it.second->GetChain()->LoadTree(fCurrentEvent);

			if (it.second->GetChain() == fReferenceTree && fUseFilterBranch) {
				fIOTimeCount.Start();
				fReferenceTree->GetBranch("FilterWord")->GetEntry(localEntry);
				fIOTimeCount.Stop();
			}

			//Loop over all event and object branch and load the corresponding entry for each of them
			eventMap::type tempEvtMap = GetEventsForTree(it.first);
			objectMap::type tempObjMap = GetObjectsForTree(it.first);

			for (auto itEvt : tempEvtMap) {
				TBranch *evtBranch = it.second->GetChain()->GetBranch(itEvt.first);
				if (evtBranch) {
					fIOTimeCount.Start();
					evtBranch->GetEntry(localEntry);
					fIOTimeCount.Stop();
				}
			}

			for (auto itObj : tempObjMap) {
				TBranch *evtBranch = it.second->GetChain()->GetBranch(itObj.second->fBranchName);
				if (evtBranch) {
					fIOTimeCount.Start();
					evtBranch->GetEntry(localEntry);
					fIOTimeCount.Stop();
				}
			}
		}

        for(auto slimRecoPair : fPersAutoTransform) // Automatically transform all the Slim to Reco if needed
            slimRecoPair.first->ToReco(slimRecoPair.second);

		if(hasToReloadStream) {
			fCurrentFile = fReferenceTree->GetCurrentFile();
			LoadMCStream(fCurrentFileNumber);
		}
		fGraphicalMutex->UnLock();
	}
	return true;
}

bool InputTree::LoadSpecialEvent(Long64_t &iEvent) {
	/// \MemberDescr
	/// \param iEvent : Index of the event
	/// \return true
	///
	/// Load the event from the TTrees
	/// \EndMemberDescr

	treeIterator it;

	if (fSkipBadBurst && !fWithMC) {
		Long64_t goodEvent = SkipBadBurst(iEvent, true);
		if (goodEvent == -1) {
			//No more good bursts, stop processing
			iEvent = GetNSpecialEvents();
			return false;
		}
		if (goodEvent != iEvent) {
			std::cout << normal() << "Skipping bad burst from special event " << iEvent
					<< " to special event " << goodEvent << std::endl;
			iEvent = goodEvent;
		}
	}
	std::cout << debug() << "Loading special event " << iEvent << "... " << std::endl;

	if (fGraphicalMutex->Lock() == 0) {
		//Loop over all our trees
		it = fTree.find("SpecialTrigger");
		if (it != fTree.end()) {
			if (it->second->IsEnabled()){
				Long64_t localEntry = it->second->GetChain()->LoadTree(iEvent);

				//Loop over all event and object branch and load the corresponding entry for each of them
				eventMap::type tempEvtMap = GetEventsForTree(it->first);
				objectMap::type tempObjMap = GetObjectsForTree(it->first);

				for (auto itEvt : tempEvtMap) {
					TBranch *evtBranch = it->second->GetChain()->GetBranch(itEvt.first);
					if (evtBranch) {
						std::cout << trace() << "Getting entry " << iEvent
								<< " for " << itEvt.first << std::endl;
						fIOTimeCount.Start();
						evtBranch->GetEntry(localEntry);
						fIOTimeCount.Stop();
					}
				}

				for (auto itObj : tempObjMap) {
					TBranch *evtBranch = it->second->GetChain()->GetBranch(itObj.second->fBranchName);
					if (evtBranch) {
						std::cout << trace() << "Getting special entry " << iEvent
								<< " for " << itObj.second->fBranchName << std::endl;
						fIOTimeCount.Start();
						evtBranch->GetEntry(localEntry);
						fIOTimeCount.Stop();
					}
				}
			}
		}
		fGraphicalMutex->UnLock();
	}
	return true;
}

Event* InputTree::GetMCTruthEvent() {
	/// \MemberDescr
	/// \return Pointer to the MCTruthEvent
	/// \EndMemberDescr

	if (!fWithMC)
		return nullptr;
	objectTreeIterator itObjTree;
	objectIterator itObj;

	itObjTree = fObject.find("Reco");
	if (itObjTree != fObject.end()) {
		itObj = itObjTree->second.find("Generated");
		if (itObj != itObjTree->second.end())
			return static_cast<Event*>(itObj->second->fObject);
	}
	itObjTree = fObject.find("MC");
	if (itObjTree != fObject.end()) {
		itObj = itObjTree->second.find("Generated");
		if (itObj != itObjTree->second.end())
			return static_cast<Event*>(itObj->second->fObject);
	}

	std::cout << normal() << "MCTruth not found in tree Reco" << std::endl;
	return nullptr;
}

bool InputTree::GetWithMC() const {
	/// \MemberDescr
	/// \return True if the Event Tree is present
	///
	/// Do we have MC available in the files?
	/// \EndMemberDescr

	return fWithMC;
}

EventHeader* InputTree::GetEventHeaderEvent(TString treeName) {
	/// \MemberDescr
	/// \param treeName: Name of the tree from which EventHeader should be extracted
	/// \return Pointer to the EventHeader
	/// \EndMemberDescr

	if (!fWithEventHeader)
		return nullptr;
	objectTreeIterator itObjTree;
	objectIterator itObj;

	itObjTree = fObject.find(treeName);
	if (itObjTree != fObject.end()) {
		itObj = itObjTree->second.find("EventHeader");
		if (itObj != itObjTree->second.end())
			return static_cast<EventHeader*>(itObj->second->fObject);
	}
	if(treeName.EqualTo("Reco")){
		itObjTree = fObject.find("SlimReco");
		if (itObjTree != fObject.end()) {
			itObj = itObjTree->second.find("EventHeader");
			if (itObj != itObjTree->second.end())
				return static_cast<EventHeader*>(itObj->second->fObject);
		}
	}
	std::cout << standard() << "EventHeader not found in tree " << treeName << std::endl;
	return nullptr;
}

bool InputTree::GetWithEventHeader() const {
	/// \MemberDescr
	/// \return True if the EventHeader Tree is present
	///
	/// Do we have EventHeader available in the files?
	/// \EndMemberDescr

	return fWithEventHeader;
}

TChain* InputTree::GetTree(TString name) {
	/// \MemberDescr
	/// \param name : Name of the TTree to retrieve
	///	\return Pointer to the specified TTree
	/// \EndMemberDescr
	treeIterator it;

	if ((it = fTree.find(name)) != fTree.end()) {
		return it->second->GetChain();
	} else {
		std::cout << normal() << "The requested TTree pointer " << name
				<< " cannot be found" << std::endl;
	}
	return nullptr;
}

bool InputTree::TryBranchSlim(TChain *recoTree, TString branchName, TString branchClass) {
    TFile *fd = recoTree->GetCurrentFile();
    if(!fd->FindKey("SlimReco")) // No slim reco available.
        return false;

    auto slimRecoTree = fTree.find("SlimReco");

    std::pair<TString,TSlimRecoVEvent*> slimClass = getSlimClassEquivalent(branchClass);

    if(slimClass.second==nullptr){
        std::cout << normal() << "The Slim persistency equivalent of " << branchClass << " could not be found." << std::endl;
        std::cout << normal() << "Stop trying to branch the Slim branch " << branchName << std::endl;
        return false;
    }

    RequestTree("SlimReco", branchName, slimClass.first, slimClass.second);
    fPersAutoTransform.push_back(
            std::make_pair(reinterpret_cast<TSlimRecoVEvent*>(GetObject("SlimReco", branchName)),
                           reinterpret_cast<TRecoVEvent*>(GetEvent(branchName, "Reco")))
    );
    if(slimRecoTree==fTree.end()){
        TObjArray* files = recoTree->GetListOfFiles();
        TChain *slimTree = fTree["SlimReco"]->GetChain();
        for(int iFile=0; iFile<files->GetEntries(); ++iFile){
            fIOTimeCount.Start();
            slimTree->AddFile(static_cast<TChainElement*>(files->At(iFile))->GetTitle());
            fIOTimeCount.Stop();
        }
    }
    return true;
}

bool InputTree::FindAndBranchTree(TChain* tree, TString branchName,
		TString branchClass, void* const evt) {
	/// \MemberDescr
	/// \param tree :
	/// \param branchName : name of the branch
	/// \param branchClass : name of the branch class
	/// \param evt :
	/// \return False if the branch is not found
	///
	/// Branch the tree.
	/// \EndMemberDescr

	TObjArray* branchesList;
	Int_t jMax;

	std::cout << debug() << "Trying to branch " << branchName << " of type "
			<< branchClass << std::endl;

	fIOTimeCount.Start();
	std::cout << debug() << "Retrieving list of branches in " << tree->GetName()
			<< " tree" << std::endl;
	branchesList = tree->GetListOfBranches();
	fIOTimeCount.Stop();
	if (!branchesList) {
		if (!fAllowNonExisting && !IsKnownTree(tree->GetName())) {
			std::cout << normal() << "[ERROR] Unable to find TTree " << tree->GetName()
					<< std::endl;
			throw LogicException();
		} else {
			std::cout << normal() << "[Warning] Unable to find TTree "
					<< tree->GetName()
					<< ". Retrieved corresponding event will always be empty";
			return false;
		}
	}
	jMax = branchesList->GetEntries();
	for (Int_t j = 0; j < jMax; j++) {
		if (TString(branchName).CompareTo(branchesList->At(j)->GetName()) == 0) {
			if (TString("TBranchElement").CompareTo(branchesList->At(j)->ClassName()) == 0) {	// This is a complex class branch
				if (TString(branchClass).CompareTo(static_cast<TBranch*>(branchesList->At(j))->GetClassName()) != 0) {
					std::cout << normal() << "[ERROR] "
							<< "Input file inconsistent. Bad Event class (Found: "
							<< static_cast<TBranch*>(branchesList->At(j))->GetClassName()
							<< ", expected: " << branchClass << ") for "
							<< tree->GetTree()->GetName() << std::endl;
					throw LogicException();
				}
				std::cout << debug() << "Found " << branchName << " of class "
						<< branchClass << std::endl;
			}
			else if (TString("TBranch").CompareTo(branchesList->At(j)->ClassName()) == 0) {		// This is a simple branch containing pod type
				TClass *cptr = new TClass();
				EDataType bdt;
				static_cast<TBranchElement*>(branchesList->At(j))->GetExpectedType(cptr, bdt);	//Get datatype of the branch
				EDataType edt = static_cast<EDataType>(TDataType(branchClass).GetType()); // Get ROOT type of the expected datatype

				if (bdt!=edt) {	// Compare data type with expected data type
					std::cout << normal() << "[ERROR] "
							<< "Input file inconsistent. Bad primitive type (Found: "
							<< TDataType::GetTypeName(bdt)
							<< ", expected: " << branchClass << ") for "
							<< TDataType::GetTypeName(edt) << std::endl;	// Transform the name of the expected datatype into ROOT name
					throw LogicException();
				}
				std::cout << debug() << "Found " << branchName << " of primitive type "
						<< TDataType::GetTypeName(bdt) << std::endl;
			}
			//Activate branch
			std::cout << debug() << "Enabling branch " << branchName
					<< " of tree " << tree->GetName() << std::endl;
			fIOTimeCount.Start();
			tree->GetBranch(branchName)->SetStatus(1);
			tree->SetBranchAddress(branchName, evt);
			fIOTimeCount.Stop();
			return true;
		}

	}
	if (tree->GetTreeNumber() > 1 && tree->GetEntries() > 0 && (tree->GetNtrees()-tree->GetTreeNumber() < 100)) {
		//maybe last file is bad: go to previous file and retry.
	    //But only do this if there is still a tree before, if there are entries in the
	    //tree, and check a maximum of 100 files (covers any reasonable use case).
		fIOTimeCount.Start();
		Long64_t entryInPreviousFile = tree->GetChainEntryNumber(-1);
		tree->LoadTree(entryInPreviousFile);
		fIOTimeCount.Stop();
		if (entryInPreviousFile!=-1 && FindAndBranchTree(tree, branchName, branchClass, evt))
			return true;
	}
	if (!fAllowNonExisting && !IsKnownBranch(branchName) && (branchName.CompareTo("SAV")!=0)) {
		std::cout << normal() << manip::red << manip::bold << "[ERROR] Unable to find branch " << branchName
				<< " in TTree " << tree->GetName() << std::endl;
		if(branchName.CompareTo("Generated")==0)
			std::cout << normal() << "Are you trying to use a mixed Data-MC list?" << std::endl;
		std::cout << normal() << "To ignore this error and continue the "
				<< "processing, run with --ignore" << manip::reset << std::endl;
		throw LogicException();
	} else {
	    if(TString(tree->GetName()).EqualTo("Reco") && !TryBranchSlim(tree, branchName, branchClass)){
	        std::cout << normal() << manip::brown << manip::bold << "[WARNING] Unable to find branch "
	                << branchName << " in TTree " << tree->GetName()
	                << ". Retrieved corresponding event will always be empty"
	                << manip::reset << std::endl;
	    }
	}
	return false;
}

Long64_t InputTree::FillMCTruth() {
	/// \MemberDescr
	/// \return Number of events in the Event Tree
	///
	/// Branch the MC trees. Name is different if the input file comes from the MC or Reconstruction.
	/// \EndMemberDescr

	Long64_t eventNb = -1;
	if (!fWithMC)
		return eventNb;

	fIOTimeCount.Start();
	std::cout << extended() << "Retrieving number of entries in MCTruth tree"
			<< std::endl;
	if (fFastStart)
		eventNb = fMCTruthTree->GetEntriesFast();
	else
		eventNb = fMCTruthTree->GetEntries();
	fIOTimeCount.Stop();
	if (eventNb == 0) {
		fWithMC = false;
		return -1;
	}

	return eventNb;
}

Long64_t InputTree::FillEventHeader() {
	/// \MemberDescr
	/// \return Number of events in the EventHeader Tree
	///
	/// Branch the EventHeader trees.
	/// \EndMemberDescr

	Long64_t eventNb = -1;
	if (!fWithEventHeader)
		return eventNb;

	fIOTimeCount.Start();
	std::cout << extended() << "Retrieving number of entries in EventHeader tree" << std::endl;

	TChain *tree = nullptr;
	if(fEventHeaderRecoTree)
		tree = fEventHeaderRecoTree;
	else
		tree = fEventHeaderSpecialTree;

	if (fFastStart)
		eventNb = tree->GetEntriesFast();
	else
		eventNb = tree->GetEntries();
	fIOTimeCount.Stop();
	if (eventNb == 0) {
		fWithEventHeader = false;
		return -1;
	}
	return eventNb;
}

void InputTree::SetIgnoreNonExisting(bool bFlag) {
	/// \MemberDescr
	/// \param bFlag : if false, exit if one or several TTree is missing in the input file
	///
	/// Determine if the framework is allowed to run when one or several TTrees
	/// are missing in the input file.
	/// \EndMemberDescr

	if (bFlag)
		std::cout << extended() << "Ignoring non existing TTree" << std::endl;
	fAllowNonExisting = bFlag;
}

bool InputTree::OpenInput(bool specialOnly) {
	/// \MemberDescr
	/// \param inFileName : Path to the input file
	/// \param nFiles : Number of files to open
	/// \return True if the input files are checked and valid
	///
	/// Open and register the input files.
	/// \EndMemberDescr

	if (!InputHandler::OpenInput())
		return false;

	treeIterator it;
	bool inputChecked = false;
	int success = 0;

	int offset = fCurrentFileNumber>=0 ? fCurrentFileNumber+1 : 0;
	for (auto fileName=fInputfiles.begin()+offset; fileName!=fInputfiles.end(); fileName++) {
		if (!inputChecked && checkInputFile(*fileName, specialOnly))
			inputChecked = true;
		fIOTimeCount.Start();
		for (it = fTree.begin(); it != fTree.end(); it++)
			success += it->second->GetChain()->AddFile(*fileName);
		fIOTimeCount.Stop();

		if (success == 0)
			FileSkipped(offset, *fileName);
		offset++;
	}
	return inputChecked;
}

bool InputTree::checkInputFile(TString fileName, bool specialOnly) {
	/// \MemberDescr
	/// \param fileName : Name of the file to open
	/// \return True if the file can be opened
	///
	/// Open the input file to check if MC are present and if yes, what's the name of the TTree
	/// \EndMemberDescr

	TFile *fd(nullptr);
	if (fGraphicalMutex->Lock() == 0) {
		fIOTimeCount.Start();
		fd = TFile::Open(fileName.Data(), "R");
		fIOTimeCount.Stop();
		fGraphicalMutex->UnLock();
	}

	if (!fd)
		return kFALSE;

	fIOTimeCount.Start();
	TList* keys = fd->GetListOfKeys();
	fIOTimeCount.Stop();

	fWithMC = false;
	fWithEventHeader = false;
	for (int i = 0; i < keys->GetEntries(); i++) {
		TKey* k = (TKey*) keys->At(i);
		if (TString(k->GetClassName()).CompareTo("TTree") != 0)
			continue;
		TTree* tree = static_cast<TTree*>(fd->Get(k->GetName()));

		if (tree->FindBranch("Generated")) {
			fWithMC = true;
			Event *ne = new Event;
			if(!RequestTree(tree->GetName(), "Generated", "Event", ne))
				delete ne;
			if (!fMCTruthTree)
				fMCTruthTree = fTree.find(tree->GetName())->second->GetChain();
			fReferenceTree = fMCTruthTree;
		}

		if (tree->FindBranch("EventHeader")) {
			EventHeader *rh = new EventHeader;
			if (!RequestTree(tree->GetName(), "EventHeader", "EventHeader", rh))
				delete rh;
			if (TString(tree->GetName()).CompareTo("SpecialTrigger") != 0 && !specialOnly){
				fWithEventHeader = true;
				if (!fEventHeaderRecoTree)
					fEventHeaderRecoTree = fTree.find(tree->GetName())->second->GetChain();
				fReferenceTree = fEventHeaderRecoTree;
			}
			else if (TString(tree->GetName()).CompareTo("SpecialTrigger") == 0){
				fEventHeaderSpecialTree = fTree.find(tree->GetName())->second->GetChain();
				fWithEventHeader = true;
				if (!fReferenceTree)
					fReferenceTree = fEventHeaderSpecialTree;
			}
		}
	}
	fd->Close();

	if (fWithMC == false) {
		std::cout << extended() << "No MC data found" << std::endl;
		if(fEventHeaderSpecialTree!=nullptr){
			L0TPSpecialTrigger *l0sp = new L0TPSpecialTrigger;
			if (!RequestTree("SpecialTrigger", "L0TP", "L0TPSpecialTrigger", l0sp))
				delete l0sp;
		}
	}
	if (fWithEventHeader == false) {
		std::cout << normal() << "No Raw Header found" << std::endl;
	}
	return fReferenceTree != nullptr;
}

void InputTree::PrintInitSummary() const {
	/// \MemberDescr
	///
	/// Print the summary after initialization
	/// \EndMemberDescr

	InputHandler::PrintInitSummary();

	StringBalancedTable treeTable("List of requested TTrees");

	std::stringstream ss;
	for (auto itEvTree : fEvent) {
		for (auto itEv : itEvTree.second){
			ss << itEv.second->fTreeName << "->" << itEvTree.first;
			treeTable << ss.str();
			ss.clear();
			ss.str(std::string());
		}
	}
	for (auto itObjTree : fObject) {
		for (auto itObj : itObjTree.second){
			ss << itObjTree.first << "->" << itObj.second->fBranchName;
			treeTable << ss.str();
			ss.clear();
			ss.str(std::string());
		}
	}

	treeTable.Print("\t");
}

bool InputTree::CheckNewFileOpened() {
	/// \MemberDescr
	/// \return True if a new file has been opened
	///
	/// Method called by TChain when opening a new file.\n
	/// It will signal a new burst to the analyzers
	/// \EndMemberDescr

	int openedFileNumber;
	TFile *currFile;

	if (fReferenceTree) {
		openedFileNumber = fReferenceTree->GetTreeNumber();
		currFile = fReferenceTree->GetFile();
	} else
		return false;

	if (openedFileNumber > fCurrentFileNumber) {
		InputHandler::NewFileOpened(openedFileNumber, currFile);
		return true;
	}
	return false;
}

Long64_t InputTree::GetNEvents() {
	/// \MemberDescr
	/// \return Total number of events. If used with --fast-start, returns kBigNumber
	/// as long as the last file is not reached.
	/// \EndMemberDescr
	Long64_t entries;
	if (fReferenceTree) {
		fIOTimeCount.Start();
		if (fFastStart)
			entries = fReferenceTree->GetEntriesFast();
		else
			entries = fReferenceTree->GetEntries();
		fIOTimeCount.Stop();
		return entries;
	} else
		return 0;
}

Long64_t InputTree::GetNSpecialEvents() {
	/// \MemberDescr
	/// \return Total number of special events. If used with --fast-start, returns kBigNumber
	/// as long as the last file is not reached.
	/// \EndMemberDescr
	Long64_t entries;
	if (fEventHeaderSpecialTree) {
		fIOTimeCount.Start();
		if (fFastStart)
			entries = fEventHeaderSpecialTree->GetEntriesFast();
		else
			entries = fEventHeaderSpecialTree->GetEntries();
		fIOTimeCount.Stop();
		return entries;
	} else
		return 0;
}

TString InputTree::DetermineMainTree(eventMap::type events) {
	/// \MemberDescr
	/// \param detName : DetectorName (branch) for which the main tree should be identified
	/// \return Name of the main tree requested and containing the branch. The order is
	/// Reco, MC, Digis, Other
	/// \EndMemberDescr

	if (++events.begin() == events.end()) {
		//Only one branch, main is this one
		return events.begin()->first;
	}

	TString mainTree;
	//Order is Reco, MC, Digis, Anything else
	for (auto it : events) {
		if (it.first.CompareTo("Reco") == 0)
			return "Reco"; //Shortcut, cannot be higher than Reco
		else if (it.first.CompareTo("MC") == 0)
			mainTree = "MC";
		else if (it.first.CompareTo("Digis") == 0
				&& mainTree.CompareTo("MC") != 0)
			mainTree = "Digis";
		else
			mainTree = it.first;
	}

	return mainTree;
}

void InputTree::BranchFilterBranch(TChain* inTree) {
	/// \MemberDescr
	/// \param inTree : TTree from which the filterword should be retrieved
	///
	/// Append the FilterWord branch in the TTree.
	/// \EndMemberDescr

	if (inTree->FindBranch("FilterWord")) {
		FindAndBranchTree(inTree, "FilterWord", "Long64_t", &fFilterWord);
		fUseFilterBranch = true;
	}
}

bool InputTree::GetAcceptFromBit(unsigned int bitNumber) const {
	/// \MemberDescr
	/// \param bitNumber : Position of the checked bit (starts at 0)
	/// \return Accept decision from the analyzer corresponding to the given bit
	/// \EndMemberDescr

	return fFilterWord & (1 << bitNumber);
}

bool InputTree::GetAcceptFromAnalyzer(TString analyzerID) const {
	/// \MemberDescr
	/// \param analyzerID : Identification string of the analyzer (analyzer name)
	/// \return Accept decision from the analyzer
	/// \EndMemberDescr

	int bitNumber = GetBitFromAnalyzer(analyzerID);
	if (bitNumber >= 0)
		return GetAcceptFromBit(bitNumber);
	return false;
}

Long64_t InputTree::GetFilterWord() const {
	/// \MemberDescr
	/// \return Filter word retrieved from input file
	/// \EndMemberDescr

	return fFilterWord;
}

Long64_t* InputTree::GetFilterWordRef() {
	/// \MemberDescr
	/// \return Reference to filter word retrieved from input file
	/// \EndMemberDescr

	return &fFilterWord;
}

bool InputTree::IsKnownBranch(TString branchName) {
	/// \MemberDescr
	/// \param branchName : branch name to check against the standard set
	/// \return True of the provided branch name is part of the known standard set
	///
	/// The standard set of known branches is the following:
        ///  - Beam
	///  - Cedar
	///  - CHANTI
	///  - GigaTracker
	///  - LAV
	///  - LKr
	///  - MUV0
	///  - MUV1
	///  - MUV2
	///  - MUV3
	///  - RICH
	///  - Spectrometer
	///  - SAC
	///  - HAC
	///  - CHOD
	///  - NewCHOD
	///  - IRC
	///  - L0TP
	///  - L1TP
	///  - L2EB
	/// \EndMemberDescr

        if (branchName.CompareTo("Beam") == 0)
	        return true;
	if (branchName.CompareTo("Cedar") == 0)
		return true;
	if (branchName.CompareTo("CHANTI") == 0)
		return true;
	if (branchName.CompareTo("GigaTracker") == 0)
		return true;
	if (branchName.CompareTo("LAV") == 0)
		return true;
	if (branchName.CompareTo("LKr") == 0)
		return true;
	if (branchName.CompareTo("MUV0") == 0)
		return true;
	if (branchName.CompareTo("MUV1") == 0)
		return true;
	if (branchName.CompareTo("MUV2") == 0)
		return true;
	if (branchName.CompareTo("MUV3") == 0)
		return true;
	if (branchName.CompareTo("RICH") == 0)
		return true;
	if (branchName.CompareTo("Spectrometer") == 0)
		return true;
	if (branchName.CompareTo("SAC") == 0)
		return true;
	if (branchName.CompareTo("HAC") == 0)
		return true;
	if (branchName.CompareTo("CHOD") == 0)
		return true;
	if (branchName.CompareTo("NewCHOD") == 0)
		return true;
	if (branchName.CompareTo("IRC") == 0)
		return true;
	if (branchName.CompareTo("L0TP") == 0)
		return true;
	if (branchName.CompareTo("L1TP") == 0)
		return true;
	if (branchName.CompareTo("L2EB") == 0)
		return true;
	if (branchName.CompareTo("HLT") == 0)
		return true;

	return false;
}

bool InputTree::IsKnownTree(TString treeName) {
	/// \MemberDescr
	/// \param treeName : tree name to check against the standard set
	/// \return True of the provided tree name is part of the known standard set
	///
	/// The standard set of known branches is the following:
	///  - Reco
	///  - Digi
	///  - MC
	/// \EndMemberDescr

	if (treeName.CompareTo("Reco") == 0)
		return true;
	if (treeName.CompareTo("Digi") == 0)
		return true;
	if (treeName.CompareTo("MC") == 0)
		return true;
	if (treeName.CompareTo("SpecialTrigger") == 0)
		return true;

	return false;
}

InputTree::ChainObject::ChainObject(TString chainName) :
		fChain(new TChain(chainName)), fEnabled(true) {
	/// \MemberDescr
	/// \param chainName : Name of the TChain to open
	///
	/// Constructor. Creates a new chain and enable it by defautl
	/// \EndMemberDescr
}

InputTree::ChainObject::~ChainObject() {
	/// \MemberDescr
	/// Destructor. Deletes the TChain.
	/// \EndMemberDescr

	delete fChain;
}

Long64_t InputTree::SkipBadBurst(Long64_t currentEntry, bool special) {
	/// \MemberDescr
	/// \param currentEntry : Current event entry processed
	/// \return First expected event entry not belonging to a bad burst, or -1 if not found
	///
	/// Skip the events belonging to a bad burst
	/// \EndMemberDescr

	//If no rawheader available, no way to check burst ID
	if (!fWithEventHeader)
		return currentEntry;

	TString tree;
	TChain * eventHeaderTree;
	Long64_t totEvents;
	if(special){
		tree = "SpecialTrigger";
		eventHeaderTree = fEventHeaderSpecialTree;
		totEvents = GetNSpecialEvents();
	}
	else{
		tree = "Reco";
		eventHeaderTree = fEventHeaderRecoTree;
		totEvents = GetNEvents();
	}
	EventHeader* rawHeader = GetEventHeaderEvent(tree);
    if(!rawHeader) return currentEntry;

    //This burst was good. If not yet reached the next burst, we're still ok
    if( (currentEntry<fNextBurstIndex && !special) || (currentEntry<fNextSpecialBurstIndex && special))
      return currentEntry;

    //Loading EventHeader for current event
    fIOTimeCount.Start();
	Long64_t localEntry = eventHeaderTree->LoadTree(currentEntry); //Current entry in the file
	Long64_t maxEntries = eventHeaderTree->GetTree()->GetEntries(); //Max entries in the file
	eventHeaderTree->FindBranch("EventHeader")->GetEntry(localEntry);
	int fileIndex = eventHeaderTree->GetTreeNumber();
	int previousStreamIndex = fStreamTree->GetTreeNumber();
	LoadMCStream(fileIndex);
	UInt_t currentBurstID = rawHeader->GetBurstID();
	UInt_t currentRunID = rawHeader->GetRunID();
	TString currentRecoRevision = GetRecoRevision();
    if(currentRunID!=(UInt_t)NA62ConditionsService::GetInstance()->GetCurrentRunID()){ //update ConditionsService info
      NA62ConditionsService::GetInstance()->SetCurrentRunID(currentRunID);
      NA62ConditionsService::GetInstance()->SetCurrentBurstID(currentBurstID);
      TString CDBSubDir = fWithMC ? "MC" : "Data";
      NA62ConditionsService::GetInstance()->SetCDBTag(CDBSubDir+"/"+currentRecoRevision);
    }
	LoadMCStream(previousStreamIndex);
	fIOTimeCount.Stop();

	//Find first entry of next burst. Usefull even if this burst is good.
	//Try to locate the next burst. Go to the next file and see if the next
	//burst is in between the current entry and the first entry of the next file
	//Else move to following file
	Long64_t firstNextBurstEntry = -1;
	Long64_t lastCheckEntry = currentEntry + maxEntries - localEntry; //First entry of the next file (in chain reference)
	Long64_t checkedEntry = currentEntry;
	if (lastCheckEntry==totEvents) //Last file, don't go past it
		lastCheckEntry--;
	while (firstNextBurstEntry == -1 && checkedEntry < totEvents && checkedEntry!=lastCheckEntry) { //Loop as long as we don't find the next burst. Or stop if we reached the end of the chain
		fBadBurstSkipped.insert(fileIndex);
		std::cout << trace() << "Searching burst != " << currentBurstID
				<< " between " << checkedEntry << " and " << lastCheckEntry
				<< std::endl;
		firstNextBurstEntry = BinaryBurstSearch(checkedEntry, lastCheckEntry,
				currentBurstID, special); //Binary search between currentEntry and lastCheckEntry
		checkedEntry = lastCheckEntry; //Current entry is now the first entry of the (previously) next file (in chain reference)
		fIOTimeCount.Start();
		eventHeaderTree->LoadTree(lastCheckEntry); // Load it
		lastCheckEntry = std::min(checkedEntry + eventHeaderTree->GetTree()->GetEntries(), totEvents-1); // First entry of the next file, or last entry of the TChain (prevents going beyond the last event)
		fileIndex = eventHeaderTree->GetTreeNumber();
		fIOTimeCount.Stop();
	}

	if(special)
		fNextSpecialBurstIndex = firstNextBurstEntry==-1 ? totEvents : firstNextBurstEntry;
	else
		fNextBurstIndex = firstNextBurstEntry==-1 ? totEvents : firstNextBurstEntry;

	BadBursts *badBurst = BadBursts::GetInstance();
	// If current burst is good, return
    bool bbIsGood = badBurst->IsGood(currentBurstID) ^ NA62Analysis::Configuration::ConfigSettings::CLI::fInvertBadBurst;
	if (bbIsGood)
		return currentEntry;

	if (!special)
	  std::cout << normal() << "Skipping bad burst: run " <<
	    currentRunID <<" burst "<<currentBurstID << std::endl;

	if (firstNextBurstEntry == -1){
		//Reached the end of chain without finding anymore good bursts.
		std::cout << debug() << "Cannot find good burst" << std::endl;
		return -1;
	}
	else
		std::cout << debug() << "Found it at " << firstNextBurstEntry << std::endl;

	firstNextBurstEntry = SkipBadBurst(firstNextBurstEntry, special);
	return firstNextBurstEntry;
}

NA62Analysis::NA62Map<TString, TChain*>::type InputTree::GetEnabledTrees() {
	/// \MemberDescr
	/// \return A map containing the (names,pointers) to the enabled TTrees
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString, TChain*>::type enTrees;
	for (auto it : fTree) {
		if(!it.second->IsEnabled())
			continue;
		enTrees.insert(std::pair<const TString, TChain*>(it.first, it.second->GetChain()));
	}

	return enTrees;
}

void InputTree::UpdateFilterWord(Long64_t filterWord) {
	/// \MemberDescr
	/// \param filterWord : new filter word
	///
	/// Update the filterWord with the new one. Touches only the part of the
	/// trigger word related to the Analyzer currently running (does not change
	/// the part of the word imported from the input files).
	/// \EndMemberDescr

	fFilterWord = fFilterWord & (0 << GetAnalyzerList().size());
	fFilterWord = fFilterWord | (filterWord << GetAnalyzerList().size());
}

TChain* InputTree::GetReferenceTree() {
	/// \MemberDescr
	/// \return Pointer to the reference tree
	/// \EndMemberDescr
	return fReferenceTree;
}

Long64_t InputTree::BinaryBurstSearch(Long64_t firstEntry, Long64_t lastEntry, UInt_t currentBurst, bool special) {
	/// \MemberDescr
	/// \param firstEntry : first entry of the search range
	/// \param lastEntry : last entry of the search range
	/// \param currentBurst : current burst. We are searching for the first entry not belonging to it
	/// \return first entry whose burst id is different of the current burst id
	///
	/// Binary search algorithm that search for the first entry not equal to currentBurst.
	/// \EndMemberDescr

	TString tree;
	TChain * eventHeaderTree;
	if(special){
		tree = "SpecialTrigger";
		eventHeaderTree = fEventHeaderSpecialTree;
	}
	else{
		tree = "Reco";
		eventHeaderTree = fEventHeaderRecoTree;
	}
	EventHeader* rawHeader = GetEventHeaderEvent(tree);

	Long64_t localEntry;
	Long64_t midEntry = lastEntry;

	while (firstEntry < lastEntry) { //Continue as long as the first entry is not found and we have not exhausted the whole range
		fIOTimeCount.Start();
		localEntry = eventHeaderTree->LoadTree(midEntry);
		eventHeaderTree->FindBranch("EventHeader")->GetEntry(localEntry);
		fIOTimeCount.Stop();
		std::cout << trace() << "Burst " << rawHeader->GetBurstID() << " found at " << midEntry << std::endl;
		if (rawHeader->GetBurstID() == currentBurst)
			firstEntry = midEntry + 1; //We are still in the old burst, not present in first part of the interval
		else {
			//Found a new burst, but is it the first entry of this burst?
			fIOTimeCount.Start();
			localEntry = eventHeaderTree->LoadTree(midEntry - 1);
			eventHeaderTree->FindBranch("EventHeader")->GetEntry(localEntry);
			fIOTimeCount.Stop();
			if (rawHeader->GetBurstID() == currentBurst)
				return midEntry; //Yes it was the first entry of the next burst

			//Else we are already in the middle of the new burst. First entry must be in the first part of the interval
			lastEntry = midEntry;
		}
		midEntry = (firstEntry + lastEntry) / 2;

	}

	return -1;
}

void InputTree::FileSkipped(int fileIndex, TString fileName) {
	/// \MemberDescr
	/// \param fileName: Name of the skipped file
	///
	/// File has been skipped for whatever reason. Notify it in the .skipped file.
	/// Do not do it for files skipped because of bad burst
	/// \EndMemberDescr

	if(fBadBurstSkipped.count(fileIndex)==0)
		InputHandler::FileSkipped(fileIndex, fileName);
}

int InputTree::GetRunID() {
	EventHeader *evt = nullptr;
	if(fEventHeaderRecoTree)
		evt = GetEventHeaderEvent();
	else if(fEventHeaderSpecialTree)
		evt = GetEventHeaderEvent("SpecialTrigger");
	if (evt)
		return evt->GetRunID();
	else
		return InputHandler::GetRunID();
}

int InputTree::GetBurstID() {
	EventHeader *evt = nullptr;
	if(fEventHeaderRecoTree)
		evt = GetEventHeaderEvent();
	else if(fEventHeaderSpecialTree)
		evt = GetEventHeaderEvent("SpecialTrigger");
	if (evt)
		return evt->GetBurstID();
	else
		return InputHandler::GetBurstID();
}

int InputTree::GetBurstTime() {
	EventHeader *evt = nullptr;
	if(fEventHeaderRecoTree)
		evt = GetEventHeaderEvent();
	else if(fEventHeaderSpecialTree)
		evt = GetEventHeaderEvent("SpecialTrigger");
	if (evt)
		return evt->GetBurstTime();
	else
		return InputHandler::GetBurstTime();
}

InputTree::eventMap::type InputTree::GetEventsForTree(TString treeName) {
	eventMap::type tempMap;
	for(auto itEvtDet : fEvent){
		auto itEvt = itEvtDet.second.find(treeName);
		if(itEvt != itEvtDet.second.end())
			tempMap.insert(std::make_pair(itEvtDet.first, itEvt->second));
	}

	return tempMap;
}

InputTree::objectMap::type InputTree::GetObjectsForTree(TString treeName) {
	objectTreeIterator it = fObject.find(treeName);
	if(it != fObject.end())
		return it->second;
	else
		return objectMap::type();
}

std::set<TString> InputTree::GetRequestedDetectorNames() {
	std::set<TString> detNames;
	for(auto itEvent : fEvent)
		detNames.insert(itEvent.first);
	return detNames;
}

} /* namespace Core */
} /* namespace NA62Analysis */

