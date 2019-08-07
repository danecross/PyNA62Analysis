/*
 * IOTree.hh
 *
 *  Created on: Mar 9, 2015
 *      Author: ncl
 */

#ifndef IOTREE_HH_
#define IOTREE_HH_

#include <set>

#include "TDetectorVEvent.hh"
#include "TSlimRecoVEvent.hh"
#include "Event.hh"

#include "EventHeader.hh"
#include "InputHisto.hh"

class TChain;
class TTree;
class TBranch;
class Stream;

namespace NA62Analysis {
namespace Core {

/// \class InputTree
/// \Brief
/// Class containing and handling every Input TTree
/// \EndBrief
///
/// \Detailed
/// Inherits InputHandler and implements the Input methods for TTrees\n
/// TTree management (input).
/// \EndDetailed
class InputTree: public InputHisto {
public:
	InputTree();
	InputTree(const InputTree& c);
	virtual ~InputTree();

	//IO Files
	bool OpenInput(bool specialOnly);
	bool checkInputFile(TString fileName, bool specialOnly);
	bool CheckNewFileOpened();
	void FileSkipped(int fileIndex, TString fileName);

	//TTree
	void RequestTree(TString detectorName, TDetectorVEvent* const evt, TString outputStage = "");
	bool RequestTree(TString treeName, TString branchName, TString className, void* const obj);
	Long64_t BranchTrees(Long64_t eventNb);
	TChain* GetTree(TString name);
	void SetIgnoreNonExisting(bool bFlag);
	TChain* GetReferenceTree();
	std::set<TString> GetRequestedDetectorNames();

	//Events
	TDetectorVEvent *GetEvent(TString detName, TString outputName = "");
	void* GetObject(TString name, TString branchName = "", bool silent=false);
	Long64_t FillMCTruth();
	Long64_t FillEventHeader();
	virtual bool LoadEvent(Long64_t &iEvent);
	virtual bool Rewind();
	virtual bool ReloadLatest();

	Event* GetMCTruthEvent();
	bool GetWithMC() const;
	EventHeader* GetEventHeaderEvent(TString treeName = "Reco");
	bool GetWithEventHeader() const;
	Long64_t GetNEvents();
	Long64_t GetNSpecialEvents();
	virtual int GetRunID();
	virtual int GetBurstID();
	virtual int GetBurstTime();
	Long64_t GetNextBurstIndex() const {
		return fNextBurstIndex;
	}

	bool LoadSpecialEvent(Long64_t &iEvent);

	//Filter word
	Long64_t GetFilterWord() const;
	Long64_t* GetFilterWordRef();
	bool GetAcceptFromBit(unsigned int bitNumber) const;
	bool GetAcceptFromAnalyzer(TString analyzerID) const;
	NA62Analysis::NA62Map<TString, TChain*>::type GetEnabledTrees();
	void UpdateFilterWord(Long64_t filterWord);

	//Printing
	void PrintInitSummary() const;

private:
	/// \class ObjectTriplet
	/// \Brief
	/// Class containing an object branched to a custom TTree
	/// \EndBrief
	///
	/// \Detailed
	/// It contains everything needed to fully define the custom object:
	///	The class name, the tree name and the pointer to the object (stored as void*)
	/// \EndDetailed

	class ObjectTriplet {
	public:
		ObjectTriplet(TString c, TString branch, void* obj) :
				fClassName(c), fBranchName(branch), fObject(obj), fEnabled(true) {
			/// \MemberDescr
			///	\param c : Class name of the object
			///	\param branch : Name of the branch
			/// \param obj : Pointer to the object
			///	Constructor
			///	\EndMemberDescr
		};
		TString fClassName; ///< Class name of the object
		TString fBranchName; ///< Branch name
		void* fObject; ///< Pointer to the object
		bool fEnabled;
	};

	/// \class EventTriplet
	/// \Brief
	/// Class containing an event branched to a TTree
	/// \EndBrief
	///
	/// \Detailed
	/// It contains the tree from which the event is extracted and
	/// the pointer to the event itself.
	/// \EndDetailed
	class EventTriplet {
	public:
		EventTriplet(TString tree, TDetectorVEvent* obj) :
				fTreeName(tree), fEvent(obj), fEnabled(true) {
			/// \MemberDescr
			///	\param tree : Name of the TTree
			/// \param obj : Pointer to the event
			///	Constructor
			///	\EndMemberDescr
		}
		;
		~EventTriplet() {
			delete fEvent;
		}
		TString fTreeName; ///< Branch name
		TDetectorVEvent* fEvent; ///< Pointer to the event
		bool fEnabled;
	};

	/// \class ChainObject
	/// \Brief
	/// Class containing a pointer to a TChain and a flag indicating if the chain is enabled
	/// \EndBrief
	///
	/// \Detailed
	/// Class containing a pointer to a TChain and a flag indicating if the chain is enabled
	/// \EndDetailed
	class ChainObject {
	public:
		explicit ChainObject(TString chainName);
		~ChainObject();
		void Disable() {
			/// \MemberDescr
			///	Disable the chain
			///	\EndMemberDescr
			fEnabled = false;
		}
		bool IsEnabled() {
			/// \MemberDescr
			///	\return true if the chain is enabled
			///	\EndMemberDescr
			return fEnabled;
		}
		TChain *GetChain() {
			/// \MemberDescr
			///	\return The pointer to the TChain
			///	\EndMemberDescr
			return fChain;
		}
	private:
		TChain *fChain; ///< Pointer to the TChain
		bool fEnabled; ///< Flag indicating if the chain is enabled for reading
	};

	typedef NA62Analysis::NA62Map<TString, ChainObject*> treeMap;
	typedef NA62Analysis::NA62Map<TString, EventTriplet*> eventMap;
	typedef NA62Analysis::NA62Map<TString, eventMap::type> eventDetMap;
	typedef NA62Analysis::NA62Map<TString, ObjectTriplet*> objectMap;
	typedef NA62Analysis::NA62Map<TString, objectMap::type> objectTreeMap;

	typedef treeMap::type::iterator treeIterator; ///< typedef for iterators of map of TChain
	typedef eventMap::type::iterator eventIterator; ///< typedef for iterators of map of EventTriplet
	typedef eventDetMap::type::iterator eventDetIterator;
	typedef objectMap::type::iterator objectIterator; ///< typedef for iterators of map of ObjectTriplet
	typedef objectTreeMap::type::iterator objectTreeIterator;

	bool FindAndBranchTree(TChain* tree, TString branchName, TString branchClass, void* const evt);
	bool TryBranchSlim(TString branchName, TString branchClass);
	void BranchFilterBranch(TChain* inTree);
	static bool IsKnownBranch(TString branchName);
	static bool IsKnownTree(TString treeName);
	TString DetermineMainTree(eventMap::type events);
	Long64_t SkipBadBurst(Long64_t currentEntry, bool special=false);
	Long64_t BinaryBurstSearch(Long64_t firstEntry, Long64_t lastEntry, UInt_t currentBurst, bool special);


	eventMap::type GetEventsForTree(TString treeName);
	objectMap::type GetObjectsForTree(TString treeName);

	treeMap::type fTree; ///< Container for the trees (Name, pointer)
	eventDetMap::type fEvent; ///< Container for the events (Detector, EventTriplet)
	objectTreeMap::type fObject; ///< Container for the custom objects (Tree name, ObjectTriplet)
	std::set<int> fBadBurstSkipped; ///< Contains list of index of fully skipped files due to bad burst

	TChain *fMCTruthTree; ///< Pointer to (first) TTree containing MCTruth
	TChain *fEventHeaderRecoTree; ///< Pointer to Reco TTree containing EventHeader
	TChain *fEventHeaderSpecialTree; ///< Pointer to SpecialTrigger TTree containing EventHeader
	TChain *fReferenceTree; ///< Pointer to the reference TTree used to retrieve common information

	bool fWithMC; ///< Do we have MC in the file?
	bool fWithEventHeader; ///< Do we have EventHeader in the file?

	bool fAllowNonExisting; ///< Do we allow non existing trees
	Long64_t fFilterWord; ///< Filter word. To be branched with the input and output FilterWord branch
	bool fUseFilterBranch; ///< Flag to indicate whether the FilterWord branch is read or not

	Long64_t fNextBurstIndex;
	Long64_t fNextSpecialBurstIndex;
	std::vector<std::pair<TSlimRecoVEvent*, TRecoVEvent*>> fPersAutoTransform; ///< List of peristency objects that we have to automatically transform from Slim to Reco
};

} /* namespace Core */
} /* namespace NA62Analysis */

#endif /* IOTREE_HH_ */
