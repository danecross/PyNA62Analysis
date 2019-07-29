#ifndef BASEANALYSIS_HH
#define BASEANALYSIS_HH 1

#include <sstream>

#include "Analyzer.hh"
#include "EventChecker.hh"
#include "CounterHandler.hh"
#include "OutputHandler.hh"
#include "containers.hh"
#include "Verbose.hh"
#include <TSemaphore.h>
#include <TThread.h>

#include "InputTree.hh"
#include "TimeCounter.hh"

namespace NA62Analysis {
namespace Core {

/// \class BaseAnalysis
/// \Brief
/// Is taking care of the initialization and closing procedure. Process the analyzers in the required order.
/// \EndBrief
///
/// \Detailed
/// Main class of the framework. It takes care of initializing everything properly, processing the events,
/// keeping track of every object created by the framework (including user analyzers) and passing them to
/// the user when requested, communication between different parts of the framework, writing the output files.
/// \EndDetailed

class OMMainWindow;
class IOPrimitive;

class BaseAnalysis: public Verbose {
public:
	BaseAnalysis();
	~BaseAnalysis();
	void Terminate();

	void AddAnalyzer(Analyzer * const an);
	void StartAnalyzers();
	void StartContinuous(TString inFileList);
	void AddInputFiles(TString inFileName, Int_t NFiles);
	void CreateOStream(TString id);
	void AddAnalyzerToOStream(TString id, TString anName);
	void Init(TString outFileName, TString params, TString configFile, TString refFile, bool allowNonExisting);
	bool Process(Long64_t beginEvent, Long64_t maxEvent, int maxBurst);
	void InitPrimitives();
	bool ProcessSpecialTriggersForCurrentBurst();
	void WriteOutput();

	//Output methods
	void RegisterOutput(TString name, const void* const address);
	void SetOutputState(TString name, Analyzer::OutputState state);
	const void *GetOutput(TString name, Analyzer::OutputState &state) const;

	void PrintInitSummary() const;
	void CheckNewFileOpened();
	void CheckBurstAndRunChange(int maxBurst);
	void DoStartOfRunActions();
	void DoEndOfRunActions();
	void DoStartOfBurstActions();
	void DoEndOfBurstActions();

	InputHandler * GetIOHandler();
	OutputHandler * GetOHandler(TString oHandler="default");
	std::vector<OutputHandler*> GetOHandlersForAnalyzer(TString analyzerName);
	InputTree * GetIOTree();
	InputHisto * GetIOHisto();
	IOPrimitive * GetIOPrimitive();

	CounterHandler* GetCounterHandler();

	Long64_t GetNEvents();
	Long64_t RecomputeNEvents();
	TChain* GetTree(TString name);

    TFile * GetCurrentFile() {
	/// \MemberDescr
	/// \return true if IO Handler is compatible with Histo type
	/// \EndMemberDescr
	  return fIHandler->GetCurrentFile();
	}

	bool IsHistoType() {
		/// \MemberDescr
		/// \return true if IO Handler is compatible with Histo type
		/// \EndMemberDescr
		return fIHandler->GetIOType() == IOHandlerType::kHISTO
				|| fIHandler->GetIOType() == IOHandlerType::kTREE;
	};
	bool IsTreeType() {
		/// \MemberDescr
		/// \return true if IO Handler is compatible with Tree type
		/// \EndMemberDescr
		return fIHandler->GetIOType() == IOHandlerType::kTREE;
	};

	void SetGraphicMode(bool bVal) {
		/// \MemberDescr
		/// \param bVal : Graphic mode On/Off flag
		/// \EndMemberDescr
		fGraphicMode = bVal;
	};
	void SetReadType(IOHandlerType type);
	void SetContinuousReading(bool flagContinuousReading);
	void SetDownscaling(bool flagDownscaling);
	void SetFastStart(bool bVal) {
		/// \MemberDescr
		/// \param bVal : true/false
		///
		/// Enabled/Disable the FastStart option
		/// \EndMemberDescr
		fIHandler->SetFastStart(bVal);
	}
	void SetSkipIsFatal(bool bVal) {
		/// \MemberDescr
		/// \param bVal : true/false
		///
		/// Enabled/Disable the skip is fatal option
		/// \EndMemberDescr
		fIHandler->SetSkipFatal(bVal);
	}
	void SetFiltering(bool bFilter) {
		/// \MemberDescr
		/// \param bFilter : true/false
		///
		/// Enabled/Disable the filtering option
		/// \EndMemberDescr
		fFiltering = bFilter;
	}
	bool IsFiltering() { return fFiltering; }
	void SetPrimitiveFile(TString fileName);
	void SetForcePreAnalyzers(bool forcePreAnalyzers) {
		fForcePreAnalyzers = forcePreAnalyzers;
	}
	void SetSpecialOnly(bool b){
		fSpecialOnly = b;
	}

	void ReconfigureAnalyzer(TString analyzerName, TString parameterName,
			TString parameter);

	static void FillMemoryUsage(TGraph* virtualMemoryGraph, TGraph* residentMemoryGraph, int x);

	void UpdateOMWindowForAnalyzer(TString anName);

	bool AnalyzerRanOnEvent(TString thisAnName, TString anName);

	void SetIsPython(bool);

private:
	BaseAnalysis(const BaseAnalysis&); ///< Prevents copy construction
	BaseAnalysis& operator=(const BaseAnalysis&); ///< Prevents copy assignment
	void PreProcess();
	void printCurrentEvent(Long64_t iEvent, Long64_t totalEvents,
			int defaultPrecision, std::string &displayType,
			TimeCounter_us startTime);
	void printCurrentSpecialEvent(Long64_t iEvent);
	static void ContinuousLoop(void* args);
	void CreateOMWindow();
	void StartMemoryMonitor();
	void StopMemoryMonitor();
	static void memoryMonitorLoop(void* args);

	/// \struct ThreadArg
	/// \Brief
	/// Arguments to be passed to the thread function.
	/// \EndBrief
	struct ThreadArgs_t {
		BaseAnalysis* ban; ///< Pointer to the parent BaseAnalysis instance
		TString inFileList; ///< Path to the input list file
	};

	/// \struct MemoryGraphs_t
	/// \Brief
	/// Arguments to be passed to the thread function.
	/// \EndBrief
        struct MemoryGraphs_t{
          TGraph* gVirtual;
          TGraph* gResident;
        };
protected:
	/// \struct OutputStream
	/// \Brief
	/// Structure containing the OutputHandler and the list of Analyzer used
	/// for filtering for the OutputStream
	/// \EndBrief
	struct OutputStream{
		OutputStream(){ fOutputHandler = new OutputHandler("OHandler"); }

		explicit OutputStream(TString id){ fOutputHandler = new OutputHandler(Form("OHandler_%s", id.Data())); }
		bool IsAnalyzerInList(TString anName){ return fAnList.count(anName)>0; }
		bool IsEmpty(){ return fAnList.size()==0; }
		OutputHandler *fOutputHandler; ///< Pointer to the OutputHandler for this stream
		std::set<TString> fAnList; ///< List of analyzer on which this stream depends
	};

	Long64_t fNEvents; ///< Number of events available in the TChains
	Long64_t fFirstGoodEvent; ///< Index of the first good event (not belonging to a bad burst)
	Long64_t fLastSpecialTriggerEvent;
	int fEventsDownscaling; ///< Downscaling. Read 1 out of x events
	int fCurrentBurstNumber; ///< Current burst number
	int fCurrentRunNumber; ///< Current run number
	TString fCurrentMCRevision; ///< Current revision number
	TString fCurrentRecoRevision; ///< Current revision number
	int fBurstProcessed; ///< Number of burst already processed
	bool fGraphicMode; ///< Indicating if we only want output file or display
	bool fInitialized; ///< Indicate if BaseAnalysis has been initialized
	bool fContinuousReading; ///< Continuous reading enabled?
	bool fSignalStop; ///< Stop signal for the Thread
	bool fFiltering; ///< Are we using filtering feature
	bool fForcePreAnalyzers; ///< Do we force the pre-analyzers to run?
	bool fSpecialOnly;
	int fFirstAnalyzer; ///< First analyzer to run. 0 if all will run, including pre-analyzers, else will be the number of pre-analyzers.

	bool fIsPythonFile = false; // < indicates if this is being run by a python script or C++ script. 

	TString fDefaultOStream; ///< ID of the default output stream (first stream created)

	std::vector<Analyzer*> fAnalyzerList; ///< Container for the analyzers

	NA62Analysis::NA62Map<TString, const void* const >::type fOutput; ///< Container for outputs of all analyzers
	NA62Analysis::NA62Map<TString, Analyzer::OutputState>::type fOutputStates; ///< Container for output states for all analyzers

        EventChecker* fEventChecker; ///< Global instance of EventChecker
	CounterHandler fCounterHandler; ///< Handler for EventFraction and Counters
	InputHandler* fIHandler; ///< Handler for all IO objects
	NA62Analysis::NA62Map<TString, OutputStream>::type fOStream; ///< Container for OutputStreams
	IOPrimitive* fIOPrimitive; ///< Pointer to IOPrimitive instance

	TimeCounter_us fInitTime; ///< Time counter for the initialisation step (from constructor to end of Init())

	TMutex fGraphicalMutex; ///< Mutex to prevent TApplication and BaseAnalysis to perform graphical operation at the same time
	TThread *fRunThread; ///< Thread for Process during Online Monitor
	OMMainWindow *fOMMainWindow; ///< Online monitor GUI

	TGraph* fVirtualMemoryVsEvent; ///< Graph of the virtual memory usage vs. event number
	TGraph* fVirtualMemoryVsTime; ///< Graph of the virtual memory usage vs. time
	TGraph* fResidentMemoryVsEvent; ///< Graph of the resident memory usage vs. event number
	TGraph* fResidentMemoryVsTime; ///< Graph of the resident memory usage vs. time
	TThread *fMemoryMonitorThread; ///< Pointer to the memory monitor thread
};

} /* namespace Core */
} /* namespace NA62Analysis */

#endif
