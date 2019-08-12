#include "BaseAnalysis.hh"

#include <iomanip>
#include <sstream>
#include <TStyle.h>
#include <TFile.h>
#include <TThread.h>
#include <TGClient.h>

#include "ConfigAnalyzer.hh"
#include "StringBalancedTable.hh"
#include "TermManip.hh"
#include "ConfigSettings.hh"
#include "OMMainWindow.hh"
#include "IOPrimitive.hh"
#include "NA62Exceptions.hh"
#include "Misc.hh"
#include "Indexer.hh"
#include "EventHeader.hh"
#include "EventChecker.hh"
#include "BadBursts.hh"
#include "TriggerConditions.hh"
#include "BeamParameters.hh"
#include "NA62ConditionsService.hh"
#include "Stream.hh"

namespace NA62Analysis {
namespace Core {

BaseAnalysis::BaseAnalysis() :
		Verbose("BaseAnalysis"), fNEvents(-1), fFirstGoodEvent(-1), fLastSpecialTriggerEvent(0), fEventsDownscaling(0), fCurrentBurstNumber(
				-1), fCurrentRunNumber(-1), fCurrentMCRevision(""), fCurrentRecoRevision(""), fBurstProcessed(0), fGraphicMode(false), fInitialized(
				false), fContinuousReading(false), fSignalStop(
				false), fFiltering(false), fForcePreAnalyzers(false), fSpecialOnly(false), fFirstAnalyzer(0), fDefaultOStream(
				""), fEventChecker(nullptr), fIHandler(
				nullptr), fIOPrimitive(nullptr), fInitTime(true), fRunThread(
				nullptr), fOMMainWindow(nullptr), fVirtualMemoryVsEvent(new TGraph()), fVirtualMemoryVsTime(
				new TGraph()), fResidentMemoryVsEvent(new TGraph()), fResidentMemoryVsTime(new TGraph()),
				fMemoryMonitorThread(nullptr) {
	/// \MemberDescr
	/// Constructor
	/// \EndMemberDescr

	gStyle->SetOptFit(1);
	NA62Analysis::manip::enableManip =
			Configuration::ConfigSettings::global::fUseColors
			&& isatty(fileno(stdout));

	fVirtualMemoryVsEvent->SetName("fVirtualMemoryVsEvent");
	fVirtualMemoryVsEvent->SetTitle("Virtual memory vs. Event");
	fVirtualMemoryVsTime->SetName("fVirtualMemoryVsTime");
	fVirtualMemoryVsTime->SetTitle("Virtual memory vs. Time");
	fResidentMemoryVsEvent->SetName("fResidentMemoryVsEvent");
	fResidentMemoryVsEvent->SetTitle("Resident memory vs. Event");
	fResidentMemoryVsTime->SetName("fResidentMemoryVsTime");
	fResidentMemoryVsTime->SetTitle("Resident memory vs. Time");
}

BaseAnalysis::~BaseAnalysis() {
	/// \MemberDescr
	/// Destructor.
	/// \EndMemberDescr

	if(fInitialized)
		Terminate();

	if (fRunThread)
		delete fRunThread;

	if (fEventChecker)
		delete fEventChecker;

	if (fIHandler)
		delete fIHandler;

	for(auto itOStream : fOStream)
		delete itOStream.second.fOutputHandler;

	if (fOMMainWindow)
		delete fOMMainWindow;

	delete fVirtualMemoryVsEvent;
	delete fVirtualMemoryVsTime;
	delete fResidentMemoryVsEvent;
	delete fResidentMemoryVsTime;
	delete fMemoryMonitorThread;
}

void BaseAnalysis::Terminate(){
	/// \MemberDescr
	/// Terminate all running processing and threads in preparation for a delete
	/// \EndMemberDescr

	if (fRunThread) {
		fSignalStop = true;
		fIHandler->SignalExit();
		while(fSignalStop){
			gSystem->Sleep(300);
		}
		fRunThread->Delete();
		while (fRunThread->GetState() != TThread::kCanceledState
				&& fRunThread->GetState() != TThread::kFinishedState)
			gSystem->Sleep(300);
		sleep(3);
		fRunThread->Kill();
	}
	StopMemoryMonitor();

	fInitialized = false;
}

void BaseAnalysis::AddInputFiles(TString inFileName, Int_t NFiles){
	/// \MemberDescr
	/// \param inFileName : path to the input file / path to the file containing the list of input files
	///	\param NFiles : Maximum number of input files to process
	///
	/// Add all the input files in a list and verify their existence
	/// \EndMemberDescr

	std::cout << extended() << "Adding input files" << std::endl;
	if(!fIHandler->AddInputFiles(inFileName, NFiles))
		throw LogicException();
}

void BaseAnalysis::Init(TString outFileName, TString params, TString configFile, TString refFile, bool ignoreNonExisting) {
	/// \MemberDescr
	/// \param outFileName : path to the output file
	///	\param params : list of command line parameters to parse and pass to analyzers
	/// \param configFile : path to a runtime configuration file to be parsed and defining parameters for analyzers.
	/// \param refFile : Eventual name of a file containing reference plots
	/// \param ignoreNonExisting : Continue processing if input tree is not found
	///
	/// Add all the input files to TChains and to Analyzers and create branches.\n
	/// Initialize the output trees (Histograms) and create branches.
	/// \EndMemberDescr

	//##############################
	//Get data from Files
	//Check integrity
	//Check all the data are present
	//##############################
	std::cout << extended() << "Starting time based memory usage thread... " << std::endl;
	StartMemoryMonitor();
	Configuration::ConfigAnalyzer confParser;
	std::cout << extended() << "Parsing parameters" << std::endl;
	//Parse parameters from file
	confParser.ParseFile(configFile);
	//Parse parameters from commandLine
	confParser.ParseCLI(params);

	std::cout << extended() << "Initializing... " << std::endl;
	fIHandler->OpenInput(fSpecialOnly);

	//Filtering activated. Read filter section of config file and create the output streams
	if(fFiltering){
		std::map<TString, std::vector<TString> > outputStreams = confParser.GetFilterParams();
		for(auto streams : outputStreams){
			CreateOStream(streams.first);
			GetOHandler(streams.first)->SetDisabledTrees(confParser.GetStreamDisabledTrees(streams.first));
			if(fDefaultOStream=="") fDefaultOStream = streams.first;
			for(auto anName : streams.second)
				AddAnalyzerToOStream(streams.first, anName);
		}
	}

	//If 0 or 1 output stream: usual single output case. Make sure at least 1 output
	//is created.
	if(fOStream.size()==0){
		fDefaultOStream = "default";
		CreateOStream(fDefaultOStream);
		GetOHandler(fDefaultOStream)->SetDisabledTrees(confParser.GetStreamDisabledTrees(fDefaultOStream));
		GetOHandler(fDefaultOStream)->OpenOutput(outFileName);
	}
	else{
		//Multiple output streams. Append stream id to the output file name.
		std::string outFileBase, outFileExt;
		size_t dot = std::string(outFileName.Data()).find_last_of(".");
		if(dot<(size_t)outFileName.Length()){
			outFileExt = std::string(outFileName.Data()).substr(dot, outFileName.Length()-dot);
			outFileBase = std::string(outFileName.Data()).substr(0, dot);
		}
		else {
			outFileExt = "";
			outFileBase = outFileName;
		}
		for(auto itOStream : fOStream)
			itOStream.second.fOutputHandler->OpenOutput(Form("%s_%s%s",
					outFileBase.data(), itOStream.first.Data(), outFileExt.data()));
	}

	// Disable bad burst checks for some subsystems as required by command line arguments
	BadBursts::GetInstance()->MaskSystems(Configuration::ConfigSettings::CLI::fNoBadBurstSystems);
	BadBursts::GetInstance()->Print();

	if (IsTreeType()) {
		InputTree * treeHandler = static_cast<InputTree*>(fIHandler);

		treeHandler->SetReferenceFileName(refFile);
		treeHandler->SetIgnoreNonExisting(ignoreNonExisting);

		fNEvents = std::max(treeHandler->FillMCTruth(),
				treeHandler->FillEventHeader());

		fNEvents = GetIOTree()->BranchTrees(fNEvents);
		std::cout << extended() << "Using " << fNEvents << " events" << std::endl;
	} else if (IsHistoType())
		fNEvents = fIHandler->GetInputFileNumber();
	fIHandler->BranchStream();
	fIHandler->LoadMCStream(0);

	Long64_t testEvent = 0;
	if(fSpecialOnly && IsTreeType()){
		while (!GetIOTree()->LoadSpecialEvent(testEvent) && testEvent < fNEvents)
			testEvent++;
		if (testEvent == fNEvents) {
			std::cout << normal() << "[ERROR] Unable to load any event/file" << std::endl;
			fIHandler->SkippedAllFiles();
			exit(0);
		}
		fFirstGoodEvent = testEvent;
	}
	else{
		while (!fIHandler->LoadEvent(testEvent) && testEvent < fNEvents)
			testEvent++;
		if (testEvent == fNEvents) {
			if (IsTreeType() && fNEvents == 0){
				std::cout << normal()
						<< "Unable to find any event. Maybe you want to use the --histo option?"
						<< std::endl;
				fIHandler->SkippedAllFiles();
				exit(0);
			}
			else{
				std::cout << normal() << "[ERROR] Unable to load any event/file. All bad bursts?" << std::endl;
				//Complete the analysis
				// cppcheck-suppress syntaxError
				using NA62Analysis::operator-;
				float totalTime = fInitTime.GetTime() - fInitTime.GetStartTime();
				// #NOQA_VERBOSE
				std::cout << std::setprecision(2);
				std::cout << std::endl << "###################################"
						<< std::endl;
				std::cout << "Total events processed: " << fNEvents << std::endl;
				if(fEventChecker)
					std::cout << "Events skipped by EventChecker: " << fEventChecker->GetTotalSkippedEvents() << std::endl;
				std::cout << "Total time: " << std::setw(17) << std::fixed << totalTime
						<< " seconds" << std::endl;
				std::cout << " - Init time: " << std::setw(15) << std::fixed
						<< fInitTime.GetTotalTime() << " seconds" << std::endl;
				std::cout << "IO time: " << std::setw(20)
						<< fIHandler->GetIoTimeCount().GetTotalTime() << " seconds"
						<< std::endl;
				std::cout << std::endl << "Analysis complete" << std::endl
						<< "###################################" << std::endl;
				exit(0);
			}
		}
		fFirstGoodEvent = testEvent;
	}
	CheckNewFileOpened();

	std::cout << extended() << "Parsed parameters: " << std::endl;
	if (TestLevel(Verbosity::kExtended))
		confParser.Print();

	//
	if(fForcePreAnalyzers || !fFiltering)
		fFirstAnalyzer = 0;
	if (!fIsPythonFile){
		for (unsigned int i = fFirstAnalyzer; i < fAnalyzerList.size(); i++) {
			for(auto itOutputHandler : fOStream) {
				itOutputHandler.second.fOutputHandler->MkOutputDir(fAnalyzerList[i]->GetAnalyzerName());
			}
			GetOHandler(fDefaultOStream)->SetOutputFileAsCurrent(fAnalyzerList[i]->GetAnalyzerName());
	
			confParser.ApplyParams(fAnalyzerList[i]);
	
			fAnalyzerList[i]->InitOutput();
			fAnalyzerList[i]->InitHist();
			fAnalyzerList[i]->PrepareTrees();
	
			fAnalyzerList[i]->DefineMCSimple();
			fAnalyzerList[i]->PrintInitSummary();
			GetOHandler(fDefaultOStream)->SetOutputFileAsCurrent();
		}
	}
	PrintInitSummary();

	if(IsTreeType()){
		//Do not pass the list of enabled detectors, but the list of no-check detectors (NARKD-464)
		//fEventChecker = new EventChecker(GetIOTree()->GetRequestedDetectorNames());
		fEventChecker = new EventChecker(Configuration::ConfigSettings::CLI::fNoCheckSystems, fIHandler->GetRevision());
	}

	fInitialized = true;
	fInitTime.Stop();
}

void BaseAnalysis::AddAnalyzer(Analyzer* an) {
	/// \MemberDescr
	/// \param an : Pointer to the analyzer
	///
	/// Add an analyzer to the Analyzer lists
	/// \EndMemberDescr

	std::cout << normal() << "Adding analyzer " << an->GetAnalyzerName() << std::endl;
	an->SetVerbosity(GetCoreVerbosityLevel(), GetAnalyzerVerbosityLevel());
	fAnalyzerList.push_back(an);
}

void BaseAnalysis::RegisterOutput(TString name, const void * const address) {
	/// \MemberDescr
	/// \param name : Name of the output
	/// \param address : pointer to the output variable
	///
	/// Register an output
	/// \EndMemberDescr

	std::cout << extended() << "Registering output " << name << std::endl;
	std::cout << debug() << " at address " << address << std::endl;
	fOutput.insert(std::pair<const TString, const void* const >(name, address));
	fOutputStates.insert(
			std::pair<const TString, Analyzer::OutputState>(name,
					Analyzer::kOUninit));
}

void BaseAnalysis::SetOutputState(TString name, Analyzer::OutputState state) {
	/// \MemberDescr
	/// \param name : name of the output
	/// \param state : state to be set
	///
	/// Set the state of the output
	/// \EndMemberDescr

	fOutputStates[name] = state;
}

const void *BaseAnalysis::GetOutput(TString name,
		Analyzer::OutputState &state) const {
	/// \MemberDescr
	/// \param name : name of the output
	/// \param state : is filled with the current state of the output
	///
	/// Return an output variable and the corresponding state
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString, const void* const >::type::const_iterator ptr;

	if ((ptr = fOutput.find(name)) != fOutput.end()) {
		state = fOutputStates.find(name)->second;
		return ptr->second;
	} else {
		state = Analyzer::kOUninit;
		std::cout << normal() << "Output " << name << " not found" << std::endl;
		return 0;
	}
}

void BaseAnalysis::PreProcess() {
	/// \MemberDescr
	/// Pre-processing method. Reset the states of the output
	/// \EndMemberDescr

	NA62Analysis::NA62Map<TString, Analyzer::OutputState>::type::iterator itState;
	NA62Analysis::NA62Map<TString, void*>::type::iterator itOut;

	for (itState = fOutputStates.begin(); itState != fOutputStates.end();
			itState++) {
		itState->second = Analyzer::kOInvalid;
	}

	for (unsigned int j = fFirstAnalyzer; j < fAnalyzerList.size(); j++) {
		fAnalyzerList[j]->PreProcess();
	}
}

bool BaseAnalysis::Process(Long64_t beginEvent, Long64_t maxEvent, int maxBurst) {
	/// \MemberDescr
	/// \param beginEvent : index of the first event to be processed
	/// \param maxEvent : maximum number of events to be processed
	/// \param maxBurst : maximum number of burst to be processed
	/// \return True if successful
	///
	/// Main process loop. Read the files event by event and process each analyzer in turn for each event
	/// \EndMemberDescr

	if (!fInitialized)
		return false;

	TimeCounter_us processLoopTime;
	TimeCounter_us processTime;

	int i_offset;
	Long64_t exportEvent;

	processLoopTime.Start();

	std::string displayType;
	if (IsTreeType())
		displayType = "event";
	else if (IsHistoType())
		displayType = "file";

	if(beginEvent<fFirstGoodEvent)
		beginEvent = fFirstGoodEvent;

	//Print event processing summary
	if (maxEvent > fNEvents || maxEvent <= 0)
		maxEvent = fNEvents;
	std::cout << extended() << "Treating " << maxEvent << " " << displayType
			<< "s, beginning with " << displayType << " " << beginEvent
			<< std::endl;
	if(maxBurst>0)
		std::cout << extended() << "Reading maximum " << maxBurst << " bursts" << std::endl;

	if (fIHandler->IsFastStart())
		i_offset = 1000;
	else if(maxEvent>10000)
		i_offset = 10000;
	else
		i_offset = maxEvent / 100.;
	if (i_offset == 0)
		i_offset = 1;
	std::cout << extended() << "i_offset : " << i_offset << std::endl;

	int checkInterval;
	if(Configuration::ConfigSettings::global::fMemoryReportEventInterval==-1)
		checkInterval = i_offset;
	else if(Configuration::ConfigSettings::global::fMemoryReportEventInterval==0)
		checkInterval = INT32_MAX;
	else
		checkInterval = Configuration::ConfigSettings::global::fMemoryReportEventInterval;


	if(Configuration::ConfigSettings::global::fMemoryReportEventInterval!=0)
		FillMemoryUsage(fVirtualMemoryVsEvent, fResidentMemoryVsEvent, -1);


	//##############################
	//Begin event loop
	//##############################
	int defaultPrecision = std::cout.precision();
	Long64_t processEvents = std::min(beginEvent + maxEvent, fNEvents);

	std::map<TString, bool> exportFilters;
	for(auto itOutput : fOStream)
		exportFilters.insert(std::pair<const TString, bool>(itOutput.first, false));

	if (!fIsPythonFile){
		for (Long64_t i = beginEvent; (i < processEvents || processEvents < 0) && !fSignalStop; ++i) {
			//Print current event
			if (i % i_offset == 0) {
				printCurrentEvent(i, processEvents, defaultPrecision, displayType,
						processLoopTime);
			}
			if (fEventsDownscaling > 0 && (i % fEventsDownscaling != 0))
				continue;

			// Load event infos
			if (!fSpecialOnly && !fIHandler->LoadEvent(i)){
				std::cout << normal() << "Unable to read event " << i << std::endl;
				continue;
			}
			CheckNewFileOpened();
			CheckBurstAndRunChange(maxBurst);

			if(maxBurst>0 && fBurstProcessed>maxBurst){
				std::cout << extended() << maxBurst << " bursts have been processed. Stopping processing" << std::endl;
				processEvents = i;
				break;
			}

			if(fSpecialOnly && IsTreeType()){
				i = fLastSpecialTriggerEvent-1;
				//We finally know the total number of events in the sample
				if (fIHandler->IsFastStart() && fNEvents < processEvents)
					processEvents = fNEvents;
				continue;
			}


			if(IsTreeType() &&
					!(fEventChecker->IsGoodEvent(GetIOTree()->GetEventHeaderEvent()) ^ NA62Analysis::Configuration::ConfigSettings::CLI::fInvertBadEvent) ){
				std::cout << extended() << "Event " << i << " is flagged as " <<
					(Configuration::ConfigSettings::CLI::fInvertBadEvent ? "good" : "bad") << "... skipping." << std::endl;
				//We finally know the total number of events in the sample
				if (fIHandler->IsFastStart() && fNEvents < processEvents)
					processEvents = fNEvents;
				continue;
			}

			//Reset export booleans
			for(auto &itOutput : exportFilters)
				itOutput.second = false;

			PreProcess();
			//Process event in Analyzer
			exportEvent = 0;
			for (unsigned int j = fFirstAnalyzer; j < fAnalyzerList.size(); j++) {
				//Get MCSimple
				GetOHandler(fDefaultOStream)->SetOutputFileAsCurrent(fAnalyzerList[j]->GetAnalyzerName());
				if (IsTreeType() && fIHandler->GetWithMC())
					fAnalyzerList[j]->FillMCSimple(
							static_cast<InputTree*>(fIHandler)->GetMCTruthEvent());

				processTime.Start();
				fAnalyzerList[j]->Process(i);
				processTime.Stop();

				if (fGraphicalMutex.Lock() == 0) {
					fAnalyzerList[j]->UpdatePlots(i);
					fGraphicalMutex.UnLock();
				}
				if (fFiltering){
					exportEvent = exportEvent
						| (fAnalyzerList[j]->GetExportEvent() << (j-fFirstAnalyzer));
					if(fAnalyzerList[j]->GetExportEvent())
						for(auto itStreams : fOStream)
							if(itStreams.second.IsAnalyzerInList(fAnalyzerList[j]->GetAnalyzerName()))
								exportFilters[itStreams.first] = true;
				}
				GetOHandler(fDefaultOStream)->SetOutputFileAsCurrent();
			}

			for (unsigned int j = fFirstAnalyzer; j < fAnalyzerList.size(); j++) {
				GetOHandler(fDefaultOStream)->SetOutputFileAsCurrent(fAnalyzerList[j]->GetAnalyzerName());
				fAnalyzerList[j]->PostProcess();
				GetOHandler(fDefaultOStream)->SetOutputFileAsCurrent();
			}


			if (IsTreeType() && exportEvent > 0 && fFiltering){
				GetIOTree()->UpdateFilterWord(exportEvent);
				for(auto itOStream : fOStream){
					if(itOStream.second.IsEmpty() || exportFilters[itOStream.first])
						itOStream.second.fOutputHandler->WriteEvent(GetIOTree()->GetFilterWordRef(), GetIOTree()->GetEnabledTrees(), GetIOTree()->GetReferenceTree());
				}
				GetOHandler(fDefaultOStream)->SetOutputFileAsCurrent();
			}

			//We finally know the total number of events in the sample
			if (fIHandler->IsFastStart() && fNEvents < processEvents)
				processEvents = fNEvents;

			if ((i % checkInterval)	== 0 && checkInterval!=INT32_MAX)
				FillMemoryUsage(fVirtualMemoryVsEvent, fResidentMemoryVsEvent, i);

		}
	}

	//Ask the analyzer to export and draw the plots
	processTime.Start();
	DoEndOfBurstActions();
	DoEndOfRunActions();
	processTime.Stop();

	for (unsigned int j = fFirstAnalyzer; j < fAnalyzerList.size(); j++) {
		for(auto itOutputHandler : fOStream){
			itOutputHandler.second.fOutputHandler->SetOutputFileAsCurrent(fAnalyzerList[j]->GetAnalyzerName());
			processTime.Start();
			fAnalyzerList[j]->EndOfJob(fFiltering);
			fAnalyzerList[j]->ExportPlot();
			processTime.Stop();
			itOutputHandler.second.fOutputHandler->SetOutputFileAsCurrent();
		}

		if (fGraphicalMutex.Lock() == 0) {
			if (fGraphicMode)
				fAnalyzerList[j]->DrawPlot();
			fGraphicalMutex.UnLock();
		}

	}

	fIHandler->Finalise(maxEvent < fNEvents);
	WriteOutput();

	float outputIO = 0.;
	for(auto itOutputHandler : fOStream)
		outputIO += itOutputHandler.second.fOutputHandler->GetIoTimeCount().GetTotalTime();

	StopMemoryMonitor();

	//Complete the analysis
	using NA62Analysis::operator -;
	float totalTime = fInitTime.GetTime() - fInitTime.GetStartTime();
	// #NOQA_VERBOSE
	std::cout << std::setprecision(2);
	std::cout << std::endl << "###################################"
			<< std::endl;
	std::cout << "Total events processed: " << (processEvents-beginEvent) << std::endl;
	if(fEventChecker)
		std::cout << "Events skipped by EventChecker: " << fEventChecker->GetTotalSkippedEvents() << std::endl;
	std::cout << "Total time: " << std::setw(17) << std::fixed << totalTime
			<< " seconds" << std::endl;
	std::cout << " - Init time: " << std::setw(15) << std::fixed
			<< fInitTime.GetTotalTime() << " seconds" << std::endl;
	std::cout << " - Process loop time: " << std::setw(7) << std::fixed
			<< processLoopTime.GetTotalTime() << " seconds" << std::endl;
	std::cout << "   - Processing time: " << std::setw(7)
			<< processTime.GetTotalTime() << " seconds" << std::endl;
	std::cout << "IO time: " << std::setw(20)
			<< fIHandler->GetIoTimeCount().GetTotalTime()+outputIO << " seconds"
			<< std::endl;
	std::cout << std::endl << "Analysis complete" << std::endl
			<< "###################################" << std::endl;

	return true;
}

void BaseAnalysis::WriteOutput(){
	if (!fInitialized) return;
	fCounterHandler.WriteEventFraction(GetOHandler(fDefaultOStream)->GetOutputFileName());
	for(auto itOutputHandler : fOStream){
		if (IsTreeType())
			itOutputHandler.second.fOutputHandler->WriteTree();
		itOutputHandler.second.fOutputHandler->WriteAnalyzerList(std::vector<Analyzer*>(fAnalyzerList.begin()+fFirstAnalyzer, fAnalyzerList.end()));
		itOutputHandler.second.fOutputHandler->SetOutputFileAsCurrent();
		fVirtualMemoryVsEvent->Write();
		fVirtualMemoryVsTime->Write();
		fResidentMemoryVsEvent->Write();
		fResidentMemoryVsTime->Write();
		itOutputHandler.second.fOutputHandler->Finalise();
	}
}

void BaseAnalysis::PrintInitSummary() const {
	/// \MemberDescr
	/// Print summary after initialization.
	/// \EndMemberDescr

	if (!TestLevel(Verbosity::kExtended))
		return;

	NA62Analysis::NA62Map<TString, const void* const >::type::const_iterator itOutput;

	StringBalancedTable anTable("List of loaded Analyzers");
	StringBalancedTable outputTable("List of Outputs");

	for (auto itAn : skip<const decltype(fAnalyzerList)>(fAnalyzerList, fFirstAnalyzer)) {
		anTable << itAn->GetAnalyzerName();
	}

	for (itOutput = fOutput.begin(); itOutput != fOutput.end(); itOutput++) {
		outputTable << itOutput->first;
	}

	std::cout
			<< "================================================================================"
			<< std::endl;
	std::cout << std::endl << "\t *** Global settings for AnalysisFW ***"
			<< std::endl << std::endl;

	anTable.Print("\t");
	fCounterHandler.PrintInitSummary();
	outputTable.Print("\t");
	fIHandler->PrintInitSummary();
	std::cout
			<< "================================================================================"
			<< std::endl;
}

void BaseAnalysis::CheckNewFileOpened() {
	/// \MemberDescr
	/// Method called by TChain when opening a new file.\n
	/// It will signal a new burst to the analyzers
	/// \EndMemberDescr

	GetOHandler(fDefaultOStream)->SetOutputFileAsCurrent();
	if (!fIHandler->CheckNewFileOpened())
		return;
	//New file opened
	for(auto itOutputHandler : fOStream){
		itOutputHandler.second.fOutputHandler->AddFileName(fIHandler->GetCurrentFile()->GetName());
		itOutputHandler.second.fOutputHandler->AddStreamEvent(fIHandler->GetStreamInfo());
	}
	std::cout << extended() << "New file opened: " << fIHandler->GetCurrentFile()->GetName() << std::endl;
	//first burst or not? Update histos only if it's not
	if (fIHandler->GetCurrentFileNumber() > fFirstGoodEvent && IsHistoType())
			GetIOHisto()->UpdateInputHistograms();

	if (fIHandler->IsFastStart() || fNEvents==TChain::kBigNumber)
		fNEvents = fIHandler->GetNEvents();
}

void BaseAnalysis::CheckBurstAndRunChange(int maxBurst) {
	/// \MemberDescr
	/// \param doit: Do Analyzer actions only if true
	/// Method called by TChain when opening a new file.\n
	/// It will signal a new burst to the analyzers
	/// \EndMemberDescr

	int runNumber = fIHandler->GetRunID();
	int burstID = fIHandler->GetBurstID();

	if(runNumber>0) NA62ConditionsService::GetInstance()->SetCurrentRunID(runNumber);
	else			NA62ConditionsService::GetInstance()->SetCurrentRunID(6610); //default (for MC)
	if(!fIHandler->GetWithMC()) { //ignore burst ID for MC
		NA62ConditionsService::GetInstance()->SetCurrentBurstID(burstID);
	}

	TString mcrevision = fIHandler->GetMCRevision();
	TString recorevision = fIHandler->GetRecoRevision();

	TString CDBSubDir = fIHandler->GetWithMC() ? "MC" : "Data";
	NA62ConditionsService::GetInstance()->SetCDBTag(CDBSubDir+"/"+recorevision);
        
	GetOHandler(fDefaultOStream)->SetOutputFileAsCurrent();
	std::cout << debug() << "Verifying burst change: " <<
			PRINTVAR(mcrevision) << PRINTVAR(fCurrentMCRevision) <<
			PRINTVAR(recorevision) << PRINTVAR(fCurrentRecoRevision) <<
			PRINTVAR(runNumber) << PRINTVAR(fCurrentRunNumber) <<
			PRINTVAR(burstID) << PRINTVAR(fCurrentBurstNumber) << std::endl;

	bool revisionChanged = (mcrevision!=fCurrentMCRevision) || (recorevision!=fCurrentRecoRevision);
	if(runNumber==fCurrentRunNumber && burstID==fCurrentBurstNumber && !revisionChanged)
		return;

	if(burstID!=fCurrentBurstNumber)
		std::cout << extended() << "Changing burst number (" << fCurrentBurstNumber << "->" << burstID << ")" << std::endl;
	if(runNumber!=fCurrentRunNumber)
		std::cout << extended() << "Changing run number (" << fCurrentRunNumber << "->" << runNumber << ")" << std::endl;
	if(mcrevision.CompareTo(fCurrentMCRevision)!=0)
		std::cout << extended() << "Changing MC revision (" << fCurrentMCRevision << "->" << mcrevision << ")" << std::endl;
	if(recorevision.CompareTo(fCurrentRecoRevision)!=0)
		std::cout << extended() << "Changing Reco revision (" << fCurrentRecoRevision << "->" << recorevision << ")" << std::endl;

	if(burstID!=fCurrentBurstNumber)
		++fBurstProcessed;


        // checking all the bursts of the file
	if(fIHandler->GetStreamInfo()){
		std::vector<UInt_t> rID = fIHandler->GetStreamInfo()->GetRecoInfo().GetRunID();
		std::vector<UInt_t> bID = fIHandler->GetStreamInfo()->GetRecoInfo().GetBurstID();
		if(!rID.size()) { // use MC
                  for(UInt_t iBurst=0; iBurst<fIHandler->GetStreamInfo()->GetMCInfo().GetRunNumber().size();iBurst++){
                    rID.push_back(fIHandler->GetStreamInfo()->GetMCInfo().GetRunNumber().at(iBurst));
                    bID.push_back(-1);
                  }
                }
                for(UInt_t iBurst=0; iBurst<rID.size();iBurst++){
                  std::cout << extended() << Form("Burst found in file: %06d %04d",rID[iBurst],bID[iBurst]) << std::endl;
                }
	}

	if(maxBurst>0 && fBurstProcessed>maxBurst)
		// No need to call the EOB/EOR (will be done by the finishing process
		// SOB/SOR should NOT be called as the next run/burst is not going to
		// be processed.
		return;

	//Run the start/end events calls in order for all analyzers, event by event.
	//first burst or not? Call end of burst only if it's not
	if(fCurrentBurstNumber!=-1) {
		fIHandler->Rewind();
		if(burstID!=fCurrentBurstNumber) //end of burst
			DoEndOfBurstActions();

		if(runNumber!=fCurrentRunNumber || revisionChanged) //end of run
			DoEndOfRunActions();
		fIHandler->ReloadLatest();
	}

	if(runNumber!=fCurrentRunNumber || revisionChanged)
		DoStartOfRunActions();
	if(burstID!=fCurrentBurstNumber)
		DoStartOfBurstActions();

	fCurrentBurstNumber = burstID;
	fCurrentRunNumber = runNumber;
	fCurrentMCRevision = mcrevision;
	fCurrentRecoRevision = recorevision;
	TString digit = TString(fIHandler->GetRevision()(1, 4).Data());

	if (IsTreeType() && (digit.IsWhitespace() || !digit.IsDigit() || (digit.Atoi()>1411)) )
		ProcessSpecialTriggersForCurrentBurst();
}

InputHandler* BaseAnalysis::GetIOHandler() {
	/// \MemberDescr
	///	\return Pointer to the IOHandler instance
	/// \EndMemberDescr

	return fIHandler;
}

OutputHandler* BaseAnalysis::GetOHandler(TString oHandler) {
	/// \MemberDescr
	/// \param oHandler: id of the output handler to get
	///	\return Pointer to the IOHandler instance
	/// \EndMemberDescr

	return fOStream[oHandler].fOutputHandler;
}

std::vector<OutputHandler*> BaseAnalysis::GetOHandlersForAnalyzer(TString analyzerName){
	std::vector<OutputHandler*> vHandlers;
	if(fFiltering){
		for(auto stream : fOStream)
			if(stream.second.IsAnalyzerInList(analyzerName)) vHandlers.push_back(stream.second.fOutputHandler);
	}
	else
		vHandlers.push_back(GetOHandler(fDefaultOStream));

	return vHandlers;
}

CounterHandler* BaseAnalysis::GetCounterHandler() {
	/// \MemberDescr
	///	\return Pointer to the CounterHandler instance
	/// \EndMemberDescr

	return &fCounterHandler;
}

Long64_t BaseAnalysis::GetNEvents() {
	/// \MemberDescr
	///	\return Total number of events loaded from the input files
	/// \EndMemberDescr
	return fNEvents;
}

TChain* BaseAnalysis::GetTree(TString name) {
	/// \MemberDescr
	/// \param name : Name of the TChain
	///
	///	\return Pointer to the TChain
	/// \EndMemberDescr

	if (IsTreeType())
		return GetIOTree()->GetTree(name);
	else
		return nullptr;
}

InputTree* BaseAnalysis::GetIOTree() {
	/// \MemberDescr
	/// \return Pointer to IOTree ifIOHandler is of Tree Type
	/// \EndMemberDescr

	if (IsTreeType())
		return static_cast<InputTree*>(fIHandler);
	else
		return nullptr;
}

InputHisto* BaseAnalysis::GetIOHisto() {
	/// \MemberDescr
	/// \return Pointer to IOHisto ifIOHandler is of Histo Type
	/// \EndMemberDescr

	if (IsHistoType())
		return static_cast<InputHisto*>(fIHandler);
	else
		return nullptr;
}

void BaseAnalysis::SetReadType(IOHandlerType type) {
	/// \MemberDescr
	/// \param type: Type of IOHandler to use
	///
	/// Create the correct instance of IOHandler
	/// \EndMemberDescr

	std::cout << extended() << "Creating IOHandler of type "
			<< (type == IOHandlerType::kHISTO ? "kHisto" : "kTree")
			<< std::endl;
	if (type == IOHandlerType::kHISTO)
		fIHandler = new InputHisto();
	else
		fIHandler = new InputTree();

	fIHandler->SetMutex(&fGraphicalMutex);
}

void BaseAnalysis::CreateOStream(TString id){
	/// \MemberDescr
	/// \param id: id of the output stream
	///
	/// If not existing, create and initialize the OutputStream
	/// \EndMemberDescr

	if(fOStream.count(id)==0){
		std::cout << extended() << "Create output handler " << id << std::endl;
		fOStream.insert(std::pair<const TString, OutputStream>(id, OutputStream(id)));
	}
}

void BaseAnalysis::AddAnalyzerToOStream(TString id, TString anName){
	/// \MemberDescr
	/// \param id: id of the output stream
	/// \param anName: name of the analyzer to add
	///
	/// Check if the analyzer exists and if yes, add it to the list of analyzer
	/// for which the output stream checks the filter accept.
	/// \EndMemberDescr

	bool found = false;
	for(auto itAn : skip<decltype(fAnalyzerList)>(fAnalyzerList, fFirstAnalyzer))
		if(itAn->GetAnalyzerName().CompareTo(anName, TString::kIgnoreCase)==0)
			found = true;

	if(!found){
		std::cout << always() << "Analyzer " << anName << " requested for "
				<< "filter stream " << id << " but is not found in the list of"
				<< " analyzers." << std::endl;
		throw LogicException();
	}
	std::cout << normal() << "Add analyzer " << anName << " to output handler " << id << std::endl;
	fOStream[id].fAnList.insert(anName);
}

void BaseAnalysis::printCurrentEvent(Long64_t iEvent, Long64_t totalEvents,
		int defaultPrecision, std::string &displayType, TimeCounter_us startTime) {
	/// \MemberDescr
	/// \param iEvent: currently processed object
	/// \param totalEvents: total number of objects
	/// \param defaultPrecision: default floating point number precision in cout
	/// \param displayType: Type of object (event, file)
	/// \param startTime: start time of the processing
	///
	/// Print the currently processed object. Formatting done according to settings.
	/// Also print the percentage of completion and the estimated remaining time.
	/// \EndMemberDescr

	if (!TestLevel(Verbosity::kNormal))
		return;
	std::stringstream ss;

	//Print current event
	if (Configuration::ConfigSettings::global::fUseColors)
		std::cout << manip::red << manip::bold;

	float elapsed = startTime.GetTotalTime();
	float eta = 0;
	if (iEvent > 0)
		eta = (elapsed) * ((totalEvents - iEvent) / (double) iEvent);
	float totalTime = iEvent > 0 ? elapsed + eta : elapsed;

	//Processing what current/total =>
	if(totalEvents==TChain::kBigNumber)
		ss << "Processing " << displayType << " " << iEvent;
	else
		ss << "Processing " << displayType << " " << iEvent << "/"
				<< totalEvents;
	std::pair<double,double> memoryUsage = GetMemoryUsage();
	std::cout << "[" << PrintDate() << "] " << std::setprecision(2) << std::fixed <<
				"[Memory V:" << std::setw(7) << memoryUsage.first <<
				", R:" << memoryUsage.second << "] " <<
				std::setw(35) << std::left << ss.str() << " => ";
	// percentage%
	std::cout << std::setprecision(2) << std::fixed << std::setw(6)
			<< std::right << ((double) iEvent / (double) totalEvents) * 100
			<< "%" << std::setprecision(0);
	// ETA: 123s
	if (iEvent == 0 || totalEvents==TChain::kBigNumber)
		std::cout << std::setw(8) << "ETA: " << "----s";
	else
		std::cout << std::setw(8) << "ETA: " << eta << "s";

	// Elapsed: 123s
	std::cout << std::setw(12) << "Elapsed: " << elapsed << "s";
	// Total: 123s
	if(totalEvents!=TChain::kBigNumber)
		std::cout << std::setw(10) << "Total: " << totalTime << "s";

	if (Configuration::ConfigSettings::global::fUseColors)
		std::cout << manip::reset;
	if (Configuration::ConfigSettings::global::fProcessOutputNewLine)
		std::cout << std::endl;
	else
		std::cout << std::setw(10) << "\r" << std::flush;

	//Reset to default
	std::cout.precision(defaultPrecision);
	std::cout.unsetf(std::ios_base::floatfield);
}

void BaseAnalysis::printCurrentSpecialEvent(Long64_t iEvent) {
	/// \MemberDescr
	/// \param iEvent: currently processed object
	///
	/// Print the currently processed special event. Formatting done according to settings.
	/// \EndMemberDescr

	if (!TestLevel(Verbosity::kNormal))
		return;
	std::stringstream ss;

	//Print current event
	if (Configuration::ConfigSettings::global::fUseColors)
		std::cout << manip::red << manip::bold;

	//Processing what current/total =>
	ss << "Processing special event " << iEvent;
	std::cout << "[" << PrintDate() << "] " << std::setw(35) << std::left << ss.str();

	if (Configuration::ConfigSettings::global::fUseColors)
		std::cout << manip::reset;
	if (Configuration::ConfigSettings::global::fProcessOutputNewLine)
		std::cout << std::endl;
	else
		std::cout << std::right << std::setw(10) << "\r" << std::flush;
}

void BaseAnalysis::SetContinuousReading(bool flagContinuousReading) {
	/// \MemberDescr
	/// \param flagContinuousReading: enable/disable flag
	///
	/// Enable/Disable the continuousReading mode in both BaseAnalysis and the IOHandler
	/// \EndMemberDescr

	fContinuousReading = flagContinuousReading;
	fIHandler->SetContinuousReading(flagContinuousReading);
}

void BaseAnalysis::SetDownscaling(bool flagDownscaling) {
	/// \MemberDescr
	/// \param flagDownscaling: enable/disable flag
	///
	/// Enable/Disable the downscaling
	/// \EndMemberDescr

	if (flagDownscaling)
		fEventsDownscaling =
				Configuration::ConfigSettings::global::fEventsDownscaling;
	else
		fEventsDownscaling = 0;
}

void BaseAnalysis::StartContinuous(TString inFileList) {
	/// \MemberDescr
	/// \param inFileList: Path to a text file containing a list of input root files
	///
	/// Start the continuous reading loop:
	/// - Setup the GUI
	/// - Start the Processing loop in its own thread to decouple the GUI from the processing
	/// \EndMemberDescr

	//Prepare TThread arguments (needs reference to this and input file list
	ThreadArgs_t *args = new ThreadArgs_t();
	args->ban = this;
	args->inFileList = inFileList;

	//Create GUI
	CreateOMWindow();

	//Start the thread
	fRunThread = new TThread("t0", (void (*)(void*))&ContinuousLoop, (void*) args);
	//Allow cancelation of thread only at specific points
	fRunThread->SetCancelOn();
	fRunThread->SetCancelDeferred();
	fRunThread->Run();

	while (1) {
		//Graphical loop. Only process GUI events when the Process loop is not touching graphical objects.
		//Else crashes occurs
		if (fGraphicalMutex.Lock() == 0) {
			if(fSignalStop) {
				fGraphicalMutex.UnLock();
				return;
			}
			GetOHandler(fDefaultOStream)->SetOutputFileAsCurrent();
			gSystem->ProcessEvents();
			fGraphicalMutex.UnLock();
		}
		if(fSignalStop)
			return;
	}
}

void BaseAnalysis::CreateOMWindow() {
	/// \MemberDescr
	/// Create GUI.
	/// - Create main window
	/// - Create one tab for each Analyzer
	/// - Within the analyzer tab, create one tab for each CanvasOrganizer
	///   and pass the created canvas to the Organizer
	/// \EndMemberDescr

	fOMMainWindow = new OMMainWindow(gClient->GetRoot(),
			gClient->GetDisplayHeight(), gClient->GetDisplayWidth());
	for (auto it : skip<decltype(fAnalyzerList)>(fAnalyzerList, fFirstAnalyzer)) {
		if(it->GetCanvases().size()==0)
			continue;
		fOMMainWindow->AddAnalyzerTab(it->GetAnalyzerName());
		for (auto itCanvas : it->GetIteratorCanvas()) {
			itCanvas->SetCanvas(fOMMainWindow->AddAnalyzerCanvas(it->GetAnalyzerName(), itCanvas->GetName()));
		}
	}
	fOMMainWindow->Create();
}

void BaseAnalysis::UpdateOMWindowForAnalyzer(TString anName){
	if (fGraphicalMutex.Lock() == 0) {
		for(auto itAn : skip<decltype(fAnalyzerList)>(fAnalyzerList, fFirstAnalyzer)){
			if(itAn->GetAnalyzerName().CompareTo(anName, TString::kIgnoreCase)==0){
				if(itAn->GetCanvases().size()==0)
					break;
				//Create the analyzer window if not yet existing
				if(!fOMMainWindow->Exists(anName))
					fOMMainWindow->AddAnalyzerTab(anName);

				//Loop over canvases and add missing (no removing possible)
				for (auto itCanvas : itAn->GetIteratorCanvas()) {
					if(!fOMMainWindow->AnalyzerCanvasExists(anName, itCanvas->GetName())){
						itCanvas->SetCanvas(fOMMainWindow->AddAnalyzerCanvas(anName,itCanvas->GetName()));
					}
				}
				//recreate the window
				fOMMainWindow->Create();
				break;
			}
		}
		fGraphicalMutex.UnLock();
	}
}

void BaseAnalysis::ContinuousLoop(void* args) {
	/// \MemberDescr
	/// \param args: arguments passed to the TThread. Expected a pointer to ThreadArgs_t struct.
	///
	/// Process loop:
	/// - Wait for valid input files list
	/// - Process the root files
	/// - Start again until main thread signal end of processing through fSignalStop
	/// \EndMemberDescr

	BaseAnalysis* ban = static_cast<ThreadArgs_t*>(args)->ban;
	TString inFileList = static_cast<ThreadArgs_t*>(args)->inFileList;
	delete static_cast<ThreadArgs_t*>(args);
	Long64_t currNEvents = 0;
	int runNumber = -1;
	int burstID = -1;
	while (!ban->fSignalStop) {
		ban->GetOHandler()->SetOutputFileAsCurrent();
		if (ban->IsTreeType() && ban->GetIOTree()->GetEventHeaderEvent()){
			runNumber = ban->GetIOTree()->GetEventHeaderEvent()->GetRunID();
			burstID = ban->GetIOTree()->GetEventHeaderEvent()->GetBurstID();
		}
		ban->fOMMainWindow->UpdateTitle(runNumber, burstID);
		ban->Process(currNEvents, -1, -1);
		TThread::CancelPoint();
		while(!ban->GetIOHandler()->AddInputFiles(inFileList, -1)) {}
		ban->GetIOHandler()->OpenInput();
		currNEvents = ban->RecomputeNEvents();
	}
	ban->fSignalStop = false;

	TThread::Exit();
}

void BaseAnalysis::ReconfigureAnalyzer(TString analyzerName,
		TString parameterName, TString parameter) {
	/// \MemberDescr
	/// \param analyzerName : Analyzer to reconfigure
	/// \param parameterName : Parameter to modify
	/// \param parameter : Value to set
	///
	/// Modify parameter of an analyzer during the processing
	/// \EndMemberDescr
	for (auto it : skip<decltype(fAnalyzerList) >(fAnalyzerList,fFirstAnalyzer)) {
		if (it->GetAnalyzerName().CompareTo(analyzerName) == 0) {
			it->ApplyParam(parameterName, parameter);
		}
	}
}

IOPrimitive* BaseAnalysis::GetIOPrimitive() {
	/// \MemberDescr
	/// \return Pointer to IOPrimitive instance (nullptr if not instantiated)
	/// \EndMemberDescr
	return fIOPrimitive;
}

void BaseAnalysis::SetPrimitiveFile(TString fileName) {
	/// \MemberDescr
	/// \param fileName : Path to the prrimitive file
	///
	/// If IOPrimitive instantiated, set the primitive root file
	/// \EndMemberDescr
	if (fIOPrimitive)
		fIOPrimitive->SetFile(fileName);
}

void BaseAnalysis::InitPrimitives() {
	/// \MemberDescr
	/// Instantiate the IOPrimitive instance if not yet done.
	/// \EndMemberDescr
	if (!fIOPrimitive)
		fIOPrimitive = new IOPrimitive();
}

void BaseAnalysis::FillMemoryUsage(TGraph* virtualMemoryGraph, TGraph* residentMemoryGraph, int x) {
	/// \MemberDescr
	/// \param memoryGraph : TGraph to fill
	/// \param x : x position of the point to fill
	///
	/// Fill the TGraph at position x with the current usage of virtual memory
	/// \EndMemberDescr
	int npoints = virtualMemoryGraph->GetN();
	std::pair<double,double> memoryUsage = GetMemoryUsage();
	virtualMemoryGraph->SetPoint(npoints, x, memoryUsage.first);
	npoints = residentMemoryGraph->GetN();
	residentMemoryGraph->SetPoint(npoints, x, memoryUsage.second);
}

void BaseAnalysis::StartMemoryMonitor() {
	/// \MemberDescr
	/// Start a new thread for the memory monitoring (time based)
	/// \EndMemberDescr
	//Start the thread

	if(Configuration::ConfigSettings::global::fMemoryReportTimeInterval==0) return; //Disable time-based memory monitor

	MemoryGraphs_t *args = new MemoryGraphs_t();
	args->gVirtual = fVirtualMemoryVsTime;
	args->gResident = fResidentMemoryVsTime;

	fMemoryMonitorThread = new TThread("tMemoryMonitor", memoryMonitorLoop,
			(void*) args);
	fMemoryMonitorThread->SetCancelOn();
	fMemoryMonitorThread->Run();
}

void BaseAnalysis::StopMemoryMonitor() {
	/// \MemberDescr
	/// Stop the memory monitoring thread
	/// \EndMemberDescr
	if (fMemoryMonitorThread) {
		fMemoryMonitorThread->Kill();
		while (fMemoryMonitorThread->GetState() != TThread::kCanceledState
				&& fMemoryMonitorThread->GetState() != TThread::kFinishedState) {
			gSystem->Sleep(300);
		}
		delete fMemoryMonitorThread;
		fMemoryMonitorThread = nullptr;
	}
}

void BaseAnalysis::memoryMonitorLoop(void* args) {
	/// \MemberDescr
	/// \param args : pointer to the TGraph to be filled with the virtual memory usage
	///
	/// Main loop of the memory monitoring thread. Fills the TGraph every
	/// MemoryReportTimeInterval seconds (set in settings)
	/// \EndMemberDescr

	TGraph* gVirtual  = static_cast<MemoryGraphs_t*>(args)->gVirtual;
	TGraph* gResident = static_cast<MemoryGraphs_t*>(args)->gResident;
	delete static_cast<MemoryGraphs_t*>(args);

	int i = 0;
	int interval = Configuration::ConfigSettings::global::fMemoryReportTimeInterval;
	int smallInt = 0;

	while (true) {
		if(smallInt>=interval)
			BaseAnalysis::FillMemoryUsage(gVirtual, gResident, (i++)*interval);
		TThread::CancelPoint();
		sleep(1);
		++smallInt;
	}
}

Long64_t BaseAnalysis::RecomputeNEvents() {
	/// \MemberDescr
	/// Recompute the number of events in the IOHandler
	///
	/// \return Number of events in the IOHandler
	/// \EndMemberDescr

	Long64_t nevents = fNEvents;
	fNEvents = fIHandler->GetNEvents();
	return nevents;
}

void BaseAnalysis::StartAnalyzers() {
	/// \MemberDescr
	/// Notify that further calls to AddAnalyzer will add analyzers and not pre-analyzers
	/// \EndMemberDescr

	//fFirstAnalyzer = fAnalyzerList.size();
}

bool BaseAnalysis::AnalyzerRanOnEvent(TString thisAnName, TString anName){
	bool foundThis = false;
	for(auto an : skip<const decltype(fAnalyzerList)>(fAnalyzerList, fFirstAnalyzer)){
		if(an->GetIdentifier()==thisAnName)
			foundThis = true; //Found the current analyzer doing the request
		else if(an->GetIdentifier()==anName)
			return !foundThis; //Analyzer is apparently running after "this" one. Therefore anName did not yet run when "this" requests
	}
	return false;
}

bool BaseAnalysis::ProcessSpecialTriggersForCurrentBurst() {
	Long64_t iSpecialTrigger = fLastSpecialTriggerEvent;
	GetIOTree()->LoadSpecialEvent(iSpecialTrigger);

	EventHeader *stEventHeader = GetIOTree()->GetEventHeaderEvent("SpecialTrigger");
	if(!stEventHeader){
		std::cout << debug() << "No EventHeader for special trigger. Nothing to do here" << std::endl;
		return false; // No EventHeader for special trigger. Nothing to do here
	}
	int currBurstID = GetIOTree()->GetEventHeaderEvent("SpecialTrigger")->GetBurstID();
	int currRunID = GetIOTree()->GetEventHeaderEvent("SpecialTrigger")->GetRunID();

	while( (iSpecialTrigger<GetIOTree()->GetNSpecialEvents()) &&
			(currBurstID==fCurrentBurstNumber) && (currRunID==fCurrentRunNumber) ){

	  //printCurrentSpecialEvent(iSpecialTrigger);

		for (auto analyzer : skip<decltype(fAnalyzerList) >(fAnalyzerList,fFirstAnalyzer)) {
			GetOHandler(fDefaultOStream)->SetOutputFileAsCurrent(analyzer->GetAnalyzerName());
			analyzer->ProcessSpecialTrigger(iSpecialTrigger);
		}

		if (fFiltering){
			for(auto itOStream : fOStream)
				itOStream.second.fOutputHandler->WriteSpecialEvent(GetIOTree()->GetFilterWordRef(), GetIOTree()->GetEnabledTrees(), GetIOTree()->GetReferenceTree());
			GetOHandler(fDefaultOStream)->SetOutputFileAsCurrent();
		}
		GetOHandler(fDefaultOStream)->SetOutputFileAsCurrent();

		if (!GetIOTree()->LoadSpecialEvent(++iSpecialTrigger)){
			std::cout << normal() << "Unable to read special event " << iSpecialTrigger << std::endl;
			continue;
		}

		currBurstID = GetIOTree()->GetEventHeaderEvent("SpecialTrigger")->GetBurstID();
		currRunID = GetIOTree()->GetEventHeaderEvent("SpecialTrigger")->GetRunID();
	}
	fLastSpecialTriggerEvent = iSpecialTrigger;

	return true;
}

void BaseAnalysis::DoStartOfRunActions() {

        // NB: Common StartOfRun actions should be done BEFORE
        // calling the StartOfRun function for all the analyzers!

        // Init BeamParameters for the current run
        BeamParameters::GetInstance()->InitBeamParameters(fIHandler->GetRunID(), fIHandler->GetWithMC());

        // Call StartOfRun for all the analyzers
	for (auto analyzer : skip<decltype(fAnalyzerList) >(fAnalyzerList,fFirstAnalyzer))
		analyzer->StartOfRun();
}

void BaseAnalysis::DoEndOfRunActions() {

        // NB: Common EndOfRun actions should be done AFTER
        // calling the EndOfRun function for all the analyzers!

        // Call EndOfRun for all the analyzers
	for (auto analyzer : skip<decltype(fAnalyzerList) >(fAnalyzerList,fFirstAnalyzer)){
		GetOHandler(fDefaultOStream)->SetOutputFileAsCurrent(analyzer->GetAnalyzerName());
		analyzer->EndOfRun(fFiltering);
	}
}

void BaseAnalysis::DoStartOfBurstActions() {

        // NB: Common StartOfBurst actions should be done BEFORE
        // calling the StartOfBurst function for all the analyzers!

        // Call StartOfBurst for all the analyzers
	for (auto analyzer : skip<decltype(fAnalyzerList) >(fAnalyzerList,fFirstAnalyzer))
		analyzer->StartOfBurst();
}

void BaseAnalysis::DoEndOfBurstActions() {

        // NB: Common EndOfBurst actions should be done AFTER
        // calling the EndOfBurst function for all the analyzers!

        // Call EndOfBurst for all the analyzers
	for (auto analyzer : skip<decltype(fAnalyzerList) >(fAnalyzerList,fFirstAnalyzer)){
		GetOHandler(fDefaultOStream)->SetOutputFileAsCurrent(analyzer->GetAnalyzerName());
		analyzer->EndOfBurst();
	}
}

void BaseAnalysis::SetIsPython(bool isPython){
	fIsPythonFile = isPython;
}

} /* namespace Core */
} /* namespace NA62Analysis */

