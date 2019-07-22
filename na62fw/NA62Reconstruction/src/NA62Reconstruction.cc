/// \mainpage NA62 Framework
/// \htmlinclude basicinstructions.html

/// \class NA62Reconstruction
/// \Brief
/// Main Reconstruction class, which manages the whole reconstruction process.\n
/// An instance of this class is created by NA62Reco.cc or NA62EventDisplay.cc.
/// \EndBrief
///
/// \Detailed
/// It makes use of base classes (NA62VNamedModule, NA62VReconstruction, NA62VDigitizer, NA62VRawDecoder)
/// to control data processing from NA62MC output (Hits) or binary raw files (if the proper raw decoder is
/// implemented) to the requested stage (Digis or Reco). Each submodule is loaded into a library by
/// InitLibraries() and is made available for runtime task order decision.
/// The name extension of the first file to process is used to distinguish between raw data and ROOT files:
///     -       in the first case it tries to find requested raw decoders in the library
///     -       in the second case it tries to retrieve the requested data type (Hits or Digis TBranches) from TTree
///             objects stored in the file, checking for mismatches in number of entries
///     -       upon fail it should throw an exception and exit
///     .
/// It implements also some basic trigger logic, via a common interface defined in NA62VReconstruction.
/// The single event logic is defined by NextEvent(). The task execution and branch selection (Hits or Digis)
/// is controlled by a configuration file (config/NA62Reconstruction.conf), and has some fallback solution.
/// It also initialize the seed for a global random number generator (for digitizers).
/// \n
/// The data persistency is divided in 3 event based stages:
///      -      Hits (native output of NA62MC)
///      -      Digis (produced from binary raw files, by a module inheriting from NA62VRawDecoder,
///             or from NA62MC Hits, by a module inheriting from NA62VDigitizer)
///      -      Reco (result of reconstruction algorithms applied alternatively to Hits o Digis)
///      .
/// Each of them has a common interface (TDetectorVHit, TVDigi, TRecoVHit and related event classes) and
/// the whole chain is linked from the source to the target; this link is made transient at this moment,
/// but it can be easily made persistent, at the cost of disk space.
/// \n
/// End user analysis can be performed on the fly or by reading the output files; in the first case
/// the easiest solution is to fill the empty loop in NA62Reco.cc
/// with your code, and it will take care of the rest. In the second case refer to NA62Analysis package
/// \n
/// An example of what can be used in the main loop:
/// \code
/// while(NA62Reco->NextEvent()){
///  TRecoRICHEvent * RICHEvent = static_cast<TRecoRICHEvent*>(NA62Reco->FindRecoEvent("RICH"));
///  Int_t NRingCandidates = static_cast<TRecoRICHEvent*>( RICHEvent)->GetNRingCandidates();
///  for(Int_t iCandidate = 0; iCandidate < NRingCandidates; iCandidate++){
///    TRecoRICHCandidate * Ring = static_cast<TRecoRICHEvent*>(RICHEvent)->GetRingCandidate(iCandidate);
///    Double_t Radius = Ring->GetRingRadius();
///    Double_t Time = Ring->GetRingTime();
///    TVector2 Center = Ring->GetRingCenter();
///     }
/// }
/// \endcode
/// Please note: the flag fEventQualityMask in the EventHeader contains the information of the
/// subdetectors in which at least a critical error is occurred
/// (bitwise, using the subdetector numbering defined in NA62Global.hh)
/// In order to have all the subdetectors *without* errors you should select those events with fEventQualityMask==0.
/// \todo Separate output and processing stages.
/// \author Antonino Sergi (Antonino.Sergi@cern.ch)
/// \EndDetailed

#include "Riostream.h"
#include "TMath.h"
#include "TSystem.h"
#include "TRegexp.h"
#include "TMath.h"

#include "NA62Reconstruction.hh"
#include "CedarReconstruction.hh"
#include "CedarDigitizer.hh"
#include "CedarRawDecoder.hh"
#include "CedarRawEncoder.hh"
#include "CHANTIReconstruction.hh"
#include "CHANTIDigitizer.hh"
#include "CHANTIRawDecoder.hh"
#include "CHANTIRawEncoder.hh"
#include "CHODReconstruction.hh"
#include "CHODDigitizer.hh"
#include "CHODRawDecoder.hh"
#include "CHODRawEncoder.hh"
#include "GigaTrackerReconstruction.hh"
#include "GigaTrackerDigitizer.hh"
#include "GigaTrackerRawDecoder.hh"
#include "GigaTrackerRawEncoder.hh"
#include "IRCReconstruction.hh"
#include "IRCDigitizer.hh"
#include "IRCRawDecoder.hh"
#include "IRCRawEncoder.hh"
#include "LAVReconstruction.hh"
#include "LAVDigitizer.hh"
#include "LAVRawDecoder.hh"
#include "LAVRawEncoder.hh"
#include "LKrReconstruction.hh"
#include "LKrDigitizer.hh"
#include "LKrRawDecoder.hh"
#include "LKrRawEncoder.hh"
#include "MUV0Reconstruction.hh"
#include "MUV0Digitizer.hh"
#include "MUV0RawDecoder.hh"
#include "MUV0RawEncoder.hh"
#include "MUV1Reconstruction.hh"
#include "MUV1Digitizer.hh"
#include "MUV1RawDecoder.hh"
#include "MUV1RawEncoder.hh"
#include "MUV2Reconstruction.hh"
#include "MUV2Digitizer.hh"
#include "MUV2RawDecoder.hh"
#include "MUV2RawEncoder.hh"
#include "MUV3Reconstruction.hh"
#include "MUV3Digitizer.hh"
#include "MUV3RawDecoder.hh"
#include "MUV3RawEncoder.hh"
#include "NewCHODReconstruction.hh"
#include "NewCHODDigitizer.hh"
#include "NewCHODRawDecoder.hh"
#include "NewCHODRawEncoder.hh"
#include "RICHReconstruction.hh"
#include "RICHDigitizer.hh"
#include "RICHRawDecoder.hh"
#include "RICHRawEncoder.hh"
#include "SACReconstruction.hh"
#include "SACDigitizer.hh"
#include "SACRawDecoder.hh"
#include "SACRawEncoder.hh"
#include "HACReconstruction.hh"
#include "HACDigitizer.hh"
#include "HACRawDecoder.hh"
#include "HACRawEncoder.hh"
#include "SpectrometerReconstruction.hh"
#include "SpectrometerDigitizer.hh"
#include "SpectrometerRawDecoder.hh"
#include "SpectrometerRawEncoder.hh"
#include "SAVReconstruction.hh"
#include "SAVDigitizer.hh"
#include "SAVRawDecoder.hh"
#include "SAVRawEncoder.hh"
#include "GitRevision.hh"
#include "EventHeader.hh"
#include "NA62RecoManager.hh"
#include "TSpecialTriggerEvent.hh"
#include "TCedarSpecialTriggerEvent.hh"
#include "TRecoGigaTrackerHit.hh"
#include "NA62BufferProto.hh"
#include "L0TPRawEncoder.hh"
#include "NA62Global.hh"
#include "NA62ConditionsService.hh"
#include "StringInterpreter.hh"
#include "TSlimRecoVEvent.hh"

#include <sys/stat.h>
#include <unistd.h>

#ifdef ONLINEHLT
//HLT

#include <l0/offline/Subevent.h>
#include <common/decoding/OfflineDecoderHandler.h>
#include <l1/KtagAlgo.h>
#include <l1/StrawAlgo.h>
#include <l1/straw_algorithm/Track.h>
#include <l1/LAVAlgo.h>
#include <l1/L1InfoToStorage.h>
#include <bitset>
#include <dlfcn.h>
#endif

NA62Reconstruction::NA62Reconstruction(TObjArray * InputFileNameList, TString ConfFileName, TFile * OutputFile, Int_t NEvt, Int_t NEvtPerFile, Int_t JumpNEvt, UInt_t Seed) :
  NA62VReconstruction(OutputFile , "NA62",ConfFileName), fHLTLib(nullptr), fInputFileNameList(InputFileNameList), fBinaryEvent(0,0), fNEvt(NEvt), fNEvtPerFile(NEvtPerFile), fJumpNEvt(JumpNEvt), fGlobalSeed(Seed), fTimer(true){

    /// \MemberDescr
    /// *** Standard constructor used in NA62Reco and NA62OnlineMonitor/NA62EventDisplay ***
    /// \param InputFileNameList     It contains the list of file names to process
    /// \param OutputFile            The output file name
    /// \param NEvt                  Number of events to be processed
    /// \param NEvtPerFile           Number of events to be processed per file
    /// \param JumpNEvt              Number of events to be jumped before starting the processing
    /// \param Seed                  The seed of the global random generator (TRandom)

    ResetVariables();
    ParseConfigFile(ConfFileName);
    fContinuousReading = kFALSE; //force continuous reading OFF: continuous reading used only in the other constructor
    Init();
  }

NA62Reconstruction::NA62Reconstruction(TString InputListFileName, TString ConfFileName, TString OutputFileName, Int_t NEvt, Int_t NEvtPerFile, Int_t JumpNEvt, UInt_t Seed, Int_t NFiles) :
  NA62VReconstruction(TFile::Open(OutputFileName.Data(), "RECREATE"), "NA62",ConfFileName), fHLTLib(nullptr), fInputListFileName(InputListFileName), fBinaryEvent(0,0), fNEvt(NEvt), fNEvtPerFile(NEvtPerFile), fJumpNEvt(JumpNEvt), fGlobalSeed(Seed), fTimer(true) {
    /// \MemberDescr
    /// *** Constructor used in NA62OnlineMonitor/NA62EventDisplay for the -l option only ***
    /// \param InputFileNameList     It contains the list of file names to process
    /// \param OutputFile            The output file name
    /// \param NEvt                  Number of event toy be processed
    /// \param NEvtPerFile           Number of events to be processed per file
    /// \param JumpNEvt              Number of event to be jumped before starting the processing
    /// \param Seed                  The seed of the global random generator (TRandom)
    /// \param NFiles                Requested # of files to read

    ResetVariables();
    ParseConfigFile(ConfFileName);
    ReadInputList(InputListFileName, NFiles);
    Init();
  }

NA62Reconstruction::~NA62Reconstruction(){
  if(fMCTruthEvent){
    delete fMCTruthEvent;
    fMCTruthEvent=0;
  }
  if (fStream) {
    delete fStream;
    fStream=0;
  }
  for(Int_t iReco = 0; iReco < fNReconstructions; iReco++)
    if(fReconstructions[iReco])
      delete fReconstructions[iReco];
  for(Int_t iDigi = 0; iDigi < fNDigitizers; iDigi++)
    if(fDigitizers[iDigi])
      delete fDigitizers[iDigi];
  for(Int_t iRaw = 0; iRaw < fNRawDecoders; iRaw++)
    if(fRawDecoders[iRaw])
      delete fRawDecoders[iRaw];
  for(Int_t iBinary = 0; iBinary < fNRawEncoders; iBinary++)
    if(fRawEncoders[iBinary])
      delete fRawEncoders[iBinary];
  if(fGVirtMem) {
    delete fGVirtMem;
    fGVirtMem=0;
  }
  if(fGResMem) {
    delete fGResMem;
    fGResMem=0;
  }
  if(fGFileSize) {
    delete fGFileSize;
    fGFileSize=0;
  }
  if(fGSystemFileSize) {
    delete fGSystemFileSize;
    fGSystemFileSize=0;
  }
  for(Int_t iRaw = 0; iRaw < fNRawDecoders; iRaw++){
    if(fNCriticalErrors[iRaw]){
      delete [] fNCriticalErrors[iRaw];
      fNCriticalErrors[iRaw]=0;
    }
    if(fNQualityWarnings[iRaw]){
      delete [] fNQualityWarnings[iRaw];
      fNQualityWarnings[iRaw]=0;
    }
    if(fNWrongSlots[iRaw]){
      delete [] fNWrongSlots[iRaw];
      fNWrongSlots[iRaw]=0;
    }
    if(fNTotalSlots[iRaw]){
      delete [] fNTotalSlots[iRaw];
      fNTotalSlots[iRaw]=0;
    }
    if(fNHitsFromMaskedChannels[iRaw]){
      delete [] fNHitsFromMaskedChannels[iRaw];
      fNHitsFromMaskedChannels[iRaw]=0;
    }
  }
  if(fEventNumbers){
    delete [] fEventNumbers;
    fEventNumbers=0;
  }
  if(fTriggerTypes){
    delete [] fTriggerTypes;
    fTriggerTypes=0;
  }
  if(fEventOffsets){
    delete [] fEventOffsets;
    fEventOffsets=0;
  }
  if(fEventAlreadyProcessed){
    delete [] fEventAlreadyProcessed;
    fEventAlreadyProcessed=0;
  }
}

void NA62Reconstruction::InitDetectorsInfo() {
  fMainReco = this; //init NA62Reconstruction main reco as itself
  // Set common attributes in each reco class
  for(Int_t iReco=0; iReco<fNReconstructions;iReco++){
    if(FindReco(fRecoSequence[iReco])){
      FindReco(fRecoSequence[iReco])->SetRawDecoder(FindRaw(fRecoSequence[iReco]));
      FindReco(fRecoSequence[iReco])->SetT0ReferenceDetector(fT0ReferenceDetectors[iReco]);
      FindReco(fRecoSequence[iReco])->SetT0NHitsMin(fT0NHitsMinValues[iReco]);
      FindReco(fRecoSequence[iReco])->SetHistosLevel(fHistosLevel);
    }
    // turn warnings OFF if MainReco has warnings disabled
    if(!fWarningsLevel && FindRaw(fRecoSequence[iReco]) && FindRaw(fRecoSequence[iReco])->GetDecoder()) FindRaw(fRecoSequence[iReco])->GetDecoder()->SetWarningsLevel(0);
  }
  // Reconstruction instances
  if(FindReco("Cedar")) static_cast<CedarReconstruction*>(FindReco("Cedar"))->Init(this);
  if(FindReco("CHANTI")) static_cast<CHANTIReconstruction*>(FindReco("CHANTI"))->Init(this);
  if(FindReco("CHOD")) static_cast<CHODReconstruction*>(FindReco("CHOD"))->Init(this);
  if(FindReco("GigaTracker")) static_cast<GigaTrackerReconstruction*>(FindReco("GigaTracker"))->Init(this);
  if(FindReco("HAC")) static_cast<HACReconstruction*>(FindReco("HAC"))->Init(this);
  if(FindReco("IRC")) static_cast<IRCReconstruction*>(FindReco("IRC"))->Init(this);
  if(FindReco("LAV")) static_cast<LAVReconstruction*>(FindReco("LAV"))->Init(this);
  if(FindReco("LKr")) static_cast<LKrReconstruction*>(FindReco("LKr"))->Init(this);
  if(FindReco("MUV0")) static_cast<MUV0Reconstruction*>(FindReco("MUV0"))->Init(this);
  if(FindReco("MUV1")) static_cast<MUV1Reconstruction*>(FindReco("MUV1"))->Init(this);
  if(FindReco("MUV2")) static_cast<MUV2Reconstruction*>(FindReco("MUV2"))->Init(this);
  if(FindReco("MUV3")) static_cast<MUV3Reconstruction*>(FindReco("MUV3"))->Init(this);
  if(FindReco("NewCHOD")) static_cast<NewCHODReconstruction*>(FindReco("NewCHOD"))->Init(this);
  if(FindReco("RICH")) static_cast<RICHReconstruction*>(FindReco("RICH"))->Init(this);
  if(FindReco("SAC")) static_cast<SACReconstruction*>(FindReco("SAC"))->Init(this);
  if(FindReco("Spectrometer")) static_cast<SpectrometerReconstruction*>(FindReco("Spectrometer"))->Init(this);
  if(FindReco("SAV")) static_cast<SAVReconstruction*>(FindReco("SAV"))->Init(this);
  // RawDecoder instances
  if(fIsRawData){
    if(FindRaw("Cedar")) static_cast<CedarRawDecoder*>(FindRaw("Cedar"))->Init();
    if(FindRaw("CHANTI")) static_cast<CHANTIRawDecoder*>(FindRaw("CHANTI"))->Init();
    if(FindRaw("CHOD")) static_cast<CHODRawDecoder*>(FindRaw("CHOD"))->Init();
    if(FindRaw("GigaTracker")) static_cast<GigaTrackerRawDecoder*>(FindRaw("GigaTracker"))->Init();
    if(FindRaw("HAC")) static_cast<HACRawDecoder*>(FindRaw("HAC"))->Init();
    if(FindRaw("IRC")) static_cast<IRCRawDecoder*>(FindRaw("IRC"))->Init();
    if(FindRaw("LAV")) static_cast<LAVRawDecoder*>(FindRaw("LAV"))->Init();
    if(FindRaw("LKr")) static_cast<LKrRawDecoder*>(FindRaw("LKr"))->Init();
    if(FindRaw("MUV0")) static_cast<MUV0RawDecoder*>(FindRaw("MUV0"))->Init();
    if(FindRaw("MUV1")) static_cast<MUV1RawDecoder*>(FindRaw("MUV1"))->Init();
    if(FindRaw("MUV2")) static_cast<MUV2RawDecoder*>(FindRaw("MUV2"))->Init();
    if(FindRaw("MUV3")) static_cast<MUV3RawDecoder*>(FindRaw("MUV3"))->Init();
    if(FindRaw("NewCHOD")) static_cast<NewCHODRawDecoder*>(FindRaw("NewCHOD"))->Init();
    if(FindRaw("RICH")) static_cast<RICHRawDecoder*>(FindRaw("RICH"))->Init();
    if(FindRaw("SAC")) static_cast<SACRawDecoder*>(FindRaw("SAC"))->Init();
    if(FindRaw("Spectrometer")) static_cast<SpectrometerRawDecoder*>(FindRaw("Spectrometer"))->Init();
    if(FindRaw("SAV")) static_cast<SAVRawDecoder*>(FindRaw("SAV"))->Init();
  }
}

void NA62Reconstruction::Init(){
  /// \MemberDescr
  /// It is divided in 3 steps:
  ///     -       a call to InitLibraries() to load all the available algorithms into memory
  ///     -       identification of data type and initialization of input (RawDecoders or TChains)
  ///             and output (Digis or Reco), assigning the requested processing sequence after checking
  ///             for availability in the library
  ///     -       a call to CleanUpLibraries() to remove unused modules from memory
  /// \EndMemberDescr

  EventHeader* EventHeader = NA62RecoManager::GetInstance()->GetEventHeader();
  fDataBuffer.resize(MAXBUFFERWORDS);

  //*****************************************************************************************

  fIsRawData = ! static_cast<TObjString*>(fInputFileNameList->At(0))->GetString().EndsWith(".root");
  NA62RecoManager::GetInstance()->InitNA62ConditionsService(static_cast<TObjString*>(fInputFileNameList->At(0))->GetString());

  if (!fRecoSequence.size()) Exception("No reconstruction sequence defined");

  InitLibraries();
  InitRecoRawTable();
  InitHistograms();

  Int_t NEntries = 0;

  Bool_t NoRawDecoder = (fRawLibrary.size() == 0);
  Bool_t NoRawEncoder = (fRawEncLibrary.size() == 0);

  TTree::SetMaxTreeSize(190000000000);

  fStream = new Stream();
  if(!fIsRawData){
    std::vector<TString> TreeNames;
    TreeNames.push_back("Streams");
    TreeNames.push_back("MC"); //it must be the last!
    if(fRequestedInputStage=="Digis") TreeNames.back() = "Digis";
    fMCTruthEvent = new Event();
    for(UInt_t iTree=0;iTree<TreeNames.size();iTree++){
      fMCChains.push_back(BuildChain(TreeNames[iTree]));
      if(fMCChains[iTree]) {
        TObjArray * Branches = fMCChains[iTree]->GetListOfBranches();
        NEntries = fMCChains[iTree]->GetEntries();
        std::cout << "Found Tree '" << TreeNames[iTree] << "' with " << Branches->GetEntries() << " branches and " << NEntries << " entries" << std::endl;
        for(Int_t iBranch = 0; iBranch < Branches->GetEntries(); iBranch++){
          TString BranchName = static_cast<TBranch*>((*Branches)[iBranch])->GetName();
          TClass * BranchObjectClass = TClass::GetClass(static_cast<TBranch*>((*Branches)[iBranch])->GetClassName());
          std::cout << "Found Branch " << BranchName.Data() << " containing " << BranchObjectClass->GetName() << std::endl;
          if(BranchName=="Stream") {
            fMCChains[iTree]->SetBranchAddress(BranchName.Data(),&fStream);
          }
          else if(BranchName=="Generated") {
            fMCChains[iTree]->SetBranchAddress(BranchName.Data(),&fMCTruthEvent);
          }
        }
        fiFile = -1;
      }
      else{
        Exception("Tree not found");
      }
    }
    // Read the run time (for MC only) and put it to EventHeader::fBurstTime
    fBurstTime = NA62RecoManager::GetInstance()->GetRunTimeFromDB(NA62ConditionsService::GetInstance()->GetCurrentRunID());
    std::cout << "[NA62Reconstruction] Run time found for the run " << NA62ConditionsService::GetInstance()->GetCurrentRunID() << " : Time = " << fBurstTime << std::endl;
  }
  else {
    fNFiles = fInputFileNameList->GetEntries();
    //NextFile(fNEvt-fNProcessedEventsInTotal);
  }

  Int_t GuestDetectorMask = 0;

  if((fOutputStage&kRecoTreeEnabled) || (fOutputStage&kDigiTreeEnabled)){
    fOutputTrees.push_back(new TTree(fOutputStageName.Data(),""));
    fOutputTrees.push_back(new TTree("SpecialTrigger",""));
    fOutputBranchEvents.push_back(new EventVector);
    fOutputBranchEvents.push_back(new EventVector);
    if(fOutputStage&kDigiTreeEnabled) {
      fOutputTrees.push_back(new TTree("Digis","")); //additional digis info for "Reco" OutputStage
      fOutputBranchEvents.push_back(new EventVector);
    }
  }
  fStreamsOutputTree = new TTree("Streams","");

  for(Int_t iReco = 0; iReco < fNInputBranches; iReco++){
    NA62VRawDecoder *RawDecoder = FindRaw(fRecoSequence[iReco]);
    Int_t iRaw = -1;
    for (UInt_t iTable=0; iTable<MAXNDETECTORS; iTable++)
      if (fRecoSequence[iReco]==fRecoRawTable[iTable]) iRaw = iTable;
    if (iRaw!=-1) {
      fRawRecoIndex[iRaw] = iReco;
      if(RawDecoder){
        fRawDecoders[iRaw] = RawDecoder;
        fNRawDecoders++;
        std::cout << "Attaching and resetting RawDecoder for " << fRecoSequence[iReco].Data() << std::endl;
      }
      else {
        std::cout << fRecoSequence[iReco].Data() << ": Raw decoding not supported" << std::endl;
        fRawDecoders[iRaw] = 0;
        NoRawDecoder = kTRUE;
      }
    }
    else Exception("Detector '"+fRecoSequence[iReco]+"' not found in fRecoRawTable");
    if(!fIsRawData && !FindMCChain("MC"))
      Exception("Tree not found");
    else if(!fIsRawData && FindMCChain("MC") && !FindMCChain("MC")->GetBranch(fRecoSequence[iReco]))
      Exception(Form("Branch %s not found", fRecoSequence[iReco].Data()));
    else if(fIsRawData && NoRawDecoder)
      Exception("Raw decoder not available");
    else {
      Bool_t ProcessingSupported = kTRUE;
      NA62VReconstruction* Reco   = FindReco(fRecoSequence[iReco]);
      NA62VDigitizer*  Digi       = FindDigi(fRecoSequence[iReco]);
      NA62VRawEncoder* RawEncoder = FindBinary(fRecoSequence[iReco]);
      if(!RawEncoder || NoRawEncoder)
        Exception("Binary Encoder not available");
      if(Reco && Digi && (!fIsRawData || (RawDecoder && fIsRawData) || (RawEncoder && fIsRawData))){
        fReconstructions.push_back(Reco);
        fDigitizers.push_back(Digi);
        fRawEncoders.push_back(RawEncoder);
        std::cout << "Attaching Reconstruction, Digitization and Encoding for " << fRecoSequence[iReco].Data() << std::endl;
        // retrieve mask for guest detectors
        UInt_t NGuestsInPreviousHosts = 0;
        for (UInt_t iHost=0; iHost < fHostsForGuestDetectors.size(); iHost++) {
          for (UInt_t iGuest=0; iGuest < fGuestDetectorNames[iHost].size(); iGuest++) {
            if (fRecoSequence[iReco] == fGuestDetectorNames[iHost][iGuest]) GuestDetectorMask |= (1<<(iGuest+NGuestsInPreviousHosts));
          }
          NGuestsInPreviousHosts += fGuestDetectorNames[iHost].size();
        }
      }
      else {
        std::cout << fRecoSequence[iReco].Data() << ": Processing not supported" << std::endl;
        ProcessingSupported = kFALSE;
      }
      if(ProcessingSupported){
        std::cout << "Preparing I/O data structures for " << fRecoSequence[iReco].Data() << std::endl;
        fInputEvents.push_back(nullptr);
        fEvents.push_back(nullptr);
        fRecoEvents.push_back(nullptr);
        fSlimRecoEvents.push_back(nullptr);
        TDigiVEvent * DigiEvent = Digi->GetDigiEvent();
        if(fIsRawData) DigiEvent = RawDecoder->GetDigiEvent();
        if(Reco->GetRecoEvent()) Reco->GetRecoEvent()->SetDigiEvent(DigiEvent);
        if(fOutputStage == kStageDigis && DigiEvent){
          if(FindOutputBranchEvent(fOutputStageName.Data())) FindOutputBranchEvent(fOutputStageName.Data())->push_back(DigiEvent);
          if(FindOutputBranchEvent("SpecialTrigger")){
            if(RawDecoder) FindOutputBranchEvent("SpecialTrigger")->push_back(RawDecoder->GetSpecialTriggerEvent());
            else           FindOutputBranchEvent("SpecialTrigger")->push_back(nullptr);
          }
          fEvents.back() = DigiEvent;
          std::cout << "Selected Digis for output of " << fRecoSequence[iReco].Data() << std::endl;
        }
        else if(fOutputStage == kStageRawHistos && DigiEvent){
          fEvents.back() = DigiEvent;
          std::cout << "Selected RawHistos for output of " << fRecoSequence[iReco].Data() << std::endl;
        }
        else if(fOutputStage == kStageSlimReco && Reco->GetSlimRecoEvent()){
          if(FindOutputBranchEvent(fOutputStageName.Data())) FindOutputBranchEvent(fOutputStageName.Data())->push_back(Reco->GetRecoEvent()); // this should be GetSlimRecoEvent()!
          if(FindOutputBranchEvent("SpecialTrigger")){
            if(RawDecoder) FindOutputBranchEvent("SpecialTrigger")->push_back(RawDecoder->GetSpecialTriggerEvent());
            else           FindOutputBranchEvent("SpecialTrigger")->push_back(nullptr);
          }
          fRecoEvents.back() = Reco->GetRecoEvent();
          fSlimRecoEvents.back() = Reco->GetSlimRecoEvent();
          std::cout << "Selected SlimReco for output of " << fRecoSequence[iReco].Data() << std::endl;
        }
        else if((fOutputStage&kRecoTreeEnabled) && Reco->GetRecoEvent()){
          if(FindOutputBranchEvent(fOutputStageName.Data())) FindOutputBranchEvent(fOutputStageName.Data())->push_back(Reco->GetRecoEvent());
          if(FindOutputBranchEvent("SpecialTrigger")){
            if(RawDecoder) FindOutputBranchEvent("SpecialTrigger")->push_back(RawDecoder->GetSpecialTriggerEvent());
            else           FindOutputBranchEvent("SpecialTrigger")->push_back(nullptr);
          }
          if((fOutputStage&kDigiTreeEnabled) && FindOutputBranchEvent("Digis")) FindOutputBranchEvent("Digis")->push_back(Reco->GetRecoEvent()->GetDigiEvent());
          fRecoEvents.back() = Reco->GetRecoEvent();
          std::cout << "Selected Reco for output of " << fRecoSequence[iReco].Data() << std::endl;
        }
        else if(fOutputStage == kStageHistos && Reco->GetRecoEvent()){
          fRecoEvents.back()  = Reco->GetRecoEvent();
          std::cout << "Selected Histos for output of " << fRecoSequence[iReco].Data() << std::endl;
        }
        else if(fOutputStage == kStageBinary && DigiEvent){
          fEvents.back() = DigiEvent;
          std::cout << "Selected Binary for output of " << fRecoSequence[iReco].Data() << std::endl;
        }
        else if(fOutputStage == kStageNone){
          std::cout << "Output and processing disabled for " << fRecoSequence[iReco].Data() << std::endl;
        }
        else{
          Exception(Form("%s not supported for %s",fOutputStageName.Data(), fRecoSequence[iReco].Data()));
        }
      }
      else
        std::cout << fRecoSequence[iReco].Data() << ": no data available" << std::endl;
    }
  }

  // Enabling L0TPRawEncoder
  NA62VRawEncoder * RawEncoder = FindBinary("L0TP");
  fRawEncoders.push_back(RawEncoder);

  UInt_t NGuestsInPreviousHosts = 0;
  for (UInt_t iHost=0; iHost < fHostsForGuestDetectors.size(); iHost++) {
    // Disable unrequested guest detectors
    for (UInt_t iGuest=0; iGuest<fGuestDetectorIndices[iHost].size(); iGuest++) {
      if (!(GuestDetectorMask & (1<<(iGuest+NGuestsInPreviousHosts)))) {
        fGuestDetectorIndices[iHost][iGuest]=-1;
      }
    }

    NGuestsInPreviousHosts += fGuestDetectorIndices[iHost].size();

    // Clean up guest detector vectors
    for (UInt_t iGuest=0; iGuest<fGuestDetectorIndices[iHost].size(); iGuest++) {
      if(fGuestDetectorIndices[iHost][iGuest]==-1) {
        RemoveGuestDetector(iHost,iGuest);
        iGuest--;
      }
    }
    if(fGuestDetectorNames[iHost].size()){
      std::cout << "--> Will add to the digi-reco chain " << fGuestDetectorNames[iHost].size() << " detectors:";
      for (UInt_t iGuest=0; iGuest<fGuestDetectorNames[iHost].size(); iGuest++) std::cout << " " << fGuestDetectorNames[iHost][iGuest];
      std::cout << " [Host Detector: " << fHostsForGuestDetectors[iHost] << "]" << std::endl;
    }
  }

  if(((fOutputStage&kRecoTreeEnabled) || (fOutputStage&kDigiTreeEnabled)) && FindOutputTree(fOutputStageName)){
    FindOutputTree(fOutputStageName)->SetDirectory(fHistoFile->GetDirectory("/"));
    FindOutputTree(fOutputStageName)->Branch("EventHeader","EventHeader", EventHeader)->SetAutoDelete(kFALSE);
    if(!fIsRawData) FindOutputTree(fOutputStageName)->Branch("Generated", "Event", &fMCTruthEvent)->SetAutoDelete(kFALSE);
    FindOutputTree(fOutputStageName)->Branch("L0TP", "L0TPData",  EventHeader->GetL0TPData())->SetAutoDelete(kFALSE);
    FindOutputTree(fOutputStageName)->Branch("L1TP", "L1TPData",  EventHeader->GetL1TPData())->SetAutoDelete(kFALSE);
    FindOutputTree(fOutputStageName)->Branch("L2EB", "L2EBData",  EventHeader->GetL2EBData())->SetAutoDelete(kFALSE);
    FindOutputTree(fOutputStageName)->Branch("Beam", "BeamData",  EventHeader->GetBeamData())->SetAutoDelete(kFALSE);
#ifdef ONLINEHLT
    FindOutputTree(fOutputStageName)->Branch("HLT",  "HLTEvent",  EventHeader->GetHLTEvent())->SetAutoDelete(kFALSE);
#endif
    FindOutputTree("SpecialTrigger")->SetDirectory(fHistoFile->GetDirectory("/"));
    FindOutputTree("SpecialTrigger")->Branch("EventHeader","EventHeader", EventHeader)->SetAutoDelete(kFALSE);
    FindOutputTree("SpecialTrigger")->Branch("L0TP", "L0TPSpecialTrigger",  EventHeader->GetL0TPSpecialTrigger())->SetAutoDelete(kFALSE);
    FindOutputTree("SpecialTrigger")->Branch("L1TP", "L1TPSpecialTrigger",  EventHeader->GetL1TPSpecialTrigger())->SetAutoDelete(kFALSE);
    FindOutputTree("SpecialTrigger")->Branch("L2EB", "L2EBSpecialTrigger",  EventHeader->GetL2EBSpecialTrigger())->SetAutoDelete(kFALSE);
    FindOutputTree("SpecialTrigger")->Branch("Beam", "BeamSpecialTrigger",  EventHeader->GetBeamSpecialTrigger())->SetAutoDelete(kFALSE);
  }
  if (fStreamsOutputTree) {
    fStreamsOutputTree->SetDirectory(fHistoFile->GetDirectory("/"));
    fStreamsOutputTree->Branch("Stream", fStream->IsA()->GetName(), &fStream)->SetAutoDelete(kFALSE);
  }

  fNReconstructions = fReconstructions.size();
  fNDigitizers = fDigitizers.size();
  fNRawEncoders = fRawEncoders.size();

  //INIT DETECTOR INFOs
  InitDetectorsInfo();

  if(!fIsRawData) std::cout << fNInputBranches << " input branches selected and " << NEntries << " entries available " << std::endl;
  std::cout << fNReconstructions << " reconstructions enabled" << std::endl;
  std::cout << fNDigitizers << " digitizers enabled" << std::endl;
  std::cout << fNRawDecoders << " raw decoders enabled" << std::endl;
  std::cout << fNRawEncoders << " raw encoders enabled" << std::endl;
  fNEvt = (NEntries > fNEvt || fIsRawData ? fNEvt : NEntries);
  fDisplayPeriod = (Int_t)TMath::Power(10,(Int_t)TMath::Log10((fNEvt > 10 ? fNEvt/10 : 1)));
  fDisplayPeriod = fDisplayPeriod > 100000 ? 100000 : fDisplayPeriod;
  fDisplayPeriod = fIsRawData ? 1000 : fDisplayPeriod;
  if(fOutputStage==kStageBinary){
    for(Int_t iBinary = 0; iBinary < fNRawEncoders; iBinary++){
      fRawEncoders[iBinary]->Init();
    }
  }

  //initialize NWrongSlots and NTotalSlots arrays (for debug)
  for(Int_t iRaw=0;iRaw<MAXNDETECTORS;iRaw++){
    if(fRawDecoders[iRaw] && fRawDecoders[iRaw]->GetDecoder() && fRawDecoders[iRaw]->GetDecoder()->GetNROMezzanines()){
      fNCriticalErrors[iRaw] = new Int_t[fRawDecoders[iRaw]->GetDecoder()->GetNROMezzanines()];
      fNQualityWarnings[iRaw] = new Int_t[fRawDecoders[iRaw]->GetDecoder()->GetNROMezzanines()];
      fNWrongSlots[iRaw] = new Int_t[fRawDecoders[iRaw]->GetDecoder()->GetNROMezzanines()];
      fNTotalSlots[iRaw] = new Int_t[fRawDecoders[iRaw]->GetDecoder()->GetNROMezzanines()];
      fNHitsFromMaskedChannels[iRaw] = new Int_t[fRawDecoders[iRaw]->GetDecoder()->GetNROMezzanines()];
    }
  }

  InitTimers();
}

void NA62Reconstruction::ReadInputList(TString InputListFileName, Int_t NFiles){
  std::cout << "Reading Input List " << InputListFileName << ".. ";
  if(fInputFileNameList) std::cout << fInputFileNameList->GetEntries() << " files found";
  std::cout << std::endl;
  Bool_t New = kFALSE;
  if(!fInputFileNameList){
    fInputFileNameList = new TObjArray();
    New = kTRUE;
  }
  else
    fInputFileNameList->Clear();

  TString InputFileName;
  Int_t iFile = 0;
  struct stat filestat;
  Int_t Counter = 0;
  char Roll[4] = {'|','/','-','\\'};
  Int_t goodlist_flag = -1;  //-1 bad, 0 good
  while((goodlist_flag != 0 && fInputFileNameList->GetEntries() == 0) || New){
    New = kFALSE;
    goodlist_flag = stat(Form(InputListFileName.Data()), &filestat); //-1 bad, 0 good
    if(fContinuousReading){
      //cout << "Waiting for a valid List File to be ready (" << fInputFileNameList->GetEntries() << ") " << Roll[Counter%2] << "\r" << flush;
      std::cout << "Waiting for a valid List File to be ready " << Roll[Counter%4] << "\r" << flush;
      Counter++;
      gSystem->Sleep(500);
    }
    if(stat(Form(InputListFileName.Data()), &filestat) == 0){
      std::ifstream InputList(InputListFileName.Data());
      while(InputFileName.ReadLine(InputList) && (iFile < NFiles || NFiles == -1)){
        //      if(stat(Form(InputFileName.Data()), &filestat) == 0)
        fInputFileNameList->Add(new TObjString(InputFileName.Data()));
        iFile++;
      }
      InputList.close();
      if(fContinuousReading){
        unlink(Form(InputListFileName.Data()));
      }
    }
    if(!fContinuousReading)
      break;
  }

  if(fInputFileNameList->GetEntries() == 0) {
    perror(Form("Input File"));
    exit(kGenericError);
  }
  fiFile = 0;
  fNFiles = fInputFileNameList->GetEntries();
}

void NA62Reconstruction::ParseConfigFile(TString ConfFileName) {
  /// \MemberDescr
  /// The configuration file name is hardcoded to be config/NA62Reconstruction.conf.
  /// It is self documented by comments, which are supported by the parser
  /// \EndMemberDescr

  std::ifstream confFile(ConfFileName.Data());
  TString Line;

  if(!confFile.is_open()) {
    perror(Form("Configuration File : %s",ConfFileName.Data()));
    exit(kWrongConfiguration);
  }

  while(Line.ReadLine(confFile)) {

    if (Line.BeginsWith("#")) continue; /* comment line */

    if(Line.BeginsWith("CedarConfigFile=")) {
      TObjArray * line = Line.Tokenize(" ");
      fCedarConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("CHANTIConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fCHANTIConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("CHODConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fCHODConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("GigaTrackerConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fGigaTrackerConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("IRCConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fIRCConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("LAVConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fLAVConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("LKrConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fLKrConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("MUV0ConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fMUV0ConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("MUV1ConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fMUV1ConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("MUV2ConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fMUV2ConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("MUV3ConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fMUV3ConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("NewCHODConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fNewCHODConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("RICHConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fRICHConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("SACConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fSACConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("HACConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fHACConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("SpectrometerConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fSpectrometerConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("SAVConfigFile=")){
      TObjArray * line = Line.Tokenize(" ");
      fSAVConfigFileName = StringInterpreter(static_cast<TObjString*>(line->At(1))->GetString());
      delete line;
    }
    else if(Line.BeginsWith("EvaluateTriggerDriftT0")){
      Line.Remove(21,1);
      fEvaluateTriggerDriftT0 = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if(Line.BeginsWith("TriggerDriftT0FileInput=")){
      TObjArray * line = Line.Tokenize(" ");
      fTriggerDriftT0FileInput = static_cast<TObjString*>(line->At(1))->GetString();
      delete line;
    }
    else if(Line.BeginsWith("DnsServerName=")){
      TObjArray * line = Line.Tokenize(" ");
      fDnsServerName = static_cast<TObjString*>(line->At(1))->GetString();
      delete line;
    }
    else if (Line.BeginsWith("DIMRecoveryFileName=")) {
      TObjArray * line = Line.Tokenize(" ");
      if (line->GetEntries()>1) {
        fDIMRecoveryFileName = static_cast<TObjString*>(line->At(1))->GetString();
        fDIMRecoveryMode = kTRUE;
      }
      delete line;
    }
    else if (Line.BeginsWith("RecoSequence=")) {
      TObjArray * line = Line.Tokenize(" ");
      fNInputBranches = line->GetEntries() - 1;
      fRecoSequence.clear();
      for(Int_t iReco = 1; iReco < line->GetEntries(); iReco++) {
        fRecoSequence.push_back(static_cast<TObjString*>(line->At(iReco))->GetString());
      }
      delete line;
    }
    else if (Line.BeginsWith("RequestedInputStage=")) {
      TObjArray * line = Line.Tokenize(" ");
      fRequestedInputStage = static_cast<TObjString*>(line->At(1))->GetString();
      delete line;
    }
    else if (Line.BeginsWith("OutputStage=")) {
      TObjArray * line = Line.Tokenize(" ");
      fOutputStageName = static_cast<TObjString*>(line->At(1))->GetString();
      if (fOutputStageName.CompareTo("Digis") == 0){
        fOutputStage = kStageDigis;
      }
      else if (fOutputStageName.CompareTo("RawHistos") == 0){
        fOutputStage = kStageRawHistos;
      }
      else if (fOutputStageName.CompareTo("Reco") == 0){
        fOutputStage = kStageReco;
      }
      else if (fOutputStageName.CompareTo("SlimReco") == 0){
        fOutputStage = kStageSlimReco;
      }
      else if (fOutputStageName.CompareTo("RecoDigis") == 0){
        fOutputStage = kStageRecoDigis;
      }
      else if (fOutputStageName.CompareTo("Histos") == 0){
        fOutputStage = kStageHistos;
      }
      else if (fOutputStageName.CompareTo("Binary") == 0){
        fOutputStage = kStageBinary;
      }
      else
        fOutputStage = kStageNone;
      delete line;
    }
    else if (Line.BeginsWith("RequestedTriggersFlag=")) {
      Line.Remove(0,21);
      fRequestedTriggersFlag = strtol(TString(Line(TRegexp("[0-9a-fA-F]+"))),NULL,16);
    }
    else if (Line.BeginsWith("ContinuousReading=")) {
      fContinuousReading = TString(Line(TRegexp("[01]"))).Atoi();
    }
    else if (Line.BeginsWith("SkipEventsLevel=")) {
      fSkipEventsLevel = TString(Line(TRegexp("[0-9]"))).Atoi();
    }
    else if (Line.BeginsWith("SkipEventsMask=")) {
      Line.Remove(0,14);
      fSkipEventsMask = strtol(TString(Line(TRegexp("[0-9a-fA-F]+"))),NULL,16);
    }
    else if (Line.BeginsWith("WarningsLevel=")) {
      fWarningsLevel = TString(Line(TRegexp("[0-9]"))).Atoi();
    }
    else if (Line.BeginsWith("HistosLevel")) {
      fHistosLevel = TString(Line(TRegexp("[0-9]"))).Atoi();
      continue;
    }
    else if(Line.BeginsWith("SkippedFileName=")) {
      TObjArray * line = Line.Tokenize(" ");
      fSkippedFileName = static_cast<TObjString*>(line->At(1))->GetString();
      delete line;
    }
    else if(Line.BeginsWith("SvcClass=")) {
      TObjArray * line = Line.Tokenize(" ");
      fSvcClass = static_cast<TObjString*>(line->At(1))->GetString();
      delete line;
    }
    else if (Line.BeginsWith("EOBFileEnabled=")) {
      fEOBFileEnabled = TString(Line(TRegexp("[01]"))).Atoi();
    }
    else if (Line.BeginsWith("LogFileEnabled=")) {
      fLogFileEnabled = TString(Line(TRegexp("[01]"))).Atoi();
    }
    else if (Line.BeginsWith("T0ReferenceDetector=")) {
      TObjArray * line = Line.Tokenize(" ");
      Int_t NEntries = line->GetEntries() - 1;
      if(NEntries!=fNInputBranches) {
        std::cerr << "[NA62Reconstruction] WARNING: " << NEntries << " T0ReferenceDetector(s) for " << fNInputBranches << " detector(s)";
        if(NEntries>fNInputBranches) {
          std::cerr << " Ignoring last " << NEntries-fNInputBranches << " occurrence(s) in T0ReferenceDetectors" << std::endl;
        }
        else {
          std::cerr << " Using '" << static_cast<TObjString*>(line->At(1))->GetString() << "' for last " << fNInputBranches-NEntries << " detector(s)" << std::endl;
          for(Int_t i=0;i<fNInputBranches-NEntries;i++) {
            TObject * obj = line->At(1)->Clone();
            line->Add(obj);
          }
        }
        NEntries = fNInputBranches;
      }
      for(Int_t iReco = 1; iReco <= NEntries; iReco++) {
        fT0ReferenceDetectors.push_back(static_cast<TObjString*>(line->At(iReco))->GetString());
      }
      delete line;
    }
    else if (Line.BeginsWith("T0NHitsMin=")) {
      TObjArray * line = Line.Tokenize(" ");
      Int_t NEntries = line->GetEntries() - 1;
      if(NEntries!=fNInputBranches) {
        std::cerr << "[NA62Reconstruction] WARNING: " << NEntries << " T0NHitsMin for " << fNInputBranches << " detectors";
        if(NEntries>fNInputBranches) {
          std::cerr << " Ignoring last " << NEntries-fNInputBranches << " occurrences in T0NHitsMin" << std::endl;
        }
        else {
          std::cerr << " Using '" << static_cast<TObjString*>(line->At(1))->GetString() << "' for last " << fNInputBranches-NEntries << " detectors.." << std::endl;
          for(Int_t i=0;i<fNInputBranches-NEntries;i++) {
            TObject * obj = line->At(1)->Clone();
            line->Add(obj);
          }
        }
        NEntries = fNInputBranches;
      }
      for(Int_t iReco = 1; iReco <= NEntries; iReco++) {
        fT0NHitsMinValues.push_back(static_cast<TObjString*>(line->At(iReco))->GetString().Atoi());
      }
      delete line;
    }
    else if (Line.BeginsWith("DownscalingFactor=")) {
      fDownscalingFactor = TString(Line(TRegexp("[0-9]+"))).Atoi();
      if(fDownscalingFactor<=1) fDownscalingFactor = 1;
      continue;
    }
    else if (Line.BeginsWith("OnlineMonitorMode=")) {
      TObjArray * line = Line.Tokenize(" ");
      TString OnlineMonitorModeString = static_cast<TObjString*>(line->At(1))->GetString();
      if(OnlineMonitorModeString.CompareTo("Shifter")==0)     fOnlineMonitorMode = kShifter;
      else if(OnlineMonitorModeString.CompareTo("Expert")==0) fOnlineMonitorMode = kExpert;
      else std::cerr << "WARNING: Invalid OnlineMonitorMode '" << OnlineMonitorModeString << "'! Using default.. [=Shifter]" << std::endl;
      delete line;
      continue;
    }
    else if (Line.BeginsWith("SaveOnlineMonitorPlots=")) {
      fSaveOnlineMonitorPlots = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("SaveOnlineMonitorPlotsDir=")) {
      TObjArray * line = Line.Tokenize(" ");
      fSaveOnlineMonitorPlotsDir = static_cast<TObjString*>(line->At(1))->GetString();
      delete line;
      continue;
    }
    else if (Line.BeginsWith("OnlineMonitorReferenceFile=")) {
      TObjArray * line = Line.Tokenize(" ");
      fOnlineMonitorReferenceFileName = static_cast<TObjString*>(line->At(1))->GetString();
      delete line;
      continue;
    }
    else if (Line.BeginsWith("HostDetectors=")) {
      TObjArray * line = Line.Tokenize(" ");
      Int_t NEntries = line->GetEntries() - 1;
      for(Int_t iEntry = 1; iEntry <= NEntries; iEntry++) {
        fHostsForGuestDetectors.push_back(static_cast<TObjString*>(line->At(iEntry))->GetString());
        std::cout<<"Found Host detector "<<fHostsForGuestDetectors.back().Data()<<std::endl;
      }
      fGuestDetectorNames.resize(fHostsForGuestDetectors.size());
      delete line;
      continue;
    }
    else if (Line.BeginsWith("GuestDetectors")) {
      for(UInt_t iHost=0;iHost<fHostsForGuestDetectors.size();iHost++){
        if(Line.BeginsWith(Form("GuestDetectors_%02d=",iHost))){
          if(fGuestDetectorNames.size()<=iHost) {
            std::cerr << "[NA62Reconstruction] WARNING: line starting with '" << Form("GuestDetectors_%02d=",iHost) << "' found but only " << fGuestDetectorNames.size() << " Host detectors declared! Ignoring line.. " << std::endl;
            continue;
          }
          else{
            std::cout<<"Found line with guest detectors for "<<fHostsForGuestDetectors[iHost]<<std::endl;
          }
          TObjArray * line = Line.Tokenize(" ");
          Int_t NEntries = line->GetEntries() - 1;
          fGuestDetectorNames[iHost].clear();
          for(Int_t iEntry = 1; iEntry <= NEntries; iEntry++) {
            fGuestDetectorNames[iHost].push_back(static_cast<TObjString*>(line->At(iEntry))->GetString());
            std::cout<<"Added guest detector "<<fGuestDetectorNames[iHost].back().Data()<<std::endl;
          }
          delete line;
          continue;
        }
      }
    }
    else if (Line.BeginsWith("DataFormat=")) {
      fDataFormat = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("L0TPFineTimeBit=")) {
      Line.Remove(0,4);
      fL0TPFineTimeBit = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("ReferenceDetector=")) {
      TObjArray * line = Line.Tokenize(" ");
      fReferenceDetectorName = static_cast<TObjString*>(line->At(1))->GetString();
      delete line;
      continue;
    }
    else if (Line.BeginsWith("ReferenceDetectorNHitsMin=")) {
      fReferenceDetectorNHitsMin = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("CheckErrno=")) {
      fCheckErrno = TString(Line(TRegexp("[01]"))).Atoi();
    }
  }
  confFile.close();
}

TChain * NA62Reconstruction::BuildChain(TString TreeName){
  /// \MemberDescr
  /// \param TreeName       Name of subdetector TTree to look for in the files
  /// \return               The pointer to the TChain or 0 if no data is found.
  ///
  /// It adds all file names in the provided list fInputFileNameList to a TChain
  /// and checks if there is any data or not.
  /// \EndMemberDescr

  TChain * Chain = new TChain(TreeName.Data());
  for(Int_t iFile = 0; iFile < fInputFileNameList->GetEntries(); iFile++)
    Chain->AddFile(CheckProtocols(static_cast<TObjString*>(fInputFileNameList->At(iFile))->GetString()));
  if(Chain->GetEntries() == 0){
    delete Chain;
    Chain = 0;
  }
  return Chain;
}

void NA62Reconstruction::InitHistograms() {
  fGVirtMem = new TGraph();
  fGVirtMem->SetName("VirtualMemory");
  fGVirtMem->SetTitle("Virtual Memory Usage");
  fGVirtMem->GetXaxis()->SetTitle("Processed Events");
  fGVirtMem->GetYaxis()->SetTitle("Virtual Memory (MB)");

  fGResMem = new TGraph();
  fGResMem->SetName("ResidentMemory");
  fGResMem->SetTitle("Resident Memory Usage");
  fGResMem->GetXaxis()->SetTitle("Processed Events");
  fGResMem->GetYaxis()->SetTitle("Resident Memory (MB)");

  fGFileSize = new TGraph();
  fGFileSize->SetName("FileSize");
  fGFileSize->SetTitle("File size");
  fGFileSize->GetXaxis()->SetTitle("Processed Events");
  fGFileSize->GetYaxis()->SetTitle("File size (MB)");

  fGSystemFileSize = new TGraph();
  fGSystemFileSize->SetName("SystemFileSize");
  fGSystemFileSize->SetTitle("System file size");
  fGSystemFileSize->GetXaxis()->SetTitle("Processed Events");
  fGSystemFileSize->GetYaxis()->SetTitle("File size (MB)");

  fHNEventsProcessedPerBurst = new TH1D("EventsPerBurst", "EventsPerBurst", 3000, -0.5, 2999.5);
  fHNEventsProcessedPerBurst->GetXaxis()->SetTitle("Burst ID");
  fHEventSize = new TH1D("EventSize", "EventSize", 5000, -0.05, 199.95);
  fHEventSize->GetXaxis()->SetTitle("Event size [KB]");

  // Event timestamp monitoring
  fHEventTimeStamp = new TH1D("EventTimeStamp", "EventTimeStamp", 1400, -0.0025e9, 6.9975e9);
  fHEventTimeStamp->GetXaxis()->SetTitle("TimeStamp [ns]");
  fHSkippedEventTimeStamp = new TH1D("SkippedEventTimeStamp", "SkippedEventTimeStamp", 1400, -0.0025e9, 6.9975e9);
  fHSkippedEventTimeStamp->GetXaxis()->SetTitle("TimeStamp [ns]");
  fHNCriticalEventsPerDetector = new TH1D("NCriticalEventsPerDetector", "NCriticalEventsPerDetector", MAXNDETECTORS, -0.5, MAXNDETECTORS-0.5);
  fHNEventsWithQualityWarningsPerDetector = new TH1D("NEventsWithQualityWarningsPerDetector", "NEventsWithQualityWarningsPerDetector", MAXNDETECTORS, -0.5, MAXNDETECTORS-0.5);
  for(Int_t iRaw=0;iRaw<MAXNDETECTORS;iRaw++){
    fHNCriticalEventsPerDetector->GetXaxis()->SetBinLabel(iRaw+1,fRecoRawTable[iRaw]);
    fHNEventsWithQualityWarningsPerDetector->GetXaxis()->SetBinLabel(iRaw+1,fRecoRawTable[iRaw]);
  }
  fHDeltaTimeStamp = new TH1D("DeltaTimeStamp", "DeltaTimeStamp", 1000, -5, 99995);
  fHDeltaTimeStamp->GetXaxis()->SetTitle("TimeStamp - Previous TimeStamp [ns]");
  fHDeltaTimeStampStored = new TH1D("DeltaTimeStampStored", "DeltaTimeStampStored", 1000, -5, 99995);
  fHDeltaTimeStampStored->GetXaxis()->SetTitle("TimeStamp - Previous TimeStamp [ns]");

  fHEventTimeStampBits = new TH1D("EventTimeStampBits", "EventTimeStampBits", 32, -31.5, 0.5);
  fHEventTimeStampBits->GetXaxis()->SetTitle("Event TimeStamp Bits");
  fHEventTimeStampBits->Sumw2() ;

  fHEventTimeStamp16  = new TH1D("EventTimeStamp16", "EventTimeStamp16", 16, -0.5, 15.5);
  fHEventTimeStamp16->GetXaxis()->SetTitle("TimeStamp mod 16");

  fHEventTimeStamp128 = new TH1D("EventTimeStamp128", "EventTimeStamp128", 128, -0.5, 127.5);
  fHEventTimeStamp128->GetXaxis()->SetTitle("TimeStamp mod 128");

  fHEventTimeStamp1024 = new TH1D("EventTimeStamp1024", "EventTimeStamp1024", 256, -0.5, 1023.5);
  fHEventTimeStamp1024->GetXaxis()->SetTitle("TimeStamp mod 1024");

  //L0TP monitoring
  fHL0TriggerFlags = new TH2F("L0TriggerFlags", "L0TriggerFlags", 32, -0.5, 31.5, 32, -0.5, 31.5);
  fHL0TriggerFlags->GetXaxis()->SetTitle("L0 Trigger bits");
  fHL0TriggerFlags->GetYaxis()->SetTitle("L0 Data types");

  //L1TP monitoring
  fHL1TriggerFlags = new TH2F("L1TriggerFlags", "L1TriggerFlags", 16, -0.5, 15.5, 8, -0.5, 7.5);
  fHL1TriggerFlags->GetXaxis()->SetTitle("L0 Trigger bits");
  //fHL1TriggerFlags->GetYaxis()->SetTitle("L1 Trigger bits");
  for(Int_t iBit=0;iBit<fHL1TriggerFlags->GetNbinsY();iBit++){
    fHL1TriggerFlags->GetYaxis()->SetBinLabel(iBit+1,GetL1TriggerType(iBit).Data());
  }
  fHL1Counters = new TH1F("L1Counters", "L1Counters", kL1APPhysicsPassedPerMask+3*NMAXL0MASKS,0.5,kL1APPhysicsPassedPerMask+3*NMAXL0MASKS+0.5);
  fHL1Counters->GetXaxis()->SetBinLabel(kL1APTotal,               "APTotal"               );
  fHL1Counters->GetXaxis()->SetBinLabel(kL1APControl,             "APControl"             );
  fHL1Counters->GetXaxis()->SetBinLabel(kL1APPeriodics,           "APPeriodics"           );
  fHL1Counters->GetXaxis()->SetBinLabel(kL1APPhysicsPassed,       "APPhysicsPassed"       );
  fHL1Counters->GetXaxis()->SetBinLabel(kL1APPhysics,             "APPhysics"             );
  fHL1Counters->GetXaxis()->SetBinLabel(kL1APPhysicsPassedPerMask,"APPhysicsPassedPerMask");
  fHL1Counters->GetXaxis()->SetBinLabel(kL1APPhysicsPerMask,      "APPhysicsPerMask"      );
  fHL1Counters->GetXaxis()->SetBinLabel(kL1PhysicsPassedPerMask,  "PhysicsPassedPerMask"  );

  fHTimingProfile = 0;
  fHTiming2D = 0; // do not know size yet, set on first fill
}

void NA62Reconstruction::InitLibraries(){
  /// \MemberDescr
  /// Here is the initialization of all the requested modules. If you build a module, or a set of modules,
  /// which inherits from NA62VReconstruction, NA62VDigitizer, NA62VRawDecoder, you have to add it here to
  /// make it available for usage.
  /// \EndMemberDescr

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"Cedar")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new CedarReconstruction(fHistoFile, fCedarConfigFileName));
    fDigiLibrary.push_back(new CedarDigitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new CedarRawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new CedarRawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"CHANTI")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new CHANTIReconstruction(fHistoFile, fCHANTIConfigFileName));
    fDigiLibrary.push_back(new CHANTIDigitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new CHANTIRawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new CHANTIRawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"CHOD")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new CHODReconstruction(fHistoFile, fCHODConfigFileName));
    fDigiLibrary.push_back(new CHODDigitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new CHODRawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new CHODRawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"GigaTracker")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new GigaTrackerReconstruction(fHistoFile, fGigaTrackerConfigFileName));
    fDigiLibrary.push_back(new GigaTrackerDigitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new GigaTrackerRawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new GigaTrackerRawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"HAC")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new HACReconstruction(fHistoFile, fHACConfigFileName));
    fDigiLibrary.push_back(new HACDigitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new HACRawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new HACRawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"IRC")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new IRCReconstruction(fHistoFile, fIRCConfigFileName));
    fDigiLibrary.push_back(new IRCDigitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new IRCRawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new IRCRawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"LAV")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new LAVReconstruction(fHistoFile, fLAVConfigFileName));
    fDigiLibrary.push_back(new LAVDigitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new LAVRawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new LAVRawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"LKr")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new LKrReconstruction(fHistoFile, fLKrConfigFileName));
    fDigiLibrary.push_back(new LKrDigitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new LKrRawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new LKrRawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"MUV0")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new MUV0Reconstruction(fHistoFile, fMUV0ConfigFileName));
    fDigiLibrary.push_back(new MUV0Digitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new MUV0RawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new MUV0RawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"MUV1")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new MUV1Reconstruction(fHistoFile, fMUV1ConfigFileName));
    fDigiLibrary.push_back(new MUV1Digitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new MUV1RawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new MUV1RawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"MUV2")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new MUV2Reconstruction(fHistoFile, fMUV2ConfigFileName));
    fDigiLibrary.push_back(new MUV2Digitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new MUV2RawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new MUV2RawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"MUV3")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new MUV3Reconstruction(fHistoFile, fMUV3ConfigFileName));
    fDigiLibrary.push_back(new MUV3Digitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new MUV3RawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new MUV3RawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"NewCHOD")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new NewCHODReconstruction(fHistoFile, fNewCHODConfigFileName));
    fDigiLibrary.push_back(new NewCHODDigitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new NewCHODRawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new NewCHODRawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"RICH")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new RICHReconstruction(fHistoFile, fRICHConfigFileName));
    fDigiLibrary.push_back(new RICHDigitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new RICHRawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new RICHRawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"SAC")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new SACReconstruction(fHistoFile, fSACConfigFileName));
    fDigiLibrary.push_back(new SACDigitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new SACRawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new SACRawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"Spectrometer")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new SpectrometerReconstruction(fHistoFile, fSpectrometerConfigFileName));
    fDigiLibrary.push_back(new SpectrometerDigitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new SpectrometerRawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new SpectrometerRawEncoder(fRecoLibrary.back()));
  }

  if(std::find(fRecoSequence.begin(),fRecoSequence.end(),"SAV")!=fRecoSequence.end()){
    fRecoLibrary.push_back(new SAVReconstruction(fHistoFile, fSAVConfigFileName));
    fDigiLibrary.push_back(new SAVDigitizer(fRecoLibrary.back()));
    fRawLibrary.push_back(new SAVRawDecoder(fRecoLibrary.back()));
    fRawEncLibrary.push_back(new SAVRawEncoder(fRecoLibrary.back()));
  }

  fRawEncLibrary.push_back(new L0TPRawEncoder(this));
}

void NA62Reconstruction::InitRecoRawTable() {
  fRecoRawTable[kDummy       ]="dummy";
  fRecoRawTable[kCedar       ]="Cedar";
  fRecoRawTable[kGigaTracker ]="GigaTracker";
  fRecoRawTable[kCHANTI      ]="CHANTI";
  fRecoRawTable[kLAV         ]="LAV";
  fRecoRawTable[kSpectrometer]="Spectrometer";
  fRecoRawTable[kCHOD        ]="CHOD";
  fRecoRawTable[kRICH        ]="RICH";
  fRecoRawTable[kIRC         ]="IRC";
  fRecoRawTable[kLKr         ]="LKr";
  fRecoRawTable[kMUV1        ]="MUV1";
  fRecoRawTable[kMUV2        ]="MUV2";
  fRecoRawTable[kMUV3        ]="MUV3";
  fRecoRawTable[kSAC         ]="SAC";
  fRecoRawTable[kNewCHOD     ]="NewCHOD";
  fRecoRawTable[kHAC         ]="HAC";
  fRecoRawTable[kL0TP        ]="L0TP";
  fRecoRawTable[kL1TP        ]="L1TP";
  fRecoRawTable[kL2EB        ]="L2EB";
  fRecoRawTable[kDIM         ]="DIM";
  fRecoRawTable[kMUV0        ]="MUV0";
  fRecoRawTable[kSAV         ]="SAV";
  for (UInt_t i=0; i<MAXNDETECTORS; i++) {
    fRawDecoders.push_back (nullptr);
    fRawRecoIndex.push_back (-1);
  }
  // ------------------ Init GuestDetectorIndices ------------------ //
  fGuestDetectorIndices.clear();
  fGuestDetectorIndices.resize(fHostsForGuestDetectors.size());
  for (UInt_t iHost=0; iHost < fHostsForGuestDetectors.size(); iHost++) {
    fGuestDetectorIndices[iHost].clear();
    for(UInt_t iGuest=0;iGuest<fGuestDetectorNames[iHost].size();iGuest++){
      fGuestDetectorIndices[iHost].push_back(-1);
      for(UInt_t iTable=0;iTable<MAXNDETECTORS;iTable++){
        if(fRecoRawTable[iTable]==fGuestDetectorNames[iHost][iGuest]) {
          fGuestDetectorIndices[iHost][iGuest] = iTable;
          break;
        }
      }
    }
  }
  // --------------------------------------------------------------- //
}

void NA62Reconstruction::CleanUpLibraries(){
  /// \MemberDescr
  /// It removes the modules unused during the current run.
  /// \EndMemberDescr

  std::cout << "Cleaning up libraries" << std::endl;
  Bool_t Delete;
  for(UInt_t iRecoL = 0; iRecoL < fRecoLibrary.size(); iRecoL++)
    if(fRecoLibrary[iRecoL]){
      Delete = kTRUE;
      for(UInt_t iReco = 0; iReco < fReconstructions.size(); iReco++)
        if(fRecoLibrary[iRecoL] == fReconstructions[iReco])
          Delete = kFALSE;
      if(Delete){
        delete fRecoLibrary[iRecoL];
        fRecoLibrary.erase(fRecoLibrary.begin() + iRecoL);
        iRecoL--;
      }
    }

  for(UInt_t iDigiL = 0; iDigiL < fDigiLibrary.size(); iDigiL++)
    if(fDigiLibrary[iDigiL]){
      Delete = kTRUE;
      for(UInt_t iDigi = 0; iDigi < fDigitizers.size(); iDigi++)
        if(fDigiLibrary[iDigiL] == fDigitizers[iDigi])
          Delete = kFALSE;
      if(Delete){
        delete fDigiLibrary[iDigiL];
        fDigiLibrary.erase(fDigiLibrary.begin() + iDigiL);
        iDigiL--;
      }
    }

  for(UInt_t iBinaryL = 0; iBinaryL < fRawEncLibrary.size(); iBinaryL++)
    if(fRawEncLibrary[iBinaryL]){
      Delete = kTRUE;
      for(UInt_t iBinary = 0; iBinary < fRawEncoders.size(); iBinary++)
        if(fRawEncLibrary[iBinaryL] == fRawEncoders[iBinary])
          Delete = kFALSE;
      if(Delete){
        delete fRawEncLibrary[iBinaryL];
        fRawEncLibrary.erase(fRawEncLibrary.begin() + iBinaryL);
        iBinaryL--;
      }
    }

  for(UInt_t iRawL = 0; iRawL < fRawLibrary.size(); iRawL++)
    if(fRawLibrary[iRawL]){
      Delete = kTRUE;
      for(UInt_t iRaw = 0; iRaw < fRawDecoders.size(); iRaw++)
        if(fRawLibrary[iRawL] == fRawDecoders[iRaw])
          Delete = kFALSE;
      if(Delete){
        delete fRawLibrary[iRawL];
        fRawLibrary.erase(fRawLibrary.begin() + iRawL);
        iRawL--;
      }
    }
}

void NA62Reconstruction::CleanUpSequence(){
  /// \MemberDescr
  /// Single event management and processing decision.
  /// \EndMemberDescr

  for(Int_t iReco = 0; iReco < fNReconstructions; iReco++){
    if(!fInputEvents[iReco]){
      std::cout << "Disabling Reconstruction, Digitization, Encoding and RawDecoding for " << fDigitizers[iReco]->GetName() << std::endl;
      CleanUpTimers(iReco); //it must be before cleaning up fReconstructions
      fDigitizers.erase(fDigitizers.begin() + iReco);
      fReconstructions.erase(fReconstructions.begin() + iReco);
      fRawEncoders.erase(fRawEncoders.begin() + iReco);
      fInputEvents.erase(fInputEvents.begin() + iReco);
      fEvents.erase(fEvents.begin() + iReco);
      fRecoEvents.erase(fRecoEvents.begin() + iReco);
      fSlimRecoEvents.erase(fSlimRecoEvents.begin() + iReco);
      for(UInt_t iTree=0; iTree<fOutputTrees.size(); iTree++) fOutputBranchEvents[iTree]->erase(fOutputBranchEvents[iTree]->begin() + iReco);
      fT0ReferenceDetectors.erase(fT0ReferenceDetectors.begin() + iReco);
      fT0NHitsMinValues.erase(fT0NHitsMinValues.begin() + iReco);
      fNDigitizers = fDigitizers.size();
      fNReconstructions = fReconstructions.size();
      fNRawEncoders = fRawEncoders.size();
      iReco--;
    }
    if(iReco>=0) fRecoSequence[iReco]=fReconstructions[iReco]->GetName();
  }
  if(!fNReconstructions) {
    std::cout << std::endl;
    std::cerr << "[NA62Reconstruction] WARNING: No valid Reconstructions found in file '" << fCurrentFileName << "'. *** Check the enabled Reconstructions! ***" << std::endl;
    std::cout << std::endl;
  }
  //resetting fRawRecoIndex
  for (UInt_t iRaw=0; iRaw<MAXNDETECTORS; iRaw++) fRawRecoIndex[iRaw]=-1;

  for(Int_t iReco = 0; iReco < fNReconstructions; iReco++){
    Int_t iRaw = -1;
    for (UInt_t iTable=0; iTable<MAXNDETECTORS; iTable++)
      if (fRecoSequence[iReco]==fRecoRawTable[iTable]) iRaw = iTable;
    if(iRaw != -1)
      fRawRecoIndex[iRaw] = iReco;
  }
  //removing unused RawDecoders
  for(UInt_t iRaw=0;iRaw<fRawDecoders.size();iRaw++) {
    if(fRawRecoIndex[iRaw]==-1 && fRawDecoders[iRaw]) fRawDecoders[iRaw] = 0;
  }
  CleanUpLibraries();
  //printing updated Detectors VS Index table
  std::cout <<"****** Detectors VS Index table: ******" << std::endl;
  for (Int_t i =0; i<MAXNDETECTORS; i++) {
    if(fRecoRawTable[i].Sizeof()>8) std::cout << fRecoRawTable[i] << "\t" << fRawRecoIndex[i] << std::endl;
    else std::cout << fRecoRawTable[i] << "\t\t" << fRawRecoIndex[i] << std::endl;
  }
  std::cout <<"***************************************" << std::endl;
}

TDetectorVEvent * NA62Reconstruction::FindMCEvent(TString Name){
  /// \MemberDescr
  /// It retrieves the TSubDetectorEvent (inheriting from TDetectorVEvent), which holds MC hits,
  /// related to the requested SubDetector.
  /// Since it involves TString comparison, it is recommended to do it once. The pointer
  /// to the object should never change during the run.
  /// \EndMemberDescr

  for(UInt_t iLib = 0; iLib < fReconstructions.size(); iLib++)
    if(fReconstructions[iLib]->GetName().CompareTo(Name) == 0)
      return fInputEvents[iLib];
  return 0;
}

TRecoVEvent * NA62Reconstruction::FindRecoEvent(TString Name){
  /// \MemberDescr
  /// It retrieves the TRecoSubDetectorEvent, which holds result of reconstruction,
  /// related to the requested SubDetector.
  /// Since it involves TString comparison, it is recommended to do it once. The pointer
  /// to the object should never change during the run.
  /// \EndMemberDescr

  for(UInt_t iLib = 0; iLib < fReconstructions.size(); iLib++)
    if(fReconstructions[iLib]->GetName().CompareTo(Name) == 0)
      return fRecoEvents[iLib];
  return 0;
}

TSlimRecoVEvent * NA62Reconstruction::FindSlimRecoEvent(TString Name){
  /// \MemberDescr
  /// It retrieves the TSlimRecoSubDetectorEvent, which holds result of reconstruction,
  /// related to the requested SubDetector.
  /// Since it involves TString comparison, it is recommended to do it once. The pointer
  /// to the object should never change during the run.
  /// \EndMemberDescr

  for(UInt_t iLib = 0; iLib < fReconstructions.size(); iLib++)
    if(fReconstructions[iLib]->GetName().CompareTo(Name) == 0)
      return fSlimRecoEvents[iLib];
  return 0;
}

NA62VReconstruction * NA62Reconstruction::FindReco(TString Name){
  /// \MemberDescr
  /// It looks for the requested Reconstruction module in the library.
  /// Since it involves TString comparison, it is recommended to do it once. The pointer
  /// to the object should never change during the run.
  /// \EndMemberDescr

  for(UInt_t iLib = 0; iLib < fRecoLibrary.size(); iLib++)
    if(fRecoLibrary[iLib]->GetName().CompareTo(Name) == 0)
      return fRecoLibrary[iLib];
  return 0;
}

NA62VDigitizer * NA62Reconstruction::FindDigi(TString Name){
  /// \MemberDescr
  /// It looks for the requested Digitization module in the library.
  /// Since it involves TString comparison, it is recommended to do it once. The pointer
  /// to the object should never change during the run.
  /// \EndMemberDescr

  for(UInt_t iLib = 0; iLib < fDigiLibrary.size(); iLib++)
    if(fDigiLibrary[iLib]->GetName().CompareTo(Name) == 0)
      return fDigiLibrary[iLib];
  return 0;
}

NA62VRawDecoder * NA62Reconstruction::FindRaw(TString Name){
  /// \MemberDescr
  /// It looks for the requested RawDecoding module in the library.
  /// Since it involves TString comparison, it is recommended to do it once. The pointer
  /// to the object should never change during the run.
  /// \EndMemberDescr

  for(UInt_t iLib = 0; iLib < fRawLibrary.size(); iLib++){
    fRawLibrary[iLib]->GetName();
    if(fRawLibrary[iLib]->GetName().CompareTo(Name) == 0)
      return fRawLibrary[iLib];
  }
  return 0;
}

NA62VRawEncoder * NA62Reconstruction::FindBinary(TString Name){
  /// \MemberDescr
  /// It looks for the requested RawEncoding module in the library.
  /// Since it involves TString comparison, it is recommended to do it once. The pointer
  /// to the object should never change during the run.
  /// \EndMemberDescr
  for(UInt_t iLib = 0; iLib < fRawEncLibrary.size(); iLib++){
    fRawEncLibrary[iLib]->GetName();
    if(fRawEncLibrary[iLib]->GetName().CompareTo(Name) == 0)
      return fRawEncLibrary[iLib];
  }
  return 0;
}

TTree* NA62Reconstruction::FindOutputTree(TString TreeName){
  /// \MemberDescr
  /// It looks for the requested tree in fOutputTrees.
  /// Since it involves TString comparison, it is recommended to do it once. The pointer
  /// to the object should never change during the run.
  /// \EndMemberDescr
  for(UInt_t iTree=0;iTree<fOutputTrees.size();iTree++){
    if(((TString)fOutputTrees[iTree]->GetName()).CompareTo(TreeName) == 0) return fOutputTrees[iTree];
  }
  std::cerr << "[NA62Reconstruction] WARNING: Tree '" << TreeName << "' not found!" << std::endl;
  return 0;
}

NA62Reconstruction::EventVector* NA62Reconstruction::FindOutputBranchEvent(TString TreeName){
  /// \MemberDescr
  /// It looks for the requested OutputBranchEvent in fOutputBranchEvents.
  /// Since it involves TString comparison, it is recommended to do it once. The pointer
  /// to the object should never change during the run.
  /// \EndMemberDescr
  for(UInt_t iTree=0;iTree<fOutputTrees.size();iTree++){
    if(((TString)fOutputTrees[iTree]->GetName()).CompareTo(TreeName) == 0) return fOutputBranchEvents[iTree];
  }
  std::cerr << "[NA62Reconstruction] WARNING: Tree '" << TreeName << "' not found!" << std::endl;
  return 0;
}

TChain* NA62Reconstruction::FindMCChain(TString ChainName){
  /// \MemberDescr
  /// It looks for the requested TChain in fMCChains.
  /// Since it involves TString comparison, it is recommended to do it once. The pointer
  /// to the object should never change during the run.
  /// \EndMemberDescr
  for(UInt_t iChain=0;iChain<fMCChains.size();iChain++){
    if(((TString)fMCChains[iChain]->GetName()).CompareTo(ChainName) == 0) return fMCChains[iChain];
    if(ChainName=="MC" && ((TString)fMCChains[iChain]->GetName()).CompareTo("Digis") == 0) return fMCChains[iChain];
  }
  std::cerr << "[NA62Reconstruction] WARNING: Chain '" << ChainName << "' not found!" << std::endl;
  return 0;
}

void NA62Reconstruction::StartOfBurst(){
  std::cout << "------------------ Start of Burst ------------------" << std::endl;
  if(FindReco("Cedar")) static_cast<CedarReconstruction*>(FindReco("Cedar"))->StartOfBurst();
  if(FindReco("CHANTI")) static_cast<CHANTIReconstruction*>(FindReco("CHANTI"))->StartOfBurst();
  if(FindReco("CHOD")) static_cast<CHODReconstruction*>(FindReco("CHOD"))->StartOfBurst();
  if(FindReco("GigaTracker")) static_cast<GigaTrackerReconstruction*>(FindReco("GigaTracker"))->StartOfBurst();
  if(FindReco("HAC")) static_cast<HACReconstruction*>(FindReco("HAC"))->StartOfBurst();
  if(FindReco("IRC")) static_cast<IRCReconstruction*>(FindReco("IRC"))->StartOfBurst();
  if(FindReco("LAV")) static_cast<LAVReconstruction*>(FindReco("LAV"))->StartOfBurst();
  if(FindReco("LKr")) static_cast<LKrReconstruction*>(FindReco("LKr"))->StartOfBurst();
  if(FindReco("MUV0")) static_cast<MUV0Reconstruction*>(FindReco("MUV0"))->StartOfBurst();
  if(FindReco("MUV1")) static_cast<MUV1Reconstruction*>(FindReco("MUV1"))->StartOfBurst();
  if(FindReco("MUV2")) static_cast<MUV2Reconstruction*>(FindReco("MUV2"))->StartOfBurst();
  if(FindReco("MUV3")) static_cast<MUV3Reconstruction*>(FindReco("MUV3"))->StartOfBurst();
  if(FindReco("NewCHOD")) static_cast<NewCHODReconstruction*>(FindReco("NewCHOD"))->StartOfBurst();
  if(FindReco("RICH")) static_cast<RICHReconstruction*>(FindReco("RICH"))->StartOfBurst();
  if(FindReco("SAC")) static_cast<SACReconstruction*>(FindReco("SAC"))->StartOfBurst();
  if(FindReco("Spectrometer")) static_cast<SpectrometerReconstruction*>(FindReco("Spectrometer"))->StartOfBurst();
  if(FindReco("SAV")) static_cast<SAVReconstruction*>(FindReco("SAV"))->StartOfBurst();
  // RawDecoder instances
  if(fIsRawData){
    if(FindRaw("Cedar")) static_cast<CedarRawDecoder*>(FindRaw("Cedar"))->StartOfBurst();
    if(FindRaw("CHANTI")) static_cast<CHANTIRawDecoder*>(FindRaw("CHANTI"))->StartOfBurst();
    if(FindRaw("CHOD")) static_cast<CHODRawDecoder*>(FindRaw("CHOD"))->StartOfBurst();
    if(FindRaw("GigaTracker")) static_cast<GigaTrackerRawDecoder*>(FindRaw("GigaTracker"))->StartOfBurst();
    if(FindRaw("HAC")) static_cast<HACRawDecoder*>(FindRaw("HAC"))->StartOfBurst();
    if(FindRaw("IRC")) static_cast<IRCRawDecoder*>(FindRaw("IRC"))->StartOfBurst();
    if(FindRaw("LAV")) static_cast<LAVRawDecoder*>(FindRaw("LAV"))->StartOfBurst();
    if(FindRaw("LKr")) static_cast<LKrRawDecoder*>(FindRaw("LKr"))->StartOfBurst();
    if(FindRaw("MUV0")) static_cast<MUV0RawDecoder*>(FindRaw("MUV0"))->StartOfBurst();
    if(FindRaw("MUV1")) static_cast<MUV1RawDecoder*>(FindRaw("MUV1"))->StartOfBurst();
    if(FindRaw("MUV2")) static_cast<MUV2RawDecoder*>(FindRaw("MUV2"))->StartOfBurst();
    if(FindRaw("MUV3")) static_cast<MUV3RawDecoder*>(FindRaw("MUV3"))->StartOfBurst();
    if(FindRaw("NewCHOD")) static_cast<NewCHODRawDecoder*>(FindRaw("NewCHOD"))->StartOfBurst();
    if(FindRaw("RICH")) static_cast<RICHRawDecoder*>(FindRaw("RICH"))->StartOfBurst();
    if(FindRaw("SAC")) static_cast<SACRawDecoder*>(FindRaw("SAC"))->StartOfBurst();
    if(FindRaw("Spectrometer")) static_cast<SpectrometerRawDecoder*>(FindRaw("Spectrometer"))->StartOfBurst();
    if(FindRaw("SAV")) static_cast<SAVRawDecoder*>(FindRaw("SAV"))->StartOfBurst();
  }
}

void NA62Reconstruction::EndOfBurst(){
  if(FindReco("Cedar")) static_cast<CedarReconstruction*>(FindReco("Cedar"))->EndOfBurst();
  if(FindReco("CHANTI")) static_cast<CHANTIReconstruction*>(FindReco("CHANTI"))->EndOfBurst();
  if(FindReco("CHOD")) static_cast<CHODReconstruction*>(FindReco("CHOD"))->EndOfBurst();
  if(FindReco("GigaTracker")) static_cast<GigaTrackerReconstruction*>(FindReco("GigaTracker"))->EndOfBurst();
  if(FindReco("HAC")) static_cast<HACReconstruction*>(FindReco("HAC"))->EndOfBurst();
  if(FindReco("IRC")) static_cast<IRCReconstruction*>(FindReco("IRC"))->EndOfBurst();
  if(FindReco("LAV")) static_cast<LAVReconstruction*>(FindReco("LAV"))->EndOfBurst();
  if(FindReco("LKr")) static_cast<LKrReconstruction*>(FindReco("LKr"))->EndOfBurst();
  if(FindReco("MUV0")) static_cast<MUV0Reconstruction*>(FindReco("MUV0"))->EndOfBurst();
  if(FindReco("MUV1")) static_cast<MUV1Reconstruction*>(FindReco("MUV1"))->EndOfBurst();
  if(FindReco("MUV2")) static_cast<MUV2Reconstruction*>(FindReco("MUV2"))->EndOfBurst();
  if(FindReco("MUV3")) static_cast<MUV3Reconstruction*>(FindReco("MUV3"))->EndOfBurst();
  if(FindReco("NewCHOD")) static_cast<NewCHODReconstruction*>(FindReco("NewCHOD"))->EndOfBurst();
  if(FindReco("RICH")) static_cast<RICHReconstruction*>(FindReco("RICH"))->EndOfBurst();
  if(FindReco("SAC")) static_cast<SACReconstruction*>(FindReco("SAC"))->EndOfBurst();
  if(FindReco("Spectrometer")) static_cast<SpectrometerReconstruction*>(FindReco("Spectrometer"))->EndOfBurst();
  if(FindReco("SAV")) static_cast<SAVReconstruction*>(FindReco("SAV"))->EndOfBurst();
  // RawDecoder instances
  if(fIsRawData){
    if(FindRaw("Cedar")) static_cast<CedarRawDecoder*>(FindRaw("Cedar"))->EndOfBurst();
    if(FindRaw("CHANTI")) static_cast<CHANTIRawDecoder*>(FindRaw("CHANTI"))->EndOfBurst();
    if(FindRaw("CHOD")) static_cast<CHODRawDecoder*>(FindRaw("CHOD"))->EndOfBurst();
    if(FindRaw("GigaTracker")) static_cast<GigaTrackerRawDecoder*>(FindRaw("GigaTracker"))->EndOfBurst();
    if(FindRaw("HAC")) static_cast<HACRawDecoder*>(FindRaw("HAC"))->EndOfBurst();
    if(FindRaw("IRC")) static_cast<IRCRawDecoder*>(FindRaw("IRC"))->EndOfBurst();
    if(FindRaw("LAV")) static_cast<LAVRawDecoder*>(FindRaw("LAV"))->EndOfBurst();
    if(FindRaw("LKr")) static_cast<LKrRawDecoder*>(FindRaw("LKr"))->EndOfBurst();
    if(FindRaw("MUV0")) static_cast<MUV0RawDecoder*>(FindRaw("MUV0"))->EndOfBurst();
    if(FindRaw("MUV1")) static_cast<MUV1RawDecoder*>(FindRaw("MUV1"))->EndOfBurst();
    if(FindRaw("MUV2")) static_cast<MUV2RawDecoder*>(FindRaw("MUV2"))->EndOfBurst();
    if(FindRaw("MUV3")) static_cast<MUV3RawDecoder*>(FindRaw("MUV3"))->EndOfBurst();
    if(FindRaw("NewCHOD")) static_cast<NewCHODRawDecoder*>(FindRaw("NewCHOD"))->EndOfBurst();
    if(FindRaw("RICH")) static_cast<RICHRawDecoder*>(FindRaw("RICH"))->EndOfBurst();
    if(FindRaw("SAC")) static_cast<SACRawDecoder*>(FindRaw("SAC"))->EndOfBurst();
    if(FindRaw("Spectrometer")) static_cast<SpectrometerRawDecoder*>(FindRaw("Spectrometer"))->EndOfBurst();
    if(FindRaw("SAV")) static_cast<SAVRawDecoder*>(FindRaw("SAV"))->EndOfBurst();
  }
  if(fChokeON){ //no choke OFF signal found!
    fChokeONEndTime=NA62RecoManager::GetInstance()->GetEventHeader()->GetTimeStamp()*ClockPeriod;
    fChokeONTimeInFile+=(fChokeONEndTime-fChokeONStartTime);
    fChokeONTimeInTotal+=(fChokeONEndTime-fChokeONStartTime);
  }
  std::cout << "------------------- End of Burst -------------------" << std::endl;
}

void NA62Reconstruction::NextFile(Int_t nMaxEvents){
  /// \MemberDescr
  /// It loads the next raw file into the buffer and resets file and buffer offset counters.
  /// \EndMemberDescr

  ResetFileVariables();

  if(fiFile == fNFiles && fContinuousReading){
    if(fInputListFileName.CompareTo("") == 0)
      Exception("Input List FileName not defined");
    ReadInputList(fInputListFileName,-1);
  }

  while (fInputFileDescriptor == -1 && fiFile < fNFiles) {

    fCurrentFileName = static_cast<TObjString*>(fInputFileNameList->At(fiFile))->GetString();
    // Skip commented entries
    if (fCurrentFileName.Contains("#") || fCurrentFileName.Contains("!")) {
      std::cout << "[" << TimeString() << "] Skipping commented entry (" << fiFile+1 << "/" << fNFiles << "): " << fCurrentFileName << std::endl;
      fiFile++;
      continue;
    }

    std::cout << "[" << TimeString() <<"] Opening file (" << fiFile+1 << "/" << fNFiles << "): " << CheckProtocols(fCurrentFileName).Data() << std::endl;
    fTimer.StartTimer(fOpenInputTimer);
    fInputFileDescriptor = open(CheckProtocols(fCurrentFileName).Data(),O_RDONLY|O_NONBLOCK);

    if (fInputFileDescriptor == -1) {
      fTimer.StopTimer(fOpenInputTimer, kFALSE);
      std::cout << "[" << TimeString() << "] File not found, skipping (" << fiFile+1 << "/" << fNFiles << "): " << fCurrentFileName << std::endl;
      TString mode = (fSkippedFileDescriptor)?"a":"w";
      fSkippedFileDescriptor = fopen(fSkippedFileName.Data(),mode.Data());
      fprintf (fSkippedFileDescriptor, "%s\n",fCurrentFileName.Data());
      fclose(fSkippedFileDescriptor);
      fNSkippedFilesInTotal++;
      fiFile++;
      continue;
    }
    fTimer.StopTimer(fOpenInputTimer);

    fiFile++;
    fPreviousEventNumber = -1; //for debug
  }

  WriteFileName(fCurrentFileName);
  NextChunk(fInputFileDescriptor);

  if (fInputFileDescriptor!=-1 && !fNWordsInFile) {

    std::cout << "[" << TimeString() << "] Input file is empty (" << fiFile << "/" << fNFiles << "): " << fCurrentFileName << std::endl;
    close(fInputFileDescriptor);
    if(fEOBFileEnabled && fEOBFileDescriptor) fclose(fEOBFileDescriptor);
    if(fLogFileEnabled && fLogFileDescriptor) fclose(fLogFileDescriptor);
    NextFile(nMaxEvents);
  }
}

void NA62Reconstruction::NextChunk(Int_t FileDescriptor){
  fTimer.StartTimer(fInputReadTimer);
  fNWordsInChunk = 0;
  fRawFileEOF = kFALSE;
  fpDataBuffer = fDataBuffer.data();
  Long_t bSizeIn = fDataBuffer.size()*4; //fill the buffer
  Long_t bSizeOut = read(FileDescriptor, fpDataBuffer, bSizeIn);
  fNWordsInChunk = bSizeOut/4;
  if(fNWordsInChunk == fDataBuffer.size()){
    Int_t NScannedWords=0;
    Bool_t LastEventFound  = kFALSE;
    Int_t NWordsInLastEvent=0;
    while(!LastEventFound && (UInt_t)NScannedWords<fNWordsInChunk-1){
      if(fNWordsInChunk-NScannedWords<fDataBuffer.size() && (fDataBuffer[fNWordsInChunk-1-NScannedWords]&0xff000000) == 0x62000000){ //candidate for last complete event header found
        UInt_t LastEventID = fDataBuffer[fNWordsInChunk-1-NScannedWords]&0x00ffffff;
        NWordsInLastEvent = fDataBuffer[fNWordsInChunk-NScannedWords];
        if(NWordsInLastEvent && 0<=(Int_t)fNWordsInChunk-2-NScannedWords+NWordsInLastEvent && fNWordsInChunk-1-NScannedWords+NWordsInLastEvent<fDataBuffer.size()
            && fDataBuffer[fNWordsInChunk-2-NScannedWords+NWordsInLastEvent] == LastEventID && (fDataBuffer[fNWordsInChunk-1-NScannedWords+NWordsInLastEvent]&0xff000000) == 0x62000000) {
          LastEventFound = kTRUE; //trailer of last event found where it was supposed to be
        }
      }
      NScannedWords++;
    }
    if(LastEventFound) fNWordsInChunk-=(NScannedWords-NWordsInLastEvent);
    else fNWordsInChunk=0;
    fCurrentOffsetInFile = lseek(FileDescriptor, (off64_t)(fNWordsInFile + fNWordsInChunk)*4, SEEK_SET);
  }
  else {
    fRawFileEOF = kTRUE;
  }
  fNWordsInFile  += fNWordsInChunk;

  Double_t Elapsed = fTimer.StopTimer(fInputReadTimer);
  if (fNWordsInChunk) std::cout << fNWordsInChunk << " words loaded into memory [" << fNWordsInChunk*4/1024./1024./(Elapsed/1000.) << " MB/s]" << std::endl;

  return;
}

void NA62Reconstruction::WriteFileName(TString newFile) {
  /// \MemberDescr
  /// \param newFile: Name of the input file
  ///
  /// Add a file name to the output
  /// \EndMemberDescr

  if(fContinuousReading) return; //do nothing in case of continuous reading (no file is written!)

  //Print fileName in the output file for future reference
  if (!fHistoFile->FindKey("InputFiles")) fHistoFile->mkdir("InputFiles");
  fHistoFile->cd("InputFiles");
  TObjString fileName;
  if (newFile.BeginsWith("/") || newFile.BeginsWith("root://")) {
    //It is an absolute path
    fileName = TObjString(newFile);
  } else {
    fileName = TObjString(TString(get_current_dir_name()) + "/" + newFile);
  }
  fileName.Write();
  fHistoFile->cd("/");
}

Bool_t NA62Reconstruction::HandleFile() {
  /// \MemberDescr
  /// It manages the input file and prepare the event to be decoded by DecodeEvent()
  /// \EndMemberDescr

  fTimer.StartTimer(fRawDecTimers[0]); // all reco
  if      (!fNWordsInFile && !fContinuousReading) NextFile(fNEvt-fNProcessedEventsInTotal);
  else if (!fNWordsInFile &&  fContinuousReading) NextFile(fNEvt);
  if ((fpDataBuffer>=fDataBuffer.data()+fNWordsInChunk && (fRawFileEOF || fErrorInReading)) || fNProcessedEventsInFile>=(UInt_t)fNEvtPerFile) {
    // all the words loaded in memory have been processed and (eof|| error in reading file) OR enough ProcessedEventsInFile
    EndOfBurst();
    std::cout << "[" << TimeString() <<"] Closing file (" << fiFile << "/" << fNFiles << "): " << fCurrentFileName << std::endl;
    close(fInputFileDescriptor);
    if(fEOBFileEnabled && fEOBFileDescriptor) fclose(fEOBFileDescriptor);
    if(fLogFileEnabled && fLogFileDescriptor) fclose(fLogFileDescriptor);
    if(fiFile < fNFiles && !fContinuousReading) NextFile(fNEvt-fNProcessedEventsInTotal);
    else if(fContinuousReading) NextFile(fNEvt);
    else {
      fTimer.StopTimer(fRawDecTimers[0]); // all reco
      return kFALSE;
    }
  }
  if(fpDataBuffer>=fDataBuffer.data()+fNWordsInChunk && !fRawFileEOF && !fErrorInReading) { //all the words loaded in memory have been processed and !eof
    NextChunk(fInputFileDescriptor);
  }

  //---------------- look for Burst header ----------------//
  if(!fNReadEventsInFile) DecodeBurstHeader();
  //-------------------------------------------------------//

  return kTRUE;
}

Bool_t NA62Reconstruction::DecodeEvent(UInt_t * &pDataBuffer, std::vector<UInt_t> &DataBuffer) {
  /// \MemberDescr
  /// It calls the registered RawDecoder modules in the requested sequence, building
  /// input events for the Reconstruction modules, or for data output
  /// \EndMemberDescr

#ifdef ONLINEHLT
  //HLT online HLT initialization
  fHLTLib->handleInitKtag();
  fHLTLib->handleInitLav();
  fHLTLib->handleInitStraw();

  using functype_subevent_constructor = na62::l0::Subevent*(const uint_fast16_t pexpectedPacketsNum);
  std::function<functype_subevent_constructor> create_subevent{reinterpret_cast<functype_subevent_constructor*>(dlsym(fHLTLib->get(), "create_subevent"))};

  using functype_subevent_destructor = void(na62::l0::Subevent*);
  std::function<functype_subevent_destructor> delete_subevent{reinterpret_cast<functype_subevent_destructor*>(dlsym(fHLTLib->get(), "destroy_subevent"))};

  using functype_l1storage_constructor = L1InfoToStorage*(void);
  std::function<functype_l1storage_constructor> create_l1storage{reinterpret_cast<functype_l1storage_constructor*>(dlsym(fHLTLib->get(), "create_l1storage"))};

  using functype_l1storage_destructor = void(L1InfoToStorage*);
  std::function<functype_l1storage_destructor> delete_l1storage{reinterpret_cast<functype_l1storage_destructor*>(dlsym(fHLTLib->get(), "destroy_l1storage"))};


  int const max_fragment_number = 32;
  na62::l0::Subevent* subevent = create_subevent(max_fragment_number);
  L1InfoToStorage* l1Info = create_l1storage();


  uint_fast8_t cedar_trigger;
  uint_fast8_t lav_trigger;
  uint_fast8_t straw_trigger;
#endif

  fNReadEventsInFile++;
  fNReadEventsInTotal++;

  EventHeader* EventHeader = NA62RecoManager::GetInstance()->GetEventHeader();
  if(!EventHeader->SetHeader(pDataBuffer)){
    //Exception ("Bad header");
    //fTimer.StopTimer(fRawDecTimers[0]); // all reco
    //return kFALSE;
    cerr_en(fWarningsLevel,WARN_EVT) << "[NA62Reconstruction] WARNING: Bad header detected! Skipping file.";
    cerr_en(fWarningsLevel,WARN_EVT) << " [EventSeq: " << fNProcessedEventsInFile << " EventID: " << std::hex << EventHeader->GetEventNumber() << std::dec << " HeaderWord: " <<  std::hex << *pDataBuffer << std::dec << "]" << std::endl;
    std::cout << "[" << TimeString() <<"] Closing file (" << fiFile << "/" << fNFiles << "): " << fCurrentFileName << std::endl;
    close(fInputFileDescriptor);
    if(fEOBFileEnabled && fEOBFileDescriptor) fclose(fEOBFileDescriptor);
    if(fLogFileEnabled && fLogFileDescriptor) fclose(fLogFileDescriptor);
    if(fiFile < fNFiles && !fContinuousReading) NextFile(fNEvt-fNProcessedEventsInTotal);
    else if(fContinuousReading) NextFile(fNEvt);
    else {
      fTimer.StopTimer(fRawDecTimers[0]); // all reco
      return kFALSE;
    }
    fNSkippedFilesInTotal++;
    fOutputStatus = kStatusBadHeader; //skip reconstruction of this event
    return kTRUE;
  }
  if((EventHeader->GetTriggerType()&0xff)==0x08 || (EventHeader->GetTriggerType()&0xff)==0x31 || (EventHeader->GetTriggerType()&0xff)==0x32){
    fNReadPeriodicTriggerEventsInFile++;
    fNReadPeriodicTriggerEventsInTotal++;
  }
  else if((EventHeader->GetTriggerType()&0xff)==0x30){
    fNReadCalibrationTriggerEventsInFile++;
    fNReadCalibrationTriggerEventsInTotal++;
  }
  else if(isL0SpecialFrame(EventHeader->GetTriggerType()<<2)){
    fNReadSpecialTriggerEventsInFile++;
    fNReadSpecialTriggerEventsInTotal++;
    if(isL0SOB(EventHeader->GetTriggerType()<<2)){ //SOB
      fNReadSOBEventsInFile++;
      fNReadSOBEventsInTotal++;
    }
    else if(isL0EOB(EventHeader->GetTriggerType()<<2)){ //EOB
      fNReadEOBEventsInFile++;
      fNReadEOBEventsInTotal++;
    }
    else if(isL0CHOKEON(EventHeader->GetTriggerType()<<2)){ //CHOKE ON
      if(!fChokeON) fChokeONStartTime=EventHeader->GetTimeStamp()*ClockPeriod;
      fChokeON = kTRUE;
    }
    else if(isL0CHOKEOFF(EventHeader->GetTriggerType()<<2)){ //CHOKE OFF
      fChokeONEndTime=EventHeader->GetTimeStamp()*ClockPeriod;
      fChokeONTimeInFile+=(fChokeONEndTime-fChokeONStartTime);
      fChokeONTimeInTotal+=(fChokeONEndTime-fChokeONStartTime);
      fChokeON = kFALSE;
    }
  }
  else { //physics or control
    if(EventHeader->GetTriggerType()&0x1) {
      fNReadPhysicsTriggerEventsInFile++;
      fNReadPhysicsTriggerEventsInTotal++;
    }
    if(EventHeader->GetTriggerType()&0x10){
      fNReadControlTriggerEventsInFile++;
      fNReadControlTriggerEventsInTotal++;
    }
  }

  //------------ Check if the event should be processed ------------//
  TRandom3 aRandom(EventHeader->GetEventNumber());
  if((fEventAlreadyProcessed && fiCurrentEventInFile<fNEventsInFile && fEventAlreadyProcessed[fiCurrentEventInFile]) ||
      ((aRandom.Rndm()>1.0/fDownscalingFactor || !fRequestedTriggersFlag || fNReadEventsInTotal<(UInt_t)fJumpNEvt) &&
       fNProcessedEventsInFile && !isL0SpecialFrame(EventHeader->GetTriggerType()<<2))) {
    // Events are skipped if already processed
    // downscaling applied for ContinuousReading=1 only; Events are randomly selected;
    // First event and SpecialTriggers always processed (if not already processed)

    if(fEventAlreadyProcessed && fiCurrentEventInFile<fNEventsInFile && fEventAlreadyProcessed[fiCurrentEventInFile]) fOutputStatus = kStatusAlreadyProcessed;
    else if(!fRequestedTriggersFlag) fOutputStatus = kStatusUnrequested;
    else if(fNReadEventsInTotal<(UInt_t)fJumpNEvt) fOutputStatus = kStatusUnrequested;
    else fOutputStatus = kStatusDownscaling;

    UInt_t ELen = EventHeader->GetEventLength();
    pDataBuffer += ELen;

    fTimer.StopTimer(fRawDecTimers[0]); // all reco

    return kTRUE;
  }
  //----------------------------------------------------------------//

  fOutputStatus = kStatusGood;
  if(fEOBFileEnabled && fEOBFileDescriptor) EventHeader->SetEOBFileDescriptor(fEOBFileDescriptor);
  if(EventHeader->GetEventNumber()-fPreviousEventNumber>1) {
    cerr_en(fWarningsLevel,WARN_MAX) << "[NA62Reconstruction] WARNING: " << EventHeader->GetEventNumber()-fPreviousEventNumber-1 << " missing events in RAW data";
    cerr_en(fWarningsLevel,WARN_MAX) << " [File: " << fCurrentFileName << " CurrentEventNumber: " << EventHeader->GetEventNumber() << " PreviousEventNumber: " << fPreviousEventNumber << "]" << std::endl;
  }
  Int_t NMaxCriticalErrors = 0; //for debug
  Int_t NMaxQualityWarnings = 0; //for debug
  Int_t NMaxWrongSlots = 0; //for debug
  Int_t NMaxHitsFromMaskedChannels = 0; //for debug
  UInt_t NDet = EventHeader->GetNumOfDetectors();
  UInt_t *fpDetectorTable = pDataBuffer + O_DETECTORTABLE;
  std::vector<Int_t> iDetHost(fHostsForGuestDetectors.size(),-1);
  UInt_t * DIMBlockStart=0;
  UInt_t * DIMBlockEnd=0;
  UInt_t NGuestDetectors = 0;
  for (UInt_t iHost=0; iHost<fHostsForGuestDetectors.size(); iHost++) NGuestDetectors += fGuestDetectorIndices[iHost].size();
  for (UInt_t jDet = 0; jDet < NDet + NGuestDetectors; jDet++){
    //---- patch to unwrap subdetectors read-out by a host ----//
    Int_t iDet = jDet;
    if (jDet >= NDet){
      UInt_t NGuestsInPreviousHosts = 0;
      for (UInt_t iHost=0; iHost<fHostsForGuestDetectors.size(); iHost++){
        if(jDet>=NDet+NGuestsInPreviousHosts) iDet = iDetHost[iHost];
        NGuestsInPreviousHosts += fGuestDetectorIndices[iHost].size();
      }
    }
    if (iDet<0) {
      for (UInt_t iHost=0; iHost<fHostsForGuestDetectors.size(); iHost++){
        if(iDetHost[iHost]<0){
          std::cout << "*** Host Detector " << fHostsForGuestDetectors[iHost] << " not found in data! Disabling guest detectors.. ***" << std::endl;
          for (UInt_t iGuest=0;iGuest<fGuestDetectorIndices[iHost].size();iGuest++) {
            RemoveGuestDetector(iHost,iGuest);
            iGuest--;
          }
        }
      }
      break;
    }
    Int_t iRaw = (*(fpDetectorTable+iDet)&M_DETECTORID)>>S_DETECTORID;
    if(iRaw < 0 || iRaw >= MAXNDETECTORS){
      UInt_t ELen = EventHeader->GetEventLength();
      pDataBuffer += ELen;

      fTimer.StopTimer(fRawDecTimers[0]); // all reco

      cerr_en(fWarningsLevel,WARN_EVT) << "[NA62Reconstruction] WARNING: Unknown Detector ID 0x" << std::hex << iRaw*4 << std::dec << " in file '" << fCurrentFileName << "'! Skipping event.." << std::endl;
      fOutputStatus = kStatusBadRawID; //skip reconstruction of this event
      return kTRUE;
    }
    UInt_t NGuestsInPreviousHosts = 0;
    for (UInt_t iHost=0; iHost<fHostsForGuestDetectors.size(); iHost++){
      if(jDet<NDet){
        // Remove guest detector in case it's found in data
        for (UInt_t iGuest=0;iGuest<fGuestDetectorIndices[iHost].size();iGuest++) {
          if(iRaw>=0 && iRaw == fGuestDetectorIndices[iHost].at(iGuest) && jDet<NDet) {
            std::cout << "*** Guest Detector " << fGuestDetectorNames[iHost].at(iGuest) << " found in data! It will be read as a standard detector.. ***" << std::endl;
            RemoveGuestDetector(iHost,iGuest);
          }
        }
      }
      else if(0<=(Int_t)jDet-(Int_t)NDet-(Int_t)NGuestsInPreviousHosts && jDet-NDet-NGuestsInPreviousHosts<fGuestDetectorIndices[iHost].size()) iRaw = fGuestDetectorIndices[iHost].at(jDet-NDet-NGuestsInPreviousHosts);
      if(fRecoRawTable[iRaw]==fHostsForGuestDetectors[iHost] && iDetHost[iHost]==-1) iDetHost[iHost] = iDet; //Host detector found in data
      NGuestsInPreviousHosts += fGuestDetectorIndices[iHost].size();
    }
    //---------------------------------------------------------//
    EventHeader->SetDetectorID(iRaw);
    UInt_t Offset = *(fpDetectorTable+iDet)&M_DETECTOROFFSET;
    UInt_t NextOffset = (UInt_t)iDet + 1 < NDet ? *(fpDetectorTable+iDet+1)&M_DETECTOROFFSET : EventHeader->GetEventLength() - 1;
    UInt_t *SubDetData = pDataBuffer + Offset;
    if (iDet==0){
      ULong64_t NBytes = 0;
      if(fEventOffsets && fiCurrentEventInFile<fNEventsInFile) NBytes = fEventOffsets[fiCurrentEventInFile]*4;
      EventHeader->SetStartByte(NBytes);
    }

    // Append to EventHeader the trigger-related information
    if(!isL0SpecialFrame(EventHeader->GetTriggerType()<<2)){ // Physics triggers
      if(fRecoRawTable[iRaw]=="L0TP") EventHeader->SetL0TPData(SubDetData);
      if(fRecoRawTable[iRaw]=="L1TP") EventHeader->SetL1TPData(SubDetData);
      if(fRecoRawTable[iRaw]=="L2EB") EventHeader->SetL2EBData(SubDetData);
    }
    else { // SpecialTrigger block
      if(fRecoRawTable[iRaw]=="L0TP") EventHeader->SetL0TPSpecialTrigger(SubDetData);
      //if(fRecoRawTable[iRaw]=="L1TP") EventHeader->SetL1TPSpecialTrigger(SubDetData); //initialized from DIM block
      //if(fRecoRawTable[iRaw]=="L2EB") EventHeader->SetL2EBSpecialTrigger(SubDetData); //initialized from DIM block
      if(fRecoRawTable[iRaw]=="DIM") { //DIM block must be decoded as last detector
        DIMBlockStart = SubDetData;
        DIMBlockEnd   = pDataBuffer+NextOffset;
      }
    }
    if(fRawDecoders[iRaw] && fRawDecoders[iRaw]->GetDecoder()){
      // Check if the detector is a guest or a host
      fRawDecoders[iRaw]->GetDecoder()->SetIsAGuestOrHostDetector(false);
      for (UInt_t iHost=0; iHost<fHostsForGuestDetectors.size(); iHost++) {
        if(fHostsForGuestDetectors[iHost] == fRecoRawTable[iRaw]) fRawDecoders[iRaw]->GetDecoder()->SetIsAGuestOrHostDetector(true);
        for(UInt_t iGuest=0;iGuest<fGuestDetectorNames[iHost].size();iGuest++) {
          if(fGuestDetectorNames[iHost].at(iGuest) == fRecoRawTable[iRaw]) fRawDecoders[iRaw]->GetDecoder()->SetIsAGuestOrHostDetector(true);
        }
      }
      Int_t iReco = fRawRecoIndex[iRaw];
      fTimer.StartTimer(fRawDecTimers[iReco+1]); // 0 = all rawdecoders
      fRawDecoders[iRaw]->Reset();
      fInputEvents[iReco] = fRawDecoders[iRaw]->DecodeNextEvent(SubDetData, EventHeader, pDataBuffer + NextOffset);


#ifdef ONLINEHLT
      //HLT
      if (not ((EventHeader->GetTriggerType() & 0xff) == 0x08) and // TRIGGER_L2_TIMEOUT
          not ((EventHeader->GetTriggerType() & 0xff) == 0x30) and // TRIGGER_L0_CALIBRATION1_LKR
          not ((EventHeader->GetTriggerType() & 0xff) == 0x31) and // TRIGGER_L0_PULSER_GTK
          not ((EventHeader->GetTriggerType() & 0xff) == 0x32) and // TRIGGER_L0_CALIBRATION3_LKR
          not (isL0SpecialFrame(EventHeader->GetTriggerType() << 2)) and
          (EventHeader->GetTriggerType() & 0x1 or EventHeader->GetTriggerType() & 0x10)) { //TRIGGER_L0_PHYSICS_TYPE TRIGGER_L0_CONTROL_TYPE
        if (fRecoRawTable[iRaw] == "Cedar" or fRecoRawTable[iRaw] == "LAV" or fRecoRawTable[iRaw] == "Spectrometer") {
          UInt_t* rolling = SubDetData;
          UInt_t* end_data =  pDataBuffer + NextOffset;
          uint fragments = 0;
          while (rolling < end_data) { // LOOP ON ALL SUBDETECTOR BLOCKS
            na62::l0::MEPFragment_HDR* fragment_header = (na62::l0::MEPFragment_HDR*) rolling;
            subevent->addFragment(fragment_header);
            //fHLTLib.m_subevent->addFragment(fragment_header);
            rolling += fragment_header->eventLength_ / 4;
            ++fragments;
          }

          EventIndexer event(subevent, EventHeader->GetTimeStamp());
          event.setFinetime(EventHeader->GetFineTime());
          na62::DecoderHandler decoded_event(&event);

          using functype_algo_process = uint_fast8_t(uint, na62::DecoderHandler&, L1InfoToStorage*);
          if (fRecoRawTable[iRaw] == "Cedar") {
            std::function<functype_algo_process> handle_process_ktag{reinterpret_cast<functype_algo_process*>(dlsym(fHLTLib->get(), "process_ktag"))};
            cedar_trigger = handle_process_ktag(0, decoded_event, l1Info);

            EventHeader->GetHLTEvent()->SetKTAGResponse(cedar_trigger);

            EventHeader->GetHLTEvent()->SetKTAGSectors(l1Info->getL1KTAGNSectorsL0TP(), l1Info->getL1KTAGNSectorsCHOD());
            EventHeader->GetHLTEvent()->SetKTAGProcessInfo(l1Info->isL1KTAGProcessed(), l1Info->isL1KTAGEmptyPacket(), l1Info->isL1KTAGBadData());
            // std::cout <<"Ktag trigger word mask 0: "<< l1Info.getL1KTAGTrgWrd(0) << " and cedar trigger" << cedar_trigger << std::endl;

          } else if (fRecoRawTable[iRaw] == "LAV") {
            std::function<functype_algo_process> handle_process_lav{reinterpret_cast<functype_algo_process*>(dlsym(fHLTLib->get(), "process_lav"))};
            lav_trigger = handle_process_lav(0, decoded_event, l1Info);

            EventHeader->GetHLTEvent()->SetLAVResponse(lav_trigger);
            //std::cout << "Lav sectors " << l1Info->getL1LAVNHits() << std::endl;
            EventHeader->GetHLTEvent()->SetLAVHits(l1Info->getL1LAVNHits());
            EventHeader->GetHLTEvent()->SetLAVProcessInfo(l1Info->isL1LAVProcessed(), l1Info->isL1LAVEmptyPacket(), l1Info->isL1LAVBadData());

          } else if (fRecoRawTable[iRaw] == "Spectrometer") {
            EventHeader->GetHLTEvent()->Clear();
            using functype_create_straw_algo = na62::StrawAlgo*();
            std::function<functype_create_straw_algo> create_straw_algo{reinterpret_cast<functype_create_straw_algo*>(dlsym(fHLTLib->get(), "create_strawalgo"))};

            na62::StrawAlgo* runstraw = create_straw_algo();

            using functype_straw_process = uint_fast8_t(uint, na62::DecoderHandler&, L1InfoToStorage*, na62::StrawAlgo*);
            std::function<functype_straw_process> handle_process_straw{reinterpret_cast<functype_straw_process*>(dlsym(fHLTLib->get(), "process_straw"))};
            straw_trigger = handle_process_straw(0, decoded_event, l1Info, runstraw);

            EventHeader->GetHLTEvent()->SetSTRAWResponse(straw_trigger);
            EventHeader->GetHLTEvent()->SetSTRAWNTracks(l1Info->getL1StrawNTracks());
            EventHeader->GetHLTEvent()->SetSTRAWProcessInfo(l1Info->isL1StrawProcessed(), l1Info->isL1StrawEmptyPacket(), l1Info->isL1StrawBadData(), l1Info->isL1StrawOverflow());

            for (uint track_index = 0; track_index < l1Info->getL1StrawNTracks(); ++track_index) {
              na62::Track& STRAWTrack = runstraw->getTracks(track_index);
              HLTTrack track_temp;

              track_temp.SetHLTTrackID(track_index);
              track_temp.SetNHoughIntersections(STRAWTrack.ncentrali);
              track_temp.SetNHoughAdjacents(STRAWTrack.nlaterali);
              track_temp.Setdydz(STRAWTrack.my);
              track_temp.SetQy(STRAWTrack.qy);
              track_temp.SetdxdzBeforeMagnet(STRAWTrack.m1x);
              track_temp.SetdxdzAfterMagnet(STRAWTrack.m2x);
              track_temp.SetQxBeforeMagnet(STRAWTrack.q1x);
              track_temp.SetQxAfterMagnet(STRAWTrack.q2x);
              track_temp.SetZVertex(STRAWTrack.zvertex);
              track_temp.SetPz(STRAWTrack.pz);
              track_temp.SetCDA(STRAWTrack.cda);
              track_temp.SetTrailing(STRAWTrack.trailing);
              track_temp.SetMomentumBeforeMagnet(STRAWTrack.m1x, STRAWTrack.my, STRAWTrack.pz);
              track_temp.SetMomentumAfterMagnet(STRAWTrack.m2x, STRAWTrack.my, STRAWTrack.pz);
              track_temp.SetVertex(STRAWTrack.m1x, STRAWTrack.q1x, STRAWTrack.my, STRAWTrack.qy, STRAWTrack.zvertex);
              EventHeader->GetHLTEvent()->AddHLTTracks(track_temp);
              HLTTrack TrackHLT = EventHeader->GetHLTEvent()->GetHLTTracks().back();
            }

            using functype_delete_straw_algo = void(na62::StrawAlgo*);
            std::function<functype_delete_straw_algo> destroy_strawalgo{reinterpret_cast<functype_delete_straw_algo*>(dlsym(fHLTLib->get(), "destroy_strawalgo"))};
            destroy_strawalgo(runstraw);
          }
          subevent->reset();
        }
      }
#endif

      fTimer.StopTimer(fRawDecTimers[iReco+1]);  // 0 = all rawdecoders
      for(UInt_t iMezzanine=0;iMezzanine<fRawDecoders[iRaw]->GetDecoder()->GetNROMezzanines();iMezzanine++){
        fNCriticalErrors[iRaw][iMezzanine] = fRawDecoders[iRaw]->GetDecoder()->GetNCriticalErrors(iMezzanine);
        fNQualityWarnings[iRaw][iMezzanine] = fRawDecoders[iRaw]->GetDecoder()->GetNQualityWarnings(iMezzanine);
        fNWrongSlots[iRaw][iMezzanine] = fRawDecoders[iRaw]->GetDecoder()->GetNWrongSlots(iMezzanine);
        fNTotalSlots[iRaw][iMezzanine] = fRawDecoders[iRaw]->GetDecoder()->GetNTotalSlots(iMezzanine);
        fNHitsFromMaskedChannels[iRaw][iMezzanine] = fRawDecoders[iRaw]->GetDecoder()->GetNHitsFromMaskedChannels(iMezzanine);
        if(fNCriticalErrors[iRaw][iMezzanine]) EventHeader->UpdateEventQualityMask(iRaw); //set event mask in the raw header
        if(fNWrongSlots[iRaw][iMezzanine])     EventHeader->UpdateEventQualityMask(iRaw); //set event mask in the raw header
        if((fSkipEventsMask&(1<<iRaw))){ //check of subdetector is enabled
          if(NMaxCriticalErrors<fNCriticalErrors[iRaw][iMezzanine]) NMaxCriticalErrors = fNCriticalErrors[iRaw][iMezzanine];
          if(NMaxQualityWarnings<fNQualityWarnings[iRaw][iMezzanine]) NMaxQualityWarnings = fNQualityWarnings[iRaw][iMezzanine];
          if(NMaxWrongSlots<fNWrongSlots[iRaw][iMezzanine]) NMaxWrongSlots = fNWrongSlots[iRaw][iMezzanine];
          if(NMaxHitsFromMaskedChannels<fNHitsFromMaskedChannels[iRaw][iMezzanine]) NMaxHitsFromMaskedChannels = fNHitsFromMaskedChannels[iRaw][iMezzanine];
        }
      }
    }
  }
  // Decode the DIM block
  if(DIMBlockEnd-DIMBlockStart>0) DecodeDIMBlock(DIMBlockStart,DIMBlockEnd);

  std::stringstream WarningStream;
  if(NMaxCriticalErrors>0){
    fOutputStatus = kStatusCriticalError;
    WarningStream << "[NA62Reconstruction] WARNING: At least one critical error detected!";
    if(fSkipEventsLevel>=kStatusCriticalError) WarningStream << " Skipping event";
    WarningStream <<" [EventSeq: " << fNProcessedEventsInFile << " EventID: " << std::hex << EventHeader->GetEventNumber() << std::dec << "]" << std::endl;
    WarningStream << "[NA62Reconstruction]          --> Detectors with critical errors:";
    for(UInt_t iRaw=0;iRaw<MAXNDETECTORS;iRaw++){
      if(fRawDecoders[iRaw] && fRawDecoders[iRaw]->GetDecoder()){
        for(UInt_t iMezzanine=0;iMezzanine<fRawDecoders[iRaw]->GetDecoder()->GetNROMezzanines();iMezzanine++){
          if(fNCriticalErrors[iRaw][iMezzanine]) {
            WarningStream << " " << fRecoRawTable[iRaw] << "_ROb" << iMezzanine/fRawDecoders[iRaw]->GetDecoder()->GetNROMezzaninesPerFullBoard() << ","<< iMezzanine%fRawDecoders[iRaw]->GetDecoder()->GetNROMezzaninesPerFullBoard() << "=" << fNCriticalErrors[iRaw][iMezzanine];
          }
        }
      }
    }
    WarningStream << std::endl;
    PrintWarning(WarningStream);
    fNCriticalEventsInFile++;
    fNCriticalEventsInTotal++;
  }
  if(NMaxWrongSlots>0){
    fOutputStatus = kStatusInconsistentTS;
    WarningStream << "[NA62Reconstruction] WARNING: At least one slot inconsistent with the TriggerTS!";
    if(fSkipEventsLevel>=kStatusInconsistentTS) WarningStream << " Skipping event";
    WarningStream << " [EventSeq: " << fNProcessedEventsInFile << " EventID: " << std::hex << EventHeader->GetEventNumber() << std::dec << "]" << std::endl;
    WarningStream << "[NA62Reconstruction]          --> Detectors with inconsistent slots:";
    for(UInt_t iRaw=0;iRaw<MAXNDETECTORS;iRaw++){
      if(fRawDecoders[iRaw] && fRawDecoders[iRaw]->GetDecoder()){
        for(UInt_t iMezzanine=0;iMezzanine<fRawDecoders[iRaw]->GetDecoder()->GetNROMezzanines();iMezzanine++){
          if(fNWrongSlots[iRaw][iMezzanine]) {
            WarningStream << " " << fRecoRawTable[iRaw] << "_ROb" << iMezzanine/fRawDecoders[iRaw]->GetDecoder()->GetNROMezzaninesPerFullBoard() << ","<< iMezzanine%fRawDecoders[iRaw]->GetDecoder()->GetNROMezzaninesPerFullBoard() << "=" << fNWrongSlots[iRaw][iMezzanine]<<"/"<<fNTotalSlots[iRaw][iMezzanine];
          }
        }
      }
    }
    WarningStream << std::endl;
    PrintWarning(WarningStream);
  }
  if(NMaxHitsFromMaskedChannels>0){
    fOutputStatus = kStatusMaskedChannel;
    WarningStream << "[NA62Reconstruction] WARNING: At least one hit from a masked channel";
    if(fSkipEventsLevel>=kStatusMaskedChannel) WarningStream << " Skipping event";
    WarningStream << " [EventSeq: " << fNProcessedEventsInFile << " EventID: " << std::hex << EventHeader->GetEventNumber() << std::dec << "]" << std::endl;
    WarningStream << "[NA62Reconstruction]          --> Detectors with masked channels:";
    for(UInt_t iRaw=0;iRaw<MAXNDETECTORS;iRaw++){
      if(fRawDecoders[iRaw] && fRawDecoders[iRaw]->GetDecoder()){
        for(UInt_t iMezzanine=0;iMezzanine<fRawDecoders[iRaw]->GetDecoder()->GetNROMezzanines();iMezzanine++){
          if(fNHitsFromMaskedChannels[iRaw][iMezzanine]) {
            WarningStream << " " << fRecoRawTable[iRaw] << "_ROb" << iMezzanine/fRawDecoders[iRaw]->GetDecoder()->GetNROMezzaninesPerFullBoard() << ","<< iMezzanine%fRawDecoders[iRaw]->GetDecoder()->GetNROMezzaninesPerFullBoard() << "=" << fNHitsFromMaskedChannels[iRaw][iMezzanine];
          }
        }
      }
    }
    WarningStream << std::endl;
    PrintWarning(WarningStream);
  }
  //consistency check: same handling of special triggers for all the subdetectors
  Bool_t BadInputEvent = kFALSE;
  for(Int_t iReco=0; iReco<fNReconstructions-1;iReco++) {
    if(!fInputEvents[iReco]) continue;
    if(!fInputEvents[iReco+1]) continue;
    if(fInputEvents[iReco]->GetID()!=fInputEvents[iReco+1]->GetID()) {
      BadInputEvent = kTRUE;
    }
    if(fInputEvents[iReco]->IsA()->InheritsFrom("TSpecialTriggerEvent") && !fInputEvents[iReco+1]->IsA()->InheritsFrom("TSpecialTriggerEvent")) BadInputEvent = kTRUE;
    if(!fInputEvents[iReco]->IsA()->InheritsFrom("TSpecialTriggerEvent") && fInputEvents[iReco+1]->IsA()->InheritsFrom("TSpecialTriggerEvent")) BadInputEvent = kTRUE;
  }
  if(BadInputEvent){
    cerr_en(fWarningsLevel,WARN_EVT) << "[NA62Reconstruction] WARNING: Event type mismatching in file '" << fCurrentFileName << "'! Skipping event.." << std::endl;
    for(Int_t iReco=0; iReco<fNReconstructions;iReco++) {
      if(!fInputEvents[iReco]) continue;
      cerr_en(fWarningsLevel,WARN_EVT) << "[NA62Reconstruction]          --> InputEvents[" << iReco << "]: " << fRecoSequence[iReco] << " ID: " << fInputEvents[iReco]->GetID();
      cerr_en(fWarningsLevel,WARN_EVT) << " EventType: " << fInputEvents[iReco]->GetName() << std::endl;
    }
    fOutputStatus = kStatusEventTypeMismatch;
  }

  // -----------------------------  OM histos ------------------------------ //
  if(!isL0SpecialFrame(EventHeader->GetTriggerType()<<2)){ //only physics/random/calibration triggers studied
    //============ L0TP and L1TP info monitoring (it must be before the RequestedTrigger rejection)
    fHDeltaTimeStamp->Fill((EventHeader->GetL0TPData()->GetTimeStamp()-EventHeader->GetL0TPData()->GetPreviousTimeStamp())*ClockPeriod);
    fHDeltaTimeStampStored->Fill((EventHeader->GetTimeStamp()-EventHeader->GetPreviousTimeStamp())*ClockPeriod);
    if(EventHeader->GetL0TPData()->GetDataType()&0x1){ //physics events
      for(Int_t iL0Bit=0;iL0Bit<16;iL0Bit++){
        if(EventHeader->GetL0TPData()->GetTriggerFlags()&(1<<iL0Bit)){
          fHL0TriggerFlags->Fill(iL0Bit,EventHeader->GetL0TPData()->GetDataType());
          for(Int_t iL1Bit=0;iL1Bit<8;iL1Bit++){
            if(((EventHeader->GetTriggerType()&0xff00)>>8)&(1<<iL1Bit))  fHL1TriggerFlags->Fill(iL0Bit,iL1Bit);
          }
        }
      }
    }
    //============ Info for L1 Rejection and Input/Output Occupancy plots vs BurstID
    if(EventHeader->GetTriggerType()&0x8000){ //L1 autopass bit ON
      fHL1Counters->Fill(kL1APTotal);
      if(EventHeader->GetL0TPData()->GetDataType()&0x2){
        fHL1Counters->Fill(kL1APPeriodics);
      }
      if(EventHeader->GetL0TPData()->GetDataType()&0x10){
        fHL1Counters->Fill(kL1APControl);
      }
      if(EventHeader->GetL0TPData()->GetDataType()&0x1){ //physics events
        if(EventHeader->GetTriggerType()&0x0100) fHL1Counters->Fill(kL1APPhysicsPassed);
        fHL1Counters->Fill(kL1APPhysics);
        std::vector<L1MaskBlock> L1Infos = EventHeader->GetL1TPData()->GetL0Masks();
        UInt_t nL0MasksOn = L1Infos.size();
        // Loop on the active masks of L0
        for (UInt_t iMask=0; iMask<nL0MasksOn; iMask++) {
          Int_t l0MaskID = (Int_t) L1Infos.at(iMask).GetL0MaskID();
          Int_t l1TriggerWord = (Int_t) L1Infos.at(iMask).GetL1TriggerWord();
          if(EventHeader->GetL0TPData()->GetTriggerFlags()&(1<<l0MaskID)){
            if(l1TriggerWord==1) fHL1Counters->Fill(kL1APPhysicsPassedPerMask+l0MaskID);
            fHL1Counters->Fill(kL1APPhysicsPerMask+l0MaskID);
          }
        }
      }
    }

    if(EventHeader->GetL0TPData()->GetDataType()&0x1){ //physics events
      std::vector<L1MaskBlock> L1Infos = EventHeader->GetL1TPData()->GetL0Masks();
      UInt_t nL0MasksOn = L1Infos.size();
      // Loop on the active masks of L0
      for (UInt_t iMask=0; iMask<nL0MasksOn; iMask++) {
        Int_t l0MaskID = (Int_t) L1Infos.at(iMask).GetL0MaskID();
        Int_t l1TriggerWord = (Int_t) L1Infos.at(iMask).GetL1TriggerWord();
        if(EventHeader->GetL0TPData()->GetTriggerFlags()&(1<<l0MaskID) && (l1TriggerWord==1)){
          fHL1Counters->Fill(kL1PhysicsPassedPerMask+l0MaskID);
        }
      }
    }

    //============ Event quality info monitoring
    fHEventTimeStamp->Fill(EventHeader->GetTimeStamp()*ClockPeriod);
    if(fOutputStatus!=kStatusGood) fHSkippedEventTimeStamp->Fill(EventHeader->GetTimeStamp()*ClockPeriod);
    for(Int_t iRaw=0;iRaw<MAXNDETECTORS;iRaw++){
      if(fRawDecoders[iRaw] && fRawDecoders[iRaw]->GetDecoder()){
        Int_t NCriticalErrors = 0, NQualityWarnings = 0;
        for(UInt_t iMezzanine=0;iMezzanine<fRawDecoders[iRaw]->GetDecoder()->GetNROMezzanines();iMezzanine++){
          NCriticalErrors+=fNCriticalErrors[iRaw][iMezzanine];
          NCriticalErrors+=fNWrongSlots[iRaw][iMezzanine];
          NQualityWarnings+=fNQualityWarnings[iRaw][iMezzanine];
        }
        if(NCriticalErrors)  fHNCriticalEventsPerDetector->Fill(iRaw);
        if(NQualityWarnings) fHNEventsWithQualityWarningsPerDetector->Fill(iRaw);
      }
    }
    //============ Expert plots for L0 TimeStamp monitoring
    std::bitset< 32 > TimeStampBits = ( EventHeader->GetTimeStamp() ) ;
    for(int iBit = 0 ; iBit <32 ; ++iBit){
      size_t index = 31-iBit ;
      if(TimeStampBits[index] == 1) fHEventTimeStampBits->Fill(iBit-31) ;
    }
    fHEventTimeStamp16->Fill(EventHeader->GetTimeStamp()%16) ;
    fHEventTimeStamp128->Fill(EventHeader->GetTimeStamp()%128) ;
    fHEventTimeStamp1024->Fill(EventHeader->GetTimeStamp()%1024) ;
    // ----------------------------------------------------------------------- //
  }
  // ----------------------------------------------------------------------- //

  UInt_t ELen = EventHeader->GetEventLength();
  if(pDataBuffer+ELen-DataBuffer.data()<=(Int_t)DataBuffer.size()){
    pDataBuffer += ELen;
    if(((*(pDataBuffer-1)&0xff000000) != 0x00000000)||(*(pDataBuffer-1)&0x00ffffff) != EventHeader->GetEventNumber()) {
      //Exception ("Bad trailer");
      cerr_en(fWarningsLevel,WARN_EVT) << "[NA62Reconstruction] WARNING: Bad trailer in file '" << fCurrentFileName << "'";
      cerr_en(fWarningsLevel,WARN_EVT) << " [EventSeq: " << fNProcessedEventsInFile << " EventID: " << std::hex << EventHeader->GetEventNumber() << std::dec << "]" << std::endl;
    }
  }
  fPreviousEventNumber = EventHeader->GetEventNumber();

  fTimer.StopTimer(fRawDecTimers[0]); // all reco
  if(fEventAlreadyProcessed && fiCurrentEventInFile<fNEventsInFile) fEventAlreadyProcessed[fiCurrentEventInFile] = kTRUE; // set this event as already processed

  //#ifdef ONLINEHLT
  //  delete_subevent(subevent);
  //  delete_l1storage(l1Info);
  //#endif
  return kTRUE;
}

Bool_t NA62Reconstruction::NextEvent(){
  /// \MemberDescr
  /// Single event management and processing decision.
  /// \EndMemberDescr

  errno = 0; // Reset errno
  fTimer.StartTimer(fGlobalTimers[0]);
  EventHeader* EventHeader = NA62RecoManager::GetInstance()->GetEventHeader();

  if(!fContinuousReading && (fNProcessedEventsInTotal >= (UInt_t)fNEvt || fNReconstructions == 0)){
    fTimer.StopTimer(fGlobalTimers[0],false);
    return kFALSE;
  }

  if (fIsRawData){
    if(!HandleFile()){
      fTimer.StopTimer(fGlobalTimers[0],false);
      return kFALSE;
    }
#ifdef ONLINEHLT
    if(!fNReadEventsInFile){
      // load the configuration files required by the L1 algos
      fHLTLib->handleLavConfig(NA62ConditionsService::GetInstance()->GetFullPath("LAV-RawDecoderSettings.dat").Data());
      fHLTLib->handleStrawConfig(NA62ConditionsService::GetInstance()->GetFullPath("Spectrometer-RawDecoderSettings.dat").Data(),
          NA62ConditionsService::GetInstance()->GetFullPath("Spectrometer-CoarseT0.HLT.dat").Data(),
          NA62ConditionsService::GetInstance()->GetFullPath("Spectrometer-MagicT0.HLT.dat").Data());
    }
#endif
    if(fNReadEventsInFile == 1 && fTriggerTypes && fNEventsInFile>0 && (fTriggerTypes[fNEventsInFile-1]&0xff)==0x23) ReadEOB(); // Read EOB just after reading the SOB
    else {
      if (!DecodeEvent(fpDataBuffer,fDataBuffer)) {
        fTimer.StopTimer(fGlobalTimers[0],false);
        return kFALSE;
      }
      if(fiCurrentEventInFile>=fNEventsInFile) cerr_en(fWarningsLevel,WARN_EVT) << "[NA62Reconstruction] WARNING: The current event index ("<<fiCurrentEventInFile<<") is bigger than the expected number of events in file (" << fNEventsInFile << ")!" << std::endl;
      fiCurrentEventInFile++;
    }
  }
  else if(FindMCChain("MC")){
    for(UInt_t iChain=0; iChain<fMCChains.size(); iChain++){
      if(fContinuousReading && (fNProcessedEventsInTotal >= (UInt_t)fNEvt)){
        ReadInputList(fInputListFileName,-1);
        for(Int_t iFile = 0; iFile < fInputFileNameList->GetEntries(); iFile++)
          fMCChains[iChain]->AddFile(static_cast<TObjString*>(fInputFileNameList->At(iFile))->GetString().Data());
        fNEvt = fMCChains[iChain]->GetEntries();
      }
    }
    if(fiFile==-1){
      if(FindMCChain("Streams")) FindMCChain("Streams")->SetBranchAddress("Stream",&(fStream));
      for(Int_t iReco = 0; iReco < fNReconstructions; iReco++){
        FindMCChain("MC")->SetBranchAddress(fRecoSequence[iReco].Data(),&(fInputEvents[iReco]));
      }
    }
    FindMCChain("MC")->GetEntry(fNProcessedEventsInTotal+fJumpNEvt);

    // Set EventHeader info
    if(FindMCChain("Streams")){
      FindMCChain("Streams")->GetEntry(fiFile+1);
      EventHeader->SetRunID    (fStream->GetMCInfo().GetRunNumber().size()==0 ? -1 : fStream->GetMCInfo().GetRunNumber()[0]);
      EventHeader->SetBurstID  (fStream->GetMCInfo().GetRandomSeed().size()==0 ? -1 : fStream->GetMCInfo().GetRandomSeed()[0]);
      EventHeader->SetBurstTime(fStream->GetMCInfo().GetRandomSeed().size()==0 ? -1 : fBurstTime);
    }
    EventHeader->SetEventNumber(fMCTruthEvent->GetEventNumber());
    EventHeader->SetTimeStamp(fMCTruthEvent->GetEventNumber());

    // Set trigger information for MC events (all triggers ON)
    EventHeader->SetFineTime(fMCTruthEvent->GetEventNumber()%256);
    EventHeader->SetTriggerType(0xFFFFFFFF);
    EventHeader->GetL0TPData()->SetDataType(0xFF);
    EventHeader->GetL0TPData()->SetTriggerType(0xFF);
    EventHeader->GetL0TPData()->SetTriggerFlags(0xFFFF);
    EventHeader->GetL0TPData()->SetReferenceFineTime(EventHeader->GetFineTime());
    EventHeader->GetL1TPData()->SetTimeStamp(0xFFFFFFFFFFFFFFFF);
    EventHeader->GetL1TPData()->SetL1ReferenceFineTime(EventHeader->GetFineTime());
    EventHeader->GetL2EBData()->SetTimeStamp(0xFFFFFFFFFFFFFFFF);
    EventHeader->GetL2EBData()->SetL2ReferenceFineTime(EventHeader->GetFineTime());

    Int_t fileNumber = FindMCChain("MC")->GetTreeNumber();
    fCurrentFileName = FindMCChain("MC")->GetFile()->GetName();
    if(fileNumber != fiFile) {
      WriteFileName(fCurrentFileName);
      fiFile = fileNumber;
      // Set random seeds to ensure reproducibility
      for(Int_t iDigitizer = 0; iDigitizer < fNDigitizers; iDigitizer++){
        fDigitizers[iDigitizer]->SetSeed(fGlobalSeed+fStream->GetMCInfo().GetRandomSeed()[0]);
      }
    }
  }
  else{
    Exception("No MCChain built");
  }

  if((fIsRawData && fNReadEventsInTotal == 1) || (!fIsRawData && !fNProcessedEventsInTotal)) CompleteInit();
  if(fIsRawData && fNReadEventsInFile  == 1) StartOfBurst();

  if(fOutputStatus!=kStatusGood && fOutputStatus<=fSkipEventsLevel) { // Do not process the event
    if(fOutputStatus!=kStatusAlreadyProcessed) { //increment skip event counters
      if(fOutputStatus==kStatusDownscaling) {
        fNSkippedDownscaledEventsInFile++;
        fNSkippedDownscaledEventsInTotal++;
      }
      else if(fOutputStatus==kStatusUnrequested) {
        fNSkippedUnrequestedEventsInFile++;
        fNSkippedUnrequestedEventsInTotal++;
      }
      else{
        fNSkippedCorruptedEventsInFile++;
        fNSkippedCorruptedEventsInTotal++;
      }
      fNSkippedEventsInFile++;
      fNSkippedEventsInTotal++;
    }
    fTimer.StopTimer(fGlobalTimers[0]);
    return kTRUE;
  }

  // process RequestedTriggers only
  Bool_t RequestedTrigger = true;
  if(!isL0SpecialFrame(EventHeader->GetTriggerType()<<2) && ((EventHeader->GetL0TPData()->GetDataType()&0x1)||(EventHeader->GetL0TPData()->GetDataType()&0x10)||(EventHeader->GetL0TPData()->GetDataType()&0x2)||(EventHeader->GetL0TPData()->GetDataType()&0x8))) RequestedTrigger = false; //physics/CTRL/periodics/calibration
  if(EventHeader->GetL0TPData()->GetDataType()&0x1  && (EventHeader->GetL0TPData()->GetTriggerFlags()&(fRequestedTriggersFlag&0xffff))) RequestedTrigger = true;
  if(EventHeader->GetL0TPData()->GetDataType()&0x10 && (fRequestedTriggersFlag&0x10000)) RequestedTrigger = true;
  if(EventHeader->GetL0TPData()->GetDataType()&0x2  && (fRequestedTriggersFlag&0x20000)) RequestedTrigger = true;
  if(EventHeader->GetL0TPData()->GetDataType()&0x8  && (fRequestedTriggersFlag&0x40000)) RequestedTrigger = true;

  if(!RequestedTrigger){
    fNSkippedUnrequestedEventsInFile++;
    fNSkippedUnrequestedEventsInTotal++;
    fNSkippedEventsInFile++;
    fNSkippedEventsInTotal++;
    fTimer.StopTimer(fGlobalTimers[0]);
    return kTRUE;
  }

  if((fNProcessedEventsInTotal+1)%fDisplayPeriod == 0){
    std::pair<Double_t,Double_t> memoryUsage = GetMemoryUsage();
    std::cout << "[" << TimeString() << "] Scanning event " << fNProcessedEventsInTotal+1 << " [Memory usage -> Virtual: " << memoryUsage.first << " MB, Resident: " << memoryUsage.second << " MB]" << std::endl;
  }

  ProcessEvent();

  // Memory usage monitoring: check memory every N processed events (VM and RM from first N events is always stored)
  Int_t MemDownscalingFactor = 10;
  if(fNProcessedEventsInTotal<(UInt_t)MemDownscalingFactor){
    std::pair<Double_t,Double_t> memoryUsage = GetMemoryUsage();
    fGVirtMem->SetPoint(fNProcessedEventsInTotal, fNProcessedEventsInTotal, memoryUsage.first);
    fGResMem->SetPoint(fNProcessedEventsInTotal, fNProcessedEventsInTotal, memoryUsage.second);
    fGFileSize->SetPoint(fNProcessedEventsInTotal, fNProcessedEventsInTotal, GetFileSizeUsage());
    fGSystemFileSize->SetPoint(fNProcessedEventsInTotal, fNProcessedEventsInTotal, GetFileSizeUsage(2));
  }
  else if(!(fNProcessedEventsInTotal%MemDownscalingFactor)){
    std::pair<Double_t,Double_t> memoryUsage = GetMemoryUsage();
    fGVirtMem->SetPoint(fNProcessedEventsInTotal/MemDownscalingFactor+MemDownscalingFactor-1, fNProcessedEventsInTotal, memoryUsage.first);
    fGResMem->SetPoint(fNProcessedEventsInTotal/MemDownscalingFactor+MemDownscalingFactor-1, fNProcessedEventsInTotal, memoryUsage.second);
    fGFileSize->SetPoint(fNProcessedEventsInTotal/MemDownscalingFactor+MemDownscalingFactor-1, fNProcessedEventsInTotal, GetFileSizeUsage());
    fGSystemFileSize->SetPoint(fNProcessedEventsInTotal/MemDownscalingFactor+MemDownscalingFactor-1, fNProcessedEventsInTotal, GetFileSizeUsage(2));
  }

  if( ! fHTimingProfile ){ // now we know the number of timers we can book the histograms
    int nBin =  fTimer.GetNTimers();
    fHTimingProfile = new TProfile("RecoTimingProf","Timing of per event reconstruction",nBin,-0.5,((double)nBin)-0.5);
    fHTimingProfile->GetYaxis()->SetTitle("Average event time (ms)");
    fHTimingProfile->SetMarkerSize(0.4);
    fHTimingProfile->SetMarkerStyle(20);

    fHTiming2D = new TH2F("RecoTiming2D","Log of time of per event",nBin,-0.5,((double)nBin)-0.5,36,-2.5,6.5);
    fHTiming2D->GetYaxis()->SetTitle("Event time (log10(time/ms))");
    fHTimingProfile->GetXaxis()->SetBinLabel(1,TString("Set me"));

    for(unsigned int iTime = 0 ; iTime < fTimer.GetNTimers() ; ++iTime ){
      const TString & tName = fTimer.GetTimerName(iTime);
      fHTimingProfile->GetXaxis()->SetBinLabel(iTime+1,tName);
      fHTiming2D->GetXaxis()->SetBinLabel(iTime+1,tName);
    }
  }

  // Fill timing histogram here
  for(unsigned int iTime = 0 ; iTime < fTimer.GetNTimers() ; ++iTime ){
    if(fTimer.TimerGetElapsed(iTime)<0) continue; // Timer not used
    fHTimingProfile->Fill(iTime,fTimer.TimerGetElapsed(iTime));
    fHTiming2D->Fill(iTime,log10(fTimer.TimerGetElapsed(iTime)));
  }

  fNProcessedEventsInFile++;
  fNProcessedEventsInTotal++;
  if(EventHeader->GetL0TPData()->GetDataType()&0x1) {
    fNProcessedPhysicsTriggerEventsInFile++;
    fNProcessedPhysicsTriggerEventsInTotal++;
  }
  if(EventHeader->GetL0TPData()->GetDataType()&0x10){
    fNProcessedControlTriggerEventsInFile++;
    fNProcessedControlTriggerEventsInTotal++;
  }
  if(EventHeader->GetL0TPData()->GetDataType()&0x2 ){
    fNProcessedPeriodicTriggerEventsInFile++;
    fNProcessedPeriodicTriggerEventsInTotal++;
  }
  if(EventHeader->GetL0TPData()->GetDataType()&0x8 ){
    fNProcessedCalibrationTriggerEventsInFile++;
    fNProcessedCalibrationTriggerEventsInTotal++;
  }
  if(isL0SpecialFrame(EventHeader->GetTriggerType()<<2)){
    fNProcessedSpecialTriggerEventsInFile++;
    fNProcessedSpecialTriggerEventsInTotal++;
    if(isL0SOB(EventHeader->GetTriggerType()<<2)){ //SOB
      fNProcessedSOBEventsInFile++;
      fNProcessedSOBEventsInTotal++;
    }
    if(isL0EOB(EventHeader->GetTriggerType()<<2)){ //EOB
      fNProcessedEOBEventsInFile++;
      fNProcessedEOBEventsInTotal++;
    }
  }
  fTimer.StopTimer(fGlobalTimers[0]);

  if(fCheckErrno && errno) {
    cerr_en(fWarningsLevel,WARN_EVT) << "[NA62Reconstruction] WARNING: An internal error was detected: errno = " << errno;
    cerr_en(fWarningsLevel,WARN_EVT) << " [EventSeq: " << fNProcessedEventsInFile << " EventID: " << std::hex << EventHeader->GetEventNumber() << std::dec << "]" << std::endl;
  }

  return kTRUE;
}

TDetectorVEvent * NA62Reconstruction::Trigger(TDetectorVEvent*, Event*){
  /// \MemberDescr
  /// Fake trigger decision: implementation of NA62VReconstruction::Trigger is mandatory
  /// \EndMemberDescr
  return (TDetectorVEvent*)-1;
}

TRecoVEvent * NA62Reconstruction::ProcessEvent(TDetectorVEvent*, Event*){
  /// \MemberDescr
  /// Single event processing logic: it calls Digitization and Reconstruction modules, implementing a global
  /// trigger decision. It provides each Reconstruction module with fMCTruthEvent.
  /// If requested it stores the requested data on disk.
  /// \todo Improve the trigger decision logic and related information.
  /// \EndMemberDescr

  fHNEventsProcessedPerBurst->Fill(NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID());
  fHEventSize->Fill(NA62RecoManager::GetInstance()->GetEventHeader()->GetEventLength()*4./1024.); //(NumberOfWords*4)/1024

  Bool_t TriggerStatus = kTRUE;
  Int_t iTrigger = 0;
  Int_t NMaxHitsOutOfSlot = 0; //for debug
  while(TriggerStatus){
    if(!fIsRawData || (fOutputStage&kRecoEnabled)){
      fTimer.StartTimer(fDigiTimers[0]); // all digi + trigger
      for(Int_t iReco = 0; iReco < fNReconstructions; iReco++){
        fTimer.StartTimer(fDigiTimers[iReco+1]); // 0 = all digi
        fEvents[iReco] = fDigitizers[iReco]->ProcessEvent(fInputEvents[iReco]);
        if(fOutputStage==kStageBinary){
          fRawEncoders[iReco]->EncodeNextEvent(fEvents[iReco],fIsRawData);
          if(fRawEncoders[iReco]->GetEncoder() && NMaxHitsOutOfSlot<fRawEncoders[iReco]->GetEncoder()->GetNHitsOutOfSlot()){
            NMaxHitsOutOfSlot = fRawEncoders[iReco]->GetEncoder()->GetNHitsOutOfSlot();
          }
        }
        if(fOutputStage == kStageReco && 0){
          TDetectorVEvent * Trigger = fReconstructions[iReco]->Trigger(fEvents[iReco], fMCTruthEvent);
          TriggerStatus &= Trigger != 0;
          if(Trigger != 0)
            fEvents[iReco] = Trigger;
        }
        fTimer.StopTimer(fDigiTimers[iReco+1]); // 0 = all digi
      }
      fTimer.StopTimer(fDigiTimers[0]); // all digi + trigger
    }

    // Encoding event for L0TP data
    if(fOutputStage==kStageBinary) FindBinary("L0TP")->EncodeNextEvent(0,fIsRawData);

    if(NMaxHitsOutOfSlot>0){
      cerr_en(fWarningsLevel,WARN_EVT) << "[NA62Reconstruction] WARNING: At least one HIT (Out of Slot) inconsistent with the TriggerTS!" << std::endl;
    }
    if(TriggerStatus){
      iTrigger++;
      fTimer.StartTimer(fRecoTimers[0]); // all reco
      //consistency check: same handling of special triggers for all the subdetectors
      Bool_t BadEvent = kFALSE;
      if(fOutputStage != kStageBinary){
        for(Int_t iReco=0; iReco<fNReconstructions-1;iReco++) {
          if(!fEvents[iReco]) continue;
          if(!fEvents[iReco+1]) continue;
          if(fEvents[iReco]->GetID()!=fEvents[iReco+1]->GetID()) {
            BadEvent = kTRUE;
          }
          if(fEvents[iReco]->IsA()->InheritsFrom("TSpecialTriggerEvent") && !fEvents[iReco+1]->IsA()->InheritsFrom("TSpecialTriggerEvent")) BadEvent = kTRUE;
          if(!fEvents[iReco]->IsA()->InheritsFrom("TSpecialTriggerEvent") && fEvents[iReco+1]->IsA()->InheritsFrom("TSpecialTriggerEvent")) BadEvent = kTRUE;
        }
        if(BadEvent){
          cerr_en(fWarningsLevel,WARN_EVT) << "[NA62Reconstruction] WARNING: Event type mismatching in file '" << fCurrentFileName << "'! Skipping event.." << std::endl;
          for(Int_t iReco=0; iReco<fNReconstructions;iReco++) {
            if(!fEvents[iReco]) continue;
            cerr_en(fWarningsLevel,WARN_EVT) << "[NA62Reconstruction]          --> Events[" << iReco << "]: " << fRecoSequence[iReco] << " ID: " << fEvents[iReco]->GetID();
            cerr_en(fWarningsLevel,WARN_EVT) << " EventType: " << fEvents[iReco]->GetName() << std::endl;
          }
          fOutputStatus = kStatusEventTypeMismatch;
        }
      }
      for(Int_t iReco = 0; iReco < fNReconstructions; iReco++){
        if((fOutputStatus==kStatusGood || fOutputStatus>fSkipEventsLevel) && (fOutputStage&kRecoEnabled)){
          fTimer.StartTimer(fRecoTimers[iReco+1]); // 0 = all reco
          fRecoEvents[iReco] = fReconstructions[iReco]->ProcessEvent(fEvents[iReco], fMCTruthEvent);
          if(fSlimRecoEvents[iReco] && fRecoEvents[iReco]) fSlimRecoEvents[iReco]->FromReco(fRecoEvents[iReco]);
          fTimer.StopTimer(fRecoTimers[iReco+1]);  // 0 = all reco
        }
        if ((fOutputStatus==kStatusGood || fOutputStatus>fSkipEventsLevel) && ((fOutputStage&kRecoTreeEnabled) || (fOutputStage&kDigiTreeEnabled)) && !fContinuousReading){
          if(fInputEvents[iReco]->IsA()->InheritsFrom("TSpecialTriggerEvent")){
            if(FindOutputTree("SpecialTrigger")) FindOutputTree("SpecialTrigger")->SetBranchAddress(fRecoSequence[iReco].Data(),
                &(FindOutputBranchEvent("SpecialTrigger")->at(iReco)));
          }
          else{
            if(fOutputStage == kStageSlimReco && fSlimRecoEvents[iReco]){ // custom handling for slim events
              if(FindOutputTree(fOutputStageName)) FindOutputTree(fOutputStageName)->SetBranchAddress(fRecoSequence[iReco].Data(), &(fSlimRecoEvents[iReco]));
            }
            else{
              if(FindOutputTree(fOutputStageName)) FindOutputTree(fOutputStageName)->SetBranchAddress(fRecoSequence[iReco].Data(),
                  &(FindOutputBranchEvent(fOutputStageName)->at(iReco)));
            }
            if(fOutputStage == kStageRecoDigis && FindOutputTree("Digis")) {
              FindOutputTree("Digis")->SetBranchAddress(fRecoSequence[iReco].Data(),&(FindOutputBranchEvent("Digis")->at(iReco)));
            }
          }
        }
      }

      //FillTimes for T0-evaluation
      if(fInputEvents[0] && !fInputEvents[0]->IsA()->InheritsFrom("TSpecialTriggerEvent")) {
        FillTimes();
        EvaluateInstantaneousIntensity();
      }

      if ((fOutputStatus==kStatusGood || fOutputStatus>fSkipEventsLevel) && ((fOutputStage&kRecoTreeEnabled) || (fOutputStage&kDigiTreeEnabled)) && !fContinuousReading){
        Int_t WriteStatus = 9999;
        for(UInt_t iTree=0;iTree<fOutputTrees.size();iTree++){
          if(((TString)fOutputTrees[iTree]->GetName()).CompareTo("SpecialTrigger")==0 && fInputEvents[0] && fInputEvents[0]->IsA()->InheritsFrom("TSpecialTriggerEvent")){
            WriteStatus = fOutputTrees[iTree]->Fill();
          }
          if(((TString)fOutputTrees[iTree]->GetName()).CompareTo("SpecialTrigger")!=0 && fInputEvents[0] && !fInputEvents[0]->IsA()->InheritsFrom("TSpecialTriggerEvent")){
            WriteStatus = fOutputTrees[iTree]->Fill();
          }
          if(WriteStatus<=0) Exception("Error while writing the output file!",kWriteError); // Error in writing the output file (e.g. disk quota exceeded)
        }
      }
      if(fOutputStatus!=kStatusGood && fOutputStatus<=fSkipEventsLevel) { // Events is not written
        fNSkippedCorruptedEventsInFile++;
        fNSkippedCorruptedEventsInTotal++;
        fNSkippedEventsInFile++;
        fNSkippedEventsInTotal++;
      }
      //special trigger handling
      Int_t iFirstSubDet = 0;
      if(fNReconstructions>0){
        if (iFirstSubDet+1<fNReconstructions && !fReconstructions[iFirstSubDet]) iFirstSubDet++; //skipping disabled reconstructions
        if (fEvents[iFirstSubDet] && fEvents[iFirstSubDet]->IsA()->InheritsFrom("TSpecialTriggerEvent")) {
          if(fEvents[iFirstSubDet]->GetNHits()){
            //if(isL0SOB(static_cast<TSpecialTrigger *>( fEvents[iFirstSubDet]->GetHit(0))->GetTriggerType())) StartOfBurst();
            //if(isL0EOB(static_cast<TSpecialTrigger *>( fEvents[iFirstSubDet]->GetHit(0))->GetTriggerType())) EndOfBurst();
            CheckSpecialTriggerTimeStamps();
          }
        }
        fTimer.StopTimer(fRecoTimers[0]); // 0 = all reco
      }
    }
    //if(fOutputStage != kStageReco)
    TriggerStatus = kFALSE;
  }

  return 0;
}

void NA62Reconstruction::EndProcessing(){
  /// \MemberDescr
  /// It ensures that the output file is closed properly and the requested data is saved.
  /// Each Reconstruction module implements such a method and should save its private data (i.e. monitoring
  /// histograms) into its own directory (conventionally called SubDetectorNameMonitor).
  /// \sa RICHReconstruction::Init, RICHReconstruction::EndProcessing
  /// \EndMemberDescr

  fDataBuffer.clear();          // empty the vector
  fDataBuffer.shrink_to_fit();  // release the used memory

  // EvaluateTriggerDrift T0
  if(fIsRawData && fEvaluateTriggerDriftT0) EvaluateTriggerDriftT0();

  if (fOutputStage != kStageNone && fOutputStage != kStageBinary) { // save histograms
    fHistoFile->cd("/");
    for(Int_t iReco = 0; iReco < fNReconstructions; iReco++){
      fReconstructions[iReco]->EndProcessing();
      fHistoFile->cd("/");
    }
  }
  else if(fOutputStage==kStageBinary){
    for(Int_t iBinary = 0; iBinary < fNRawEncoders; iBinary++){
      fRawEncoders[iBinary]->End();
    }
  }
  for(UInt_t iTree=0;iTree<fOutputTrees.size();iTree++){
    Int_t WriteStatus = fOutputTrees[iTree]->Write();
    if(WriteStatus<=0) Exception("Error while writing the output file!",kWriteError); // Error in writing the output file (e.g. disk quota exceeded)
  }
  // Setting Stream reco attributes
  fStream->GetRecoInfo().SetRevision(GetCurrentGitRevision());
  fStream->GetRecoInfo().SetRunID(NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID());
  fStream->GetRecoInfo().SetBurstID(NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID());
  fStream->GetRecoInfo().SetBurstTime(NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstTime());
  fStream->GetRecoInfo().SetNReadEvents(fNReadEventsInFile);
  fStream->GetRecoInfo().SetNProcessedEvents(fNProcessedEventsInFile);
  fStream->GetRecoInfo().SetNSkippedEvents(fNSkippedEventsInFile);
  fStream->GetRecoInfo().SetNCriticalEvents(fNCriticalEventsInFile);
  fStream->GetRecoInfo().SetNPhysicsTriggerEvents(fNProcessedPhysicsTriggerEventsInFile);
  fStream->GetRecoInfo().SetNControlTriggerEvents(fNProcessedControlTriggerEventsInFile);
  fStream->GetRecoInfo().SetNPeriodicTriggerEvents(fNProcessedPeriodicTriggerEventsInFile);
  fStream->GetRecoInfo().SetNSpecialTriggerEvents(fNProcessedSpecialTriggerEventsInFile);
  fStream->GetRecoInfo().SetKaonRate(fKaonRate);
  fStream->GetRecoInfo().SetKaonRateError(fKaonRateError);
  fStream->GetRecoInfo().SetChokeONTime(fChokeONTimeInFile);
  // Writing Stream tree
  if(fStreamsOutputTree){
    fStreamsOutputTree->SetBranchAddress("Stream",&fStream);
    fStreamsOutputTree->Fill();
    fStreamsOutputTree->Write();
  }

  fHistoFile->cd("/");
  fGVirtMem->Write();
  fGResMem->Write();
  fGFileSize->Write();
  fGSystemFileSize->Write();
  fHNEventsProcessedPerBurst->Write();
  fHEventSize->Write();
  fHEventTimeStamp->Write();
  fHSkippedEventTimeStamp->Write();
  fHNCriticalEventsPerDetector->Write();
  fHNEventsWithQualityWarningsPerDetector->Write();
  fHEventTimeStampBits->Write();
  fHEventTimeStamp16->Write();
  fHEventTimeStamp128->Write();
  fHEventTimeStamp1024->Write();
  fHDeltaTimeStamp->Write();
  fHDeltaTimeStampStored->Write();
  fHL0TriggerFlags->Write();
  fHL1TriggerFlags->Write();
  fHL1Counters->Write();
  if(fHTimingProfile) fHTimingProfile->Write();
  if(fHTiming2D)      fHTiming2D->Write();
  fHistoFile->Purge();
  fHistoFile->Close();
  PrintInfo();
}

void NA62Reconstruction::PrintInfo(){
  fTimer.PrintTimers(cout);
  PrintRecoSummary(cout);
}

void NA62Reconstruction::CompleteInit(){
  /// \MemberDescr
  /// This function is called just once before processing the first event.
  /// It completes the initialisation of the output branches, after disabling the detectors not found in data.
  /// \EndMemberDescr

  if(fIsRawData) CleanUpSequence();
  for(Int_t iReco = 0; iReco < fNReconstructions; iReco++){
    if((fOutputStage&kRecoTreeEnabled) || (fOutputStage&kDigiTreeEnabled)){
      for(UInt_t iTree=0;iTree<fOutputTrees.size();iTree++){
        fOutputTrees[iTree]->SetDirectory(fHistoFile->GetDirectory("/"));
        if(((TString)fOutputTrees[iTree]->GetName()).CompareTo("SpecialTrigger")==0) {
          fOutputTrees[iTree]->Branch(fRecoSequence[iReco].Data(),FindOutputBranchEvent("SpecialTrigger")->at(iReco)->IsA()->GetName(),
              &(FindOutputBranchEvent("SpecialTrigger")->at(iReco)))->SetAutoDelete(kFALSE);
        }
        else if(((TString)fOutputTrees[iTree]->GetName()).CompareTo(fOutputStageName.Data())==0) {
          if(fOutputStage == kStageSlimReco && fSlimRecoEvents[iReco]){ // custom handling for slim events
            fOutputTrees[iTree]->Branch(fRecoSequence[iReco].Data(),fSlimRecoEvents[iReco]->IsA()->GetName(), &(fSlimRecoEvents[iReco]))->SetAutoDelete(kFALSE);
          }
          else{
            fOutputTrees[iTree]->Branch(fRecoSequence[iReco].Data(),FindOutputBranchEvent(fOutputStageName.Data())->at(iReco)->IsA()->GetName(),
                &(FindOutputBranchEvent(fOutputStageName.Data())->at(iReco)))->SetAutoDelete(kFALSE);
          }
        }
        else if(fOutputStage == kStageRecoDigis) {
          fOutputTrees[iTree]->Branch(fRecoSequence[iReco].Data(),FindOutputBranchEvent("Digis")->at(iReco)->IsA()->GetName(),
              &(FindOutputBranchEvent("Digis")->at(iReco)))->SetAutoDelete(kFALSE);
        }
      }
    }
  }
  if((fOutputStage&kRecoTreeEnabled) || (fOutputStage&kDigiTreeEnabled)){
    FindOutputTree(fOutputStageName.Data())->SetAutoFlush(-fBasketSize);
    if(fOutputStage == kStageRecoDigis)
      FindOutputTree("Digis")->SetAutoFlush(-fBasketSize);
  }
  fReferenceDetectorReco = FindReco(fReferenceDetectorName);
  if(fReferenceDetectorReco) { //use reference detector time
    std::cout << "[NA62Reconstruction] Using '" << fReferenceDetectorName << "' as reference detector.." << std::endl;
  }
  else if(fReferenceDetectorName!="Trigger" && fReferenceDetectorName!="RICHPrim") {
    std::cerr << "[NA62Reconstruction] WARNING: reference detector '" << fReferenceDetectorName << "' not found! Trigger time will be used." << std::endl;
  }
  PrintT0Settings(cout);

  // Read Trigger drift T0 file, if exists
  if(fIsRawData && NA62ConditionsService::GetInstance()->Open(fTriggerDriftT0FileInput)==kSuccess){
    Bool_t BurstFound = false;
    TString Line;
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fTriggerDriftT0FileInput)) && !BurstFound) {
      if (Line.BeginsWith("#")) continue;
      TObjArray * l = Line.Tokenize(" ");
      UInt_t ReadRunID=0, ReadBurstID=0;
      if(l->GetEntries()>0) ReadRunID     = static_cast<TObjString*>(l->At(0))->GetString().Atoi();
      if(l->GetEntries()>1) ReadBurstID   = static_cast<TObjString*>(l->At(1))->GetString().Atoi();
      if(ReadRunID==NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID() && ReadBurstID==NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID()){
        if(l->GetEntries()>2) fTriggerDriftT0 = static_cast<TObjString*>(l->At(2))->GetString().Atof();
        BurstFound = true;
      }
      delete l;
    }
    if(BurstFound) std::cout << "[NA62Reconstruction] TriggerDriftT0 set to " << fTriggerDriftT0 << " ns!" << std::endl;
    else {
      std::cerr << "[NA62Reconstruction] WARNING: Burst " << NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID() << " not found for run " << NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID() << " in file '"<< fTriggerDriftT0FileInput << "'!" << std::endl;
    }
    NA62ConditionsService::GetInstance()->Close(fTriggerDriftT0FileInput);
  }
  // SetRecoCHODEvent for Spectrometer (needed for 2015 data)
  if(FindReco("Spectrometer")) static_cast<SpectrometerReconstruction*>(FindReco("Spectrometer"))->SetRecoCHODEvent(static_cast<TRecoCHODEvent*>(FindRecoEvent("CHOD")));

}

void NA62Reconstruction::FillTimes(Double_t /*aReferenceTime*/) { //aReferenceTime not used!
  /// \MemberDescr
  /// It fills the histograms (event by event) for the evaluation of the T0 corrections with respect to a reference subdetector.
  /// \EndMemberDescr

  Bool_t FillDigiTimes = true;
  EventHeader* EventHeader = NA62RecoManager::GetInstance()->GetEventHeader();
  Double_t ReferenceFineTime = EventHeader->GetFineTime()*ClockPeriod/256; //default value in case reference detector is not found
  if(fReferenceDetectorName == "RICHPrim" && (EventHeader->GetL0TPData()->GetDataType()&0x10)){ //use RICH time also for control triggers
    Double_t RICHPrimitiveTime = 0.;
    Int_t NPrimitives = 0;
    for(UInt_t iSlot=0;iSlot<3;iSlot++){
      L0Primitive Primitive = EventHeader->GetL0TPData()->GetPrimitive(iSlot,kL0RICH);
      UInt_t primWord = Primitive.GetPrimitiveID(); //0 if not found
      if(!primWord) continue;
      RICHPrimitiveTime = EventHeader->GetL0TPData()->GetPrimitiveCorrectedFineTime(iSlot,kL0RICH,fL0TPFineTimeBit)*ClockPeriod/256.;
      NPrimitives++;
    }
    if(NPrimitives==1) ReferenceFineTime = RICHPrimitiveTime; //only use it if there is exactly one match
  }
  if(fReferenceDetectorReco) { //use reference detector time
    FillDigiTimes = false;
    //clusterise digis into digi-candidates
    fReferenceDetectorReco->TimeClustering(fReferenceDetectorReco->GetRecoEvent()->GetDigiEvent(),5.);
    //loop on the reference detector digi-candidates
    TTimeCluster * BestCand = 0;
    Double_t TimeDistance = 10000.;
    for(Int_t iCand=0; iCand<fReferenceDetectorReco->GetRecoEvent()->GetDigiEvent()->GetNCandidates(); iCand++){
      TTimeCluster * CurrentCand = static_cast<TTimeCluster*>(fReferenceDetectorReco->GetRecoEvent()->GetDigiEvent()->GetCandidate(iCand));
      if(CurrentCand->GetNHits()>=fReferenceDetectorNHitsMin && fabs(CurrentCand->GetAverage()-ReferenceFineTime)<TimeDistance) {
        TimeDistance = fabs(CurrentCand->GetAverage()-ReferenceFineTime);
        BestCand = CurrentCand;
      }
    }
    if(BestCand && TimeDistance<ClockPeriod) {
      ReferenceFineTime = BestCand->GetAverage();
      FillDigiTimes = true;
    }
  }
  if(FillDigiTimes){
    for(UInt_t iRaw = 0; iRaw < fRawDecoders.size(); iRaw++){
      if(fRawDecoders[iRaw] && fRawDecoders[iRaw]->GetDecoder()) {
        //FillDigiTimes for fast T0-evaluation
        fRawDecoders[iRaw]->FillDigiTimes(ReferenceFineTime);
      }
    }
  }

  for(Int_t iReco = 0; iReco < fNReconstructions; iReco++){
    if(fFillTimesEnabled) {
      TRecoVEvent* ReferenceRecoEvent = 0;
      if(FindReco(fT0ReferenceDetectors[iReco])) ReferenceRecoEvent = FindReco(fT0ReferenceDetectors[iReco])->GetRecoEvent();
      else if(fT0ReferenceDetectors[iReco]!="Trigger") {
        std::cerr << "[NA62Reconstruction] WARNING (T0 Evaluation): Cannot find reference detector '" << fT0ReferenceDetectors[iReco] << "' used for " << fRecoSequence[iReco] << "! Using trigger instead.." << std::endl;
        fT0ReferenceDetectors[iReco]="Trigger";
      }
      if(fT0ReferenceDetectors[iReco]=="Trigger"){
        Double_t ReferenceTime = EventHeader->GetFineTime()*ClockPeriod/256;
        if((fOutputStatus==kStatusGood || fOutputStatus>fSkipEventsLevel) && (fOutputStage&kRecoEnabled) && !fEvents[iReco]->IsA()->InheritsFrom("TSpecialTriggerEvent"))
          fReconstructions[iReco]->FillTimes(ReferenceTime);
      }
      else if(ReferenceRecoEvent){
        //loop on all the reference detector candidates
        for(Int_t iRefCandidate = 0; iRefCandidate<ReferenceRecoEvent->GetNCandidates();iRefCandidate++){
          //quality requirement on reference detector candidates
          if(ReferenceRecoEvent->GetCandidate(iRefCandidate)->GetNHits()<fT0NHitsMinValues[iReco]) continue;
          Double_t ReferenceTime = ReferenceRecoEvent->GetCandidate(iRefCandidate)->GetTime();
          if((fOutputStatus==kStatusGood || fOutputStatus>fSkipEventsLevel) && (fOutputStage&kRecoEnabled) && !fEvents[iReco]->IsA()->InheritsFrom("TSpecialTriggerEvent"))
            fReconstructions[iReco]->FillTimes(ReferenceTime);
        }
      }
    }
  }
  return;
}

TString NA62Reconstruction::CheckProtocols(TString OldStr){
  TString NewStr=OldStr;
  if(NewStr.EndsWith("\r")) NewStr.Remove(NewStr.Last('\r')); // Remove any residual EOL special character (^M) [for Windows-DOS compatibility]
  if(NewStr.BeginsWith("/eos/") && !NewStr.Contains("root://")){
    if(NewStr.Contains("/eos/experiment")) NewStr = "root://eosna62.cern.ch/"+NewStr;
    else if(NewStr.Contains("/eos/user"))  NewStr = "root://eosuser.cern.ch/"+NewStr;
  }
  else if(NewStr.BeginsWith("/castor/")){
    if(!NewStr.Contains("root://")){
      NewStr = "root://castorpublic.cern.ch/"+NewStr;
    }
    if(!NewStr.Contains("svcClass")){
      NewStr = NewStr+"?svcClass="+fSvcClass;
    }
  }
  return NewStr;
}

std::pair<Double_t,Double_t> NA62Reconstruction::GetMemoryUsage(){
  std::ifstream file;
  file.open("/proc/self/stat");
  TString datum;
  double virtualMemory = 0.;
  double residentMemory = 0.;
  if (file.is_open()) {
    for (int i = 0; i < 23; i++)
      datum.ReadToDelim(file, ' ');
    //virtual memory is in bytes
    virtualMemory = datum.Atoll() / (1024. * 1024.);
    datum.ReadToDelim(file, ' ');
    //resident memory is in pages (4kb each)
    residentMemory = datum.Atoll()*4 / (1024.);
  }
  file.close();
  return std::make_pair(virtualMemory, residentMemory);
}

Double_t NA62Reconstruction::GetFileSizeUsage(int option){
  if(option==1)
    return fHistoFile->GetSize()/(1024.*1024.);
  else if(option==2){
    struct stat statbuf;
    if (stat(fHistoFile->GetName(), &statbuf) != -1)
      return statbuf.st_blocks*512;
    else
      return 0;
  }
  return 0;
}

void NA62Reconstruction::InitTimers(){
  // need to define timers
  fOpenInputTimer = fTimer.AddTimer("Open input",1);
  fInputReadTimer = fTimer.AddTimer("Input read",1);
  fRawDecTimers.push_back(fTimer.AddTimer("Total RawDecoder",1));
  for(Int_t iReco = 0; iReco < fNRawDecoders; iReco++){
    fRawDecTimers.push_back(fTimer.AddTimer(fRecoSequence[iReco],2));
  }
  fDigiTimers.push_back(fTimer.AddTimer("Total Digi",1));
  for(Int_t iReco = 0; iReco < fNDigitizers; iReco++){
    fDigiTimers.push_back(fTimer.AddTimer(fRecoSequence[iReco],2));
  }
  fRecoTimers.push_back(fTimer.AddTimer("Total Reco",1));
  for(Int_t iReco = 0; iReco < fNReconstructions; iReco++){
    fRecoTimers.push_back(fTimer.AddTimer(fRecoSequence[iReco],2));
  }
  fGlobalTimers.push_back(fTimer.AddTimer("Total",0));
}

void NA62Reconstruction::CleanUpTimers(UInt_t iReco){
  // cleaning up timers
  fTimer.RemoveTimers(fReconstructions[iReco]->GetName());
  CleanUpTimer(fRawDecTimers.at(iReco+1)); // 0 = all rawdecoders
  CleanUpTimer(fDigiTimers.at(iReco+1));   // 0 = all digi
  CleanUpTimer(fRecoTimers.at(iReco+1));   // 0 = all reco
}

void NA62Reconstruction::CleanUpTimer(UInt_t iTimer){
  for(UInt_t iReco = 0; iReco < fRawDecTimers.size(); iReco++){
    if(iTimer==fRawDecTimers.at(iReco)) fRawDecTimers.erase(fRawDecTimers.begin() + iReco);
    if(iReco==fRawDecTimers.size()) break;
    if(iTimer<fRawDecTimers.at(iReco))  fRawDecTimers[iReco]--;
  }
  for(UInt_t iReco = 0; iReco < fDigiTimers.size(); iReco++){
    if(iTimer==fDigiTimers.at(iReco))   fDigiTimers.erase(fDigiTimers.begin() + iReco);
    if(iReco==fDigiTimers.size()) break;
    if(iTimer<fDigiTimers.at(iReco))    fDigiTimers[iReco]--;
  }
  for(UInt_t iReco = 0; iReco < fRecoTimers.size(); iReco++){
    if(iTimer==fRecoTimers.at(iReco))   fRecoTimers.erase(fRecoTimers.begin() + iReco);
    if(iReco==fRecoTimers.size()) break;
    if(iTimer<fRecoTimers.at(iReco))    fRecoTimers[iReco]--;
  }
  for(UInt_t iGlob = 0; iGlob < fGlobalTimers.size(); iGlob++){
    if(iTimer==fGlobalTimers.at(iGlob)) fGlobalTimers.erase(fGlobalTimers.begin() + iGlob);
    if(iGlob==fGlobalTimers.size()) break;
    if(iTimer<fGlobalTimers.at(iGlob))  fGlobalTimers[iGlob]--;
  }
}

TString NA62Reconstruction::GetL1TriggerType(UInt_t L1TriggerBit) {
  TString l1TriggerType = "";
  switch (L1TriggerBit) {
    case 0:
      l1TriggerType = "L1 Physics Accepted";
      break;
    case 3:
      l1TriggerType = "L1 Timeout";
      break;
    case 4:
      l1TriggerType = "All L1 Disabled";
      break;
    case 5:
      l1TriggerType = "L1 Bypass";
      break;
    case 6:
      l1TriggerType = "L1 Flag";
      break;
    case 7:
      l1TriggerType = "L1 AP";
      break;
    default:
      l1TriggerType = "Not Used";
  }
  return l1TriggerType;
}

void NA62Reconstruction::PrintWarning(std::stringstream &ss){
  cerr_en(fWarningsLevel,WARN_EVT) << ss.str();
  if(fLogFileDescriptor) fprintf(fLogFileDescriptor, "%s", ss.str().c_str());
  ss.str("");
  ss.clear();
}

void NA62Reconstruction::PrintT0Settings(std::ostream &os){
  //******************** T0 Evaluation settings *********************//
  os << setfill('*') << setw(32) << "*";
  os << " T0 Evaluation settings ";
  os << setfill('*') << setw(32) << "*" << std::endl;
  //settings for the selected reco sequence
  for(Int_t iReco=0; iReco<fNReconstructions; iReco++){
    os << setfill(' ') << left  << "* " << setw(12) << fRecoSequence[iReco];
    os << setfill(' ') << left  << " will use " << setw(12) << fT0ReferenceDetectors[iReco] << " as reference detector";
    if(fT0ReferenceDetectors[iReco]!="Trigger") {
      os << setfill(' ') << right << " [at least "<<setw(2) << fT0NHitsMinValues[iReco] << " hits required] *" << std::endl;
    }
    else  os << setfill(' ') << right << setw(30) << "*" << std::endl;
  }
  os << setfill('*') << setw(88) << "*" << setfill(' ') << std::endl;
  //*****************************************************************//
}

void NA62Reconstruction::PrintRecoSummary(std::ostream &os){
  //****************** NA62Reconstruction Summary *******************//
  os << setfill('*') << setw(12) << "*";
  os << " NA62Reconstruction Summary ";
  os << setfill('*') << setw(12) << "*" << std::endl;
  //total number of read events
  os << setfill(' ') << setw(40) << left  << "* Total number of read events:";
  os << setfill(' ') << setw(10) << right << fNReadEventsInTotal;
  os << " *" << std::endl;
  //total number of physics triggers
  os << setfill(' ') << setw(40) << left  << "*              - physics triggers:";
  os << setfill(' ') << setw(10) << right << fNReadPhysicsTriggerEventsInTotal;
  os << " *" << std::endl;
  //total number of control triggers
  os << setfill(' ') << setw(40) << left  << "*              - control triggers:";
  os << setfill(' ') << setw(10) << right << fNReadControlTriggerEventsInTotal;
  os << " *" << std::endl;
  //total number of periodic triggers
  os << setfill(' ') << setw(40) << left  << "*              - periodic triggers:";
  os << setfill(' ') << setw(10) << right << fNReadPeriodicTriggerEventsInTotal;
  os << " *" << std::endl;
  //total number of calibration triggers
  os << setfill(' ') << setw(40) << left  << "*              - calibration triggers:";
  os << setfill(' ') << setw(10) << right << fNReadCalibrationTriggerEventsInTotal;
  os << " *" << std::endl;
  //total number of special triggers
  os << setfill(' ') << setw(40) << left  << "*              - special triggers:";
  os << setfill(' ') << setw(10) << right << fNReadSpecialTriggerEventsInTotal;
  os << " *" << std::endl;
  //total number of SOBs
  os << setfill(' ') << setw(40) << left  << "*                     - SOBs:";
  os << setfill(' ') << setw(10) << right << fNReadSOBEventsInTotal;
  os << " *" << std::endl;
  //total number of EOBs
  os << setfill(' ') << setw(40) << left  << "*                     - EOBs:";
  os << setfill(' ') << setw(10) << right << fNReadEOBEventsInTotal;
  os << " *" << std::endl;
  //total number of processed events
  os << setfill(' ') << setw(40) << left  << "* Total number of processed events:";
  os << setfill(' ') << setw(10) << right << fNProcessedEventsInTotal;
  os << " *" << std::endl;
  //total number of physics triggers
  os << setfill(' ') << setw(40) << left  << "*              - physics triggers:";
  os << setfill(' ') << setw(10) << right << fNProcessedPhysicsTriggerEventsInTotal;
  os << " *" << std::endl;
  //total number of control triggers
  os << setfill(' ') << setw(40) << left  << "*              - control triggers:";
  os << setfill(' ') << setw(10) << right << fNProcessedControlTriggerEventsInTotal;
  os << " *" << std::endl;
  //total number of periodic triggers
  os << setfill(' ') << setw(40) << left  << "*              - periodic triggers:";
  os << setfill(' ') << setw(10) << right << fNProcessedPeriodicTriggerEventsInTotal;
  os << " *" << std::endl;
  //total number of calibration triggers
  os << setfill(' ') << setw(40) << left  << "*              - calibration triggers:";
  os << setfill(' ') << setw(10) << right << fNProcessedCalibrationTriggerEventsInTotal;
  os << " *" << std::endl;
  //total number of special triggers
  os << setfill(' ') << setw(40) << left  << "*              - special triggers:";
  os << setfill(' ') << setw(10) << right << fNProcessedSpecialTriggerEventsInTotal;
  os << " *" << std::endl;
  //total number of SOBs
  os << setfill(' ') << setw(40) << left  << "*                     - SOBs:";
  os << setfill(' ') << setw(10) << right << fNProcessedSOBEventsInTotal;
  os << " *" << std::endl;
  //total number of EOBs
  os << setfill(' ') << setw(40) << left  << "*                     - EOBs:";
  os << setfill(' ') << setw(10) << right << fNProcessedEOBEventsInTotal;
  os << " *" << std::endl;
  //total number of events with critical errors
  os << setfill(' ') << setw(40) << left  << "* Total number of critical events:";
  os << setfill(' ') << setw(10) << right << fNCriticalEventsInTotal;
  os << " *" << std::endl;
  //total number of skipped events
  os << setfill(' ') << setw(40) << left  << "* Total number of skipped events:";
  os << setfill(' ') << setw(10) << right << fNSkippedEventsInTotal;
  os << " *" << std::endl;
  //total number of skipped events due to unrequested events (triggers or -j option)
  os << setfill(' ') << setw(40) << left  << "*              - unrequested events:";
  os << setfill(' ') << setw(10) << right << fNSkippedUnrequestedEventsInTotal;
  os << " *" << std::endl;
  //total number of skipped events due to downscaling
  os << setfill(' ') << setw(40) << left  << "*              - downscaled events:";
  os << setfill(' ') << setw(10) << right << fNSkippedDownscaledEventsInTotal;
  os << " *" << std::endl;
  //total number of skipped events due to corruption
  os << setfill(' ') << setw(40) << left  << "*              - corrupted events:";
  os << setfill(' ') << setw(10) << right << fNSkippedCorruptedEventsInTotal;
  os << " *" << std::endl;
  //total number of skipped files
  os << setfill(' ') << setw(40) << left  << "* Total number of skipped files:";
  os << setfill(' ') << setw(10) << right << fNSkippedFilesInTotal;
  os << " *" << std::endl;
  os << setfill('*') << setw(52) << "*" << setfill(' ') << std::endl;
  //total Choke ON time
  os << setfill(' ') << setw(40) << left  << "* Total Choke ON time (ns):";
  os << setfill(' ') << setw(10) << right << fChokeONTimeInTotal;
  os << " *" << std::endl;
  os << setfill('*') << setw(52) << "*" << setfill(' ') << std::endl;
  //*****************************************************************//
}

void NA62Reconstruction::ResetVariables(){

  fHL1Counters = nullptr;

  ResetFileVariables();
  ResetTotalVariables();

  fNRawDecoders = 0;
  fNRawEncoders = 0;
  fNReconstructions = 0;
  fNDigitizers = 0;
  fIsRawData = kFALSE;
  fNWordsInChunk = 0;
  fiFile = 0;
  fNFiles = 0;
  fErrorInReading = kFALSE;

  fRecoSequence.clear();
  fOutputTrees.clear();
  fOutputBranchEvents.clear();
  fT0ReferenceDetectors.clear();
  fT0NHitsMinValues.clear();
  fEvaluateTriggerDriftT0 = kFALSE;
  fTriggerDriftT0 = 0.;
  fTriggerDriftT0FileInput = "";
  fMCChains.clear();
  fMCTruthEvent = 0;
  fStreamsOutputTree = 0;
  fStream = 0;
  fContinuousReading = kFALSE;
  fSkipEventsLevel = 0;
  fSkipEventsMask = 0xffffffff;
  fEOBFileDescriptor = 0;
  fLogFileDescriptor = 0;
  fSkippedFileDescriptor = 0;
  fSkippedFileName = "NA62Reco.skipped";                    //default
  fSvcClass = "na62";                                       //default
  fEOBFileEnabled = kFALSE;                                 //default
  fLogFileEnabled = kFALSE;                                 //default
  fFillTimesEnabled = kTRUE;                                //default, not controlled by user
  fDownscalingFactor = 1;                                   //default
  fOnlineMonitorMode = kShifter;                            //default
  fSaveOnlineMonitorPlots = false;                          //default
  fSaveOnlineMonitorPlotsDir = "OMPlots";                   //default
  fOnlineMonitorReferenceFileName = "OMRefFile.root";       //default
  fRequestedTriggersFlag = 0x7ffff;                         //default (all enabled) [bits 0-15: 16masks, bit 16: control, bit 17:periodics, bit 18: calibration]
  fCheckErrno = false;                                      //default
  fWarningsLevel = 1;                                       //default
  fDataFormat = -1;
  fL0TPFineTimeBit = 0;

  fCedarConfigFileName        = "";
  fCHANTIConfigFileName       = "";
  fCHODConfigFileName         = "";
  fGigaTrackerConfigFileName  = "";
  fHACConfigFileName          = "";
  fIRCConfigFileName          = "";
  fLAVConfigFileName          = "";
  fLKrConfigFileName          = "";
  fMUV0ConfigFileName         = "";
  fMUV1ConfigFileName         = "";
  fMUV2ConfigFileName         = "";
  fMUV3ConfigFileName         = "";
  fNewCHODConfigFileName      = "";
  fRICHConfigFileName         = "";
  fSACConfigFileName          = "";
  fSpectrometerConfigFileName = "";
  fSAVConfigFileName          = "";

  fDIMRecoveryMode            = kFALSE; // Default is no-recovery of DIM information

  fOutputStatus = kStatusGood;
  fPreviousEventNumber = -1;    //for debug
  fGVirtMem = 0;
  fGResMem = 0;
  fGFileSize = 0;
  fGSystemFileSize = 0;
  for(Int_t iRaw=0;iRaw<MAXNDETECTORS;iRaw++) {
    fNWrongSlots[iRaw] = 0;  //for debug
    fNTotalSlots[iRaw] = 0;  //for debug
  }

  // Burst Header variables
  fBurstHeaderFormat = 0;
  fNEventsInFile = 0;
  fEventNumbers = 0;
  fTriggerTypes = 0;
  fEventOffsets = 0;
  fEventAlreadyProcessed = 0;

  // Guest detector variables
  fHostsForGuestDetectors.clear();
  fGuestDetectorIndices.clear();
  fGuestDetectorNames.clear();

  // Reference detector variables
  fReferenceDetectorName = "Cedar"; //default
  fReferenceDetectorNHitsMin = 8;   //default
  fReferenceDetectorReco = 0;

  // Stream additional info
  fKaonRate = 0;
  fKaonRateError = 0;

  fChokeON=kFALSE;
  fChokeONStartTime=0.;
  fChokeONEndTime=0.;
}

void NA62Reconstruction::ResetTotalVariables(){
  //Counters
  fNReadEventsInTotal = 0;
  fNReadPhysicsTriggerEventsInTotal = 0;
  fNReadControlTriggerEventsInTotal = 0;
  fNReadPeriodicTriggerEventsInTotal = 0;
  fNReadCalibrationTriggerEventsInTotal = 0;
  fNReadSpecialTriggerEventsInTotal = 0;
  fNReadSOBEventsInTotal = 0;
  fNReadEOBEventsInTotal = 0;
  fNProcessedEventsInTotal = 0;
  fNProcessedPhysicsTriggerEventsInTotal = 0;
  fNProcessedControlTriggerEventsInTotal = 0;
  fNProcessedPeriodicTriggerEventsInTotal = 0;
  fNProcessedCalibrationTriggerEventsInTotal = 0;
  fNProcessedSpecialTriggerEventsInTotal = 0;
  fNProcessedSOBEventsInTotal = 0;
  fNProcessedEOBEventsInTotal = 0;
  fNSkippedEventsInTotal = 0;
  fNSkippedFilesInTotal = 0;
  fNSkippedCorruptedEventsInTotal = 0;
  fNSkippedDownscaledEventsInTotal = 0;
  fNSkippedUnrequestedEventsInTotal = 0;
  fNCriticalEventsInTotal = 0;
  fChokeONTimeInTotal=0.;
}

void NA62Reconstruction::ResetFileVariables(){

  fInputFileDescriptor = -1;
  fErrorInReading = kFALSE;
  fNWordsInFile = 0;
  fiCurrentEventInFile = 0;
  fCurrentOffsetInFile = 0;

  // Counters
  fNReadEventsInFile = 0;
  fNReadPhysicsTriggerEventsInFile = 0;
  fNReadControlTriggerEventsInFile = 0;
  fNReadPeriodicTriggerEventsInFile = 0;
  fNReadCalibrationTriggerEventsInFile = 0;
  fNReadSpecialTriggerEventsInFile = 0;
  fNReadSOBEventsInFile = 0;
  fNReadEOBEventsInFile = 0;
  fNProcessedEventsInFile = 0;
  fNProcessedPhysicsTriggerEventsInFile = 0;
  fNProcessedControlTriggerEventsInFile = 0;
  fNProcessedPeriodicTriggerEventsInFile = 0;
  fNProcessedCalibrationTriggerEventsInFile = 0;
  fNProcessedSpecialTriggerEventsInFile = 0;
  fNProcessedSOBEventsInFile = 0;
  fNProcessedEOBEventsInFile = 0;
  fNSkippedEventsInFile = 0;
  fNSkippedCorruptedEventsInFile = 0;
  fNSkippedDownscaledEventsInFile = 0;
  fNSkippedUnrequestedEventsInFile = 0;
  fNCriticalEventsInFile = 0;
  fChokeONTimeInFile=0.;

  //L1TP monitoring variables
  if(fHL1Counters) fHL1Counters->Reset("M");
}

void NA62Reconstruction::DecodeBurstHeader() {
  // Reset any previous values
  fBurstHeaderFormat = 0;
  fNEventsInFile     = 0;
  if(fEventNumbers){
    delete [] fEventNumbers;
    fEventNumbers=0;
  }
  if(fTriggerTypes){
    delete [] fTriggerTypes;
    fTriggerTypes=0;
  }
  if(fEventOffsets){
    delete [] fEventOffsets;
    fEventOffsets=0;
  }
  if(fEventAlreadyProcessed){
    delete [] fEventAlreadyProcessed;
    fEventAlreadyProcessed=0;
  }
  NA62RecoManager::GetInstance()->GetEventHeader()->SetRunID(0);
  // Look for Burst Header
  if(((*fpDataBuffer)&0xff000000) == 0x0){ //>=2015 format
    fBurstHeaderFormat = *(fpDataBuffer);
    fNEventsInFile     = *(fpDataBuffer+1);
    Int_t RunID        = *(fpDataBuffer+2);
    Int_t BurstID      = *(fpDataBuffer+3);
    NA62RecoManager::GetInstance()->GetEventHeader()->SetRunID(RunID);
    NA62RecoManager::GetInstance()->GetEventHeader()->SetBurstID(BurstID); //overwritten by event header
    NA62ConditionsService::GetInstance()->SetCurrentRunID(RunID);
    NA62ConditionsService::GetInstance()->SetCurrentBurstID(BurstID);
    if(fNEventsInFile) {
      fEventNumbers = new ULong_t[fNEventsInFile];
      fTriggerTypes = new ULong_t[fNEventsInFile];
      fEventOffsets = new ULong_t[fNEventsInFile];
      fEventAlreadyProcessed = new Bool_t[fNEventsInFile];
      for(UInt_t iEvent=0;iEvent<fNEventsInFile;iEvent++){
        fEventNumbers[iEvent]  = (*(fpDataBuffer+0*fNEventsInFile+4+iEvent))&0x00ffffff;
        fTriggerTypes[iEvent]  = (*(fpDataBuffer+1*fNEventsInFile+4+iEvent))&0x00ffffff;
        fEventOffsets[iEvent]  =  *(fpDataBuffer+2*fNEventsInFile+4+iEvent);
        fEventAlreadyProcessed[iEvent] = kFALSE;
        if(iEvent && fEventOffsets[iEvent]<fEventOffsets[iEvent-1]) std::cerr <<"[NA62Reconstruction] WARNING: Invalid event offset for event " << iEvent << "! [fEventOffsets["<< iEvent-1 <<"]: " << std::hex << fEventOffsets[iEvent-1] << std::dec << " fEventOffsets["<< iEvent <<"]: " << std::hex << fEventOffsets[iEvent] << std::dec << "]" << std::endl;
      }
      if(fEventOffsets[0]!=3*fNEventsInFile+4){
        std::cerr << "[NA62Reconstruction] WARNING: Event offset inconsistency: its value is " << std::hex << fEventOffsets[0] << " while it should be " << 3*fNEventsInFile+4 << std::dec << std::endl;
      }
      fpDataBuffer+=3*fNEventsInFile+4; //point to first event in the file
    }
    else {
      std::cerr << "[NA62Reconstruction] WARNING: File seems to be empty. DecodeBurstHeader found fNEventsInFile = 0." << std::endl;
      if(!fContinuousReading) _Exit(kCorruptedFile); // unlike exit() does not execute atexit functions
      return;
    }
  }
  else if(((*fpDataBuffer)&0xff000000) != 0x62000000){ // not compatible with either 2015 or previous formats
    std::cerr << "[NA62Reconstruction] WARNING: Incompatible format '" << std::hex << ((*fpDataBuffer)&0xff000000) << std::dec << "' found in Burst Header" << std::endl;
  }
  // Keep only the file name (just for warning printouts)
  TObjArray * l = fCurrentFileName.Tokenize("/");
  fCurrentFileName = static_cast<TObjString*>(l->Last())->GetString();
  if (fNWordsInFile) {
    NA62RecoManager::GetInstance()->GetEventHeader()->SetHeader(fpDataBuffer);
    time_t BurstTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstTime();
    std::cout << "FileName: " << fCurrentFileName << " BurstTime: " << BurstTime << " ["<< TimeString(BurstTime) <<"]" << std::endl;
    if(!NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID()){ // Extract run number from data file name if not in EventHeader (2014 data)
      if(fCurrentFileName.Length()){
        TObjArray *line = fCurrentFileName.Tokenize("-");
        if(line->GetEntries()>2) NA62RecoManager::GetInstance()->GetEventHeader()->SetRunID(static_cast<TObjString*>(line->At(2))->GetString().Atoi());
        delete line;
      }
    }
  }
  delete l;
  // Open .eob and .log files
  const TString EOBDir = "eob";
  const TString LogDir = "log";
  TString BurstEOBFileName = fCurrentFileName;
  TString BurstLogFileName = fCurrentFileName;
  Ssiz_t DotPos = fCurrentFileName.Last('.');
  if (DotPos<0) DotPos = fCurrentFileName.Length();
  BurstEOBFileName = EOBDir+"/"+BurstEOBFileName.Remove(DotPos).Append(".eob");
  BurstLogFileName = LogDir+"/"+BurstLogFileName.Remove(DotPos).Append(".log");

  struct stat filestat;
  if(fEOBFileEnabled){
    if(stat(EOBDir, &filestat)!=0) mkdir(EOBDir, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
    std::cout << "Opening EndOfBurst file: " << BurstEOBFileName.Data() << std::endl;
    fEOBFileDescriptor = fopen(Form(BurstEOBFileName.Data()),Form("w"));
    if(fEOBFileDescriptor == NULL) perror(Form("Input File:"));
  }
  if(fLogFileEnabled){
    if(stat(LogDir, &filestat)!=0) mkdir(LogDir, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
    std::cout << "Opening log file: " << BurstLogFileName.Data() << std::endl;
    fLogFileDescriptor = fopen(Form(BurstLogFileName.Data()),Form("w"));
    if(fLogFileDescriptor == NULL) perror(Form("Input File:"));
  }
}

void NA62Reconstruction::DecodeDIMBlock(UInt_t *pDataBuffer, UInt_t * NextOffset){
  // Clear DIM block variables
  NA62RecoManager::GetInstance()->GetEventHeader()->ClearDIMBlock();

  //UInt_t NumberOfBlockDataRead=0;
  // Subdetector Block Header (generic part)
  // word 1
  //UInt_t DataBlockByteSize = DataBlockSize(*pDataBuffer);
  //UInt_t DetectorSubID     = DetectorSubType(*pDataBuffer);
  //UInt_t DataBlockFormatID = DataBlockFormat(*pDataBuffer);
  pDataBuffer++;
  //NumberOfBlockDataRead++;
  // word 2
  //Long_t DataBlockTS       = DataBlockTimeStamp(*pDataBuffer);
  pDataBuffer++;
  //NumberOfBlockDataRead++;
  // --- temporary patch to fix bug in pc farm --- //
  // word 3
  pDataBuffer++;
  //NumberOfBlockDataRead++;
  // --------------------------------------------- //
  Bool_t BeamBlockFound = false;
  while(pDataBuffer < NextOffset){ // LOOP ON ALL SUBDETECTOR BLOCKS
    // Subdetector Block Header (detector-specific part)
    UInt_t SubDetID      = DIMEOBSubDetID(*pDataBuffer)/4;
    UInt_t NumberOfWords = DIMEOBNumberOfWords(*pDataBuffer);
    //Sub-detector DIM EOB
    if(/*0<=SubDetID && True by definition as SubDetID is unsigned*/ SubDetID<MAXNDETECTORS) {
      Int_t iReco = fRawRecoIndex[SubDetID];
      TSpecialTriggerEvent * SpecTrigEvent = 0;
      if(0<=iReco && iReco<fNReconstructions && fInputEvents[iReco] && fInputEvents[iReco]->IsA()->InheritsFrom("TSpecialTriggerEvent")){
        SpecTrigEvent = static_cast<TSpecialTriggerEvent*>(fInputEvents[iReco]);
      }
      NA62RecoManager::GetInstance()->StoreDIMBlock(fRecoRawTable[SubDetID],pDataBuffer,NumberOfWords,SpecTrigEvent);
      if(SubDetID==kDIM) BeamBlockFound = true;
    }
    else{
      std::cerr << "[NA62Reconstruction] WARNING: Unknown DIM sub-detectorID [SubDetID: " << std::hex << SubDetID << std::dec << "]" << std::endl;
    }
    if(!NumberOfWords) {
      NumberOfWords=1; //avoid infinite loops for corrupted blocks
      std::cerr << "[NA62Reconstruction] WARNING: Empty DIM block! [SubDetID: " << std::hex << SubDetID << std::dec << "]" << std::endl;
    }

    pDataBuffer+=NumberOfWords;
    //NumberOfBlockDataRead+=NumberOfWords;
  }
  if(fDIMRecoveryMode && !BeamBlockFound && isL0EOB(NA62RecoManager::GetInstance()->GetEventHeader()->GetTriggerType()<<2)){
    // Patch to recover T10/Argonion counts when beam info is missing from raw data
    int timeToSearch = NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstTime();
    float t10 = FindT10(timeToSearch);
    NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->SetIntensityT10(t10);
    cout << "[FindT10]: T10 for UNIX time " << timeToSearch << " is: " << t10 << '\n';
  }
}

void NA62Reconstruction::CheckSpecialTriggerTimeStamps(){
  // Check the consistency of the SpecTrig TimeStamps
  std::vector<ULong_t> SpecTrigTimeStamps;
  std::vector<vector<TString> > SpecTrigROBoardIDs;
  UInt_t nHits_tot=0;
  for(UInt_t iRaw = 0; iRaw < fRawDecoders.size(); iRaw++){
    if(fRawDecoders[iRaw] && fRawDecoders[iRaw]->GetDecoder()){
      UInt_t nHits = fRawDecoders[iRaw]->GetDecoder()->GetSpecialTriggerEvent()->GetNHits();
      nHits_tot+=nHits;
      for(UInt_t iHit=0;iHit<nHits;iHit++){
        TSpecialTrigger * SpecTrig = reinterpret_cast<TSpecialTrigger*>(fRawDecoders[iRaw]->GetDecoder()->GetSpecialTriggerEvent()->GetHit(iHit));
        ULong_t TimeStamp = SpecTrig->GetTimeStamp();
        UInt_t  ROBoardID = SpecTrig->GetROBoardID();
        Int_t iThisTimeStamp = -1;
        // Loop over all the existing TimeStamps
        for(UInt_t iTimeStamp=0;iTimeStamp<SpecTrigTimeStamps.size();iTimeStamp++){
          if(SpecTrigTimeStamps[iTimeStamp]==TimeStamp) { //TimeStamp found
            iThisTimeStamp=iTimeStamp;
            break;
          }
        }
        if(iThisTimeStamp<0){ // TimeStamp not found: add it
          iThisTimeStamp=SpecTrigTimeStamps.size();
          SpecTrigTimeStamps.push_back(TimeStamp);
          SpecTrigROBoardIDs.resize(SpecTrigROBoardIDs.size()+1);
        }
        if(fRawDecoders[iRaw]->GetDecoder()->GetNROBoards()>1) SpecTrigROBoardIDs[iThisTimeStamp].push_back(Form("%s_ROb%d",fRecoRawTable[iRaw].Data(),ROBoardID));
        else SpecTrigROBoardIDs[iThisTimeStamp].push_back(fRecoRawTable[iRaw]);
      }
    }
  }
  // Sort the TimeStamps
  for(UInt_t iTimeStamp=0;iTimeStamp<SpecTrigTimeStamps.size()-1;iTimeStamp++){
    for(UInt_t jTimeStamp=0;jTimeStamp<SpecTrigTimeStamps.size()-1-iTimeStamp;jTimeStamp++){
      if(SpecTrigTimeStamps[iTimeStamp]>SpecTrigTimeStamps[iTimeStamp+1]){
        ULong_t tmp_TS = SpecTrigTimeStamps[iTimeStamp+1];
        std::vector<TString> tmp_BoardID = SpecTrigROBoardIDs[iTimeStamp+1];
        SpecTrigTimeStamps[iTimeStamp+1] = SpecTrigTimeStamps[iTimeStamp];
        SpecTrigROBoardIDs[iTimeStamp+1] = SpecTrigROBoardIDs[iTimeStamp];
        SpecTrigTimeStamps[iTimeStamp]   = tmp_TS;
        SpecTrigROBoardIDs[iTimeStamp]   = tmp_BoardID;
      }
    }
  }
  if(SpecTrigTimeStamps.size()>1){
    // Print the Special Trigger TimeStamp occurrencies
    std::cerr << "[NA62Reconstruction] WARNING: Special Trigger TimeStamp mismatching. " << SpecTrigTimeStamps.size() << " Special Trigger TimeStamps found:" << std::endl;
    for(UInt_t iTimeStamp=0;iTimeStamp<SpecTrigTimeStamps.size();iTimeStamp++){
      std::cerr << "                              --> TimeStamp: " << SpecTrigTimeStamps[iTimeStamp] << " (" << std::hex << SpecTrigTimeStamps[iTimeStamp] << std::dec << ") occurring " << Form("%3lu",SpecTrigROBoardIDs[iTimeStamp].size()) << "/" << nHits_tot << " times";
      if(SpecTrigROBoardIDs[iTimeStamp].size()<10){
        for (UInt_t iROBoard=0;iROBoard<SpecTrigROBoardIDs[iTimeStamp].size();iROBoard++) {
          if(!iROBoard) std::cerr << " [" << SpecTrigROBoardIDs[iTimeStamp].at(iROBoard);
          else std::cerr << " " << SpecTrigROBoardIDs[iTimeStamp].at(iROBoard);
        }
        std::cerr << "]";
      }
      std::cerr << std::endl;
    }
  }
}

void NA62Reconstruction::RemoveGuestDetector(Int_t iHost, Int_t iGuest){
  fGuestDetectorIndices[iHost].erase(fGuestDetectorIndices[iHost].begin()+iGuest);
  fGuestDetectorNames[iHost].erase(fGuestDetectorNames[iHost].begin()+iGuest);
}

void NA62Reconstruction::ReadSingleEvent(Int_t iEvent){
  if(iEvent<0){
    std::cerr << "[NA62Reconstruction] WARNING: Attempt of reading an event with ID<0 [="<<iEvent<<"]! Event not read!" << std::endl;
    return;
  }
  if(!fEventNumbers || !fTriggerTypes || !fEventOffsets){
    std::cerr << "[NA62Reconstruction] WARNING: BurstHeader not found.. ReadSingleEvent cannot be used!" << std::endl;
    return;
  }
  std::cout << "[ReadSingleEvent] Reading Event: "<< iEvent << std::hex << " [EventID: " <<  fEventNumbers[iEvent] << " TriggerType: " << fTriggerTypes[iEvent] << " Offset: " << fEventOffsets[iEvent] << std::dec << "]" << std::endl;

  // Load event from file to memory
  lseek(fInputFileDescriptor, fEventOffsets[iEvent]*4, SEEK_SET); //go to the right position in the file
  UInt_t BufferTmp[2];
  Long_t bSizeOut1 = read(fInputFileDescriptor, &BufferTmp[0], 8); //read first 2 words of the event
  UInt_t EventLength = BufferTmp[1];
  std::vector<UInt_t> Buffer(EventLength,0);
  UInt_t * pBuffer = Buffer.data();
  for(UInt_t iWordTmp=0;iWordTmp<2;iWordTmp++) { //copy first 2 words in buffer
    Buffer[iWordTmp] = BufferTmp[iWordTmp];
  }
  Long_t bSizeOut2 = read(fInputFileDescriptor, pBuffer+2, (EventLength-2)*4); //read the remaining part of the event
  lseek(fInputFileDescriptor, fCurrentOffsetInFile, SEEK_SET); //set back the pointer where it was

  // Decode event
  if(bSizeOut1 && bSizeOut2){
    Long_t CurrentEventInFile = fiCurrentEventInFile; // save current event index
    fiCurrentEventInFile = iEvent; //overwrite current index
    DecodeEvent(pBuffer,Buffer);
    fiCurrentEventInFile = CurrentEventInFile; // restore correct event index
  }
  else{
    std::cerr << "[ReadSingleEvent] WARNING: Error while reading event: "<< iEvent << std::hex << "! Skipping it.. [EventID: " <<  fEventNumbers[iEvent] << " TriggerType: " << fTriggerTypes[iEvent] << " Offset: " << fEventOffsets[iEvent] << std::dec << "]" << std::endl;
    fNReadEventsInFile++;
    fNReadEventsInTotal++;
  }
}


float NA62Reconstruction::FindT10(int timestamp) { // read in T10 intensity from csv file

  if (NA62ConditionsService::GetInstance()->Open(fDIMRecoveryFileName)!=kSuccess) {
    std::cerr << "[FindT10] WARNING: cannot open file " << fDIMRecoveryFileName << std::endl;
    std::cerr << "[FindT10] WARNING: execution of FindT10 failed\n";
    return -1;
  }

  map<int,long long> m;
  string line;
  while(getline(NA62ConditionsService::GetInstance()->Get(fDIMRecoveryFileName),line)){
    stringstream   lineStream(line);
    string         cell;
    vector<string> result;
    while(getline(lineStream,cell,',')){
      result.push_back(cell.c_str());
    }

    if (result.size()==2){
      // strip the last three digits from the time
      result[0].erase(result[0].length()-3);
      int resultint;
      istringstream convertint(result[0]);
      if( !(convertint >> resultint) ) resultint = 0;
      long long resultll;
      istringstream convertll(result[1]);
      if( !(convertll >> resultll) ) resultll = 0;
      m[resultint] = resultll;
    }
    else {
      cerr << "[FindT10]: Warning, the T10 list is not in proper csv format\n";
      return -1;
    }
  }
  NA62ConditionsService::GetInstance()->Close(fDIMRecoveryFileName);

  map<int,long long>::iterator greater = m.upper_bound(timestamp);
  if ( greater == m.end() ) {
    if ( m.rbegin()->first == timestamp ){
      return m.rbegin()->second;
    }
    else {
      cerr << "[FindT10]: Warning , burst-time beyond last list entry\n";
      return -1;
    }
  }
  else if ( greater == m.begin() ){
    cerr << "[FindT10]: Warning , burst-time before first list entry\n";
    return -1;
  }
  else {
    map<int,long long>::iterator lesser = greater;
    --lesser;

    if ( (timestamp - lesser->first) < (greater->first - timestamp) ){    // return the with UNIX time closest to input time
      if(abs(timestamp - lesser->first) <=20){
        cerr << "[FindT10] Found good entry in database, dt = " << abs(timestamp - lesser->first) << endl;
        return (float)(lesser->second)/1.e11;
      }

      if(abs(timestamp - lesser->first) >20 && abs(timestamp - lesser->first) <3600){
        cerr << "[FindT10] Warning: more than 20 Seconds away from available entry, dt = " << abs(timestamp - lesser->first) << endl;
        return (float)(lesser->second)/1.e11;
      }

      if(abs(timestamp - lesser->first) >=3600){
        cerr << "[FindT10] Warning: more than 1 hour away from available entry, not valid, dt = " << abs(timestamp - lesser->first) << endl;
        return -1;
      }
    }
    else {
      if(abs(greater->first - timestamp) <=20) {
        cerr << "[FindT10] Found good entry, dt = " << abs(greater->first - timestamp) << endl;
        return (float)(greater->second)/1.e11;
      }
      if(abs(greater->first - timestamp) >20 && abs(greater->first - timestamp) <3600) {
        cerr << "[FindT10] Warning: more than 20 Seconds away from available entry, dt = " << abs(greater->first - timestamp) << endl;
        return (float)(greater->second)/1.e11;
      }
      if(abs(greater->first - timestamp) >=3600) {
        cerr << "[FindT10] Warning: more than 1 hour away from available entry, not valid, dt = " << abs(greater->first - timestamp) << endl;
        return -1;
      }
    }
  }
  cerr << "[FindT10] Warning: execution of FindT10 failed\n";
  return -1;
}

void NA62Reconstruction::ReadEOB(){
  ReadSingleEvent(fNEventsInFile-1);
}

void NA62Reconstruction::EvaluateTriggerDriftT0(){
  /// \MemberDescr
  /// Get the TriggerDriftT0 candidates from the Cedar and RICH RO boards (RICHMult excluded).
  /// A first rough TriggerDriftT0 is evaluated via votation (40 ps bins).
  /// Then the mean of the candidates within 50ps from the rough TriggerDriftT0 is taken.
  /// \EndMemberDescr

  std::vector<Double_t> TriggerDriftT0Candidates;
  TriggerDriftT0Candidates.clear();

  for(UInt_t iReco=0;iReco<fReconstructions.size();iReco++){
    if(fReconstructions[iReco]->GetName()!="Cedar" && fReconstructions[iReco]->GetName()!="RICH") continue;
    if(fReconstructions[iReco]->GetNStations()<=0) continue;

    // Evaluate the TriggerDriftT0
    if(fReconstructions[iReco]->GetRawDecoder() && fReconstructions[iReco]->GetRawDecoder()->GetDecoder()){
      UInt_t NROBoardsPerStation = fReconstructions[iReco]->GetRawDecoder()->GetDecoder()->GetNROBoardsPerStation(0); //Cedar or RICH (not RICH mult!)
      UInt_t NROBoards   = fReconstructions[iReco]->GetRawDecoder()->GetDecoder()->GetNROBoards();
      UInt_t NROMezzaninesPerFullBoard = fReconstructions[iReco]->GetRawDecoder()->GetDecoder()->GetNROMezzaninesPerFullBoard();
      TH2F* InputHisto2D = fReconstructions[iReco]->GetRawDecoder()->GetDecoder()->GetHDigiTimeRawFine();
      if(!NROBoardsPerStation) continue;
      if(!NROBoards)    continue;
      if(!InputHisto2D) continue;
      // Get trigger drift candidates (one from each ROBoard)
      for(UInt_t iROBoard=0;iROBoard<NROBoardsPerStation;iROBoard++){
        Int_t iFirstBin = iROBoard*InputHisto2D->GetNbinsX()/NROBoards+1;
        Int_t iLastBin  = (iROBoard+1)*InputHisto2D->GetNbinsX()/NROBoards;
        TH1F * hDigiTimeRawFine1D = reinterpret_cast<TH1F*>(InputHisto2D->ProjectionY(Form("TriggerDriftT0_%02d",iROBoard),iFirstBin,iLastBin));
        // Find fit range
        Double_t MaxContent = hDigiTimeRawFine1D->GetBinContent(hDigiTimeRawFine1D->GetMaximumBin());
        Double_t PeakFraction = 0.70;
        Int_t iBeginOfPeak=1;
        for (Int_t i=hDigiTimeRawFine1D->GetMaximumBin(); i>=0; i--) {
          if (hDigiTimeRawFine1D->GetBinContent(i)<PeakFraction*MaxContent) {
            iBeginOfPeak = i;
            break;
          }
        }
        Int_t iEndOfPeak=hDigiTimeRawFine1D->GetNbinsX();
        for (Int_t i=hDigiTimeRawFine1D->GetMaximumBin(); i<=hDigiTimeRawFine1D->GetNbinsX()+1; i++) {
          if (hDigiTimeRawFine1D->GetBinContent(i)<PeakFraction*MaxContent) {
            iEndOfPeak = i;
            break;
          }
        }
        // Fit the peak with a gaussian
        TF1 * fGauss = new TF1("fGauss","gaus",hDigiTimeRawFine1D->GetBinCenter(iBeginOfPeak),hDigiTimeRawFine1D->GetBinCenter(iEndOfPeak));
        Int_t FitStatus = hDigiTimeRawFine1D->Fit("fGauss","NQR"); //0: success; any other values: failure
        Double_t NewT0Corrections = fGauss->GetParameter(1); //mean
        Double_t OldT0Corrections = fReconstructions[iReco]->GetStationT0(0)+fReconstructions[iReco]->GetRawDecoder()->GetDecoder()->GetROMezzanineT0(iROBoard*NROMezzaninesPerFullBoard);
        if(!FitStatus) TriggerDriftT0Candidates.push_back(NewT0Corrections-OldT0Corrections);
        delete fGauss;
        delete hDigiTimeRawFine1D;
      }
    }
  }

  // Vote for the TriggerDriftT0
  TH1F * hDriftT0 = new TH1F("hDriftT0","",1250,-25.02,24.98);
  for(UInt_t iDriftT0Candidate=0;iDriftT0Candidate<TriggerDriftT0Candidates.size();iDriftT0Candidate++){
    hDriftT0->Fill(TriggerDriftT0Candidates[iDriftT0Candidate]);
  }
  Double_t DriftT0Verdict = hDriftT0->GetBinCenter(hDriftT0->GetMaximumBin());
  Int_t DriftT0NVotes     = hDriftT0->GetBinContent(hDriftT0->GetMaximumBin());
  delete hDriftT0;

  // Refine DriftT0 estimate
  Double_t TriggerDriftT0 = 0.;
  Int_t NGoodCandidates = 0;
  for(UInt_t iDriftT0Candidate=0;iDriftT0Candidate<TriggerDriftT0Candidates.size();iDriftT0Candidate++){
    if(fabs(TriggerDriftT0Candidates[iDriftT0Candidate]-DriftT0Verdict)>0.050) continue;
    TriggerDriftT0+=TriggerDriftT0Candidates[iDriftT0Candidate];
    NGoodCandidates++;
  }
  if(NGoodCandidates) TriggerDriftT0/=NGoodCandidates;

  // Write the output file
  ofstream OutputTriggerDriftT0File;
  OutputTriggerDriftT0File.open(Form("Trigger-DriftT0.run%06d_%04d-run%06d_%04d.dat",
        NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID(),NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID(),NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID(),NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID())); //default
  OutputTriggerDriftT0File<<"### TriggerDriftT0 file for run " << NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID() << ", burst " << NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID() << std::endl;
  OutputTriggerDriftT0File<<"# Format: RunID BurstID TriggerDriftT0 RoughTriggerDriftT0 NVotes" << std::endl;
  OutputTriggerDriftT0File<<"# TriggerDriftT0 file Generated on  " << TimeString() << std::endl;
  OutputTriggerDriftT0File<<std::endl;
  OutputTriggerDriftT0File<<setw(6)<<NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID()<<setw(6)<<NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID()<<" "<<TriggerDriftT0<<" "<< DriftT0Verdict<< " "<< DriftT0NVotes <<std::endl;
  OutputTriggerDriftT0File.close();
}

void NA62Reconstruction::EvaluateInstantaneousIntensity(){

  // Find GigaTracker reconstruction
  Int_t RefReco = -1;
  for(UInt_t iReco=0;iReco<fReconstructions.size();iReco++){
    if(fReconstructions[iReco]->GetName()!="GigaTracker") continue;
    if(fReconstructions[iReco]->GetNStations()<=0) continue;
    RefReco = iReco;
  }
  if(RefReco<0) return;
  if(!fRecoEvents[RefReco]) return;

  Double_t ReferenceTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256;

  // Instantaneous intensity variables
  const Double_t CentralGap    =   5.00; //ns [-2.50,2.50]
  const Double_t LowerEdgeTime = -25.00; //ns
  const Double_t UpperEdgeTime = +25.00; //ns

  // Evaluate instantaneous beam intensity using GigaTracker
  Double_t  Lambda[3] = {0.,0.,0.};
  Double_t eLambda[3] = {0.,0.,0.};
  for (Int_t iHit=0; iHit<fRecoEvents[RefReco]->GetNHits(); iHit++) {
    TRecoVHit *RecoHit = static_cast<TRecoVHit*>(fRecoEvents[RefReco]->GetHit(iHit));
    TVDigi* Digi  = RecoHit->GetDigi();
    Double_t Time = RecoHit->GetTime();
    //Int_t ChID    = RecoHit->GetChannelID();
    Int_t StationID = static_cast<TRecoGigaTrackerHit*>(RecoHit)->GetStationNo();
    if(fabs(Time-ReferenceTime)<0.5*CentralGap)                continue; //in-time hit
    if(Time-ReferenceTime<LowerEdgeTime-fReconstructions[RefReco]->GetT0Correction(Digi)) continue; //hit too close to the lower edge
    if(Time-ReferenceTime>UpperEdgeTime-fReconstructions[RefReco]->GetT0Correction(Digi)) continue; //hit too close to the upper edge
    Lambda[StationID]++; // Best estimator of Poissonian parameter
  }
  Double_t  Average = 0.;
  Double_t eAverage = 0.;
  for (Int_t iStation=0; iStation<3; iStation++) {
    eLambda[iStation] = sqrt(Lambda[iStation]); //it must be before conversion
    // Conversion counts -> MHz
    Double_t SampleTimeInterval = UpperEdgeTime-LowerEdgeTime-CentralGap;
    Lambda[iStation] /= 0.001*SampleTimeInterval;
    eLambda[iStation] /= 0.001*SampleTimeInterval;
    Average  += Lambda[iStation];
    eAverage += pow(eLambda[iStation],2.);
  }
  Average/=3.;
  eAverage = sqrt(eAverage)/3.;

  NA62RecoManager::GetInstance()->GetEventHeader()->SetBeamInstantaneousIntensity(Average,eAverage);
}
