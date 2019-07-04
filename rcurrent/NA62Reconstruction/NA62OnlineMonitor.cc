// ---------------------------------------------------------------
// History:
//
// Created by Karim Massri (karim.massri@cern.ch) 2015-08-10
//
// ---------------------------------------------------------------

#include "TROOT.h"
#include "TSystem.h"
#include "TStyle.h"
#include "Riostream.h"
#include "TApplication.h"
#include "TColor.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TThread.h"
#include "TMutex.h"

#include "NA62BufferProto.hh"
#include "NA62RecoManager.hh"
#include "NA62Reconstruction.hh"
#include "CedarReconstruction.hh"
#include "CHANTIReconstruction.hh"
#include "CHODReconstruction.hh"
#include "GigaTrackerReconstruction.hh"
#include "HACReconstruction.hh"
#include "IRCReconstruction.hh"
#include "LAVReconstruction.hh"
#include "LKrReconstruction.hh"
#include "MUV0Reconstruction.hh"
#include "MUV1Reconstruction.hh"
#include "MUV2Reconstruction.hh"
#include "MUV3Reconstruction.hh"
#include "NewCHODReconstruction.hh"
#include "RICHReconstruction.hh"
#include "SACReconstruction.hh"
#include "SAVReconstruction.hh"
#include "SpectrometerReconstruction.hh"

#include "NA62OnlineMonitor.hh"
#include "NA62VOnlineMonitor.hh"
#include "CedarOnlineMonitor.hh"
#include "CHANTIOnlineMonitor.hh"
#include "CHODOnlineMonitor.hh"
#include "GigaTrackerOnlineMonitor.hh"
#include "HACOnlineMonitor.hh"
#include "IRCOnlineMonitor.hh"
#include "LAVOnlineMonitor.hh"
#include "LKrOnlineMonitor.hh"
#include "MUV0OnlineMonitor.hh"
#include "MUV1OnlineMonitor.hh"
#include "MUV2OnlineMonitor.hh"
#include "MUV3OnlineMonitor.hh"
#include "NewCHODOnlineMonitor.hh"
#include "RICHOnlineMonitor.hh"
#include "SACOnlineMonitor.hh"
#include "SAVOnlineMonitor.hh"
#include "SpectrometerOnlineMonitor.hh"

#include "NA62Global.hh"
#include "NA62ConditionsService.hh"

#include <sys/stat.h>
#include <unistd.h>

#ifdef DIM
#include <dim/dis.hxx>
#endif

NA62Reconstruction * NA62Reco;
Bool_t Finished;
NA62OnlineMonitor * NA62OM = 0;
CedarOnlineMonitor * CedarOM = 0;
CHANTIOnlineMonitor * CHANTIOM = 0;
CHODOnlineMonitor * CHODOM = 0;
GigaTrackerOnlineMonitor * GigaTrackerOM = 0;
HACOnlineMonitor * HACOM = 0;
IRCOnlineMonitor * IRCOM = 0;
LAVOnlineMonitor * LAVOM = 0;
LKrOnlineMonitor * LKrOM = 0;
MUV0OnlineMonitor * MUV0OM = 0;
MUV1OnlineMonitor * MUV1OM = 0;
MUV2OnlineMonitor * MUV2OM = 0;
MUV3OnlineMonitor * MUV3OM = 0;
NewCHODOnlineMonitor * NewCHODOM = 0;
RICHOnlineMonitor * RICHOM = 0;
SACOnlineMonitor * SACOM = 0;
SAVOnlineMonitor * SAVOM = 0;
SpectrometerOnlineMonitor * SpectrometerOM = 0;

Timer OMTimer(true);

void usage(char* name){
  std::cout << "Usage: "<< name << " [-h] [-B #MaxFiles] [-i InputFile.dat/.root] [-l InputListFile.txt] [-n #MaxEvents] [-o OutputFile.root] [-s seed] [-c ConfigFileName.conf] [-j #JumpFirstNEvents] [-N #MaxEventsPerBurst] [-C ConditionsDirectoryPath] [-e #ExitLevel] [-m]"
    << std::endl;
}

void * RunReco(void * /*ptr*/){

  //Add timers
  unsigned int LoopTimer   = OMTimer.AddTimer("Total Loop",1);
  unsigned int ResetTimer  = OMTimer.AddTimer("Reset",2);
  unsigned int UpdateTimer = OMTimer.AddTimer("Update",2);
  unsigned int PrintTimer  = OMTimer.AddTimer("Print",2);

  gSystem->Sleep(3000);
  Int_t RunID, BurstID, PreviousBurstID = -1;
  std::cout << "Starting reconstruction" << std::endl;
  while(NA62Reco->NextEvent() && !Finished){

    // Terminate if no online monitor module is enabled
    if (!NA62OM && !CedarOM && !CHANTIOM && !CHODOM && !GigaTrackerOM && !HACOM && !IRCOM &&
        !LAVOM && !LKrOM && !MUV0OM && !MUV1OM && !MUV2OM && !MUV3OM && !NewCHODOM && !RICHOM &&
        !SACOM && !SAVOM && !SpectrometerOM) {
      NA62VReconstruction::Exception("No OnlineMonitor modules found!");
    }

    OMTimer.StartTimer(LoopTimer);
    //------------------------------ ResetTabs and UpdateTabs initialization ------------------------------//
    // ResetTabs:  Tabs are reset every new burst
    // UpdateTabs: Tabs are updated every time the events in the buffer are all processed
    Bool_t ResetTabs  = kFALSE;
    Bool_t UpdateTabs = kFALSE;
    //Bool_t EndOfBurst = kFALSE;
    RunID = NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID();
    BurstID = NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID();
    if(PreviousBurstID!=BurstID) ResetTabs = kTRUE;
    if(NA62Reco->GetChunkIsFinished() && NA62Reco->GetRawFileEOF()) UpdateTabs = kTRUE;
    PreviousBurstID = BurstID;
    //if(isL0EOB(NA62RecoManager::GetInstance()->GetEventHeader()->GetTriggerType()<<2)) EndOfBurst = kTRUE;
    //-----------------------------------------------------------------------------------------------------//

    if(ResetTabs){
      TThread::Lock();
      //cout << "[NA62OnlineMonitor]   ----- ResetTabs  -----" << std::endl;
      OMTimer.StartTimer(ResetTimer);
      if(NA62OM)         NA62OM->GetMainWindow()->SetWindowName(Form("NA62 Online Monitor [Run %d, Burst %d]",RunID,BurstID));
      if(NA62OM)         NA62OM->BurstReset(BurstID);
      if(CedarOM)        CedarOM->BurstReset(BurstID);
      if(CHANTIOM)       CHANTIOM->BurstReset(BurstID);
      if(CHODOM)         CHODOM->BurstReset(BurstID);
      if(GigaTrackerOM)  GigaTrackerOM->BurstReset(BurstID);
      if(HACOM)          HACOM->BurstReset(BurstID);
      if(IRCOM)          IRCOM->BurstReset(BurstID);
      if(LAVOM)          LAVOM->BurstReset(BurstID);
      if(LKrOM)          LKrOM->BurstReset(BurstID);
      if(MUV0OM)         MUV0OM->BurstReset(BurstID);
      if(MUV1OM)         MUV1OM->BurstReset(BurstID);
      if(MUV2OM)         MUV2OM->BurstReset(BurstID);
      if(MUV3OM)         MUV3OM->BurstReset(BurstID);
      if(NewCHODOM)      NewCHODOM->BurstReset(BurstID);
      if(RICHOM)         RICHOM->BurstReset(BurstID);
      if(SACOM)          SACOM->BurstReset(BurstID);
      if(SAVOM)          SAVOM->BurstReset(BurstID);
      if(SpectrometerOM) SpectrometerOM->BurstReset(BurstID);
      OMTimer.StopTimer(ResetTimer);
      TThread::UnLock();
    }

    if(UpdateTabs){
      TThread::Lock();
      //cout << "[NA62OnlineMonitor]   ----- UpdateTabs -----" << std::endl;
      OMTimer.StartTimer(UpdateTimer);
      if(NA62OM)         NA62OM->Update(BurstID);
      if(CedarOM)        CedarOM->Update(BurstID);
      if(CHANTIOM)       CHANTIOM->Update(BurstID);
      if(CHODOM)         CHODOM->Update(BurstID);
      if(GigaTrackerOM)  GigaTrackerOM->Update(BurstID);
      if(HACOM)          HACOM->Update(BurstID);
      if(IRCOM)          IRCOM->Update(BurstID);
      if(LAVOM)          LAVOM->Update(BurstID);
      if(LKrOM)          LKrOM->Update(BurstID);
      if(MUV0OM)         MUV0OM->Update(BurstID);
      if(MUV1OM)         MUV1OM->Update(BurstID);
      if(MUV2OM)         MUV2OM->Update(BurstID);
      if(MUV3OM)         MUV3OM->Update(BurstID);
      if(NewCHODOM)      NewCHODOM->Update(BurstID);
      if(RICHOM)         RICHOM->Update(BurstID);
      if(SACOM)          SACOM->Update(BurstID);
      if(SAVOM)          SAVOM->Update(BurstID);
      if(SpectrometerOM) SpectrometerOM->Update(BurstID);
      OMTimer.StopTimer(UpdateTimer);
      if(NA62Reco->GetSaveOnlineMonitorPlots() && NA62Reco->GetRawFileEOF() && (BurstID%10==0)){
        //cout << "[NA62OnlineMonitor]   ----- EndOfBurst -----" << std::endl;
        OMTimer.StartTimer(PrintTimer);
        if(NA62OM)         NA62OM->Print(BurstID);
        if(CedarOM)        CedarOM->Print(BurstID);
        if(CHANTIOM)       CHANTIOM->Print(BurstID);
        if(CHODOM)         CHODOM->Print(BurstID);
        if(GigaTrackerOM)  GigaTrackerOM->Print(BurstID);
        if(HACOM)          HACOM->Print(BurstID);
        if(IRCOM)          IRCOM->Print(BurstID);
        if(LAVOM)          LAVOM->Print(BurstID);
        if(LKrOM)          LKrOM->Print(BurstID);
        if(MUV0OM)         MUV0OM->Print(BurstID);
        if(MUV1OM)         MUV1OM->Print(BurstID);
        if(MUV2OM)         MUV2OM->Print(BurstID);
        if(MUV3OM)         MUV3OM->Print(BurstID);
        if(NewCHODOM)      NewCHODOM->Print(BurstID);
        if(RICHOM)         RICHOM->Print(BurstID);
        if(SACOM)          SACOM->Print(BurstID);
        if(SAVOM)          SAVOM->Print(BurstID);
        if(SpectrometerOM) SpectrometerOM->Print(BurstID);
        OMTimer.StopTimer(PrintTimer);
      }
      TThread::UnLock();
    }
    OMTimer.StopTimer(LoopTimer);
  }
  NA62Reco->PrintInfo();
  OMTimer.PrintTimers(cout);
  Finished = kTRUE;
  return 0;
}

int main(Int_t argc, char **argv){
  extern char *optarg;
  int opt;
  TString OutputFileName("OutputFile.root");
  TString InputFileName("InputFile.root");
  TString ConfFileName("config/NA62Reconstruction.conf");
  // Available Exit Levels:
  //   0: exit if any not found or empty file is detected [for production]
  //   1: ignore not found files, exit if any empty file is detected [default]
  // >=2: ignore not found or empty files
  Int_t   ExitLevel=0;
  TString ConditionsDirPath("");
  TString InputListFileName;
  Int_t NFiles = 100000, NEvt = 100000000, JumpNEvt=0, NEvtPerFile=100000000;
  UInt_t Seed = 4357;
  Bool_t MultiCanvases = false; //if true, one separated canvas for each detector
  //struct stat filestat;

  unsigned int Init = OMTimer.AddTimer("Init",1);

  Int_t n_options_read = 0;
  Int_t nb=0, nc=0, nC=0, ne=0, ni=0, nj=0, nl=0, nn=0, nN=0, no=0, ns=0;
  while ((opt = getopt(argc, argv, "B:c:C:e:h:i:j:l:m:n:N:o:s:")) != -1) { //it must end with a :
    n_options_read++;
    switch (opt) {
      case 'B':
        nb++;
        NFiles = TString(optarg).Atoi();
        break;
      case 'c':
        nc++;
        ConfFileName = TString(optarg);
        break;
      case 'C':
        nC++;
        ConditionsDirPath = TString(optarg);
        break;
      case 'e':
        ne++;
        ExitLevel = TString(optarg).Atoi();
        break;
      case 'h':
        usage(argv[0]);
        return 0;
      case 'i':
        ni++;
        InputFileName = TString(optarg);
        break;
      case 'j':
        nj++;
        JumpNEvt = TString(optarg).Atoi();
        break;
      case 'l':
        nl++;
        InputListFileName = TString(optarg);
        break;
      case 'm':
        MultiCanvases = true;
        break;
      case 'n':
        nn++;
        NEvt = TString(optarg).Atoi();
        break;
      case 'N':
        nN++;
        NEvtPerFile = TString(optarg).Atoi();
        break;
      case 'o':
        no++;
        OutputFileName = TString(optarg);
        break;
      case 's':
        ns++;
        Seed = (UInt_t)TString(optarg).Atoi();
        break;
      default:
        usage(argv[0]);
        return 0;
    }
  }

  // Sanity checks on the input
  if (!n_options_read || NEvt<=0 || NFiles<=0 || JumpNEvt<0 || NEvtPerFile<0) {
    usage(argv[0]);
    return 1;
  }
  if (nb>1 || nc>1 || nC>1 || ne>1 || ni>1 || nj>1 || nl>1 || nn>1 || nN>1 || no>1 || ns>0) {
    std::cerr << "[NA62OnlineMonitor] Multiple arguments of the same type are not allowed" << std::endl;
    return 1;
  }

  // Protection against potentially incorrect output filenames
  struct stat buffer;
  if (!OutputFileName.EndsWith(".root") && !stat(OutputFileName.Data(), &buffer)) {
    std::cout << " [NA62OnlineMonitor] Output file exists and is not *.root: potentially a destructive call" << std::endl;
    return 1;
  }

  OMTimer.StartTimer(Init);

  TObjArray InputFileNameList;
  TFile * OutputFile;
  NA62ConditionsService::GetInstance()->SetExitLevel(ExitLevel);
  NA62ConditionsService::GetInstance()->SetExternalCDBDirectoryPath(ConditionsDirPath);
  if(InputListFileName.CompareTo("")) { //-l option used
    NA62Reco = new NA62Reconstruction(InputListFileName, ConfFileName, OutputFileName, NEvt, NEvtPerFile, JumpNEvt, Seed, NFiles);
  }
  else if(InputFileName.CompareTo("")){ //-i option used
    InputFileNameList.Add(new TObjString(InputFileName.Data()));
    OutputFile = TFile::Open(OutputFileName.Data(),"RECREATE");
    NA62Reco = new NA62Reconstruction(&InputFileNameList, ConfFileName, OutputFile, NEvt, NEvtPerFile, JumpNEvt, Seed);
  }
  NA62Reco->NextEvent(); //find the modules to be disabled

  //if(InputFileNameList.GetEntries() == 0) {
  //    perror(Form("Input File"));
  //    exit(kWrongConfiguration);
  //}

  TApplication NA62OnlineMonitorApp("NA62OnlineMonitorApp", &argc, argv);
  gROOT->Reset("a");
  gSystem->ResetSignal(kSigBus, true);
  gSystem->ResetSignal(kSigSegmentationViolation, true);
  gSystem->ResetSignal(kSigIllegalInstruction, true);
  gSystem->ResetSignal(kSigFloatingException, true);

  if(gROOT->IsBatch()) {
    std::cout << "error: NA62OnlineMonitor cannot run in batch mode." << std::endl;
    exit(kWrongConfiguration);
  }

#ifdef DIM
  //Set DNS Node -  address resolution
  DimServer::setDnsNode( NA62Reco->GetDnsServerName().Data() );
  DimServer::start("ONLINEMONITOR");
  std::cout << "DIM: Connected to Dns Node" << std::endl;
#endif

  gStyle->SetOptFit();
  gStyle->SetOptStat(111111);
  gStyle->SetPalette(1);
  gStyle->SetCanvasBorderMode(0);
  gStyle->SetFrameBorderMode(0);
  gStyle->SetTitleFillColor(kWhite);
  gStyle->SetFrameFillColor(kWhite);
  gStyle->SetStatColor(kWhite);
  gStyle->SetCanvasColor(kWhite);
  gStyle->SetPadColor(kWhite);

  TRootBrowser* NA62MainWindow  = 0;
  if(!MultiCanvases) {
    NA62MainWindow = new TRootBrowser();
    NA62MainWindow->SetWindowName("NA62 Online Monitor");
    const_cast<TGWindow*>(NA62MainWindow->GetTabLeft()->GetParent())->Resize(1,0);
    const_cast<TGWindow*>(NA62MainWindow->GetTabBottom()->GetParent())->Resize(0,1);
    NA62MainWindow->MoveResize(0, 0, 2000, 1300);
    NA62MainWindow->Layout();
  }

  Int_t OMMode = NA62Reco->GetOnlineMonitorMode();

  CedarReconstruction * CedarReco = static_cast<CedarReconstruction*>(NA62Reco->FindReco("Cedar"));
  CHANTIReconstruction * CHANTIReco = static_cast<CHANTIReconstruction*>(NA62Reco->FindReco("CHANTI"));
  CHODReconstruction * CHODReco = static_cast<CHODReconstruction*>(NA62Reco->FindReco("CHOD"));
  GigaTrackerReconstruction * GigaTrackerReco = static_cast<GigaTrackerReconstruction*>(NA62Reco->FindReco("GigaTracker"));
  HACReconstruction * HACReco = static_cast<HACReconstruction*>(NA62Reco->FindReco("HAC"));
  IRCReconstruction * IRCReco = static_cast<IRCReconstruction*>(NA62Reco->FindReco("IRC"));
  LAVReconstruction * LAVReco = static_cast<LAVReconstruction*>(NA62Reco->FindReco("LAV"));
  LKrReconstruction * LKrReco = static_cast<LKrReconstruction*>(NA62Reco->FindReco("LKr"));
  MUV0Reconstruction * MUV0Reco = static_cast<MUV0Reconstruction*>(NA62Reco->FindReco("MUV0"));
  MUV1Reconstruction * MUV1Reco = static_cast<MUV1Reconstruction*>(NA62Reco->FindReco("MUV1"));
  MUV2Reconstruction * MUV2Reco = static_cast<MUV2Reconstruction*>(NA62Reco->FindReco("MUV2"));
  MUV3Reconstruction * MUV3Reco = static_cast<MUV3Reconstruction*>(NA62Reco->FindReco("MUV3"));
  NewCHODReconstruction * NewCHODReco = static_cast<NewCHODReconstruction*>(NA62Reco->FindReco("NewCHOD"));
  RICHReconstruction * RICHReco = static_cast<RICHReconstruction*>(NA62Reco->FindReco("RICH"));
  SACReconstruction * SACReco = static_cast<SACReconstruction*>(NA62Reco->FindReco("SAC"));
  SAVReconstruction * SAVReco = static_cast<SAVReconstruction*>(NA62Reco->FindReco("SAV"));
  SpectrometerReconstruction * SpectrometerReco = static_cast<SpectrometerReconstruction*>(NA62Reco->FindReco("Spectrometer"));
  std::cout << "Creating Online Monitor modules.." << std::endl;
  if(NA62Reco)         NA62OM         = new NA62OnlineMonitor(NA62MainWindow,NA62Reco,OMMode);
  if(CedarReco)        CedarOM        = new CedarOnlineMonitor(NA62MainWindow,CedarReco,OMMode);
  if(CHANTIReco)       CHANTIOM       = new CHANTIOnlineMonitor(NA62MainWindow,CHANTIReco,OMMode);
  if(CHODReco)         CHODOM         = new CHODOnlineMonitor(NA62MainWindow,CHODReco,OMMode);
  if(GigaTrackerReco)  GigaTrackerOM  = new GigaTrackerOnlineMonitor(NA62MainWindow,GigaTrackerReco,OMMode);
  if(HACReco)          HACOM          = new HACOnlineMonitor(NA62MainWindow,HACReco,OMMode);
  if(IRCReco)          IRCOM          = new IRCOnlineMonitor(NA62MainWindow,IRCReco,OMMode);
  if(LAVReco)          LAVOM          = new LAVOnlineMonitor(NA62MainWindow,LAVReco,OMMode);
  if(LKrReco)          LKrOM          = new LKrOnlineMonitor(NA62MainWindow,LKrReco,OMMode);
  if(MUV0Reco)         MUV0OM         = new MUV0OnlineMonitor(NA62MainWindow,MUV0Reco,OMMode);
  if(MUV1Reco)         MUV1OM         = new MUV1OnlineMonitor(NA62MainWindow,MUV1Reco,OMMode);
  if(MUV2Reco)         MUV2OM         = new MUV2OnlineMonitor(NA62MainWindow,MUV2Reco,OMMode);
  if(MUV3Reco)         MUV3OM         = new MUV3OnlineMonitor(NA62MainWindow,MUV3Reco,OMMode);
  if(NewCHODReco)      NewCHODOM      = new NewCHODOnlineMonitor(NA62MainWindow,NewCHODReco,OMMode);
  if(RICHReco)         RICHOM         = new RICHOnlineMonitor(NA62MainWindow,RICHReco,OMMode);
  if(SACReco)          SACOM          = new SACOnlineMonitor(NA62MainWindow,SACReco,OMMode);
  if(SAVReco)          SAVOM          = new SAVOnlineMonitor(NA62MainWindow,SAVReco,OMMode);
  if(SpectrometerReco) SpectrometerOM = new SpectrometerOnlineMonitor(NA62MainWindow,SpectrometerReco,OMMode);

  Finished = kFALSE;
  printf("Starting Thread 0\n");
  TThread * t = new TThread("t0", RunReco, nullptr);
  t->Run();

  OMTimer.StopTimer(Init);

  NA62OnlineMonitorApp.Run();
  exit(0);
}
