#include <stdio.h>
#include <cstdio>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <time.h>
#include <sys/stat.h>

#include "TROOT.h"
#include "TSystem.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TString.h"
#include "TThread.h"
#include "TGTab.h"

#include "TStyle.h"
#include "Riostream.h"
#include "TApplication.h"
#include "TEveManager.h"
#include "TEveEventManager.h"
#include "TRegexp.h"

#include "LAVPrimMon.hh"
#include "MUVPrimMon.hh"
#include "RICHPrimMon.hh"
#include "CHODPrimMon.hh"
#include "AllPrimMon.hh"
#include "TALKPrimMon.hh"
#include "LKRPrimMon.hh"
#include "IRCPrimMon.hh"

#ifdef DIM
#include <dim/dic.hxx>
#endif

using namespace std;


Bool_t Finished;
Bool_t FileInput;
TString InFilename;
TString DirInput;
TString PlotsDir;
TString PlotsWebDir;
LAVPrimMon * LAVMon = 0;
RICHPrimMon * RICHMon = 0;
CHODPrimMon * CHODMon = 0;
MUVPrimMon * MUVMon = 0;
AllPrimMon * AllMon = 0;
IRCPrimMon * IRCMon = 0;
TALKPrimMon * TALKMon = 0;
LKRPrimMon * LKRMon = 0;

//------------------------------------------------------//
// DIM client: SOB/EOB
#ifdef DIM
class Burst : public DimClient {
  DimUpdatedInfo Sob;
  DimUpdatedInfo Eob;
public:
  Burst(): Sob("NA62/Timing/SOB",-1,this), Eob("NA62/Timing/EOB",-1,this), InBurst(0), TimeStamp(0){}
  int GetInBurst(){return InBurst;}
  int GetTS() {return TimeStamp;}
  void infoHandler() {
    DimUpdatedInfo *curr=(DimUpdatedInfo *)getInfo();
    if (curr==&Sob) {
      TimeStamp = curr->getInt();
      cout << "SOB received " << curr->getInt() << endl;
      InBurst = 1;
    }
    if (curr==&Eob) {
      cout << "EOB received " << curr->getInt() << endl;
      cout << endl;
      InBurst = 0;
    }
  }
  int InBurst;
  int TimeStamp;
};
#endif
//------------------------------------------------------//
// Get the last generated primitive dump file
int GetDir (string Dir, vector<string> &Files)
{
  // DIR *dp;
  // struct dirent *dirp;
  // if((dp = opendir(dir.c_str())) == NULL) {
  //   cout << "Error(" << errno << ") opening " << dir << endl;
  //   return errno;
  // }
  // while ((dirp = readdir(dp)) != NULL) {
  //   files.push_back(string(dirp->d_name));
  //   cout << string(dirp->d_name) << endl;
  // }
  // closedir(dp);

  // take the last few files in the dir and write to a temp file
  system(Form("ls -ltr %s | tail > tempfile.txt", Dir.c_str() ) ) ;
  gSystem->Sleep(10) ;
  ifstream MyFile;
  MyFile.open ("tempfile.txt");

  string Line;
  if (MyFile.is_open()){
    while ( getline (MyFile,Line) ){
      cout << Line << " -> " ;
      int Pos = Line.rfind( string(" ") ) ;
      Pos += 1;
      Line = Line.substr( Pos, Line.size()-Pos ) ;
      Files.push_back( Line ) ;
    }
    MyFile.close();
  }
  return 0;
}
//------------------------------------------------------//
void* RunMon(void * /*ptr*/){
  TString InputFileName("");
  TString OldInputFileName("");
  TFile* InFile = NULL ;
  TTree * TLAV = NULL ;
  TTree * TRICH = NULL ;
  TTree * TCHOD = NULL ;
  TTree * TMUV = NULL ;
  TTree * TLKR = NULL ;
  TTree * TTALK = NULL ;
  TTree * TIRC = NULL ;

  if (FileInput){
    // Parsing the filename to get the run and burst IDs
    Int_t pos = InFilename.Index("run");
    TString number = InFilename(pos+3, 4);
    UInt_t RunID = atoi(number);
    if (RunID == 0 || pos == -1) {
      cout << "[warning] Could not parse run number from file name, using 0.\n";
      RunID = 0;
    }
    pos = InFilename.Index("burst");
    number = InFilename(pos+5, 4);
    UInt_t BurstID = atoi(number);

    if (BurstID == 0 || pos == -1) {
      cout << "[warning] Could not parse burst number from file name, using 0.\n";
      BurstID = 0;
    }
    TString RunBurstName(Form(PlotsDir+"/run%d_burst%d_",RunID, BurstID));

    TString RunBurst(Form("Run: %d  Burst: %d",RunID, BurstID));

    printf("Processing Run: %d, Burst: %d \n", RunID, BurstID);

    InFile = new TFile( Form("%s",InFilename.Data()) ) ;
    TLAV = NULL ;
    InFile->GetObject("LAV", TLAV);
    TMUV = NULL ;
    InFile->GetObject("MUV3", TMUV);
    TRICH = NULL ;
    InFile->GetObject("RICH", TRICH);
    TCHOD = NULL ;
    InFile->GetObject("CHOD", TCHOD);
    TLKR = NULL ;
    InFile->GetObject("LKr", TLKR);
    TTALK = NULL ;
    InFile->GetObject("TALK", TTALK);
    TIRC = NULL ;
    InFile->GetObject("IRC", TIRC);

    cout << "now updating the All tab. If any trees are missing then the corresponding plots won't be made." << endl ;
    if(AllMon) AllMon->Update(TLAV,TRICH, TCHOD, TMUV, TTALK, TIRC, TLKR, RunBurst, RunBurstName);

    if(TLAV){
      cout << "now updating the LAV tab. N entries: "<< TLAV->GetEntries() << endl ;
      if(LAVMon && TLAV->GetEntries()!=0) LAVMon->Update(TLAV, RunBurst, RunBurstName);
    }
    else cout << " the LAV tree is missing. Will not update LAV tab" << endl ;

    if(TRICH){
      cout << "now updating the RICH tab. N entries: "<< TRICH->GetEntries() << endl ;
      if(RICHMon && TRICH->GetEntries()!=0) RICHMon->Update(TRICH, RunBurst, RunBurstName);
    }
    else cout << " the RICH tree is missing. Will not update RICH tab" << endl ;

    if(TCHOD){
      cout << "now updating the CHOD tab. N entries: "<< TCHOD->GetEntries() << endl ;
      if(CHODMon&& TCHOD->GetEntries()!=0) CHODMon->Update(TCHOD, RunBurst, RunBurstName);
    }
    else cout << " the CHOD tree is missing. Will not update CHOD tab" << endl ;

    if(TMUV){
      cout << "now updating the MUV3 tab. N entries: "<< TMUV->GetEntries() << endl ;
      if(MUVMon && TMUV->GetEntries()!=0) MUVMon->Update(TMUV, RunBurst, RunBurstName);
    }
    else cout << " the MUV3 tree is missing. Will not update MUV3 tab" << endl ;

    if(TLKR){
      cout << "now updating the LKR tab. N entries: "<< TLKR->GetEntries() << endl ;
      if(LKRMon && TLKR->GetEntries()!=0) LKRMon->Update(TLKR, RunBurst, RunBurstName);
    }
    else cout << " the LKR tree is missing. Will not update LKR tab" << endl ;

    if(TIRC){
      cout << "now updating the IRC (NewCHOD) tab. N entries: "<< TIRC->GetEntries() << endl ;
      if(IRCMon && TIRC->GetEntries()!=0) IRCMon->Update(TIRC, RunBurst, RunBurstName);
    }
    else cout << " the IRC (NewCHOD) tree is missing. Will not update IRC tab" << endl ;

    if(TTALK){
      cout << "now updating the TALK tab. N entries: "<< TTALK->GetEntries() << endl ;
      if(TALKMon && TTALK->GetEntries()!=0) TALKMon->Update(TTALK, RunBurst, RunBurstName);
    }
    else cout << " the TALK tree is missing. Will not update TALK tab" << endl ;

  }

  else {

#ifdef DIM
    struct tm *Time;
    time_t Time_t;
    Int_t NFiles;
    Burst Timing;
    int InBurst = 0;

    while(!Finished){
      gSystem->Sleep(1000);
      if(Timing.GetInBurst() && !InBurst){
	InBurst = 1;
	cout << "SOB" << endl;
      }
      if(!Timing.GetInBurst() && InBurst){
	cout << "EOB" << endl;
	gSystem->Sleep(1000);
	InBurst = 0;

	vector<string> files = vector<string>();

	// get list of files in Dir.
	//int succ = GetDir(DirInput.Data(),files) ;

	// get number of files in Dir
	NFiles = files.size();
	if(NFiles == 0){
	  cout << " No files in the list. Waiting 10 seconds and trying again." << endl ;
	  gSystem->Sleep(10000) ;
	  continue;
	}

	InputFileName = DirInput+"/"+files[NFiles-2].c_str();
	cout << " input file is " << files[NFiles-2].c_str() << endl ;
	if(InputFileName == OldInputFileName){
	  cout << " the latest file is currently displayed. I will wait for a new file." << endl ;
	  continue ;
	}
	OldInputFileName = InputFileName ;

	// Parsing the filename to get the run and burst IDs
	Int_t pos = InputFileName.Index("run");
	TString number = InputFileName(pos+3, 4);
	UInt_t RunID = atoi(number);
	if (RunID == 0 || pos == -1) {
	  RunID = 0;
	}
	pos = InputFileName.Index("burst");
	number = InputFileName(pos+5, 4);
	UInt_t BurstID = atoi(number);

	if (BurstID == 0 || pos == -1) {
	  BurstID = 0;
	}
	TString RunBurstName(Form(PlotsDir+"/run%d_burst%d_",RunID, BurstID));

	printf("Processing Run: %d, Burst: %d \n", RunID, BurstID);

	InFile = new TFile( Form("%s",InputFileName.Data()) ) ;
	TLAV = NULL ;
	InFile->GetObject("LAV", TLAV);
	TMUV = NULL ;
	InFile->GetObject("MUV3", TMUV);
	TRICH = NULL ;
	InFile->GetObject("RICH", TRICH);
	TCHOD = NULL ;
	InFile->GetObject("CHOD", TCHOD);
	TLKR = NULL ;
	InFile->GetObject("LKr", TLKR);
	TTALK = NULL ;
	InFile->GetObject("TALK", TTALK);
	TIRC = NULL ;
	InFile->GetObject("IRC", TIRC);

	// get local time
	Time_t = Timing.GetTS();
	Time = localtime(&Time_t);
	string Months12[]= {"Jan", "Feb", "Mar", "Apr",
			    "May", "Jun", "Jul", "Aug",
			    "Sep", "Oct", "Nov", "Dec"};
	TString DateTime(Form("%02d %s %d %02d:%02d:%02d  Run: %d   Burst: %d",Time->tm_mday , Months12[Time->tm_mon].c_str(),1900+Time->tm_year , Time->tm_hour, Time->tm_min, Time->tm_sec, RunID, BurstID));
	cout << "now updating the All tab. If any trees are missing then the corresponding plots won't be made." << endl ;

	if(AllMon) AllMon->Update(TLAV,TRICH, TCHOD, TMUV, TTALK, TIRC, TLKR, DateTime, RunBurstName);

	if(TMUV){
	  cout << "now updating the MUV tab. N entries: "<< TMUV->GetEntries() << endl ;
	  if(MUVMon && TMUV->GetEntries()!=0) MUVMon->Update(TMUV, DateTime, RunBurstName);
	}
	else cout << "No MUV tree" << endl ;

	if(TIRC){
	  cout << "now updating the IRC (NewCHOD) tab. N entries: "<< TIRC->GetEntries() << endl ;
	  if(IRCMon && TIRC->GetEntries()!=0) IRCMon->Update(TIRC, DateTime, RunBurstName);
	}
	else cout << " the IRC (NewCHOD) tree is missing. Will not update IRC tab" << endl ;

	if(TCHOD){
	  cout << "now updating the CHOD tab. N entries: "<< TCHOD->GetEntries() << endl ;
	  if(CHODMon&& TCHOD->GetEntries()!=0) CHODMon->Update(TCHOD, DateTime, RunBurstName);
	}
	else cout << " the CHOD tree is missing. Will not update CHOD tab" << endl ;

	if(TRICH){
	  cout << "now updating the RICH tab. N entries: "<< TRICH->GetEntries() << endl ;
	  if(RICHMon&& TRICH->GetEntries()!=0) RICHMon->Update(TRICH, DateTime, RunBurstName);
	}
	else cout << " the RICH tree is missing. Will not update RICH tab" << endl ;

	if(TLKR){
	  cout << "now updating the LKR tab. N entries: "<< TLKR->GetEntries() << endl ;
	  if(LKRMon&& TLKR->GetEntries()!=0) LKRMon->Update(TLKR, DateTime, RunBurstName);
	}
	else cout << " the LKR tree is missing. Will not update LKR tab" << endl ;

	if(TTALK){
	  cout << "now updating the TALK tab. N entries: "<< TTALK->GetEntries() << endl ;
	  if(TALKMon&& TTALK->GetEntries()!=0) TALKMon->Update(TTALK, DateTime, RunBurstName);
	}
	else cout << " the TALK tree is missing. Will not update TALK tab" << endl ;

	if(TLAV){
	  cout << "now updating the LAV tab. N entries: "<< TLAV->GetEntries() << endl ;
	  if(LAVMon&& TLAV->GetEntries()!=0) LAVMon->Update(TLAV, DateTime, RunBurstName);
	}
	else cout << " the LAV tree is missing. Will not update LAV tab" << endl ;

	InFile->Close("R");
      }
    }
#endif
    cout << "ERROR: The PrimMonitor needs DIM to work! Recompile it with DIM!" << endl;
    Finished = kTRUE;
    return 0 ;

  }
  return nullptr;
}
//------------------------------------------------------//
int main(int argc, char* argv[]) {

  // load Primitives.conf file
  std::ifstream confFile("config/Primitives.conf");
  if (!confFile.is_open()) {
    printf("Cannot open config/Primitives.conf");
    exit(1);
  }
  Int_t SavePlots = 0;
  TString Line;
  while (Line.ReadLine(confFile)) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("SavePrimitiveMonitorPlots")) {
      SavePlots = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("PrimitiveMonitorPlotsDir")) {
      TObjArray * l = Line.Tokenize( " " );
      PlotsDir = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("PrimitiveMonitorPlotsWebDir")) {
      TObjArray * l = Line.Tokenize( " " );
      PlotsWebDir = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("DecodedPrimitiveDumpsDir")) {
      TObjArray * l = Line.Tokenize( " " );
      DirInput = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
  }

  confFile.close();
  cout << PlotsDir.Data() << " " << SavePlots << endl;
  struct stat filestat;
  if(SavePlots && stat(PlotsDir, &filestat)!=0) mkdir(PlotsDir, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);

  //
  int opt;
  extern char *optarg;
  while ((opt = getopt(argc, argv, "i:h")) != -1) {
    switch (opt) {
    case 'h': printf("usage: %s [OPTION] [FILE] \n", argv[0]);
      printf("OPTIONS: \n");
      printf("-h   help \n");
      printf("-i   input file \n");
      printf("The PrimMonitor operates in two modes. It can process \nan input file (-i) or it can operate in monitoring mode (no option). \nIn monitoring mode the PrimMonitor just reads the last primitives \nroot file produced after the EOB signal. \n");
      return 0;
    case 'i':
      InFilename = TString(optarg);
      FileInput = kTRUE;
      break;

    default:
      FileInput = kFALSE;
    }
  }

  TApplication PrimMonApp("PrimMonApp", &argc, argv);
  gROOT->Reset("a");
  gSystem->ResetSignal(kSigBus, true);
  gSystem->ResetSignal(kSigSegmentationViolation, true);
  gSystem->ResetSignal(kSigIllegalInstruction, true);
  gSystem->ResetSignal(kSigFloatingException, true);
  gStyle->SetOptStat(0);
  gStyle->SetPadColor(kWhite);
  gStyle->SetTitleFontSize(0.06);
  gStyle->SetTitleOffset(0.85);
  gStyle->SetTitleOffset(0.7, "X");
  gStyle->SetTitleOffset(0.7, "Y");
  gStyle->SetTitleSize(0.06, "X");
  gStyle->SetTitleSize(0.06, "Y");
  gStyle->SetLabelSize(0.05, "X");
  gStyle->SetLabelSize(0.05, "Y");
  TRootBrowser* NA62MainWindow = new TRootBrowser();
  NA62MainWindow->SetWindowName("NA62 Primitive Monitor");
  ((TGWindow*)NA62MainWindow->GetTabLeft()->GetParent())->Resize(1,0);
  ((TGWindow*)NA62MainWindow->GetTabBottom()->GetParent())->Resize(0,1);
  NA62MainWindow->MoveResize(100, 100, 1600, 800);
  NA62MainWindow->Layout();

  AllMon  = new AllPrimMon(NA62MainWindow,  "All");
  RICHMon = new RICHPrimMon(NA62MainWindow, "RICH");
  CHODMon = new CHODPrimMon(NA62MainWindow, "CHOD");
  IRCMon  = new IRCPrimMon(NA62MainWindow,  "NewCHOD");
  MUVMon  = new MUVPrimMon(NA62MainWindow,  "MUV3");
  LAVMon  = new LAVPrimMon(NA62MainWindow,  "LAV");
  LKRMon  = new LKRPrimMon(NA62MainWindow,  "LKR");
  TALKMon = new TALKPrimMon(NA62MainWindow, "TALK");

  // Possible to save only NewCHOD and RICH plots as png

  AllMon->SetSavePngFiles(SavePlots);
  IRCMon->SetSavePngFiles(SavePlots);
  RICHMon->SetSavePngFiles(SavePlots);
  AllMon->SetPngFilesWebDir(PlotsWebDir);
  IRCMon->SetPngFilesWebDir(PlotsWebDir);
  RICHMon->SetPngFilesWebDir(PlotsWebDir);

  TThread * Thrd = new TThread("td", RunMon, (void*) 0);
  Thrd->Run();
  PrimMonApp.Run();

  exit(EXIT_SUCCESS);
}
