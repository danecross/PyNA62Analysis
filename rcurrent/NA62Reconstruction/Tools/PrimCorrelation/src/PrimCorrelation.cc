#include <initializer_list>
#include <vector>
#include <iostream>
#include <cmath>
#include <chrono>
#include <map>
#include <omp.h>
#include <dirent.h>
#include <memory>
#include <signal.h>
#include "TROOT.h"
#include "TFile.h"
#include "TMath.h"
#include "TTree.h"
#include "TBrowser.h"
#include "TGraph.h"
#include "TLegend.h"
#include "TLatex.h"
#include "TAxis.h"
#include "TCanvas.h"
#include "TMultiGraph.h"
#include "TPrimitive.hh"
#include "ComputeCorrelation.hh"


#define START1             80E6   //2 seconds in 25ns
#define END1               90E6   //2.25 seconds in 25 ns
#define START2            120E6   //3 seconds in 25ns
#define END2              130E6   //3.25 seconds in 25 ns
#define START3            160E6   //4 seconds in 25ns
#define END3              170E6   //4.25 seconds in 25 ns
#define STEP             1          //DT increment to find correlation
#define WIDTH            10         //Correlation Window 1 ns
#define DELTA            1000       // Range in witch find correlation 100 ns

//-------------------------------------------------------------------
void usage(char* name){
  cout << endl << " Usage: " << name << " [-options] <input file/path> [detector list]" << endl << "\t\t or " << name << " -h  for a detailed help." << endl << endl;
}

//-------------------------------------------------------------------
void sighandler(int sig){
  (void)sig;
  cout << ":-)"<<flush;
}


//-------------------------------------------------------------------
void help(char* name){
  cout << endl << " " << name << ": Correlation for NA62 trigger-primitive decoded file " << endl <<
    ""        << endl;
  usage(name);
  cout <<
    " Parameters:"        << endl <<
    "     <input file/path> : Root file containing NA62 primitives (TPrimitive) decoded with PrimiRawDecoder." << endl <<
    "                         If a path is given, files corresponding to [detector list] will be looked for in the path. " << endl <<
    "       [detector list] : A list of NA62 detector names (CHOD, RICH, LAV, MUV3, IRC, TALK, LKr) separated by space.  " << endl <<
    "                         In case the list is not given, the whole list will be assumed by default."        << endl <<
    "                         Correlations will be performed on all the possible pairs of detectors in the list."        << endl <<
    "                         The order in which the list is written matters for the sign of the correlation."        << endl <<
    ""        << endl <<
    " Options:"        << endl <<
    "  -h: Prints this detailed help." << endl <<
    "  -o: output path." <<endl<<
    "  -q: Quick, evaluates correlations wrt RICH only." << endl <<    
    "  -a: Add all correlations to the multigraph, without this option only correlation wrt to RICH are added." << endl <<    
    "  -v: Verbose mode is enabled." << endl <<
    "  -m: Start of correlation range, in unit of 0.1 ns (default " << -DELTA << ")." << endl <<
    "  -M: End of correlation range, in unit of 0.1 ns (default " << DELTA << ")." << endl <<
    "  -s: Step of correlation range, in unit of 0.1 ns (default " << STEP << ")." << endl <<
    "  -w: Width of the coincidence window correlation computing, in unit of 0.1 ns (default " << WIDTH << ")." << endl <<
    "  -b: Burst number, in case a path is given." << endl <<
    "  -r: Run number, in case a path is given." << endl <<
    "  -t: Timestamp is given alternatively to run and bust mode, in case a path is given." << endl <<
    " "        << endl;
}


//***********************************************

Int_t main(Int_t argc, char **argv){
  signal(SIGHUP,sighandler);
  Bool_t verbose = kFALSE;

  if (argc == 1) {
    usage(argv[0]);
    return 0;
  }

  Int_t DeltaMin  = -DELTA;
  Int_t DeltaMax  = DELTA;
  Int_t DeltaStep = STEP;
  Int_t Width     = WIDTH;
  TString OutPath = "./";

  Int_t opt;
  Bool_t all = kFALSE;
  Bool_t rich_only = kFALSE;
  Int_t Burst = 0;
  Int_t Run = 0;
  Int_t TimeStamp = 0;
  map <string, string> DetectorMap;
  map <string, int> DetectorColor;
  string detmapindex;


  DetectorMap["CHOD"] = "chod";
  DetectorMap["RICH"] = "rich";
  DetectorMap["LAV"]  = "lav12";
  DetectorMap["LKr"]  = "lkr";
  DetectorMap["MUV3"] = "muv3";
  DetectorMap["IRC"]  = "irc";
  DetectorMap["TALK"] = "talk";

  DetectorColor["CHOD"] = kBlack;
  DetectorColor["RICH"] = kRed;
  DetectorColor["LAV"]  = kGreen+2;
  DetectorColor["LKr"]  = kRed-5;
  DetectorColor["MUV3"] = kMagenta;
  DetectorColor["IRC"]  = kBlue-2;
  DetectorColor["TALK"] = kGreen;


  extern char *optarg;
  extern int optind;
  while ((opt = getopt(argc, argv, "haqvm:M:s:w:b:r:t:o:")) != -1) {

    switch (opt) {
    case 'h':
      help(argv[0]);
      return 0;
      break;
    case 'a':
      all = kTRUE;
      break;
    case 'q':
      rich_only = kTRUE;
      break;
    case 'v':
      verbose = kTRUE;
      break;
    case 'm':
      DeltaMin = TString(optarg).Atoi();
      break;
    case 'M':
      DeltaMax = TString(optarg).Atoi();
      break;
    case 's':
      DeltaStep = TString(optarg).Atoi();
      break;
    case 'w':
      Width = TString(optarg).Atoi();
      break;
    case 'b':
      Burst = TString(optarg).Atoi();
      break;
    case 'r':
      Run = TString(optarg).Atoi();
      break;
    case 't':
      TimeStamp = TString(optarg).Atoi();
      break;
    case 'o':
      OutPath= TString(optarg);
      break;
    }
  }


  printf("[info] Correlation will be performed from: %d (%.2f ns) to %d (%.2f ns) in step of %d (%.2f ns)\n", DeltaMin, DeltaMin*0.1, DeltaMax, DeltaMax*0.1, DeltaStep, DeltaStep*0.1);
  printf("[info] The coincidence width is set to %d (%.2f ns)\n", Width, Width*0.1);

  if (DeltaMin >= DeltaMax) {
      printf("[error] The minimum must be smaller than the maximum!\n");
      return -1;
  }

  if (DeltaStep > DeltaMax - DeltaMin) {
      printf("[error] The step must be smaller than the interval!\n");
      return -1;
  }

  TString path;
  TString name;
  vector <TString> DetectorNames;
  vector <TString> DetectorFileNames;
  vector <TFile*> DetectorFiles;
  TFile *f;

  DIR *dp;
  if ((dp = opendir(argv[optind])) == NULL ) {
    // it's not a directory, may be a file or nothing
    path = "";
    f = new TFile(argv[optind]);
    if(!f){
      cout << "[fatal] File "<< argv[optind] <<" not found." << endl ;
      return -1;
    }
  } else {
    // it's a directory
    f = new TFile("");
    if (Burst == 0 && Run == 0 && TimeStamp == 0) {
      printf("[Fatal] No burst/run number or timestamp was given in path mode.\n");
      return -1;
    }
    path = argv[optind];
  }

  if(TimeStamp ==0) {
    name = Form("reduced_run%05d_burst%05d", Run, Burst);
  } else {
    name = Form("reduced_%010d", TimeStamp);
  }

  if ((argc - optind) == 1) {
    printf("[info] No detector list given, assuming all but TALK by default\n");
    DetectorNames = {"RICH", "CHOD", "LAV", "MUV3", "IRC", "LKr"};
    
    for (UInt_t i=0; i< DetectorNames.size(); i++) {
      if (path.Length() > 0) DetectorFileNames.push_back(Form("%s/%s-%s.root", path.Data(), DetectorMap[DetectorNames[i].Data()].c_str(), name.Data()) );
    }

  } else {
    for (Int_t i = optind + 1; i < argc; i++) {
      DetectorNames.push_back(argv[i]);
      detmapindex = Form("%s", argv[i]);
      if (path.Length() > 0) DetectorFileNames.push_back(Form("%s/%s-%s.root", path.Data(), DetectorMap[detmapindex].c_str(), name.Data()) );
    }
  }

  for (UInt_t i =0; i < DetectorFileNames.size(); i++) {
    cout << "[info] Opening file " <<  DetectorFileNames[i].Data() << "..." << endl;
    DetectorFiles.push_back(  new TFile( DetectorFileNames[i].Data() ) );
    if (DetectorFiles[i]->IsZombie() ) {
      cout << "[Warning] File: " << DetectorFileNames[i].Data() << " not found." << endl;
    }
  }

  Int_t NDet = DetectorNames.size();

  Int_t nthreads= omp_get_max_threads();
  omp_set_num_threads(nthreads);

  vector <TTree*> DetectorTrees(NDet, NULL);
  vector<TPrimitive*> Primitive(NDet, NULL);

  if(path.Length()==0){
    for (Int_t iDet = 0; iDet<NDet; iDet++) {
      f->GetObject(DetectorNames[iDet], DetectorTrees[iDet]);
      if(!DetectorTrees[iDet]){
	cout << "[warning] No tree for detector: '" << DetectorNames[iDet] << "'  was found in file: '" << argv[optind] << "'" << endl ;
      }
    }
  }
  else{
    for (UInt_t i =0; i < DetectorFiles.size(); i++){
      if(!DetectorFiles[i]->IsZombie()){
	DetectorFiles[i]->GetObject(DetectorNames[i], DetectorTrees[i]);
	if(!DetectorTrees[i]){
	  cout << "[warning] No tree for detector: '" << DetectorNames[i].Data() << "'  was found in file: '" <<  DetectorFileNames[i].Data() << "'" << endl ;
	}
      } else {
	cout << "[warning] Cannot find file: '" << DetectorFileNames[i].Data() << "'" << endl ;
      }
    }
  }


  vector < vector<Long64_t>* > time, SendTS;
  vector < vector<Double_t>*    > MTPOffset;
  vector <Long64_t>  ts(NDet);
  vector <Int_t> ft(NDet),sts(NDet), pid(NDet);
  vector <Long64_t> Nentries(NDet, 0);
  vector <Double_t> delta;
  vector <Float_t> Min(NDet, 9999.9);
  vector <Float_t> Max(NDet,-9999.9);
  vector <Float_t> MeanCanvas(NDet,-9999.9);
  vector <Float_t> Sum(NDet, 0.0);
  vector <Int_t>   Entries(NDet, 0);
  string OutFile;

  if (Run > 0 && Burst > 0)  {
    OutFile = Form("Corr-run%05d_burst%05d.root", Run, Burst);
  } else if (TimeStamp > 0) {
    OutFile = Form("Corr-%010d.root", TimeStamp);
  } else {
    OutFile = Form("Corr-%s.root", argv[optind]);
  }
  cout<<"[info] Output file " << OutFile << " will be created." << endl;  


  TFile *g = new TFile(Form("%s/%s",OutPath.Data(),OutFile.c_str() ), "RECREATE");

  cout<<"[info] Getting trees and setting branch addresses..." << endl;

  for(Int_t iDet = 0; iDet<NDet; iDet++) {
    if(!DetectorTrees[iDet]){
      Nentries[iDet] = 0;

    } else {
      Nentries[iDet] = DetectorTrees[iDet]->GetEntries();
      DetectorTrees[iDet]->GetBranch("fPrimitive");
      DetectorTrees[iDet]->SetBranchAddress("fPrimitive",&Primitive[iDet] );
    }
    cout<<"[info] " << DetectorNames[iDet] << " has " << Nentries[iDet] << " entries." << flush << endl;
  }


  //cout<<" done."<<endl <<flush;

  // Loading primitives into vectors

  for(Int_t iDet = 0; iDet<NDet; iDet++) {
    TString start = "";
    cout << "[info] Loading " << DetectorNames[iDet] << "'s primitives into vector ..." << flush;
    time.push_back(new vector <Long64_t>);
    SendTS.push_back(new vector <Long64_t>);

    for (Long64_t iEntry=0; iEntry<Nentries[iDet]; iEntry++) {
      DetectorTrees[iDet]->GetEntry(iEntry);

      if ( (Primitive[iDet]->GetTimeStamp() >= START1 && Primitive[iDet]->GetTimeStamp() <= END1)
	   || (Primitive[iDet]->GetTimeStamp() >= START2 && Primitive[iDet]->GetTimeStamp() <= END2)
	   || (Primitive[iDet]->GetTimeStamp() >= START3 && Primitive[iDet]->GetTimeStamp() <= END3) ) {

	Long64_t time_tot=(Primitive[iDet]->GetTimeStamp()<<8 | Primitive[iDet]->GetFineTime());
	time[iDet]->push_back(time_tot);
	if(verbose) cout<<"[verbose] iDet "<<iDet<<" "<<" timestamp "<<Primitive[iDet]->GetTimeStamp()<<" finetime "<<Primitive[iDet]->GetFineTime() <<" time "<<time[iDet]<<endl;
	//   Primitive[iDet]->Print();
	SendTS[iDet]->push_back(Primitive[iDet]->GetSendTimeStamp());
	Double_t DTsend = ((Double_t)Primitive[iDet]->GetTimeStamp() - (Double_t)Primitive[iDet]->GetSendTimeStamp())/256.;

	// evaluating MTP offset
	Sum[iDet] += DTsend; 
	Entries[iDet]++;
	if (DTsend > Max[iDet] || iEntry == 0) Max[iDet] = DTsend;
	if (DTsend < Min[iDet] || iEntry == 0) Min[iDet] = DTsend;
      }
    }
    Double_t Mean = 0.0;
    if (Entries[iDet]) Mean = Sum[iDet]/(1.0*Entries[iDet]); else Mean = 0.0;
    MeanCanvas[iDet] = Mean;
    cout << " done.\n" << flush;
    printf( "[message] %s's  MTP offset is  Min %4.02f  Mean %4.02f  Max %4.02f\n",  DetectorNames[iDet].Data(), Min[iDet], Mean, Max[iDet]);
  }




  // preparing scan range
  for (Int_t i=DeltaMin; i<DeltaMax; i+=DeltaStep ) {
    delta.push_back(i);
  }

  // scanning
  bool IsRICHEnabled=kFALSE;
  std::chrono::time_point<std::chrono::system_clock> start,end;
  start=std::chrono::system_clock::now();
  TGraph * gCorr[NDet];
  TMultiGraph *gCorrelations = new TMultiGraph();
  TLegend *corrleg = new TLegend(0.555,0.655,0.755,0.895);
  corrleg->SetLineColor(0);
  corrleg->SetTextSize(0.040);
  for(Int_t iDet = 0; iDet<NDet; iDet++) {
    for(Int_t jDet = iDet+1; jDet<NDet; jDet++) {

      if(iDet==jDet) continue;
      if (rich_only && (!DetectorNames[iDet].EqualTo("RICH") && !DetectorNames[jDet].EqualTo("RICH"))) continue;

      cout << "[info] Computing correlation: " << DetectorNames[iDet] << " - " << DetectorNames[jDet] << " " << flush;
      gCorr[iDet] = (TGraph*)ComputeCorrelation(*time[jDet], *time[iDet], delta, Width);
      gCorr[iDet]->SetName(Form("gCorr%s_%s", DetectorNames[iDet].Data(), DetectorNames[jDet].Data()));
      gCorr[iDet]->SetTitle(Form("%s - %s correlation", DetectorNames[iDet].Data(), DetectorNames[jDet].Data()));
      gCorr[iDet]->SetMarkerColor(DetectorColor[DetectorNames[jDet].Data()]);
      gCorr[iDet]->SetLineWidth(1);
      gCorr[iDet]->SetMarkerStyle(7);
      gCorr[iDet]->SetLineColor(DetectorColor[DetectorNames[jDet].Data()]);
      if (all || (DetectorNames[iDet] == "RICH")){ 
	IsRICHEnabled=kTRUE;
	gCorrelations->Add(gCorr[iDet]);
	corrleg->AddEntry(gCorr[iDet],Form("%s  %4.01f %4.01f %4.01f",DetectorNames[jDet].Data(), Min[jDet], MeanCanvas[jDet], Max[jDet]),"l"); 
      }
      gCorr[iDet]->Write();
      cout << " done.\n" << flush;
    }
  }
  TCanvas *cc = new TCanvas("cc","",600,400);
  TString gcorrelationtitle; 
  TLatex *corrtext = new TLatex();
  corrtext->SetTextSize(0.04);


  gCorrelations->SetTitle(Form("Primitive Timing wrt RICH: Run %d Burst %4d",Run,Burst));
  gCorrelations->Draw("apl");
  cc->Update();
  corrleg->Draw();
  for(Int_t iDet = 0; iDet<NDet; iDet++) {
    if (DetectorNames[iDet] == "RICH"){
      IsRICHEnabled=kTRUE;
      corrtext->SetTextFont(42); 
      corrtext->DrawLatex(18., 0.675, Form("%s %4.01f %4.01f %4.01f",DetectorNames[iDet].Data(), Min[iDet], MeanCanvas[iDet], Max[iDet]));
    }
  }
  if (IsRICHEnabled){ 
    gCorrelations->GetXaxis()->SetRangeUser(-40.,52.);
    gCorrelations->GetXaxis()->SetTitle("Time [ns]");
    gCorrelations->GetXaxis()->SetTitleOffset(1.0);
    gCorrelations->GetYaxis()->SetDecimals();
    gCorrelations->GetYaxis()->SetTitle("Correlation [a.u.]");  
    gCorrelations->SetName("gCorrelations");
    cc->Write();
  }

  end =std::chrono::system_clock::now();
  std::chrono::duration<Double_t> elapsed_seconds = end-start;
  
      
  
  
  cout << "[info] All done in "<<elapsed_seconds.count()<< " seconds.\n" << endl;
  
  g->Close();
  return 0;
}
