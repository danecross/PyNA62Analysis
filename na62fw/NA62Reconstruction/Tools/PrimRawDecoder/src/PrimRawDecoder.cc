#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <TTree.h>
#include <TBranch.h>
#include <TFile.h>
#include <TString.h>
#include <TObjString.h>
#include <TObjArray.h>
#include "TPrimitive.hh"
#include "NA62Global.hh"
#include <iostream>
#include <fstream>
#include <string>
#include <TH1.h>
#include <TH2.h>
#include <TGraph.h>
#include <chrono>
#include <signal.h>

//64 bit version:

#define SOURCE_ID  0xFF000000 //first  word
#define SEND_TS    0x00FFFFFF //first  word

#define SUB_ID     0xFF000000 //second word
#define NPRIM      0x00FF0000 //second word
#define MTP_LENGTH 0x0000FFFF //second word

#define PRIM_ID    0xFFFF0000 //third word
#define FTIME      0x000000FF //third word

#define TSTAMP     0xFFFFFFFF //fourth word



//32 bit version
//Header:
#define MTP_ASSEMBLY    0x00FFFFFF
//Timestamp word
#define TSTAMP_HIGH     0x00FFFFFF
//Primitive Data
#define TSTAMP_LOW      0x0000FF00

#define MAX_FILES  100
#define MAXBUFFERWORDS 400000
#define NDETECTORS 8  //

using namespace std;

const static TString DetectorName[] = {"LAV", "CHOD", "RICH", "IRC", "LKr", "MUV3", "NewCHOD", "TALK", "MERGED"};
const static char DetectorInitial[] = {'L', 'C', 'R', 'I', 'K', 'M', 'N', 'T', 'E'};
const static UInt_t SubDetectorID[] = {0x10, 0x18, 0x1c, 0x20, 0x24, 0x30, 0x38, 0x60, 0x0};
const static Int_t SDNum[] = {-1, -1, -1, -1, 0, -1, 1, 2, 3, 4, -1, -1, 5, -1, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, 7};


Bool_t verbose = kFALSE;
Bool_t veryverbose = kFALSE;
Bool_t OldSendTSMode = kFALSE;


//Signal Handling
void sighandler(int sig){
  (void)sig;
  cout << ":-)"<<flush;
}


//Algorithm to reconstruct the RAM address on the L0TP
long long L0address(unsigned int timestamp, unsigned int finetime, int bitfinetime){

  unsigned int ftMSBmask;
  unsigned int timestampLSBmask;
  long long address;
  unsigned int timestampLSB;
  unsigned int ftMSB;

  if(bitfinetime==3) timestampLSBmask = 2047; //11111111111 (binary)
  if(bitfinetime==2) timestampLSBmask = 4095;
  if(bitfinetime==1) timestampLSBmask = 8191;
  if(bitfinetime==0) timestampLSBmask = 16383;

  if(bitfinetime==3) ftMSBmask=224;////11100000 (binary)
  if(bitfinetime==2) ftMSBmask=192;////11000000 (binary)
  if(bitfinetime==1) ftMSBmask=128;////10000000 (binary)
  if(bitfinetime==0) ftMSBmask=0;  ////00000000 (binary)

  timestampLSB = timestampLSBmask & timestamp;
  ftMSB = ftMSBmask & finetime;
  ftMSB = ftMSB >> ((unsigned int)8-(unsigned int)bitfinetime);

  address = timestampLSB;
  address = address <<(unsigned int)bitfinetime;
  address = address |  ftMSB;

  return address;

}



//-------------------------------------------------------------------
void usage(char* name){
  cout << "Usage: " << name << " [-h] [-i <input file>] [-l <input-file list>]  [-r <run number> -b <burst number>]  [-L <run-and-burst list>] [-c <config file>] [-o <output path>] [-f <format>] [-s] [-m] [-v] [-V] "  << std::endl; //
}

//-------------------------------------------------------------------
void help(char* name){
  cout << endl << " " << name << ": Decoder for NA62 trigger-primitive raw files. " << endl <<
    ""        << endl;
  usage(name);

  cout << endl <<
    " Options:"        << endl <<
    "  -h: Prints this detailed help." << endl <<
    "  -i: Decode single raw file <input file>, in this mode all the parameters in the config file are ignored." << endl <<
    "  -l: Decode all files specified in <input-file list>, in this mode all the parameters in the config file are ignored." << endl <<
    "  -r -u: Decode the file corresponding to run <run number> and burst <burst number> looking in the paths specified in the config file (given with option -c, or the default one)." << endl <<
    "         The raw files of all the detectors specified in the config file will be searched for." << endl <<
    "  -L: Decode all files corresponding to run and burst numbers (separated by a space) specified in the <run-and-burst list> file. See -r -u for details." << endl <<
    "  -c: Specify the file <config file> from which the configuration parameters will be read." << endl <<
    "  -o: Scpecify the path in which the output root file(s) will be created." << endl <<
    "  -m: Additional monitor plots are created inside the root output file." << endl <<
    "  -M: Only additional monitor plots are created in the root output file." << endl <<
    "  -s: The decoding of the MTP timestamp is performed in assuming LSB 6.4 microsecond." << endl <<
    "  -S: The decoding of the MTP timestamp is performed in assuming LSB 25 nanosecond." << endl <<
    "  -g: Granularity of L0TP RAM expressed in number of bits of fine time. Used for monitoring plots, default = 1" << endl <<
    "  -v: Verbose mode is enabled." << endl <<
    "  -V: Very Verbose mode is enabled (will print info of all primitives)." << endl <<
    " "        << endl <<
    " Info:"        << endl <<
    "  When decoding, if everything is allright a series of dots will be printed, every 100000 data words decoded." << endl <<
    "  When a new detector is found in data, the capital initial letter of that detector is printed (with the exeption of K for LKr)." << endl <<
    "  If some error is found, one or more characters will printed instead:" << endl <<
    "   r: repeated primitive " << endl <<
    "   u: unsorted primitive " << endl <<
    "   j: missing MTPs (temporarily disabled)" << endl <<
    "   m: unsorted MTP (only with MTP timestamp at 6.4 us)" << endl <<
    "   d: duplicated detector in file (very serious) " << endl <<
    "" << endl <<
    "   If one of these errors occurs, it is recomended to run the decoder again with the -v option to investigate the problem." << endl <<
    ""  << endl;

}

//-------------------------------------------------------------------
TString CheckProtocols(TString OldStr){
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
      TString fSvcClass = "na62";
      NewStr = NewStr+"?svcClass="+fSvcClass;
    }
  }
  return NewStr;
}


Int_t DecodePrimitives(TString InputFileName, TString OutputFileName, TString PrimitivesPath, vector<Bool_t>& det,  vector<TString>& PrimitivesSubPath, vector<TString>& PrimitivesFilePrefix, Bool_t castor, Int_t monitor,Int_t Format=32,Int_t Granularity=2){
  Bool_t  first = kFALSE;
  Int_t   Ngood[NDETECTORS];
  Int_t   Nbad[NDETECTORS];
  Int_t   Nrepeat[NDETECTORS];
  Int_t   Nunsort[NDETECTORS];
  UInt_t  LostMTP[NDETECTORS];
  Bool_t  Summary[NDETECTORS];
  for(int i=0; i<NDETECTORS; ++i){
    Ngood[i]   = 0 ;
    Nbad[i]    = 0 ;
    Nrepeat[i] = 0 ;
    Nunsort[i] = 0 ;
    LostMTP[i] = 0;
    Summary[i] = kFALSE;
  }
  TString Dot = "";
  vector <TH2F*>  h2Delta(NDETECTORS, NULL), h2MTPOffset(NDETECTORS, NULL),  h2DeltaSendTS(NDETECTORS, NULL), h2MTPvsSendTS(NDETECTORS, NULL), h2BitTransition(NDETECTORS, NULL);
  vector <TH1D*>  h1BitPopulation(NDETECTORS, NULL), h1TimeStamp(NDETECTORS, NULL), h1FineTime(NDETECTORS, NULL), h1Delta(NDETECTORS, NULL);

  if (monitor) {
    if (monitor < 2) {
      cout << "[info] Monitor mode enabled\n";
    } else {
      cout << "[info] Monitor ONLY mode enabled\n";
    }
    for (Int_t i=0; i<NDETECTORS; i++) {
      h1TimeStamp[i]     = new TH1D (Form("%s_h1TimeStamp",DetectorName[i].Data()),Form("Timestamp of %s",DetectorName[i].Data()),256*4, -0.5*ClockPeriod, 255.5*ClockPeriod);
      h1FineTime[i]      = new TH1D (Form("%s_h1FineTime",DetectorName[i].Data()), Form("Finetime of %s", DetectorName[i].Data()), 256, -0.5, 255.5);
      h1Delta[i]         = new TH1D (Form("%s_h1Delta", DetectorName[i].Data()), Form("Difference between consecutive primitives of %s", DetectorName[i].Data()),
          601, (-502-0.5)*TdcCalib, (2502+0.5)*TdcCalib);
      // 601, (-575-0.5)*TdcCalib, (30075+0.5)*TdcCalib); // long range.
      h2Delta[i]         = new TH2F (Form("%s_h2Delta", DetectorName[i].Data()), Form("Difference between consecutive primitives for %s vs timestamp", DetectorName[i].Data()),
          256*4, -0.5*ClockPeriod, 255.5*ClockPeriod, 321, (-322-0.5)*TdcCalib, (1282+0.5)*TdcCalib);
      h2DeltaSendTS[i]   = new TH2F (Form("%s_h2DeltaSendTS", DetectorName[i].Data()), Form("Difference between consecutive MTP timestamps for %s vs timestamp", DetectorName[i].Data()),
          256*4, -0.5*ClockPeriod, 255.5*ClockPeriod, 25, -5*(256*ClockPeriod)/1000.0, 20*(256*ClockPeriod)/1000.0);
      h2MTPOffset[i]     = new TH2F (Form("%s_h2MTPOffset", DetectorName[i].Data()), Form("MTP Offset for %s vs timestamp", DetectorName[i].Data()),
          256*4, -0.5*ClockPeriod, 255.5*ClockPeriod, 100, -5*(256*ClockPeriod)/1000.0, 20*(256*ClockPeriod)/1000.0);
      h2MTPvsSendTS[i]   = new TH2F (Form("%s_h2MTPvsSendTS", DetectorName[i].Data()), Form("MTP number vs MTP timestamp for %s", DetectorName[i].Data()), 256, -0.5*ClockPeriod, 255.5*ClockPeriod, 100, 0, 1E6);
      h1BitPopulation[i] = new TH1D (Form("%s_h1BitPopulation",DetectorName[i].Data()),Form("Bit population for %s",DetectorName[i].Data()),16,0,15);
      h2BitTransition[i] = new TH2F (Form("%s_h2BitTransition",DetectorName[i].Data()),Form("Bit transition for %s",DetectorName[i].Data()),17,0,17,2,0,2);

      h1FineTime[i]     ->GetXaxis()->SetTitle("Finetime");
      h1TimeStamp[i]    ->GetXaxis()->SetTitle("Timestamp [ms]");
      h2Delta[i]        ->GetXaxis()->SetTitle("Timestamp [ms]");
      h1Delta[i]        ->GetXaxis()->SetTitle("Time [ns]");
      h2DeltaSendTS[i]  ->GetXaxis()->SetTitle("Timestamp [ms]");
      h2MTPOffset[i]    ->GetXaxis()->SetTitle("Timestamp [ms]");
      h2MTPvsSendTS[i]  ->GetXaxis()->SetTitle("Timestamp [ms]");
      h2BitTransition[i]->GetXaxis()->SetTitle("From bit");
      h1BitPopulation[i]->GetXaxis()->SetTitle("Bit");

      h2Delta[i]        ->GetYaxis()->SetTitle("Time [ns]");
      h2DeltaSendTS[i]  ->GetYaxis()->SetTitle("Time [us]");
      h2MTPOffset[i]    ->GetYaxis()->SetTitle("Time [us]");
      h2MTPvsSendTS[i]  ->GetYaxis()->SetTitle("MTP number");
      h2BitTransition[i]->GetYaxis()->SetTitle("");
      h1BitPopulation[i]->GetYaxis()->SetTitle("Entries");
      h1FineTime[i]     ->GetYaxis()->SetTitle("Entries");
      h1TimeStamp[i]    ->GetYaxis()->SetTitle("Entries");

      h2Delta[i]        ->SetOption("colz");
      h2DeltaSendTS[i]  ->SetOption("colz");
      h2MTPOffset[i]    ->SetOption("colz");
      h2MTPvsSendTS[i]  ->SetOption("colz");
      h2BitTransition[i]->SetOption("colz");

      h2BitTransition[i]->GetYaxis()->SetBinLabel(1,"lost");
      h2BitTransition[i]->GetYaxis()->SetBinLabel(2,"gained");
      for (int bin=1; bin<17; bin++) {
        h2BitTransition[i]->GetXaxis()->SetBinLabel(bin, Form("Bit %02d",bin-1));
      }
      h2BitTransition[i]->GetXaxis()->SetBinLabel(17, "Total");
    }

    cout << "[info] Monitor histograms initialised\n";
  }

  cout << "[info] Decode primtives called:  InputFileName: " << InputFileName << endl << "[info] OutputFileName: " <<  OutputFileName << endl <<  "[info] PrimitivesPath: " <<  PrimitivesPath << endl;
  if (!castor) cout << "[info] Single file mode" << endl; else  cout << "[info] Automatic file search mode" << endl;
  if (verbose)  for (Int_t i =0 ; i< NDETECTORS+1 ; i++) cout << "[info] DET: " << DetectorName[i] << " INC: " << det[i] << " SUBPATH: " << PrimitivesSubPath[i] << " PREFIX: " << PrimitivesFilePrefix[i] << endl;

  // Parsing filename to get run and burst numbers
  Int_t pos = InputFileName.Index("run");
  TString number = InputFileName(pos+3, 5);
  UInt_t RunID = atoi(number);
  if (RunID == 0 || pos == -1) {
    cout << "[warning] Could not parse run number from file name, using 0.\n";
    RunID = 0;
  }

  pos = InputFileName.Index("burst");
  number = InputFileName(pos+5, 5);
  UInt_t BurstID = atoi(number);

  if (BurstID == 0 || pos == -1) {
    cout << "[warning] Could not parse burst number from file name, using 0.\n";
    BurstID = 0;
  }

  printf("[info] Run: %d, Burst: %d parsed from file name\n", RunID, BurstID);

  vector<Int_t> primfile;
  vector<TString> primfile_name;

  if (!castor) {
    cout << "[info] Opening single file: " << InputFileName << "\n";
    primfile.push_back(open(CheckProtocols(InputFileName).Data(),O_RDONLY|O_NONBLOCK));
    primfile_name.push_back(InputFileName);
  } else if(det[NDETECTORS]) { //if the last detecors is included, it means that the merged (2015) file is being used
    TString FileName = PrimitivesPath + "/" + PrimitivesSubPath[NDETECTORS]  + "/" + PrimitivesFilePrefix[NDETECTORS] +InputFileName;
    cout << "[info] Opening single merged file: " << FileName << "\n";
    primfile.push_back(open(CheckProtocols(FileName).Data(),O_RDONLY|O_NONBLOCK));
    primfile_name.push_back(InputFileName);
  }  else {
    for (Int_t i=0; i < NDETECTORS; i++ ) {
      if (det[i]){
        TString FileName = PrimitivesPath + "/" + PrimitivesSubPath[i]   + "/" +  PrimitivesFilePrefix[i] + InputFileName;
        cout << "[info] Opening file: " << FileName << " for detector " << DetectorName[i] << "\n";
        primfile.push_back(open(CheckProtocols(FileName).Data(),O_RDONLY|O_NONBLOCK));
        primfile_name.push_back(FileName);
      }
    }
  }

  if (!primfile.size()) {
    cout << "[error] No file to open\n";
    return -2;
  }

  // create the ttree structure
  TTree* tPrim[NDETECTORS];
  TPrimitive* Primitive = new TPrimitive();

  for (Int_t i = 0; i < NDETECTORS; i++) {
    tPrim[i] = new TTree(DetectorName[i], DetectorName[i] + "Primitives");
    tPrim[i]->Branch("fPrimitive", "TPrimitive", &Primitive);
  }

  Int_t DetectorID = -1;


  for (UInt_t it = 0; it < primfile.size(); it++) {

    TString FileName = primfile_name.at(0);
    primfile_name.erase(primfile_name.begin());
    if (primfile[it] == -1) {
      printf("[warning] File %s was not found\n", FileName.Data());
      continue;

    } else {

      printf("[info] Reading from file: %s ", FileName.Data());
    }

    // UInt_t FirstSendTS = 0;
    UInt_t TimeStampHigh=0,TimeStampLow=0;
    UInt_t TimeStamp = 0, AssemblyTS = 0, OldAssemblyTS = 0, OldTimeStamp = 0;
    UInt_t AssemblyTimeStamp = 0, OldAssemblyTimeStamp = 0,  TSCounter = 0, MTP = 0;
    UInt_t SourceID = 0, SubID = 0, PID = 0, FineTime = 0, OldFineTime = 0, NPrimitive = 0, OldSourceID = 0, OldPID = 0, MaybePID=0;
    Int_t  Nword = 0, MTPWord = 1, PrimToDo = 0, state = 1;
    UInt_t buffer[MAXBUFFERWORDS];
    Long_t bufSizeIn = MAXBUFFERWORDS*4;
    Long_t bufSizeOut = 0;
    UInt_t nw = bufSizeOut/4;
    UInt_t loss = 0;
    Bool_t isFirstPrimitive=1;
    long long PrimitiveAddress=0, OldPrimitiveAddress=0;

    while(state != 0){

      bufSizeOut = read(primfile[it], buffer, bufSizeIn);
      nw = bufSizeOut/4;
      if (nw < MAXBUFFERWORDS) state = 0;

      for (UInt_t iw=0; iw<nw; iw++){

        Nword++;

        switch(MTPWord){

          case 1:
            if(Format==32){
              SourceID=(buffer[iw] & SOURCE_ID)>>24;
              AssemblyTS=(buffer[iw] & MTP_ASSEMBLY);

              DetectorID = SDNum[SourceID/4];

              if (OldSourceID != SourceID && OldSourceID!=0) {
                if (DetectorID == -1) {
                  cout << "[fatal] Unknown detector\n";

                }
                if (verbose) {
                  cout << "[verbose] Found new detector: " << DetectorName[DetectorID] << endl;
                }else {
                  cout << DetectorInitial[DetectorID] << flush;
                }

                AssemblyTimeStamp = 0;
                OldAssemblyTimeStamp = 0;
                OldSourceID = SourceID;
                OldTimeStamp = 0;
                OldFineTime  = 0;
                TSCounter = 0;
                OldAssemblyTS = 0;
                MTP = 0;
                first = kTRUE;
              } else {
                first = kFALSE;
              }

              if (AssemblyTS < OldAssemblyTS) {
                if (OldSendTSMode) {
                  TSCounter ++;
                } else {
                  if (verbose) {
                    printf("[warning] unsorted MTP timestamps, previous: 0x%08x, current: 0x%08x\n", AssemblyTS, OldAssemblyTS);
                  } else {
                    if (!Dot.Contains("m")) Dot = Dot + "m";
                  }
                }
              }

              OldAssemblyTimeStamp = AssemblyTimeStamp;

              if (OldSendTSMode) {
                AssemblyTimeStamp = AssemblyTS + TSCounter * 0x1000000;
              } else {
                AssemblyTimeStamp = AssemblyTS * 0x100;
              }

              if (OldAssemblyTimeStamp == 0) {
                loss = 0;
              } else {
                loss = AssemblyTimeStamp/0x100 - OldAssemblyTimeStamp/0x100 - 1;
              }

              LostMTP[DetectorID] += loss;
              if (loss !=0) {
                if (verbose) {
                  printf("[warning] Missing MTP: %u (TOT %u)\n", loss, LostMTP[DetectorID]);
                } else {
                  if (!Dot.Contains("j")) Dot = Dot + "j";
                }
              }


              MTPWord = 2;
              break;
            }
            else if(Format==64){
              SourceID=(buffer[iw]&SOURCE_ID)>>24;
              OldAssemblyTS=AssemblyTS;
              AssemblyTS=(buffer[iw]&SEND_TS);
              MTP++;
              DetectorID = SDNum[SourceID/4];
              if (OldSourceID != SourceID && OldSourceID!=0) {
                if (DetectorID == -1) {
                  cout << "[fatal] Unknown detector\n";
                  //return(-1); // do this better...
                }
                if (verbose) {
                  cout << "[verbose] Found new detector: " << DetectorName[DetectorID] << endl;
                }else {
                  cout << DetectorInitial[DetectorID] << flush;
                }

                AssemblyTimeStamp = 0;
                OldAssemblyTimeStamp = 0;
                OldSourceID = SourceID;
                OldTimeStamp = 0;
                OldFineTime  = 0;
                TSCounter = 0;
                OldAssemblyTS = 0;
                MTP = 0;
                first = kTRUE;
              } else {
                first = kFALSE;
              }

              if (AssemblyTS < OldAssemblyTS) {
                if (OldSendTSMode) {
                  TSCounter ++;
                } else {
                  if (verbose) {
                    printf("[warning] unsorted MTP timestamps, previous: 0x%08x, current: 0x%08x\n", AssemblyTS, OldAssemblyTS);
                  } else {
                    if (!Dot.Contains("m")) Dot = Dot + "m";
                  }
                }
              }

              OldAssemblyTimeStamp = AssemblyTimeStamp;

              if (OldSendTSMode) {
                AssemblyTimeStamp = AssemblyTS + TSCounter * 0x1000000;
              } else {
                AssemblyTimeStamp = AssemblyTS * 0x100;
              }

              if (OldAssemblyTimeStamp == 0) {
                loss = 0;
              } else {
                loss = AssemblyTimeStamp/0x100 - OldAssemblyTimeStamp/0x100 - 1;
              }

              LostMTP[DetectorID] += loss;
              if (loss !=0) {
                if (verbose) {
                  printf("[warning] Missing MTP: %u (TOT %u)\n", loss, LostMTP[DetectorID]);
                } else {
                  if (!Dot.Contains("j")) Dot = Dot + "j";
                }
              }


              MTPWord = 2;
              break;
            }
            else{
              cout<<"[fatal]: Unknown format: "<<Format<<endl;
              return -3;
            }



          case 2:
            if(Format==32){
              MTP++;
              NPrimitive=(buffer[iw]&NPRIM)>>16;
              SubID=(buffer[iw]&SUB_ID)>>24;
              PrimToDo = NPrimitive;
              isFirstPrimitive=1;
              MTPWord=3;
              break;
            }

            else if(Format==64){
              NPrimitive=(buffer[iw]&NPRIM)>>16;
              SubID=(buffer[iw]&SUB_ID)>>24;
              PrimToDo = NPrimitive;
              MTPWord = 3;
              break;
            }
            else{
              cout<<"[fatal] Unknown format: "<<Format<<endl;
              return -3;
            }

          case 3:
            if(Format==32){
              if (PrimToDo!=0){
                MaybePID=(buffer[iw]&PRIM_ID)>>16;
                if(isFirstPrimitive==1) {
                  if((MaybePID & 0x4000)!=0){
                    cout<<"[fatal] Primitives with wrong format!!"<<endl;
                    return -1;
                  }
                  else{
                    isFirstPrimitive=0;
                  }
                }

                if ((MaybePID & 0xFF00) ==0) {
                  TimeStampHigh=(buffer[iw]&TSTAMP_HIGH);
                  MTPWord = 3;
                  break;
                } else {
                  //That's a real PID
                  OldPID=PID;
                  PID=MaybePID;
                  PrimToDo--;
                  TimeStampLow=(buffer[iw] & TSTAMP_LOW)>>8;
                  OldFineTime=FineTime;
                  FineTime=buffer[iw]&FTIME;
                  OldTimeStamp = TimeStamp;
                  TimeStamp = ((TimeStampHigh<<8) | TimeStampLow);
                  //ATT: Without break to go forward.
                }
              } else {
                MTPWord = 4;
                break;
              }
            }
            else if(Format==64){
              if (PrimToDo!=0) PrimToDo--;
              OldPID = PID;
              PID=(buffer[iw]&PRIM_ID)>>16;
              OldFineTime=FineTime;
              FineTime=buffer[iw]&FTIME;
              MTPWord = 4;
              break;
            }
            else{
              cout<<"[fatal] Unknown format: "<<Format<<endl;
              return -3;
            }
            [[fallthrough]];

          case 4:
            if(Format==32){
              if (PrimToDo == 0) {
                MTPWord = 1;
              } else {
                MTPWord = 3;
              }
            } else if(Format==64){
              OldTimeStamp = TimeStamp;
              TimeStamp = buffer[iw]&TSTAMP;

              if (PrimToDo == 0) {
                MTPWord = 1;
              } else {
                MTPWord = 3;
              }
            }

            if (first) {
              if ( Ngood[DetectorID] > 0) {
                if (verbose) {
                  printf("[warning] duplicated detector %s in file\n", DetectorName[DetectorID].Data());
                } else {
                  if (!Dot.Contains("d")) Dot = Dot + "d";
                }
              }
            }


            if (monitor) {
              OldPrimitiveAddress = PrimitiveAddress;
              PrimitiveAddress = L0address(TimeStamp,FineTime,Granularity);
              h2DeltaSendTS[DetectorID]->Fill(TimeStamp*ClockPeriod/1E6, (AssemblyTimeStamp*ClockPeriod - OldAssemblyTimeStamp*ClockPeriod)/1000.0);
            }

            if (NPrimitive==0) break; //

            if (monitor < 2) {
              Primitive->SetTimeStamp(TimeStamp);
              Primitive->SetFineTime(FineTime);
              Primitive->SetPrimitiveID(PID);
              Primitive->SetSourceID(SourceID);
              Primitive->SetSubID(SubID);
              Primitive->SetMTP(MTP);
              Primitive->SetSendTimeStamp(AssemblyTimeStamp);
              Primitive->SetRunID(RunID);
              Primitive->SetBurstID(BurstID);
            }

            if (DetectorID != -1) {
              if (monitor < 2) {
                tPrim[DetectorID]->Fill();
              }
              Ngood[DetectorID]++;
              Long_t delta_long = (Long_t)TimeStamp - (Long_t)OldTimeStamp;
              Double_t delta = (delta_long*ClockPeriod + (FineTime*TdcCalib - OldFineTime*TdcCalib));
              if (veryverbose){
                printf("[primitive] Current: 0x%08x %02x %04x. Delta: %f \n", TimeStamp, FineTime, PID, delta);
              }
              if (delta <= -ClockPeriod ){
                Nbad[DetectorID]++;
                Nunsort[DetectorID]++;
                if (verbose) {
                  printf("[warning] unsorted primitives: Previous: 0x%08x %02x, Current: 0x%08x %02x, delta %f ns.\n", OldTimeStamp, OldFineTime, TimeStamp, FineTime, delta);
                } else {
                  if (!Dot.Contains("u")) Dot = Dot + "u";
                }
              }

              if (delta == 0){
                Nbad[DetectorID]++;
                Nrepeat[DetectorID]++;
                if (verbose) {
                  printf("[warning] repeated primitives: Current: 0x%08x %02x %04x. Old: 0x%08x %02x %04x.\n", TimeStamp, FineTime, PID, OldTimeStamp, OldFineTime, OldPID);
                } else {
                  if (!Dot.Contains("r")) Dot = Dot + "r";
                }
              }
              if (monitor) {
                Double_t TimeStamp_in_sec = TimeStamp*ClockPeriod/1E6;
                h1TimeStamp[DetectorID] -> Fill(TimeStamp_in_sec);
                h1FineTime[DetectorID]  -> Fill(FineTime);
                h2Delta[DetectorID]->Fill(TimeStamp_in_sec, delta);
                h1Delta[DetectorID]->Fill(delta);
                h2MTPOffset[DetectorID]->Fill(TimeStamp_in_sec, (AssemblyTimeStamp*ClockPeriod - TimeStamp*ClockPeriod)/1000.0); // difference in us
                h2MTPvsSendTS[DetectorID]->Fill(AssemblyTimeStamp*ClockPeriod/1E6, MTP);

                for(int bit=0;bit<16;bit++){
                  if(PID & (1<<bit)) h1BitPopulation[DetectorID] -> Fill(bit);
                }

                //Check overwriting process
                if(OldPrimitiveAddress==PrimitiveAddress  && TimeStamp>=OldTimeStamp){ //cross check this condition
                  h2BitTransition[DetectorID] -> Fill(16,0);
                  for(int bit=0;bit<16;bit++){
                    if((OldPID & (1<<bit))!=0 && (PID & (1<<bit))==0) h2BitTransition[DetectorID] -> Fill(bit,0);
                    if((OldPID & (1<<bit))==0 && (PID & (1<<bit))!=0) {
                      h2BitTransition[DetectorID] -> Fill(bit,1);
                    }
                  }
                }
              }

              break;
            } else {
              printf("[warning] Unknown sub-detector ID found: 0x%x, skipping.\n", SourceID);
            }
        } //switch
      }
      if(Nword%100000==0) {
        if (verbose ) {
          printf("[verbose] Detector: %s \n", DetectorName[DetectorID].Data());
          printf("[verbose] TStamp: 0x%x   NPrimitives: %u \n",TimeStamp, NPrimitive);
          printf("[verbose] Primitives left to do: %d \n", PrimToDo);
          printf("[verbose] Source ID: 0x%x   Sub ID: 0x%x \n", SourceID, SubID);
          printf("[verbose] Primitive ID: 0x%x   Fine time: 0x%x \n", PID, FineTime);
        }  else {
          if (Dot.Length() == 0) Dot = ".";
          cout << Dot <<flush;
          Dot = "";
        }
      }
    } //while

    close(primfile[it]);

    if (verbose) {
      printf("[verbose] file closed\n");
    } else {
      cout <<" done.\n"<<flush;
    }

    printf("[info] Summary for %s \n", FileName.Data());
    for (Int_t i =0; i < NDETECTORS; i++) {
      if (Ngood[i] > 0 && !Summary[i]) {
        printf("[info] %s: %u primitives were decoded (of which %u have some problem)\n", DetectorName[i].Data(), Ngood[i], Nbad[i]);
        printf("[info] %s: %u MTPs were lost, %u primitives are unsorted, %u primitives are repeated\n", DetectorName[i].Data(), LostMTP[i], Nunsort[i], Nrepeat[i]);
        Summary[i] = kTRUE;
      }
    }
  }// end prim file

  cout <<"[info] Creating output file: " << OutputFileName << endl;
  TFile* OutputFile = TFile::Open(OutputFileName.Data(),"RECREATE");

  if (monitor) OutputFile->mkdir("PrimitiveMonitor");

  for (Int_t i = 0; i < NDETECTORS; i++) {
    if (Ngood[i]>0 && monitor < 2) {
      tPrim[i]->Write();
    }
    if (monitor && Ngood[i]>0) {
      OutputFile->cd("PrimitiveMonitor");
      h2Delta[i]->Write();
      h1Delta[i]->Write();
      h2DeltaSendTS[i]-> Write();
      h2MTPOffset[i]-> Write();
      h2MTPvsSendTS[i]-> Write();
      h1BitPopulation[i]-> Write();
      h2BitTransition[i] -> Write();
      h1TimeStamp[i] -> Write();
      h1FineTime[i] -> Write();
      OutputFile->cd("");
    }

    delete h2Delta[i];
    delete h1Delta[i];
    delete h2DeltaSendTS[i];
    delete h2MTPOffset[i];
    delete h2MTPvsSendTS[i];
    delete h1BitPopulation[i];
    delete h2BitTransition[i];
    delete h1TimeStamp[i];
    delete h1FineTime[i];
  }
  OutputFile->Close();
  return 0;
}


//------------------------------------------------------------------
int main(int argc, char* argv[]) {
  signal(SIGHUP,sighandler);
  TString OutputFileName("PrimOutputFile.root");
  TString OutputPath("./");
  TString InputFileName("PrimInputFile.root");
  TString InputListFileName("PrimInputListFile.txt");
  TString InputListRunBurstFileName("PrimInputListFile.txt");
  TString ConfFileName("config/Primitives.conf");
  TString PrimitivesPath("/castor/cern.ch/na62/data/2016/raw/run/primitives/");
  static const TString sub_path[] =  {"/", "/", "/", "/", "/", "/", "/", "/", "/"};
  static const TString file_prefix[] =  {"lav12_", "chod_", "rich_", "irc_", "lkr_", "muv3_", "newchod_", "talk_", "merged_"};
  vector<TString> PrimitivesFilePrefix(file_prefix, file_prefix + sizeof(file_prefix)/sizeof(file_prefix[0]));
  vector<TString> PrimitivesSubPath(sub_path, sub_path + sizeof(sub_path)/sizeof(sub_path[0]));
  Int_t is32bit=0;
  std::chrono::time_point<std::chrono::system_clock> start,end;
  start=std::chrono::system_clock::now();

  Int_t monitor = 0;
  Int_t Granularity = 2;

  Int_t options = 0, NBurst = 0, NRun = 0;
  int opt;
  extern char *optarg;
  while ((opt = getopt(argc, argv, "b:c:hi:l:L:p:r:o:g:sSmMvV")) != -1) {
    options++;
    switch (opt) {
      case 'b':
        NBurst = TString(optarg).Atoi();
        if (NBurst<0) {
          usage(argv[0]);
          return 0;
        }
        break;
      case 'c':
        ConfFileName = TString(optarg);
        break;
      case 'h':
        help(argv[0]);
        return 0;
      case 'i':
        InputFileName = TString(optarg);
        break;
      case 'l':
        InputListFileName = TString(optarg);
        break;
      case 'L':
        InputListRunBurstFileName = TString(optarg);
        break;
      case 'p':
        PrimitivesPath = TString(optarg);
        break;
      case 'r':
        NRun = TString(optarg).Atoi();
        if (NRun<=0) {
          usage(argv[0]);
          return 0;
        }
        break;
      case'o':
        OutputPath = TString(optarg);
        break;
      case'g':
        Granularity = TString(optarg).Atoi();
        break;
      case 'm':
        monitor = 1;
        break;
      case 'M':
        monitor = 2;
        break;
      case 'v':
        verbose = kTRUE;
        break;
      case 'V':
        veryverbose = kTRUE;
        break;
      case 's':
        OldSendTSMode = kFALSE;
        break;
      case 'S':
        OldSendTSMode = kTRUE;
        break;
      default:
        usage(argv[0]);
        return 0;
    }
  }

  if (!options) {
    usage(argv[0]);
    return 0;
  }

  // Decode config file
  cout << "[info] Opening config file: " << ConfFileName.Data() << endl;
  ifstream confFile( ConfFileName.Data() );
  if (confFile.fail()) cout << "[warning] Unable to open configuration file: " << ConfFileName.Data() << endl;

  TString Line;
  vector<Bool_t> detlist(NDETECTORS+1);
  Bool_t AllDisabled = kTRUE;

  while(Line.ReadLine(confFile)) {

    if (Line.BeginsWith("#")) continue;
    if(Line.BeginsWith("DecodePrimitives=")) {
      for (Int_t i = 0; i < (NDETECTORS+1); i++) {
        if (Line.Contains(DetectorName[i])) {
          AllDisabled = kFALSE;
          detlist[i] = kTRUE;
        }
      }
    }

    if(Line.BeginsWith("PrimitivesPath=")) {
      TObjArray *l = Line.Tokenize("=");
      if(l->GetEntries()>1) PrimitivesPath = ((TObjString*)(l->At(1)))->GetString();
      delete l;
    }

    for (Int_t i=0; i<(NDETECTORS+1); i++) {
      if(Line.BeginsWith(DetectorName[i]+"PrimitivesSubPath=")) {
        TObjArray *l = Line.Tokenize("=");
        if(l->GetEntries()>1) PrimitivesSubPath[i] = ((TObjString*)(l->At(1)))->GetString();
        delete l;
      }

      if(Line.BeginsWith(DetectorName[i]+"PrimitivesFilePrefix=")) {
        TObjArray *l = Line.Tokenize("=");
        if(l->GetEntries()>1) PrimitivesFilePrefix[i] = ((TObjString*)(l->At(1)))->GetString();
        delete l;
      }

    }

  }

  // -L option: list of Run and Burst numbers is used
  struct stat filestat;
  TObjArray InputFileNameList;
  TObjArray OutputFileNameList;
  TObjArray* InputLine;
  Int_t lineparse;

  vector<Int_t> run;
  vector<Int_t> burst;
  if(stat(Form(InputListRunBurstFileName.Data()), &filestat) == 0) {
    ifstream RunBurstList(InputListRunBurstFileName.Data());
    Int_t nruns=0;
    while (Line.ReadLine(RunBurstList)) {
      InputLine = Line.Tokenize(" ");
      run.push_back((((TObjString*)(InputLine->At(0)))->GetString()).Atoi());
      burst.push_back( (((TObjString*)(InputLine->At(1)))->GetString()).Atoi() );
      nruns++;
    }
    printf("[info] Decoding primitives from the list: %s\n",InputListRunBurstFileName.Data());
    if (AllDisabled) {
      cout <<"[warning] No detector was enabled in config file, enabling all by default" << endl;
      for (Int_t i = 0; i < NDETECTORS; i++) detlist[i] = kTRUE;
    }

    for (Int_t i=0; i<nruns; i++) {
      OutputFileName = TString(Form("run%05d_burst%05d.root", run[i], burst[i]));
      InputFileName  = TString(Form("run%05d_burst%05d", run[i], burst[i]));

      is32bit=DecodePrimitives(InputFileName, OutputPath + "/" + OutputFileName, PrimitivesPath, detlist, PrimitivesSubPath, PrimitivesFilePrefix, kTRUE, monitor,32,Granularity);
      if(is32bit==-1)DecodePrimitives(InputFileName, OutputPath + "/" + OutputFileName, PrimitivesPath, detlist, PrimitivesSubPath, PrimitivesFilePrefix, kTRUE, monitor,64,Granularity);

    }

    end =std::chrono::system_clock::now();
    std::chrono::duration<Double_t> elapsed_seconds = end-start;
    cout << "[info] All done in " << elapsed_seconds.count() << " seconds." <<endl;
    return 0;
  }

  // -r, -b options: run and burst numbers
  else if(NRun > 0 && NBurst > 0) {
    printf("[info] Decoding primitives for run %i, burst %i\n", NRun, NBurst);
    OutputFileName = TString(Form("run%05d_burst%05d.root", NRun, NBurst));
    InputFileName  = TString(Form("run%05d_burst%05d", NRun, NBurst));
    if (AllDisabled) {
      cout <<"[warning] No detector was enabled in config file, enabling all by default" << endl;
      for (Int_t i = 0; i < NDETECTORS; i++) detlist[i] = kTRUE;
    }

    is32bit=DecodePrimitives(InputFileName, OutputPath + "/" + OutputFileName, PrimitivesPath, detlist,  PrimitivesSubPath, PrimitivesFilePrefix, kTRUE, monitor,32,Granularity);
    if(is32bit==-1)DecodePrimitives(InputFileName, OutputPath + "/" + OutputFileName, PrimitivesPath, detlist,  PrimitivesSubPath, PrimitivesFilePrefix, kTRUE, monitor,64,Granularity);




    end =std::chrono::system_clock::now();
    std::chrono::duration<Double_t> elapsed_seconds = end-start;
    cout << "[info] All done in " << elapsed_seconds.count() << " seconds." <<endl;
    return 0;
  }

  // -l option: list of files
  else if(stat(Form(InputListFileName.Data()), &filestat) == 0) {
    ifstream InputList(InputListFileName.Data());
    while(InputFileName.ReadLine(InputList)){
      InputFileNameList.Add(new TObjString(InputFileName.Data()));
      InputLine = InputFileName.Tokenize("/");
      lineparse = InputLine->GetEntries();
      OutputFileName = ((TObjString*)(InputLine->At(lineparse-1)))->GetString()+".root";
      OutputFileNameList.Add(new TObjString(OutputFileName.Data()));
    }
    printf("Decoding primitives from the list: %s\n",InputListFileName.Data());
  }

  // -i option: input file name is specified
  else {
    InputFileNameList.Add(new TObjString(InputFileName.Data()));
    InputLine = InputFileName.Tokenize("/");
    lineparse = InputLine->GetEntries();
    if (lineparse){
      OutputFileName = ((TObjString*)(InputLine->At(lineparse-1)))->GetString()+".root";
      OutputFileNameList.Add(new TObjString(OutputFileName.Data()));
      printf("[info] Decoding primitives from input file: %s\n",InputFileName.Data());
    }
    else {
      OutputFileName = InputFileName+"root";
      OutputFileNameList.Add(new TObjString(OutputFileName.Data()));
    }
  }


  if(InputFileNameList.GetEntries() == 0) {
    perror(Form("[error] No Input File \n"));
    exit(1);
  }
  else {
    for (Int_t iFile = 0; iFile < InputFileNameList.GetEntries(); iFile++ ) {
      InputFileName  = ((TObjString*)InputFileNameList.At(iFile))->GetString();
      OutputFileName = ((TObjString*)OutputFileNameList.At(iFile))->GetString();

      is32bit=DecodePrimitives(InputFileName, OutputPath + "/" + OutputFileName, PrimitivesPath, detlist, PrimitivesSubPath, PrimitivesFilePrefix, kFALSE, monitor,32,Granularity);
      if(is32bit==-1)DecodePrimitives(InputFileName, OutputPath + "/" + OutputFileName, PrimitivesPath, detlist, PrimitivesSubPath, PrimitivesFilePrefix, kFALSE, monitor,64,Granularity);

    }
    end =std::chrono::system_clock::now();
    std::chrono::duration<Double_t> elapsed_seconds = end-start;
    cout << "[info] All done in " << elapsed_seconds.count() << " seconds." <<endl;
    return 0;
  }
}
