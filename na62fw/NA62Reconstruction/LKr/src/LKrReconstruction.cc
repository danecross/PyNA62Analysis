#include "LKrGeometry.hh"
#include "NA62RecoManager.hh"
#include "NA62Reconstruction.hh"
#include "LKrReconstruction.hh"
#include "TRecoLKrEvent.hh"
#include "TSlimRecoLKrEvent.hh"
#include "TRecoLKrHit.hh"
#include "TLKrEvent.hh"
#include "TLKrHit.hh"
#include "TRecoLKrCandidate.hh"
#include "TLKrMicroCellHit.hh"
#include "LKrDigitizer.hh"
#include "TLKrDigi.hh"
#include "FADCEvent.hh"
#include "CREAMRawDecoder.hh"
#include "LKrParameters.hh"
#include "LKrCommon.hh"
#include "LKrDigiManager.hh"
#include "NA62ConditionsService.hh"
#include "NA62Utilities.hh"
#include "LKrL0Emulator.hh"

#include "CLHEP/Units/SystemOfUnits.h"
using namespace CLHEP;

#define readinit readinit_
#define clus0 clus0_
#define clus1 clus1_
#define testiter testiter_
#define saveoutput saveoutput_
#define type_of_call

extern "C"
{
  void type_of_call readinit();
  void type_of_call clus0(int&);
  void type_of_call clus1(int&,int&);
  void type_of_call testiter(int&);
  void type_of_call saveoutput(int&);
}

/// \class LKrReconstruction
/// \Brief
/// LKr cluster reconstruction
/// \EndBrief
/// \Detailed
/// This is the steering class for the LKr reconstruction
/// The bulk of the reconstruction logic has been implemented porting directly
/// the NA48 Fortran code written by G. Unal and described in note NA48 98/01,
/// redistributed as note NA62-15-02
///
/// Classes used:
/// LKRParameters
/// Reads the calibration constant file defined in the config file and fills the fortran commons for the NA48 code
/// LKRDigiManager
/// Processes the raw FADC data, computes energy from the ADC counts (filling fortran commons) and finds the cluster seeds
///
/// Clusters are created in the fortran functions clus0 and clus1
///
/// \author Created by Giuseppe Ruggiero (2012) (giuseppe.ruggiero@cern.ch) with the porting of the NA48 code
/// Implementation of the new format for the calibration constants by R. Fantechi (riccardo.fantechi@cern.ch) and M.Zamkovsky (michal.zamkovsky@cern.ch)(2015)
/// \EndDetailed

LKrReconstruction::LKrReconstruction(TFile* HistoFile, TString ConfigFileName) : NA62VReconstruction(HistoFile, "LKr", ConfigFileName) {
  // Pointer to some parameter class
  fGeo = LKrGeometry::GetInstance();
  fPar = LKrParameters::GetInstance();
  fPar->SetT0FileName(fT0FileName); //T0 file name read by NA62VReco::ParseConfFile()
  ParseConfFile(ConfigFileName);

  // Pointer to the commons
  fCom = LKrCommon::GetInstance();

  // Pointer to the digi manager class
  fLKrDigiManager = new LKrDigiManager();

  fRecoEvent = new TRecoLKrEvent();
  fSlimRecoEvent = new TSlimRecoLKrEvent();

  //Primitive checks
  Double_t Tolerance = 1000.; //MeV
  for(UInt_t iBit=0;iBit<16;iBit++) fThrEnergy[iBit] = 0.;
  fThrEnergy[ 8] = 20000.+Tolerance;
  fThrEnergy[ 9] = 20000.+Tolerance;
  fThrEnergy[11] = 40000.+Tolerance;
  fThrEnergy[12] = 20000.+Tolerance;
  fThrEnergy[13] = 10000.+Tolerance;
  fThrEnergy[14] = 30000.+Tolerance;

  fLKrL0Emulator = new LKrL0Emulator();
}

LKrReconstruction::~LKrReconstruction() {
  if(fLKrDigiManager) delete fLKrDigiManager;
}

void LKrReconstruction::Init(NA62VReconstruction* MainReco) {

    // Common part for all the subdetectors
    NA62VReconstruction::Init(MainReco);

    InitHistograms();
}

void LKrReconstruction::ParseConfFile(TString ConfFileName) {
    /// \MemberDescr
    /// The default configuration file name is config/LKr.conf, and can be changed in config/NA62Reconstruction.conf.
    /// It is self documented by comments, which are supported by the parser
    /// \EndMemberDescr

    std::ifstream confFile(ConfFileName.Data());
    TString Line;

    if(!confFile.is_open()) {
        perror(Form("Configuration File : %s",ConfFileName.Data()));
        exit(kWrongConfiguration);
    }
    double units = 1;
    Int_t nItems=0;
    while(Line.ReadLine(confFile)) {
        if(Line.BeginsWith("#")) continue; /* comment line */
        else if(Line.BeginsWith("HardwareType")){
          TObjArray * line = Line.Tokenize(" ");
          TString HardwareName = static_cast<TObjString*>(line->At(2))->GetString();
          if(HardwareName.CompareTo("CPD") == 0)
            SetHwType(kCPD);
          else if(HardwareName.CompareTo("SLM") == 0)
            SetHwType(kSLM);
          else if(HardwareName.CompareTo("CREAM") == 0)
            SetHwType(kCREAM);
          else Exception("Bad hardware type in LKr config file");
          nItems++;
          delete line;
        } else if(Line.BeginsWith("Decoding_param=")) {
          TObjArray * l = Line.Tokenize(" ");
          fPar->SetDecodingParam(0,static_cast<TObjString*>(l->At(1))->GetString().Atoi());
          fPar->SetDecodingParam(1,static_cast<TObjString*>(l->At(2))->GetString().Atoi());
          fPar->SetDecodingParam(2,static_cast<TObjString*>(l->At(3))->GetString().Atoi());
          fPar->SetDecodingParam(3,static_cast<TObjString*>(l->At(4))->GetString().Atoi());
          fPar->SetDecodingParam(4,static_cast<TObjString*>(l->At(5))->GetString().Atoi());
          fPar->SetDecodingParam(5,static_cast<TObjString*>(l->At(6))->GetString().Atoi());
          fPar->SetDecodingParam(6,static_cast<TObjString*>(l->At(7))->GetString().Atoi());
          fPar->SetDecodingParam(7,static_cast<TObjString*>(l->At(8))->GetString().Atoi());
          fPar->SetDecodingParam(8,static_cast<TObjString*>(l->At(9))->GetString().Atoi());
          fPar->SetDecodingParam(9,static_cast<TObjString*>(l->At(10))->GetString().Atoi());
          nItems++;
          delete l;
        } else if(Line.BeginsWith("DigiFilter_PreProcess=")) {
          TObjArray * l = Line.Tokenize(" ");
          fPar->SetDigiFilterPreProcess(static_cast<TObjString*>(l->At(1))->GetString().Atoi());
          nItems++;
          delete l;
        } else if(Line.BeginsWith("NSample_Pedestals=")) {
          TObjArray * l = Line.Tokenize(" ");
          fPar->SetNSamplePedestals(static_cast<TObjString*>(l->At(1))->GetString().Atoi());
          nItems++;
          delete l;
       } else if(Line.BeginsWith("NSample_Noise=")) {
          TObjArray * l = Line.Tokenize(" ");
          fPar->SetNSampleNoise(static_cast<TObjString*>(l->At(1))->GetString().Atoi());
          nItems++;
          delete l;
       } else if(Line.BeginsWith("Sample_TimeRange=")) {
         TObjArray * l = Line.Tokenize(" ");
         fPar->SetSampleTimeLow(static_cast<TObjString*>(l->At(1))->GetString().Atoi());
         fPar->SetSampleTimeHigh(static_cast<TObjString*>(l->At(2))->GetString().Atoi());
         nItems++;
         delete l;
       } else if(Line.BeginsWith("Seed_EnergyCut=")) {
         TObjArray * l = Line.Tokenize(" ");
         if (l->GetEntriesFast()>3) units = NA62Utilities::GetInstance()->GetUnitFromString(static_cast<TObjString*>(l->At(3))->GetString());
         else { std::cout << "WARNING: Seed Energy Cut in " << ConfFileName << " without units" << std::endl; exit(kWrongConfiguration); }
         fPar->SetSeedEnergyCut(static_cast<TObjString*>(l->At(1))->GetString().Atof()*(units/GeV));
         fPar->SetSeedECutRatio(static_cast<TObjString*>(l->At(2))->GetString().Atof());
         nItems++;
         delete l;
       } else if(Line.BeginsWith("Cluster_XCorr=")) {
         TObjArray * l = Line.Tokenize(" ");
         fPar->SetClusterXCorr(0, static_cast<TObjString*>(l->At(1))->GetString().Atof());
         fPar->SetClusterXCorr(1, static_cast<TObjString*>(l->At(2))->GetString().Atof());
         fPar->SetClusterXCorr(2, static_cast<TObjString*>(l->At(3))->GetString().Atof());
         fPar->SetClusterXCorr(3, static_cast<TObjString*>(l->At(4))->GetString().Atof());
         fPar->SetClusterXCorr(4, static_cast<TObjString*>(l->At(5))->GetString().Atof());
         fPar->SetClusterXCorr(5, static_cast<TObjString*>(l->At(6))->GetString().Atof());
         fPar->SetClusterXCorr(6, static_cast<TObjString*>(l->At(7))->GetString().Atof());
         fPar->SetClusterXCorr(7, static_cast<TObjString*>(l->At(8))->GetString().Atof());
         fPar->SetClusterXCorr(8, static_cast<TObjString*>(l->At(9))->GetString().Atof());
         fPar->SetClusterXCorr(9, static_cast<TObjString*>(l->At(10))->GetString().Atof());
         fPar->SetClusterXCorr(10,static_cast<TObjString*>(l->At(11))->GetString().Atof());
         fPar->SetClusterXCorr(11,static_cast<TObjString*>(l->At(12))->GetString().Atof());
         nItems++;
         delete l;
       } else if(Line.BeginsWith("Cluster_YCorr=")) {
         TObjArray * l = Line.Tokenize(" ");
         fPar->SetClusterYCorr(0,static_cast<TObjString*>(l->At(1))->GetString().Atof());
         fPar->SetClusterYCorr(1,static_cast<TObjString*>(l->At(2))->GetString().Atof());
         fPar->SetClusterYCorr(2,static_cast<TObjString*>(l->At(3))->GetString().Atof());
         fPar->SetClusterYCorr(3,static_cast<TObjString*>(l->At(4))->GetString().Atof());
         fPar->SetClusterYCorr(4,static_cast<TObjString*>(l->At(5))->GetString().Atof());
         fPar->SetClusterYCorr(5,static_cast<TObjString*>(l->At(6))->GetString().Atof());
         nItems++;
         delete l;
       } else if(Line.BeginsWith("Cluster_EvsXCorr=")) {
         TObjArray * l = Line.Tokenize(" ");
         fPar->SetClusterEvsXCorr(0,static_cast<TObjString*>(l->At(1))->GetString().Atof());
         fPar->SetClusterEvsXCorr(1,static_cast<TObjString*>(l->At(2))->GetString().Atof());
         fPar->SetClusterEvsXCorr(2,static_cast<TObjString*>(l->At(3))->GetString().Atof());
         fPar->SetClusterEvsXCorr(3,static_cast<TObjString*>(l->At(4))->GetString().Atof());
         fPar->SetClusterEvsXCorr(4,static_cast<TObjString*>(l->At(5))->GetString().Atof());
         fPar->SetClusterEvsXCorr(5,static_cast<TObjString*>(l->At(6))->GetString().Atof());
         nItems++;
         delete l;
       } else if(Line.BeginsWith("Cluster_EvsYCorr=")) {
         TObjArray * l = Line.Tokenize(" ");
         fPar->SetClusterEvsYCorr(0,static_cast<TObjString*>(l->At(1))->GetString().Atof());
         fPar->SetClusterEvsYCorr(1,static_cast<TObjString*>(l->At(2))->GetString().Atof());
         fPar->SetClusterEvsYCorr(2,static_cast<TObjString*>(l->At(3))->GetString().Atof());
         nItems++;
         delete l;
       } else if(Line.BeginsWith("Cluster_RMSCorr=")) {
         TObjArray * l = Line.Tokenize(" ");
         fPar->SetClusterRMSCorr(0,static_cast<TObjString*>(l->At(1))->GetString().Atof());
         fPar->SetClusterRMSCorr(1,static_cast<TObjString*>(l->At(2))->GetString().Atof());
         nItems++;
         delete l;
       } else if(Line.BeginsWith("Cluster_HoleCorr=")) {
         TObjArray * l = Line.Tokenize(" ");
         fPar->SetClusterHoleCorr(0,static_cast<TObjString*>(l->At(1))->GetString().Atof());
         fPar->SetClusterHoleCorr(1,static_cast<TObjString*>(l->At(2))->GetString().Atof());
         fPar->SetClusterHoleCorr(2,static_cast<TObjString*>(l->At(3))->GetString().Atof());
         fPar->SetClusterHoleCorr(3,static_cast<TObjString*>(l->At(4))->GetString().Atof());
         nItems++;
         delete l;
       } else if(Line.BeginsWith("Cluster_OutCorr=")) {
         TObjArray * l = Line.Tokenize(" ");
         fPar->SetClusterOutCorr(0,static_cast<TObjString*>(l->At(1))->GetString().Atof());
         fPar->SetClusterOutCorr(1,static_cast<TObjString*>(l->At(2))->GetString().Atof());
         fPar->SetClusterOutCorr(2,static_cast<TObjString*>(l->At(3))->GetString().Atof());
         fPar->SetClusterOutCorr(3,static_cast<TObjString*>(l->At(4))->GetString().Atof());
         nItems++;
         delete l;
       } else if(Line.BeginsWith("EnergyScale=")) {
         TObjArray * l = Line.Tokenize(" ");
         fPar->SetEnergyScale(static_cast<TObjString*>(l->At(1))->GetString().Atof());
         nItems++;
         delete l;
       } else if(Line.BeginsWith("ClusterTimeOffset=")) {
         TObjArray * l = Line.Tokenize(" ");
         fClusterTimeOffset = static_cast<TObjString*>(l->At(1))->GetString().Atof();
         nItems++;
         delete l;
       } else if(Line.BeginsWith("TimeParameters=")) {
         TObjArray * l = Line.Tokenize(" ");
         fPar->SetTimePulNent(static_cast<TObjString*>(l->At(1))->GetString().Atoi());
         fPar->SetTimePulStep(static_cast<TObjString*>(l->At(2))->GetString().Atof());
         fPar->SetTimePulNpeak(static_cast<TObjString*>(l->At(3))->GetString().Atoi());
         fPar->SetTimePulScale(static_cast<TObjString*>(l->At(4))->GetString().Atof());
         fPar->SetTimePBase(static_cast<TObjString*>(l->At(5))->GetString().Atof());
         nItems++;
         delete l;
       } else if(Line.BeginsWith("ZeroSuppressionAlgorithm=")) {
         TObjArray * l = Line.Tokenize(" ");
         fPar->SetZSAlgorithm(static_cast<TObjString*>(l->At(1))->GetString().Atoi());
         nItems++;
         delete l;
       } else if(Line.BeginsWith("OutputHits=")) {
         TObjArray * l = Line.Tokenize(" ");
         fPar->SetOutputHits(static_cast<TObjString*>(l->At(1))->GetString().Atoi());
         nItems++;
         delete l;
       } else if(Line.BeginsWith("SlopeFileName=")) {
         TObjArray * l = Line.Tokenize(" ");
         TString slopefilename = static_cast<TObjString*>(l->At(1))->GetString();
         fPar->SetSlopeFileName(slopefilename);
         nItems++;
         delete l;
       } else if(Line.BeginsWith("PedestalFileName=")) {
         TObjArray * l = Line.Tokenize(" ");
         TString pedfilename = static_cast<TObjString*>(l->At(1))->GetString();
         fPar->SetPedFileName(pedfilename);
         nItems++;
         delete l;
       } else if(Line.BeginsWith("RefShapeFileName=")) {
         TObjArray * l = Line.Tokenize(" ");
         TString refshapefilename = static_cast<TObjString*>(l->At(1))->GetString();
         fPar->SetRefShapeFileName(refshapefilename);
         nItems++;
         delete l;
       } else if(Line.BeginsWith("DigFilterFileName=")) {
         TObjArray * l = Line.Tokenize(" ");
         TString digfiltfilename = static_cast<TObjString*>(l->At(1))->GetString();
         fPar->SetDigFiltConstFileName(digfiltfilename);
         nItems++;
         delete l;
       } else if(Line.BeginsWith("Ke3CorrFileName=")) {
         TObjArray * l = Line.Tokenize(" ");
         TString ke3filename = static_cast<TObjString*>(l->At(1))->GetString();
         fPar->SetKe3CorrFileName(ke3filename);
         nItems++;
         delete l;
       } else if(Line.BeginsWith("TimePulConsFileName=")) {
         TObjArray * l = Line.Tokenize(" ");
         TString pulconsfilename = static_cast<TObjString*>(l->At(1))->GetString();
         fPar->SetPulConsFileName(pulconsfilename);
         nItems++;
         delete l;
       } else if(Line.BeginsWith("TimePulLineFileName=")) {
         TObjArray * l = Line.Tokenize(" ");
         TString pullinefilename = static_cast<TObjString*>(l->At(1))->GetString();
         fPar->SetPulLineFileName(pullinefilename);
         nItems++;
         delete l;
       } else if(Line.BeginsWith("PedestalsEvaluation=")) {
         TObjArray * l = Line.Tokenize(" ");
         fPar->SetPedestalsEvaluation(static_cast<TObjString*>(l->At(1))->GetString().Atoi());
         nItems++;
         delete l;
       } else if(Line.BeginsWith("MCHitTimeSmearing=")) {
         TObjArray * l = Line.Tokenize(" ");
         fPar->SetMCHitTimeSmearing(static_cast<TObjString*>(l->At(1))->GetString().Atof());
         nItems++;
         delete l;
       }
    }
    //cout << "LKr hardware type " << GetHwType() << std::endl;
#ifdef close
#undef close
  confFile.close();
#define close           rfio_close
#endif
}

TDetectorVEvent * LKrReconstruction::Trigger(TDetectorVEvent * tEvent, Event* /*tGenEvent*/)
{
  return tEvent;
}

void LKrReconstruction::StartOfBurst(){
  NA62VReconstruction::StartOfBurst(); // common part for all the subdetectors

  // Read Constants
  fPar->DefineDataType(static_cast<NA62Reconstruction*>(fMainReco)->GetIsRawData());
  fPar->SetTriggerDriftT0(static_cast<NA62Reconstruction*>(fMainReco)->GetTriggerDriftT0());
  fPar->Fill(this);

  // Read LKr Jitter file to correct "full" jitters
  ReadJitterFile();
}

void LKrReconstruction::EndOfBurst(){
  NA62VReconstruction::EndOfBurst(); // common part for all the subdetectors

  UpdateLKrMonitorHisto();
  // Look for oscillating cells
  TH2F* hPedestals = static_cast<CREAMRawDecoder*>(fRawDecoder->GetDecoder())->GetHPedestals();
  TH2F* hPedestalsRMS = static_cast<CREAMRawDecoder*>(fRawDecoder->GetDecoder())->GetHPedestalsRMS();
  for(Int_t iMezzanine=0;iMezzanine<hPedestals->GetNbinsX();iMezzanine++){
    TH1D* h = hPedestals->ProjectionY(Form("LKrPedestals_%d",iMezzanine),iMezzanine+1,iMezzanine+1);
    Double_t RMS = h->GetRMS();
    hPedestalsRMS->Fill(iMezzanine,RMS);
    delete h;
  }
}

TRecoVEvent * LKrReconstruction::ProcessEvent(TDetectorVEvent* tEvent, Event* tGenEvent){

  if (tEvent->IsA() == TSpecialTriggerEvent::Class()) {
    return 0;
  }

  //common part for all the subdetectors
  NA62VReconstruction::ProcessEvent(tEvent, tGenEvent);

  fNClusters = 0;
  fNClustersAboveThr = 0;
  if ((tEvent->GetTriggerType()&0xff) == 0x30) { // do not process Calibration triggers
    return fRecoEvent;
  }

  // Pointer to the pre-processed (digitized if MC) event (ADC bank)
  fFADCEvent = static_cast<FADCEvent*>(tEvent);

  // ADC bank -> Energy bank
  fLKrDigiManager->Reset();
  fLKrDigiManager->ImportEvent(fFADCEvent);
  fFADCEventEnergy = fLKrDigiManager->ADCtoEnergy();

  // Read the pre-processed classes
  iFlagRec = fLKrDigiManager->CalRead();
  if (iFlagRec){
    //cout << "Error in calread" << " " << iFlagRec << std::endl;
    EndEvent(iFlagRec);
    return fRecoEvent;
  }

  fPar->SetTriggerType(NA62RecoManager::GetInstance()->GetEventHeader()->GetTriggerType());

  // Find clusters
  clus0(iFlagRec);
  if (iFlagRec){
    iFlagRec *= -1;
    std::cout << "[LKrReconstruction] Error in clus0" << std::endl;
    EndEvent(iFlagRec);
    return fRecoEvent;
  }
  fNClusters = fCom->GetCLU()->NCLUS;
  if (!fNClusters || iFlagRec){
    EndEvent(iFlagRec);
    return fRecoEvent;
  }

  // Reconstruct clusters (first iteration)
  Int_t iteration = 1;
  clus1(iteration,iFlagRec);
  if (iFlagRec){
    iFlagRec *= -1;
    std::cout << "[LKrReconstruction] Error in clus1" << std::endl;
    fCom->GetCLU()->NCLUS = 0;
    EndEvent(iFlagRec);
    return fRecoEvent;
  }

  // Reconstruct clusters (second iteration)
  testiter(iFlagRec);
  Int_t niter;
  if (iFlagRec>0){
    niter = ((iFlagRec>>1)&1) ? 2 : 1;
    for (Int_t iter=0; iter<niter; iter++) clus1(niter,iFlagRec);
  }
  if (iFlagRec){
    //std::cout << "Error in testiter" << std::endl;
    fCom->GetCLU()->NCLUS = 0;
    EndEvent(iFlagRec);
    return fRecoEvent;
  }

  // Save and write output
  EndEvent(iFlagRec);
  return fRecoEvent;
}

void LKrReconstruction::EndProcessing(){
  NA62VReconstruction::EndProcessing(); // call base class for raw hist output
  SaveHistograms();
}

void LKrReconstruction::FillTimes(Double_t ReferenceTime) {
  //common part for all the subdetectors
  NA62VReconstruction::FillTimes(ReferenceTime);

  //use of all reco hits - no candidate structure
  TClonesArray  &Hits = (*(fRecoEvent->GetHits()));
  for (Int_t iHit=0; iHit<fRecoEvent->GetNHits(); iHit++) {
    TRecoLKrHit *RecoHit = static_cast<TRecoLKrHit *>(Hits[iHit]);
    if (RecoHit->GetEnergy()<250.) continue; // remove noise
    Int_t ix     = RecoHit->GetXCellID();
    Int_t iy     = RecoHit->GetYCellID();
    Int_t iCell  = ix*128+iy;
    Int_t iROCh  = fRawDecoder->GetDecoder()->GetChannelRO(RecoHit->GetChannelID());
    Double_t Time  = RecoHit->GetTime();
    if (fHRecoHitTimeWrtReferenceVsCell)          fHRecoHitTimeWrtReferenceVsCell->Fill(iCell, Time-ReferenceTime);
    if (fHRecoHitTimeWrtReferenceVsCellNoT0)      fHRecoHitTimeWrtReferenceVsCellNoT0->Fill(iCell, Time+fPar->GetCellT0(ix,iy)-ReferenceTime);
    if (fHRecoHitTimeWrtReferenceVsROChannel)     fHRecoHitTimeWrtReferenceVsROChannel->Fill(iROCh, Time-ReferenceTime);
    if (fHRecoHitTimeWrtReferenceVsROChannelNoT0) fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(iROCh, Time+fPar->GetCellT0(ix,iy)-ReferenceTime);
  }
}

void LKrReconstruction::EndEvent(Int_t iflag){
  saveoutput(iflag);
  if (fPar->GetOutputHits()) FillOutputHits();
  FillOutput();
  fLKrL0Emulator->Process(fFADCEvent, NA62RecoManager::GetInstance()->GetEventHeader()->GetL0TPData());
  ApplyEnergyCorrections();
}

void LKrReconstruction::FillOutputHits(){
  Int_t nHits = fFADCEventEnergy->GetNHits();
  if(fFADCEvent->GetFADCID()==10) fHNHits->Fill(nHits); //only ZS events!
  TClonesArray &cellsEnergy = (*(fFADCEventEnergy->GetHits()));
  TClonesArray &cellsADC = (*(fFADCEvent->GetHits()));
  Int_t nSamples[8];
  Int_t trigflag = NA62RecoManager::GetInstance()->GetEventHeader()->GetL0TPData()->GetTriggerFlags();

  streambuf *backup=0;
  ofstream filestr;
  if(fPar->GetPedestalsEvaluation()) {
    streambuf *psbuf=0;
    filestr.open ("samplings.txt", ofstream::app);
    backup = cout.rdbuf();     // back up cout's streambuf
    psbuf = filestr.rdbuf();   // get file's streambuf
    cout.rdbuf(psbuf);         // assign streambuf to cout
  }

  for (Int_t j=0; j<8; j++) nSamples[j] = 0;
  Double_t fineTriggerTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256;
  UInt_t primBits = GetPrimitiveBits();
  Double_t totCellEnergy = 0;
  for (Int_t icell=0; icell<nHits; icell++){
    TLKrDigi *CellEnergy = static_cast<TLKrDigi *>(cellsEnergy[icell]);
    Int_t ix = CellEnergy->GetXCellID();
    Int_t iy = CellEnergy->GetYCellID();
    if ((CellEnergy->GetFlags()>>kCREAMNoCalibrationBit)&1 || fPar->IsDeadCell(ix,iy)) continue; // Reject dead cells (from database)
    if(fPar->GetPedestalsEvaluation()) {
      cout << setw(4) << ix << setw(4) << iy << " ";
    }

    TRecoLKrHit *hit = static_cast<TRecoLKrHit *>(fRecoEvent->AddHit(CellEnergy));
    hit->DecodeChannelID();
    hit->SetEnergy(1000.*CellEnergy->GetPeakEnergy());
    hit->SetGain(CellEnergy->GetGain());
    Double_t x = fCom->GetCUN()->XCELL_LKR[ix];
    Double_t y = fCom->GetCUN()->YCELL_LKR[iy];
    hit->SetPosition(TVector3(10.*x,10.*y,0));
    hit->SetTime(CellEnergy->GetPeakTime()-GetT0Correction(CellEnergy)-GetJitterCorrection(CellEnergy)-fPar->GetCellT0(ix,iy));
    hit->SetPedestal(CellEnergy->GetADCPeakEnergy()); // ADCPeakEnergy not used for cellEnergy, used for real pedestal propagation
    TLKrDigi *CellADC = static_cast<TLKrDigi *>(cellsADC[icell]);

    // Histograms for Online Monitoring
    Double_t *ADCSamples = CellADC->GetAllSamples();

    //Double_t Pedestal = (Double_t)(ADCSamples[0]+ADCSamples[1])/2.;
    Double_t Pedestal = hit->GetPedestal();

    Int_t maximum = 0;
    Int_t maxgain = -1;
    Int_t maxsample = 0;

    if(fPar->GetPedestalsEvaluation()) {
      cout << setw(5) << ADCSamples[0] << " " << setw(5) << ADCSamples[1];
    }
    for (Int_t j=0; j<8; j++) {
      nSamples[j] += ADCSamples[j];
      if (ADCSamples[j]>maximum){
        maximum = ADCSamples[j];
        maxgain = j;
      }
      if (ADCSamples[j]>maxsample) {
        maxsample=ADCSamples[j];
      }
      fHADCCountLow->Fill(ADCSamples[j]);
      fHADCCountAll->Fill(ADCSamples[j]);
    }
    if(fPar->GetPedestalsEvaluation()) {
      cout << endl;
    }

    fHMaxSample->Fill(maxgain);
    for(Int_t iL0Mask=0;iL0Mask<16;iL0Mask++){
      if(trigflag&(1<<iL0Mask)) fHMaxSampleVsL0Mask->Fill(maxgain,iL0Mask);
    }

    Int_t xBin = ix+1; //take care of offset in the bin numbering
    Int_t yBin = iy+1; //take care of offset in the bin numbering

    fHZSCounter->SetBinContent(xBin,yBin,fHZSCounter->GetBinContent(xBin,yBin)+1);
    if((NA62RecoManager::GetInstance()->GetEventHeader()->GetTimeStamp()*ClockPeriod)*1e-9 <0.9 ) {
      fHZSCounterOOB->SetBinContent(xBin,yBin,fHZSCounterOOB->GetBinContent(xBin,yBin)+1);
    }
    fHPedestal_xy->SetBinContent(xBin,yBin,(Pedestal+(fHPedestal_xy->GetBinContent(xBin,yBin)*(fHZSCounter->GetBinContent(xBin,yBin)-1)))/fHZSCounter->GetBinContent(xBin,yBin));

    fHSigma->SetBinContent(xBin,yBin,TMath::Sqrt((TMath::Power(fHSigma->GetBinContent(xBin,yBin),2)*(fHZSCounter->GetBinContent(xBin,yBin)-1)+TMath::Power((fHPedestal_xy->GetBinContent(xBin,yBin)-ADCSamples[0]),2))/fHZSCounter->GetBinContent(xBin,yBin)));

    fHPedestal->Fill(Pedestal);
    fHHitMap->Fill(ix,iy);
    fHHitMapEnergy->Fill(ix,iy,1000*CellEnergy->GetPeakEnergy());
    //----- (x,y) -> (Crate,Slot,Channel) conversion -----//
    Int_t iCrate = (ix/16)*4 +3-iy/32;
    Int_t iSlot  = (ix%16)/4  +17-((iy%32)/8)*4 -((iy%32)/16)*2;
    Int_t iChannel = 7-(iy%8)+(ix%4)*8;
    //special cases
    if(iCrate== 0){
      iCrate += 4;  //0 -> 4
      iSlot  -= 16; //19,20 -> 3,4
    }
    if(iCrate==3){
      iCrate += 4;  //3 -> 7
      iSlot  += 12; //5,6 -> 17,18
    }
    if(iCrate==28){
      iCrate -= 4;  //28 -> 24
      iSlot  -= 12; //17,18 -> 5,6
    }
    if(iCrate==31){
      iCrate -= 4;  //31 -> 27
      iSlot  += 16; //3,4 -> 19,20
    }
    fHHitMapCrateSlot->Fill(iCrate+0.5*0.75-(iChannel/8)*0.25,iSlot+0.5*0.875-(iChannel%8)*0.125);
    fHHitMapEnergyCrateSlot->Fill(iCrate+0.5*0.75-(iChannel/8)*0.25,iSlot+0.5*0.875-(iChannel%8)*0.125,1000*CellEnergy->GetPeakEnergy());

    Double_t cell_energy = hit->GetEnergy();
    Double_t delta_time = hit->GetTime()-fineTriggerTime;
    //    cout << delta_time << " " << cell_energy << endl;
    if (cell_energy<50) continue;
    fHCellTimeVsEnergy->Fill(cell_energy,delta_time);
    if (fabs(delta_time)>20) continue;
    totCellEnergy += cell_energy;
  }
  fHCellTotalEnergy->Fill(totCellEnergy,trigflag);
  if(NA62RecoManager::GetInstance()->GetEventHeader()->GetL0TPData()->GetDataType()&0x10){ //control triggers only
    for(UInt_t iBit=0; iBit<16; iBit++){
      if(primBits&(1<<iBit)) {
        fHCellTotalEnergyVsPrimBits->Fill(totCellEnergy,iBit);
        fHEnergyPrimBitsCellsNum[iBit]->Fill(totCellEnergy);
      }
      fHEnergyPrimBitsCellsDen[iBit]->Fill(totCellEnergy);
    }
    for (Int_t iHit=0; iHit<fRecoEvent->GetNHits(); iHit++){
      TRecoLKrHit *hit = static_cast<TRecoLKrHit *>(fRecoEvent->GetHit(iHit));
      if (hit->GetEnergy()<50.) continue;
      if (fabs(hit->GetTime()-fineTriggerTime)>20) continue;
      for(UInt_t iBit=0; iBit<16; iBit++){
        if(totCellEnergy>fThrEnergy[iBit]){
          if(primBits&(1<<iBit)) fHHitMapPrimBitsCellsNum[iBit]->Fill(hit->GetXCellID(),hit->GetYCellID());
          fHHitMapPrimBitsCellsDen[iBit]->Fill(hit->GetXCellID(),hit->GetYCellID());
        }
      }
    }
  }
  fTotCellEnergy = totCellEnergy;
  if(fPar->GetPedestalsEvaluation()) {
    cout.rdbuf(backup); // restore cout's original streambuf
    filestr.close();
  }
}

void LKrReconstruction::UpdateLKrMonitorHisto(){
  // Update PrimBits plots
  for(UInt_t iBit=0; iBit<16; iBit++){
    fHHitMapPrimBitsCellsEff[iBit]->Divide(fHHitMapPrimBitsCellsNum[iBit],fHHitMapPrimBitsCellsDen[iBit],1,1,"B");
    fHHitMapPrimBitsClustersEff[iBit]->Divide(fHHitMapPrimBitsClustersNum[iBit],fHHitMapPrimBitsClustersDen[iBit],1,1,"B");
    fHEnergyPrimBitsCellsEff[iBit]->Divide(fHEnergyPrimBitsCellsNum[iBit],fHEnergyPrimBitsCellsDen[iBit],1,1,"B");
    fHEnergyPrimBitsClustersEff[iBit]->Divide(fHEnergyPrimBitsClustersNum[iBit],fHEnergyPrimBitsClustersDen[iBit],1,1,"B");
  }
}

void LKrReconstruction::FillOutput(){
  TRecoLKrEvent *lkrEvent = static_cast<TRecoLKrEvent *>(fRecoEvent);
  lkrEvent->SetEnergyTotal(1000.*(Double_t)fCom->GetOUT()->ETOTAL);
  lkrEvent->SetRecFlag(fCom->GetOUT()->IRECFLAG);
  lkrEvent->SetID(fFADCEvent->GetID());
  lkrEvent->SetTimeStamp(NA62RecoManager::GetInstance()->GetEventHeader()->GetTimeStamp());
  fHNClusters->Fill(fCom->GetOUT()->NCLUSTERS);
  Double_t fineTriggerTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256;
  Int_t trigflag = NA62RecoManager::GetInstance()->GetEventHeader()->GetL0TPData()->GetTriggerFlags();
  UInt_t primBits = GetPrimitiveBits();
  Double_t totalEnergy = 0.0;
  fNClusters = fCom->GetOUT()->NCLUSTERS;

  for (Int_t iClus=0; iClus<fNClusters; iClus++){
    TRecoLKrCandidate *cluster = static_cast<TRecoLKrCandidate *>(fRecoEvent->AddCandidate());
    cluster->SetId(fCom->GetOUT()->IDCLUS[iClus]-1); //fortran -> c++
    cluster->SetNCells(fCom->GetOUT()->NCELLS[iClus]);
    cluster->SetIdSeed(fCom->GetOUT()->IDSEEDCELL[iClus]);
    cluster->SetClusterEnergy(1000.*(Double_t)fCom->GetOUT()->ENERGY[iClus]);
    fHClusterEnergy->Fill(cluster->GetClusterEnergy());
    if(cluster->GetClusterEnergy()>1000.) fNClustersAboveThr++;
    cluster->SetClusterEnergyError(1000.*(Double_t)fCom->GetOUT()->EENERGY[iClus]);
    cluster->SetClusterStatus(fCom->GetOUT()->STATFLAG[iClus]);
    cluster->SetClusterX(10.*(Double_t)fCom->GetOUT()->XPOS[iClus]);
    cluster->SetClusterY(10.*(Double_t)fCom->GetOUT()->YPOS[iClus]);
    cluster->SetClusterRMSX((fGeo->GetLKrCellLength()/mm)*(Double_t)fCom->GetOUT()->RMSX[iClus]);
    cluster->SetClusterRMSY((fGeo->GetLKrCellLength()/mm)*(Double_t)fCom->GetOUT()->RMSY[iClus]);
    cluster->SetTime((Double_t)fCom->GetOUT()->TIME[iClus]-fClusterTimeOffset);
    cluster->SetClusterChi2RMS((Double_t)fCom->GetOUT()->CHI2RMS[iClus]);
    cluster->SetClusterTimeLateralCell((Double_t)fCom->GetOUT()->TLATCELL[iClus]);
    cluster->SetClusterDDeadCell(10.*(Double_t)fCom->GetOUT()->DDEADCELL[iClus]);
    cluster->SetClusterUEnergy(1000.*(Double_t)fCom->GetOUT()->UENERGY[iClus]);
    cluster->SetClusterEnoise(1000.*(Double_t)fCom->GetOUT()->E2SAMPALL[iClus]);
    cluster->SetCluster77Energy(1000.*(Double_t)fCom->GetOUT()->E77CLUS[iClus]);
    cluster->SetSpaceChargeCorr((Double_t)fCom->GetOUT()->SPACHARCORR[iClus]);
    cluster->SetClusterKe3Energy(1000.*(Double_t)fCom->GetOUT()->ECORRKE3[iClus]);
    cluster->SetCluster77Enoise(1000.*(Double_t)fCom->GetOUT()->ENOIS77[iClus]);
    cluster->SetN77Cells(fCom->GetOUT()->N77[iClus]);
    fHClusterXY->Fill(cluster->GetClusterX(),cluster->GetClusterY());
    for (Int_t iCell=0; iCell<fCom->GetOUT()->N77[iClus]; iCell++){
      cluster->SetId77Cell(iCell,fCom->GetOUT()->IDCELL77[iCell][iClus]);
      cluster->SetFlag77Cell(iCell,fCom->GetOUT()->IFLCELL77[iCell][iClus]);
      cluster->SetEnergy77Cell(iCell,1000.*(Double_t)fCom->GetOUT()->ECELL77[iCell][iClus]);
      cluster->SetTime77Cell(iCell,(Double_t)fCom->GetOUT()->TCELL77[iCell][iClus]);
      for(Int_t iHit=0;iHit<fRecoEvent->GetNHits();iHit++){ //Add hits77 to cluster candidate
        TRecoLKrHit* Hit = static_cast<TRecoLKrHit*>(fRecoEvent->GetHit(iHit));
        if(Hit->GetChannelID()==fCom->GetOUT()->IDCELL77[iCell][iClus]){
          cluster->AddHit(iHit);
          break;
        }
      }
    }
    cluster->SetClusterSeedEnergy(1000.*(Double_t)fCom->GetOUT()->ESEEDCELL[iClus]);
    // Compute the total energy in time with the trigger
    Double_t clus_energy = cluster->GetClusterEnergy();
    Double_t delta_time = cluster->GetTime()-fineTriggerTime;
    fHClusterTimeVsEnergy->Fill(clus_energy,delta_time);
    if (fabs(delta_time)>20.) continue;
    totalEnergy += clus_energy;
  }
  for(Int_t iL0Mask=0;iL0Mask<16;iL0Mask++){
    if(trigflag&(1<<iL0Mask)) fHTotalEnergyVsL0Mask->Fill(totalEnergy,iL0Mask);
  }

  if(NA62RecoManager::GetInstance()->GetEventHeader()->GetL0TPData()->GetDataType()&0x10){ //control triggers only
    for(UInt_t iBit=0; iBit<16; iBit++){
      Bool_t SelectedEvent = false;
      if(iBit!=8 && iBit!=9 && iBit!=14) SelectedEvent = true;
      if((iBit==8 || iBit==14) && fNClustersAboveThr>1)  SelectedEvent = true;
      if(iBit==9 && fNClustersAboveThr==1) SelectedEvent = true;

      if(SelectedEvent){
        if(primBits&(1<<iBit)) {
          fHTotalEnergyVsPrimBits->Fill(totalEnergy,iBit);
          fHEnergyPrimBitsClustersNum[iBit]->Fill(totalEnergy);
        }
        fHEnergyPrimBitsClustersDen[iBit]->Fill(totalEnergy);
        if(totalEnergy>fThrEnergy[iBit]){
          for (Int_t iCandidate=0; iCandidate<fRecoEvent->GetNCandidates(); iCandidate++){
            TRecoLKrCandidate *cluster = static_cast<TRecoLKrCandidate *>(fRecoEvent->GetCandidate(iCandidate));
            if (fabs(cluster->GetTime()-fineTriggerTime)>20) continue;
            if(primBits&(1<<iBit)) fHHitMapPrimBitsClustersNum[iBit]->Fill(cluster->GetClusterX(),cluster->GetClusterY());
            fHHitMapPrimBitsClustersDen[iBit]->Fill(cluster->GetClusterX(),cluster->GetClusterY());
          }
        }
      }
    }
  }

  if (trigflag>0) {
    fHClusVsCellTotE->Fill(totalEnergy,fTotCellEnergy);
    if (totalEnergy==0) {
      for (Int_t jcell=0; jcell<fRecoEvent->GetNHits(); jcell++) {
        TRecoLKrHit *cell = static_cast<TRecoLKrHit *>(fRecoEvent->GetHit(jcell));
        fHCellXYEzero->Fill(cell->GetXCellID(),cell->GetYCellID());
      }
    }

    if (fTotCellEnergy>25000.) {
      for (Int_t iClus=0; iClus<fCom->GetOUT()->NCLUSTERS; iClus++) {
        TRecoLKrCandidate *cluster = static_cast<TRecoLKrCandidate *>(fRecoEvent->GetCandidate(iClus));
        Double_t clus_energy = cluster->GetClusterEnergy();
        Double_t delta_time = cluster->GetTime()-fineTriggerTime;
        fHClusterTimeVsEnergy_over->Fill(clus_energy,delta_time);
      }
    }
  }
}

///////////////////////////////////////////////////////////////////////////////////////
// First-order cluster energy calibration (EG, 10 March 2019):
// corrections for the overall energy scale, non-linearity and energy loss in the hole.

void LKrReconstruction::ApplyEnergyCorrections() {

  Double_t TotalEnergy = 0.0;
  for (Int_t i=0; i<fCom->GetOUT()->NCLUSTERS; i++) {
    TRecoLKrCandidate *c = static_cast<TRecoLKrCandidate*>(fRecoEvent->GetCandidate(i));

    Double_t Energy = fPar->GetIsRawData() ?
      CorrectedEnergyData(c->GetNCells(), c->GetClusterEnergy()) :
      CorrectedEnergyMC  (c->GetNCells(), c->GetClusterEnergy());

    /////////////////////////////////////////////////////////////////////////////////////////
    // Energy loss in the hole (common correction for data and MC).
    // This correction comes from the NA48 code:
    // /afs/cern.ch/na48/offline2/compact/compact-7.3/compact/rlib/anasrc/fuser_lkrcalcor98.F
    // See function RadCorr(E,r)

    Double_t Radius = sqrt(c->GetClusterX()*c->GetClusterX()+
			   c->GetClusterY()*c->GetClusterY());
    if (Radius>=140.0 && Radius<=185.0) Energy = Energy / (0.97249+0.00014692*Radius) * 0.9999;
    Double_t rcorr2 = 0.0;
    if      (Radius>=140.0 && Radius<180.0) rcorr2 =  0.00420 - 3.7e-5*Radius;
    else if (Radius>=180.0 && Radius<200.0) rcorr2 = -0.00211;
    else if (Radius>=200.0 && Radius<220.0) rcorr2 = -0.01694 + 7.769e-5*Radius;

    Double_t CorrectedEnergy = Energy*(1.0-rcorr2);
    c->SetClusterEnergy(CorrectedEnergy);
    TotalEnergy += CorrectedEnergy;
  }
  TRecoLKrEvent *evt = static_cast<TRecoLKrEvent*>(fRecoEvent);
  evt->SetEnergyTotal(TotalEnergy);
}

/////////////////////////////////////////////////////
// Run-independent cluster energy correction for data

Double_t LKrReconstruction::CorrectedEnergyData(Int_t NCells, Double_t E0) {

  Double_t Energy = E0;

  ///////////////////////////
  // Non-linearity correction

  Double_t correctedEnergy;

  if (NCells>4) {

    // The baseline correction
    if      (E0<22000.0) correctedEnergy = E0 / (0.7666  +0.0573489*log(1e-3*E0));
    else if (E0<65000.0) correctedEnergy = E0 / (0.828962+0.0369797*log(1e-3*E0));
    else                 correctedEnergy = E0 / (0.828962+0.0369797*log(65.0));

    // Further run-dependent corrections
    Double_t e = 0.001*correctedEnergy; // MeV --> GeV
    if (e< 6.0) e =  6.0; // no energy-dependence below 6 GeV
    if (e>45.0) e = 45.0; // no energy-dependence above 45 GeV
    correctedEnergy *= (0.99 / (0.94 + 0.0037*e - 9.4e-05*e*e + 8.9e-07*e*e*e));
    Energy = correctedEnergy;
  }

  // Energy loss at low energy
  if (Energy<15000.0) Energy = (Energy+15.0)/15015.0*15000.0;

  // Global scale factor
  Energy *= 1.03;

  return Energy;
}

////////////////////////////////////////////////////////////////////////
// MC cluster energy correction evaluated from simulation of GPS photons
// in LKr with energy in the range 1-80 GeV
// A.Shaikhiev (shaykhiev@inr.ru), Nov 2016

Double_t LKrReconstruction::CorrectedEnergyMC(Int_t NCells, Double_t E0) {
  Double_t Energy = E0;
  if (NCells>4) {
    Double_t Scale = 24.12/(17.05 + sqrt(E0)) + 0.926;
    Energy = E0*Scale;
  }
  return Energy;
}

void LKrReconstruction::InitHistograms(){
  TDirectory *LKrDir = GetOrMakeDir(fHistoFile,"LKrMonitor");
  fHistoFile->cd("LKrMonitor");
  fHHitMap = new TH2F("HitMap","Cell hit map XY",128,-0.5,127.5,128,-0.5,127.5);
  fHHitMapEnergy = new TH2F("HitMapEnergy","Total energy deposited per cell",128,-0.5,127.5,128,-0.5,127.5);
  fHHitMapCrateSlot = new TH2F("HitMapCrateSlot","Cell Hit Map",128,-0.5,31.5,192,-0.5,23.5);
  fHHitMapCrateSlot->GetXaxis()->SetTitle("Crate Number");
  fHHitMapCrateSlot->GetYaxis()->SetTitle("Slot Number");
  fHHitMapEnergyCrateSlot = new TH2F("HitMapEnergyCrateSlot","Total energy deposited per cell",128,-0.5,31.5,192,-0.5,23.5);
  fHHitMapEnergyCrateSlot->GetXaxis()->SetTitle("Crate Number");
  fHHitMapEnergyCrateSlot->GetYaxis()->SetTitle("Slot Number");
  fHPedestal_xy = new TH2F("Pedestals_xy_reco","Pedestal value vs (x,y)",128,-0.5,127.5,128,-0.5,127.5);
  fHSigma = new TH2F("Pedestal sigma","Pedestal Sigma value",128,-0.5,127.5,128,-0.5,127.5);
  fHZSCounter = new TH2F("ZeroSuppressionCounter_reco","Zero-suppressed cell counter",128,-0.5,127.5,128,-0.5,127.5);
  fHZSCounterOOB = new TH2F("ZeroSuppressionCounterOOB","Zero-suppressed cell counter (first 0.9s)",128,-0.5,127.5,128,-0.5,127.5);
  fHPedestal = new TH1F("Pedestals_reco","Pedestal value",200,0,1000);
  fHMaxSample = new TH1F("MaxSample","Sample of the maximum ADC counts",8,-0.5,7.5);
  fHMaxSampleVsL0Mask  = new TH2F("MaxSampleVsL0Mask","Sample of the maximum ADC counts Vs L0Mask",8,-0.5,7.5,16,-0.5,15.5);
  fHADCCountLow   = new TH1F("ADCCountLow","ADC counts in the 0-1000 range",1000,-0.5,999.5);
  fHADCCountAll   = new TH1F("ADCCountAll","ADC counts",200,0,16000);
  fHNHits = new TH1F("NHits","Number of reconstructed hits",16835,-0.5,16834.5);
  fHNClusters = new TH1F("NClusters","Number of reconstructed clusters",20,0,20);
  fHClusterEnergy = new TH1F("ClusterEnergy","Energy of the reconstructed clusters",400,0,100000);
  fHClusterXY     = new TH2F("ClusterXY","Reconstructed clusters XY map",128,-1263.5,1263.5,128,-1263.5,1263.5);
  fHClusterTimeVsEnergy = new TH2F("ClusterTimeVsEnergy","Cluster time vs cluster energy",1000,0,100000,300,-150,150);
  fHClusterTimeVsEnergy_over = new TH2F("ClusterTimeVsEnergy_over","Cluster time vs cluster energy above threshold",1000,0,100000,300,-150,150);
  fHCellTimeVsEnergy = new TH2F("CellTimeVsEnergy","Cell time vs cell energy",1000,0,100000,300,-150,150);
  fHCellTotalEnergy = new TH2F("CellTotalEnergy","Total (sum over cells) in time energy vs trigger mask",1000,0,100000,64,0,64);
  fHClusVsCellTotE = new TH2F("ClusVsCellTotE","Total cell Energy Vs Cluster Energy",200,0,100000,200,0,100000);
  fHCellXYEzero = new TH2F("CellXYEzero","XY cells with cluster total energy = 0",128,0,128,128,0,128);
  fHTotalEnergyVsL0Mask = new TH2F("TotalEnergyVsL0Mask","Total (sum over clusters) in time energy vs L0Mask",1000,0,100000,16,-0.5,15.5);
  LKrDir->mkdir("PrimBits");
  fHCellTotalEnergyVsPrimBits = new TH2F("CellTotalEnergyVsPrimBits","Total (sum over cells) in time energy vs primitive bits",100,0,100000,16,-0.5,15.5);
  fHCellTotalEnergyVsPrimBits->GetYaxis()->SetTitle("Primitive bits");
  fHCellTotalEnergyVsPrimBits->GetXaxis()->SetTitle("Cell Total Energy [MeV]");
  fHTotalEnergyVsPrimBits = new TH2F("TotalEnergyVsPrimBits","Total (sum over clusters) in time energy vs primitive bits",100,0,100000,16,-0.5,15.5);
  fHTotalEnergyVsPrimBits->GetYaxis()->SetTitle("Primitive bits");
  fHTotalEnergyVsPrimBits->GetXaxis()->SetTitle("Total Energy [MeV]");
  for(Int_t iBit=0;iBit<16;iBit++) {
    fHHitMapPrimBitsCellsNum[iBit] = new TH2F(Form("HitMapPrimBitsCellsNum_%02d",iBit),Form("HitMapPrimBitsCells_%02d",iBit),32,-0.5,127.5,32,-0.5,127.5);
    fHHitMapPrimBitsCellsDen[iBit] = new TH2F(Form("HitMapPrimBitsCellsDen_%02d",iBit),Form("HitMapPrimBitsCells_%02d",iBit),32,-0.5,127.5,32,-0.5,127.5);
    fHHitMapPrimBitsCellsEff[iBit] = new TH2F(Form("HitMapPrimBitsCellsEff_%02d",iBit),Form("Bit %02d efficiency",iBit),32,-0.5,127.5,32,-0.5,127.5);
    fHHitMapPrimBitsCellsEff[iBit]->SetMinimum(0.);
    fHHitMapPrimBitsCellsEff[iBit]->SetMaximum(1.05);
    fHHitMapPrimBitsCellsEff[iBit]->GetYaxis()->SetTitle("Y (Cell ID)");
    fHHitMapPrimBitsCellsEff[iBit]->GetXaxis()->SetTitle("X (Cell ID)");
    fHHitMapPrimBitsClustersNum[iBit] = new TH2F(Form("HitMapPrimBitsClustersNum_%02d",iBit),Form("HitMapPrimBitsClusters_%02d",iBit),32,-1263.5,1263.5,32,-1263.5,1263.5);
    fHHitMapPrimBitsClustersDen[iBit] = new TH2F(Form("HitMapPrimBitsClustersDen_%02d",iBit),Form("HitMapPrimBitsClusters_%02d",iBit),32,-1263.5,1263.5,32,-1263.5,1263.5);
    fHHitMapPrimBitsClustersEff[iBit] = new TH2F(Form("HitMapPrimBitsClustersEff_%02d",iBit),Form("Bit %02d efficiency",iBit),32,-1263.5,1263.5,32,-1263.5,1263.5);
    fHHitMapPrimBitsClustersEff[iBit]->SetMinimum(0.);
    fHHitMapPrimBitsClustersEff[iBit]->SetMaximum(1.05);
    fHHitMapPrimBitsClustersEff[iBit]->GetYaxis()->SetTitle("Y (mm)");
    fHHitMapPrimBitsClustersEff[iBit]->GetXaxis()->SetTitle("X (mm)");
    fHEnergyPrimBitsCellsNum[iBit] = new TH1F(Form("EnergyPrimBitsCellsNum_%02d",iBit),Form("EnergyPrimBitsCells_%02d",iBit),100,0.,100000.);
    fHEnergyPrimBitsCellsDen[iBit] = new TH1F(Form("EnergyPrimBitsCellsDen_%02d",iBit),Form("EnergyPrimBitsCells_%02d",iBit),100,0.,100000.);
    fHEnergyPrimBitsCellsEff[iBit] = new TH1F(Form("EnergyPrimBitsCellsEff_%02d",iBit),Form("Bit %02d efficiency",iBit),100,0.,100000.);
    fHEnergyPrimBitsCellsEff[iBit]->Sumw2();
    fHEnergyPrimBitsCellsEff[iBit]->SetMinimum(0.);
    fHEnergyPrimBitsCellsEff[iBit]->SetMaximum(1.05);
    fHEnergyPrimBitsCellsEff[iBit]->GetYaxis()->SetTitle("Efficiency");
    fHEnergyPrimBitsCellsEff[iBit]->GetXaxis()->SetTitle("Cell Total Energy [MeV]");
    fHEnergyPrimBitsClustersNum[iBit] = new TH1F(Form("EnergyPrimBitsClustersNum_%02d",iBit),Form("EnergyPrimBitsClusters_%02d",iBit),100,0.,100000.);
    fHEnergyPrimBitsClustersDen[iBit] = new TH1F(Form("EnergyPrimBitsClustersDen_%02d",iBit),Form("EnergyPrimBitsClusters_%02d",iBit),100,0.,100000.);
    fHEnergyPrimBitsClustersEff[iBit] = new TH1F(Form("EnergyPrimBitsClustersEff_%02d",iBit),Form("Bit %02d efficiency",iBit),100,0.,100000.);
    fHEnergyPrimBitsClustersEff[iBit]->Sumw2();
    fHEnergyPrimBitsClustersEff[iBit]->SetMinimum(0.);
    fHEnergyPrimBitsClustersEff[iBit]->SetMaximum(1.05);
    fHEnergyPrimBitsClustersEff[iBit]->GetYaxis()->SetTitle("Efficiency");
    fHEnergyPrimBitsClustersEff[iBit]->GetXaxis()->SetTitle("Total Energy [MeV]");
  }
  Double_t DeltaTime = 40.;
  Int_t nTimeBins = ((Int_t)DeltaTime)*10; //200 ps resolution
  fHRecoHitTimeWrtReferenceVsCell = new TH2F
    ("RecoHitTimeWrtReferenceVsCell", "RecoHit-"+fT0ReferenceDetector+" Time vs RO Channel",
     16384, -0.5, 16383.5, nTimeBins, -DeltaTime, DeltaTime);
  fHRecoHitTimeWrtReferenceVsCell->GetXaxis()->SetTitle("RO channel number");
  fHRecoHitTimeWrtReferenceVsCell->GetYaxis()->SetTitle("Reconstructed hit time wrt "+fT0ReferenceDetector+" (ns)");

  fHRecoHitTimeWrtReferenceVsCellNoT0 = new TH2F
    ("RecoHitTimeWrtReferenceVsCellNoT0", "Uncorrected RecoHit-"+fT0ReferenceDetector+" Time vs RO Channel",
     16384, -0.5, 16383.5, nTimeBins, -DeltaTime, DeltaTime);
  fHRecoHitTimeWrtReferenceVsCellNoT0->GetXaxis()->SetTitle("RO channel number");
  fHRecoHitTimeWrtReferenceVsCellNoT0->GetYaxis()->SetTitle("Uncorrected hit time wrt "+fT0ReferenceDetector+" (ns)");
  LKrDir->mkdir("LKrL0Emulator");
}

void LKrReconstruction::SaveHistograms(){
  fHistoFile->cd("LKrMonitor");
  fHRecoHitTimeWrtReferenceVsCell->Write();
  fHRecoHitTimeWrtReferenceVsCellNoT0->Write();
  fHHitMap->Write();
  fHHitMapEnergy->Write();
  fHHitMapCrateSlot->Write();
  fHHitMapEnergyCrateSlot->Write();
  fHPedestal_xy->Write();
  fHSigma->Write();
  fHPedestal->Write();
  fHZSCounter->Write();
  fHZSCounterOOB->Write();
  fHMaxSample->Write();
  fHMaxSampleVsL0Mask->Write();
  fHADCCountLow->Write();
  fHADCCountAll->Write();
  fHNHits->Write();
  fHNClusters->Write();
  fHClusterEnergy->Write();
  fHClusterXY->Write();
  fHClusterTimeVsEnergy->Write();
  fHClusterTimeVsEnergy_over->Write();
  fHCellTotalEnergy->Write();
  fHCellTimeVsEnergy->Write();
  fHClusVsCellTotE->Write();
  fHCellXYEzero->Write();
  fHTotalEnergyVsL0Mask->Write();
  fHistoFile->cd("LKrMonitor/PrimBits");
  fHCellTotalEnergyVsPrimBits->Write();
  fHTotalEnergyVsPrimBits->Write();
  for(Int_t iBit=0;iBit<16;iBit++) {
    fHHitMapPrimBitsCellsNum[iBit]->Write();
    fHHitMapPrimBitsCellsDen[iBit]->Write();
    fHHitMapPrimBitsCellsEff[iBit]->Write();
    fHHitMapPrimBitsClustersNum[iBit]->Write();
    fHHitMapPrimBitsClustersDen[iBit]->Write();
    fHHitMapPrimBitsClustersEff[iBit]->Write();
    fHEnergyPrimBitsCellsNum[iBit]->Write();
    fHEnergyPrimBitsCellsDen[iBit]->Write();
    fHEnergyPrimBitsCellsEff[iBit]->Write();
    fHEnergyPrimBitsClustersNum[iBit]->Write();
    fHEnergyPrimBitsClustersDen[iBit]->Write();
    fHEnergyPrimBitsClustersEff[iBit]->Write();
  }

  // write LKrL0Emulator plots to output file
  fHistoFile->cd("LKrMonitor/LKrL0Emulator");
  fLKrL0Emulator->WriteHistograms();
  fHistoFile->cd("/");
}

UInt_t LKrReconstruction::GetPrimitiveBits(){
  const Int_t L0TPFineTimeBit = static_cast<NA62Reconstruction*>(fMainReco)->GetL0TPFineTimeBit();
  UInt_t primBits = 0;
  Double_t fineTriggerTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*ClockPeriod/256;
  for(UInt_t iSlot=0;iSlot<3;iSlot++){
    L0Primitive Primitive = NA62RecoManager::GetInstance()->GetEventHeader()->GetL0TPData()->GetPrimitive(iSlot,kL0Calo);
    UInt_t primWord     = Primitive.GetPrimitiveID(); //0 if not found
    if(!primWord) continue;
    Double_t primTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetL0TPData()->GetPrimitiveCorrectedFineTime(iSlot,kL0Calo,L0TPFineTimeBit)*ClockPeriod/256.;
    if(fabs(primTime-fineTriggerTime)<20.) primBits=primWord; //warning: hard-coded
  }
  return primBits;
}

void LKrReconstruction::ReadJitterFile(){
  TString JitterFileName = "LKr-Jitters.dat";
  TString Line;
  if(NA62ConditionsService::GetInstance()->Open(JitterFileName)!=kSuccess) return;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(JitterFileName))) {
    if (Line.BeginsWith("#")) continue;
    // Format: RunID BurstID NCheckedCREAMS NJitters <CrateID-SlotID NEntries MinRatio MaxRatio Amin Amax TShift>_0...<CrateID-SlotID...>_NJitters-1
    TObjArray *Tokens = Line.Tokenize("[ -]");
    Int_t NJitters = ((TObjString*)Tokens->At(3))->GetString().Atoi();
    for(int i=0; i<NJitters; i++){
      Int_t JitterCrate    = static_cast<TObjString*>(Tokens->At( 4+8*i))->GetString().Atoi();
      Int_t JitterSlot     = static_cast<TObjString*>(Tokens->At( 5+8*i))->GetString().Atoi();
      Double_t MinRatio = static_cast<TObjString*>(Tokens->At( 7+8*i))->GetString().Atof();
      if(MinRatio < 0.85 ) continue;
      Double_t JitterTime  = static_cast<TObjString*>(Tokens->At(11+8*i))->GetString().Atof();
      Int_t TimeSlotsShift = round(JitterTime/ClockPeriod);
      LKrJitter NewJitter;
      NewJitter.Crate = JitterCrate;
      NewJitter.Slot = JitterSlot;
      NewJitter.TimeSlotsShift = TimeSlotsShift;
      fJitters.push_back(NewJitter);
    }
    delete Tokens;
  }
  NA62ConditionsService::GetInstance()->Close(JitterFileName);
}

Double_t LKrReconstruction::GetJitterCorrection(TLKrDigi *Digi){
  for(UInt_t i=0; i<fJitters.size(); i++){
    Int_t ROChID = fRawDecoder->GetDecoder()->GetChannelRO(Digi->GetChannelID());
    Int_t CrateID = ROChID/512;
    Int_t SlotID = (ROChID%512)/32;
    SlotID = (SlotID<8) ? SlotID+3 : SlotID+5;
    if(CrateID==fJitters.at(i).Crate && SlotID==fJitters.at(i).Slot){
      return fJitters.at(i).TimeSlotsShift*ClockPeriod;
    }
  }
  return 0.0;
}
