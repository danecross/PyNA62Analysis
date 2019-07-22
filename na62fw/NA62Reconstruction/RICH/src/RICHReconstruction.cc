//-------------------------------------------------------------------------------
// Created by Antonio Cassese (antonio.cassese@cern.ch)
//
// Modified by Francesca Bucci (fbucci@cern.ch), Monica Pepe (monica.pepe@cern.ch)
// Single ring fit and mirror alignment reading added by Viacheslav Duk (Viacheslav.Duk@cern.ch)
//-------------------------------------------------------------------------------
/// \class RICHReconstruction
/// \Brief
/// Main RICH reconstruction: starts from RICH Hits and collects them in Time Clusters (group of hits near in time). Hits belonging to
/// Time Clusters are geometrically fitted as Rings. Time Clusters and Rings represent the RICH Candidates.
/// Digi hits come from single PMs or from SuperCell (SuperCell is the OR signal from a group of 8 adjacents PMs).
/// \EndBrief
///
/// \Detailed
/// It makes use of base class NA62VReconstruction and of classes in NA62MC/RICH/Persistency (TRecoRICHEvent, TRecoRICHCandidate, TRecoRICHHit).
/// The RICH reconstruction basic parameters are set in NA62Reconstruction/config/RICH.conf
/// \n
/// =========
/// \n
/// RICH.conf
/// \n
/// =========
/// \n
/// Description of parameters can be found in RICH.conf, here the parameters you may want to change to perform your own reconstruction:
/// \n
/// TimeWindow=10       --> used to build TimeCluster for hits inside TimeWindow width (+/- 0.5*TimeWindow)
/// \n
/// EnableSuperCells= 0 --> Enable (1) or disable (0) SuperCells Reconstruction : Digi hits from SC are skipped and not included in RICH Reconstructed Hits
/// \n
/// StationsT0 (ns) --> used to center Leading Time of TDC around 0.
/// \n
/// NOTE: signals from SC have LeadingTime shifted by 50 ns wrt PMs (2 time slots).
/// Its value depends on the detector used as reference time and is run dependent: until the information will be retrieved from DataBase,
/// values correspondng to different run ranges are shown in comments and must be used properly.
/// \n
/// In the analysis it may be useful to know the position of PMs and SCs:
/// Position of PMs (PMPosition_SC_i, i=0-121) and SuperCells (SCPosition_SC_i i=0-60) are x-y coordinates inside the PM Disk
/// reference system as a function of the Sequential Number of 976 PMs ans 122 SC in either Jura/Saleve Disk.
///  Real positions on each individual disk are obtained by adding the position of disk centers JuraShift and SaleveShift.
/// \n
/// =======================================================================================================================================================
/// \n
/// RICH Reconstruction
/// \n
/// The reconstruction rejects (if any) events with more tha 400 Digi hits: normally less than 250 Digi hits are present.
///    ****  Limit removed by Jurgen Engelfried, 10 August 2018. ***
/// Digi hits are then ordered to have first all hits from PMs and then hits from SuperCells.
/// Digi hits from SC are reconstructed only if the flag EnableSuperCells=1 in RICH.conf
/// \n
/// TimeCandidate
/// \n
/// Iterations of the time candidate clustering procedure (NCandidateClusteringIterations= 1 or 2)
/// \n
/// A cluster is defined collecting hits in 0.5*TimeWindow
/// \n
///   - 1: a new cluster is defined if a hit away more than 0.5*TimeWindow from the existing cluster is found
///   - 2: clusters closer than TimeWindow to a larger cluster are checked in order to include them in the existing larger cluster
///   .
/// This procedure is done first for SC (if enabled) and then for PMs hits: a total number NTimeCandidates of Time Candidates is
/// produced comprising NSCTimeCandidates (from SC hits, if enabled) and NPMTimeCandidates from PMs hits.
/// CandidateTime is the average time of hits belonging to that TimeCandidate
/// \n
/// Ring Candidate
/// \n
/// Rings are reconstructed from PMs Time Candidates only, even if SC reconstruction is enabled.
/// For each PMTimeCandidate:
/// \n
/// A single ring fit is performed to each PM Time Candidate and the Fit information is added to the PMTimeCandidate only for test purpose.
/// For analysis only the result of the MultiFit reconstruction must be used --> RingCandidates.
/// \n
/// =======================================================================================================================================================
/// \n
/// MultiRing Reconstruction:
/// \n
///
/// Start from PTOLEMY algorithm: choose a starting triplet of hits and loop on all other hits to find the best Ptolemy condition.
/// Starting triplet is taken from 8 different directions:
/// Left, Right, Down, Up, 45 Degree Left, 45 Degree Right, 45 Degree Down, 45 Degree Up.
/// Construct the first ring for each direction using the Ptolemy condition and adding points that lie on the fitted ring.
/// Choose a new starting triplet with the remaining points and construct the second ring.
/// Repeat procedure until there are less than 4 points or no good starting triplet left.
/// Choose the best (Best Chi2) multiring sample among the 8. (One or more rings can be obtained from a single TimeCandidate).
/// A total number of NRingCandidates is found. For each Ring Candidate the RingTime is the average time of its hits.
/// \n
/// RICH Candidates array contains : (SCTimeCandidates + PMTimeCandidates) + RingCandidates
/// following the above order and where (NSCTimeCandidates + NPMTimeCandidates)=NTimeCandidates
/// \n
/// All methods to retrieve the number of candidates and the Candidate arrays are implemented in TRecoRICHEvent:
/// \code
/// Int_t             GetNTimeCandidates()                              { return fNTimeCandidates;              };
/// Int_t             GetNRingCandidates()                              { return fNRingCandidates;              };
/// Int_t             GetNPMTimeCandidates()                            { return fNPMTimeCandidates;            };
/// Int_t             GetNSCTimeCandidates()                            { return fNSCTimeCandidates;            };
/// TRecoRICHCandidate * GetSCTimeCandidate(Int_t);
/// TRecoRICHCandidate * GetPMTimeCandidate(Int_t);
/// TRecoRICHCandidate * GetTimeCandidate(Int_t);
/// TRecoRICHCandidate * GetRingCandidate(Int_t);
/// \endcode
/// \n
/// An example of code to be used in NA62UserAnalysis:
/// \n
/// \code
///    TRecoRICHEvent * RICHEvent = static_cast<TRecoRICHEvent*>(GetEvent("RICH"));
///
///    for(int iRICHCand=0; iRICHCand<RICHEvent->GetNPMTimeCandidates(); iRICHCand++){ //loop su PM Time Cand
///      TRecoRICHCandidate *TimeCandidate = RICHEvent->GetPMTimeCandidate(iRICHCand);
///      TimeCandidate->SetEvent(RICHEvent);
///      FillHisto("hTimePMTCandidate",TimeCandidate->GetTime());
///    }
///
///    for(int iRICHCand=0; iRICHCand<RICHEvent->GetNSCTimeCandidates(); iRICHCand++){ //loop su SC Time Cand
///      TRecoRICHCandidate *TimeCandidate = RICHEvent->GetSCTimeCandidate(iRICHCand);
///      TimeCandidate->SetEvent(RICHEvent);
///      FillHisto("hTimeSCTCandidate",TimeCandidate->GetTime());
///    }
///
///    for(int iRICHCand=0; iRICHCand<RICHEvent->GetNRingCandidates(); iRICHCand++){ //loop su Ring Cand
///      TRecoRICHCandidate *RingCandidate = RICHEvent->GetRingCandidate(iRICHCand);
///      RingCandidate->SetEvent(RICHEvent);
///      FillHisto("hRICHRingTime",RingCandidate->GetRingTime());
///      FillHisto("hRICHRingRadius",RingCandidate->GetRingRadius());
///      Int_t nHits = RingCandidate->GetNHits();
///      TVector3 HitPosOnDisk, HitPosForRingFit;
///      for(Int_t ihit=0; ihit<nHits;ihit++) {
///         TRecoRICHHit *Hit = static_cast<TRecoRICHHit *>( RingCandidate->GetHit(ihit));
///         FillHisto("hTimeHitRCandidate",Hit->GetTime());
///
///         HitPosOnDisk = Hit->GetPosition();  // hit position on Jura or Saleve Disk
///         HitPosForRingFit = Hit->GetFitPosition();  // hit position on combined disk used to fit the Ring
///
///         Int_t GeoID  = Hit->GetChannelID(); // Encoded RICHChannelID
///         Int_t SeqID  = Hit->GetChannelSeqID(); // Sequential Channel ID (PM seq number)
///         Int_t DiskID = Hit->GetDiskID(); // 0=Jura 1=Saleve : Disk where the PM is located
///         Int_t UpDownOnDisk = Hit->GetUpDownDiskID(); // 0=Up , 1=Down : Pm on upper or lower part of Disk
///         Int_t ORsc = Hit->GetOrSuperCellID() ; // Flag = 0 for PMs , 1 for SC
///      } // end loop hits in ring
///    }  //loop on Ring Cand
///
/// \endcode
/// \author Created by Antonio Cassese (antonio.cassese@cern.ch). Modified by Francesca Bucci (fbucci@cern.ch), Monica Pepe (monica.pepe@pg.infn.it)
/// T0 and Slewing correction modified by Roberta Volpe (roberta.volpe@cern.ch)
/// Single ring fit and mirror alignment reading developed by Viacheslav Duk (Viacheslav.Duk@cern.ch)
/// \EndDetailed

#include "Riostream.h"
#include "TString.h"
#include "TRegexp.h"

#include "NA62Reconstruction.hh"
#include "NA62RecoManager.hh"
#include "NA62ConditionsService.hh"
#include "RICHReconstruction.hh"
#include "RICHChannel.hh"
#include "TRecoRICHEvent.hh"
#include "TSlimRecoRICHEvent.hh"
#include "TRecoRICHHit.hh"
#include "TRICHDigi.hh"
#include "TDCBRawDecoder.hh"
#include "TRICHHit.hh"

#include "CLHEP/Units/SystemOfUnits.h"
using namespace CLHEP;

std::vector<TVector2> fHitPosForChiFit;
std::vector<Double_t> fHitTimeForIterationFit;
std::vector<TVector2> fHitPosForIterationFit;
std::vector<Int_t> fHitIndexForIterationFit;
Double_t fRingRadiusIterationFit;
Double_t fRingRadiusErrorIterationFit;
TVector2 fRingCenterIterationFit;
TVector2 fRingCenterErrorIterationFit;
Double_t fRingChi2IterationFit;

/**********************************************//**
 * Constructor called in
 * src/NA62Reconstruction.cc
 ************************************************/

RICHReconstruction::RICHReconstruction(TFile * HistoFile, TString ConfigFileName) :
  NA62VReconstruction(HistoFile, "RICH", ConfigFileName),
  fCandidate(nullptr),
  fSpotCenterSaleve(0.,0.),
  fSpotCenterJura(0.,0.),
  fAngleRotationSaleve(0.,0.),
  fAngleRotationJura(0.,0.),
  fSpotCenter(0.,0.),
  fMirrorAlignmentDBName(""),
  fHRecoHitTimeWrtReferenceVsSeqChannel(nullptr),
  fHRecoHitTimeWrtReferenceVsSeqChannelNoT0(nullptr)
{
  // Initialize the variables
  fRecoEvent = new TRecoRICHEvent();
  fSlimRecoEvent = new TSlimRecoRICHEvent();
  fPreviousEventCandidateTime = 0;
  fNSCChannels = 0;
  fEnableSuperCells = kFALSE;
  fEnableSlewingCorr = kTRUE;
  fEvaluateSlewingCorr = kFALSE;
  fEdgeRequirement     = 3;
  fNCandidateClusteringIterations = 0;
  fMultiRingRecoFlag = kTRUE;
  fnRICHEvent=0;
  fPMsPositions = 0;
  fPMsPositionsShifted = 0;
  // fAlignEnabled        = kTRUE;

  fHitPos.clear();
  fPtolHitPos = new TVector2[4];
  for(UInt_t i=0;i<4;i++) fPtolHitPos[i] = TVector2(0.,0.);
  fPtolHitChID.resize(3);

  // Take inputs from config file
  ParseConfFile(ConfigFileName);

  //VARIABLES FOR FIT
  fNPars = 3; // Parameters for fit
  fFitter = new TMinuit(fNPars);
  fFitter->SetFCN(RingChi2FCN);

  fLoadAlignmentFlag = true;
}

/********************************************//**
 * Initialization called in
 * src/NA62Reconstruction.cc
 * in method
 * void NA62Reconstruction::InitDetectorsInfo()
 ************************************************/

void RICHReconstruction::Init(NA62VReconstruction* MainReco) {

  //common part for all the subdetectors
  NA62VReconstruction::Init(MainReco);

  //force the initialisation of the channel histos if general T0 evaluation is enabled
  if(static_cast<NA62Reconstruction *>(fMainReco)->GetFillTimesEnabled() &&  fEvaluateSlewingCorr ) fChannelHistograms = kTRUE;

  for (Int_t ROID = 0; ROID < static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetNROChannels(); ROID++){
    Int_t PositionID = fRawDecoder->GetDecoder()->GetChannelRemap(ROID);
    if (PositionID >=0 ){
      Int_t SeqID = FromGeoIDtoSeqID(PositionID);
      fChannels[SeqID] = new RICHChannel(PositionID,ROID,SeqID,fChannelHistograms,fPMsPositions[SeqID],fPMsPositionsShifted[SeqID]);
    }
  }

  ReadT0s();
  ReadSlewingCorrections(); //for all PMTs+SC channels
  InitHistograms();
}

/********************************************//**
 * Destructor
 ************************************************/

RICHReconstruction::~RICHReconstruction(){
  if(fPMsPositions) {
    delete [] fPMsPositions;
    fPMsPositions=0;
  }
  if(fPMsPositionsShifted) {
    delete [] fPMsPositionsShifted;
    fPMsPositionsShifted=0;
  }
  if(fFitter){
    delete fFitter;
    fFitter=0;
  }
  if(fPtolHitPos){
    delete [] fPtolHitPos;
    fPtolHitPos=0;
  }
}

/********************************************//**
 * Read the config file
 ************************************************/

void RICHReconstruction::ParseConfFile(TString ConfFileName){

  std::ifstream confFile(ConfFileName.Data());
  if (!confFile.is_open()){
    perror(ConfFileName);
    exit(kWrongConfiguration);
  }

  TString Line;
  while (Line.ReadLine(confFile)){
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("NSCChannels")){
      fNSCChannels = TString(Line(TRegexp("[0-9]+"))).Atoi();
      fPMsPositions = new TVector2[fNChannels];
      fPMsPositionsShifted = new TVector2[fNChannels];
      for(Int_t iCh=0; iCh<fNChannels;iCh++){
        fPMsPositions[iCh] = TVector2(0.,0.);
        fPMsPositionsShifted[iCh] = TVector2(0.,0.);
      }
      continue;
    }
    else if (Line.BeginsWith("EdgeRequirement")){
      fEdgeRequirement = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("TimeWindow")){
      fTimeWindow = TString(Line(TRegexp("[0-9]+"))).Atof();
      continue;
    }
    else if (Line.BeginsWith("SaleveShift")){
      TObjArray* l = Line.Tokenize(" ");
      fSpotCenterSaleve = TVector2(static_cast<TObjString*>(l->At(1))->GetString().Atof(),
          static_cast<TObjString*>(l->At(2))->GetString().Atof());
      delete l;
      continue;
    }
    else if (Line.BeginsWith("JuraShift")){
      TObjArray* l = Line.Tokenize(" ");
      fSpotCenterJura = TVector2(static_cast<TObjString*>(l->At(1))->GetString().Atof(),
          static_cast<TObjString*>(l->At(2))->GetString().Atof());
      delete l;
      continue;
    }
    else if (Line.BeginsWith("SaleveRotation")){
      TObjArray* l = Line.Tokenize(" ");
      fAngleRotationSaleve = TVector2(static_cast<TObjString*>(l->At(1))->GetString().Atof(),
          static_cast<TObjString*>(l->At(2))->GetString().Atof());
      delete l;
      continue;
    }
    else if (Line.BeginsWith("JuraRotation")){
      TObjArray* l = Line.Tokenize(" ");
      fAngleRotationJura = TVector2(static_cast<TObjString*>(l->At(1))->GetString().Atof(),
          static_cast<TObjString*>(l->At(2))->GetString().Atof());
      delete l;
      continue;
    }
    else if (Line.BeginsWith("MirrorAlignmentDBName")){
      TObjArray *l = Line.Tokenize(" ");
      fMirrorAlignmentDBName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if (Line.BeginsWith("FocalLength")){
      fFocalLength = TString(Line(TRegexp("[0-9]+"))).Atof();
      continue;
    }

    if (Line.BeginsWith("PMPosition_SC_")){
      for(int i = 0; i < (Int_t)(((fNChannels-fNSCChannels)/2)/8); i++){
        if(Line.BeginsWith(Form("PMPosition_SC_%d=",i))){
          TObjArray * l = Line.Tokenize(" ");
          for(int j = 0; j < 8; j++){
            Double_t x = static_cast<TObjString*>(l->At(2*j+1))->GetString().Atof();
            Double_t y = static_cast<TObjString*>(l->At(2*j+2))->GetString().Atof();
            fPMsPositions[8*i+j] = TVector2(x,y);
            fPMsPositions[8*i+j+((fNChannels-fNSCChannels)/2)] = TVector2(x,y);
            fPMsPositionsShifted[8*i+j] = fPMsPositions[8*i+j]+fSpotCenterJura;
            fPMsPositionsShifted[8*i+j+((fNChannels-fNSCChannels)/2)] = fPMsPositions[8*i+j]+fSpotCenterSaleve;
          }
          delete l;
        }
      }
    }
    else if (Line.BeginsWith("SCPosition_SC_")){
      for(int i = 0; i < (Int_t)((fNSCChannels/2)/2); i++){
        if(Line.BeginsWith(Form("SCPosition_SC_%d=",i))){
          Int_t iSC_0=1952;
          TObjArray * l = Line.Tokenize(" ");
          for(int j = 0; j < 2; j++){
            Double_t x = static_cast<TObjString*>(l->At(2*j+1))->GetString().Atof();
            Double_t y = static_cast<TObjString*>(l->At(2*j+2))->GetString().Atof();
            fPMsPositions[iSC_0+2*i+j] = TVector2(x,y);
            fPMsPositions[iSC_0+2*i+j+(fNSCChannels/2)] = TVector2(x,y);
            fPMsPositionsShifted[iSC_0+2*i+j] = fPMsPositions[iSC_0+2*i+j]+fSpotCenterJura;
            fPMsPositionsShifted[iSC_0+2*i+j+(fNSCChannels/2)] = fPMsPositions[iSC_0+2*i+j]+fSpotCenterSaleve;
          }
          delete l;
        }
      }
    }
    else if (Line.BeginsWith("NCandidateClusteringIterations")){
      fNCandidateClusteringIterations = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("EnableSuperCells")){
      fEnableSuperCells = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("EnableSlewingCorrection")){
      fEnableSlewingCorr = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("EvaluateSlewingCorrection")){
      fEvaluateSlewingCorr = TString(Line(TRegexp("[0-1]"))).Atoi();
      continue;
    }
    else if (Line.BeginsWith("MinPMsForEvent")){
      fMinPMsForEvent = TString(Line(TRegexp("[0-9]+"))).Atof();
      continue;
    }
    //    else if (Line.BeginsWith("EnableMultiRingReco")){
    //      fMultiRingRecoFlag = TString(Line(TRegexp("[0-1]"))).Atoi();
    //      continue;
    //    }
    else if (Line.BeginsWith("RingRecoType")){
      Line.Remove(0,14);
      TString Type = TString(Line(TRegexp("[A-Z-a-z-0-9]+")));
      if (Type.BeginsWith("Chi2")){
        fFlagChi2 = kTRUE;
        continue;
      }
      continue;
    }
    else if (Line.BeginsWith("ChargeThreshold")){
      fChargeThreshold = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
      continue;
    }
    else if (Line.BeginsWith("TimeWidthSigma")){
      fTimeWidthSigma = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof()*ns;
      continue;
    }
    else if (Line.BeginsWith("WidthConstant")){
      fWidthConstant = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof()*ns;
      continue;
    }
    else if (Line.BeginsWith("MinWidth")){
      fMinWidth = TString(Line(TRegexp("[0-9]+"))).Atof();
      continue;
    }
    else if (Line.BeginsWith("MaxWidth")){
      fMaxWidth = TString(Line(TRegexp("[0-9]+"))).Atof();
      continue;
    }
    else if (Line.BeginsWith("NHitsForT0")){
      Line.Remove(9,1);
      fNHitsForT0 = TString(Line(TRegexp("[0-9]+"))).Atof();
      continue;
    }
    else if (Line.BeginsWith("SlewingCorrectionFileInput")){
      TObjArray *l = Line.Tokenize(" ");
      fSlewingFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
    else if(Line.BeginsWith("MinHitForMulti")){
      fMinHitForMulti = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
    else if(Line.BeginsWith("MinTripDist")){
      fMinTripDist = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
    else if(Line.BeginsWith("MinHitAssociated")){
      fMinHitAssociated = TString(Line(TRegexp("[0-9]+"))).Atoi();
      continue;
    }
    else if(Line.BeginsWith("PtolemyCondition")){
      fPTolCondition = TString(Line(TRegexp("[0-9]+"))).Atof();
      continue;
    }
  }
  confFile.close();

  if (fEnableSuperCells){
    std::cout << "RICH Reconstruction for SuperCells enabled" << std::endl;
  }
  else {
    std::cout << "RICH Reconstruction for SuperCells disabled" << std::endl;
  }

  if (abs(fEdgeRequirement)>3) {
    std::cout << "ERROR: RICHReconstruction invalid edge requirement"<< fEdgeRequirement << std::endl;
    exit(kWrongConfiguration);
  }

  if (fEvaluateSlewingCorr) fChannelHistograms = kTRUE;
  //if (fEvaluateSlewingCorr) fEnableSlewingCorr = kFALSE; // disable existing slewing correction

  if (!fSlewingFileName.Length())
  {
    std::cout << "Error: RICH slewing correction file not defined" << std::endl;
    exit(kWrongConfiguration);
  }
  if (fTimeWindow<=0)
  {
    std::cout << "Error: RICH reconstruction time window <= 0" << std::endl;
    exit(kWrongConfiguration);
  }
}

/********************************************//**
 * Read slewing corrections
 ************************************************/

void RICHReconstruction::ReadSlewingCorrections(){

  NA62ConditionsService::GetInstance()->Open(fSlewingFileName);

  // ------------------------------------------------------------------------
  // The SlewingCorr.dat file has 5 column:
  //  SeqID      MinWidth        MaxWidth        SC_MinWidth     SC_MaxWidth
  //-------------------------------------------------------------------------
  TString Line;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fSlewingFileName))){
    if (Line.BeginsWith("#")) continue;
    else{
      if(!fEnableSlewingCorr) continue;
      TObjArray *l = Line.Tokenize(" ");

      Int_t Ch = static_cast<TObjString*>(l->At(0))->GetString().Atoi();
      Double_t sc_wmin = static_cast<TObjString*>(l->At(6))->GetString().Atof();
      Double_t sc_wmax = static_cast<TObjString*>(l->At(8))->GetString().Atof();
      Double_t wmin = static_cast<TObjString*>(l->At(2))->GetString().Atof();
      Double_t wmax = static_cast<TObjString*>(l->At(4))->GetString().Atof();

      Double_t meanwidth = 0.5*(wmin+wmax);
      if (0<=Ch && Ch < fNChannels) {
        static_cast<RICHChannel*>(fChannels[Ch])->SetSlewingCorrectionParameters(wmin,wmax,sc_wmin,sc_wmax);
        static_cast<RICHChannel*>(fChannels[Ch])->SetMeanWidth(meanwidth);
      }
      delete l;
      continue;
    }
  }
  NA62ConditionsService::GetInstance()->Close(fSlewingFileName);
}

/********************************************//**
 * Trigger algorithm
 ************************************************/

TDetectorVEvent * RICHReconstruction::Trigger(TDetectorVEvent * tEvent, Event* /*tGenEvent*/)
{
  if(tEvent->IsA() != TDCEvent::Class())
    NA62VReconstruction::Exception(Form("%s is not a %s", tEvent->IsA()->GetName(), TDCEvent::Class()->GetName()));
  return tEvent;
}

/********************************************//**
 * Operations at the begin of each Burst
 ************************************************/

void RICHReconstruction::StartOfBurst() {
  NA62VReconstruction::StartOfBurst(); // common part for all the subdetectors
}

void RICHReconstruction::LoadAlignmentParameters(Int_t RunNumber) {
  Double_t fExtraOffsetX[2] = {0., 0.};
  Double_t fExtraOffsetY[2] = {0., 0.};

  if (NA62ConditionsService::GetInstance()->Open(fMirrorAlignmentDBName)!=kSuccess) {
    std::cout << "[RICHReconstruction] Error: mirror alignment file " << fMirrorAlignmentDBName << " not found anywhere or empty" << std::endl;
    exit(kConditionFileNotFound);
  }
  TString Line;
  Bool_t RunNotFound = true;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fMirrorAlignmentDBName)) && RunNotFound) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    Int_t FirstRun = static_cast<TObjString*>(l->At(0))->GetString().Atoi();
    Int_t LastRun =  static_cast<TObjString*>(l->At(1))->GetString().Atoi();
    delete l;
    if (RunNumber<FirstRun || RunNumber>LastRun) { // Not this entry
      Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fMirrorAlignmentDBName));
      Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fMirrorAlignmentDBName));
      Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fMirrorAlignmentDBName));
    }
    else { // Here we really read!
      RunNotFound = false;
      Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fMirrorAlignmentDBName));
      TObjArray *l = Line.Tokenize(" ");
      // read Jura (x, y) and Saleve (x, y) offsets
      fExtraOffsetX[0] = static_cast<TObjString*>(l->At(0))->GetString().Atof();
      fExtraOffsetY[0] = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      fExtraOffsetX[1] = static_cast<TObjString*>(l->At(2))->GetString().Atof();
      fExtraOffsetY[1] = static_cast<TObjString*>(l->At(3))->GetString().Atof();
      std::cout << "[RICHReconstruction] Mirror alignment global offsets (JuraX, JuraY, SaleveX, SaleveY) " <<
        fExtraOffsetX[0] << " " << fExtraOffsetY[0] << " " << fExtraOffsetX[1] << " " << fExtraOffsetY[1] << std::endl;
      // skip the 1st line with residual mirror alignment (X coordinates)
      Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fMirrorAlignmentDBName));
      // skip the 2nd line with residual mirror alignment (Y coordinates)
      Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fMirrorAlignmentDBName));
      delete l;
    }
  }
  NA62ConditionsService::GetInstance()->Close(fMirrorAlignmentDBName);

  if (!RunNotFound) {
    std::cout << "[RICHReconstruction] Mirror alignment constants (data) loaded for run " << RunNumber << std::endl;
  }
  else {
    std::cout <<  "[RICHReconstruction] Run " << RunNumber << " not found in mirror alignment database "
      << fMirrorAlignmentDBName << std::endl;
    exit(kGenericError);
  }

  // correct the mirror mislignment offsets
  TVector2 JuraOffset(fExtraOffsetX[0], fExtraOffsetY[0]);
  TVector2 SaleveOffset(fExtraOffsetX[1], fExtraOffsetY[1]);
  fAngleRotationJura = fAngleRotationJura + JuraOffset;
  fAngleRotationSaleve = fAngleRotationSaleve + SaleveOffset;
}

/********************************************//**
 * Operations at the end of each burst
 ************************************************/

void RICHReconstruction::EndOfBurst() {
  NA62VReconstruction::EndOfBurst(); // common part for all the subdetectors
}

/********************************************//**
 * Time clusterization of the hits and
 * candidates first definition
 ************************************************/

TRecoVEvent * RICHReconstruction::ProcessEvent(TDetectorVEvent* tEvent, Event* tGenEvent){

  fnRICHEvent++;
  if (tEvent->IsA() == TSpecialTriggerEvent::Class())
    return 0;

  //common part for all the subdetectors
  NA62VReconstruction::ProcessEvent(tEvent, tGenEvent);

  // load mirror alignment offsets
  Int_t fRunID  = NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID();
  if(fLoadAlignmentFlag){
    LoadAlignmentParameters(fRunID);
    fLoadAlignmentFlag = false;
  }

  for (Int_t iCh = 0; iCh < fNChannels; iCh++) // PMs + SC
    fChannels[iCh]->Reset();


  TDCEvent * TdcEvent = static_cast<TDCEvent*>(tEvent);
  TDCEventMonitor(TdcEvent);

  Int_t nDigis = TdcEvent->GetNHits();
  fHNTotHits->Fill(nDigis);

  TClonesArray& Digis = (*(TdcEvent->GetHits()));

  Int_t nPMJ, nPMS, nSCJ, nSCS ; // count digi hits for PM and SC
  nPMJ = nPMS = nSCJ = nSCS = 0;
  for (Int_t iDigi = 0; iDigi < nDigis; iDigi++)
  {
    TRICHDigi *Digi = static_cast<TRICHDigi*>( Digis[iDigi]);
    if(!Digi->GetOrSuperCellID()) { // PMTs
      if (Digi->GetDiskID() == kJura) {
        nPMJ++;
      }else{ // if Saleve
        nPMS++;
      }
    }
    if(Digi->GetOrSuperCellID()){ // SC
      if (Digi->GetDiskID() == kJura) {
        nSCJ++;
      }else{ // if Saleve
        nSCS++;
      }
    }
  }
  fHNHitsPMJura->Fill(nPMJ);
  fHNHitsPMSaleve->Fill(nPMS);
  fHNHitsSCJura->Fill(nSCJ);
  fHNHitsSCSaleve->Fill(nSCS);

  if (!nDigis)
    return fRecoEvent;


  Digis.Sort(); //to order Digis: first digis from PM, then digis from SC

  Double_t FitPositionX,FitPositionY;
  FitPositionX=FitPositionY=9999.;

  Int_t nHitsPMJ, nHitsPMS, nHitsSCJ, nHitsSCS;
  nHitsPMJ = nHitsPMS = nHitsSCJ = nHitsSCS = 0;

  // Reconstruct PMT hit times: Digis --> RecoHits
  for (Int_t iDigi = 0; iDigi < nDigis; iDigi++)
  {
    TRICHDigi *Digi = static_cast<TRICHDigi*>( Digis[iDigi]);
    Int_t ChannelID = Digi->GetChannelID();
    Int_t SeqChannelID = Digi->GetChannelSeqID();
    Int_t ROCh = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(ChannelID);

    // Add SuperCells digi only if Enabled:
    if (!fEnableSuperCells && Digi->GetOrSuperCellID()) break; // exit when 1st SC is found

    fChannels[SeqChannelID]->AddHit();

    fHHitStatus->Fill(Digi->GetDetectedEdge());
    //
    // Edge Requirement for the hit reconstruction:
    //  1 = at least one leading,  2 = at least one trailing, 3 = leading+trailing (default)
    //  0 = every possible hit (at least one leading or at least one trailing)
    // -1 = leading only,         -2 = trailing only
    //

    Bool_t EdgeRequirement = (Digi->GetDetectedEdge()==3 || Digi->GetDetectedEdge()==fEdgeRequirement);

    if      (fEdgeRequirement<0) EdgeRequirement = (Digi->GetDetectedEdge()==abs(fEdgeRequirement));
    else if (fEdgeRequirement==0)  EdgeRequirement = kTRUE;  // reco for all hits

    if (!EdgeRequirement && Digi->GetOrSuperCellID()!=1) continue; //PM hits not matching required edge condition are not reconstructed

    if (Digi->GetDetectedEdge()>1) fHTrailingTime->Fill(Digi->GetTrailingEdge());
    if (Digi->GetDetectedEdge()==1 || Digi->GetDetectedEdge()==3) fHLeadingTimeVsSlot->Fill(Digi->GetSlot(), Digi->GetLeadingEdge() - GetT0Correction(Digi));
    if (Digi->GetDetectedEdge()>1) fHTrailingTimeVsSlot->Fill(Digi->GetSlot(), Digi->GetTrailingEdge() - GetT0Correction(Digi));

    Double_t RecoWidth = GetRecoTimeWidth(Digi);
    Double_t RecoTime = GetRecoTime(Digi);

    TRecoRICHHit* RecoHit = static_cast<TRecoRICHHit*>( fRecoEvent->AddHit(Digi));
    RecoHit->SetChannelID(Digi->GetChannelID()); //Patch to be removed
    RecoHit->DecodeChannelID();

    //  PMs con ID= 0-1951 e  Supercelle con ID = 1952-2195
    RecoHit->SetHitQuality(Digi->GetDetectedEdge());
    RecoHit->SetROChannelID(ROCh);
    RecoHit->SetTimeWidth(RecoWidth);
    RecoHit->SetTime(RecoTime);
    FitPositionX=static_cast<RICHChannel*>(fChannels[RecoHit->GetChannelSeqID()])->GetCenteredPosition().X()-GetChPosAngCorr(RecoHit->GetChannelSeqID()).X();
    FitPositionY=static_cast<RICHChannel*>(fChannels[RecoHit->GetChannelSeqID()])->GetCenteredPosition().Y()-GetChPosAngCorr(RecoHit->GetChannelSeqID()).Y();
    RecoHit->SetPosition(TVector3(static_cast<RICHChannel*>(fChannels[RecoHit->GetChannelSeqID()])->GetCenteredPosition().X(),static_cast<RICHChannel*>(fChannels[RecoHit->GetChannelSeqID()])->GetCenteredPosition().Y(),0));
    RecoHit->SetFitPosition(TVector3(FitPositionX,FitPositionY,0));

    //    fHOccupancy->Fill(RecoHit->GetChannelSeqID());
    fHROOccupancy->Fill(RecoHit->GetROChannelID());

    // Illumination
    if (!RecoHit->GetOrSuperCellID()){ // only PMTs
      fHWidth->Fill(RecoHit->GetTimeWidth());
      fHHitWidthvsChannel->Fill(RecoHit->GetChannelSeqID(),RecoHit->GetTimeWidth());
      fHHitWidthvsROChannel->Fill(RecoHit->GetROChannelID(),RecoHit->GetTimeWidth());
      if (Digi->GetDetectedEdge() & 1) {
        // fHHitLeadingTimevsChannel->Fill(RecoHit->GetChannelSeqID(),Digi->GetLeadingEdge()-GetT0Correction(Digi));
        fHHitLeadingTimevsROChannel->Fill(RecoHit->GetROChannelID(),Digi->GetLeadingEdge()-GetT0Correction(Digi));
      }
      if(RecoHit->GetDiskID() == kSaleve){// if Saleve
        nHitsPMS++;
        // fHOccupancySaleve->Fill(RecoHit->GetChannelSeqID());
        fHROOccupancySaleve->Fill(RecoHit->GetROChannelID());

        fHPMTIlluminationSaleve->Fill(static_cast<RICHChannel*>(fChannels[RecoHit->GetChannelSeqID()])->GetCenteredPosition().X(),static_cast<RICHChannel*>(fChannels[RecoHit->GetChannelSeqID()])->GetCenteredPosition().Y());

      }else{ //if Jura
        nHitsPMJ++;
        // fHOccupancyJura->Fill(RecoHit->GetChannelSeqID());
        fHROOccupancyJura->Fill(RecoHit->GetROChannelID());
        fHPMTIlluminationJura->Fill(static_cast<RICHChannel*>(fChannels[RecoHit->GetChannelSeqID()])->GetCenteredPosition().X(),static_cast<RICHChannel*>(fChannels[RecoHit->GetChannelSeqID()])->GetCenteredPosition().Y());
      }
    }

    if (RecoHit->GetOrSuperCellID()){ //only SC
      fHSuperCellIllumination->Fill(static_cast<RICHChannel*>(fChannels[RecoHit->GetChannelSeqID()])->GetCenteredPosition().X() - GetChPosAngCorr(RecoHit->GetChannelSeqID()).X(),static_cast<RICHChannel*>(fChannels[RecoHit->GetChannelSeqID()])->GetCenteredPosition().Y());

      fHSCWidthvsChannel->Fill(RecoHit->GetChannelSeqID(),RecoHit->GetTimeWidth());
      fHSCWidthvsROChannel->Fill(RecoHit->GetROChannelID(),RecoHit->GetTimeWidth());
      if (Digi->GetDetectedEdge() & 1) {
        // fHSCLeadingTimevsChannel->Fill(RecoHit->GetChannelSeqID(),Digi->GetLeadingEdge()-GetT0Correction(Digi));
        fHSCLeadingTimevsROChannel->Fill(RecoHit->GetROChannelID(),Digi->GetLeadingEdge()-GetT0Correction(Digi));
      }
      if(RecoHit->GetDiskID() == kSaleve){// if Saleve
        nHitsSCS++;
        // fHSuperCellOccupancySaleve->Fill(RecoHit->GetChannelSeqID());
        fHSuperCellROOccupancySaleve->Fill(RecoHit->GetROChannelID());
        fHSuperCellIlluminationSaleve->Fill(static_cast<RICHChannel*>(fChannels[RecoHit->GetChannelSeqID()])->GetCenteredPosition().X(),static_cast<RICHChannel*>(fChannels[RecoHit->GetChannelSeqID()])->GetCenteredPosition().Y());
      }else{// if Jura
        nHitsSCJ++;
        // fHSuperCellOccupancyJura->Fill(RecoHit->GetChannelSeqID());
        fHSuperCellROOccupancyJura->Fill(RecoHit->GetROChannelID());
        fHSuperCellIlluminationJura->Fill(static_cast<RICHChannel*>(fChannels[RecoHit->GetChannelSeqID()])->GetCenteredPosition().X(),static_cast<RICHChannel*>(fChannels[RecoHit->GetChannelSeqID()])->GetCenteredPosition().Y());
      }
    }
  }// end loop on Digi

  Int_t nHits = fRecoEvent->GetNHits();

  if (!nHits)
    return fRecoEvent;
  fHNRecoHits->Fill(nHits);
  fHNRecoHitsPMJ->Fill(nHitsPMJ);
  fHNRecoHitsPMS->Fill(nHitsPMS);
  fHNRecoHitsSCJ->Fill(nHitsSCJ);
  fHNRecoHitsSCS->Fill(nHitsSCS);
  // RecoHits time clusterization: 1 cluster = 1 candidate

  ReconstructCandidates(fRecoEvent);
  Int_t nSCCand = 0;
  nSCCand = fRecoEvent->GetNCandidates();
  //*****************************************//
  ReconstructPMCandidates(fRecoEvent);

  //**********************************************//

  Int_t nCand = 0;
  nCand = fRecoEvent->GetNCandidates();

  Int_t nPMCand = 0;
  nPMCand = nCand-nSCCand;
  fHNTimeCandidates->Fill(nPMCand);
  if(!nCand) return fRecoEvent;

  // Evaluate hit - Candidate time distribution only for PMT
  for (Int_t iCandidate = nSCCand; iCandidate < nCand; iCandidate++)
  {
    TRecoRICHCandidate *Candidate = static_cast<TRecoRICHCandidate *>( fRecoEvent->GetCandidate(iCandidate));
    Double_t CandidateTime = Candidate->GetTime();

    fHNHitperTimeCandidate->Fill(Candidate->GetNHits());
    if(Candidate->GetNHits()>1){
      for (Int_t iHit = 0; iHit < Candidate->GetNHits(); iHit++)
      {
        TRecoRICHHit *Hit = static_cast<TRecoRICHHit *>( Candidate->GetHit(iHit));
        Double_t TimeDiff = Hit->GetTime() - CandidateTime;
        fHHitCandidateTimeDiff->Fill(TimeDiff);
        fHHitCandidateTimeDiffvsChannel->Fill(Hit->GetChannelSeqID(),TimeDiff);
        fHHitCandidateTimeDiffvsROChannel->Fill(Hit->GetROChannelID(),TimeDiff);
        fHHitCandidateTimeDiffvsWidth->Fill(Hit->GetTimeWidth(),TimeDiff);
      }
    }
  }// end of Hit Time-Candidate Time for PM

  // Ring reconstruction (for each good PM candidate)

  for (Int_t iCand = nSCCand; iCand < nCand; iCand++)
  {

    TRecoRICHCandidate *Candidate = static_cast<TRecoRICHCandidate*>( fRecoEvent->GetCandidate(iCand));
    if (Candidate->GetNHits() < fMinPMsForEvent) continue;

    RingFit(Candidate);

    if ( Candidate->GetRingRadius() > 0. ) fHSingleRingTime->Fill(Candidate->GetRingTime());

  }

  // iteration single ring fit for PM time candidates
  for (Int_t iCandidate = nSCCand; iCandidate < nCand; iCandidate++)
  {
    TRecoRICHCandidate *ThisCandidate = static_cast<TRecoRICHCandidate*>( fRecoEvent->GetCandidate(iCandidate));
    // check if this candidate has enough hits
    if (ThisCandidate->GetNHits() < fMinPMsForEvent) continue;

    // fill arrays (hit position, hit time)
    fHitPosForIterationFit.clear();
    fHitTimeForIterationFit.clear();
    fHitIndexForIterationFit.clear();
    for (Int_t iHit = 0; iHit < ThisCandidate->GetNHits(); iHit++)
    {
      TRecoRICHHit *ThisHit = static_cast<TRecoRICHHit *>( ThisCandidate->GetHit(iHit));
      Int_t ChannelID = ThisHit->GetChannelSeqID();
      TVector2 NewPosition = static_cast<RICHChannel*>(fChannels[ChannelID])->GetCenteredPosition() - GetChPosAngCorr(ChannelID);
      fHitPosForIterationFit.push_back(NewPosition);
      fHitTimeForIterationFit.push_back(ThisHit->GetTime() );
      fHitIndexForIterationFit.push_back(iHit);
    }

    // iteration procedure
    Double_t SingleHitResolution = 4.7;
    Double_t TimeResolution = 0.28;
    Double_t Chi2MaxAllowed = 16.;
    Int_t MaxNumberOfIterations = 5;
    Double_t TimeMean;
    Double_t Chi2Max;
    Int_t HitIndex_Chi2Max;
    Int_t NumberOfIterations = 0;
    // initial values for the ring center and radius
    fRingCenterIterationFit = ThisCandidate->GetRingCenter();
    fRingRadiusIterationFit = ThisCandidate->GetRingRadius();

    // start loop over iteratons
    for(Int_t iIteration=0; iIteration<MaxNumberOfIterations; iIteration++){
      TimeMean = 0.;
      Chi2Max = 0.;

      // calculate mean time of all hits in the fit
      for(UInt_t iHit=0; iHit<fHitTimeForIterationFit.size(); iHit++){
        TimeMean = TimeMean + fHitTimeForIterationFit[iHit]/fHitTimeForIterationFit.size();
      }
      // calculate max chi2 among all hits and find the worst hit
      for (UInt_t iHit = 0; iHit < fHitPosForIterationFit.size(); iHit++) {
        Double_t ThisChi2 = 0.;
        TVector2 HitPosition = fHitPosForIterationFit[iHit];
        Double_t u = HitPosition.X() - fRingCenterIterationFit.X();
        Double_t v = HitPosition.Y() - fRingCenterIterationFit.Y();
        Double_t d = TMath::Sqrt(u*u+v*v);
        ThisChi2 = (d - fRingRadiusIterationFit)/SingleHitResolution*(d - fRingRadiusIterationFit)/SingleHitResolution +
          (fHitTimeForIterationFit[iHit] - TimeMean)/TimeResolution*(fHitTimeForIterationFit[iHit] - TimeMean)/TimeResolution;
        if(ThisChi2>Chi2Max){
          Chi2Max = ThisChi2;
          HitIndex_Chi2Max = iHit;
        }
      }

      // remove hit(s) if they are far away and if NHits is large enough
      if(fHitPosForIterationFit.size()>(UInt_t)fMinPMsForEvent && Chi2Max>Chi2MaxAllowed){
        // remove one hit from the fit
        fHitPosForIterationFit.erase(fHitPosForIterationFit.begin() + HitIndex_Chi2Max);
        fHitTimeForIterationFit.erase(fHitTimeForIterationFit.begin() + HitIndex_Chi2Max);
        fHitIndexForIterationFit.erase(fHitIndexForIterationFit.begin() + HitIndex_Chi2Max);
        // redo the ring fit
        Chi2FitSpecial();
        NumberOfIterations++;
        // recalculate the mean time
        TimeMean = 0.;
        for(UInt_t iHit=0; iHit<fHitTimeForIterationFit.size(); iHit++){
          TimeMean = TimeMean + fHitTimeForIterationFit[iHit]/fHitTimeForIterationFit.size();
        }
      }
    } // end iteration cycle

    // create the array with hit indexes
    Int_t NHits = fHitIndexForIterationFit.size();
    Int_t HitIndexArray[NHits];
    for(Int_t iHit=0; iHit<NHits; iHit++){
      Int_t ThisIndex = fHitIndexForIterationFit[iHit];
      HitIndexArray[iHit] = ThisIndex;
    }

    // remove unphysical values
    if(fRingRadiusIterationFit>3000. || fRingCenterIterationFit.Mod()>3000. || NumberOfIterations<0){
      ThisCandidate->SetRingTimeSingleRing(9999999);
      ThisCandidate->SetRingRadiusSingleRing(-10.);
      ThisCandidate->SetRingRadiusErrorSingleRing(-10.);
      fRingCenterIterationFit.Set(500., 500.);
      fRingCenterErrorIterationFit.Set(500., 500.);
      ThisCandidate->SetRingCenterSingleRing(fRingCenterIterationFit);
      ThisCandidate->SetRingCenterErrorSingleRing(fRingCenterErrorIterationFit);
      ThisCandidate->SetNHitsSingleRing(0);
      ThisCandidate->SetRingChi2SingleRing(-100.);
      ThisCandidate->SetNIterationsSingleRing(0);
    }
    else { // write fit results to the PM time candidate
      ThisCandidate->SetRingTimeSingleRing(TimeMean);
      ThisCandidate->SetRingRadiusSingleRing(fRingRadiusIterationFit);
      ThisCandidate->SetRingRadiusErrorSingleRing(fRingRadiusErrorIterationFit);
      ThisCandidate->SetRingCenterSingleRing(fRingCenterIterationFit);
      ThisCandidate->SetRingCenterErrorSingleRing(fRingCenterErrorIterationFit);
      ThisCandidate->SetHitIndexesSingleRing(HitIndexArray, fHitIndexForIterationFit.size());
      ThisCandidate->SetNHitsSingleRing(fHitPosForIterationFit.size() );
      ThisCandidate->SetRingChi2SingleRing(fRingChi2IterationFit);
      ThisCandidate->SetNIterationsSingleRing(NumberOfIterations);
    }

  } // end loop over PM time candidates


  // Multi ring: pattern recognition
  if (fMultiRingRecoFlag)
  {
    Int_t LastFittedCandidate =   nCand; // last Time Candidate (PM+SC)
    for (Int_t iCand = nSCCand; iCand < nCand; iCand++)
    {
      for (Int_t i = 0; i < 8; i++) fCandSideAss[i] = -1;

      TRecoRICHCandidate *Candidate = static_cast<TRecoRICHCandidate*>( fRecoEvent->GetCandidate(iCand));
      if (!Candidate)
      {
        std::cout << "Candidate " << iCand << " doesn't exist actually, please check..." << std::endl;
        continue;
      }

      if (Candidate->GetNHits() < fMinPMsForEvent) continue;
      if (Candidate->GetNHits() < fMinHitForMulti) continue;

      Candidate->SetRingChi2(-20);
      MultiRingReco(Candidate);

      for (Int_t jCand = LastFittedCandidate; jCand < fRecoEvent->GetNCandidates() ; jCand++)
      {
        TRecoRICHCandidate *jCandidate = static_cast<TRecoRICHCandidate*>( fRecoEvent->GetCandidate(jCand));
        if (jCandidate->GetNHits() < fMinPMsForEvent) continue;
        RingFit(jCandidate);
        jCandidate->SetTimeCandidateIndex(iCand);
      } // end loop on side candidates


      Double_t TotChi2[8];
      for (Int_t iSide = 0; iSide < 8; iSide++)
      {
        Int_t iStart = fCandSideAss[iSide];
        if (iStart < 0)
        {
          TotChi2[iSide] = 1e9;
          continue;
        }
        Int_t iEnd = -2;
        for (Int_t jSide = iSide+1; jSide < 8; jSide++)
        {
          iEnd = fCandSideAss[jSide];
          if (iEnd >= 0) break;
        }
        if (iEnd < 0) iEnd = fRecoEvent->GetNCandidates();
        TotChi2[iSide] = 0;
        for (Int_t lCand = iStart; lCand < iEnd; lCand++)
        {
          Double_t Chi2 = static_cast<TRecoRICHCandidate*>(fRecoEvent->GetCandidate(lCand))->GetRingChi2();
          if (Chi2 < 0)
          {
            if (Chi2 == -10) continue;
            Chi2 = 5;
          }
          TotChi2[iSide] += Chi2;
        }
      }
      Int_t IndexChi2 = -1;
      Double_t MinChi2 = 1e8;
      for (Int_t iSide = 0; iSide < 8; iSide++)
      {
        if (TotChi2[iSide] >= MinChi2) continue;
        MinChi2 = TotChi2[iSide];
        IndexChi2 = iSide;
      }
      if (IndexChi2 >= 0)
      {
        Int_t iStart = fCandSideAss[IndexChi2];
        Int_t iEnd = -2;
        for (Int_t jSide = IndexChi2+1; jSide < 8; jSide++)
        {
          iEnd = fCandSideAss[jSide];
          if (iEnd >= 0) break;
        }
        if (iEnd < 0) iEnd = fRecoEvent->GetNCandidates();
        for (Int_t lCand = LastFittedCandidate; lCand < fRecoEvent->GetNCandidates(); lCand++)
        {
          if (lCand < iStart)
          {
            fRecoEvent->RemoveCandidate(lCand);
            iStart--;
            iEnd--;
            lCand--;
            continue;
          }
          if (lCand >= iEnd)
          {
            fRecoEvent->RemoveCandidate(lCand);
            lCand--;
            continue;
          }
        }
      }

      LastFittedCandidate = fRecoEvent->GetNCandidates();
    } // end loop on TimeCandidates
  }// end of multiring pattern recognition


  //Check bad fit on Ring Candidates and remove them

  for (Int_t iCand = nCand; iCand < fRecoEvent->GetNCandidates(); iCand++)
  {
    TRecoRICHCandidate *Candidate = static_cast<TRecoRICHCandidate*>( fRecoEvent->GetCandidate(iCand));
    if (Candidate->GetRingRadius()<0.) fRecoEvent->RemoveCandidate(iCand);
  }

  static_cast<TRecoRICHEvent*>(fRecoEvent)->SetNRingCandidates(fRecoEvent->GetNCandidates()-nCand);

  fHNRingCandidates->Fill(static_cast<TRecoRICHEvent*>(fRecoEvent)->GetNRingCandidates());

  Int_t nJ,nS;
  for(Int_t ifCand=0; ifCand<static_cast<TRecoRICHEvent*>(fRecoEvent)->GetNRingCandidates(); ifCand++){//loop on Ring Candidates
    nS=nJ=0;
    TRecoRICHCandidate* finalCand= static_cast<TRecoRICHEvent*>( fRecoEvent)->GetRingCandidate(ifCand);

    Int_t nHits =  finalCand->GetNHits();
    fHNHitperRingCandidate->Fill(finalCand->GetNHits());

    Double_t avg1,avg2;
    avg1=avg2=0.;

    Int_t n_avg1,n_avg2;
    n_avg1=n_avg2=0;

    for(Int_t ihit=0; ihit<nHits;ihit++){ //loop on hits
      TRecoRICHHit *Hit = static_cast<TRecoRICHHit *>( finalCand->GetHit(ihit));
      fHPMTIllumination->Fill(Hit->GetFitPosition().X(),Hit->GetFitPosition().Y());
      if(Hit->GetDiskID() == kSaleve){// if Saleve
        fHPMTFitIlluminationSaleve->Fill(Hit->GetFitPosition().X(),Hit->GetFitPosition().Y());
        nS++;
      }else{
        fHPMTFitIlluminationJura->Fill(Hit->GetFitPosition().X(),Hit->GetFitPosition().Y());
        nJ++;
      }

      if(ihit<nHits/2){
        avg1=avg1+Hit->GetTime();
        n_avg1++;
      }else{
        avg2=avg2+Hit->GetTime();
        n_avg2++;
      }
    }

    avg1=avg1/n_avg1;
    avg2=avg2/n_avg2;

    fHTimeResolution->Fill(avg1-avg2);

    fHRingRadius->Fill(finalCand->GetRingRadius());
    fHRingChi2->Fill(finalCand->GetRingChi2());
    fHRingChi2VsRingRadius->Fill(finalCand->GetRingRadius(),finalCand->GetRingChi2());
    fHRingXCenter->Fill(finalCand->GetRingCenter().X());
    fHRingYCenter->Fill(finalCand->GetRingCenter().Y());
    fHRingTime->Fill(finalCand->GetRingTime());
    fHRingCenter->Fill(finalCand->GetRingCenter().X(),finalCand->GetRingCenter().Y());

  }// loop on Ring Candidates

  return fRecoEvent;
}

/********************************************//**
 * Compute the reconstructed hit time width
 ************************************************/

Double_t RICHReconstruction::GetRecoTimeWidth(TRICHDigi *Digi)

{
  Double_t Width=0.;
  Int_t SeqID = Digi->GetChannelSeqID();
  RICHChannel* Channel = static_cast<RICHChannel*>(fChannels[SeqID]);

  if (Digi->GetDetectedEdge() ==3 ) { // Both leading and trailing always OK
    Width = Digi->GetTrailingEdge() - Digi->GetLeadingEdge();
  }
  else {   // Correct for missing leading/trailing if requested
    if (fEdgeRequirement < 3  || Digi->GetOrSuperCellID())
      Width = (fEnableSlewingCorr) ? Channel->GetMeanWidth() : 0.0 ;
  }
  return Width;
}

/********************************************//**
 * Compute the reconstructed hit time
 ************************************************/

Double_t RICHReconstruction::GetRecoTime(TRICHDigi *Digi)
{
  Double_t LeadingEdge=0.0, TrailingEdge, Width=0.0;
  Int_t SeqID = Digi->GetChannelSeqID();
  RICHChannel* Channel = static_cast<RICHChannel*>(fChannels[SeqID]);

  if (Digi->GetDetectedEdge() ==3 ) { // Both leading and trailing always OK
    LeadingEdge = Digi->GetLeadingEdge();
    TrailingEdge = Digi->GetTrailingEdge();
    Width = TrailingEdge - LeadingEdge;
  }
  else {   // Correct for missing leading/trailing if requested
    if (fEdgeRequirement < 3   || Digi->GetOrSuperCellID() ){
      if (Digi->GetDetectedEdge() ==1 ) { // leading only
        Width = (fEnableSlewingCorr) ? Channel->GetMeanWidth() : 0.0;
        LeadingEdge = Digi->GetLeadingEdge();
      }
      else if (Digi->GetDetectedEdge() ==2 ){ // trailing only
        Width = (fEnableSlewingCorr) ? Channel->GetMeanWidth() : 0.0;
        LeadingEdge = Digi->GetTrailingEdge() - Width;
      }
    } // EdgeRequirement<3
  }
  Double_t T0 = (fEnableT0) ? Channel->GetT0() : 0.0;
  Double_t SlewCorr = (fEnableSlewingCorr) ? Channel->GetSlewingCorrection(Width) : 0.0;
  Double_t Time = LeadingEdge - GetT0Correction(Digi) - T0 - SlewCorr;

  return Time;
}

/********************************************//**
 * Histograms initialization
 ************************************************/

void RICHReconstruction::InitHistograms() {
  TDirectory *RICHDir = GetOrMakeDir(fHistoFile,"RICHMonitor");
  if (fChannelHistograms) {
    TDirectory *RICHChannelsDir = GetOrMakeDir(RICHDir,"RICHChannels");
    GetOrMakeDir(RICHChannelsDir,"T0");
    GetOrMakeDir(RICHChannelsDir,"Width");
    GetOrMakeDir(RICHChannelsDir,"SlewingCorr");
  }
  fHistoFile->cd("RICHMonitor");

  fHTimeResolution = new TH1F("TimeResolution","TimeResolution",100,-5,5);
  fHTimeResolution->GetXaxis()->SetTitle("T1-T2 (ns)");

  if (fHRecoHitTimeWrtReferenceVsROChannel) {
    fHRecoHitTimeWrtReferenceVsSeqChannel = new TH2F
      ("RecoHitTimeWrtReferenceVsSeqChannel","RecoHitTimeWrtReferenceVsSeqChannel",
       fNChannels, -0.5, fNChannels-0.5, 500, -5, 5);
    fHRecoHitTimeWrtReferenceVsSeqChannelNoT0 = new TH2F
      ("RecoHitTimeWrtReferenceVsSeqChannelNoT0","RecoHitTimeWrtReferenceVsSeqChannelNoT0",
       fNChannels, -0.5, fNChannels-0.5, 500, -5, 5);
  }

  fHLeadingTimeVsSlot = new TH2F("LeadingTimeVsSlot","LeadingTimeVsSlot",51, -25.5, 25.5, 600, -300., 300.);
  fHTrailingTimeVsSlot = new TH2F("TrailingTimeVsSlot","TrailingTimeVsSlot",51, -25.5, 25.5, 600, -300., 300.);
  fHTrailingTime = new TH1D("TrailingTimeRaw","TrailingTimeRaw", 5000, -5000, +5000);
  fHTrailingTime->GetXaxis()->SetTitle("Trailing Time (ns)");
  fHNTotHits = new TH1I("NTotHits","Total number of hits  (PM + SC)",400,-0.5,399.5);
  fHNTotHits->GetXaxis()->SetTitle("Total Number of hits (PM + SC)");
  fHNHitsPMJura = new TH1I("NHitsPMJura","Number of PM hits Jura",400,-0.5,399.5);
  fHNHitsPMJura->GetXaxis()->SetTitle("Number of PM hits Jura");
  fHNHitsPMSaleve = new TH1I("NHitsPMSaleve","Number of PM hits Saleve",400,-0.5,399.5);
  fHNHitsPMSaleve->GetXaxis()->SetTitle("Number of PM hits Saleve");
  fHNHitsSCJura = new TH1I("NHitsSCJura","Number of SC hits Jura",400,-0.5,399.5);
  fHNHitsSCJura->GetXaxis()->SetTitle("Number of SC hits Jura");
  fHNHitsSCSaleve = new TH1I("NHitsSCSaleve","Number of SC hits Saleve",400,-0.5,399.5);
  fHNHitsSCSaleve->GetXaxis()->SetTitle("Number of SC hits Saleve");
  fHNRecoHits = new TH1I("NRecoHits","Total number of Reco hits",200,-0.5,199.5);
  fHNRecoHits->GetXaxis()->SetTitle("Number of Reco hits");
  fHNRecoHitsPMJ = new TH1I("NRecoHitsPMJ","Number of PM Reco hits Jura",200,-0.5,199.5);
  fHNRecoHitsPMJ->GetXaxis()->SetTitle("Number of PM Reco hits Jura");
  fHNRecoHitsPMS = new TH1I("NRecoHitsPMS","Number of PM Reco hits Saleve",200,-0.5,199.5);
  fHNRecoHitsPMS->GetXaxis()->SetTitle("Number of PM Reco hits Saleve");
  fHNRecoHitsSCJ = new TH1I("NRecoHitsSCJ","Number of SC Reco hits Jura",200,-0.5,199.5);
  fHNRecoHitsSCJ->GetXaxis()->SetTitle("Number of SC Reco hits Jura");
  fHNRecoHitsSCS = new TH1I("NRecoHitsSCS","Number of SC Reco hits Saleve",200,-0.5,199.5);
  fHNRecoHitsSCS->GetXaxis()->SetTitle("Number of SC Reco hits Saleve");
  fHNRingCandidates = new TH1I("NRingCandidates","Total number of reco candidates",20,-0.5,19.5);
  fHNRingCandidates->GetXaxis()->SetTitle("Number of reco ring Candidates");
  fHNTimeCandidates = new TH1I("NTimeCandidates","Total number of reco candidates",20,-0.5,19.5);
  fHNTimeCandidates->GetXaxis()->SetTitle("Number of reco time Candidates");

  fHNHitperRingCandidate = new TH1I("NHitperRingCandidate","Number of hits per reco candidate",40,-0.5,39.5);
  fHNHitperRingCandidate->GetXaxis()->SetTitle("Number of hits per reco ring Candidate");
  fHNHitperTimeCandidate = new TH1I("NHitperTimeCandidate","Number of hits per reco candidate",40,-0.5,39.5);
  fHNHitperTimeCandidate->GetXaxis()->SetTitle("Number of hits per reco time Candidate");

  fHHitStatus = new TH1I("HitStatus","Hit Status",4,-0.5,3.5);
  fHHitStatus->GetXaxis()->SetTitle("Hit Status: 1=Leading edge 2=Trailing edge 3=Both edges ");
  fHRingRadius = new TH1F("RingRadius","Ring Radius",600,0.,300.);
  fHRingRadius->GetXaxis()->SetTitle("Radius (mm)");
  fHRingChi2 = new TH1F("RingChi2","Ring #chi^{2}",1000,-1.,100.);
  fHRingChi2->GetXaxis()->SetTitle("#chi^{2}");
  fHRingChi2VsRingRadius = new TH2F("RingChi2VsRingRadius","Ring #chi^{2} Vs Ring Radius",600,0.,300.,400,-1.,10.);
  fHRingChi2VsRingRadius->GetXaxis()->SetTitle("Radius (mm)");
  fHRingChi2VsRingRadius->GetYaxis()->SetTitle("#chi^{2}");
  fHRingXCenter = new TH1F("RingXCenter","Ring X Center",300,-600.,600.);
  fHRingXCenter->GetXaxis()->SetTitle("X (mm)");
  fHRingYCenter = new TH1F("RingYCenter","Ring Y Center",300,-600.,600.);
  fHRingYCenter->GetXaxis()->SetTitle("Y (mm)");
  fHRingTime = new TH1F("RingTime","Ring Time",1000,-1000.,1000.);
  fHRingTime->GetXaxis()->SetTitle("T (ns)");
  fHSingleRingTime = new TH1F("Single RingTime","Single Ring Time",1000,-1000.,1000.);
  fHSingleRingTime->GetXaxis()->SetTitle("T (ns)");
  // fHOccupancy = new TH1F("Occupancy","Channel occupancy (PMTs + SC)",2196,-0.5,2195.5);
  //fHOccupancy->GetXaxis()->SetTitle("");

  //  fHOccupancyJura = new TH1F("OccupancyJura","PMT Channel occupancy",976,-0.5,975.5);
  // fHOccupancyJura->GetXaxis()->SetTitle("");
  // fHOccupancySaleve = new TH1F("OccupancySaleve","PMT Channel occupancy",976,975.5,1951.5);
  //fHOccupancySaleve->GetXaxis()->SetTitle("");

  //fHSuperCellOccupancyJura = new TH1F("SuperCellOccupancyJura","SuperCell occupancy",122,1951.5,2073.5);
  // fHSuperCellOccupancyJura->GetXaxis()->SetTitle("");
  // fHSuperCellOccupancySaleve = new TH1F("SuperCellOccupancySaleve","SuperCell occupancy",122,2073.5,2195.5);
  //fHSuperCellOccupancySaleve->GetXaxis()->SetTitle("");

  fHROOccupancy = new TH1F("ROOccupancy","RO Channel occupancy PMT+SC",2560,-0.5,2559.5);
  fHROOccupancy->GetXaxis()->SetTitle("");

  fHROOccupancyJura = new TH1F("ROOccupancyJura","PMT RO Channel occupancy",1024,-0.5,1023.5);
  fHROOccupancyJura->GetXaxis()->SetTitle("");
  fHROOccupancySaleve = new TH1F("ROOccupancySaleve","PMT RO Channel occupancy",1024,1023.5,2047.5);
  fHROOccupancySaleve->GetXaxis()->SetTitle("");

  fHSuperCellROOccupancyJura = new TH1F("ROSuperCellOccupancyJura","SuperCell RO Channel occupancy",256,2047.5,2303.5);
  fHSuperCellROOccupancyJura->GetXaxis()->SetTitle("");
  fHSuperCellROOccupancySaleve = new TH1F("ROSuperCellOccupancySaleve","SuperCell RO Channel occupancy",256,2303.5,2559.5);
  fHSuperCellROOccupancySaleve->GetXaxis()->SetTitle("");

  fHWidth = new TH1F("Width","PM Time width",305,0.5,30.5);
  fHWidth->GetXaxis()->SetTitle("PM Time Width (ns)");
  fHPtolemy = new TH1F("Ptolemy","Ptolemy",500,0,500);
  fHPtolemy->GetXaxis()->SetTitle("Ptolemy (mm^{2})");
  Double_t WIDTH_Y = 9.*sqrt(3.)/2.;
  fHPMTIlluminationSaleve = new TH2F("PMTIlluminationSaleve","PMT illumination Saleve side",112,-504,504,64,-32.5*WIDTH_Y*2,31.5*WIDTH_Y*2);
  fHPMTIlluminationSaleve->GetXaxis()->SetTitle("X (mm)");
  fHPMTIlluminationSaleve->GetYaxis()->SetTitle("Y (mm)");
  fHPMTIlluminationJura = new TH2F("PMTIlluminationJura","PMT illumination Jura side",112,-504,504,64,-32.5*WIDTH_Y*2,31.5*WIDTH_Y*2);
  fHPMTIlluminationJura->GetXaxis()->SetTitle("X (mm)");
  fHPMTIlluminationJura->GetYaxis()->SetTitle("Y (mm)");

  fHPMTFitIlluminationSaleve = new TH2F("PMTFitIlluminationSaleve","PMT fit illumination Saleve side",112,-504,504,64,-32.5*WIDTH_Y*2,31.5*WIDTH_Y*2);
  fHPMTFitIlluminationSaleve->GetXaxis()->SetTitle("X (mm)");
  fHPMTFitIlluminationSaleve->GetYaxis()->SetTitle("Y (mm)");
  fHPMTFitIlluminationJura = new TH2F("PMTFitIlluminationJura","PMT fit illumination Jura side",112,-504,504,64,-32.5*WIDTH_Y*2,31.5*WIDTH_Y*2);
  fHPMTFitIlluminationJura->GetXaxis()->SetTitle("X (mm)");
  fHPMTFitIlluminationJura->GetYaxis()->SetTitle("Y (mm)");

  fHPMTIllumination = new TH2F("PMTIllumination","PMT illumination ",112,-504,504,64,-32.5*WIDTH_Y*2,31.5*WIDTH_Y*2);
  fHPMTIllumination->GetXaxis()->SetTitle("X (mm)");
  fHPMTIllumination->GetYaxis()->SetTitle("Y (mm)");
  fHSuperCellIlluminationSaleve = new TH2F("SuperCellIlluminationSaleve","SuperCell illumination Saleve side",112,-504,504,64,-32.5*WIDTH_Y*2,31.5*WIDTH_Y*2);
  fHSuperCellIlluminationSaleve->GetXaxis()->SetTitle("X (mm)");
  fHSuperCellIlluminationSaleve->GetYaxis()->SetTitle("Y (mm)");
  fHSuperCellIlluminationJura = new TH2F("SuperCellIlluminationJura","SuperCell illumination Jura side",112,-504,504,64,-32.5*WIDTH_Y*2,31.5*WIDTH_Y*2);
  fHSuperCellIlluminationJura->GetXaxis()->SetTitle("X (mm)");
  fHSuperCellIlluminationJura->GetYaxis()->SetTitle("Y (mm)");
  fHSuperCellIllumination = new TH2F("SuperCellIllumination","SuperCell illumination ",112,-504,504,64,-32.5*WIDTH_Y*2,31.5*WIDTH_Y*2);
  fHSuperCellIllumination->GetXaxis()->SetTitle("X (mm)");
  fHSuperCellIllumination->GetYaxis()->SetTitle("Y (mm)");
  fHRingCenter = new TH2F("RingCenter","Ring Center",300,-600.,600.,300,-600.,600.);
  fHRingCenter->GetXaxis()->SetTitle("X (mm)");
  fHRingCenter->GetYaxis()->SetTitle("Y (mm)");
  fHHitCandidateTimeDiff = new TH1F("HitCandidateTimeDiff","T_{Hit}-T_{Candidate}",96,-6.5,6.5);
  fHHitCandidateTimeDiff->GetXaxis()->SetTitle("T_{Hit}-T_{Candidate} (ns)");
  fHHitCandidateTimeDiffvsChannel = new TH2F("HitCandidateTimeDiffvsChannel","T_{Hit}-T_{Candidate} vs CHannel",1952,-0.5,1951.5,100,-6.,6.);
  fHHitCandidateTimeDiffvsChannel->GetXaxis()->SetTitle("PM Channel");
  fHHitCandidateTimeDiffvsChannel->GetYaxis()->SetTitle("T_{Hit}-T_{Candidate} (ns)");
  fHHitCandidateTimeDiffvsROChannel = new TH2F("HitCandidateTimeDiffvsROChannel","T_{Hit}-T_{Candidate} vs ROCHannel",1952,-0.5,1951.5,100,-6.,6.);
  fHHitCandidateTimeDiffvsROChannel->GetXaxis()->SetTitle("RO Channel");
  fHHitCandidateTimeDiffvsROChannel->GetYaxis()->SetTitle("T_{Hit}-T_{Candidate} (ns)");
  fHHitCandidateTimeDiffvsWidth = new TH2F("HitCandidateTimeDiffvsWidth","T_{Hit}-T_{Candidate} vs Width",305,0.5,30.5,100,-10.,10.);
  fHHitCandidateTimeDiffvsWidth->GetXaxis()->SetTitle("Width (ns)");
  fHHitCandidateTimeDiffvsWidth->GetYaxis()->SetTitle("T_{Hit}-T_{Candidate} (ns)");
  // fHHitLeadingTimevsChannel = new TH2F("HitLeadingTimevsChannel","Hit LeadingTime vs Channel",1952, -0.5, 1951.5,1000, -500., 500.);
  // fHHitLeadingTimevsChannel->GetXaxis()->SetTitle("Channel number");
  //fHHitLeadingTimevsChannel->GetYaxis()->SetTitle("T_{Hit} (LeadingTime-Offset) (ns)");
  fHHitLeadingTimevsROChannel = new TH2F("HitLeadingTimevsROChannel","Hit LeadingTime vs ROChannel",2048, -0.5, 2047.5,1000, -500., 500.);
  fHHitLeadingTimevsROChannel->GetXaxis()->SetTitle("RO Channel number");
  fHHitLeadingTimevsROChannel->GetYaxis()->SetTitle("T_{Hit} (LeadingTime-Offset) (ns)");
  fHHitWidthvsChannel = new TH2F("WidthvsChannel","Width_{Hit} vs Channel",1952, -0.5, 1951.5,305,0.5,30.5);
  fHHitWidthvsChannel->GetXaxis()->SetTitle("Channel");
  fHHitWidthvsChannel->GetYaxis()->SetTitle("T_{Hit} (ns)");
  fHHitWidthvsROChannel = new TH2F("WidthvsROChannel","Width_{Hit} vs ROChannel",2048, -0.5, 2047.5,305,0.5,30.5);
  fHHitWidthvsROChannel->GetXaxis()->SetTitle("ROChannel");
  fHHitWidthvsROChannel->GetYaxis()->SetTitle("T_{Hit} (ns)");

  //  fHSCLeadingTimevsChannel = new TH2F("SCLeadingTimevsChannel","SC LeadingTime vs Channel",244, 1951.5, 2195.5,1000, -500., 500.);
  // fHSCLeadingTimevsChannel->GetXaxis()->SetTitle("Channel number");
  //fHSCLeadingTimevsChannel->GetYaxis()->SetTitle("T_{SC} (LeadingTime-Offset) (ns)");
  fHSCLeadingTimevsROChannel = new TH2F("SCLeadingTimevsROChannel","SC LeadingTime vs ROChannel",512, 2047.5, 2559.5,1000, -500., 500.);
  fHSCLeadingTimevsROChannel->GetXaxis()->SetTitle("RO Channel number");
  fHSCLeadingTimevsROChannel->GetYaxis()->SetTitle("T_{SC} (LeadingTime-Offset) (ns)");
  fHSCWidthvsChannel = new TH2F("SCWidthvsChannel","Width_{SC} vs Channel",244, 1951.5, 2195.5,305,0.5,30.5);
  fHSCWidthvsChannel->GetXaxis()->SetTitle("Channel");
  fHSCWidthvsChannel->GetYaxis()->SetTitle("T_{SC} (ns)");
  fHSCWidthvsROChannel = new TH2F("SCWidthvsROChannel","Width_{SC} vs ROChannel",512, 2047.5, 2559.5,305,0.5,30.5);
  fHSCWidthvsROChannel->GetXaxis()->SetTitle("ROChannel");
  fHSCWidthvsROChannel->GetYaxis()->SetTitle("T_{SC} (ns)");
  fHistoFile->cd("/");
}

/********************************************//**
 * Evaluating time corrections and
 * saving histograms
 ************************************************/

void RICHReconstruction::EndProcessing() {
  NA62VReconstruction::EndProcessing(); // call base class for raw hist output
  SaveHistograms();
}

/********************************************//**
 * Filling T0 histograms
 ************************************************/

void RICHReconstruction::FillTimes(Double_t ReferenceTime) {

  //common part for all the subdetectors
  NA62VReconstruction::FillTimes(ReferenceTime);
  if(fT0ReferenceDetector == "RICH") { //RICH it's its own reference detector
    // Fill the histos used to evaluate T0 and Slewing Corrections
    // In the past the variable used was:
    // Delta_i = Thit(i)-Tcand
    // where Tcand = Sum(j=1,Nhit)[Thit(j)] 1/Nhit
    // in the sum the the  Thit(i) is included, this makes the Delta_i slightly biased, the effect depends on the number of hits we are considering.
    // In order to get rid of this small effect, now we use a variable called UnbiasedDeltaT defined as:
    // UnbiasedDeltaT = Thit(i) -  Sum(i.ne.i)[Thit(j)]  1/(Nhit-1))
    // UnbiasedDeltaT = Nhit/(Nhit-1) [Thit(i)-Tcand]

    //find the closest candidate to ReferenceTime
    Double_t TimeWrtReference = 1.e28;
    Int_t ClosestCandidateID = -1;

    Int_t nSCCand = static_cast<TRecoRICHEvent*>(fRecoEvent)->GetNSCTimeCandidates();
    Int_t nPMCand = static_cast<TRecoRICHEvent*>(fRecoEvent)->GetNPMTimeCandidates();

    for(int iCand= 0; iCand< nSCCand+nPMCand ;iCand++){
      fCandidate = static_cast<TRecoRICHCandidate*>(fRecoEvent->GetCandidate(iCand));
      if (fabs(fCandidate->GetTime()-ReferenceTime)<TimeWrtReference){
        TimeWrtReference = fabs(fCandidate->GetTime()-ReferenceTime);
        ClosestCandidateID = iCand;
      }
    }
    //return if no candidates are found
    if(ClosestCandidateID == -1) return;
    //use the closest candidate only
    fCandidate = static_cast<TRecoRICHCandidate*>(fRecoEvent->GetCandidate(ClosestCandidateID));
    for (Int_t ihit=0; ihit<fCandidate->GetNHits(); ihit++) {
      TRecoRICHHit *RecoHit = static_cast<TRecoRICHHit*>(fCandidate->GetHit(ihit));
      Int_t iSeqCh   = RecoHit->GetChannelSeqID();
      Int_t iROCh   = RecoHit->GetROChannelID();
      Double_t Time  = RecoHit->GetTime();
      Double_t Width = RecoHit->GetTimeWidth();
      Double_t alpha = ((Double_t) fCandidate->GetNHits())/((Double_t)(fCandidate->GetNHits()-1)); //UnbiasedDeltaT
      if (fEvaluateSlewingCorr ){
        //	fChannels[iSeqCh]->FillTime(Width,alpha*(Time-ReferenceTime),ReferenceTime);
        static_cast<RICHChannel*>(fChannels[iSeqCh])->FillTime(Width,alpha*(Time-ReferenceTime));
      }

      // 2D histos (Time-ReferenceTime vs Sequential ChID), input to the T0Computation analysis code
      fHRecoHitTimeWrtReferenceVsSeqChannel->Fill(iSeqCh,Time-ReferenceTime);
      fHRecoHitTimeWrtReferenceVsSeqChannelNoT0->Fill(iSeqCh,Time-ReferenceTime+fChannels[iSeqCh]->GetT0());
      // 2D histos (Time-ReferenceTime vs ReadOut ChID), input to the T0Computation analysis code
      if (fHRecoHitTimeWrtReferenceVsROChannel)         fHRecoHitTimeWrtReferenceVsROChannel->Fill(iROCh,Time-ReferenceTime);
      if (fHRecoHitTimeWrtReferenceVsROChannelNoT0)     fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(iROCh,
          Time-ReferenceTime+fChannels[iSeqCh]->GetT0());
      if (fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim) fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim->Fill(iROCh,
          Time-ReferenceTime+fChannels[iSeqCh]->GetT0()+static_cast<RICHChannel*>(fChannels[iSeqCh])->GetSlewingCorrection(Width));
    }
  }
  else{ //other detector is RICH reference detector
    //use of all reco hits - no candidate structure
    TClonesArray  &Hits = (*(fRecoEvent->GetHits()));
    for (Int_t iHit=0; iHit<fRecoEvent->GetNHits(); iHit++) {
      TRecoRICHHit *RecoHit = static_cast<TRecoRICHHit *>(Hits[iHit]);
      Int_t iSeqCh   = RecoHit->GetChannelSeqID();
      Int_t iROCh   = RecoHit->GetROChannelID();
      Double_t Time  = RecoHit->GetTime();
      Double_t Width = RecoHit->GetTimeWidth();
      if ( fEvaluateSlewingCorr ){
        static_cast<RICHChannel*>(fChannels[iSeqCh])->FillTime(Width,Time-ReferenceTime);
      }
      if (fHRecoHitTimeWrtReferenceVsROChannel) {
        // 2D histos (Time-ReferenceTime vs Sequential ChID), input to the T0Computation analysis code
        fHRecoHitTimeWrtReferenceVsSeqChannel->Fill(iSeqCh,Time-ReferenceTime);
        fHRecoHitTimeWrtReferenceVsSeqChannelNoT0->Fill(iSeqCh,Time-ReferenceTime+fChannels[iSeqCh]->GetT0());
        // 2D histos (Time-ReferenceTime vs ReadOut ChID), input to the T0Computation analysis code
        if (fHRecoHitTimeWrtReferenceVsROChannel)         fHRecoHitTimeWrtReferenceVsROChannel->Fill(iROCh,Time-ReferenceTime);
        if (fHRecoHitTimeWrtReferenceVsROChannelNoT0)     fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(iROCh,
            Time-ReferenceTime+fChannels[iSeqCh]->GetT0());
        if (fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim) fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim->Fill(iROCh,
            Time-ReferenceTime+fChannels[iSeqCh]->GetT0()+static_cast<RICHChannel*>(fChannels[iSeqCh])->GetSlewingCorrection(Width));
      }
    }
  }
}

/* Save the histograms
 ************************************************/

void RICHReconstruction::SaveHistograms() {
  fHistoFile->cd("RICHMonitor");

  if (fHRecoHitTimeWrtReferenceVsSeqChannel) fHRecoHitTimeWrtReferenceVsSeqChannel->Write();
  if (fHRecoHitTimeWrtReferenceVsSeqChannel) fHRecoHitTimeWrtReferenceVsSeqChannelNoT0->Write();
  fHTimeResolution->Write();
  fHLeadingTimeVsSlot->Write();
  fHTrailingTimeVsSlot->Write();
  fHTrailingTime->Write();
  fHNTotHits->Write();
  fHNHitsPMJura->Write();
  fHNHitsPMSaleve->Write();
  fHNHitsSCJura->Write();
  fHNHitsSCSaleve->Write();
  fHNRecoHits->Write();
  fHNRecoHitsPMJ->Write();
  fHNRecoHitsPMS->Write();
  fHNRecoHitsSCJ->Write();
  fHNRecoHitsSCS->Write();
  fHNRingCandidates->Write();
  fHNTimeCandidates->Write();
  fHNHitperRingCandidate->Write();
  fHNHitperTimeCandidate->Write();
  fHHitStatus->Write();
  fHRingRadius->Write();
  fHRingChi2->Write();
  fHRingChi2VsRingRadius->Write();
  fHRingXCenter->Write();
  fHRingYCenter->Write();
  fHRingTime->Write();
  fHSingleRingTime->Write();
  // fHOccupancy->Write();
  // fHOccupancyJura->Write();
  // fHOccupancySaleve->Write();
  //fHSuperCellOccupancyJura->Write();
  //fHSuperCellOccupancySaleve->Write();
  fHROOccupancy->Write();
  fHROOccupancyJura->Write();
  fHROOccupancySaleve->Write();
  fHSuperCellROOccupancyJura->Write();
  fHSuperCellROOccupancySaleve->Write();
  fHWidth->Write();
  fHPtolemy->Write();
  fHPMTIlluminationSaleve->Write();
  fHPMTIlluminationJura->Write();
  fHPMTFitIlluminationSaleve->Write();
  fHPMTFitIlluminationJura->Write();
  fHPMTIllumination->Write();
  fHSuperCellIlluminationSaleve->Write();
  fHSuperCellIlluminationJura->Write();
  fHSuperCellIllumination->Write();
  fHRingCenter->Write();
  fHHitCandidateTimeDiff->Write();
  fHHitCandidateTimeDiffvsChannel->Write();
  fHHitCandidateTimeDiffvsROChannel->Write();
  fHHitCandidateTimeDiffvsWidth->Write();
  //  fHHitLeadingTimevsChannel->Write();
  fHHitLeadingTimevsROChannel->Write();
  fHHitWidthvsChannel->Write();
  fHHitWidthvsROChannel->Write();
  // fHSCLeadingTimevsChannel->Write();
  fHSCLeadingTimevsROChannel->Write();
  fHSCWidthvsChannel->Write();
  fHSCWidthvsROChannel->Write();

  for (Int_t iCh = 0; iCh < fNChannels; iCh++)
    fChannels[iCh]->Write(fHistoFile);
  fHistoFile->cd("/");

  if(fEvaluateSlewingCorr){
    for (Int_t iCh = 0; iCh < fNChannels; iCh++)
      static_cast<RICHChannel*>(fChannels[iCh])->Write(fHistoFile);
  }
  fHistoFile->cd("/");
}

/********************************************//**
 * TDC Event monitoring
 ************************************************/

void RICHReconstruction::TDCEventMonitor(TDCEvent* /*TdcEvent*/) {}

/********************************************//**
 * Single ring fit algorythm
 * Applies mirror focus corrections
 ************************************************/

Bool_t RICHReconstruction::RingFit(TRecoRICHCandidate *Candidate)
{
  fHitPos.clear();
  Int_t nHits = Candidate->GetNHits();
  Double_t fitTime=0.;

  for (Int_t iHit = 0; iHit < nHits; iHit++)
  {
    TRecoRICHHit *Hit = static_cast<TRecoRICHHit *>( Candidate->GetHit(iHit));
    Int_t ChannelID = Hit->GetChannelSeqID();
    TVector2 NewPosition = static_cast<RICHChannel*>(fChannels[ChannelID])->GetCenteredPosition() - GetChPosAngCorr(ChannelID);
    fHitPos.push_back(NewPosition);
    fitTime += Hit->GetTime();
  }
  fitTime = fitTime/nHits;
  Candidate->SetTime(fitTime);

  if (fFlagChi2)
  {
    if (Chi2Fit(Candidate))
      return kTRUE;
    return kFALSE;
  }
  return kTRUE;
}

/********************************************//**
 * Mirror focus position corrections
 ************************************************/

TVector2 RICHReconstruction::GetChPosAngCorr(Int_t iCh)
{
  TVector2 Beta;
  if (iCh < (fNChannels-fNSCChannels)/2 || ((fNChannels-fNSCChannels) <= iCh && iCh < fNChannels-(fNSCChannels/2)))
    Beta = fAngleRotationJura;
  else if (((fNChannels-fNSCChannels)/2 <= iCh && iCh < (fNChannels-fNSCChannels)) || (fNChannels-(fNSCChannels/2) <= iCh && iCh <fNChannels ))
    Beta = fAngleRotationSaleve;
  TVector2 Correction = Beta;// the correction due to the mirror rotation is already multiplied by the fFocalLength and it is given in mm;
  return Correction;
}

/********************************************//**
 * Chi2 Fit
 ************************************************/

Bool_t RICHReconstruction::Chi2Fit(TRecoRICHCandidate *Candidate)
{
  Float_t Xcog = 0;
  Float_t Ycog = 0;
  Float_t Rmean = 0;
  UInt_t nHits = fHitPos.size();
  fHitPosForChiFit.clear();
  for (UInt_t iHit = 0; iHit < nHits; iHit++)
  {
    fHitPosForChiFit.push_back(fHitPos[iHit]);
    Xcog += fHitPos[iHit].X();
    Ycog += fHitPos[iHit].Y();
  }

  Xcog /= nHits;
  Ycog /= nHits;

  for (UInt_t iHit = 0; iHit < nHits; iHit++)
    Rmean += sqrt(pow(Xcog - fHitPos[iHit].X(),2) + pow(Ycog - fHitPos[iHit].Y(),2));

  Rmean /= nHits;

  Double_t amin,edm,errdef;
  Int_t nvpar,nparx,icstat,ierflag;
  Double_t pars[10],epars[10];

  Double_t arglist[1];
  arglist[0] = -1;
  fFitter->mnexcm("SET PRI", arglist, 1, ierflag); // Set MINUIT print level
  fFitter->mnexcm("SET NOW", arglist, 0, ierflag); // Set MINUIT warnings

  fFitter->mnparm(0, "x0", Xcog, 0.01, 0., 0., ierflag);
  fFitter->mnparm(1, "y0", Ycog, 0.01, 0., 0., ierflag);
  fFitter->mnparm(2, "R", Rmean, 0.01, 0., 0., ierflag);

  fFitter->mnparm(0, "x0", Xcog, 0.01, 0., 0., ierflag);
  fFitter->mnparm(1, "y0", Ycog, 0.01, 0., 0., ierflag);
  fFitter->mnparm(2, "R", Rmean, 0.01, 0., 0., ierflag);

  arglist[0] = 0;
  fFitter->mnexcm("MIGRAD", arglist, 1, ierflag); // Calls the minimization
  fFitter->mnstat(amin,edm,errdef,nvpar,nparx,icstat);
  for(Int_t iPar = 0; iPar < fNPars; iPar++)
    fFitter->GetParameter(iPar,pars[iPar],epars[iPar]);

  TVector2 CurrentRingCenter;
  Double_t CurrentRingRadius, CurrentRingChi2;

  CurrentRingCenter.Set(pars[0],pars[1]);
  CurrentRingRadius = pars[2];
  CurrentRingChi2 = RingChi2(pars);

  if((CurrentRingRadius > 210000 || CurrentRingCenter.Mod() > 3000)) //WARNING
  {
    TVector2 BadFitCenter;
    BadFitCenter.Set(500.,500.);
    Candidate->SetRingCenter(BadFitCenter);
    Candidate->SetRingRadius(-10);
    Candidate->SetRingChi2(-100);
    Candidate->SetRingTime(999999999);
    return kFALSE;
  }

  Candidate->SetRingCenter(CurrentRingCenter);
  Candidate->SetRingRadius(CurrentRingRadius);
  Candidate->SetRingChi2(CurrentRingChi2);
  Candidate->SetRingTime(Candidate->GetTime());
  return kTRUE;
}

/*************************************************
 * special Chi2 iteration fit
 ***********************************************/

Bool_t RICHReconstruction::Chi2FitSpecial()
{
  Float_t Xcog = 0;
  Float_t Ycog = 0;
  Float_t Rmean = 0;
  UInt_t nHits = fHitPosForIterationFit.size();
  fHitPosForChiFit.clear();
  for (UInt_t iHit = 0; iHit < nHits; iHit++)
  {
    fHitPosForChiFit.push_back(fHitPosForIterationFit[iHit]);
    Xcog += fHitPos[iHit].X();
    Ycog += fHitPos[iHit].Y();
  }

  Xcog /= nHits;
  Ycog /= nHits;

  for (UInt_t iHit = 0; iHit < nHits; iHit++)
    Rmean += sqrt(pow(Xcog - fHitPos[iHit].X(),2) + pow(Ycog - fHitPos[iHit].Y(),2));
  Rmean /= nHits;

  Double_t amin,edm,errdef;
  Int_t nvpar,nparx,icstat,ierflag;
  Double_t pars[10],epars[10];

  Double_t arglist[1];
  arglist[0] = -1;
  fFitter->mnexcm("SET PRI", arglist, 1, ierflag); // Set MINUIT print level
  fFitter->mnexcm("SET NOW", arglist, 0, ierflag); // Set MINUIT warnings

  fFitter->mnparm(0, "x0", Xcog, 0.01, 0., 0., ierflag);
  fFitter->mnparm(1, "y0", Ycog, 0.01, 0., 0., ierflag);
  fFitter->mnparm(2, "R", Rmean, 0.01, 0., 0., ierflag);

  fFitter->mnparm(0, "x0", Xcog, 0.01, 0., 0., ierflag);
  fFitter->mnparm(1, "y0", Ycog, 0.01, 0., 0., ierflag);
  fFitter->mnparm(2, "R", Rmean, 0.01, 0., 0., ierflag);

  arglist[0] = 0;
  fFitter->mnexcm("MIGRAD", arglist, 1, ierflag); // Calls the minimization
  fFitter->mnstat(amin,edm,errdef,nvpar,nparx,icstat);
  for(Int_t iPar = 0; iPar < fNPars; iPar++)
    fFitter->GetParameter(iPar,pars[iPar],epars[iPar]);

  fRingRadiusIterationFit = pars[2];
  fRingRadiusErrorIterationFit = epars[2];
  fRingCenterIterationFit.Set(pars[0],pars[1]);
  fRingCenterErrorIterationFit.Set(epars[0],epars[1]);
  fRingChi2IterationFit = RingChi2(pars);

  if(fRingRadiusIterationFit > 210000 || fRingCenterIterationFit.Mod() > 3000) return kFALSE;
  return kTRUE;
}

/********************************************//**
 * Minuit minimization function
 ************************************************/

void RICHReconstruction::RingChi2FCN(Int_t &, Double_t *, Double_t &f, Double_t *par, Int_t iflag)
{
  if(1 || iflag == 4)
  {
    UInt_t nHits = fHitPosForChiFit.size();
    f = 0.;
    for (UInt_t iHit = 0; iHit < nHits; iHit++)
    {
      TVector2 PMPosition = fHitPosForChiFit[iHit];
      Double_t u = PMPosition.X() - par[0];
      Double_t v = PMPosition.Y() - par[1];
      Double_t d = TMath::Sqrt(u*u+v*v);
      Double_t dr = d - par[2];
      f += dr*dr/(4.7*4.7);
    }
  } // iflag=4 for Minuit
}

/********************************************//**
 * Ring Chi2
 ************************************************/

Double_t RICHReconstruction::RingChi2(Double_t *par)
{
  UInt_t nHits = fHitPosForChiFit.size();
  Double_t f = 0.;
  for (UInt_t iHit = 0; iHit < nHits; iHit++)
  {
    TVector2 PMPosition = fHitPosForChiFit[iHit];
    Double_t u = PMPosition.X() - par[0];
    Double_t v = PMPosition.Y() - par[1];
    Double_t d = TMath::Sqrt(u*u+v*v);
    Double_t dr = par[2] - d;
    f += dr*dr/(4.7*4.7);
  }
  return f/(nHits-3);
}

/********************************************//**
 * Applies Ptolemy algorythm for
 * hit ring clusterization
 ************************************************/

Bool_t RICHReconstruction::MultiRingReco(TRecoRICHCandidate *Candidate){

  for(Int_t iPtol=0;iPtol<4;iPtol++) fPtolHitPos[iPtol].Set(0.,0.); //reset

  Int_t nHits = Candidate->GetNHits();
  Mapping(Candidate);

  // iSide:
  // 0. Left
  // 1. Right
  // 2. Down
  // 3. Up
  // 4. 45 Degree Left
  // 5. 45 Degree Right
  // 6. 45 Degree Down
  // 7. 45 Degree Up

  for (Int_t iSide = 0; iSide < 8; iSide++)
  {
    for (Int_t iHit = 0; iHit < Candidate->GetNHits(); iHit++) static_cast<TRecoRICHHit *>( Candidate->GetHit(iHit))->SetIsOnCircle(kFALSE);
    TRecoRICHCandidate *NewCandidate = 0;
    fnHitRes = nHits;
    Int_t jSide = 0;

    if (!StartingTriplet(iSide,Candidate)) continue;
    NewCandidate = HitAssociation(Candidate);
    if (!NewCandidate) continue;
    fCandSideAss[iSide] = (fRecoEvent->GetNCandidates() - 1);
    HitReAssociation(Candidate,NewCandidate);

    while(fnHitRes > 0 && jSide < 8)
    {
      TRecoRICHCandidate *NewCand = 0;
      if (!StartingTriplet(jSide,Candidate))
      {
        jSide++;
        continue;
      }
      NewCand = HitAssociation(Candidate);
      if (!NewCand)
      {
        jSide++;
        continue;
      }
      HitReAssociation(Candidate,NewCand);
      jSide++;
    }
  }
  return kTRUE;
}

/********************************************//**
 * Sorting fSortMapX and fSortMapY
 * Sorting with 45 degree rotation
 ************************************************/

typedef std::pair<Int_t,TVector3> SortPair;
Bool_t ComparatorX ( const SortPair &a, const SortPair &b) { return (a.second).X() < (b.second).X(); }
Bool_t ComparatorY ( const SortPair &a, const SortPair &b) { return (a.second).Y() < (b.second).Y(); }

/********************************************//**
 * Filling fSortMapX and fSortMapY
 ************************************************/

void RICHReconstruction::Mapping(TRecoRICHCandidate *Candidate)
{
  fSortMapX.clear();
  fSortMapY.clear();
  fSortMapXY.clear();
  fSortMapYX.clear();
  Int_t nHits = Candidate->GetNHits();
  for (Int_t iHit = 0; iHit < nHits; iHit++)
  {
    TRecoRICHHit *Hit = static_cast<TRecoRICHHit *>( Candidate->GetHit(iHit));
    fPosPair.first = iHit;
    fPosPair.second = Hit->GetPosition();
    fSortMapX.push_back(fPosPair);
    fSortMapY.push_back(fPosPair);
    (fPosPair.second).RotateZ(TMath::Pi()/4.);
    fSortMapXY.push_back(fPosPair);
    fSortMapYX.push_back(fPosPair);
  }
  std::sort (fSortMapX.begin(), fSortMapX.end(), ComparatorX);
  std::sort (fSortMapY.begin(), fSortMapY.end(), ComparatorY);
  std::sort (fSortMapXY.begin(), fSortMapXY.end(), ComparatorX);
  std::sort (fSortMapYX.begin(), fSortMapYX.end(), ComparatorY);
}

/********************************************//**
 * Choose the starting triplet
 ************************************************/

Bool_t RICHReconstruction::StartingTriplet(Int_t i, TRecoRICHCandidate *Candidate)
{
  std::vector< std::pair<Int_t,TVector3> > fSortMap;

  Int_t nHits = Candidate->GetNHits();

  if (i == 0 || i == 2 || i == 4 || i == 6)
  {
    if (i == 0) fSortMap = fSortMapX;
    if (i == 2) fSortMap = fSortMapY;
    if (i == 4) fSortMap = fSortMapXY;
    if (i == 6) fSortMap = fSortMapYX;
    for (Int_t iHit = 0; iHit < nHits; iHit++)
    {
      Int_t FirstHit = fSortMap[iHit].first;
      TRecoRICHHit *RICHHitFi = static_cast<TRecoRICHHit *>( Candidate->GetHit(FirstHit));

      if (RICHHitFi->GetIsOnCircle()) continue;
      fPtolHitPos[0] = static_cast<RICHChannel*>(fChannels[RICHHitFi->GetChannelSeqID()])->GetCenteredPosition()
        - GetChPosAngCorr(RICHHitFi->GetChannelSeqID());
      fPtolHitChID[2] = FirstHit;
      for (Int_t jHit = iHit+1; jHit < nHits; jHit++)
      {
        Int_t SecondHit = fSortMap[jHit].first;
        TRecoRICHHit *RICHHitSe = static_cast<TRecoRICHHit *>( Candidate->GetHit(SecondHit));

        if (RICHHitSe->GetIsOnCircle()) continue;
        fPtolHitPos[1] = static_cast<RICHChannel*>(fChannels[RICHHitSe->GetChannelSeqID()])->GetCenteredPosition()
          - GetChPosAngCorr(RICHHitSe->GetChannelSeqID());
        fPtolHitChID[1] = SecondHit;
        if (!TripDistance(1,i)) continue;
        for (Int_t kHit = jHit+1; kHit < nHits; kHit++)
        {
          Int_t ThirdHit = fSortMap[kHit].first;
          TRecoRICHHit *RICHHitTh = static_cast<TRecoRICHHit *>( Candidate->GetHit(ThirdHit));

          if (RICHHitTh->GetIsOnCircle()) continue;
          fPtolHitPos[2] = static_cast<RICHChannel*>(fChannels[RICHHitTh->GetChannelSeqID()])->GetCenteredPosition()
            - GetChPosAngCorr(RICHHitTh->GetChannelSeqID());
          fPtolHitChID[0] = ThirdHit;
          if (!TripDistance(2,i)) continue;
          return kTRUE;
        }
      }
    }
    return kFALSE;
  }

  else if (i == 1 || i == 3 || i == 5 || i == 7)
  {
    if (i == 1) fSortMap = fSortMapX;
    if (i == 3) fSortMap = fSortMapY;
    if (i == 5) fSortMap = fSortMapXY;
    if (i == 7) fSortMap = fSortMapYX;
    for (Int_t iHit = nHits-1; iHit >= 0; iHit--)
    {
      Int_t FirstHit = fSortMap[iHit].first;
      TRecoRICHHit *RICHHitFi = static_cast<TRecoRICHHit *>( Candidate->GetHit(FirstHit));

      if (RICHHitFi->GetIsOnCircle()) continue;
      fPtolHitPos[0] = static_cast<RICHChannel*>(fChannels[RICHHitFi->GetChannelSeqID()])->GetCenteredPosition()
        - GetChPosAngCorr(RICHHitFi->GetChannelSeqID());
      fPtolHitChID[0] = FirstHit;
      for (Int_t jHit = iHit-1; jHit >= 0; jHit--)
      {
        Int_t SecondHit = fSortMap[jHit].first;
        TRecoRICHHit *RICHHitSe = static_cast<TRecoRICHHit *>( Candidate->GetHit(SecondHit));


        if (RICHHitSe->GetIsOnCircle()) continue;
        fPtolHitPos[1] = static_cast<RICHChannel*>(fChannels[RICHHitSe->GetChannelSeqID()])->GetCenteredPosition()
          - GetChPosAngCorr(RICHHitSe->GetChannelSeqID());
        fPtolHitChID[1] = SecondHit;
        if (!TripDistance(1,i)) continue;
        for (Int_t kHit = jHit-1; kHit >= 0; kHit--)
        {
          Int_t ThirdHit = fSortMap[kHit].first;
          TRecoRICHHit *RICHHitTh = static_cast<TRecoRICHHit *>( Candidate->GetHit(ThirdHit));

          if (RICHHitTh->GetIsOnCircle()) continue;
          fPtolHitPos[2] = static_cast<RICHChannel*>(fChannels[RICHHitTh->GetChannelSeqID()])->GetCenteredPosition()
            - GetChPosAngCorr(RICHHitTh->GetChannelSeqID());
          fPtolHitChID[2] = ThirdHit;
          if (!TripDistance(2,i)) continue;
          return kTRUE;
        }
      }
    }
    return kFALSE;
  }

  else if (i >= 8)
  {
    for (Int_t iHit = 0; iHit < nHits; iHit++)
    {
      TRecoRICHHit *RICHHitFi = static_cast<TRecoRICHHit *>( Candidate->GetHit(iHit));

      if (RICHHitFi->GetIsOnCircle()) continue;
      fPtolHitPos[0] = static_cast<RICHChannel*>(fChannels[RICHHitFi->GetChannelSeqID()])->GetCenteredPosition()
        - GetChPosAngCorr(RICHHitFi->GetChannelSeqID());
      fPtolHitChID[2] = iHit;
      for (Int_t jHit = iHit+1; jHit < nHits; jHit++)
      {
        TRecoRICHHit *RICHHitSe = static_cast<TRecoRICHHit *>( Candidate->GetHit(jHit));

        if (RICHHitSe->GetIsOnCircle()) continue;
        fPtolHitPos[1] = static_cast<RICHChannel*>(fChannels[RICHHitSe->GetChannelSeqID()])->GetCenteredPosition()
          - GetChPosAngCorr(RICHHitSe->GetChannelSeqID());
        fPtolHitChID[1] = jHit;
        if (!TripDistance(1,i)) continue;
        for (Int_t kHit = jHit+1; kHit < nHits; kHit++)
        {
          TRecoRICHHit *RICHHitTh = static_cast<TRecoRICHHit *>( Candidate->GetHit(kHit));

          if (RICHHitTh->GetIsOnCircle()) continue;
          fPtolHitPos[2] = static_cast<RICHChannel*>(fChannels[RICHHitTh->GetChannelSeqID()])->GetCenteredPosition()
            - GetChPosAngCorr(RICHHitTh->GetChannelSeqID());
          fPtolHitChID[0] = kHit;
          if (!TripDistance(2,i)) continue;
          return kTRUE;
        }
      }
    }
    return kFALSE;
  }

  std::cout << "[RICHReconstruction] WARNING: Something wrong in the triplet association" << std::endl;
  return kFALSE;
}

/********************************************//**
 * Check if triplet points are too close
 ************************************************/

Bool_t RICHReconstruction::TripDistance(Int_t n, Int_t nLoop)
{
  for (Int_t i = 0; i < n; i++)
  {
    TVector2 Diff = fPtolHitPos[i] - fPtolHitPos[n];
    if (Diff.Mod() <= fMinTripDist) return kFALSE;
  }
  if (n == 1 || n == 2)
  {
    TVector2 Difference = fPtolHitPos[n] - fPtolHitPos[0];
    if (nLoop > 3 && nLoop < 8) Difference.Rotate(TMath::Pi()/4.);
    if (fabs(Difference.X()) <= fMinTripDist && (nLoop == 0 || nLoop == 1 || nLoop == 4 || nLoop == 5 ))
    {
      if (fabs(Difference.Y()) < 80) return kFALSE;
    }
    if (fabs(Difference.Y()) <= fMinTripDist && (nLoop == 2 || nLoop == 3 || nLoop == 6 || nLoop == 7 ))
    {
      if (fabs(Difference.X()) < 80) return kFALSE;
    }
  }
  if (n == 2)
  {
    Double_t m01 = (fPtolHitPos[0].Y() - fPtolHitPos[1].Y()) / (fPtolHitPos[0].X() - fPtolHitPos[1].X());
    m01 = -1/m01;
    Double_t y01 = (fPtolHitPos[0].Y() + fPtolHitPos[1].Y()) / 2.;
    Double_t x01 = (fPtolHitPos[0].X() + fPtolHitPos[1].X()) / 2.;
    Double_t q01 = -m01*x01 + y01;
    Double_t m12 = (fPtolHitPos[1].Y() - fPtolHitPos[2].Y()) / (fPtolHitPos[1].X() - fPtolHitPos[2].X());
    m12 = -1/m12;
    Double_t y12 = (fPtolHitPos[1].Y() + fPtolHitPos[2].Y()) / 2.;
    Double_t x12 = (fPtolHitPos[1].X() + fPtolHitPos[2].X()) / 2.;
    Double_t q12 = -m12*x12 + y12;
    Double_t x = (q12-q01) / (m01-m12);
    Double_t y = (m01*q12 - m12*q01) / (m01 - m12);
    TVector2 Center(x,y);
    Double_t Radius = (Center-fPtolHitPos[0]).Mod();
    if (Radius > 200) return kFALSE;
  }
  return kTRUE;
}

/********************************************//**
 * Checks hit by hit if they satisfy Ptolemy
 * conditions and creates a new candidate
 * if necessary
 ************************************************/

TRecoRICHCandidate * RICHReconstruction::HitAssociation(TRecoRICHCandidate *Candidate)
{
  TRecoRICHCandidate * NewCandidate = 0;
  Int_t NHitAssociated = 0;

  for (Int_t iHit = 0; iHit < Candidate->GetNHits(); iHit++)
  {
    TRecoRICHHit *RICHHit = static_cast<TRecoRICHHit *>( Candidate->GetHit(iHit));
    if (RICHHit->GetIsOnCircle()) continue;
    Bool_t FlagTrip = kFALSE;
    for (Int_t iTrip = 0; iTrip < 3; iTrip++)
    {
      if (iHit == fPtolHitChID[iTrip])
      {
        FlagTrip = kTRUE;
        continue;
      }
    }
    if (FlagTrip) continue;
    fPtolHitPos[3] = static_cast<RICHChannel*>(fChannels[RICHHit->GetChannelSeqID()])->GetCenteredPosition()
      - GetChPosAngCorr(RICHHit->GetChannelSeqID());
    Double_t Ptol = PtolemyValue();
    RICHHit->SetPtolemy(Ptol);
    if (Ptol > fPTolCondition) continue;
    NHitAssociated++;
    fnHitRes--;
    if (NHitAssociated == 1)
    {
      NewCandidate = static_cast<TRecoRICHCandidate*>( fRecoEvent->AddCandidate());
      std::sort(fPtolHitChID.begin(), fPtolHitChID.end());
      for (Int_t iTrip = 2; iTrip >= 0; iTrip--)
      {
        Int_t HitIndex = (Candidate->GetHitsIndexes())[fPtolHitChID[iTrip]];
        NewCandidate->AddHit(HitIndex);
        static_cast<TRecoRICHHit *>( Candidate->GetHit(fPtolHitChID[iTrip]))->SetIsOnCircle(kTRUE);
      }
    }
    Int_t HitIndex = (Candidate->GetHitsIndexes())[iHit];
    NewCandidate->AddHit(HitIndex);
    static_cast<TRecoRICHHit *>( Candidate->GetHit(iHit))->SetIsOnCircle(kTRUE);
  }

  if (NewCandidate) return NewCandidate;
  return 0;
}

/********************************************//**
 * Checks if residual hit lies on the same
 * circle of the new candidate
 ************************************************/

void RICHReconstruction::HitReAssociation(TRecoRICHCandidate *Candidate,TRecoRICHCandidate *NewCandidate)
{
  RingFit(NewCandidate);
  for (Int_t iHit = 0; iHit < Candidate->GetNHits(); iHit++)
  {
    TRecoRICHHit *RICHHit = static_cast<TRecoRICHHit *>( Candidate->GetHit(iHit));
    if (RICHHit->GetIsOnCircle()) continue;
    TVector2 HitPos = static_cast<RICHChannel*>(fChannels[RICHHit->GetChannelSeqID()])->GetCenteredPosition()
      - GetChPosAngCorr(RICHHit->GetChannelSeqID());
    TVector2 Distance = NewCandidate->GetRingCenter() - HitPos;
    Double_t Difference = fabs(Distance.Mod() - NewCandidate->GetRingRadius());
    if (Difference > 29) continue;
    fnHitRes--;
    Int_t HitIndex = (Candidate->GetHitsIndexes())[iHit];
    NewCandidate->AddHit(HitIndex);
    static_cast<TRecoRICHHit *>( Candidate->GetHit(iHit))->SetIsOnCircle(kTRUE);
  }
}

/********************************************//**
 * Computes Ptolemy variable
 ************************************************/

Double_t RICHReconstruction::PtolemyValue()
{
  Int_t AHit = 0;
  Int_t BHit = 1;
  Int_t CHit = 2;
  Int_t DHit = 3;

  TVector2 AB;
  TVector2 BC;
  TVector2 CD;
  TVector2 AD;
  TVector2 AC;
  TVector2 BD;

  Double_t MinValue = 1e9;

  for (Int_t i = 0; i < 6; i++)
  {
    if (i == 1 || i == 4)
    {
      Int_t Aux = CHit;
      CHit = DHit;
      DHit = Aux;
    }
    if (i == 2 || i == 5)
    {
      Int_t Aux = BHit;
      BHit = CHit;
      CHit = Aux;
    }
    if (i == 3)
    {
      Int_t Aux = BHit;
      BHit = DHit;
      DHit = Aux;
    }

    AB = fPtolHitPos[AHit] - fPtolHitPos[BHit];
    BC = fPtolHitPos[BHit] - fPtolHitPos[CHit];
    CD = fPtolHitPos[CHit] - fPtolHitPos[DHit];
    AD = fPtolHitPos[AHit] - fPtolHitPos[DHit];
    AC = fPtolHitPos[AHit] - fPtolHitPos[CHit];
    BD = fPtolHitPos[BHit] - fPtolHitPos[DHit];

    Double_t value = AD.Mod()*BC.Mod() + AB.Mod()*CD.Mod() - AC.Mod()*BD.Mod();
    if (value < MinValue) MinValue = value;
  }

  fHPtolemy->Fill(fabs(MinValue));
  return fabs(MinValue);
}

Int_t RICHReconstruction::FromGeoIDtoSeqID(Int_t GeoID){

  Int_t SeqID,DiskID,UpDwID,SCID,OrID,PMID;

  DiskID = GeoID/100000;
  UpDwID = (GeoID-100000*DiskID)/10000;
  SCID = (GeoID-100000*DiskID-10000*UpDwID)/100;
  OrID = (GeoID-100000*DiskID-10000*UpDwID-SCID*100)/10;
  PMID = GeoID-100000*DiskID-10000*UpDwID-SCID*100-OrID*10;

  if(OrID<1){
    SeqID = SCID*8+PMID+UpDwID*61*8+DiskID*61*8*2;
  }else{
    SeqID = 61*8*2*2+SCID+UpDwID*61+DiskID*61*2;
  }
  return SeqID;
}

void RICHReconstruction::ReconstructCandidates(TRecoVEvent*){
  Int_t nHits = fRecoEvent->GetNHits();

  // RecoHits time clusterization: 1 cluster = 1 candidate

  // Two (or less) iterations:
  // 1) Define a new cluster if a hit away more than 0.5*fTimeWindow is found from the existing clusters
  // 2) Remove the "bad" clusters and try to include them in existing "good" clusters


  if (fEnableSuperCells) { // look for SC candidates only if enabled
    TClonesArray  &Hits = (*(fRecoEvent->GetHits()));

    for (Int_t iIter = 0; iIter < fNCandidateClusteringIterations; iIter++) //loop on iterator for SC time clustering
      {
	for (Int_t iHit = 0; iHit < nHits; iHit++) //loop on hits for SC time clustering
	  {

	    TRecoRICHHit *Hit = static_cast<TRecoRICHHit *>(Hits[iHit]);

	    if(!Hit->GetOrSuperCellID()) continue;

	    Bool_t FlagNewCluster = kTRUE;
	    Double_t HitTime = Hit->GetTime();

	    Double_t HitCandidateTime = 9999*ns;
	    Int_t CandidateID = -1;
	    for (Int_t iCand= 0; iCand < fRecoEvent->GetNCandidates(); iCand++)
	      {
		fCandidate = static_cast<TRecoRICHCandidate *>( fRecoEvent->GetCandidate(iCand));
		if (fabs(fCandidate->GetTime() - HitTime) < HitCandidateTime)
		  {
		    HitCandidateTime = fabs (fCandidate->GetTime() - HitTime);
		    CandidateID = iCand;
		  }
	      }
	    if (CandidateID >= 0)
	      {
		fCandidate = static_cast<TRecoRICHCandidate *>( fRecoEvent->GetCandidate(CandidateID));
		if (fabs(fCandidate->GetTime() - HitTime) < 0.5*fTimeWindow)
		  FlagNewCluster = kFALSE;
	      }
	    if (FlagNewCluster)
	      {
		CandidateID = fRecoEvent->GetNCandidates();
		fCandidate = static_cast<TRecoRICHCandidate *>( fRecoEvent->AddCandidate());
	      }
	    Bool_t FlagMissingHit = kTRUE;
	    if (iIter > 0)
	      for (Int_t i = 0; i < fCandidate->GetNHits(); i++)
		if (fCandidate->GetHitsIndexes()[i] == iHit) FlagMissingHit = kFALSE;

	    if (!FlagMissingHit) continue;
	    fCandidate->AddHit(iHit);
	    fCandidate->UpdateTime(HitTime);

	  }//end loop on hits for SC clustering


	// End of iteration: calculate the candidate info
	// (DeltaTimeClosestCandidate, NHitsClosestCandidate, IsSelected)

	for (Int_t iCand = 0; iCand < fRecoEvent->GetNCandidates(); iCand++)
	  {
	    fCandidate = static_cast<TRecoRICHCandidate *>( fRecoEvent->GetCandidate(iCand));
	    Double_t CandidateTime = fCandidate->GetTime();
	    if (fCandidate->GetNHits() >= fMinPMsForEvent)
	      fCandidate->SetIsSelected(kTRUE);
	    Double_t DeltaTimeClosestCandidate = 999*ns;
	    Int_t NHitsClosestCandidate = 0;

	    // Check the minimum time distance from any other candidate
	    for (Int_t jCand = 0; jCand < fRecoEvent->GetNCandidates(); jCand++)
	      {
		TRecoRICHCandidate *OtherCandidate = static_cast<TRecoRICHCandidate *>( fRecoEvent->GetCandidate(jCand));
		if (iCand != jCand && fabs(OtherCandidate->GetTime()-CandidateTime) < fabs(DeltaTimeClosestCandidate))
		  {
		    DeltaTimeClosestCandidate = OtherCandidate->GetTime() - CandidateTime;
		    NHitsClosestCandidate = OtherCandidate->GetNHits();
		  }
	      }
	    fCandidate->SetDeltaTimeClosestCandidate(DeltaTimeClosestCandidate);
	    fCandidate->SetNHitsClosestCandidate(NHitsClosestCandidate);
	  }

	for (Int_t iCand = 0; iCand < fRecoEvent->GetNCandidates(); iCand++)
	  {
	    fCandidate = static_cast<TRecoRICHCandidate*>( fRecoEvent->GetCandidate(iCand));
	    if (iIter < fNCandidateClusteringIterations-1)
	      {
		if (!fCandidate->GetIsSelected() || (fCandidate->GetNHits() < fCandidate->GetNHitsClosestCandidate() &&
						     fCandidate->GetDeltaTimeClosestCandidate() < fTimeWindow)) // remove bad candidates
		  {
		    fRecoEvent->RemoveCandidate(iCand);
		    iCand--;
		  }
		else
		  {
		    for (Int_t iHitCand = 0; iHitCand < fCandidate->GetNHits(); iHitCand++)
		      {
			Double_t HitTime = fCandidate->GetHit(iHitCand)->GetTime();
			if (fabs(HitTime - fCandidate->GetTime()) > 0.5*fTimeWindow)
			  {
			    fCandidate->RemoveHit(iHitCand);
			    fCandidate->UpdateTime();
			    iHitCand--;
			  }
		      }
		  }
	      }
	  }// end of loop on candidates
      }// end of time candidates for SC Time clustering
  }// if SuperCells Enabled


  Int_t nSCCand = 0;
  nSCCand = fRecoEvent->GetNCandidates();
  static_cast<TRecoRICHEvent*>(fRecoEvent)->SetNSCTimeCandidates(nSCCand);

}

void RICHReconstruction::ReconstructPMCandidates(TRecoVEvent*){
  Int_t nHits = fRecoEvent->GetNHits();

  TClonesArray  &Hits = (*(fRecoEvent->GetHits()));

  Int_t nSCCand = 0;
  nSCCand = fRecoEvent->GetNCandidates();

  for (Int_t iIter = 0; iIter < fNCandidateClusteringIterations; iIter++) //loop on iterator for PMT time clustering
  {
    for (Int_t iHit = 0; iHit < nHits; iHit++) //loop on hits for PMT time clustering
    {
      TRecoRICHHit *Hit = static_cast<TRecoRICHHit *>(Hits[iHit]);

      if(Hit->GetOrSuperCellID()) break;

      Bool_t FlagNewCluster = kTRUE;
      Double_t HitTime = Hit->GetTime();

      Double_t HitCandidateTime = 9999*ns;
      Int_t CandidateID = -1;
      for (Int_t iCand= nSCCand; iCand < fRecoEvent->GetNCandidates(); iCand++)
      {
        fCandidate = static_cast<TRecoRICHCandidate *>( fRecoEvent->GetCandidate(iCand));
        if (fabs(fCandidate->GetTime() - HitTime) < HitCandidateTime)
        {
          HitCandidateTime = fabs (fCandidate->GetTime() - HitTime);
          CandidateID = iCand;
        }
      }
      if (CandidateID >= 0)
      {
        fCandidate = static_cast<TRecoRICHCandidate *>( fRecoEvent->GetCandidate(CandidateID));
        if (fabs(fCandidate->GetTime() - HitTime) < 0.5*fTimeWindow)
          FlagNewCluster = kFALSE;
      }
      if (FlagNewCluster)
      {
        CandidateID = fRecoEvent->GetNCandidates();
        fCandidate = static_cast<TRecoRICHCandidate *>( fRecoEvent->AddCandidate());
      }
      Bool_t FlagMissingHit = kTRUE;
      if (iIter > 0)
        for (Int_t i = 0; i < fCandidate->GetNHits(); i++)
        {
          if (fCandidate->GetHitsIndexes()[i] == iHit) FlagMissingHit = kFALSE;
        }
      if (!FlagMissingHit) continue;
      fCandidate->AddHit(iHit);
      fCandidate->UpdateTime(HitTime);

    }//end loop on hits for PMT clustering

    //  calculate the candidate info
    // (DeltaTimeClosestCandidate, NHitsClosestCandidate, IsSelected)

    for (Int_t iCand = nSCCand; iCand < fRecoEvent->GetNCandidates(); iCand++)
    {
      fCandidate = static_cast<TRecoRICHCandidate *>( fRecoEvent->GetCandidate(iCand));
      Double_t CandidateTime = fCandidate->GetTime();
      if (fCandidate->GetNHits() >= fMinPMsForEvent)
        fCandidate->SetIsSelected(kTRUE);
      Double_t DeltaTimeClosestCandidate = 999*ns;
      Int_t NHitsClosestCandidate = 0;

      // Check the minimum time distance from any other candidate
      for (Int_t jCand = nSCCand; jCand < fRecoEvent->GetNCandidates(); jCand++)
      {
        TRecoRICHCandidate *OtherCandidate = static_cast<TRecoRICHCandidate *>( fRecoEvent->GetCandidate(jCand));
        if (iCand != jCand && fabs(OtherCandidate->GetTime()-CandidateTime) < fabs(DeltaTimeClosestCandidate))
        {
          DeltaTimeClosestCandidate = OtherCandidate->GetTime() - CandidateTime;
          NHitsClosestCandidate = OtherCandidate->GetNHits();
        }
      }
      fCandidate->SetDeltaTimeClosestCandidate(DeltaTimeClosestCandidate);
      fCandidate->SetNHitsClosestCandidate(NHitsClosestCandidate);
    }

    for (Int_t iCand = nSCCand; iCand < fRecoEvent->GetNCandidates(); iCand++)
    {
      fCandidate = static_cast<TRecoRICHCandidate*>( fRecoEvent->GetCandidate(iCand));
      if (iIter < fNCandidateClusteringIterations-1)
      {
        if (!fCandidate->GetIsSelected() || (fCandidate->GetNHits() < fCandidate->GetNHitsClosestCandidate() &&
              fCandidate->GetDeltaTimeClosestCandidate() < fTimeWindow)) // remove bad candidates
        {
          fRecoEvent->RemoveCandidate(iCand);
          iCand--;
        }
        else
        {
          for (Int_t iHitCand = 0; iHitCand < fCandidate->GetNHits(); iHitCand++)
          {
            Double_t HitTime = fCandidate->GetHit(iHitCand)->GetTime();
            if (fabs(HitTime - fCandidate->GetTime()) > 0.5*fTimeWindow)
            {
              fCandidate->RemoveHit(iHitCand);
              fCandidate->UpdateTime();
              iHitCand--;
            }
          }
        }
      }
    }// end of loop on PM candidates
  }// end of iterations time candidates for PMT Time clustering
  Int_t nCand = 0;
  nCand = fRecoEvent->GetNCandidates();

  static_cast<TRecoRICHEvent*>(fRecoEvent)->SetNTimeCandidates(nCand);
  Int_t nPMCand = 0;

  nPMCand = nCand-nSCCand;

  static_cast<TRecoRICHEvent*>(fRecoEvent)->SetNPMTimeCandidates(nPMCand);
}
