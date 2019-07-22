//-------------------------------------------------------------------------------
//  Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//  Modified by Viacheslav Duk (Viacheslav.Duk@cern.ch)
//  Modified by Riccardo Lollini (riccardo.lollini@cern.ch) 2017-07-24
//-------------------------------------------------------------------------------
/// \class CHODReconstruction
/// \Brief
/// CHOD reconstruction: takes CHOD digis (signals from PMs), makes CHOD reco hits. Then reco hit pairs \n
/// from the same quadrant and close in time are combined into \n
/// CHOD reco candidates (all combinations of a hit in the V-plane and a hit in the H-plane are considered). \n
/// The candidate time is the average time of two hits, both times are corrected according to their position (T0 correction) and \n
/// (if the appropriate flag is set to 1) their TOT (slewing correction). \n
/// A time candidate is made of all reco hit pairs selected as described above.\n
/// \EndBrief
///
/// \Detailed
/// The CHOD reconstruction makes use of the base class NA62VReconstruction and several \n
/// classes in NA62MC/CHOD/Persistency (TRecoCHODEvent, TRecoCHODCandidate, TRecoCHODHit). \n
/// The CHOD reconstruction basic parameters are set in NA62Reconstruction/config/CHOD.conf \n
/// =========
/// \n
/// CHOD.conf
/// \n
/// =========
/// The configuration of the 2014 Run is in config/CHOD.2014.conf. The 2015 configuration (with the new channel map) is in config/CHOD.2015.conf.\n
/// The parameter description can be found in ./config/CHOD.conf, here the parameters you may want to change to perform your own reconstruction: \n
/// NMaxSlabs= 40   --> CHOD candidates can be produced only for events with N_hits_around_trigger(25ns) < NMaxSlabs \n
/// EnableSlewCorr= 1   --> use (=1) or not use (=0) slewing corrections for the calculation of the candidate time  \n
/// HitEnergyThreshold= 1   --> energy threshold for hits (1 MeV) is used only in the digitization \n
/// StationsT0 (ns) --> used to center Leading Time of TDC around 0. \n
/// \n
/// =======================================================================================================================================================
/// \n
/// CHOD Reconstruction
/// \n
/// The reconstruction starts with the cycle over all digis. For CHOD hits at low threshold \n
/// (CHOD signals are read by two-threshold LAV FEE, low and high) reco hits are created. \n
/// CHOD high threshold hits are not in the readout since 2015. \n
/// \n
/// Next step of the CHOD reconstruction is the cycle over reco hits. \n
/// All hit pairs (a hit in the V-plane, ID from 0 to 63; a hit in the H-plane, ID from 64 to 127) \n
/// are considered as possible CHOD reco candidates. The following hit pairs are sent \n
/// to the QuadrantHitAssiciation subroutine: \n
///  - hit time (uncorrected) difference < 10ns; \n
///  - both hits are in the same quadrant (V1&H1 or V2&H2 or V3&H3 or V4&H4); \n
///  - difference between a hit time and the trigger time is less than 25ns \n
///  - both T0's are good (i.e. |T0|<900), to avoid hit combinations where the slabs do not have a physical crossing region; \n
///  - number of hits (in the event) around the trigger (+- 25ns) is less than NMaxSlabs \n
/// \n
/// Before the last cut all variables (time, coordinates, number of hit pairs) \n
/// for the TimeCandidate are (re)calculated, adding a current hit pair. \n
/// If the number of good hit pairs is greater than 0, a TimeCandidate is produced. \n
/// In the QuadrantHitAssociation subroutine  CHOD reco candidates \n
/// are formed for the following hit pairs: \n
///  - corrected time (T0+slewing or only T0, depending on the value of the flag EnableSlewCorr) difference < 10ns. \n
/// The candidate time is the average time of two hits, both times are corrected according to their position (T0 correction) and \n
/// (if the flag EnableSlewCorr is set to 1) their TOT (slewing correction). Slewing corrections are performed only \n
/// if TOT<15ns for both hits (it corresponds to the main TOT peak). \n
/// The TimeCandidate is made of all good hit pairs. The time of the TimeCandidate is the average time of all hit pairs with T0 corrections. \n
/// The TimeCandidate coordinates are calculated as the average over all hit pairs. \n
/// \n
/// To use the CHOD candidate time as a reference time for other detectors it is recommended to select events with one candidate. \n
/// An example of the code (NA62Analysis): \n
/// \code
///  TRecoCHODEvent *CHODEvent = static_cast<TRecoCHODEvent*>(GetEvent("CHOD"));
///  Int_t nCand_CHOD = CHODEvent->GetNCandidates();
///  FillHisto("hCHOD_NCands",nCand_CHOD);
///  if(CHODEvent->GetNCandidates()==1){
///    TRecoCHODCandidate* CHODCandidate = static_cast<TRecoCHODCandidate*>(CHODEvent->GetCandidate(0));
///    Double_t x_chod = CHODCandidate->GetHitPosition().X();
///    Double_t y_chod = CHODCandidate->GetHitPosition().Y();
///    Int_t *hit_indexes = CHODCandidate->GetHitsIndexes();
///    // get first hit
///    Int_t iHit1 = hit_indexes[0];
///    TRecoCHODHit* RecoHit1 = static_cast<TRecoCHODHit*>(CHODEvent->GetHit(iHit1));
///    // get second hit
///    Int_t iHit2 = hit_indexes[1];
///    TRecoCHODHit* RecoHit2 = static_cast<TRecoCHODHit*>(CHODEvent->GetHit(iHit2));
///    // calculate IPs
///    Int_t IP1 = (RecoHit2->GetChannelID()-64)%16;
///    Int_t IP2 = RecoHit1->GetChannelID()%16;
///  }
/// \endcode
/// \EndDetailed

#include "Riostream.h"

#include "NA62RecoManager.hh"
#include "NA62ConditionsService.hh"
#include "CHODGeometry.hh"
#include "CHODReconstruction.hh"
#include "CHODChannel.hh"
#include "TCHODDigi.hh"

#include "TRecoCHODEvent.hh"
#include "TSlimRecoCHODEvent.hh"
#include "TSpecialTriggerEvent.hh"
#include "TDCBRawDecoder.hh"

#include "TString.h"
#include "TRegexp.h"

CHODReconstruction::CHODReconstruction(TFile* HistoFile, TString ConfigFileName) : NA62VReconstruction(HistoFile, "CHOD", ConfigFileName) {
  // Initialize variables and histos
  //CHODGeometry::GetInstance()->GetWhatYouNeed();
  fRecoEvent = new TRecoCHODEvent();
  fSlimRecoEvent = new TSlimRecoCHODEvent();

  ParseConfFile(ConfigFileName);

  ReadLightVelocities();
  ReadTOT();
  ReadTOTpdf();
  ReadSlewCorr();
}

void CHODReconstruction::Init(NA62VReconstruction* MainReco) {

  //common part for all the subdetectors
  NA62VReconstruction::Init(MainReco);

  TVector2 Position(0.,0.);
  for (Int_t iSlab=0; iSlab<128; iSlab++) {
    if (iSlab<64) {
      Position.SetX(fSlabCenter[iSlab]);
      fChannels[iSlab] = new CHODChannel(Position,1,fRawDecoder->GetDecoder()->GetChannelRO(iSlab));
    }
    else {
      Position.SetY(fSlabCenter[iSlab]);
      fChannels[iSlab] = new CHODChannel(Position,0,fRawDecoder->GetDecoder()->GetChannelRO(iSlab));
    }
  }

  // reading all configuration files
  ReadT0();

  InitHistograms();

}

CHODReconstruction::~CHODReconstruction() {}

void CHODReconstruction::ParseConfFile(TString ConfFileName){

  std::ifstream confFile(ConfFileName.Data());
  if(!confFile.is_open()){
    perror(ConfFileName);
    exit(kWrongConfiguration);
  }
  else std::cout << "[CHODReconstruction] Reading config file: " << ConfFileName << std::endl;

  TString Line;
  while(Line.ReadLine(confFile))
  {
    if (Line.BeginsWith("#")) continue;
    else if(Line.BeginsWith("HighTH="))
    {
      fHighTh = TString(Line(TRegexp("[0-9]+"))).Atof();
    }
    else if(Line.BeginsWith("LowTH="))
    {
      fLowTh = TString(Line(TRegexp("[0-9]+"))).Atof();
    }
    else if(Line.BeginsWith("TimeWindSlew="))
    {
      fTimeWindowForSlew = TString(Line(TRegexp("[0-9]+"))).Atof();
    }
    else if(Line.BeginsWith("TimeIntervalAroundTrigger="))
    {
      fTimeIntervalAroundTrigger = TString(Line(TRegexp("[0-9]+"))).Atof();
    }
    else if(Line.BeginsWith("NMaxSlabs="))
    {
      fNMaxSlabs = TString(Line(TRegexp("[0-9]+"))).Atoi();
    }
    else if(Line.BeginsWith("EnableSlewCorr="))
    {
      fEnableSlewCorr = TString(Line(TRegexp("[0-9]+"))).Atoi();
    }
    else if(Line.BeginsWith("HitEnergyThreshold="))
    {
      fHitEnergyThreshold = TString(Line(TRegexp("[0-9]+"))).Atoi();
    }
    else if(Line.BeginsWith("TOFBetweenCHODPlanes="))
    {
      fTOFBetweenCHODPlanes = TString(Line(TRegexp("-*[0-9]+.*[0-9]+"))).Atof();
    }
    else if(Line.BeginsWith("CHODLightVelocitiesFileInput="))
    {
      TObjArray * l = Line.Tokenize( " " );
      fCHODLightVelocitiesFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
    }
    else if(Line.BeginsWith("CHODSlewCorrFileInput="))
    {
      TObjArray * l = Line.Tokenize( " " );
      fCHODSlewCorrFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
    }
    else if(Line.BeginsWith("CHODTOTFileInput="))
    {
      TObjArray * l = Line.Tokenize( " " );
      fCHODTOTFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
    }
    else if(Line.BeginsWith("CHODTOTpdfFileInput="))
    {
      TObjArray * l = Line.Tokenize( " " );
      fCHODTOTpdfFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
    }

  }

  //for (Int_t iCh=0 ; iCh < fNChannels; iCh++) std::cout << "iCh: " << iCh << "\tx: " << (static_cast<CHODChannel*>(fChannels[iCh])->GetPosition().X() << "\ty: " << (static_cast<CHODChannel*>(fChannels[iCh])->GetPosition().Y() << std::endl));
  confFile.close();

  // check if all files are readable
  if (!fT0FileName.Length())
  {
    std::cout << "Error: CHOD T0 correction file not defined" << std::endl;
    NA62VReconstruction::Exception("Error: CHOD T0 correction file not defined");
  }
  if (!fCHODLightVelocitiesFileName.Length())
  {
    std::cout << "Error: CHOD light velocities file not defined" << std::endl;
    NA62VReconstruction::Exception("Error: CHOD light velocities file not defined");
  }
  if (!fCHODSlewCorrFileName.Length())
  {
    std::cout << "Error: CHOD slewing correction file not defined" << std::endl;
    NA62VReconstruction::Exception("Error: CHOD slewing correction file not defined");
  }
  if (!fCHODTOTFileName.Length())
  {
    std::cout << "Error: CHOD TOT file not defined" << std::endl;
    NA62VReconstruction::Exception("Error: CHOD TOT file not defined");
  }
  if (!fCHODTOTpdfFileName.Length())
  {
    std::cout << "Error: CHOD TOT pdf file not defined" << std::endl;
    NA62VReconstruction::Exception("Error: CHOD TOT pdf file not defined");
  }
}

void CHODReconstruction::InitHistograms() {
  GetOrMakeDir(fHistoFile,"CHODMonitor")->cd();

  // stability plots for the Online Monitor
  if (fHistosLevel>0) {
    fHCandidateTimeWrtReferenceVsBurst = new TH2F
      ("CandidateTimeWrtReferenceVsBurst", "CandidateTimeWrtReferenceVsBurst;Burst ID", 3000, -0.5, 2999.5, 160, -40, 40);
    fHTimeVsSlot = new TH2F("TimeVsSlot","CHOD leading time (low thr) vs Slot", 120, -39.5, 80.5, 3000, -1000.,2000.);
    fHTimeVsID   = new TH2F("TimeVsID","CHOD leading time (low thr) vs CHOD channel ID",128, -0.5, 127.5, 3000, -1000., 2000.);
    fHDeltaLeadingEdgesVsChannelsLow = new TH2F("DeltaLeadingEdgesVsChannelsLow","CHOD (low thr) DeltaLeadingEdgesVsChannelsLow",
        128, -0.5, 127.5, 1000, 0., 200.);
    fHMultPerSlab = new TH2F("MultPerSlab","CHOD Multiplicity Per Slab", 128, -0.5, 127.5, 10, 0.5, 10.5);
  }

  // standard input for T0 computations
  if(fHRecoHitTimeWrtReferenceVsROChannelNoT0){
    fHRecoHitTimeWrtReferenceVsIntersectionIDNoT0 = new TH2F
      ("RecoHitTimeWrtReferenceVsIntersectionIDNoT0",
       "CHOD RecoHitTimeWrtReferenceVsIntersectionIDNoT0", 2048, -0.5, 2047.5, 250, -25., 25.);
    fHRecoHitTimeWrtReferenceVsIntersectionID = new TH2F
      ("RecoHitTimeWrtReferenceVsIntersectionID",
       "CHOD RecoHitTimeWrtReferenceVsIntersectionID", 2048, -0.5, 2047.5, 250, -25., 25.);
    // histogram for L0 T0 computations
    fHRecoHitTimeWrtReferenceVsL0IDNoT0 = new TH2F
      ("RecoHitTimeWrtReferenceVsL0IDNoT0",
       "CHOD RecoHitTimeWrtReferenceVsL0IDNoT0", 128, -0.5, 127.5, 250, -25., 25.);
  }
  fHCandidateTimeWrtReference  = new TH1D("CandidateTimeWrtReference", "CandidateTimeWrtReference", 500, -25., 25.);

  fHEnergyVvsDt          = new TH2F("EnergyInVPlaneVsDt","Energy in V-plane vs Tv-Th",200,-10.,10.,100.,0.,50.);
  fHEnergyHvsDt          = new TH2F("EnergyInHPlaneVsDt","Energy in H-plane vs Tv-Th",200,-10.,10.,100.,0.,50.);

  fHDtUncorr             = new TH1D("DtBeforeCorrections","CHOD Tv-Th before corrections", 200, -100.5*TdcCalib, 99.5*TdcCalib);
  fHDtUncorr2Hits        = new TH1D("DtBeforeCorrections2Hits","CHOD Tv-Th, 2 hit events", 200, -10., 10.);
  fHDtAllCorr            = new TH1D("DtAfterT0AndSlewingCorrections","CHOD Tv-Th after T0 and slewing corrections", 200, -10., 10.);
  fHDtAllCorr2Hits       = new TH1D("DtAfterT0AndSlewingCorrections2Hits","CHOD Tv-Th after T0 and slewing corrections, 2 hit events", 200, -10., 10.);
  fHDtT0Corr             = new TH1D("DtAfterT0Corrections","CHOD Tv-Th after T0 corrections", 200, -10., 10.);
  fHDtT0Corr2Hits        = new TH1D("DtAfterT0Corrections2Hits","CHOD Tv-Th after T0 corrections, 2 hit events", 200, -10., 10.);

  if (fHistosLevel>0) {
    fHTOTvsID              = new TH2F("TOTvsID", "CHOD TOT vs ID", 128, -0.5, 127.5, 300, 0., 30.);
    fHTOTvsID2Hits         = new TH2F("TOTvsID2Hits", "CHOD TOT vs ID, 2 hit events", 128, -0.5, 127.5, 300, 0., 30.);
    fHTOTvsIDvsIP          = new TH3D("TOTvsIDvsIP","x=TOT, y=ID, z=IP", 250, 5., 30., 128, -0.5, 127.5, 16, -0.5, 15.5);
  }

  fHNHitsPerEventLow      = new TH1D("HitPerEventLow","HitPerEventLow", 128, -0.5, 127.5);
  fHChannelOccupancyLow   = new TH1D("ChannelOccupancyLow","ChannelOccupancyLow",128,-0.5,127.5);
  fHHitLeadingTimeLow     = new TH1D("LeadingTimeLow","LeadingTimeLow", 1500, -1000.5, 499.5);
  fHHitWidthLow           = new TH1D("WidthLow","WidthLow", 300, 0.5*TdcCalib, 300.5*TdcCalib);

  fHNErrorWord            = new TH1D("Errors","Errors",50,-0.5,49.5);
  fHNDetectedEdge         = new TH1D("DetectedEdges","CHOD DetectedEdges",4,-0.5,3.5);
  fHNHitsPerEventInTime   = new TH1D("HitPerEventInTime","HitPerEventInTime", 50, -0.5, 49.5);

  if (fHistosLevel>0) {
    Int_t nBin = 32;
    Double_t xBins[33] = {-1210.,-1110.,-1010.,-910.,-810.,-710.,-650.,-580.5,-520.,-450.5,-390.,-320.5,-260.,-190.5,-130.,-60.5,0.,60.5,130.,190.5,260.,320.5,390.,450.5,520.,580.5,650.,710.,810.,910.,1010.,1110.,1210.};
    Double_t yBins[33] = {-1210.,-1110.,-1010.,-910.,-810.,-710.,-650.,-580.5,-520.,-450.5,-390.,-320.5,-260.,-190.5,-130.,-60.5,0.,60.5,130.,190.5,260.,320.5,390.,450.5,520.,580.5,650.,710.,810.,910.,1010.,1110.,1210.};
    fHChannelIllumination = new TH2F("ChannelIllumination","CHOD Channel Illumination",nBin,xBins,nBin,yBins);
  }

  fHNHitsPerEventLow->SetTitle("NHitsPerEventLow");
  fHNHitsPerEventLow->GetXaxis()->SetTitle("NHits");
  fHChannelOccupancyLow->SetTitle("ChannelOccupancyLow");
  fHChannelOccupancyLow->GetXaxis()->SetTitle("Channel");
  fHHitLeadingTimeLow->SetTitle("LeadingTimeLow");
  fHHitLeadingTimeLow->GetXaxis()->SetTitle("Time (ns)");
  fHHitWidthLow->SetTitle("TimeWidthLow");
  fHHitWidthLow->GetXaxis()->SetTitle("Time (ns)");

  if (fHDeltaLeadingEdgesVsChannelsLow) {
    fHDeltaLeadingEdgesVsChannelsLow->GetXaxis()->SetTitle("Channel");
    fHDeltaLeadingEdgesVsChannelsLow->GetYaxis()->SetTitle("Refiring-First Hit Time (ns)");
    fHDeltaLeadingEdgesVsChannelsLow->SetDrawOption("COLZ");
  }

  fHNErrorWord->SetTitle("ErrorWord");
  fHNErrorWord->GetXaxis()->SetTitle("Number of error words");
  fHNDetectedEdge->SetTitle("DetectedEdge");
  fHNDetectedEdge->GetXaxis()->SetTitle("Number of detected Edge");
  fHNHitsPerEventInTime->SetTitle("HitInTime");
  fHNHitsPerEventInTime->GetXaxis()->SetTitle("Number of hit in time");
  if (fHChannelIllumination) {
    fHChannelIllumination->SetTitle("CHOD illumination");
    fHChannelIllumination->GetXaxis()->SetTitle("Position (mm)");
    fHChannelIllumination->GetYaxis()->SetTitle("Position (mm)");
    fHChannelIllumination->SetDrawOption("ZCOL2");
  }
  fHistoFile->cd("/");
}

void CHODReconstruction::SaveHistograms() {
  fHistoFile->cd("CHODMonitor");

  if (fHCandidateTimeWrtReferenceVsBurst) fHCandidateTimeWrtReferenceVsBurst->Write();
  if (fHTimeVsSlot) fHTimeVsSlot->Write();
  if (fHTimeVsID) fHTimeVsID->Write();
  if (fHMultPerSlab) fHMultPerSlab->Write();
  if (fHRecoHitTimeWrtReferenceVsIntersectionIDNoT0) fHRecoHitTimeWrtReferenceVsIntersectionIDNoT0->Write();
  if (fHRecoHitTimeWrtReferenceVsIntersectionID) fHRecoHitTimeWrtReferenceVsIntersectionID->Write();
  if (fHRecoHitTimeWrtReferenceVsL0IDNoT0) fHRecoHitTimeWrtReferenceVsL0IDNoT0->Write();
  fHCandidateTimeWrtReference->Write();

  fHDtUncorr->Write();
  fHDtUncorr2Hits->Write();
  fHDtAllCorr->Write();
  fHDtAllCorr2Hits->Write();
  fHDtT0Corr->Write();
  fHDtT0Corr2Hits->Write();
  if (fHTOTvsID) fHTOTvsID->Write();
  if (fHTOTvsID2Hits) fHTOTvsID2Hits->Write();
  if (fHTOTvsIDvsIP) fHTOTvsIDvsIP->Write();

  fHNHitsPerEventLow->Write();
  fHChannelOccupancyLow->Write();
  fHHitLeadingTimeLow->Write();
  fHHitWidthLow->Write();

  fHNErrorWord->Write();
  fHNDetectedEdge->Write();
  fHNHitsPerEventInTime->Write();
  if (fHChannelIllumination) fHChannelIllumination->Write();

  fHistoFile->cd("/");
}

TDetectorVEvent * CHODReconstruction::Trigger(TDetectorVEvent * tEvent, Event* /*tGenEvent*/) {
  return tEvent;
}

void CHODReconstruction::StartOfBurst(){
  NA62VReconstruction::StartOfBurst(); // common part for all the subdetectors
  fNRecoHitsPerBurst = 0;
  fNCandidatesPerBurst = 0;
}

void CHODReconstruction::EndOfBurst(){
  NA62VReconstruction::EndOfBurst(); // common part for all the subdetectors
}


TRecoVEvent * CHODReconstruction::ProcessEvent(TDetectorVEvent* tEvent, Event* tGenEvent){

  if(tEvent->IsA() == TSpecialTriggerEvent::Class()){
    //read it
    return 0;
  }

  //common part for all the subdetectors
  NA62VReconstruction::ProcessEvent(tEvent, tGenEvent);

  TDCEvent* TdcEvent = static_cast<TDCEvent*>(tEvent);

  Int_t nDigis = TdcEvent->GetNHits();

  fHNErrorWord->Fill(TdcEvent->GetNErrors());

  Int_t nDigisCHodLow;

  if (!nDigis) return fRecoEvent;
  TClonesArray& Digis = (*(TdcEvent->GetHits()));
  nDigisCHodLow=0;

  fNHitsCHodLow = 0;

  Int_t DigiCounterLow[128];
  Double_t FirstLeadingEdgeLow[128];
  for(Int_t iCh=0; iCh<128;iCh++) {
    DigiCounterLow[iCh] = 0;
    FirstLeadingEdgeLow[iCh] = -1e28;
  }

  // a patch for the 2014 run
  // V3-09 (ID=40): low threshold channel is dead (TDC problem), switching to high threshold channel
  if(500<fRecoEvent->GetRunID() && fRecoEvent->GetRunID()<1600){ // (<=2014 data)
    for (Int_t iDigi=0; iDigi<nDigis; iDigi++) {
      TCHODDigi *Digi = static_cast<TCHODDigi*>( Digis[iDigi]);
      if (Digi->GetChannelID()==1040) Digi->SetChannelID(40);
    }
  }
  // end of the patch

  // multiplicity per slab
  Int_t MultPerSlab[128];
  for(Int_t i=0; i<128; i++) MultPerSlab[i]=0;
  for (Int_t iDigi=0; iDigi<nDigis; iDigi++) {
    TCHODDigi *Digi = static_cast<TCHODDigi*>( Digis[iDigi]);
    if (Digi->GetChannelID()<128) {
      MultPerSlab[Digi->GetChannelID()]++;
    }
    Digi->DecodeChannelID();
  }
  if (fHMultPerSlab) {
    for (Int_t i=0; i<128; i++) {
      if (MultPerSlab[i]>0) fHMultPerSlab->Fill(i, MultPerSlab[i]);
    }
  }

  for (Int_t iDigi=0; iDigi<nDigis; iDigi++) {
    TCHODDigi *Digi = static_cast<TCHODDigi*>( Digis[iDigi]);

    //CHOD low thresholds
    if (Digi->GetChannelID()<128) {
      nDigisCHodLow++;
      fHChannelOccupancyLow->Fill(Digi->GetChannelID());
      if (fHTimeVsSlot) fHTimeVsSlot->Fill(Digi->GetSlot(), Digi->GetLeadingEdge() );
      if (fHTimeVsID)   fHTimeVsID->Fill(Digi->GetChannelID(), Digi->GetLeadingEdge() );
      fHHitLeadingTimeLow->Fill(Digi->GetLeadingEdge());
      if (Digi->GetDetectedEdge()==3){
        fHHitWidthLow->Fill(Digi->GetTrailingEdge()-Digi->GetLeadingEdge());
        if (fHTOTvsID) fHTOTvsID->Fill(Digi->GetChannelID(), Digi->GetTrailingEdge()-Digi->GetLeadingEdge());
      }
      if (Digi->GetDetectedEdge()!=2) { // If there is at least a leading edge
        if (!DigiCounterLow[Digi->GetChannelID()]) {
          FirstLeadingEdgeLow[Digi->GetChannelID()] = Digi->GetLeadingEdge();
        }
        else {
          if (fHDeltaLeadingEdgesVsChannelsLow) {
            fHDeltaLeadingEdgesVsChannelsLow->Fill(Digi->GetChannelID(),Digi->GetLeadingEdge()-FirstLeadingEdgeLow[Digi->GetChannelID()]);
          }
        }
        DigiCounterLow[Digi->GetChannelID()]++;
        TRecoCHODHit* RecoHit = static_cast<TRecoCHODHit*>(fRecoEvent->AddHit(Digi));
        RecoHit->SetChannelID(Digi->GetChannelID());
        RecoHit->SetTime(Digi->GetLeadingEdge()-GetT0Correction(Digi)-fChannels[Digi->GetChannelID()]->GetT0());
        Double_t XHit = 0;
        Double_t  YHit = 0;
        if(Digi->GetChannelID()<64) XHit = static_cast<CHODChannel*>(fChannels[Digi->GetChannelID()])->GetPosition().X();
        if(Digi->GetChannelID()>=64) YHit = static_cast<CHODChannel*>(fChannels[Digi->GetChannelID()])->GetPosition().Y();
        TVector3 HitPosition = TVector3 (XHit, YHit, 0.);
        RecoHit->SetPosition(HitPosition);
        if (Digi->GetDetectedEdge()==3) RecoHit->SetTimeWidth(Digi->GetTrailingEdge() - Digi->GetLeadingEdge());
        else RecoHit->SetTimeWidth(0.);
        fNHitsCHodLow++;
      }
    }
    fHNDetectedEdge->Fill(Digi->GetDetectedEdge());
  }

  fHNHitsPerEventLow->Fill(nDigisCHodLow);

  Int_t NRecoHits = fRecoEvent->GetNHits();
  if (!NRecoHits) return fRecoEvent;

  fNRecoHitsPerBurst += NRecoHits;

  Double_t TriggerTime = NA62RecoManager::GetInstance()->GetEventHeader()->GetFineTime()*TdcCalib;
  Int_t NHitsAroundTrigger = 0;
  for(Int_t iRecoHit = 0 ; iRecoHit < fRecoEvent->GetNHits() ; iRecoHit++){
    TRecoCHODHit* RecoHit = static_cast<TRecoCHODHit*>(fRecoEvent->GetHit(iRecoHit));
    if(fabs(RecoHit->GetTime()-TriggerTime)<fTimeIntervalAroundTrigger) NHitsAroundTrigger++;
    RecoHit->DecodeChannelID();
  }

  // variables for the time candidate
  Double_t TimeCandidateTime = 0.;
  Double_t TimeCandidateCoordinateX = 0.;
  Double_t TimeCandidateCoordinateY = 0.;
  Int_t TimeCandidateNHitPairs = 0;
  Int_t TimeCandidateHitIndexV;
  Int_t TimeCandidateHitIndexH;
  Int_t FlagQuadrant[4];
  for(Int_t i=0; i<4; i++) FlagQuadrant[i] = 0;

  // start cycle over reco hits to select good hit pairs
  for(Int_t iRecoHit = 0 ; iRecoHit < NRecoHits ; iRecoHit++){

    TRecoCHODHit* RecoHitV = static_cast<TRecoCHODHit*>(fRecoEvent->GetHit(iRecoHit));
    Int_t CounterV = RecoHitV->GetChannelID();
    Double_t TimeV = RecoHitV->GetTime();
    //Double_t TOTV = RecoHitV->GetTimeWidth();
    if (CounterV >63) continue;
    for (Int_t jRecoHit = 0; jRecoHit < NRecoHits ; jRecoHit++)
    {
      if (jRecoHit == iRecoHit) continue;
      TRecoCHODHit* RecoHitH = static_cast<TRecoCHODHit*>(fRecoEvent->GetHit(jRecoHit));
      Int_t CounterH = RecoHitH->GetChannelID();
      Double_t TimeH = RecoHitH->GetTime();
      //Double_t TOTH = RecoHitH->GetTimeWidth();

      // selection criteria for possible CHOD candidates
      // uncorrected time difference should be < 10 ns
      if (fabs(TimeV - TimeH)>=10.) continue;
      // hits should be in the same quadrant (Q1 condition)
      if( (GetQuadrant(CounterH)-GetQuadrant(CounterV))!=4) continue;
      // both hits should have a trailing
      //  if(TOTV<5. || TOTH<5.) continue;

      // check if hits are close in time with the trigger
      if(fabs(TimeV-TriggerTime)>fTimeIntervalAroundTrigger || fabs(TimeH-TriggerTime)>fTimeIntervalAroundTrigger ) continue;

      // correct hit times with light velocities
      Double_t TimeVCorr = TimeV + LightVelocitiesTimeCorrection(CounterV, CounterH);
      Double_t TimeHCorr = TimeH + LightVelocitiesTimeCorrection(CounterH, CounterV);

      // time and coordinate of the TimeCandidate
      //TimeCandidateTime = TimeCandidateTime + 0.5*(TimeVCorr - T0Correction1 + TimeHCorr - T0Correction2);
      TimeCandidateTime = TimeCandidateTime + 0.5*(TimeVCorr + TimeHCorr);
      TimeCandidateCoordinateX = TimeCandidateCoordinateX + static_cast<CHODChannel*>(fChannels[CounterV])->GetPosition().X();
      TimeCandidateCoordinateY = TimeCandidateCoordinateY + static_cast<CHODChannel*>(fChannels[CounterH])->GetPosition().Y();
      if(TimeCandidateNHitPairs==0){
        TimeCandidateHitIndexV = iRecoHit;
        TimeCandidateHitIndexH = jRecoHit;
      }
      TimeCandidateNHitPairs++;

      // avoid candidates for events with huge multiplicity
      if(NHitsAroundTrigger>fNMaxSlabs) continue;

      // create a standard candidate
      QuadrantHitAssociation(iRecoHit,jRecoHit);

      // mark this quadrant as fired
      Int_t iQuadrant = GetQuadrant(CounterV);
      FlagQuadrant[iQuadrant] = 1;
    }
  }

  // write number of quadrants fired
  Int_t NQuadrantsFired = 0;
  for(Int_t i=0; i<4; i++) NQuadrantsFired = NQuadrantsFired + FlagQuadrant[i];
  static_cast<TRecoCHODEvent*>(fRecoEvent)->SetNQuadrants(NQuadrantsFired);

  // try to create the TimeCandidate
  static_cast<TRecoCHODEvent*>(fRecoEvent)->SetNTimeCandidates(0);
  if(TimeCandidateNHitPairs>0){
    TimeCandidateTime = TimeCandidateTime/TimeCandidateNHitPairs;
    TimeCandidateCoordinateX = TimeCandidateCoordinateX/TimeCandidateNHitPairs;
    TimeCandidateCoordinateY = TimeCandidateCoordinateY/TimeCandidateNHitPairs;

    TRecoCHODCandidate* CHODCandidate = static_cast<TRecoCHODCandidate*>( fRecoEvent->AddCandidate());
    // add two hits to the TimeCandidate
    CHODCandidate->AddHit(TimeCandidateHitIndexV);
    CHODCandidate->AddHit(TimeCandidateHitIndexH);

    TVector2 CenterOfMassPosition = TVector2 (TimeCandidateCoordinateX,TimeCandidateCoordinateY);
    CHODCandidate->SetTime(TimeCandidateTime);
    CHODCandidate->SetHitPosition(CenterOfMassPosition);
    CHODCandidate->SetNHitPairs(TimeCandidateNHitPairs);

    // subtract a time candidate from the total candidate number
    Int_t NumberOfAllCandidates = fRecoEvent->GetNCandidates();
    fRecoEvent->SetNCandidates(NumberOfAllCandidates-1);

    // set number of TimeCandidates
    static_cast<TRecoCHODEvent*>(fRecoEvent)->SetNTimeCandidates(1);
  }

  return fRecoEvent;
}

void CHODReconstruction::QuadrantHitAssociation(Int_t iRecoHit,Int_t jRecoHit) {
  TRecoCHODHit* RecoHitV = static_cast<TRecoCHODHit*>(fRecoEvent->GetHit(iRecoHit));
  TRecoCHODHit* RecoHitH = static_cast<TRecoCHODHit*>(fRecoEvent->GetHit(jRecoHit));
  Int_t CounterV = RecoHitV->GetChannelID();
  Double_t TimeV = RecoHitV->GetTime();
  Double_t TOTV = RecoHitV->GetTimeWidth();
  Int_t CounterH = RecoHitH->GetChannelID();
  Double_t TimeH = RecoHitH->GetTime();
  Double_t TOTH = RecoHitH->GetTimeWidth();
  TVector2 HitPositionV = static_cast<CHODChannel*>(fChannels[CounterV])->GetPosition();
  TVector2 HitPositionH = static_cast<CHODChannel*>(fChannels[CounterH])->GetPosition();
  Int_t Planes[2] = {static_cast<CHODChannel*>(fChannels[CounterV])->GetPlane(),static_cast<CHODChannel*>(fChannels[CounterH])->GetPlane()};
  if (Planes[0] == Planes[1]) return;
  Double_t TimeVCorr = TimeV + LightVelocitiesTimeCorrection(CounterV, CounterH);
  Double_t TimeHCorr = TimeH + LightVelocitiesTimeCorrection(CounterH, CounterV);

  // slewing corrections
  Double_t SlewSlopeV = 0.;
  Double_t SlewSlopeH = 0.;
  Double_t SlewConstantV = 0.;
  Double_t SlewConstantH = 0.;

  //correction for V-plane
  SlewSlopeV = fSlewCorrSlope[CounterV][(CounterH-64)%16];
  SlewConstantV = fSlewCorrConstant[CounterV][(CounterH-64)%16];

  //correction for H-plane
  SlewSlopeH = fSlewCorrSlope[CounterH][CounterV%16];
  SlewConstantH = fSlewCorrConstant[CounterH][CounterV%16];

  // corrected time
  // slewing; slewing corrections are made only for the main TOT peak (TOT<15ns)
  // if TOT>15ns, apply corrections as if TOT=15ns.
  Double_t TOT_offset = 0.; // special offset for slab #79
  if(CounterH==79) TOT_offset = 7.;
  Double_t TOTVCorr, TOTHCorr;
  if (TOTV<15.) TOTVCorr = TOTV;
  else TOTVCorr = 15.;
  if (TOTH-TOT_offset<15.) TOTHCorr = TOTH-TOT_offset;
  else TOTHCorr = 15.;

  if (fEnableSlewCorr && TOTVCorr>0. && TOTHCorr>0.) {
    TimeVCorr = TimeVCorr - SlewSlopeV*TOTVCorr - SlewConstantV;
    TimeHCorr = TimeHCorr - SlewSlopeH*TOTHCorr - SlewConstantH;
  }

  fHDtUncorr->Fill(TimeV+LightVelocitiesTimeCorrection(CounterV, CounterH)+fChannels[CounterV]->GetT0()-
		   (TimeH+LightVelocitiesTimeCorrection(CounterH, CounterV)+fChannels[CounterH]->GetT0()));
  fHDtT0Corr->Fill(TimeV+LightVelocitiesTimeCorrection(CounterV, CounterH) -
		   (TimeH+LightVelocitiesTimeCorrection(CounterH, CounterV)));
  fHDtAllCorr->Fill(TimeVCorr - TimeHCorr);
  if(fNHitsCHodLow==2){
    fHDtUncorr2Hits->Fill(TimeV+LightVelocitiesTimeCorrection(CounterV, CounterH)+fChannels[CounterV]->GetT0()-
			  (TimeH+LightVelocitiesTimeCorrection(CounterH, CounterV)+fChannels[CounterH]->GetT0()));
    fHDtT0Corr2Hits->Fill(TimeV+LightVelocitiesTimeCorrection(CounterV, CounterH) -
			  (TimeH+LightVelocitiesTimeCorrection(CounterH, CounterV)));
    fHDtAllCorr2Hits->Fill(TimeVCorr - TimeHCorr);

    // TOT vs ID for 2 hits
    if (fHTOTvsID2Hits) {
      fHTOTvsID2Hits->Fill(CounterV, TOTV);
      fHTOTvsID2Hits->Fill(CounterH, TOTH);
    }
    if (fHTOTvsIDvsIP) {
      fHTOTvsIDvsIP->Fill(TOTV, CounterV, (CounterH-64)%16);
      fHTOTvsIDvsIP->Fill(TOTH, CounterH, CounterV%16);
    }
  }

  fHEnergyVvsDt->Fill(TimeV - TimeH, RecoHitV->GetEnergy());
  fHEnergyHvsDt->Fill(TimeV - TimeH, RecoHitH->GetEnergy());

  // reject candidates with hits not close in time
  if (fabs(TimeVCorr-TimeHCorr)>10.) return;

  TRecoCHODCandidate* CHODCandidate = static_cast<TRecoCHODCandidate*>( fRecoEvent->AddCandidate());
  CHODCandidate->AddHit(iRecoHit);
  CHODCandidate->AddHit(jRecoHit);
  Double_t AverageTime = 0.5*(TimeVCorr+TimeHCorr);
  TVector2 CandidatePosition = TVector2(HitPositionV.X(),HitPositionH.Y());
  CHODCandidate->SetTime(AverageTime);
  CHODCandidate->SetHitPosition(CandidatePosition);
  CHODCandidate->SetNHitPairs(1);
  if (fHChannelIllumination) fHChannelIllumination->Fill(CandidatePosition.X(),CandidatePosition.Y());

  fNCandidatesPerBurst++;
}

Double_t CHODReconstruction::TimeCorrections(TCHODDigi *DigiLowTh,TDCEvent *TdcEvent) {
  Int_t nDigis = TdcEvent->GetNHits();
  TClonesArray& Digis = (*(TdcEvent->GetHits()));
  for (Int_t iDigi=0; iDigi<nDigis; iDigi++) {
    TCHODDigi *DigiHighTh = static_cast<TCHODDigi*>( Digis[iDigi]);
    if (DigiHighTh->GetChannelID() != (1000+DigiLowTh->GetChannelID())) continue;
    Double_t t1 = DigiLowTh->GetLeadingEdge();
    Double_t t2 = DigiHighTh->GetLeadingEdge();
    if (fabs(t1-t2) <= fTimeWindowForSlew)
    {
      Double_t Time = t1- fLowTh*(t2-t1)/(fHighTh-fLowTh);
      return Time;
    }
  }
  return DigiLowTh->GetLeadingEdge();
}

void CHODReconstruction::ReadSlewCorr() {
  // reading slewing correction constants from file
  if (NA62ConditionsService::GetInstance()->Open(fCHODSlewCorrFileName)!=kSuccess) return;
  Int_t LineIndex = 0;
  TString Line;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fCHODSlewCorrFileName))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    if(l->GetEntries()>=2) {
      fSlewCorrSlope[LineIndex/16][LineIndex%16] = static_cast<TObjString*>(l->At(0))->GetString().Atof();
      fSlewCorrConstant[LineIndex/16][LineIndex%16] = static_cast<TObjString*>(l->At(1))->GetString().Atof();
      LineIndex++;
    }
    delete l;
  }
  if(LineIndex!=2048) std::cerr << "[CHODReconstruction] Error while reading SlewCorr!" << std::endl;
  NA62ConditionsService::GetInstance()->Close(fCHODSlewCorrFileName);
  return;
}

void CHODReconstruction::ReadT0() {
  TString Line;

  // reading T0 corrections from file
  if (NA62ConditionsService::GetInstance()->Open(fT0FileName)!=kSuccess) {
    for(UInt_t iSlab=0;iSlab<128;iSlab++){ //resetting T0s
      fChannels[iSlab]->SetT0(0.);
    }
    return;
  }
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fT0FileName))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    Int_t iSlab = static_cast<TObjString*>(l->At(1))->GetString().Atoi();
    Double_t  t0 = static_cast<TObjString*>(l->At(2))->GetString().Atof();
    if (iSlab>127) continue;
    if (fabs(t0) > 999) t0 = 0.; // -999.999: masked channel, +999.999 failed to compute t0
    fChannels[iSlab]->SetT0(t0);
    delete l;
  }
  NA62ConditionsService::GetInstance()->Close(fT0FileName);
}

void CHODReconstruction::ReadLightVelocities() {
  // reading LightVelocities
  NA62ConditionsService::GetInstance()->Open(fCHODLightVelocitiesFileName);
  Int_t SlabIndex = 0;
  TString Line;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fCHODLightVelocitiesFileName))) {
    if (Line.BeginsWith("#")) continue;
    fLightVelocities[SlabIndex] = Line.Atof();
    SlabIndex++;
  }
  if(SlabIndex!=128) std::cerr << "[CHODReconstruction] Error while reading LightVelocities!" << std::endl;
  NA62ConditionsService::GetInstance()->Close(fCHODLightVelocitiesFileName);
  return;
}

void CHODReconstruction::ReadTOT() {
  // reading TOT
  NA62ConditionsService::GetInstance()->Open(fCHODTOTFileName);
  for(Int_t i=0; i<128; i++){
    NA62ConditionsService::GetInstance()->Get(fCHODTOTFileName) >> fTOTAtPM[i] >> fTOTSlope[i];
  }
  NA62ConditionsService::GetInstance()->Close(fCHODTOTFileName);
  return;
}

void CHODReconstruction::ReadTOTpdf() {
  // reading TOT pdf
  NA62ConditionsService::GetInstance()->Open(fCHODTOTpdfFileName);
  Int_t LineIndex = 0;
  TString Line;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fCHODTOTpdfFileName))) {
    if (Line.BeginsWith("#")) continue;
    fTOTpdf[LineIndex] = Line.Atof();
    LineIndex++;
  }
  if(LineIndex!=12) std::cerr << "[CHODReconstruction] Error while reading TOTpdf!" << std::endl;
  NA62ConditionsService::GetInstance()->Close(fCHODTOTpdfFileName);
  return;
}

Double_t CHODReconstruction::GetHitEnergyThreshold() {
  return fHitEnergyThreshold;
}

Double_t CHODReconstruction::GetSlewCorrSlope(Int_t ID, Int_t IP) {
  Double_t Slope = 0.;
  if(ID>=0 && ID<128 && IP>=0 && IP<16) Slope = fSlewCorrSlope[ID][IP];
  return Slope;
}

Double_t CHODReconstruction::GetSlewCorrConst(Int_t ID, Int_t IP) {
  Double_t Const = 0.;
  if (ID>=0 && ID<128 && IP>=0 && IP<16) Const = fSlewCorrConstant[ID][IP];
  return Const;
}

Double_t CHODReconstruction::GetLightVelocity(Int_t ID) {
  return fLightVelocities[ID];
}

Double_t CHODReconstruction::GetTOTAtPM(Int_t ID) {
  return fTOTAtPM[ID];
}

Double_t CHODReconstruction::GetTOTSlope(Int_t ID) {
  return fTOTSlope[ID];
}

Double_t CHODReconstruction::GetTOTpdf(Int_t i) {
  return fTOTpdf[i];
}

Double_t CHODReconstruction::GetLowThreshold() {
  return fLowTh;
}

Double_t CHODReconstruction::GetHighThreshold() {
  return fHighTh;
}

Double_t CHODReconstruction::GetTOFBetweenCHODPlanes() {
  return fTOFBetweenCHODPlanes;
}


Int_t CHODReconstruction::GetQuadrant(Int_t RecoID) {
  Int_t iQuadrant = 0;
  // low FEE threshold
  if(RecoID<128) iQuadrant = RecoID/16;
  // high FEE threshold
  if(RecoID>=1000 && RecoID<1028) iQuadrant = (RecoID-1000)/16;

  return iQuadrant;
}

Int_t CHODReconstruction::GetPlane(Int_t RecoID) {
  Int_t iPlane = 0;
  // low FEE threshold
  if(RecoID<128) iPlane = RecoID/64;
  // high FEE threshold
  if(RecoID>=1000 && RecoID<1028) iPlane = (RecoID-1000)/64;

  return iPlane;
}

void CHODReconstruction::EndProcessing() {
  NA62VReconstruction::EndProcessing(); // call base class for raw hist output
  // Write histos
  SaveHistograms();
}

void CHODReconstruction::FillTimes(Double_t ReferenceTime) {

  //common part for all the subdetectors
  NA62VReconstruction::FillTimes(ReferenceTime);

  // loop on hit pairs to fill the reference histograms
  for(Int_t iRecoHit = 0 ; iRecoHit < fRecoEvent->GetNHits(); iRecoHit++) {
    TRecoCHODHit* RecoHit = static_cast<TRecoCHODHit*>(fRecoEvent->GetHit(iRecoHit));
    Int_t Counter = RecoHit->GetChannelID();
    Double_t Time = RecoHit->GetTime();
    Double_t TOT = RecoHit->GetTimeWidth();
    if (fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim) {
      fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim->Fill
        (static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(Counter), Time-ReferenceTime+fChannels[Counter]->GetT0());
    }
    // select hit pairs having trailings (TOT>5.) and TOT<15.
    Double_t TOTOffSet = 0.; // special offset for slab #79
    if(Counter==79) TOTOffSet =7.;
    if(TOT<5. || (TOT-TOTOffSet)>15.) continue;

    Int_t NCrossingsAroundReferenceTime = 0;
    for (Int_t jRecoHit = 0; jRecoHit < fRecoEvent->GetNHits(); jRecoHit++) {
      if (jRecoHit == iRecoHit) continue;
      TRecoCHODHit* RecoHitCross = static_cast<TRecoCHODHit*>(fRecoEvent->GetHit(jRecoHit));
      Int_t CounterCross = RecoHitCross->GetChannelID();
      Double_t TimeCross = RecoHitCross->GetTime();
      if(fabs(GetQuadrant(CounterCross)-GetQuadrant(Counter))!=4) continue; // check Q1 condition
      if(fabs(Time-TimeCross)<25.) NCrossingsAroundReferenceTime++;
    }
    if(NCrossingsAroundReferenceTime>1) continue; //clean the sample

    for (Int_t jRecoHit = 0; jRecoHit < fRecoEvent->GetNHits(); jRecoHit++) {
      if (jRecoHit == iRecoHit) continue;
      TRecoCHODHit* RecoHitCross = static_cast<TRecoCHODHit*>(fRecoEvent->GetHit(jRecoHit));
      Int_t CounterCross = RecoHitCross->GetChannelID();
      Double_t TimeCross = RecoHitCross->GetTime() + LightVelocitiesTimeCorrection(CounterCross, Counter);
      Double_t TOTCross = RecoHitCross->GetTimeWidth();

      if(fabs(GetQuadrant(CounterCross)-GetQuadrant(Counter))!=4) continue; // check Q1 condition

      // select hit pairs having trailings (TOT>5.) and TOT<15.
      Double_t TOTOffSetCross = 0.; // special offset for slab #79
      if(CounterCross==79) TOTOffSetCross =7.;
      if(TOTCross<5. || (TOTCross-TOTOffSetCross)>15.) continue;

      Int_t IP = CounterCross%16;

      // T0 histograms for the CHOD slab intersections
      if(fHRecoHitTimeWrtReferenceVsIntersectionIDNoT0){
	fHRecoHitTimeWrtReferenceVsIntersectionIDNoT0->Fill(Counter*16+IP,Time-ReferenceTime+fChannels[Counter]->GetT0());
      }
      // correct hit times with light velocities
      Double_t TimeCorr = Time + LightVelocitiesTimeCorrection(Counter, CounterCross);

      if(fHRecoHitTimeWrtReferenceVsIntersectionID){
        fHRecoHitTimeWrtReferenceVsIntersectionID->Fill(Counter*16+IP,TimeCorr-ReferenceTime);
      }

      if(fHRecoHitTimeWrtReferenceVsROChannelNoT0 ){
        fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(Counter), TimeCorr-ReferenceTime+fChannels[Counter]->GetT0());
      }
      if(fHRecoHitTimeWrtReferenceVsROChannel ){
        fHRecoHitTimeWrtReferenceVsROChannel->Fill(static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(Counter), TimeCorr-ReferenceTime);
      }

      Double_t CandidateTime = 0.5*(TimeCorr + TimeCross);
      fHCandidateTimeWrtReference->Fill(CandidateTime-ReferenceTime);

      // T0 histograms for L0 4x4 supercells
      Int_t L0_IP = IP/4;
      Int_t L0_ID = Counter/4;
      if (fHRecoHitTimeWrtReferenceVsL0IDNoT0) {
        fHRecoHitTimeWrtReferenceVsL0IDNoT0->Fill(L0_ID*4+L0_IP, TimeCorr-ReferenceTime+fChannels[Counter]->GetT0());
      }
    }
  }
}

Double_t CHODReconstruction::LightVelocitiesTimeCorrection(Int_t Counter, Int_t IntersectingCounter) {
  if (Counter >= 0 && Counter < 128 && IntersectingCounter >= 0 && IntersectingCounter < 128)
    return -fLightVelocities[Counter]*fabs(fSlabCenter[IntersectingCounter]-fPMCoordinate[Counter]);
  else
    return 0;
}
