// --------------------------------------------------------------
// History:
//
//
// 2015-03-19 T. Spadaro (tommaso.spadaro@lnf.infn.it)
// - promoting c++ variables to root types whenever possible
// - added management of cluster properties, algorithm type, cluster type, etc.
// - added doxygen compliant documentation
// Totally modified by T. Spadaro - new Reconstruction code 2015-01-22
// Modified by Mauro Raggi 2012-10-22
//   Added InitHistograms, SaveHistograms
// Modified by Vito Palladino 2011-12-08
//   Added recoHit 
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
// --------------------------------------------------------------
/// \class LAVReconstruction
/// \Brief
/// Central class for reconstruction of LAV information: 
/// the Digi hits are translated to Reconstructed hits grouping low and high threshold edges (TRecoLAVHit instances).
/// \EndBrief
/// 
/// \Detailed
/// First, a vector of LAVRecoBlock objects is filled. A LAVRecoBlock instance includes all of the edges of a given block in the event.
/// The edges are then sorted in time and grouped according to the expected order (leading low, leading high, trailing high, trailing low).
/// Finally, time and charge are reconstructed applying slewing corrections.
/// \n Adjacent blocks are grouped by the clustering algorithm (LAVClusterMaker). Blocks close in time and azimuthal angle are grouped by the tracking algorithm (LAVTrakFinder).
/// Clusters are instances of TRecoLAVCandidate.
/// \n The following options are used in the code from the corresponding tags in the LAV.conf datacard:
/// \n fMakeLAVClusters --> MakeLAVClusters (0/1)
/// \n fMakeLAVTracks --> MakeLAVTracks (0/1)
/// \n fPreferredTimeAlgorithm --> PreferredTimeAlgorithm (0--> top priority to usage of slope of leading; 1--> top priority to usage of TOT from low threshold; 2--> top priority to TOT high 
/// \n fApplyResidualSlewingCorrections --> ApplyLAVSlewingCorrections (0/1, if set to 1, the parameters should be in the input t0 file)
/// \n The datacard options for T0 and Threshold setting are explained in the LAVCalibration and LAVConfiguration classes, respectively.
/// \n Example code to retrieve candidate of a given type from a given algorithm:
/// \code 
///  Int_t nLAVRecoCandi = LAVEvent->GetNCandidates();
///  TClonesArray& clusArray = (* (LAVEvent->GetCandidates()));
///  for (Int_t iCl = 0; iCl < nLAVRecoCandi; iCl++) {
///    	TRecoLAVCandidate* clus = static_cast<TRecoLAVCandidate*>( clusArray[iCl]);
///     if(clus->GetAlgorithm() == kTracking) { // or kAdjacentCells
///         if (clus->GetClusterType() == kMIP)     DoMIPAnalysis();
///     else if(clus->GetClusterType() == kShower)  DoShowerAnalysis();
///     else                                        ClassifyUnknownClusters();
///  }
/// \endcode
/// \EndDetailed

#include "Riostream.h"
#include "NA62RecoManager.hh"
#include "NA62Reconstruction.hh"
#include "LAVReconstruction.hh"
#include "TMath.h"

#include "TRecoLAVEvent.hh"
#include "TRecoLAVDigi.hh"
#include "TLAVDigi.hh"
#include "TLAVEvent.hh"
#include "TSpecialTriggerEvent.hh"
#include "TTDCBSpecialTrigger.hh"
#include "NA62Buffer.hh"
#include "NA62BufferProto.hh"
#include "LAVDataCardMessenger.hh"
#include "TDCBRawDecoder.hh"
#include <ctime>


LAVReconstruction::LAVReconstruction(TFile* HistoFile, TString ConfigFileName) :
    NA62VReconstruction(HistoFile, "LAV", ConfigFileName),
    fCalib(nullptr),
    fCandidate(nullptr)
{

    // Initialize variables and histos

    fGeometryInstance = LAVGeometry::GetInstance();

    fRecoEvent = new TRecoLAVEvent();

    fLAVClusterMaker = new LAVClusterMaker();
    fLAVTrackFinder = new LAVTrackFinder();

    fInitialTime = -1;
    fTotalTime = 0;

    LAVDataCardMessenger* DataCard = LAVDataCardMessenger::GetInstance();
    DataCard->ParseConfFile(ConfigFileName);
    DataCard->SetLAVT0CorrectionFileName(fT0FileName); //T0 file name read by NA62VReco::ParseConfFile()
    fMakeLAVT0s = DataCard->HasToMakeLAVT0s();
    fMakeLAVClusters = DataCard->GetLAVClusterMethod();
    fMakeLAVTracks = DataCard->GetLAVTrackMethod();
    fMakeRecoHistos = DataCard->GetRecoHistoLevel();
    fPreferredTimeAlgorithm = DataCard->GetPreferredTimeAlgorithm();
    fThresholdMethod = DataCard->GetLAVThresholdInputMode();
    fLowThreshold = -1;
    fHighThreshold = -1;
    if (DataCard->CheckLAVThresholdNominalValuesProvided()) {
      fLowThreshold = DataCard->GetLAVThresholdLowValue();
      fHighThreshold = DataCard->GetLAVThresholdHighValue();
    }

    fNFiredBlocks = 0;
    for (Int_t i=0; i<MAXBLOCKMAP; i++) {
      fIFiredBlock[i] = -1;
      fGoodHitTimes[i] = -1;
    }

    ResetHistograms();


}

void LAVReconstruction::Init(NA62VReconstruction* MainReco) {
  //common part for all the subdetectors 
  NA62VReconstruction::Init(MainReco);
  Int_t MCFlag = 0;
  if (!static_cast<NA62Reconstruction*>(MainReco)->GetIsRawData()) {
    MCFlag = 1;
  }

  fCalib = LAVCalibration::GetInstance();
  LAVDataCardMessenger* DataCard = LAVDataCardMessenger::GetInstance();
  fCalib->AssignSlewingCorrections(MCFlag); // take data or MC slewing corrections on the basis as needed

  fT0available = fCalib->HasReadT0s();
  fApplyResidualSlewingCorrections = DataCard->GetLAVSlewingCorrectionMethod();
  fResidualSlewingAvailable = fCalib->HasReadResidualSlewingCorrections();
  if (fResidualSlewingAvailable) 
    for (Int_t i=0; i<2;i++)
      for (Int_t j=0; j<MAXPARSPERTHRESHOLD; j++) fResidualSlewPars[i][j] = fCalib->GetResidualSlewingCorrectionPar(i,j);

  InitHistograms();
}

LAVReconstruction::~LAVReconstruction(){
  delete fLAVClusterMaker;
  delete fLAVTrackFinder;
  DeleteHistograms();
}


TDetectorVEvent * LAVReconstruction::Trigger(TDetectorVEvent * tEvent, Event* /*tGenEvent*/){
  
    return tEvent;
}

void LAVReconstruction::StartOfBurst(){
  NA62VReconstruction::StartOfBurst(); // common part for all the subdetectors
  fStatus = -99;
  for (Int_t i=0; i<MAXBLOCKMAP; i++) {
    fGoodHitTimes[i] = -1;
  }
  fTotalEventsPerBurst = 0;
  //output file for bad channels list
  Int_t RunID = NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID();
  Int_t BurstID = NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID();
  fLAVDeadCh.open(Form("LAV-DeadChannels.run%06d_%04d-run%06d_%04d.dat",RunID, BurstID, RunID, BurstID));
  fLAVDeadChEOB.open(Form("LAV-DeadChannelsEOB.run%06d_%04d-run%06d_%04d.dat",RunID, BurstID, RunID, BurstID));
  fLAVNoisyCh.open(Form("LAV-NoisyChannels.run%06d_%04d-run%06d_%04d.dat",RunID, BurstID, RunID, BurstID));
  fLAVNoisyChEOB.open(Form("LAV-NoisyChannelsEOB.run%06d_%04d-run%06d_%04d.dat",RunID, BurstID, RunID, BurstID));
  fLAVBadBurst.open(Form("LAV-BadBurst.run%06d_%04d-run%06d_%04d.dat",RunID, BurstID, RunID, BurstID));
  fLAVBadBurstEOB.open(Form("LAV-BadBurstEOB.run%06d_%04d-run%06d_%04d.dat",RunID, BurstID, RunID, BurstID));
}

void LAVReconstruction::EndOfBurst(){
  NA62VReconstruction::EndOfBurst(); // common part for all the subdetectors
  
  Int_t LAVRange[12] = {320, 320, 320, 320, 320, 480, 480, 480, 512, 512, 512, 512};
  Int_t LAVCompactRange[12] = {160, 160, 160, 160, 160, 240, 240, 240, 240, 240, 240, 256};
  Int_t DeadLAVsCode = 0;
  Int_t TotLAVHit[12] = {0};
  Double_t MeanLAVHit[12] = {0};
  Int_t nNoisyCh[12] = {0};
  Int_t nDeadCh[12] = {0};
  vector <Int_t> deadChannels;
  vector <Int_t> noisyChannels;
  vector <Int_t> deadLAVs;
  vector <Int_t> noisyLAVs;
  Int_t Min = -1, Max = -1;                                                                                                              
  Int_t CountsXCh = 0;                                                                                                                   
  Int_t GeoCh = -1;
  Int_t TotLAVCh[12] = {0};  
  deadChannels.clear();
  noisyChannels.clear();
  deadLAVs.clear();
  noisyLAVs.clear();
  
  if(static_cast<NA62Reconstruction*>(fMainReco)->GetNProcessedEOBEventsInFile() == 0 && fStatus == -99) fStatus = -1;
  else if(static_cast<NA62Reconstruction*>(fMainReco)->GetNProcessedEOBEventsInFile() == 0)              fStatus = -3; //It should never happen
  
  if(fStatus == 1){
    for(Int_t lav=0; lav<12; lav++){                                                                                                       
      if(fHEOBChannelOccupancy[lav]->Integral() == 0){
	deadLAVs.push_back(lav+1);
	DeadLAVsCode += pow(2,lav);
	fStatus = 0;
	continue;
      }
      //Mean Counts. Max and Min count removed
      Min = fHEOBChannelOccupancy[lav]->GetBinContent(1);
      Max = fHEOBChannelOccupancy[lav]->GetBinContent(1);
      for(Int_t n=0; n<LAVRange[lav]; n++){
	GeoCh = fRawDecoder->GetDecoder()->GetChannelRemap(lav*512 + n);
        if(GeoCh != -1){
	  CountsXCh = fHEOBChannelOccupancy[lav]->GetBinContent(n+1);
	  if(CountsXCh < 10){
	    nDeadCh[lav]++;
	    deadChannels.push_back(GeoCh);
	  } else {
	    TotLAVHit[lav]+=CountsXCh;
	    TotLAVCh[lav]++;
	  }
	}
        if(CountsXCh<Min) Min = CountsXCh;
        if(CountsXCh>Max) Max = CountsXCh;
      }
      MeanLAVHit[lav] = (TotLAVHit[lav]-Min-Max)/((double)TotLAVCh[lav]-2);
      for(Int_t n=0; n<LAVRange[lav]; n++){
        GeoCh = fRawDecoder->GetDecoder()->GetChannelRemap(lav*512 + n);
        if(GeoCh != -1){
          CountsXCh = fHEOBChannelOccupancy[lav]->GetBinContent(n+1);
	  if(CountsXCh > (MeanLAVHit[lav]*10 +1)){
	    noisyChannels.push_back(GeoCh);
	    nNoisyCh[lav]++;
	  }
	}
      }
      if(nNoisyCh[lav] > 15) {
	noisyLAVs.push_back(lav+1);
      }
      if(nDeadCh[lav] > 15) {
	deadLAVs.push_back(lav+1);
	DeadLAVsCode += pow(2,lav);
	fStatus = 0;
      }
    }
  }

  fLAVBadBurstEOB << Form("%06d %04d",NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID(),NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID()) << " " << fStatus;
  if(fStatus == 0){
    //verified bad burst
    fLAVBadBurstEOB << " " << DeadLAVsCode << " LAV:";
    for(Int_t i=0;i<(Int_t)deadLAVs.size();i++){
      fLAVBadBurstEOB << " " << deadLAVs.at(i);
    }
  }
  fLAVBadBurstEOB << endl;

  if(fStatus == 1){
    //good burst: create bad channels DB
    fLAVDeadChEOB << Form("%06d %04d",NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID(),
			  NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID());
    for(Int_t i=0;i<(Int_t)deadChannels.size();i++){
      fLAVDeadChEOB << " " << deadChannels.at(i);
    }
    fLAVDeadChEOB << endl;
    fLAVNoisyChEOB << Form("%06d %04d",NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID(),
			   NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID());
    for(Int_t i=0;i<(Int_t)noisyChannels.size();i++){
      fLAVNoisyChEOB << " " << noisyChannels.at(i);
    }
    fLAVNoisyChEOB << endl;
  }
  else { //add a dummy line
    fLAVDeadChEOB << Form("%06d %04d -1",NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID(),
        NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID()) << endl;
    fLAVNoisyChEOB << Form("%06d %04d -1",NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID(),
        NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID()) << endl;
  }
  fLAVDeadChEOB.close();
  fLAVNoisyChEOB.close();
  fLAVBadBurstEOB.close();

  DeadLAVsCode = 0;
  for(Int_t i=0;i<12;i++){
    TotLAVHit[i] = 0;
    MeanLAVHit[i] = 0;
    nNoisyCh[i] = 0;
    nDeadCh[i] = 0;
    TotLAVCh[i] = 0;  
  }
  Min = -1; 
  Max = -1;
  Int_t GeoChLow = -1, GeoChHigh = -1;
  CountsXCh = 0;
  deadChannels.clear();
  noisyChannels.clear();
  deadLAVs.clear();
  noisyLAVs.clear();
  fStatus = 1;

  for(Int_t lav=0; lav<12; lav++){
    if(fHNHitLow[lav]->Integral() == 0 && fHNHitHigh[lav]->Integral() == 0){
      deadLAVs.push_back(lav+1);
      DeadLAVsCode += pow(2,lav);
      fStatus = 0;
      continue;
    }
    Min = fHNHitLow[lav]->GetBinContent(1);
    Max = fHNHitHigh[lav]->GetBinContent(1);

    for(Int_t n=0; n<LAVCompactRange[lav]; n++){
      Int_t ExtendedCh = n*2;
      if(lav+1 == 9 || lav+1 == 10 || lav+1 == 11){
	ExtendedCh = ExtendedCh + 8*(Int_t)(n/60);
      }
      GeoChLow = fRawDecoder->GetDecoder()->GetChannelRemap(lav*512 + ExtendedCh);
      if(GeoChLow != -1){
	CountsXCh = fHNHitLow[lav]->GetBinContent(n+1);
	if(CountsXCh == 0){
	  nDeadCh[lav]++;
	  deadChannels.push_back(GeoChLow);
	} else {
	  TotLAVHit[lav]+=CountsXCh;
	  TotLAVCh[lav]++;
	}
      }
      if(CountsXCh<Min) Min = CountsXCh;
      if(CountsXCh>Max) Max = CountsXCh;
      GeoChHigh = fRawDecoder->GetDecoder()->GetChannelRemap(lav*512 + ExtendedCh + 1);
      if(GeoChHigh != -1){
	CountsXCh = fHNHitHigh[lav]->GetBinContent(n+1);
	if(CountsXCh == 0){
	  nDeadCh[lav]++;
	  deadChannels.push_back(GeoChHigh);
	} else {
	  TotLAVHit[lav]+=CountsXCh;
	  TotLAVCh[lav]++;
	}
      }
      if(CountsXCh<Min) Min = CountsXCh;
      if(CountsXCh>Max) Max = CountsXCh;
    }

    MeanLAVHit[lav] = (TotLAVHit[lav]-Min-Max)/((double)TotLAVCh[lav]-2);
    for(Int_t n=0; n<LAVCompactRange[lav]; n++){
      Int_t ExtendedCh = n*2;
      if(lav+1 == 9 || lav+1 == 10 || lav+1 == 11){
	ExtendedCh = ExtendedCh + 8*(Int_t)(n/60);
      }
      GeoChLow = fRawDecoder->GetDecoder()->GetChannelRemap(lav*512 + ExtendedCh);
      if(GeoChLow != -1){
	CountsXCh = fHNHitLow[lav]->GetBinContent(n+1);
	if(CountsXCh > (MeanLAVHit[lav]*10 +1)){
	  noisyChannels.push_back(GeoChLow);
	  nNoisyCh[lav]++;
	}
      }
      GeoChHigh = fRawDecoder->GetDecoder()->GetChannelRemap(lav*512 + ExtendedCh + 1);
      if(GeoChHigh != -1){
	CountsXCh = fHNHitHigh[lav]->GetBinContent(n+1);
	if(CountsXCh > (MeanLAVHit[lav]*10 +1)){
	  noisyChannels.push_back(GeoChHigh);
	  nNoisyCh[lav]++;
	}
      }
    }
    if(nNoisyCh[lav] > 15) {
      noisyLAVs.push_back(lav+1);
    }
    if(nDeadCh[lav] > 15) {
      deadLAVs.push_back(lav+1);
      DeadLAVsCode += pow(2,lav);
      fStatus = 0;
    }
  }

  fLAVBadBurst << Form("%06d %04d",NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID(),
		       NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID()) << " " << fStatus;
  if(fStatus == 0){
    fLAVBadBurst << " " << DeadLAVsCode << " LAV:";
    for(Int_t i=0;i<(Int_t)deadLAVs.size();i++){
      fLAVBadBurst << " " << deadLAVs.at(i);
    }
  }
  fLAVBadBurst << endl;

  if(fStatus == 1){
    fLAVDeadCh << Form("%06d %04d",NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID(),
        NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID());
    for(Int_t i=0;i<(Int_t)deadChannels.size();i++){
      fLAVDeadCh << " " << deadChannels.at(i);
    }
    fLAVDeadCh << endl;
    fLAVNoisyCh << Form("%06d %04d",NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID(),
        NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID());
    for(Int_t i=0;i<(Int_t)noisyChannels.size();i++){
      fLAVNoisyCh << " " << noisyChannels.at(i);
    }
    fLAVNoisyCh << endl;
  }
  else { //add a dummy line
    fLAVDeadCh << Form("%06d %04d -1",NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID(),
        NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID()) << endl;
    fLAVNoisyCh << Form("%06d %04d -1",NA62RecoManager::GetInstance()->GetEventHeader()->GetRunID(),
        NA62RecoManager::GetInstance()->GetEventHeader()->GetBurstID()) << endl;
  }
  fLAVDeadCh.close();
  fLAVNoisyCh.close();
  fLAVBadBurst.close();

}

void LAVReconstruction::Clear(){

  for (Int_t i=0; i<fNFiredBlocks; i++) {      
    fIFiredBlock[fBlockID[i]] = -1;
  }

  for (Int_t i=0; i< (Int_t) fLAVRecoBlocks.size(); i++) {  
    delete fLAVRecoBlocks.at(i);
  }
  fLAVRecoBlocks.clear();

  fNFiredBlocks = 0;

  
}

TRecoVEvent * LAVReconstruction::ProcessEvent(TDetectorVEvent* tEvent, Event* tGenEvent){

  if(tEvent->IsA() == TSpecialTriggerEvent::Class()){
    Bool_t StartOfBurst = kFALSE;
    Bool_t EndOfBurst = kFALSE;
    UInt_t timeSpecialEvent = 0;
    if (tEvent->GetNHits() && isL0SOB(tEvent->GetTriggerType())) StartOfBurst = kTRUE;    
    if (tEvent->GetNHits() && isL0EOB(tEvent->GetTriggerType())) EndOfBurst   = kTRUE;
    if (StartOfBurst || EndOfBurst) timeSpecialEvent = reinterpret_cast<TSpecialTrigger *>(tEvent->GetHit(0))->GetTimeStamp();

    if (fInitialTime == -1 && StartOfBurst) {
      fInitialTime = timeSpecialEvent;
    }
    else if (fInitialTime !=-1 && EndOfBurst) {
      fTotalTime += (timeSpecialEvent-fInitialTime)*ClockPeriod;
      fBurstTime = (timeSpecialEvent-fInitialTime)*ClockPeriod*1E-9;
      fInitialTime = -1;
    }

    //EOB plot Filling                                                                                           
    if(EndOfBurst){
      if(fStatus==-99)   fStatus =  1; //Normal (-99: uninitialised)
      else if(fStatus>0) fStatus = -2; //More than one EOB

      for(Int_t iSpecTrig=0; iSpecTrig<tEvent->GetNHits(); iSpecTrig++){
        TTDCBSpecialTrigger * SpecTrig = reinterpret_cast<TTDCBSpecialTrigger *>(tEvent->GetHit(iSpecTrig));
        if(!SpecTrig) continue;
        if(isL0EOB(SpecTrig->GetTriggerType())){ //get TEL62 EOB information
          PrimCounter* ChannelCounts = SpecTrig->GetCounter("CHANNEL_COUNT_L");
          if(ChannelCounts){
            Int_t TEL62ID = SpecTrig->GetROBoardID();
            if(TEL62ID < 0 || TEL62ID > 11) continue;
            Int_t FPGAID  = SpecTrig->GetFPGAID();
            if(FPGAID < 0 || FPGAID > 3) continue;
            //0-127 channels loop (1 pp):
            for(UInt_t iROChannel=0;iROChannel<ChannelCounts->GetNEntries();iROChannel++){   
	      Int_t ChannelID = ChannelCounts->GetChannelID(iROChannel);
	      Int_t NCounts   = ChannelCounts->GetValue(iROChannel);
	      if(ChannelID<0) continue; // Masked channel
	      Int_t ChannelInPlot = iROChannel + FPGAID*128;
	      fHEOBChannelOccupancy[TEL62ID]->SetBinContent(ChannelInPlot+1, NCounts);
	      fHEOBChannelFrequency[TEL62ID]->SetBinContent(ChannelInPlot+1, NCounts/fBurstTime);
	    }
	  }
	}
      }
    }
    return 0;
  } 
       
  //common part for all the subdetectors 
  NA62VReconstruction::ProcessEvent(tEvent, tGenEvent);

  LAVReconstruction::Clear();


  TLAVEvent* tLAVEvent = static_cast<TLAVEvent*>(tEvent);

  // get the hits and fill edge arrays

  TClonesArray &Digis = (* (tLAVEvent->GetHits()));
  Int_t nDigis = tLAVEvent->GetNHits();

  for( Int_t idigi=0; idigi<nDigis ; idigi++ ){     
    TLAVDigi* Digi = static_cast<TLAVDigi*>(Digis[idigi]);
    Digi->DecodeChannelID(); // Populates geographical information

    Int_t BlockID = Digi->EncodeChannelID(); // Returns ID in the format SSLBBb
    Int_t threshold = Digi->GetThresholdType(); // Obtained from the ChannelID in the format TSSLBBb

    LAVRecoBlock* LAVRB;

    if (fIFiredBlock[BlockID] == -1) {
      LAVRB = new LAVRecoBlock(BlockID);

      fLAVRecoBlocks.push_back(LAVRB);

      fBlockID[fNFiredBlocks] = BlockID;
      fIFiredBlock[BlockID] = fNFiredBlocks++;
    }
    else {
      LAVRB = fLAVRecoBlocks.at(fIFiredBlock[BlockID]);
    }
    if (Digi->GetDetectedEdge() & 1) LAVRB->SetEdge( threshold, 0, Digi->GetLeadingEdge() -GetT0Correction(Digi), Digi ); //CoarseT0 corr added
    if (Digi->GetDetectedEdge() & 2) LAVRB->SetEdge( threshold, 1, Digi->GetTrailingEdge()-GetT0Correction(Digi), Digi ); //CoarseT0 corr added

    Int_t iSt = Digi->GetLAVID();
    Int_t chan = Digi->GetPackedChannelID();

    fHLAVWindow[iSt-1]->Fill(Digi->GetLeadingEdge(), Digi->GetSlot());

    if (threshold==0) {
      fHNHitLow[iSt-1]->Fill(chan);
      if (Digi->GetDetectedEdge() == 3) {
        fHNGoodHitLow[iSt-1]->Fill(chan);
        fHTOTVsChannelLow[iSt-1]->Fill(chan,Digi->GetTrailingEdge()-Digi->GetLeadingEdge());
      }
    }
    else {
      fHNHitHigh[iSt-1]->Fill(chan);
      if (Digi->GetDetectedEdge() == 3) {
        fHNGoodHitHigh[iSt-1]->Fill(chan);
        fHTOTVsChannelHigh[iSt-1]->Fill(chan,Digi->GetTrailingEdge()-Digi->GetLeadingEdge());
      }
    } 

  }

  // Loop on the Reco Blocks

  for (Int_t iReco = 0; iReco < fNFiredBlocks; iReco++) {

    //     fLAVRecoBlocks.at(iReco)->Print();

    fLAVRecoBlocks.at(iReco)->SortEdges();

    // create LAVRecoHits, each containing from 1 up to 4 edges

    fLAVRecoBlocks.at(iReco)->CreateHits();

    fLAVRecoBlocks.at(iReco)->ChargeReconstruct();     

    //     fLAVRecoBlocks.at(iReco)->EnergyReconstruct();     

  }


  // Fill output event classes

  Double_t phiBlockSpan[12] = {11.25, 11.25, 11.25, 11.25, 11.25, 7.5, 7.5, 7.5, 6., 6., 6., 5.625};

  for (Int_t iReco = 0; iReco < fNFiredBlocks; iReco++) {
    Int_t ID = fLAVRecoBlocks.at(iReco)->GetBlockID();

    Int_t NRecoHits = fLAVRecoBlocks.at(iReco)->GetNRecoHits();

    for (Int_t iHit = 0; iHit<NRecoHits; iHit++) {
      LAVRecoHit* Hit = fLAVRecoBlocks.at(iReco)->GetRecoHit(iHit);
      TVHit* hitto = static_cast<TDetectorVEvent*>( fRecoEvent)->AddHit();
      TRecoLAVHit* RecoHit = static_cast<TRecoLAVHit*>( hitto);
      RecoHit->SetChannelID(ID);
      RecoHit->DecodeChannelID(); // populate geographical information, using LAVChannelID
      Int_t edgeMask = Hit->GetEdgeMask();

      if (edgeMask & 1) RecoHit->SetLeadingEdgeLow(Hit->GetLeadingEdgeLow());       
      if (edgeMask & 2) RecoHit->SetLeadingEdgeHigh(Hit->GetLeadingEdgeHigh());       
      if (edgeMask & 4) RecoHit->SetTrailingEdgeHigh(Hit->GetTrailingEdgeHigh());       
      if (edgeMask & 8) RecoHit->SetTrailingEdgeLow(Hit->GetTrailingEdgeLow());       

      Double_t time = 0;
      if (fPreferredTimeAlgorithm == 0) {
        if (edgeMask & 1 && edgeMask & 2) time = Hit->GetTStartResults()[4]; // tStart from Slope
        else if (edgeMask & 1 && edgeMask & 8) time = Hit->GetTStartResults()[0]; // tStart from TOTLow
        else if (edgeMask & 2 && edgeMask & 4) time = Hit->GetTStartResults()[2]; // tStart from TOTHigh
        else if (edgeMask & 1) time = Hit->GetLeadingEdgeLow();
        else if (edgeMask & 2) time = Hit->GetLeadingEdgeHigh(); // here one might subtract the risetime or part of it
        else if (edgeMask & 4) time = Hit->GetTrailingEdgeHigh();
        else if (edgeMask & 8) time = Hit->GetTrailingEdgeLow();
        else {
          std::cerr << "LAVReconstruction >> Wrong edgeMask: should not happen " << edgeMask << std::endl; 
          exit(kGenericError);
        }
      }
      else if (fPreferredTimeAlgorithm == 1) {
        if (edgeMask & 1 && edgeMask & 8) time = Hit->GetTStartResults()[0]; // tStart from TOTLow
        else if (edgeMask & 1 && edgeMask & 2) time = Hit->GetTStartResults()[4]; // tStart from Slope
        else if (edgeMask & 2 && edgeMask & 4) time = Hit->GetTStartResults()[2]; // tStart from TOTHigh
        else if (edgeMask & 1) time = Hit->GetLeadingEdgeLow();
        else if (edgeMask & 2) time = Hit->GetLeadingEdgeHigh(); // here one might subtract the risetime or part of it
        else if (edgeMask & 4) time = Hit->GetTrailingEdgeHigh();
        else if (edgeMask & 8) time = Hit->GetTrailingEdgeLow();
        else {
          std::cerr << "LAVReconstruction >> Wrong edgeMask: should not happen " << edgeMask << std::endl; 
          exit(kGenericError);
        }
      }
      else {
        if (edgeMask & 2 && edgeMask & 4) time = Hit->GetTStartResults()[2]; // tStart from TOTHigh
        else if (edgeMask & 1 && edgeMask & 2) time = Hit->GetTStartResults()[4]; // tStart from Slope
        else if (edgeMask & 1 && edgeMask & 8) time = Hit->GetTStartResults()[0]; // tStart from TOTLow
        else if (edgeMask & 1) time = Hit->GetLeadingEdgeLow();
        else if (edgeMask & 2) time = Hit->GetLeadingEdgeHigh(); // here one might subtract the risetime or part of it
        else if (edgeMask & 4) time = Hit->GetTrailingEdgeHigh();
        else if (edgeMask & 8) time = Hit->GetTrailingEdgeLow();
        else {
          std::cerr << "LAVReconstruction >> Wrong edgeMask: should not happen " << edgeMask << std::endl; 
          exit(kGenericError);
        }
      }
      if (fT0available)      time -= fCalib->GetBlockT0(ID);
      if (fApplyResidualSlewingCorrections>0 && fResidualSlewingAvailable) time -= Hit->GetResidualSlewingCorrection(0,fResidualSlewPars[0],4,fResidualSlewPars[1],5);

      RecoHit->SetTime(time);

      Double_t charge = -1;
      if (edgeMask & 2 && edgeMask & 4) charge = Hit->GetChargeResults()[2]; // charge from TOTHigh
      else if (edgeMask & 1 && edgeMask & 8) charge = Hit->GetChargeResults()[0]; // charge from TOTLow
      else if (edgeMask & 1 && edgeMask & 2) charge = Hit->GetChargeResults()[4]; // charge from Slope
      RecoHit->SetEnergy(charge); // calibration of charge-->energy missing       

      //Fill Phi distribution histo 
      TVector3 HitPosition;
      Int_t noisyCh = 0;
      Int_t lav = RecoHit->GetLAVID(); //ID/10000
      Int_t layer = RecoHit->GetLayerID(); //(ID - lav*10000)/1000
      RecoHit->GetBlockPosition(HitPosition);
      Double_t PhiValue = HitPosition.Phi()*180./TMath::Pi() - phiBlockSpan[lav-1]/2.;
      Double_t span = rand()/RAND_MAX;
      PhiValue = PhiValue + span*phiBlockSpan[ID/10000-1];
      if(ID == 90083 || ID == 113002 || ID == 73100 || ID == 123083 || ID == 103040 || ID == 71041 || ID == 63081) noisyCh = 1;
      fHTRecoHits->Fill(time);
      if(!noisyCh){
        if(time > -1 && time < 50) fHPhiDistributionOnPhys->Fill((lav-1)*30 + layer+1, PhiValue); 
        if(time > 100 && time < 200) fHPhiDistributionOffPhys->Fill((lav-1)*30 + layer+1, PhiValue); 
      }
    }
  }

  // Clusterize adjacent blocks

  if (fMakeLAVClusters) {
    fLAVClusterMaker->Clear();
    fLAVClusterMaker->Clusterize(static_cast<TRecoLAVEvent*>(fRecoEvent));
    //     fLAVClusterMaker->Merge();
    fLAVClusterMaker->ComputeObservables();

    // Trasfer cluster information to output persistent structures

    Int_t NClusters = fLAVClusterMaker->GetNClusters();
    for (Int_t iClus = 0; iClus<NClusters; iClus++) {
      Int_t nHits = fLAVClusterMaker->GetCluster(iClus)->GetNHits();
      if (nHits>=2) {
        TRecoLAVCandidate* RecoCandidate = static_cast<TRecoLAVCandidate*>( fRecoEvent->AddCandidate());
        TransferClusterInformation(RecoCandidate,fLAVClusterMaker->GetCluster(iClus));
        RecoCandidate->SetAlgorithm(kAdjacentCells);
      }
    }
  }

  // Clusterize blocks close in phi and time

  if (fMakeLAVTracks) {
    fLAVTrackFinder->Init(static_cast<TRecoLAVEvent*>(fRecoEvent));
    fLAVTrackFinder->FindTracks(static_cast<TRecoLAVEvent*>(fRecoEvent));
    //     fLAVTrackFinder->Merge();

    // Trasfer cluster information to output persistent structures

    Int_t NClusters = fLAVTrackFinder->GetNClusters();
    for (Int_t iClus = 0; iClus<NClusters; iClus++) {
      TRecoLAVCandidate* RecoCandidate = static_cast<TRecoLAVCandidate*>( fRecoEvent->AddCandidate());

      TransferClusterInformation(RecoCandidate,fLAVTrackFinder->GetCluster(iClus));      
      RecoCandidate->SetAlgorithm(kTracking);

    }
  }


  Int_t nRecoBadHitsPerStation[12]={0};
  Int_t nRecoGoodHitsPerStation[12]={0};

  for (Int_t iReco = 0; iReco < fNFiredBlocks; iReco++) {

    Int_t ID = fLAVRecoBlocks.at(iReco)->GetBlockID();
    Int_t iSt = ID/10000;

    if (iSt<1 || iSt > 12) std::cerr << "LAVReconstruction wrong StationID " << ID << " " << iSt << " " << fLAVRecoBlocks.at(iReco)->GetBlockID() << std::endl;
    Int_t ilay = (ID - iSt*10000)/1000;
    Int_t iban = (ID - iSt*10000 - ilay*1000)/10;
    Int_t ich = ID - iSt*10000 - ilay*1000 - iban*10;
    Int_t idcomp = ich + 4*iban + fGeometryInstance->GetNumberOfBananas(iSt-1)*4*ilay;

    Int_t NRecoHits = fLAVRecoBlocks.at(iReco)->GetNRecoHits();     

    for (Int_t iHit = 0; iHit<NRecoHits; iHit++) {
      LAVRecoHit* Hit = fLAVRecoBlocks.at(iReco)->GetRecoHit(iHit);
      Int_t edgeMask = Hit->GetEdgeMask();

      fHNHit[iSt-1]->Fill(idcomp);
      fHEdgeMask[iSt-1]->Fill(idcomp,edgeMask);
      if (edgeMask == 15 || edgeMask == 9 || edgeMask == 11) { // complete, LL TL, LL LH TL 
        nRecoGoodHitsPerStation[iSt-1]++;

        fHNGoodHit[iSt-1]->Fill(idcomp); 
        fHTimeVsChannel[iSt-1]->Fill(idcomp,Hit->GetLeadingEdgeLow()); // absolute time

        if (fGoodHitTimes[ID] != -1) {
          fHDeltaTimeVsChannel[iSt-1]->Fill(idcomp,Hit->GetLeadingEdgeLow()-fGoodHitTimes[ID]); // difference of absolute time wrt previous hit on the same block
        }
        fGoodHitTimes[ID] = Hit->GetLeadingEdgeLow();
      }
      else {
        fHNBadHit[iSt-1]->Fill(idcomp); // bad hits
        nRecoBadHitsPerStation[iSt-1]++;
      }
      if (edgeMask == 15 || edgeMask == 11) {
        Double_t dtime = Hit->GetTStartResults()[4] - Hit->GetTStartResults()[0];
        fHdTStartVsChannel[iSt-1]->Fill(idcomp,dtime); // complete, LL TL, LL LH TL 
        Double_t dvmax = Hit->GetVMaxResults()[4] - Hit->GetVMaxResults()[0];
        fHdVMaxVsChannel[iSt-1]->Fill(idcomp,dvmax); // complete, LL TL, LL LH TL 
      }

    }
  }

  for (Int_t iSt=0; iSt<12; iSt++) {
    fHNGoodRecoHitsPerStation[iSt]->Fill(nRecoGoodHitsPerStation[iSt]);
    fHNBadRecoHitsPerStation[iSt]->Fill(nRecoBadHitsPerStation[iSt]);
  }

  fTotalEventsPerBurst++;
  return fRecoEvent;

}

void LAVReconstruction::EndProcessing(){
  NA62VReconstruction::EndProcessing(); // call base class for raw hist output
  // Write histos
  SaveHistograms();
}

void LAVReconstruction::FillTimes(Double_t ReferenceTime) {
  //common part for all the subdetectors 
  NA62VReconstruction::FillTimes(ReferenceTime);
  //-- add here the user code to fill histos for T0 evaluation --//

  if (fLowThreshold < 0 || fHighThreshold < 0) return; // at the moment, fillTimes works only if nominal threshold values are provided

  // Fill plots to evaluate residual slewing correction for low threshold only hits

  for (Int_t iReco = 0; iReco < fNFiredBlocks; iReco++) {
    Int_t ID = fLAVRecoBlocks.at(iReco)->GetBlockID();
    //Int_t iSt = ID/10000;
    //Int_t ilay = (ID - iSt*10000)/1000;
    //Int_t iban = (ID - iSt*10000 - ilay*1000)/10;
    Int_t ROch = static_cast<TDCBRawDecoder*>(fRawDecoder->GetDecoder())->GetChannelRO(ID);

    Double_t t0Channel = 0;
    if (fT0available) t0Channel = fCalib->GetBlockT0(ID);

    Int_t NRecoHits = fLAVRecoBlocks.at(iReco)->GetNRecoHits();

    for (Int_t iHit = 0; iHit<NRecoHits; iHit++) {
      LAVRecoHit* Hit = fLAVRecoBlocks.at(iReco)->GetRecoHit(iHit);
      Int_t edgeMask = Hit->GetEdgeMask();

      if (edgeMask != 9 && edgeMask != 11 && edgeMask != 15) continue;
      Double_t residualSlewing = 0;
      if (fApplyResidualSlewingCorrections>0 && fResidualSlewingAvailable) residualSlewing = Hit->GetResidualSlewingCorrection(0,fResidualSlewPars[0],4,fResidualSlewPars[1],5);

      if (edgeMask == 9) { 
        Double_t time = Hit->GetTStartResults()[0]; // tStart from TOTLow, no T0 subtraction
        Double_t dT1 = time - t0Channel - ReferenceTime;  // corrected for station T0 and for the slewing using the signal shape assumption
        fHRecoHitdTimeLowWrtTOTLow->Fill(Hit->GetTrailingEdgeLow()-Hit->GetLeadingEdgeLow(),dT1);
      }
      else {
        Double_t time = Hit->GetTStartResults()[4]; // tStart from risetime, no T0 subtraction
        Double_t dT1 = time - t0Channel - ReferenceTime;  // corrected for station T0 and for the slewing using the signal shape assumption

        Double_t dtimeMethodsSL = Hit->GetTStartResults()[4] - Hit->GetTStartResults()[0];
        Double_t dtimeMethodsHL=0;
        Double_t dtimeMethodsSH = 0;
        if (edgeMask==15) {
          dtimeMethodsHL = Hit->GetTStartResults()[2] - Hit->GetTStartResults()[0];
          dtimeMethodsSH = Hit->GetTStartResults()[4] - Hit->GetTStartResults()[2];
        }

        fHRecoHitdTimeHighWrtdTLead->Fill(Hit->GetLeadingEdgeHigh()-Hit->GetLeadingEdgeLow(),dT1); // Fill plot to evaluate residual slewing correction for hits with both low and high trheshold leading times

	if (fHRecoHitTimeWrtReferenceVsROChannel)         fHRecoHitTimeWrtReferenceVsROChannel->Fill(ROch, dT1-residualSlewing);
        if (fHRecoHitTimeWrtReferenceVsROChannelNoT0)     fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(ROch, dT1-residualSlewing+t0Channel);
        if (fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim) fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim->Fill(ROch, dT1+t0Channel);

        if (fHRecoHitTimeWrtReferenceVsROChannel)         fHRecoHitTimeWrtReferenceVsROChannel->Fill(ROch+1, dT1-residualSlewing);
        if (fHRecoHitTimeWrtReferenceVsROChannelNoT0)     fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(ROch+1, dT1-residualSlewing+t0Channel);
        if (fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim) fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim->Fill(ROch+1, dT1+t0Channel);

        if (NA62RecoManager::GetInstance()->GetEventHeader()->GetL0TPData()->GetDataType()&0x10) {
          if (fHRecoHitTimeWrtReferenceVsROChannelCT)         fHRecoHitTimeWrtReferenceVsROChannelCT->Fill(ROch, dT1 - residualSlewing);
          if (fHRecoHitTimeWrtReferenceVsROChannelNoT0CT)     fHRecoHitTimeWrtReferenceVsROChannelNoT0CT->Fill(ROch, dT1 - residualSlewing + t0Channel);
          if (fHRecoHitTimeWrtReferenceVsROChannelNoT0PrimCT) fHRecoHitTimeWrtReferenceVsROChannelNoT0PrimCT->Fill(ROch, dT1 + t0Channel);

          if (fHRecoHitTimeWrtReferenceVsROChannelCT)         fHRecoHitTimeWrtReferenceVsROChannelCT->Fill(ROch+1, dT1 - residualSlewing);
          if (fHRecoHitTimeWrtReferenceVsROChannelNoT0CT)     fHRecoHitTimeWrtReferenceVsROChannelNoT0CT->Fill(ROch+1, dT1 - residualSlewing + t0Channel);
          if (fHRecoHitTimeWrtReferenceVsROChannelNoT0PrimCT) fHRecoHitTimeWrtReferenceVsROChannelNoT0PrimCT->Fill(ROch+1, dT1 + t0Channel);
        }

        fHRecoHitTimeWrtReferenceVsBurst->Fill(fRecoEvent->GetBurstID(), dT1 - residualSlewing);

        fHRecoHitTimeWrtReferenceVsdTimeSL->Fill(dtimeMethodsSL, dT1 - residualSlewing);
        if (edgeMask==15) {
          fHRecoHitTimeWrtReferenceVsdTimeHL->Fill(dtimeMethodsHL, dT1 - residualSlewing);
          fHRecoHitTimeWrtReferenceVsdTimeSH->Fill(dtimeMethodsSH, dT1 - residualSlewing);
        }
        fHRecoHitTimeWrtReferenceVsRefTime->Fill(ReferenceTime, dT1 - residualSlewing);
      }
    }
  }
}

void LAVReconstruction::TransferClusterInformation(TRecoLAVCandidate* RecoCandidate, LAVCluster* SoftCluster){

  Int_t nHits = SoftCluster->GetNHits();
  for (Int_t iHit=0; iHit<nHits; iHit++) RecoCandidate->AddHit(SoftCluster->GetHitIndex(iHit));       

  RecoCandidate->SetTime(SoftCluster->GetTime());
  RecoCandidate->SetEnergy(SoftCluster->GetEnergy());
  RecoCandidate->SetPosition(SoftCluster->GetPosition());
  RecoCandidate->SetWeightedPosition(SoftCluster->GetWeightedPosition());

  RecoCandidate->SetZUnweightedError(SoftCluster->GetZUnweightedError());
  RecoCandidate->SetPhiUnweightedError(SoftCluster->GetPhiUnweightedError());
  RecoCandidate->SetZWeightedError(SoftCluster->GetZWeightedError());
  RecoCandidate->SetPhiWeightedError(SoftCluster->GetPhiWeightedError());

  // classify clusters on the basis of the longitudinal development

  Int_t nSingleRows = 0;
  Int_t nMultipleRows = 0;
  for (Int_t ilay=0; ilay<5; ilay++) {
    RecoCandidate->SetNHitsPerLayer(ilay,SoftCluster->GetNHitsPerLayer(ilay));
    if (SoftCluster->GetNHitsPerLayer(ilay)==1) nSingleRows++;
    else if (SoftCluster->GetNHitsPerLayer(ilay)>1) nMultipleRows++;
  }
  if (nSingleRows+nMultipleRows > 1) {
    if (nMultipleRows == 0) RecoCandidate->SetClusterType(kMIP); 
    else RecoCandidate->SetClusterType(kShower); 
  }
  else RecoCandidate->SetClusterType(kUnknown); 
}

//////////////////////// Histograms handling //////////////////////////////

void LAVReconstruction::InitHistograms(){
  GetOrMakeDir(fHistoFile,"LAVMonitor")->cd();

  // Histos to evaluate T0 corrections
  Double_t DeltaTime = 40.;
  Int_t nTimeBins = ((Int_t)DeltaTime)*10; //200 ps resolution

  if (fRawDecoder && fRawDecoder->GetDecoder() && fHRecoHitTimeWrtReferenceVsROChannel) {
    Int_t nROCh = fRawDecoder->GetDecoder()->GetNROChannels();
    fHRecoHitTimeWrtReferenceVsROChannelCT = new TH2F
      ("RecoHitTimeWrtReferenceVsReadoutChannelCT", "RecoHit-LAV Time vs RO Channel for CT",
       nROCh, -0.5, nROCh-0.5, nTimeBins, -DeltaTime, DeltaTime);
    fHRecoHitTimeWrtReferenceVsROChannelCT->GetXaxis()->SetTitle("RO channel number");
    fHRecoHitTimeWrtReferenceVsROChannelCT->GetYaxis()->SetTitle("Reconstructed hit time wrt RefDet (ns)");

    fHRecoHitTimeWrtReferenceVsROChannelNoT0CT = new TH2F
      ("RecoHitTimeWrtReferenceVsReadoutChannelNoT0CT", "Uncorrected RecoHit-LAV Time vs RO Channel for CT",
       nROCh, -0.5, nROCh-0.5, nTimeBins, -DeltaTime, DeltaTime);
    fHRecoHitTimeWrtReferenceVsROChannelNoT0CT->GetXaxis()->SetTitle("RO channel number");
    fHRecoHitTimeWrtReferenceVsROChannelNoT0CT->GetYaxis()->SetTitle("Reconstructed hit time wrt RefDet (ns)");

    fHRecoHitTimeWrtReferenceVsROChannelNoT0PrimCT = new TH2F
      ("RecoHitTimeWrtReferenceVsReadoutChannelNoT0PrimCT", "Uncorrected RecoHit-LAV Time vs RO Channel for CT [for Primitives]",
       nROCh, -0.5, nROCh-0.5, nTimeBins, -DeltaTime, DeltaTime);
    fHRecoHitTimeWrtReferenceVsROChannelNoT0PrimCT->GetXaxis()->SetTitle("RO channel number");
    fHRecoHitTimeWrtReferenceVsROChannelNoT0PrimCT->GetYaxis()->SetTitle("Reconstructed hit time wrt RefDet (ns)");
  }

  fHNGoodRecoHitsPerStation = new TH1D*[fNStations];          // Total number of reco hits per station per event
  fHNBadRecoHitsPerStation = new TH1D*[fNStations];           // Total number of bad hits per station per event

  fHNHitLow = new TH1D*[fNStations];          // Number of low-thr digi hit per channel per station
  fHNGoodHitLow = new TH1D*[fNStations];      // Number of good low-thr digi hits (chosen edge configurations) per channel per station
  fHNHitHigh = new TH1D*[fNStations];         // Number of high-thr digi hit per channel per station
  fHNGoodHitHigh = new TH1D*[fNStations];     // Number of good high-thr hits (chosen edge configurations) per channel per station
  fHTOTVsChannelLow = new TH2F*[fNStations];  // ToT vs channel for low threshold, at digi level
  fHTOTVsChannelHigh = new TH2F*[fNStations]; // ToT vs channel for high threshold, at digi level

  fHEOBChannelOccupancy = new TH1D*[fNStations]; // Number of counts at the EOB per channel per station
  fHEOBChannelFrequency = new TH1D*[fNStations]; // EOB Frequency per channel per station
  fHEOBTDCRate = new TH1D*[fNStations]; // EOB Rate Vs TDC

  fHNHit = new TH1D*[fNStations];          // Number of hit per channel per station
  fHEdgeMask = new TH2F*[fNStations];      // Edge mask per channel
  fHNGoodHit = new TH1D*[fNStations];      // Number of good hits (chosen edge configurations) per channel per station
  fHNBadHit = new TH1D*[fNStations];       // Number of bad hits (chosen edge configurations) per channel per station
  fHTimeVsChannel = new TH2F*[fNStations]; // Signal starting time per hit vs channel number for each station
  fHdTStartVsChannel = new TH2F*[fNStations]; // Difference of Signal starting times per hit vs channel number for each station
  fHdVMaxVsChannel = new TH2F*[fNStations];   // Difference of Signal peak per hit vs channel number for each station
  fHDeltaTimeVsChannel = new TH2F*[fNStations]; // Time distance btw consecutive hits per hit pair, vs channel number for each station
  fHNHitFreq = new TH1D*[fNStations];          // Frequency of hits per channel per station
  fHNGoodHitFreq = new TH1D*[fNStations];      // Frequency of good hits (chosen edge configurations) per channel per station
  fHNBadHitFreq = new TH1D*[fNStations];       // Frequency of bad hits (chosen edge configurations) per channel per station
  fHLAVWindow = new TH2F*[fNStations];  

  for(Int_t iStation = 1; iStation < fNStations+1; iStation++){
    Int_t compactBins = 256;
    Int_t extendedBins = 512;
    if (iStation < 12) compactBins = 240;
    if (iStation < 9) extendedBins = 480;
    if (iStation < 6)  compactBins = 160;
    if (iStation < 6)  extendedBins = 320;

    // Digi-level histograms

    fHNHitLow[iStation-1] = new TH1D(Form("LAV%d_DigiLowOccupancy", iStation),Form("LAV%d Low-thr digi hit Occupancy", iStation), compactBins, 0., compactBins);
    fHNGoodHitLow[iStation-1] = new TH1D(Form("LAV%d_GHDigiLowOccupancy", iStation),Form("LAV%d Good Low-thr digi hit Occupancy", iStation), compactBins, 0., compactBins);
    fHNHitHigh[iStation-1] = new TH1D(Form("LAV%d_DigiHighOccupancy", iStation),Form("LAV%d High-thr digi hit Occupancy", iStation), compactBins, 0., compactBins);
    fHNGoodHitHigh[iStation-1] = new TH1D(Form("LAV%d_GHDigiHighOccupancy", iStation),Form("LAV%d Good High-thr digi hit Occupancy", iStation), compactBins, 0., compactBins);

    fHTOTVsChannelLow[iStation-1] = new TH2F(Form("LAV%d_TOTVsChannelLow", iStation),Form("LAV%d TOT vs Channel Low threshold", iStation), 
        compactBins, 0., compactBins, 100, 0., 100.);
    fHTOTVsChannelHigh[iStation-1] = new TH2F(Form("LAV%d_TOTVsChannelHigh", iStation),Form("LAV%d TOT vs Channel High threshold", iStation), 
        compactBins, 0., compactBins, 100, 0., 100.);

    // EOB histograms
    fHEOBChannelOccupancy[iStation-1] = new TH1D(Form("LAV%d_EOBOccupancy", iStation),Form("LAV%d Occupancy at EOB",iStation)
        ,extendedBins,0.,extendedBins);
    fHEOBChannelFrequency[iStation-1] = new TH1D(Form("LAV%d_EOBFrequency", iStation),Form("LAV%d Frequency at EOB",iStation)
        ,extendedBins,0.,extendedBins);
    fHEOBTDCRate[iStation-1] = new TH1D(Form("LAV%d_EOBTDCRate",iStation),Form("LAV%d_EOBTDCRate",iStation),16,-0.5,15.5);

    // Reco-level histograms
    fHNGoodRecoHitsPerStation[iStation-1] = new TH1D(Form("LAV%d_NGoodRecoHits", iStation),Form("LAV%d Good Reco hits per event", iStation), 50, 0., 50);
    fHNBadRecoHitsPerStation[iStation-1] = new TH1D(Form("LAV%d_NBadRecoHits", iStation),Form("LAV%d Bad Reco hits per event", iStation), 50, 0., 50);

    fHEdgeMask[iStation-1] = new TH2F(Form("LAV%d_EdgeMask", iStation),Form("LAV%d EdgeMask", iStation), compactBins, 0., compactBins, 16, 0.5, 16.5);
    fHNHit[iStation-1] = new TH1D(Form("LAV%d_Occupancy", iStation),Form("LAV%d Occupancy", iStation), compactBins, 0., compactBins);
    fHNGoodHit[iStation-1] = new TH1D(Form("LAV%d_GHOccupancy", iStation),Form("LAV%d Good Hit Occupancy", iStation), compactBins, 0., compactBins);
    fHNBadHit[iStation-1] = new TH1D(Form("LAV%d_BHOccupancy", iStation),Form("LAV%d Bad Hit Occupancy", iStation), compactBins, 0., compactBins);
    fHTimeVsChannel[iStation-1] = new TH2F(Form("LAV%d_TimeVsChannel", iStation),Form("LAV%d Time vs Channel Occupancy", iStation), 
        compactBins, 0., compactBins, 100, -500., 1500.);
    fHdTStartVsChannel[iStation-1] = new TH2F(Form("LAV%d_dTStartVsChannel", iStation),Form("LAV%d Time vs Channel Occupancy", iStation), 
        compactBins, 0., compactBins, 100, -5., 5.);
    fHdVMaxVsChannel[iStation-1] = new TH2F(Form("LAV%d_dVMaxVsChannel", iStation),Form("LAV%d Time vs Channel Occupancy", iStation), 
        compactBins, 0., compactBins, 100, -2., 2.);
    fHDeltaTimeVsChannel[iStation-1] = new TH2F(Form("LAV%d_DeltaTimeVsChannel", iStation),Form("LAV%d Time vs Channel Occupancy", iStation), 
        compactBins, 0., compactBins, 100, 0., 1.E10);

    fHLAVWindow[iStation-1] = new TH2F(Form("LAV%d_Window", iStation), Form("LAV%d_Window", iStation), 500, -1000, 4000, 40, -0.5, 39.5);
  }

  fHEOBRateVsLAV = new TH1D("LAV_EOBTotRate", "LAVs Hit Rate", 12, 0.5, 12.5);
  fHPhiDistributionOnPhys = new TH2F("PhiDistributionOnPhys", "Phi Distribution per LAV per Layer on Physics", 
      350, 1., 350., 180, -180., 180.);
  fHPhiDistributionOffPhys = new TH2F("PhiDistributionOffPhys", "Phi Distribution per LAV per Layer off Physics", 
      350, 1., 350., 180, -180., 180.);
  fHTRecoHits = new TH1D("TRecoHits", "Reco Hits Time", 500, -500., 500.);
  // FillTimes histograms

  TString Name;

  //  Name = "RecoHitTimeWrtReferenceVsReadoutChannelNoResidualSlew";
  //  fHRecoHitTimeWrtReferenceVsROChannelNoResidualSlew = new TH2F
  //    (Name, Name, 2*12*256, -0.5, 2*12*256-0.5, 300, -30, 30);
  //  fHRecoHitTimeWrtReferenceVsROChannelNoResidualSlew->GetXaxis()->SetTitle("RO channel ID");
  //  fHRecoHitTimeWrtReferenceVsROChannelNoResidualSlew->GetYaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

  //  Name = "RecoHitTimeWrtReferenceVsReadoutChannel";
  //  fHRecoHitTimeWrtReferenceVsROChannel = new TH2F
  //    (Name, Name, 2*12*256, -0.5, 2*12*256-0.5, 300, -300, 300);
  //  fHRecoHitTimeWrtReferenceVsROChannel->GetXaxis()->SetTitle("RO channel ID");
  //  fHRecoHitTimeWrtReferenceVsROChannel->GetYaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

  //  Name = "RecoHitTimeWrtReferenceVsReadoutChannelNoT0";
  //  fHRecoHitTimeWrtReferenceVsROChannelNoT0 = new TH2F
  //    (Name, Name, 2*12*256, -0.5, 2*12*256-0.5, 300, -30, 30); // 0.2 ns bin for T0 fits
  //  fHRecoHitTimeWrtReferenceVsROChannelNoT0->GetXaxis()->SetTitle("RO channel ID");
  //  fHRecoHitTimeWrtReferenceVsROChannelNoT0->GetYaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

  Name = "RecoHitTimeWrtReferenceVsBurst";
  fHRecoHitTimeWrtReferenceVsBurst = new TH2F(Name, Name, 3000, -0.5, 2999.5, 160, -40, 40);
  fHRecoHitTimeWrtReferenceVsBurst->GetXaxis()->SetTitle("Burst ID");
  fHRecoHitTimeWrtReferenceVsBurst->GetYaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

  Name = "RecoHitdTimeHighWrtdTLead";
  fHRecoHitdTimeHighWrtdTLead = new TH2F(Name,Name,100,0,10,400,-20.,20.);
  fHRecoHitdTimeHighWrtdTLead->GetXaxis()->SetTitle("Difference of High and Low threshold leading times (ns)");
  fHRecoHitdTimeHighWrtdTLead->GetYaxis()->SetTitle("RecoHit time wrt CEDAR (ns)");

  Name = "RecoHitdTimeLowWrtTOTLow";
  fHRecoHitdTimeLowWrtTOTLow = new TH2F(Name,Name,100,0,200,200,-20.,20.);
  fHRecoHitdTimeLowWrtTOTLow->GetXaxis()->SetTitle("Difference of Low threshold trailing and leading times (ns)");
  fHRecoHitdTimeLowWrtTOTLow->GetYaxis()->SetTitle("RecoHit time wrt CEDAR for low-threshold-only hits (ns)");

  Name = "RecoHitdTimeHighWrtdTimeSL";
  fHRecoHitTimeWrtReferenceVsdTimeSL = new TH2F(Name,Name,100,-10,10,400,-20.,20.);
  fHRecoHitTimeWrtReferenceVsdTimeSL->GetXaxis()->SetTitle("Difference of Slope and Low threshold (from TOT) times (ns)");
  fHRecoHitTimeWrtReferenceVsdTimeSL->GetYaxis()->SetTitle("RecoHit time wrt CEDAR for high-low threshold hits (ns)");

  Name = "RecoHitdTimeHighWrtdTimeHL";
  fHRecoHitTimeWrtReferenceVsdTimeHL = new TH2F(Name,Name,100,-10,10,400,-20.,20.);
  fHRecoHitTimeWrtReferenceVsdTimeHL->GetXaxis()->SetTitle("Difference of High (from TOT) and Low (from TOT) times (ns)");
  fHRecoHitTimeWrtReferenceVsdTimeHL->GetYaxis()->SetTitle("RecoHit time wrt CEDAR for high-low threshold hits (ns)");

  Name = "RecoHitdTimeHighWrtdTimeSH";
  fHRecoHitTimeWrtReferenceVsdTimeSH = new TH2F(Name,Name,100,-10,10,400,-20.,20.);
  fHRecoHitTimeWrtReferenceVsdTimeSH->GetXaxis()->SetTitle("Difference of Slope and High threshold (from TOT) times (ns)");
  fHRecoHitTimeWrtReferenceVsdTimeSH->GetYaxis()->SetTitle("RecoHit time wrt CEDAR for high-low threshold hits (ns)");

  Name = "RecoHitdTimeHighWrtRefTime";
  fHRecoHitTimeWrtReferenceVsRefTime = new TH2F(Name,Name,200,-100,100,400,-20.,20.);
  fHRecoHitTimeWrtReferenceVsRefTime->GetXaxis()->SetTitle("Reference times (ns)");
  fHRecoHitTimeWrtReferenceVsRefTime->GetYaxis()->SetTitle("RecoHit time wrt CEDAR for high-low threshold hits (ns)");

  fHistoFile->cd("/");
}

void LAVReconstruction::SaveHistograms(){
  fHistoFile->cd("LAVMonitor");

  //  fHRecoHitTimeWrtReferenceVsROChannelNoResidualSlew->Write();
  //  fHRecoHitTimeWrtReferenceVsROChannel->Write();
  //  fHRecoHitTimeWrtReferenceVsROChannelNoT0->Write();
  if (fHRecoHitTimeWrtReferenceVsROChannelCT) fHRecoHitTimeWrtReferenceVsROChannelCT->Write();
  if (fHRecoHitTimeWrtReferenceVsROChannelNoT0CT) fHRecoHitTimeWrtReferenceVsROChannelNoT0CT->Write();
  if (fHRecoHitTimeWrtReferenceVsROChannelNoT0PrimCT) fHRecoHitTimeWrtReferenceVsROChannelNoT0PrimCT->Write();

  fHRecoHitTimeWrtReferenceVsBurst->Write();
  fHRecoHitdTimeHighWrtdTLead->Write();
  fHRecoHitdTimeLowWrtTOTLow->Write();
  fHRecoHitTimeWrtReferenceVsdTimeSL->Write();
  fHRecoHitTimeWrtReferenceVsdTimeHL->Write();
  fHRecoHitTimeWrtReferenceVsdTimeSH->Write();
  fHRecoHitTimeWrtReferenceVsRefTime->Write();
  fHEOBRateVsLAV->Write();
  fHPhiDistributionOnPhys->Write();
  fHPhiDistributionOffPhys->Write();
  fHTRecoHits->Write();

  for(Int_t iStation = 1; iStation < fNStations+1; iStation++){
    fHNHitLow[iStation-1]->Write();
    fHNGoodHitLow[iStation-1]->Write();
    fHNHitHigh[iStation-1]->Write();
    fHNGoodHitHigh[iStation-1]->Write();
    fHTOTVsChannelLow[iStation-1]->Write();
    fHTOTVsChannelHigh[iStation-1]->Write();

    fHNGoodRecoHitsPerStation[iStation-1]->Write();
    fHNBadRecoHitsPerStation[iStation-1]->Write();
    fHNHit[iStation-1]->Write();
    fHEdgeMask[iStation-1]->Write();
    fHNGoodHit[iStation-1]->Write();
    fHNBadHit[iStation-1]->Write();
    fHTimeVsChannel[iStation-1]->Write();
    fHdTStartVsChannel[iStation-1]->Write();
    fHdVMaxVsChannel[iStation-1]->Write();
    fHDeltaTimeVsChannel[iStation-1]->Write();
    fHEOBChannelOccupancy[iStation-1]->Write();
    fHEOBChannelFrequency[iStation-1]->Write();
    fHEOBTDCRate[iStation-1]->Write();

    // reweight histograms
    fHNHitFreq[iStation-1] = static_cast<TH1D*>( (fHNHit[iStation-1]->Clone(Form("LAV%d_Frequency", iStation))));
    fHNHitFreq[iStation-1]->SetTitle(Form("LAV%d Frequency", iStation));
    fHNHitFreq[iStation-1]->Scale(1./fTotalTime);
    fHNHitFreq[iStation-1]->Write();

    fHNGoodHitFreq[iStation-1] = static_cast<TH1D*>( (fHNGoodHit[iStation-1]->Clone(Form("LAV%d_GHFrequency", iStation))));
    fHNGoodHitFreq[iStation-1]->SetTitle(Form("LAV%d Good Hit Frequency", iStation));
    fHNGoodHitFreq[iStation-1]->Scale(1./fTotalTime);
    fHNGoodHitFreq[iStation-1]->Write();

    fHNBadHitFreq[iStation-1] = static_cast<TH1D*>( (fHNBadHit[iStation-1]->Clone(Form("LAV%d_BHFrequency", iStation))));
    fHNBadHitFreq[iStation-1]->SetTitle(Form("LAV%d Bad Hit Frequency", iStation));
    fHNBadHitFreq[iStation-1]->Scale(1./fTotalTime);
    fHNBadHitFreq[iStation-1]->Write();

    fHLAVWindow[iStation-1]->Write();
  }

  fHistoFile->cd("/");
}

void LAVReconstruction::ResetHistograms() {
  fHRecoHitTimeWrtReferenceVsROChannelCT = 0;
  fHRecoHitTimeWrtReferenceVsROChannelNoT0CT = 0;
  fHRecoHitTimeWrtReferenceVsROChannelNoT0PrimCT = 0;
  fHRecoHitTimeWrtReferenceVsBurst = 0;
  fHRecoHitdTimeHighWrtdTLead = 0;
  fHRecoHitdTimeLowWrtTOTLow = 0;
  fHRecoHitTimeWrtReferenceVsdTimeSL = 0;
  fHRecoHitTimeWrtReferenceVsdTimeHL = 0;
  fHRecoHitTimeWrtReferenceVsdTimeSH = 0;
  fHRecoHitTimeWrtReferenceVsRefTime = 0;
  fHEOBChannelOccupancy = 0;
  fHEOBChannelFrequency = 0;
  fHEOBTDCRate = 0;
  fHEOBRateVsLAV = 0;
  fHPhiDistributionOnPhys = 0;
  fHPhiDistributionOffPhys = 0;
  fHTRecoHits = 0;
  fHNHitLow = 0;
  fHNGoodHitLow = 0;
  fHNHitHigh = 0;
  fHNGoodHitHigh = 0;
  fHTOTVsChannelLow = 0;
  fHTOTVsChannelHigh = 0;
  fHNGoodRecoHitsPerStation = 0;
  fHNBadRecoHitsPerStation = 0;
  fHNHit = 0;
  fHEdgeMask = 0;
  fHNGoodHit = 0;
  fHNBadHit = 0;
  fHTimeVsChannel = 0;
  fHdTStartVsChannel = 0; 
  fHdVMaxVsChannel = 0; 
  fHDeltaTimeVsChannel = 0;
  fHNHitFreq = 0;
  fHNGoodHitFreq = 0;
  fHNBadHitFreq = 0;
  fHLAVWindow = 0;
}

void LAVReconstruction::DeleteHistograms() {

  if(fHRecoHitTimeWrtReferenceVsROChannelCT) delete fHRecoHitTimeWrtReferenceVsROChannelCT;
  if(fHRecoHitTimeWrtReferenceVsROChannelNoT0CT) delete fHRecoHitTimeWrtReferenceVsROChannelNoT0CT;
  if(fHRecoHitTimeWrtReferenceVsROChannelNoT0PrimCT) delete fHRecoHitTimeWrtReferenceVsROChannelNoT0PrimCT;

  if(fHNGoodRecoHitsPerStation)          delete [] fHNGoodRecoHitsPerStation;
  if(fHNBadRecoHitsPerStation)           delete [] fHNBadRecoHitsPerStation;
  if(fHNHitLow)                          delete [] fHNHitLow;
  if(fHNGoodHitLow)                      delete [] fHNGoodHitLow;
  if(fHNHitHigh)                         delete [] fHNHitHigh;
  if(fHNGoodHitHigh)                     delete [] fHNGoodHitHigh;
  if(fHTOTVsChannelLow)                  delete [] fHTOTVsChannelLow;
  if(fHTOTVsChannelHigh)                 delete [] fHTOTVsChannelHigh;
  if(fHNHit)                             delete [] fHNHit;
  if(fHEdgeMask)                         delete [] fHEdgeMask;
  if(fHNGoodHit)                         delete [] fHNGoodHit;
  if(fHNBadHit)                          delete [] fHNBadHit;
  if(fHTimeVsChannel)                    delete [] fHTimeVsChannel;
  if(fHdTStartVsChannel)                 delete [] fHdTStartVsChannel; 
  if(fHdVMaxVsChannel)                   delete [] fHdVMaxVsChannel; 
  if(fHDeltaTimeVsChannel)               delete [] fHDeltaTimeVsChannel;
  if(fHNHitFreq)                         delete [] fHNHitFreq;
  if(fHNGoodHitFreq)                     delete [] fHNGoodHitFreq;
  if(fHNBadHitFreq)                      delete [] fHNBadHitFreq;
  if(fHLAVWindow)                        delete [] fHLAVWindow;
  //EOB Occupancy Plots
  if(fHEOBChannelOccupancy)              delete [] fHEOBChannelOccupancy;
  if(fHEOBChannelFrequency)              delete [] fHEOBChannelFrequency;
  if(fHEOBTDCRate)                       delete [] fHEOBTDCRate;
  if(fHEOBRateVsLAV)                     delete fHEOBRateVsLAV;
  if(fHPhiDistributionOnPhys)            delete fHPhiDistributionOnPhys; 
  if(fHPhiDistributionOffPhys)           delete fHPhiDistributionOffPhys; 
  if(fHTRecoHits)                        delete fHTRecoHits;

  if(fHRecoHitTimeWrtReferenceVsBurst)   delete fHRecoHitTimeWrtReferenceVsBurst;
  if(fHRecoHitdTimeHighWrtdTLead)        delete fHRecoHitdTimeHighWrtdTLead;
  if(fHRecoHitdTimeLowWrtTOTLow)         delete fHRecoHitdTimeLowWrtTOTLow;
  if(fHRecoHitTimeWrtReferenceVsdTimeSL) delete fHRecoHitTimeWrtReferenceVsdTimeSL;
  if(fHRecoHitTimeWrtReferenceVsdTimeHL) delete fHRecoHitTimeWrtReferenceVsdTimeHL;
  if(fHRecoHitTimeWrtReferenceVsdTimeSH) delete fHRecoHitTimeWrtReferenceVsdTimeSH;
  if(fHRecoHitTimeWrtReferenceVsRefTime) delete fHRecoHitTimeWrtReferenceVsRefTime;
  ResetHistograms();
}
