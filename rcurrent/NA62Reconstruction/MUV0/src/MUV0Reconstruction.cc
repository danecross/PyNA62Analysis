// --------------------------------------------------------------
// History:
//
// Adam Conovaloff (Adam.William.Conovaloff@cern.ch) 2016-12-2
// --based on IRC reconstruction
// --------------------------------------------------------------
#include "Riostream.h"

#include "MUV0Reconstruction.hh"
#include "MUV0Channel.hh"
#include "TMUV0Digi.hh"

#include "TRecoMUV0Event.hh"
#include "TRecoMUV0Hit.hh"
#include "TSpecialTriggerEvent.hh"
#include "NA62BufferProto.hh"
#include "TTDCBSpecialTrigger.hh"
#include "NA62VRawDecoder.hh"
#include "NA62ConditionsService.hh"
#include "TString.h"
#include "TRegexp.h"

#include<iostream>
#include<sstream>
#include<map>
#include<utility>

#define MUV0cout std::cout << "[MUV0Reconstruction] "

MUV0Reconstruction::MUV0Reconstruction(TFile* HistoFile, TString ConfigFileName) :
  NA62VReconstruction(HistoFile, "MUV0", ConfigFileName),
  fCandidate(nullptr),
  tMUV0Event(nullptr),
  fHDigiToTVsROChannel(nullptr)
{
  // Initialize variables and histos

  fRecoEvent = new TRecoMUV0Event();
  fMUV0Geometry = new MUV0Geometry();
  ParseConfFile(ConfigFileName);
  ParseReconstructionSettingsFile(fReconstructionSettingsFileName);
  
  fDtLeadingMax = 5.;
  ResetHistograms();
}

void MUV0Reconstruction::Init(NA62VReconstruction* MainReco) {
  //common part for all the subdetectors 

  NA62VReconstruction::Init(MainReco);

  for (Int_t ich=0; ich<fNChannels; ich++) {
    Int_t PositionID = fRawDecoder->GetDecoder()->GetChannelRemap(ich);
    fChannels[ich] = new MUV0Channel(PositionID, ich, fChannelHistograms, fLowThreshold, fHighThreshold);
  }
  ReadT0s();

  InitHistograms();
}

MUV0Reconstruction::~MUV0Reconstruction(){
  DeleteHistograms();
  delete fMUV0Geometry;
  fMUV0Geometry = nullptr;
}

// Read MUV0 reconstruction parameters from a configuration file
void MUV0Reconstruction::ParseConfFile(TString ConfFileName) {

  std::ifstream confFile(ConfFileName.Data());
  if (!confFile.is_open()) {
    perror(ConfFileName);
    exit(kWrongConfiguration);
  }

  TString Line;
  while (Line.ReadLine(confFile)) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("ReconstructionSettingsFileInput")) {
      TObjArray *l = Line.Tokenize(" ");
      fReconstructionSettingsFileName = static_cast<TObjString*>(l->At(1))->GetString();
      delete l;
      continue;
    }
  }
  confFile.close();
}

void MUV0Reconstruction::ParseReconstructionSettingsFile(TString ReconstructionSettingsFileName) {

  TString Line;

  NA62ConditionsService::GetInstance()->Open(ReconstructionSettingsFileName);
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(ReconstructionSettingsFileName))) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("DiscriminatorThresholdLow")) {
      fLowThreshold =  TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
      continue;
    }
    else if (Line.BeginsWith("DiscriminatorThresholdHigh")) {
      fHighThreshold =  TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
      continue;
    }
  }
  NA62ConditionsService::GetInstance()->Close(ReconstructionSettingsFileName);
}

TDetectorVEvent * MUV0Reconstruction::Trigger(TDetectorVEvent* tEvent, Event*){
  return tEvent;
}

void MUV0Reconstruction::StartOfBurst(){
  NA62VReconstruction::StartOfBurst(); // common part for all the subdetectors
}

void MUV0Reconstruction::EndOfBurst(){
  NA62VReconstruction::EndOfBurst(); // common part for all the subdetectors
}

TRecoVEvent * MUV0Reconstruction::ProcessSOBEvent(TDetectorVEvent*, Event* ){
  
  return 0;
}

TRecoVEvent * MUV0Reconstruction::ProcessEOBEvent(TDetectorVEvent* tEvent, Event* ){


  for(Int_t iSpecTrig=0; iSpecTrig<tEvent->GetNHits(); iSpecTrig++){
    TTDCBSpecialTrigger * SpecTrig = reinterpret_cast<TTDCBSpecialTrigger *>(tEvent->GetHit(iSpecTrig));
    if(!SpecTrig) continue;
    if(isL0EOB(SpecTrig->GetTriggerType())){ //get TEL62 EOB information
      PrimCounter* ChannelCounts = SpecTrig->GetCounter("CHANNEL_COUNT_L");
      if(ChannelCounts){
        for(UInt_t iEntry=0;iEntry<ChannelCounts->GetNEntries();iEntry++){
          Int_t ChannelID = ChannelCounts->GetChannelID(iEntry);
          Int_t NCounts   = ChannelCounts->GetValue(iEntry);
          if(ChannelID<0) continue; // Masked channel
          UInt_t ich = ChannelID%10;
          Bool_t IsHighThreshold = ChannelID/10;
          if(!IsHighThreshold)
            fHHitMapEOBLowThr->SetBinContent(ich+1, NCounts);
          if(IsHighThreshold)
            fHHitMapEOBHighThr->SetBinContent(ich+1, NCounts);


#ifdef DEBUG
          MUV0cout << "EOB counts: " << ChannelID << " " << NCounts << std::endl;
#endif
        }
      }
    }
  }

  return 0;
}


TRecoVEvent * MUV0Reconstruction::ProcessEvent(TDetectorVEvent* tEvent, Event* tGenEvent){

  if (tEvent->IsA() == TSpecialTriggerEvent::Class()) {
    if(isL0EOB(tEvent->GetTriggerType())) return ProcessEOBEvent(tEvent,tGenEvent);
    if(isL0SOB(tEvent->GetTriggerType())) return ProcessSOBEvent(tEvent,tGenEvent);

    return 0;

  }

  //common part for all the subdetectors 
  NA62VReconstruction::ProcessEvent(tEvent, tGenEvent);

  tMUV0Event = static_cast<TMUV0Event*>(tEvent);

  const Int_t nDigis = tMUV0Event->GetNHits();
  fHNDigis->Fill(nDigis);
  if (!nDigis) return fRecoEvent;

  const TClonesArray& Digis = (*(tMUV0Event->GetHits()));

  //MUV0 reconstruction

  std::map<Double_t, std::pair<Int_t,TMUV0Digi*> > digiMap[9];
  for (Int_t iDigi=0; iDigi<nDigis;++iDigi) {
    TMUV0Digi* digi=(static_cast<TMUV0Digi*>(Digis[iDigi]));

    if( digi->GetStationID() != 0 ) continue; //skip all but MUV0
    Int_t PMTID = digi->EncodeChannelID()%10;
    if( digi->HasLeadingEdge() && 0<=PMTID && PMTID<9) {
      Double_t edgeTime = digi->GetLeadingEdge();
      while (digiMap[PMTID].find(edgeTime) != digiMap[PMTID].end()) {
        if (digiMap[PMTID][edgeTime].second->GetThresholdType()==0 && digi->GetThresholdType()==1) edgeTime += 0.01;
        else if (digiMap[PMTID][edgeTime].second->GetThresholdType()==1 && digi->GetThresholdType()==0) edgeTime -= 0.01;  
        else {
          MUV0cout << "SNH >> Found duplicated time: " << edgeTime << 
          " First threshold = " << digiMap[PMTID][edgeTime].second->GetThresholdType() << 
            " Second = " << digi->GetThresholdType() << std::endl;      
          digiMap[PMTID].erase(digiMap[PMTID].find(edgeTime)); //remove duplicated time from map
        }
      }
      digiMap[PMTID][edgeTime]=std::make_pair(
          (digi->GetThresholdType()?2:1),digi);
    }

    if(digi->HasTrailingEdge() && 0<=PMTID && PMTID<9) {
      Double_t edgeTime = digi->GetTrailingEdge();
      while (digiMap[PMTID].find(edgeTime) != digiMap[PMTID].end()) {
        edgeTime -= 0.01;
        MUV0cout << "Found duplicated time: " << edgeTime << std::endl;
      }
      digiMap[PMTID][edgeTime]=std::make_pair(
          (digi->GetThresholdType()?3:4),digi);
    }
  }

  for(Int_t pmtID=0;pmtID<9;++pmtID){
    Int_t thrID_bef=0;
    Double_t time_bef=0;
    TRecoMUV0Hit* hit =NULL;
    for(std::map<Double_t, std::pair<Int_t,TMUV0Digi*> >::iterator digimap_it=digiMap[pmtID].begin();
        digimap_it!=digiMap[pmtID].end();
        ++digimap_it){
      TMUV0Digi* digi=(digimap_it->second.second);
      const Int_t thrID=digimap_it->second.first;
      const Double_t time=digimap_it->first-GetT0Correction(digi); //add CoarseT0 corrections

      if( //break condition
          thrID_bef>=thrID
          ||
          (thrID==2&&thrID_bef==1&&(time-time_bef < -0.2 || time-time_bef > fDtLeadingMax))
        ){
        hit=NULL;
        thrID_bef=0;
        time_bef=0;
      }
      if(hit==NULL && thrID>2) continue; //at least one leading
      if(hit==NULL)
        hit=static_cast<TRecoMUV0Hit*>(static_cast<TDetectorVEvent*>( fRecoEvent)->AddHit());
      switch(thrID){
        case 1:
          hit->SetLeadingEdgeLow(time);
          hit->SetLowThresholdROChannelID(fRawDecoder->GetDecoder()->GetChannelRO(digi->GetChannelID()));
          break;
        case 2:
          hit->SetLeadingEdgeHigh(time);
          hit->SetHighThresholdROChannelID(fRawDecoder->GetDecoder()->GetChannelRO(digi->GetChannelID()));
          break;
        case 3:
          hit->SetTrailingEdgeHigh(time);
          hit->SetHighThresholdROChannelID(fRawDecoder->GetDecoder()->GetChannelRO(digi->GetChannelID()));
          break;
        case 4:
          hit->SetTrailingEdgeLow(time);
          hit->SetLowThresholdROChannelID(fRawDecoder->GetDecoder()->GetChannelRO(digi->GetChannelID()));
          break;
      }
      hit->SetChannelID(pmtID);
      hit->DecodeChannelID();
      hit->SetDigi(digi);
      thrID_bef=thrID;
      time_bef=time;
      Double_t T0 = -999.;
      Double_t LowThr  = -999.;
      Double_t HighThr = -999.;
      if(0<=hit->GetLowThresholdROChannelID() && 
         hit->GetLowThresholdROChannelID()<fNChannels) {
        T0 = fChannels[hit->GetLowThresholdROChannelID()]->GetT0();
        LowThr = static_cast<MUV0Channel*>(fChannels[hit->GetLowThresholdROChannelID()])->GetThreshold();
      }
      if(0<=hit->GetHighThresholdROChannelID() && 
         hit->GetHighThresholdROChannelID()<fNChannels) {
        T0 = fChannels[hit->GetHighThresholdROChannelID()]->GetT0();
        HighThr = static_cast<MUV0Channel*>(fChannels[hit->GetHighThresholdROChannelID()])->GetThreshold();
      }
      hit->SetTime(hit->GetSlewingCorrection(LowThr, HighThr) - T0);
    }
  }
  RecoHitAnalyse();
  return fRecoEvent;
}

void MUV0Reconstruction::EndProcessing(){
  NA62VReconstruction::EndProcessing(); // call base class for raw hist output
  SaveHistograms();
}

void MUV0Reconstruction::RecoHitAnalyse(){

  fHNRecoHits->Fill(fRecoEvent->GetNHits());
  for(Int_t iHit=0;iHit<fRecoEvent->GetNHits();++iHit){
    const TRecoMUV0Hit& recoHit = *static_cast<TRecoMUV0Hit*>(static_cast<TDetectorVEvent*>( fRecoEvent)->GetHit(iHit));
    const Int_t chanId=recoHit.GetChannelID();
    //MUV0cout << "Channel ID: " <<  chanId << std::endl;
    //Mask for the edges present:
    //bit 0 --> LeadingLow;
    //bit 1 --> LeadingHigh;
    //bit 2 --> TrailingHigh;
    //bit 3 --> TrailingLow.

    const Double_t XCenter = 2245.;
    fHHitMap->Fill(chanId);
    fHIllumination->Fill(fMUV0Geometry->GetTileCenterX(chanId) - XCenter,
        fMUV0Geometry->GetTileCenterY(chanId));
    if(recoHit.HasLeadingEdgeHigh()||recoHit.HasTrailingEdgeHigh())
      fHHitToTLowThrIfHighThrExists->Fill(chanId,recoHit.GetTimeOverThresholdLowThr());

    fHHitToTLowThr ->Fill(chanId,recoHit.GetTimeOverThresholdLowThr());
    fHHitToTHighThr->Fill(chanId,recoHit.GetTimeOverThresholdHighThr());

    fHTime[chanId]->Fill(recoHit.GetTime());
    fHTimeNoT0[chanId]->Fill(recoHit.GetTimeNoT0());
  }
  return;
}

void MUV0Reconstruction::FillTimes(Double_t ReferenceTime) {
  //common part for all the subdetectors 
  NA62VReconstruction::FillTimes(ReferenceTime);
  for(Int_t iHit=0;iHit<fRecoEvent->GetNHits();++iHit){
    const TRecoMUV0Hit& recoHit = *static_cast<TRecoMUV0Hit*>(static_cast<TDetectorVEvent*>( fRecoEvent)->GetHit(iHit));
    const Int_t    ch       = recoHit.GetChannelID();

    fHHitGeoChannelVsEdgeMask->Fill(ch,recoHit.GetEdgeMask());

    fHCorrectedLeadingTimeWrtReferenceVsSlewingSlope[ch]->Fill(
        recoHit.GetLeadingESlewingSlope() ,recoHit.GetTime()-ReferenceTime);

    fHCorrectedTrailingTimeWrtReferenceVsSlewingSlope[ch]->Fill(
        recoHit.GetTrailingESlewingSlope(),recoHit.GetTime()-ReferenceTime);

    fHEdgeMaskVsFirstTime[ch]->Fill((
          recoHit.HasLeadingEdgeLow()?recoHit.GetLeadingEdgeLow():
          recoHit.HasLeadingEdgeHigh()?recoHit.GetLeadingEdgeHigh():
          recoHit.HasTrailingEdgeLow()? recoHit.GetTrailingEdgeLow():
          recoHit.HasTrailingEdgeHigh()?recoHit.GetTrailingEdgeHigh():
          0)-ReferenceTime,recoHit.GetEdgeMask()
        );
    if(!recoHit.HasAll4EdgesDetected())
      continue;
    fHLowTOTVsHighTOT[ch]->Fill(recoHit.GetTimeOverThresholdLowThr(),recoHit.GetTimeOverThresholdHighThr());

    /*
     * We want all the 4 edges
     * ({Leading,Trailing}{Low,High}) to be present and to
     * be sorted.  Since the slewing corrected time is
     * produced by use of the two digis (ROChannels), we fill
     * the histogrammes twice. The same time is used for
     * both low threshold and the high threshold ROChannels.
     */
    fHHiThrTOTVsLeadingTime[ch]->Fill(recoHit.GetTime()-ReferenceTime,recoHit.GetTimeOverThresholdHighThr());
    const Int_t    roChLow  = recoHit.GetLowThresholdROChannelID();// static_cast<const TMUV0Digi*>(recoHit.GetDigi())->GetChannelID();
    const Int_t    roChHigh = recoHit.GetHighThresholdROChannelID();// static_cast<const TMUV0Digi*>(recoHit.GetDigi())->GetCorrespondingLowHighChannelId();
    if (fHRecoHitTimeWrtReferenceVsROChannel) {
      fHRecoHitTimeWrtReferenceVsROChannel->Fill(roChLow ,recoHit.GetTime()-ReferenceTime);
      fHRecoHitTimeWrtReferenceVsROChannel->Fill(roChHigh,recoHit.GetTime()-ReferenceTime);
    }
    if (fHRecoHitTimeWrtReferenceVsROChannelNoT0) {
      fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(roChLow,recoHit.GetTimeNoT0()-ReferenceTime);
      fHRecoHitTimeWrtReferenceVsROChannelNoT0->Fill(roChHigh,recoHit.GetTimeNoT0()-ReferenceTime);
    }
    if (fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim) {
      fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim->Fill(roChLow,recoHit.GetLeadingEdgeLow()-ReferenceTime);
      fHRecoHitTimeWrtReferenceVsROChannelNoT0Prim->Fill(roChHigh,recoHit.GetLeadingEdgeLow()-ReferenceTime);
    }
  }
}

void MUV0Reconstruction::InitHistograms() {

  // MUV0 histograms
  /*TDirectory *MUV0Dir = */GetOrMakeDir(fHistoFile,"MUV0Monitor");
  fHistoFile->cd("MUV0Monitor");

  fHNDigis = new TH1I("fHNDigis", "No of MUV0 Hits per Event", 20, -0.5, 19.5);
  fHNRecoHits = new TH1I("fHNRecoHits", "No of Reconstructed MUV0 Hits per Event", 20, -0.5, 19.5);
  fHHitMap = new TH1I("fHHitMap", "MUV0 Hitmap", 9, -0.5, 8.5);
  fHHitMapEOBLowThr = new TH1I("fHHitMapEOBLowThr", "MUV0 EOB Low Threshold Hitmap", 9, -0.5, 8.5);
  fHHitMapEOBHighThr = new TH1I("fHHitMapEOBHighThr", "MUV0 EOB High Threshold Hitmap", 9, -0.5, 8.5);
  const Int_t NBINS = 3;
  Double_t xEdges[NBINS + 1] = {-700.0, -100., 300., 700.};
  Double_t yEdges[NBINS + 1] = {-700.0, -300., 100., 700.};
  fHIllumination = new TH2F("fHIllumination", "MUV0 Illumination", NBINS,
    xEdges, NBINS, yEdges);
  fHHitToTLowThr =new TH2F("HitToTLowThr" ,"HitToTLowThr;Channel ID;Time over threshold [ns]",4,-.5,3.5,202,  -2,200);
  fHHitToTHighThr=new TH2F("HitToTHighThr","HitToTHighThr;Channel ID;Time over threshold [ns]",4,-.5,3.5,202,  -2,200);
  fHHitToTLowThrIfHighThrExists=new TH2F("fHHitToTLowThrIfHighThrExists","fHHitToTLowThrIfHighThrExists;Channel ID;Time over threshold [ns]",4,-.5,3.5,202,  -2,200);
  fHHitGeoChannelVsEdgeMask =new TH2F("HitGeoChannelVsEdgeMask","HitGeoChannelVsEdgeMask;ChannelID;fEdgeMask",4,-.5,4-.5,16,-.5,16-.5);
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel( 1,"__,__,__,__");
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel( 2,"LL,__,__,__");
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel( 3,"__,LH,__,__");
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel( 4,"LL,LH,__,__");
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel( 5,"__,__,TH,__");
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel( 6,"LL,__,TH,__");
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel( 7,"__,LH,TH,__");
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel( 8,"LL,LH,TH,__");
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel( 9,"__,__,__,TL");
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel(10,"LL,__,__,TL");
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel(11,"__,LH,__,TL");
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel(12,"LL,LH,__,TL");
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel(13,"__,__,TH,TL");
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel(14,"LL,__,TH,TL");
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel(15,"__,LH,TH,TL");
  fHHitGeoChannelVsEdgeMask->GetYaxis()->SetBinLabel(16,"LL,LH,TH,TL");

  TString name,title;
  for(Int_t iChan=0;iChan<9;++iChan){
    fHLowTOTVsHighTOT[iChan]=new TH2F(Form("LowTOTVsHightTOT_ch_%d",iChan),Form("LowTOTVsHightTOT_ch_%d;Time over threshold Low;Time over threshold High",iChan),100,-20,160,100,-20,160);
    fHCorrectedLeadingTimeWrtReferenceVsSlewingSlope[iChan]  =new TH2F(Form("CorrectedLeadingTimeWrtReferenceVsSlewingSlope_%d",iChan) ,Form("CorrectedLeadingTimeWrtReferenceVsSlewingSlope ch %d;(timeH-timeL)/(ThrH-ThrL);Corrected Leading time [ns]",iChan) ,300,-.5,1.5,100,-50,50);
    fHCorrectedTrailingTimeWrtReferenceVsSlewingSlope[iChan] =new TH2F(Form("CorrectedTrailingTimeWrtReferenceVsSlewingSlope_%d",iChan),Form("CorrectedTrailingTimeWrtReferenceVsSlewingSlope ch %d;(timeH-timeL)/(ThrH-ThrL);Corrected Trailing time [ns]",iChan),300,-7,1,100,-50,50);
    fHHiThrTOTVsLeadingTime[iChan]=new TH2F(Form("HiThrTOTVsLeadingTime_ch_%d",iChan),Form("HiThrTOTVsLeadingTime channel %d;Leading Time [ns];HiThr TOT [ns]",iChan),500,-80,180,500,-80,80);

    TString chanName=Form("EdgeMaskVsFirstTime_ch_%d",iChan+1);

    fHEdgeMaskVsFirstTime[iChan]=new
      TH2F(chanName,chanName,900,-200,200,16,-.5,16-.5);
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel( 1,"__,__,__,__");
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel( 2,"LL,__,__,__");
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel( 3,"__,LH,__,__");
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel( 4,"LL,LH,__,__");
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel( 5,"__,__,TH,__");
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel( 6,"LL,__,TH,__");
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel( 7,"__,LH,TH,__");
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel( 8,"LL,LH,TH,__");
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel( 9,"__,__,__,TL");
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel(10,"LL,__,__,TL");
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel(11,"__,LH,__,TL");
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel(12,"LL,LH,__,TL");
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel(13,"__,__,TH,TL");
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel(14,"LL,__,TH,TL");
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel(15,"__,LH,TH,TL");
    fHEdgeMaskVsFirstTime[iChan]->GetYaxis()->SetBinLabel(16,"LL,LH,TH,TL");

    fHTime[iChan] = new TH1F(Form("Time_ch_%d",iChan), Form("Time_ch_%d; Time",iChan), 150, -150, 150);
    fHTimeNoT0[iChan] = new TH1F(Form("TimeNoT0_ch_%d",iChan), Form("TimeNoT0_ch_%d; Time No T0",iChan), 150, -150, 150);
  }

}

void MUV0Reconstruction::SaveHistograms(){

  fHistoFile->cd("MUV0Monitor");

  // MUV0 histograms 
  fHNDigis->Write();
  fHNRecoHits->Write();
  fHHitMap->Write();
  fHHitMapEOBLowThr->Write();
  fHHitMapEOBHighThr->Write();
  fHIllumination->Write();
  fHHitToTLowThr ->Write();
  fHHitToTHighThr->Write();
  fHHitToTLowThrIfHighThrExists->Write();
  fHHitGeoChannelVsEdgeMask->Write();
  for(Int_t iChan=0;iChan<9;++iChan)
    fHLowTOTVsHighTOT[iChan]->Write();
  for(Int_t iChan=0;iChan<9;++iChan)
    fHHiThrTOTVsLeadingTime[iChan]->Write();
  for(Int_t iChan=0;iChan<9;++iChan)
    fHCorrectedLeadingTimeWrtReferenceVsSlewingSlope[iChan]->Write();
  for(Int_t iChan=0;iChan<9;++iChan)
    fHCorrectedTrailingTimeWrtReferenceVsSlewingSlope[iChan]->Write();
  for(Int_t iChan=0;iChan<9;++iChan)
    fHEdgeMaskVsFirstTime[iChan]->Write();
  for(Int_t iChan=0;iChan<9;++iChan)
    fHTime[iChan]->Write();
  for(Int_t iChan=0;iChan<9;++iChan)
    fHTimeNoT0[iChan]->Write();

  fHistoFile->cd("/");
}

void MUV0Reconstruction::ResetHistograms(){

  fHNDigis=NULL;
  fHNRecoHits=NULL;
  fHHitMap=NULL;
  fHHitMapEOBLowThr=NULL;
  fHHitMapEOBHighThr=NULL;
  fHIllumination=NULL;
  fHHitToTLowThr =NULL;
  fHHitToTHighThr=NULL;
  fHHitToTLowThrIfHighThrExists=NULL;
  fHHitGeoChannelVsEdgeMask=NULL;

  for(Int_t iChan=0;iChan<9;++iChan){
    fHLowTOTVsHighTOT[iChan]=NULL;
    fHEdgeMaskVsFirstTime[iChan]=NULL;
    fHCorrectedLeadingTimeWrtReferenceVsSlewingSlope[iChan]=NULL;
    fHCorrectedTrailingTimeWrtReferenceVsSlewingSlope[iChan]=NULL;
    fHHiThrTOTVsLeadingTime[iChan]=NULL;
    fHTime[iChan]=NULL;
    fHTimeNoT0[iChan]=NULL;
  }

}

void MUV0Reconstruction::DeleteHistograms(){

  if(fHNDigis) delete fHNDigis;
  if(fHNRecoHits) delete fHNRecoHits;
  if(fHHitMap) delete fHHitMap;
  if(fHHitMapEOBLowThr) delete fHHitMapEOBLowThr;
  if(fHHitMapEOBHighThr) delete fHHitMapEOBHighThr;
  if(fHIllumination) delete fHIllumination;
  if(fHHitToTLowThr )delete fHHitToTLowThr ;
  if(fHHitToTHighThr)delete fHHitToTHighThr;
  if(fHHitToTLowThrIfHighThrExists)delete  fHHitToTLowThrIfHighThrExists;
  if(fHHitGeoChannelVsEdgeMask    )delete fHHitGeoChannelVsEdgeMask;

  for(Int_t iChan=0;iChan<9;++iChan){
    if(fHLowTOTVsHighTOT[iChan])delete fHLowTOTVsHighTOT[iChan];
    if(fHCorrectedTrailingTimeWrtReferenceVsSlewingSlope[iChan])delete fHCorrectedTrailingTimeWrtReferenceVsSlewingSlope[iChan];
    if(fHCorrectedLeadingTimeWrtReferenceVsSlewingSlope [iChan])delete fHCorrectedLeadingTimeWrtReferenceVsSlewingSlope [iChan];
    if(fHEdgeMaskVsFirstTime[iChan])delete fHEdgeMaskVsFirstTime[iChan];
    if(fHHiThrTOTVsLeadingTime[iChan])delete fHHiThrTOTVsLeadingTime[iChan];
    if(fHTime[iChan]) delete fHTime[iChan];
    if(fHTimeNoT0[iChan]) delete fHTimeNoT0[iChan];

  }

  ResetHistograms();
}
