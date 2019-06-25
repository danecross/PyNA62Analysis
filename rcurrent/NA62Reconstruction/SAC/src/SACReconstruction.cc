// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//
// --------------------------------------------------------------
#include "Riostream.h"

#include "SACGeometry.hh"
#include "NA62RecoManager.hh"
#include "NA62Reconstruction.hh"
#include "SACReconstruction.hh"
#include "SACChannel.hh"
#include "TSACDigi.hh"

#include "TRecoSACEvent.hh"
#include "TRecoSACHit.hh"
#include "TSpecialTriggerEvent.hh"
#include "NA62BufferProto.hh"
#include "TTDCBSpecialTrigger.hh"

#include "TH1F.h"
#include "TH2F.h"
#include "TGraph.h"
#include "TVector.h"
#include "TGraphErrors.h"

#include "TString.h"
#include "TRegexp.h"

#include<iostream>
#include<sstream>
#include<map>
#include<utility>

#define SACcout std::cout << "[SACReconstruction] "

SACReconstruction::SACReconstruction(TFile* HistoFile, TString ConfigFileName) :
  NA62VReconstruction(HistoFile, "SAC", ConfigFileName),
  fCandidate(nullptr)
{
  // Initialize variables and histos
  //SACGeometry::GetInstance()->GetWhatYouNeed();
  fRecoEvent = new TRecoSACEvent();
  ParseConfFile(ConfigFileName);
  fDtLeadingMax = 5.;
  ResetHistograms();
  for(int ich = 0;ich<4;ich++) {
    fNHitsBurst [ich] = 0;
  }
}

void SACReconstruction::Init(NA62VReconstruction* MainReco) {

  //common part for all the subdetectors
  NA62VReconstruction::Init(MainReco);

  for (Int_t ich=0; ich<fNChannels; ich++) {
    Int_t PositionID = fRawDecoder->GetDecoder()->GetChannelRemap(ich);
    fChannels[ich] = new SACChannel(PositionID, ich, fChannelHistograms, fLowThreshold, fHighThreshold);
  }
  ReadT0s();

  InitHistograms();
}

SACReconstruction::~SACReconstruction(){
  DeleteHistograms();
}

// Read SAC reconstruction parameters from a configuration file
void SACReconstruction::ParseConfFile(TString ConfFileName) {

  std::ifstream confFile(ConfFileName.Data());
  if (!confFile.is_open()) {
    perror(ConfFileName);
    exit(kWrongConfiguration);
  }

  TString Line;
  while (Line.ReadLine(confFile)) {
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
  confFile.close();
}

TDetectorVEvent * SACReconstruction::Trigger(TDetectorVEvent * tEvent, Event* /*tGenEvent*/){
  return tEvent;
}

void SACReconstruction::StartOfBurst(){
  NA62VReconstruction::StartOfBurst(); // common part for all the subdetectors
  for(int ich = 0;ich<4;ich++) {
    fNHitsBurst[ich] = 0;
  }
  fHRateBurst->Reset();
}

void SACReconstruction::EndOfBurst(){
  NA62VReconstruction::EndOfBurst(); // common part for all the subdetectors
  for(Int_t iBin=0;iBin<fHRateBurst->GetNbinsX();++iBin){
    fHRateRun->SetBinContent(
        fRecoEvent->GetBurstID(),iBin,fHRateBurst->GetBinContent(iBin));
  }
}
TRecoVEvent * SACReconstruction::ProcessSOBEvent(TDetectorVEvent* /*tEvent*/, Event* /*tGenEvent*/){
  
  return 0;
}

TRecoVEvent * SACReconstruction::ProcessEOBEvent(TDetectorVEvent* tEvent, Event* /*tGenEvent*/){

  Double_t BurstIntensity = NA62RecoManager::GetInstance()->GetEventHeader()->GetBeamSpecialTrigger()->GetCountsARGONION();

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
          UInt_t ich = ChannelID%1000;
          Bool_t IsHighThreshold = ChannelID/1000;
          //SACcout << "EOB counts: " << ChannelID << " " << NCounts << std::endl;
#ifdef DEBUG
          SACcout << "\tChannel: " << ich << " NHits: " << fNHitsBurst[ich] << std::endl;
#endif
          if(IsHighThreshold) fGRateChHigh[ich]->SetPoint(fGRateChHigh[ich]->GetN(),tEvent->GetBurstID(),NCounts );
          else                fGRateChLow[ich]->SetPoint(fGRateChLow[ich]->GetN(),tEvent->GetBurstID(),NCounts );
          if(BurstIntensity > 0.)  {
            if(IsHighThreshold) fGRateChHighNorm[ich]->SetPoint(fGRateChHighNorm[ich]->GetN(),tEvent->GetBurstID(),NCounts/BurstIntensity);
            else                fGRateChLowNorm[ich]->SetPoint(fGRateChLowNorm[ich]->GetN(),tEvent->GetBurstID(),NCounts/BurstIntensity );
            fGHitRateChNorm[ich]->SetPoint(fGHitRateChNorm[ich]->GetN(),tEvent->GetBurstID(),fNHitsBurst[ich]/BurstIntensity);
            fGHitRateChNorm[ich]->SetPointError(fGHitRateChNorm[ich]->GetN()-1,0,sqrt(fNHitsBurst[ich])/BurstIntensity);
          }
        }
      }
    }
  }
  return 0;
}

TRecoVEvent * SACReconstruction::ProcessEvent(TDetectorVEvent* tEvent, Event* tGenEvent){

  if (tEvent->IsA() == TSpecialTriggerEvent::Class()) {
    if(isL0EOB(tEvent->GetTriggerType())) return ProcessEOBEvent(tEvent,tGenEvent);
    if(isL0SOB(tEvent->GetTriggerType())) return ProcessSOBEvent(tEvent,tGenEvent);
    // TDC counters
    // Example in Cedar
    return 0;
  }

  //common part for all the subdetectors 
  NA62VReconstruction::ProcessEvent(tEvent, tGenEvent);

  TSACEvent* tSACEvent = static_cast<TSACEvent*>(tEvent);

  const Int_t nDigis = tSACEvent->GetNHits();
  if (!nDigis) return fRecoEvent;

  const TClonesArray& Digis = (*(tSACEvent->GetHits()));

  //SAC reconstruction

  const Int_t HighThrPatchT0s[4] = {1,-7,-2,3}; //patch to correct wrong HighThreshold T0s in runs [7667,7873]

  std::map<Double_t, std::pair<Int_t,TSACDigi*> > digiMap[4];
  for (Int_t iDigi=0; iDigi<nDigis;++iDigi) {
    TSACDigi* digi=(static_cast<TSACDigi*>(Digis[iDigi]));

    if(static_cast<NA62Reconstruction*>(fMainReco)->GetIsRawData() && 7667<=tEvent->GetRunID()&&tEvent->GetRunID()<=7873){
      //patch to correct wrong HighThreshold T0s
      if(digi->GetThresholdType() && 0<=digi->GetPMTID() && digi->GetPMTID()<4){
        digi->SetLeadingEdge(digi->GetLeadingEdge()+HighThrPatchT0s[digi->GetPMTID()]*TdcCalib);
        digi->SetTrailingEdge(digi->GetTrailingEdge()+HighThrPatchT0s[digi->GetPMTID()]*TdcCalib);
      }
    }

    if( digi->HasLeadingEdge() && 0<=digi->GetPMTID() && digi->GetPMTID()<4) {
      Double_t edgeTime = digi->GetLeadingEdge();
      while (digiMap[digi->GetPMTID()].find(edgeTime) != digiMap[digi->GetPMTID()].end()) {
        if (digiMap[digi->GetPMTID()][edgeTime].second->GetThresholdType()==0 && digi->GetThresholdType()==1) edgeTime += 0.01; 
        else if (digiMap[digi->GetPMTID()][edgeTime].second->GetThresholdType()==1 && digi->GetThresholdType()==0) edgeTime -= 0.01;   
        else {
          SACcout << "SNH >> Found duplicated time: " << edgeTime << 
          " First threshold = " << digiMap[digi->GetPMTID()][edgeTime].second->GetThresholdType() << 
            " Second = " << digi->GetThresholdType() << std::endl;            
          digiMap[digi->GetPMTID()].erase(digiMap[digi->GetPMTID()].find(edgeTime)); //remove duplicated time from map
        }
      }
      //      digiMap[digi->GetPMTID()][digi->GetLeadingEdge()]=make_pair(
      //          (digi->GetThresholdType()?2:1),digi);
      digiMap[digi->GetPMTID()][edgeTime]=std::make_pair(
          (digi->GetThresholdType()?2:1),digi);
    }

    if(digi->HasTrailingEdge() && 0<=digi->GetPMTID() && digi->GetPMTID()<4) {
      Double_t edgeTime = digi->GetTrailingEdge();
      while (digiMap[digi->GetPMTID()].find(edgeTime) != digiMap[digi->GetPMTID()].end()) {
        edgeTime -= 0.01;
        std::cout << "Found duplicated time: " << edgeTime << std::endl;
      }
      //      digiMap[digi->GetPMTID()][digi->GetTrailingEdge()]=make_pair(
      //          (digi->GetThresholdType()?3:4),digi);
      digiMap[digi->GetPMTID()][edgeTime]=std::make_pair(
          (digi->GetThresholdType()?3:4),digi);
    }
  }

  for(Int_t pmtID=0;pmtID<4;++pmtID){
    Int_t thrID_bef=0;
    Double_t time_bef=0;
    TRecoSACHit* hit =NULL;
    for(std::map<Double_t, std::pair<Int_t,TSACDigi*> >::iterator digimap_it=digiMap[pmtID].begin();
        digimap_it!=digiMap[pmtID].end();
        ++digimap_it){
      TSACDigi* digi=(digimap_it->second.second);
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
        hit=static_cast<TRecoSACHit*>(static_cast<TDetectorVEvent*>(fRecoEvent)->AddHit());
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
        LowThr = static_cast<SACChannel*>(fChannels[hit->GetLowThresholdROChannelID()])->GetThreshold();
      }
      if(0<=hit->GetHighThresholdROChannelID() &&
         hit->GetHighThresholdROChannelID()<fNChannels) {
        T0 = fChannels[hit->GetHighThresholdROChannelID()]->GetT0();
        HighThr = static_cast<SACChannel*>(fChannels[hit->GetHighThresholdROChannelID()])->GetThreshold();
      }
      hit->SetTime(hit->GetSlewingCorrection(LowThr, HighThr) - T0);
    }
  }
  RecoHitAnalyse();
  return fRecoEvent;
}

void SACReconstruction::EndProcessing(){
  NA62VReconstruction::EndProcessing(); // call base class for raw hist output
  SaveHistograms();
}

void SACReconstruction::RecoHitAnalyse(){
  for(Int_t iHit=0;iHit<fRecoEvent->GetNHits();++iHit){
    const TRecoSACHit& recoHit = *static_cast<TRecoSACHit*>(static_cast<TDetectorVEvent*>(fRecoEvent)->GetHit(iHit));
    const Int_t chanId=recoHit.GetChannelID();

    //Mask for the edges present:
    //bit 0 --> LeadingLow;
    //bit 1 --> LeadingHigh;
    //bit 2 --> TrailingHigh;
    //bit 3 --> TrailingLow.

    if(chanId>=0 && chanId<=3 && recoHit.HasLeadingEdgeHigh() && recoHit.HasTrailingEdgeHigh() ) fNHitsBurst[chanId] ++; 

    if(recoHit.HasAll4EdgesDetected()){
      //fHHitToTAll    ->Fill(chanId,recoHit.GetTimeOverThreshold());
    }
    if(recoHit.HasAllTimesInOrder()){
      //fHHitToTOrdered->Fill(chanId,recoHit.GetTimeOverThreshold());
    }
    if(recoHit.HasLeadingEdgeHigh()||recoHit.HasTrailingEdgeHigh())
      fHHitToTLowThrIfHighThrExists->Fill(chanId,recoHit.GetTimeOverThresholdLowThr());
    //fHHitToT     ->Fill(chanId,recoHit.GetTimeOverThreshold());

    //if(recoHit.HasLeadingEdgeLow()&&recoHit.HasTrailingEdgeLow())
    fHHitToTLowThr ->Fill(chanId,recoHit.GetTimeOverThresholdLowThr());
    //if(recoHit.HasLeadingEdgeHigh()&&recoHit.HasTrailingEdgeHigh())
    fHHitToTHighThr->Fill(chanId,recoHit.GetTimeOverThresholdHighThr());

    if(recoHit.GetEdgeMask()&0x01)fHRateBurst->Fill(chanId+ 0);//fHOccupLeadLow
    if(recoHit.GetEdgeMask()&0x02)fHRateBurst->Fill(chanId+ 5);//fHOccupLeadHigh
    if(recoHit.GetEdgeMask()&0x04)fHRateBurst->Fill(chanId+10);//fHOccupTrailHigh
    if(recoHit.GetEdgeMask()&0x08)fHRateBurst->Fill(chanId+15);//fHOccupTrailLow
  }
  return;
}

void SACReconstruction::FillTimes(Double_t ReferenceTime) {
  //common part for all the subdetectors
  NA62VReconstruction::FillTimes(ReferenceTime);
  for(Int_t iHit=0;iHit<fRecoEvent->GetNHits();++iHit){
    const TRecoSACHit& recoHit = *static_cast<TRecoSACHit*>(static_cast<TDetectorVEvent*>(fRecoEvent)->GetHit(iHit));
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

    fHLowThrTOTVsLeadingTime[ch]->Fill(recoHit.GetTimeOverThresholdLowThr(),recoHit.GetTime()-ReferenceTime);
    if(!recoHit.HasAll4EdgesDetected())
      continue;

    /*
     * We want all the 4 edges
     * ({Leading,Trailing}{Low,High}) to be present and to
     * be sorted.  Since the slewing corrected time is
     * produced by use of the two digis (ROChannels), we fill
     * the histogrammes twice. The same time is used for
     * both low threshold and the high threshold ROChannels.
     */
    fHHiThrTOTVsLeadingTime[ch]->Fill(recoHit.GetTimeOverThresholdHighThr(),recoHit.GetTime()-ReferenceTime);
    const Int_t    roChLow  = recoHit.GetLowThresholdROChannelID();// static_cast<const TSACDigi*>(recoHit.GetDigi())->GetChannelID();
    const Int_t    roChHigh = recoHit.GetHighThresholdROChannelID();// static_cast<const TSACDigi*>(recoHit.GetDigi())->GetCorrespondingLowHighChannelId();
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

void SACReconstruction::InitHistograms(){

  // SAC histograms
  /*TDirectory *SACDir = */GetOrMakeDir(fHistoFile,"SACMonitor");
  fHistoFile->cd("SACMonitor");

  fHRateBurst      = new TH1F("fHRateBurst"     ,"fHRateBurst;ChannelID;counts",20,0,20);
  fHRateBurst->GetXaxis()->SetBinLabel( 1,"ST-L-Lo");
  fHRateBurst->GetXaxis()->SetBinLabel( 2,"JT-L-Lo");
  fHRateBurst->GetXaxis()->SetBinLabel( 3,"JD-L-Lo");
  fHRateBurst->GetXaxis()->SetBinLabel( 4,"SD-L-Lo");

  fHRateBurst->GetXaxis()->SetBinLabel( 6,"ST-L-Hi");
  fHRateBurst->GetXaxis()->SetBinLabel( 7,"JT-L-Hi");
  fHRateBurst->GetXaxis()->SetBinLabel( 8,"JD-L-Hi");
  fHRateBurst->GetXaxis()->SetBinLabel( 9,"SD-L-Hi");

  fHRateBurst->GetXaxis()->SetBinLabel(11,"ST-T-Hi");
  fHRateBurst->GetXaxis()->SetBinLabel(12,"JT-T-Hi");
  fHRateBurst->GetXaxis()->SetBinLabel(13,"JD-T-Hi");
  fHRateBurst->GetXaxis()->SetBinLabel(14,"SD-T-Hi");

  fHRateBurst->GetXaxis()->SetBinLabel(16,"ST-T-Lo");
  fHRateBurst->GetXaxis()->SetBinLabel(17,"JT-T-Lo");
  fHRateBurst->GetXaxis()->SetBinLabel(18,"JD-T-Lo");
  fHRateBurst->GetXaxis()->SetBinLabel(19,"SD-T-Lo");

  fHRateBurst->GetXaxis()->SetLabelSize(.06);
  fHRateBurst->SetStats(kFALSE);

  fHRateRun = new TH2F("fHRateRun" ,"fHRateRun;BurstID", 3000, 0, 3000, 20, 0, 20);
  fHRateRun->GetYaxis()->SetBinLabel( 1,"ST-L-Lo");
  fHRateRun->GetYaxis()->SetBinLabel( 2,"JT-L-Lo");
  fHRateRun->GetYaxis()->SetBinLabel( 3,"JD-L-Lo");
  fHRateRun->GetYaxis()->SetBinLabel( 4,"SD-L-Lo");

  fHRateRun->GetYaxis()->SetBinLabel( 6,"ST-L-Hi");
  fHRateRun->GetYaxis()->SetBinLabel( 7,"JT-L-Hi");
  fHRateRun->GetYaxis()->SetBinLabel( 8,"JD-L-Hi");
  fHRateRun->GetYaxis()->SetBinLabel( 9,"SD-L-Hi");

  fHRateRun->GetYaxis()->SetBinLabel(11,"ST-T-Hi");
  fHRateRun->GetYaxis()->SetBinLabel(12,"JT-T-Hi");
  fHRateRun->GetYaxis()->SetBinLabel(13,"JD-T-Hi");
  fHRateRun->GetYaxis()->SetBinLabel(14,"SD-T-Hi");

  fHRateRun->GetYaxis()->SetBinLabel(16,"ST-T-Lo");
  fHRateRun->GetYaxis()->SetBinLabel(17,"JT-T-Lo");
  fHRateRun->GetYaxis()->SetBinLabel(18,"JD-T-Lo");
  fHRateRun->GetYaxis()->SetBinLabel(19,"SD-T-Lo");

  fHRateRun->GetYaxis()->SetLabelSize(.06);
  fHRateRun->SetStats(kFALSE);

  const Int_t nROChannels = fRawDecoder->GetDecoder()->GetNROChannels();
  if(nROChannels)
    fHDigiToTVsROChannel = new TH2F("DigiToTVsROChannel",
        "DigiToTVsROChannel;Readout Channel;Time over threshold [ns]",
        nROChannels,-.5,nROChannels-.5,500,-20,200);
  fHHitToT       =new TH2F("HitToT"       ,"HitToT;Channel ID;Time over threshold [ns]"                                                     ,4,-.5,3.5,200,-100,300);
  fHHitToTAll    =new TH2F("HitToTAll"    ,"HitToTAllThrWorkd;Channel ID;Time over threshold [ns]"                                          ,4,-.5,3.5,200,-100,300);
  fHHitToTOrdered=new TH2F("HitToTOrdered","HitToTOrdered LeLo LeHi TrHi TrLo;Channel ID;Time over threshold [ns]"                          ,4,-.5,3.5,200,-100,300);
  fHHitToTLowThr =new TH2F("HitToTLowThr" ,"HitToTLowThr;Channel ID;Time over threshold [ns]"                                               ,4,-.5,3.5,202,  -2,200);
  fHHitToTHighThr=new TH2F("HitToTHighThr","HitToTHighThr;Channel ID;Time over threshold [ns]"                                              ,4,-.5,3.5,202,  -2,200);
  fHHitToTLowThrIfHighThrExists=new TH2F("fHHitToTLowThrIfHighThrExists","fHHitToTLowThrIfHighThrExists;Channel ID;Time over threshold [ns]",4,-.5,3.5,202,  -2,200);
  fHHitGeoChannelVsEdgeMask                         =new TH2F("HitGeoChannelVsEdgeMask"                        ,"HitGeoChannelVsEdgeMask;ChannelID;fEdgeMask"                        ,4,-.5,4-.5,16,-.5,16-.5);
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
  for(Int_t iChan=0;iChan<4;++iChan){
    fHLowThrTOTVsLeadingTime[iChan]=new TH2F(Form("LowThrTOTVsLeadingTime_ch_%d",iChan),Form("LowThrTOTVsLeadingTime channel %d;LowThr TOT [ns];Leading Time [ns]",iChan),500,0,250,500,-50,50);
    fHCorrectedLeadingTimeWrtReferenceVsSlewingSlope[iChan]  =new TH2F(Form("CorrectedLeadingTimeWrtReferenceVsSlewingSlope_%d",iChan) ,Form("CorrectedLeadingTimeWrtReferenceVsSlewingSlope ch %d;(timeH-timeL)/(ThrH-ThrL);Corrected Leading time [ns]",iChan) ,300,-.5,1.5,100,-50,50);
    fHCorrectedTrailingTimeWrtReferenceVsSlewingSlope[iChan] =new TH2F(Form("CorrectedTrailingTimeWrtReferenceVsSlewingSlope_%d",iChan),Form("CorrectedTrailingTimeWrtReferenceVsSlewingSlope ch %d;(timeH-timeL)/(ThrH-ThrL);Corrected Trailing time [ns]",iChan),300,-7,1,100,-50,50);
    fHHiThrTOTVsLeadingTime[iChan]=new TH2F(Form("HiThrTOTVsLeadingTime_ch_%d",iChan),Form("HiThrTOTVsLeadingTime channel %d;HiThr TOT [ns];Leading Time [ns]",iChan),500,0,250,500,-50,50);

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

    fGRateChLow [iChan] = new TGraph(); 
    fGRateChLow [iChan]->SetName(Form("HitRateLT_ch_%d",iChan));
    fGRateChLow [iChan]->SetMarkerColor(iChan+1);
    fGRateChLow [iChan]->SetMarkerStyle(23);
    fGRateChLow [iChan]->SetMinimum(0.1);
    // fGRateChLow [iChan]->SetPoint(0,-1,0);

    fGRateChHigh [iChan] = new TGraph();
    fGRateChHigh [iChan]->SetName(Form("HitRateHT_ch_%d",iChan));
    fGRateChHigh [iChan]->SetMarkerColor(iChan+1);
    fGRateChHigh [iChan]->SetMarkerStyle(26);
    fGRateChHigh [iChan]->SetMinimum(0.1);
    // fGRateChHigh [iChan]->SetPoint(0,-1,0);

    fGRateChLowNorm [iChan] = new TGraph(); 
    fGRateChLowNorm [iChan]->SetName(Form("RateLTNorm_ch_%d",iChan));
    fGRateChLowNorm [iChan]->SetTitle(Form("RateLTNorm_SCALERS"));
    fGRateChLowNorm [iChan]->SetMarkerColor(iChan+1);
    fGRateChLowNorm [iChan]->SetMarkerStyle(23);
    fGRateChLowNorm [iChan]->SetMinimum(0.1);
    // fGRateChLow [iChan]->SetPoint(0,-1,0);

    fGRateChHighNorm [iChan] = new TGraph();
    fGRateChHighNorm [iChan]->SetName(Form("RateHTNorm_ch_%d",iChan));
    fGRateChHighNorm [iChan]->SetTitle(Form("RateHTNorm_SCALERS"));
    fGRateChHighNorm [iChan]->SetMarkerColor(iChan+1);
    fGRateChHighNorm [iChan]->SetMarkerStyle(26);
    fGRateChHighNorm [iChan]->SetMinimum(1e-9);
    // fGRateChHigh [iChan]->SetPoint(0,-1,0);

    fGHitRateChNorm [iChan] = new TGraphErrors();
    fGHitRateChNorm [iChan]->SetName(Form("HitRate_ch_%d",iChan));
    fGHitRateChNorm [iChan]->SetTitle(Form("Normalized RecoHitRate"));
    fGHitRateChNorm [iChan]->SetMarkerColor(iChan+1);
    fGHitRateChNorm [iChan]->SetMarkerStyle(26);
    fGHitRateChNorm [iChan]->SetMinimum(1e-9);

  }

  return;
}

void SACReconstruction::SaveHistograms(){

  fHistoFile->cd("SACMonitor");

  // SAC histograms 
  fHRateRun  ->Write();
  fHRateBurst->Write();
  if(fHDigiToTVsROChannel) fHDigiToTVsROChannel->Write();
  fHHitToTAll    ->Write();
  fHHitToT       ->Write();
  fHHitToTOrdered->Write();
  fHHitToTLowThr ->Write();
  fHHitToTHighThr->Write();
  fHHitToTLowThrIfHighThrExists->Write();
  fHHitGeoChannelVsEdgeMask->Write();
  for(Int_t iChan=0;iChan<4;++iChan)
    fHLowThrTOTVsLeadingTime[iChan]->Write();
  for(Int_t iChan=0;iChan<4;++iChan)
    fHHiThrTOTVsLeadingTime[iChan]->Write();
  for(Int_t iChan=0;iChan<4;++iChan)
    fHCorrectedLeadingTimeWrtReferenceVsSlewingSlope[iChan]->Write();
  for(Int_t iChan=0;iChan<4;++iChan)
    fHCorrectedTrailingTimeWrtReferenceVsSlewingSlope[iChan]->Write();
  for(Int_t iChan=0;iChan<4;++iChan)
    fHEdgeMaskVsFirstTime[iChan]->Write();
  for(Int_t iChan=0;iChan<4;++iChan)
    fGRateChLow [iChan]->Write();
  for(Int_t iChan=0;iChan<4;++iChan)
    fGRateChHigh [iChan]->Write();
  for(Int_t iChan=0;iChan<4;++iChan)
    fGRateChLowNorm [iChan]->Write();
  for(Int_t iChan=0;iChan<4;++iChan)
    fGRateChHighNorm [iChan]->Write();

  for(Int_t iChan=0;iChan<4;++iChan)
    fGHitRateChNorm [iChan]->Write();
  return;
}

void SACReconstruction::ResetHistograms(){
  fHRateBurst          =NULL;
  fHRateRun            =NULL;
  fHDigiToTVsROChannel =NULL;
  fHHitToTAll    =NULL;
  fHHitToT       =NULL;
  fHHitToTOrdered=NULL;
  fHHitToTLowThr =NULL;
  fHHitToTHighThr=NULL;
  fHHitToTLowThrIfHighThrExists=NULL;
  fHHitGeoChannelVsEdgeMask=NULL;

  for(Int_t i=0;i<4;++i){
    fHLowThrTOTVsLeadingTime[i]=NULL;
    fHEdgeMaskVsFirstTime[i]=NULL;
    fHCorrectedLeadingTimeWrtReferenceVsSlewingSlope[i]=NULL;
    fHCorrectedTrailingTimeWrtReferenceVsSlewingSlope[i]=NULL;
    fHHiThrTOTVsLeadingTime[i]=NULL;
  }
  for(Int_t iChan=0;iChan<4;++iChan)
    fGRateChLow [iChan] = NULL;
  for(Int_t iChan=0;iChan<4;++iChan)
    fGRateChHigh [iChan] = NULL;
  for(Int_t iChan=0;iChan<4;++iChan)
    fGRateChLowNorm [iChan] = NULL;
  for(Int_t iChan=0;iChan<4;++iChan)
    fGRateChHighNorm [iChan] = NULL;
  for(Int_t iChan=0;iChan<4;++iChan)
    fGHitRateChNorm [iChan] = NULL;

}

void SACReconstruction::DeleteHistograms(){
  if(fHRateBurst     )delete fHRateBurst     ;
  if(fHRateRun       )delete fHRateRun       ;
  if(fHDigiToTVsROChannel)delete fHDigiToTVsROChannel;
  if(fHHitToTAll    )delete fHHitToTAll    ;
  if(fHHitToT       )delete fHHitToT       ;
  if(fHHitToTOrdered)delete fHHitToTOrdered;
  if(fHHitToTLowThr )delete fHHitToTLowThr ;
  if(fHHitToTHighThr)delete fHHitToTHighThr;
  if(fHHitToTLowThrIfHighThrExists)delete  fHHitToTLowThrIfHighThrExists;
  if(fHHitGeoChannelVsEdgeMask    )delete fHHitGeoChannelVsEdgeMask;

  for(Int_t iChan=0;iChan<4;++iChan){
    if(fHLowThrTOTVsLeadingTime[iChan])delete fHLowThrTOTVsLeadingTime[iChan];
    if(fHCorrectedTrailingTimeWrtReferenceVsSlewingSlope[iChan])delete fHCorrectedTrailingTimeWrtReferenceVsSlewingSlope[iChan];
    if(fHCorrectedLeadingTimeWrtReferenceVsSlewingSlope [iChan])delete fHCorrectedLeadingTimeWrtReferenceVsSlewingSlope [iChan];
    if(fHEdgeMaskVsFirstTime[iChan])delete fHEdgeMaskVsFirstTime[iChan];
    if(fHHiThrTOTVsLeadingTime[iChan])delete fHHiThrTOTVsLeadingTime[iChan];
    if (fGRateChLow [iChan]) delete fGRateChLow[iChan];
    if (fGRateChHigh [iChan]) delete fGRateChHigh[iChan];
    if (fGRateChLowNorm [iChan]) delete fGRateChLowNorm[iChan];
    if (fGRateChHighNorm [iChan]) delete fGRateChHighNorm[iChan];
    if (fGHitRateChNorm [iChan]) delete fGHitRateChNorm[iChan];
  }
  ResetHistograms();
  return;
}
