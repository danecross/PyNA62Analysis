// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
// Modified by Monica Pepe (monica.pepe@cern.ch)
// ---------------------------------------------------------------

#include "Riostream.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TStyle.h"
#include "NA62Global.hh"

#include "RICHOnlineMonitor.hh"
#include "RICHReconstruction.hh"

RICHOnlineMonitor::RICHOnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow,Reco, "RICH"), fHNTimeCandidatesPerBurst(nullptr), fHNRingCandidatesPerBurst(nullptr), fHNHitTimeCandidatePerBurst(nullptr), fHNHitRingCandidatePerBurst(nullptr){

  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::AddDigiTimeRawFinePlots();
  NA62VOnlineMonitor::AddDecoderErrorsPlots();
  NA62VOnlineMonitor::CompleteTab();
}

void RICHOnlineMonitor::CreateShifterModeTabs(){
  //------- PM & SC Illumination ---------------

  NA62VOnlineMonitorCanvas * Illumination = AddCanvasTab("Illumination");
  Illumination->Divide(2,2);
  gStyle->SetOptStat(0);

  Illumination->cd(1);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationJura()->SetMarkerSize(3);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationJura()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationJura()->GetYaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationJura()->GetXaxis()->SetLabelSize(.05);
  Illumination->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationJura(),"COLZ");

  Illumination->cd(2);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationSaleve()->SetMarkerSize(3);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationSaleve()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationSaleve()->GetYaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationSaleve()->GetXaxis()->SetLabelSize(.05);
  Illumination->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationSaleve(),"COLZ");

  Illumination->cd(3);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancyJura()->SetTitleSize(0.06);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancyJura()->GetXaxis()->SetTitleOffset(0.8);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancyJura()->GetXaxis()->SetTitleSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancyJura()->GetXaxis()->SetLabelSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancyJura()->GetXaxis()->SetTitle("RICH Jura PM RO Channel Number");
  Illumination->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHROOccupancyJura());

  Illumination->cd(4);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancySaleve()->SetTitleSize(0.06);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancySaleve()->GetXaxis()->SetTitleOffset(0.8);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancySaleve()->GetXaxis()->SetTitleSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancySaleve()->GetXaxis()->SetLabelSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancySaleve()->GetXaxis()->SetTitle("RICH Saleve PM RO Channel Number");
  Illumination->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHROOccupancySaleve());

  //------- Summary  ---------------

  NA62VOnlineMonitorCanvas * Summary = AddCanvasTab("Summary");
  Summary->Divide(2,2);
  gStyle->SetOptStat(0);

  Summary->cd(1);
  Double_t x1 = 0.65;
  Double_t y1 = 0.35;
  Double_t x2 = 0.98;
  Double_t y2 = 0.55;
  TLegend * NHitsPerCandidateLegend = new TLegend(x1,y1,x2,y2);
  gPad->SetLogy();
  static_cast<RICHReconstruction*>(fReco)->GetHNHitperTimeCandidate()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHNHitperTimeCandidate()->GetXaxis()->SetLabelSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHNHitperTimeCandidate()->GetXaxis()->SetTitle("Number of hits per reco candidate");
  Summary->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNHitperTimeCandidate());
  static_cast<RICHReconstruction*>(fReco)->GetHNHitperRingCandidate()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHNHitperRingCandidate()->GetXaxis()->SetLabelSize(.05);
  Summary->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNHitperRingCandidate(),"same");
  NHitsPerCandidateLegend->SetFillColor(kWhite);
  NHitsPerCandidateLegend->AddEntry(static_cast<RICHReconstruction*>(fReco)->GetHNHitperTimeCandidate(),"Time Candidates", "L");
  NHitsPerCandidateLegend->AddEntry(static_cast<RICHReconstruction*>(fReco)->GetHNHitperRingCandidate(),"Ring Candidates", "L");
  NHitsPerCandidateLegend->Draw();

  Summary->cd(2);
  static_cast<RICHReconstruction*>(fReco)->GetHRingRadius()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHRingRadius()->GetXaxis()->SetLabelSize(.05);
  Summary->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHRingRadius());

  Summary->cd(3);
  static_cast<RICHReconstruction*>(fReco)->GetHTimeResolution()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHTimeResolution()->GetXaxis()->SetLabelSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHTimeResolution()->SetTitle("T1 - T2 [ns]");
  Summary->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHTimeResolution());

  Summary->cd(4);
  Summary->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHHitCandidateTimeDiffvsROChannel(),"COLZ");
}

void RICHOnlineMonitor::CreateExpertModeTabs(){
  //------- PM & SC Illumination ---------------

  NA62VOnlineMonitorCanvas * Illumination = AddCanvasTab("Illumination");
  Illumination->Divide(2,2);
  gStyle->SetOptStat(0);

  Illumination->cd(1);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationJura()->SetMarkerSize(3);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationJura()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationJura()->GetYaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationJura()->GetXaxis()->SetLabelSize(.05);
  Illumination->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationJura(),"COLZ");

  Illumination->cd(2);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationSaleve()->SetMarkerSize(3);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationSaleve()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationSaleve()->GetYaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationSaleve()->GetXaxis()->SetLabelSize(.05);
  Illumination->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHPMTIlluminationSaleve(),"COLZ");

  Illumination->cd(3);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellIlluminationJura()->SetMarkerSize(3);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellIlluminationJura()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellIlluminationJura()->GetYaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellIlluminationJura()->GetXaxis()->SetLabelSize(.05);
  Illumination->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHSuperCellIlluminationJura(),"COLZ");

  Illumination->cd(4);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellIlluminationSaleve()->SetMarkerSize(3);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellIlluminationSaleve()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellIlluminationSaleve()->GetYaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellIlluminationSaleve()->GetXaxis()->SetLabelSize(.05);
  Illumination->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHSuperCellIlluminationSaleve(),"COLZ");

  //------- PM Occupancy  ---------------

  NA62VOnlineMonitorCanvas * PMOccupancy = AddCanvasTab("ROChannelOccupancy_PM");
  PMOccupancy->Divide(1,2);
  gStyle->SetOptStat(0);

  PMOccupancy->cd(1);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancyJura()->SetTitleSize(0.06);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancyJura()->GetXaxis()->SetTitleOffset(0.8);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancyJura()->GetXaxis()->SetTitleSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancyJura()->GetXaxis()->SetLabelSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancyJura()->GetXaxis()->SetTitle("RICH Jura PM RO Channel Number");
  PMOccupancy->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHROOccupancyJura());

  PMOccupancy->cd(2);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancySaleve()->SetTitleSize(0.06);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancySaleve()->GetXaxis()->SetTitleOffset(0.8);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancySaleve()->GetXaxis()->SetTitleSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancySaleve()->GetXaxis()->SetLabelSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHROOccupancySaleve()->GetXaxis()->SetTitle("RICH Saleve PM RO Channel Number");
  PMOccupancy->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHROOccupancySaleve());

  //------- SC Occupancy  ---------------

  NA62VOnlineMonitorCanvas * SCOccupancy = AddCanvasTab("ROChannelOccupancy_SC");
  SCOccupancy->Divide(1,2);
  gStyle->SetOptStat(0);

  SCOccupancy->cd(1);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellROOccupancyJura()->SetTitleSize(0.06);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellROOccupancyJura()->GetXaxis()->SetTitleOffset(0.8);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellROOccupancyJura()->GetXaxis()->SetTitleSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellROOccupancyJura()->GetXaxis()->SetLabelSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellROOccupancyJura()->GetXaxis()->SetTitle("RICH Jura SC RO Channel Number");
  SCOccupancy->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHSuperCellROOccupancyJura());

  SCOccupancy->cd(2);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellROOccupancySaleve()->SetTitleSize(0.06);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellROOccupancySaleve()->GetXaxis()->SetTitleOffset(0.8);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellROOccupancySaleve()->GetXaxis()->SetTitleSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellROOccupancySaleve()->GetXaxis()->SetLabelSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHSuperCellROOccupancySaleve()->GetXaxis()->SetTitle("RICH Saleve SC RO Channel Number");
  SCOccupancy->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHSuperCellROOccupancySaleve());


 //------- ChannelCheck  ---------------

  
  NA62VOnlineMonitorCanvas * ChannelCheck = AddCanvasTab("ChannelCheck");
  ChannelCheck->Divide(1,3);
  gStyle->SetOptStat(0);

  ChannelCheck->cd(1);
  
  static_cast<RICHReconstruction*>(fReco)->GetHHitWidthvsROChannel()->SetTitleSize(0.06);
  static_cast<RICHReconstruction*>(fReco)->GetHHitWidthvsROChannel()->GetXaxis()->SetTitleOffset(0.8);
  static_cast<RICHReconstruction*>(fReco)->GetHHitWidthvsROChannel()->GetXaxis()->SetTitleSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHHitWidthvsROChannel()->GetXaxis()->SetLabelSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHHitWidthvsROChannel()->GetXaxis()->SetTitle("RICH PM Width vs RO Channel Number");
  ChannelCheck->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHHitWidthvsROChannel(),"COLZ");

  ChannelCheck->cd(2);
  static_cast<RICHReconstruction*>(fReco)->GetHHitLeadingTimevsROChannel()->SetTitleSize(0.06);
  static_cast<RICHReconstruction*>(fReco)->GetHHitLeadingTimevsROChannel()->GetXaxis()->SetTitleOffset(0.8);
  static_cast<RICHReconstruction*>(fReco)->GetHHitLeadingTimevsROChannel()->GetXaxis()->SetTitleSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHHitLeadingTimevsROChannel()->GetXaxis()->SetLabelSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHHitLeadingTimevsROChannel()->GetXaxis()->SetTitle("RICH PM Leading Time vs RO Channel Number");
  ChannelCheck->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHHitLeadingTimevsROChannel(),"COLZ");

  ChannelCheck->cd(3);
  static_cast<RICHReconstruction*>(fReco)->GetHSCLeadingTimevsROChannel()->SetTitleSize(0.06);
  static_cast<RICHReconstruction*>(fReco)->GetHSCLeadingTimevsROChannel()->GetXaxis()->SetTitleOffset(0.8);
  static_cast<RICHReconstruction*>(fReco)->GetHSCLeadingTimevsROChannel()->GetXaxis()->SetTitleSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHSCLeadingTimevsROChannel()->GetXaxis()->SetLabelSize(.06);
  static_cast<RICHReconstruction*>(fReco)->GetHSCLeadingTimevsROChannel()->GetXaxis()->SetTitle("RICH SC Leading Time vs RO Channel Number");
  ChannelCheck->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHSCLeadingTimevsROChannel(),"COLZ");


  //--------------- Number of Hits PMs ----------------------

  NA62VOnlineMonitorCanvas * HitsPM = AddCanvasTab("HitsPM");
  HitsPM->Divide(2,3);
  gStyle->SetOptStat(1);
  HitsPM->cd(1);
  gPad->SetLogy();
  static_cast<RICHReconstruction*>(fReco)->GetHNTotHits()->GetXaxis()->SetTitleSize(.04);
  HitsPM->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNTotHits());
  HitsPM->cd(2);
  gPad->SetLogy();
  static_cast<RICHReconstruction*>(fReco)->GetHNRecoHits()->GetXaxis()->SetTitleSize(.04);
  HitsPM->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNRecoHits());
  HitsPM->cd(3);
  gPad->SetLogy();
  static_cast<RICHReconstruction*>(fReco)->GetHNHitsPMJura()->GetXaxis()->SetTitleSize(.04);
  HitsPM->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNHitsPMJura());
  HitsPM->cd(4);
  gPad->SetLogy();
  static_cast<RICHReconstruction*>(fReco)->GetHNRecoHitsPMJ()->GetXaxis()->SetTitleSize(.04);
  HitsPM->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNRecoHitsPMJ());
  HitsPM->cd(5);
  gPad->SetLogy();
  static_cast<RICHReconstruction*>(fReco)->GetHNHitsPMSaleve()->GetXaxis()->SetTitleSize(.04);
  HitsPM->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNHitsPMSaleve());
  HitsPM->cd(6);
  gPad->SetLogy();
  static_cast<RICHReconstruction*>(fReco)->GetHNRecoHitsPMS()->GetXaxis()->SetTitleSize(.04);
  HitsPM->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNRecoHitsPMS());

//-------- Number of Hits SuperCells ----------------

  NA62VOnlineMonitorCanvas * HitsSC = AddCanvasTab("HitsSC");
  HitsSC->Divide(2,2);
  gStyle->SetOptStat(1);
  HitsSC->cd(1);
  gPad->SetLogy();
  static_cast<RICHReconstruction*>(fReco)->GetHNHitsSCJura()->GetXaxis()->SetTitleSize(.04);
  HitsSC->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNHitsSCJura());
  HitsSC->cd(2);
  gPad->SetLogy();
  static_cast<RICHReconstruction*>(fReco)->GetHNRecoHitsSCJ()->GetXaxis()->SetTitleSize(.04);
  HitsSC->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNRecoHitsSCJ());
  HitsSC->cd(3);
  gPad->SetLogy();
  static_cast<RICHReconstruction*>(fReco)->GetHNHitsSCSaleve()->GetXaxis()->SetTitleSize(.04);
  HitsSC->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNHitsSCSaleve());
  HitsSC->cd(4);
  gPad->SetLogy();
  static_cast<RICHReconstruction*>(fReco)->GetHNRecoHitsSCS()->GetXaxis()->SetTitleSize(.04);
  HitsSC->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNRecoHitsSCS());

  //------- Signal quality ---------------

  NA62VOnlineMonitorCanvas * SignalQuality = AddCanvasTab("SignalQuality");
  SignalQuality->Divide(2,2);
  SignalQuality->cd(1);
  SignalQuality->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHHitStatus());

  SignalQuality->cd(2);
  static_cast<RICHReconstruction*>(fReco)->GetHWidth()->SetTitle("PM Time Width (ns)");
  static_cast<RICHReconstruction*>(fReco)->GetHWidth()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHWidth()->SetLabelSize(.05);
  SignalQuality->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHWidth());

  SignalQuality->cd(3);
  gPad->SetLogz();
  SignalQuality->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHHitCandidateTimeDiffvsROChannel(),"COLZ");

  SignalQuality->cd(4);
  gPad->SetLogz();
  SignalQuality->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHHitCandidateTimeDiffvsWidth(),"COLZ");

  
  //------- Summary  ---------------

  NA62VOnlineMonitorCanvas * Summary = AddCanvasTab("Summary");
  Summary->Divide(2,2);
  gStyle->SetOptStat(0);

  Summary->cd(1);
  static_cast<RICHReconstruction*>(fReco)->GetHTimeResolution()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHTimeResolution()->GetXaxis()->SetLabelSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHTimeResolution()->SetTitle("T1 - T2 [ns]");
  Summary->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHTimeResolution());

  Summary->cd(2);
  static_cast<RICHReconstruction*>(fReco)->GetHRingRadius()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHRingRadius()->GetXaxis()->SetLabelSize(.05);
  Summary->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHRingRadius());

  Summary->cd(3);
  Double_t x1 = 0.65;
  Double_t y1 = 0.35;
  Double_t x2 = 0.98;
  Double_t y2 = 0.55;
  TLegend * NCandidatesLegend = new TLegend(x1,y1,x2,y2);

  gPad->SetLogy();
  static_cast<RICHReconstruction*>(fReco)->GetHNRingCandidates()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHNRingCandidates()->GetXaxis()->SetLabelSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHNRingCandidates()->GetXaxis()->SetTitle("Number of reco candidates");
  Summary->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNRingCandidates());
  static_cast<RICHReconstruction*>(fReco)->GetHNTimeCandidates()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHNTimeCandidates()->GetXaxis()->SetLabelSize(.05);
  Summary->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNTimeCandidates(),"same");
  NCandidatesLegend->SetFillColor(kWhite);
  NCandidatesLegend->AddEntry(static_cast<RICHReconstruction*>(fReco)->GetHNRingCandidates(),"Ring Candidates", "L");
  NCandidatesLegend->AddEntry(static_cast<RICHReconstruction*>(fReco)->GetHNTimeCandidates(),"Time Candidates", "L");
  NCandidatesLegend->Draw();

  Summary->cd(4);
  gPad->SetLogy();
  static_cast<RICHReconstruction*>(fReco)->GetHNHitperTimeCandidate()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHNHitperTimeCandidate()->GetXaxis()->SetLabelSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHNHitperTimeCandidate()->GetXaxis()->SetTitle("Number of hits per reco candidate");
  Summary->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNHitperTimeCandidate());
  static_cast<RICHReconstruction*>(fReco)->GetHNHitperRingCandidate()->GetXaxis()->SetTitleSize(.05);
  static_cast<RICHReconstruction*>(fReco)->GetHNHitperRingCandidate()->GetXaxis()->SetLabelSize(.05);
  Summary->GetCurrentFrame()->DrawHisto(static_cast<RICHReconstruction*>(fReco)->GetHNHitperRingCandidate(),"same");
  NCandidatesLegend->Draw();


//------ Stability
  NA62VOnlineMonitorCanvas * Stability = AddCanvasTab("Stability");
  TString DrawOption = "ALP";
  Stability->Divide(2,2);

  Stability->cd(1);

  fHNTimeCandidatesPerBurst = new TGraph(); 
  fHNTimeCandidatesPerBurst->Set(1);
  fHNTimeCandidatesPerBurst->SetPoint(0,-1,0);
  fHNTimeCandidatesPerBurst->SetTitle("NTimeCandidates VS Burst number;BurstID;NTimeCandidates");
  fHNTimeCandidatesPerBurst->SetMarkerStyle(20);
  fHNTimeCandidatesPerBurst->SetMarkerSize(0.7);
  fHNTimeCandidatesPerBurst->SetMarkerColor(kRed);
  fHNTimeCandidatesPerBurst->SetLineColor(kRed);
  fHNTimeCandidatesPerBurst->SetLineWidth(3);
  Stability->GetCurrentFrame()->DrawHisto(fHNTimeCandidatesPerBurst,DrawOption);

  Stability->cd(2);

  fHNRingCandidatesPerBurst = new TGraph(); 
  fHNRingCandidatesPerBurst->Set(1);
  fHNRingCandidatesPerBurst->SetPoint(0,-1,0);
  fHNRingCandidatesPerBurst->SetTitle("NRingCandidates VS Burst number;BurstID;NRingCandidates");
  fHNRingCandidatesPerBurst->SetMarkerStyle(20);
  fHNRingCandidatesPerBurst->SetMarkerSize(0.7);
  fHNRingCandidatesPerBurst->SetMarkerColor(kBlue);
  fHNRingCandidatesPerBurst->SetLineColor(kBlue);
  fHNRingCandidatesPerBurst->SetLineWidth(3);
  Stability->GetCurrentFrame()->DrawHisto(fHNRingCandidatesPerBurst,DrawOption);

 Stability->cd(3);

  fHNHitTimeCandidatePerBurst = new TGraph(); 
  fHNHitTimeCandidatePerBurst->Set(1);
  fHNHitTimeCandidatePerBurst->SetPoint(0,-1,0);
  fHNHitTimeCandidatePerBurst->SetTitle("NHits per TimeCandidate VS Burst number;BurstID;NHits per TimeCandidate");
  fHNHitTimeCandidatePerBurst->SetMarkerStyle(20);
  fHNHitTimeCandidatePerBurst->SetMarkerSize(0.7);
  fHNHitTimeCandidatePerBurst->SetMarkerColor(kRed);
  fHNHitTimeCandidatePerBurst->SetLineColor(kRed);
  fHNHitTimeCandidatePerBurst->SetLineWidth(3);
  Stability->GetCurrentFrame()->DrawHisto(fHNHitTimeCandidatePerBurst,DrawOption);

  Stability->cd(4);

  fHNHitRingCandidatePerBurst = new TGraph(); 
  fHNHitRingCandidatePerBurst->Set(1);
  fHNHitRingCandidatePerBurst->SetPoint(0,-1,0);
  fHNHitRingCandidatePerBurst->SetTitle("NHits per RingCandidate VS Burst number;BurstID;NHits per RingCandidate");
  fHNHitRingCandidatePerBurst->SetMarkerStyle(20);
  fHNHitRingCandidatePerBurst->SetMarkerSize(0.7);
  fHNHitRingCandidatePerBurst->SetMarkerColor(kBlue);
  fHNHitRingCandidatePerBurst->SetLineColor(kBlue);
  fHNHitRingCandidatePerBurst->SetLineWidth(3);
  Stability->GetCurrentFrame()->DrawHisto(fHNHitRingCandidatePerBurst,DrawOption);
}

RICHOnlineMonitor::~RICHOnlineMonitor() {}


void RICHOnlineMonitor::Update(Int_t BurstID) {

  RICHReconstruction * RICHReco = static_cast<RICHReconstruction*>( fReco );

 //--- --- update RICH Stability tab
  Double_t LastValueX=-1., LastValueY=0.;

  // HNTimeCandidatesPerBurst 
  if(fHNTimeCandidatesPerBurst){
    if(fHNTimeCandidatesPerBurst->GetN()>0) {
      fHNTimeCandidatesPerBurst->GetPoint(fHNTimeCandidatesPerBurst->GetN()-1,LastValueX,LastValueY);
      if(LastValueX==BurstID || LastValueX<0) fHNTimeCandidatesPerBurst->RemovePoint(fHNTimeCandidatesPerBurst->GetN()-1); //remove last point
    }
    fHNTimeCandidatesPerBurst->Set(fHNTimeCandidatesPerBurst->GetN()+1);
    Double_t NMeanNTimeCandidates = RICHReco->GetHNTimeCandidates()->GetMean();
    fHNTimeCandidatesPerBurst->SetPoint(fHNTimeCandidatesPerBurst->GetN()-1,BurstID,NMeanNTimeCandidates);
  }

  // HNRingCandidatesPerBurst 
  if(fHNRingCandidatesPerBurst) {
    if(fHNRingCandidatesPerBurst->GetN()>0) {
      fHNRingCandidatesPerBurst->GetPoint(fHNRingCandidatesPerBurst->GetN()-1,LastValueX,LastValueY);
      if(LastValueX==BurstID || LastValueX<0) fHNRingCandidatesPerBurst->RemovePoint(fHNRingCandidatesPerBurst->GetN()-1); //remove last point
    }
    fHNRingCandidatesPerBurst->Set(fHNRingCandidatesPerBurst->GetN()+1);
    Double_t NMeanNRingCandidates = RICHReco->GetHNRingCandidates()->GetMean();
    fHNRingCandidatesPerBurst->SetPoint(fHNRingCandidatesPerBurst->GetN()-1,BurstID,NMeanNRingCandidates);
  }

  // HNHitsTimeCandidatePerBurst 
  if(fHNHitTimeCandidatePerBurst) {
    if(fHNHitTimeCandidatePerBurst->GetN()>0) {
      fHNHitTimeCandidatePerBurst->GetPoint(fHNHitTimeCandidatePerBurst->GetN()-1,LastValueX,LastValueY);
      if(LastValueX==BurstID || LastValueX<0) fHNHitTimeCandidatePerBurst->RemovePoint(fHNHitTimeCandidatePerBurst->GetN()-1); //remove last point
    }
    fHNHitTimeCandidatePerBurst->Set(fHNHitTimeCandidatePerBurst->GetN()+1);
    Double_t NMeanNHitTimeCandidate = RICHReco->GetHNHitperTimeCandidate()->GetMean();
    fHNHitTimeCandidatePerBurst->SetPoint(fHNHitTimeCandidatePerBurst->GetN()-1,BurstID,NMeanNHitTimeCandidate);
  }

  // HNHitRingCandidatePerBurst 
  if(fHNHitRingCandidatePerBurst) {
    if(fHNHitRingCandidatePerBurst->GetN()>0) {
      fHNHitRingCandidatePerBurst->GetPoint(fHNHitRingCandidatePerBurst->GetN()-1,LastValueX,LastValueY);
      if(LastValueX==BurstID || LastValueX<0) fHNHitRingCandidatePerBurst->RemovePoint(fHNHitRingCandidatePerBurst->GetN()-1); //remove last point
    }
    fHNHitRingCandidatePerBurst->Set(fHNHitRingCandidatePerBurst->GetN()+1);
    Double_t NMeanNHitRingCandidate = RICHReco->GetHNHitperRingCandidate()->GetMean();
    fHNHitRingCandidatePerBurst->SetPoint(fHNHitRingCandidatePerBurst->GetN()-1,BurstID,NMeanNHitRingCandidate);
  }

  NA62VOnlineMonitor::Update(BurstID);
}
