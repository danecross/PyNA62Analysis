// ---------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

/// \class MUV0OnlineMonitor
/// \Brief
/// Online monitoring for the MUV0 reconstruction
/// \EndBrief

#include "Riostream.h"
#include "TCanvas.h"
#include "TText.h"
#include "TLatex.h"
#include "TEllipse.h"
#include "TBox.h"
#include "TLine.h"
#include "NA62Global.hh"
#include "MUV0OnlineMonitor.hh"
#include "MUV0Reconstruction.hh"

MUV0OnlineMonitor::MUV0OnlineMonitor(TRootBrowser* MainWindow, NA62VReconstruction* Reco, Int_t OMMode) : NA62VOnlineMonitor(MainWindow, Reco, "MUV0") {
  if(OMMode==kShifter) CreateShifterModeTabs();
  else CreateExpertModeTabs();

  NA62VOnlineMonitor::AddDigiTimeRawFinePlots();
  NA62VOnlineMonitor::AddDecoderErrorsPlots();
  NA62VOnlineMonitor::CompleteTab();
}

void MUV0OnlineMonitor::CreateShifterModeTabs(){

  MUV0Reconstruction* const recoMUV0 = static_cast<MUV0Reconstruction*>( fReco);

  NA62VOnlineMonitorCanvas* Occupancy = AddCanvasTab("Occupancy");

  Occupancy->Divide(2,2);
  Occupancy->cd(1);
  Occupancy->GetCurrentFrame()->DrawHisto(recoMUV0->GetHIllumination(),
    "COLZ, HA");
  TLatex* Sside = new TLatex(-720, -200, "Sal#grave{e}ve");
  Sside->SetTextSize(0.08);
  Sside->SetTextAngle(90);
  Sside->Draw();
  TLatex* Jside = new TLatex(720, 150, "Jura");
  Jside->SetTextSize(0.08);
  Jside->SetTextAngle(-90);
  Jside->Draw();
  TEllipse* BeamDot = new TEllipse(-830,0,25);
  BeamDot->SetFillColor(kBlack);
  BeamDot->Draw();
  TLatex* Beam = new TLatex(-867, 50, "Beam");
  Beam->SetTextSize(0.03);
  Beam->Draw();
  TText* Slab[9];
  Slab[0] = new TText(-400,-500,"0");
  Slab[1] = new TText(-400,-100,"1");
  Slab[2] = new TText(-400,400,"2");
  Slab[3] = new TText(100,-500,"3");
  Slab[4] = new TText(100,-100,"4");
  Slab[5] = new TText(100,400,"5");
  Slab[6] = new TText(500,-500,"6");
  Slab[7] = new TText(500,-100,"7");
  Slab[8] = new TText(500,400,"8");
  for (Int_t i=0; i<9; i++) {
    Slab[i]->Draw();
  }
  TLine* Sline[8];
  Sline[0] = new TLine(-700, 700, -700, -700);
  Sline[1] = new TLine(-100, 700, -100, -700);
  Sline[2] = new TLine(300, 700, 300, -700);
  Sline[3] = new TLine(700, 700, 700, -700);
  Sline[4] = new TLine(700, -700, -700, -700);
  Sline[5] = new TLine(700, -300, -700, -300);
  Sline[6] = new TLine(700, 100, -700, 100);
  Sline[7] = new TLine(700, 700, -700, 700);
  for(Int_t i=0; i<8; i++) {
    Sline[i]->SetLineWidth(4);
    Sline[i]->Draw();
  }
  TBox* CutOut = new TBox(700, -500, 500, -700);
  CutOut->SetFillColor(kBlack);
  CutOut->Draw();
  Occupancy->cd(2);
  Occupancy->GetCurrentFrame()->DrawHisto(recoMUV0->GetHHitMap());
  Occupancy->cd(3);
  Occupancy->GetCurrentFrame()->DrawHisto(recoMUV0->GetHNDigis());
  Occupancy->cd(4);
  Occupancy->GetCurrentFrame()->DrawHisto(recoMUV0->GetHNRecoHits());
}

void MUV0OnlineMonitor::CreateExpertModeTabs(){

  MUV0Reconstruction* const recoMUV0 = static_cast<MUV0Reconstruction*>( fReco);

  NA62VOnlineMonitorCanvas* Occupancy = AddCanvasTab("Occupancy");

  Occupancy->Divide(2,2);
  Occupancy->cd(1);
  Occupancy->GetCurrentFrame()->DrawHisto(recoMUV0->GetHIllumination(),
    "COLZ, HA");
  TLatex* Sside = new TLatex(-720, -200, "Sal#grave{e}ve");
  Sside->SetTextSize(0.08);
  Sside->SetTextAngle(90);
  Sside->Draw();
  TLatex* Jside = new TLatex(720, 150, "Jura");
  Jside->SetTextSize(0.08);
  Jside->SetTextAngle(-90);
  Jside->Draw();
  TEllipse* BeamDot = new TEllipse(-830,0,25);
  BeamDot->SetFillColor(kBlack);
  BeamDot->Draw();
  TLatex* Beam = new TLatex(-867, 50, "Beam");
  Beam->SetTextSize(0.03);
  Beam->Draw();
  TText* Slab[9];
  Slab[0] = new TText(-400,-500,"0");
  Slab[1] = new TText(-400,-100,"1");
  Slab[2] = new TText(-400,400,"2");
  Slab[3] = new TText(100,-500,"3");
  Slab[4] = new TText(100,-100,"4");
  Slab[5] = new TText(100,400,"5");
  Slab[6] = new TText(500,-500,"6");
  Slab[7] = new TText(500,-100,"7");
  Slab[8] = new TText(500,400,"8");
  for (Int_t i=0; i<9; i++) {
    Slab[i]->Draw();
  }
  TLine* Sline[8];
  Sline[0] = new TLine(-700, 700, -700, -700);
  Sline[1] = new TLine(-100, 700, -100, -700);
  Sline[2] = new TLine(300, 700, 300, -700);
  Sline[3] = new TLine(700, 700, 700, -700);
  Sline[4] = new TLine(700, -700, -700, -700);
  Sline[5] = new TLine(700, -300, -700, -300);
  Sline[6] = new TLine(700, 100, -700, 100);
  Sline[7] = new TLine(700, 700, -700, 700);
  for(Int_t i=0; i<8; i++) {
    Sline[i]->SetLineWidth(4);
    Sline[i]->Draw();
  }
  TBox* CutOut = new TBox(700, -500, 500, -700);
  CutOut->SetFillColor(kBlack);
  CutOut->Draw();
  Occupancy->cd(2);
  Occupancy->GetCurrentFrame()->DrawHisto(recoMUV0->GetHHitMap());
  Occupancy->cd(3);
  Occupancy->GetCurrentFrame()->DrawHisto(recoMUV0->GetHNDigis());
  Occupancy->cd(4);
  Occupancy->GetCurrentFrame()->DrawHisto(recoMUV0->GetHNRecoHits());

  NA62VOnlineMonitorCanvas* OccupancyEOB = AddCanvasTab("OccupancyEOB");

  OccupancyEOB->Divide(1,2);
  OccupancyEOB->cd(1);
  OccupancyEOB->GetCurrentFrame()->DrawHisto(recoMUV0->GetHHitMapEOBLowThr());
  OccupancyEOB->cd(2);
  OccupancyEOB->GetCurrentFrame()->DrawHisto(recoMUV0->GetHHitMapEOBHighThr());

  NA62VOnlineMonitorCanvas* TimeNoT0 = AddCanvasTab("Time No T0");

  TimeNoT0->Divide(3,3);
  for (Int_t iChan=0; iChan<9; iChan++) {
    TimeNoT0->cd(iChan+1);
    TimeNoT0->GetCurrentFrame()->DrawHisto(recoMUV0->GetHTimeNoT0(iChan));
  }

/*
  ///////////////////////////
  // The only tab so far....

  NA62VOnlineMonitorCanvas* ChannelProfileTab = AddCanvasTab("ChannelProfile");
  ChannelProfileTab->Divide(3,2);
  ChannelProfileTab->cd(1);
  for (Int_t i=1; i<=6; i++) {
    ChannelProfileTab->GetCanvas()->GetPad(i)->SetLeftMargin(0.10);
    ChannelProfileTab->GetCanvas()->GetPad(i)->SetRightMargin(0.03);
    ChannelProfileTab->GetCanvas()->GetPad(i)->SetTopMargin(0.06);
    ChannelProfileTab->GetCanvas()->GetPad(i)->SetBottomMargin(0.06);
  }

  // Digi channel profile
  ChannelProfileTab->cd(1);
  ((MUV0Reconstruction*)fReco)->GetHChannelProfile()->SetTitle("MUV0 Digis: channel IDs");
  ((MUV0Reconstruction*)fReco)->GetHChannelProfile()->GetXaxis()->SetTitleSize(0.05);
  ((MUV0Reconstruction*)fReco)->GetHChannelProfile()->GetXaxis()->SetLabelSize(0.05);
  ((MUV0Reconstruction*)fReco)->GetHChannelProfile()->GetYaxis()->SetTitleSize(0.05);
  ((MUV0Reconstruction*)fReco)->GetHChannelProfile()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(((MUV0Reconstruction*)fReco)->GetHChannelProfile(),"hist");

  // Digi time wrt Cedar
  ChannelProfileTab->cd(2);
  ((MUV0Reconstruction*)fReco)->GetDigiTimeWrtReference()->SetTitle("Digi times wrt Cedar time");
  ((MUV0Reconstruction*)fReco)->GetDigiTimeWrtReference()->GetXaxis()->SetTitleSize(0.05);
  ((MUV0Reconstruction*)fReco)->GetDigiTimeWrtReference()->GetXaxis()->SetLabelSize(0.05);
  ((MUV0Reconstruction*)fReco)->GetDigiTimeWrtReference()->GetYaxis()->SetTitleSize(0.05);
  ((MUV0Reconstruction*)fReco)->GetDigiTimeWrtReference()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(((MUV0Reconstruction*)fReco)->GetDigiTimeWrtReference(),"hist");

  // Digi time wrt Cedar vs channel
  ChannelProfileTab->cd(3);
  ((MUV0Reconstruction*)fReco)->GetDigiTimeWrtReferenceVsChannel()->SetTitle("Digi times wrt Cedar time");
  ((MUV0Reconstruction*)fReco)->GetDigiTimeWrtReferenceVsChannel()->GetXaxis()->SetTitleSize(0.05);
  ((MUV0Reconstruction*)fReco)->GetDigiTimeWrtReferenceVsChannel()->GetXaxis()->SetLabelSize(0.05);
  ((MUV0Reconstruction*)fReco)->GetDigiTimeWrtReferenceVsChannel()->GetYaxis()->SetTitleSize(0.05);
  ((MUV0Reconstruction*)fReco)->GetDigiTimeWrtReferenceVsChannel()->GetYaxis()->SetLabelSize(0.05);
  ChannelProfileTab->GetCurrentFrame()->DrawHisto(((MUV0Reconstruction*)fReco)->GetDigiTimeWrtReferenceVsChannel(), "colz");
*/
}
