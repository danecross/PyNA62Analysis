// ---------------------------------------------------------------
//
// History:    
//
// Created by Viacheslav Duk (Viacheslav.Duk@cern.ch) 05.2017
// and Riccardo Lollini (riccardo.lollini@cern.ch) 05.2017
// 
// ---------------------------------------------------------------
/// \class CHODEfficiency
/// \Brief
/// Efficiency of the Spectrometer-CHOD association for the data quality control
/// \EndBrief
/// \Detailed
/// Good tracks associated with the NewCHOD
/// and forming a good vertex with the nominal kaon direction
/// are selected for the calculation of the efficiency 
/// of the Spectrometer-CHOD association.
/// The efficiency is defined as the fraction of selected tracks where this association exists.
/// This is a two-step analyzer. 
/// In the first step, only the histograms for the efficiency calculation are filled 
/// (numerator and denominator for all tracks, for single-track and for no-shower events). 
/// The histogram filled show the efficiency as a funcion of the burst ID, momentum and vertex with nominal kaon. 
/// In the second step (--histo command line option) the efficiencies are calculated reading the output
/// from the first step. A file with the list of bad bursts is created. 
/// The reason of the "bad burst" classification can be low efficiency or low statistics and 
/// it is stated in the bad burst file, next to the burst number.
/// Bad burst list is created from the single-track no-shower histogram. An efficiency > 0.9 is required.
/// \author Viacheslav Duk (Viacheslav.Duk@cern.ch)
/// and Riccardo Lollini (riccardo.lollini@cern.ch)
/// \EndDetailed 

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "CHODEfficiency.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"

#include "DownstreamTrack.hh"
#include "GeometricAcceptance.hh"
#include "BaseAnalysis.hh"
#include "SpectrometerCHODAssociationOutput.hh"
#include "ConfigSettings.hh"

#include <TStyle.h>
#include <TLine.h>
#include <TPDF.h>

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

CHODEfficiency::CHODEfficiency(Core::BaseAnalysis *ba) : Analyzer(ba, "CHODEfficiency")
{
  RequestTree("CHOD",new TRecoCHODEvent);
  RequestTree("Spectrometer",new TRecoSpectrometerEvent);
  RequestL0Data();
  Configuration::ConfigSettings::SetNoSkipBadBurst(true);// do not skip bad bursts

  fReadingData = kTRUE;
  fOutputPDFFileName = fAnalyzerName + ".pdf";

}

void CHODEfficiency::InitHist(){

  fReadingData = GetIsTree();

  fHIllumination = nullptr;

  // run analyzer only in the data reading mode
  if (fReadingData) {
    cout << user_normal() << "Reading reconstructed data" << endl;

    fHIllumination = static_cast<TH2F*>(RequestHistogram("CHODMonitor", "ChannelIllumination", true));
    if(fHIllumination) fHIllumination->SetName("hIllumination");

    BookHisto("hEfficiencyVsBurstID", new TGraphErrors());
    fHEfficiencyVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hEfficiencyVsBurstID");
    fHEfficiencyVsBurstID->SetName("hEfficiencyVsBurstID");
    fHEfficiencyVsBurstID->Set(0);
    BookHisto("hNExpectedVsBurstID", new TGraphErrors());
    fHNExpectedVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNExpectedVsBurstID");
    fHNExpectedVsBurstID->SetName("hNExpectedVsBurstID");
    fHNExpectedVsBurstID->Set(0);

    BookHisto("hEfficiencySingleTrackVsBurstID", new TGraphErrors());
    fHEfficiencySingleTrackVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hEfficiencySingleTrackVsBurstID");
    fHEfficiencySingleTrackVsBurstID->SetName("hEfficiencySingleTrackVsBurstID");
    fHEfficiencySingleTrackVsBurstID->Set(0);
    BookHisto("hNExpectedSingleTrackVsBurstID", new TGraphErrors());
    fHNExpectedSingleTrackVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNExpectedSingleTrackVsBurstID");
    fHNExpectedSingleTrackVsBurstID->SetName("hNExpectedSingleTrackVsBurstID");
    fHNExpectedSingleTrackVsBurstID->Set(0);

    BookHisto("hEfficiencyNoShowerVsBurstID", new TGraphErrors());
    fHEfficiencyNoShowerVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hEfficiencyNoShowerVsBurstID");
    fHEfficiencyNoShowerVsBurstID->SetName("hEfficiencyNoShowerVsBurstID");
    fHEfficiencyNoShowerVsBurstID->Set(0);
    BookHisto("hNExpectedNoShowerVsBurstID", new TGraphErrors());
    fHNExpectedNoShowerVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNExpectedNoShowerVsBurstID");
    fHNExpectedNoShowerVsBurstID->SetName("hNExpectedNoShowerVsBurstID");
    fHNExpectedNoShowerVsBurstID->Set(0);

    BookHisto("hEfficiencySingleTrackNoShowerVsBurstID", new TGraphErrors());
    fHEfficiencySingleTrackNoShowerVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hEfficiencySingleTrackNoShowerVsBurstID");
    fHEfficiencySingleTrackNoShowerVsBurstID->SetName("hEfficiencySingleTrackNoShowerVsBurstID");
    fHEfficiencySingleTrackNoShowerVsBurstID->Set(0);
    BookHisto("hNExpectedSingleTrackNoShowerVsBurstID", new TGraphErrors());
    fHNExpectedSingleTrackNoShowerVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNExpectedSingleTrackNoShowerVsBurstID");
    fHNExpectedSingleTrackNoShowerVsBurstID->SetName("hNExpectedSingleTrackNoShowerVsBurstID");
    fHNExpectedSingleTrackNoShowerVsBurstID->Set(0);
    BookHisto("hNMatchedSingleTrackNoShowerVsBurstID", new TGraphErrors());
    fHNMatchedSingleTrackNoShowerVsBurstID = (TGraphErrors*)fHisto.GetTGraph("hNMatchedSingleTrackNoShowerVsBurstID");
    fHNMatchedSingleTrackNoShowerVsBurstID->SetName("hNMatchedSingleTrackNoShowerVsBurstID");
    fHNMatchedSingleTrackNoShowerVsBurstID->Set(0);

    BookHisto(new TH1F("MomentumNumerator", "Efficiency vs momentum histogram, numerator", 70, 0., 70.));
    BookHisto(new TH1F("MomentumDenominator", "Efficiency vs momentum histogram, denominator", 70, 0., 70.));
    BookHisto(new TH1F("MomentumSingleTrackNumerator", "Single track efficiency vs momentum histogram, numerator", 70, 0., 70.));
    BookHisto(new TH1F("MomentumSingleTrackDenominator", "Single track efficiency vs momentum histogram, denominator", 70, 0., 70.));

    BookHisto(new TH1F("MomentumNoShowerNumerator", "Efficiency vs momentum histogram (no shower), numerator", 70, 0., 70.));
    BookHisto(new TH1F("MomentumNoShowerDenominator", "Efficiency vs momentum histogram (no shower), denominator", 70, 0., 70.));
    BookHisto(new TH1F("MomentumSingleTrackNoShowerNumerator", "Single track efficiency vs momentum histogram (no shower), numerator", 70, 0., 70.));
    BookHisto(new TH1F("MomentumSingleTrackNoShowerDenominator", "Single track efficiency vs momentum histogram (no shower), denominator", 70, 0., 70.));

    BookHisto(new TH1F("VertexNumerator", "Efficiency vs vertex histogram, numerator", 70, 100000., 170000.));
    BookHisto(new TH1F("VertexDenominator", "Efficiency vs vertex histogram, denominator", 70, 100000., 170000.));
    BookHisto(new TH1F("VertexSingleTrackNumerator", "Single track efficiency vs vertex histogram, numerator", 70, 100000., 170000.));
    BookHisto(new TH1F("VertexSingleTrackDenominator", "Single track efficiency vs vertex histogram, denominator", 70, 100000., 170000.));

    BookHisto(new TH1F("VertexNoShowerNumerator", "Efficiency vs vertex histogram (no shower), numerator", 70, 100000., 170000.));
    BookHisto(new TH1F("VertexNoShowerDenominator", "Efficiency vs vertex histogram (no shower), denominator", 70, 100000., 170000.));
    BookHisto(new TH1F("VertexSingleTrackNoShowerNumerator", "Single track efficiency vs vertex histogram (no shower), numerator", 70, 100000., 170000.));
    BookHisto(new TH1F("VertexSingleTrackNoShowerDenominator", "Single track efficiency vs vertex histogram (no shower), denominator", 70, 100000., 170000.));

    BookHisto(new TH2F("Efficiency2DNumerator", "Efficiency 2D, numerator", 32, fXBins, 32, fYBins));
    BookHisto(new TH2F("Efficiency2DDenominator", "Efficiency 2D, denominator", 32, fXBins, 32, fYBins));
    BookHisto(new TH2F("Efficiency2DSingleTrackNumerator", "Single track efficiency 2D, numerator", 32, fXBins, 32, fYBins));
    BookHisto(new TH2F("Efficiency2DSingleTrackDenominator", "Single track efficiency 2D, denominator", 32, fXBins, 32, fYBins));

    BookHisto(new TH2F("Efficiency2DNoShowerNumerator", "Efficiency 2D (no shower), numerator", 32, fXBins, 32, fYBins));
    BookHisto(new TH2F("Efficiency2DNoShowerDenominator", "Efficiency 2D (no shower), denominator", 32, fXBins, 32, fYBins));
    BookHisto(new TH2F("Efficiency2DSingleTrackNoShowerNumerator", "Single track efficiency 2D (no shower), numerator", 32, fXBins, 32, fYBins));
    BookHisto(new TH2F("Efficiency2DSingleTrackNoShowerDenominator", "Single track efficiency 2D (no shower), denominator", 32, fXBins, 32, fYBins));

  }
  else {
    cout << user_normal() << "Reading my own output" << endl;

    fHIllumination =static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "hIllumination", true));

    fHEfficiencyVsBurstID =(TGraphErrors*)RequestHistogram(fAnalyzerName, "hEfficiencyVsBurstID", true);
    fHNExpectedVsBurstID  =(TGraphErrors*)RequestHistogram(fAnalyzerName, "hNExpectedVsBurstID", true);
    fHEfficiencySingleTrackVsBurstID =(TGraphErrors*)RequestHistogram(fAnalyzerName, "hEfficiencySingleTrackVsBurstID", true);
    fHNExpectedSingleTrackVsBurstID =(TGraphErrors*)RequestHistogram(fAnalyzerName, "hNExpectedSingleTrackVsBurstID", true);
    fHEfficiencyNoShowerVsBurstID =(TGraphErrors*)RequestHistogram(fAnalyzerName, "hEfficiencyNoShowerVsBurstID", true);
    fHNExpectedNoShowerVsBurstID =(TGraphErrors*)RequestHistogram(fAnalyzerName, "hNExpectedNoShowerVsBurstID", true);
    fHEfficiencySingleTrackNoShowerVsBurstID =(TGraphErrors*)RequestHistogram(fAnalyzerName, "hEfficiencySingleTrackNoShowerVsBurstID", true);
    fHNExpectedSingleTrackNoShowerVsBurstID =(TGraphErrors*)RequestHistogram(fAnalyzerName, "hNExpectedSingleTrackNoShowerVsBurstID", true);
    
    fHNMatchedSingleTrackNoShowerVsBurstID =(TGraphErrors*)RequestHistogram(fAnalyzerName, "hNMatchedSingleTrackNoShowerVsBurstID", true);

    fHMomentumNumerator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "MomentumNumerator", true));
    fHMomentumDenominator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "MomentumDenominator", true));
    fHMomentumSingleTrackNumerator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "MomentumSingleTrackNumerator",true));
    fHMomentumSingleTrackDenominator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"MomentumSingleTrackDenominator", true));

    fHMomentumNoShowerNumerator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "MomentumNoShowerNumerator", true));
    fHMomentumNoShowerDenominator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "MomentumNoShowerDenominator", true));
    fHMomentumSingleTrackNoShowerNumerator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "MomentumSingleTrackNoShowerNumerator",true));
    fHMomentumSingleTrackNoShowerDenominator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"MomentumSingleTrackNoShowerDenominator", true));

    fHVertexNumerator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "VertexNumerator", true));
    fHVertexDenominator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "VertexDenominator", true));
    fHVertexSingleTrackNumerator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "VertexSingleTrackNumerator",true));
    fHVertexSingleTrackDenominator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"VertexSingleTrackDenominator", true));

    fHVertexNoShowerNumerator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "VertexNoShowerNumerator", true));
    fHVertexNoShowerDenominator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "VertexNoShowerDenominator", true));
    fHVertexSingleTrackNoShowerNumerator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "VertexSingleTrackNoShowerNumerator",true));
    fHVertexSingleTrackNoShowerDenominator = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"VertexSingleTrackNoShowerDenominator", true));

    fHEfficiency2DNumerator = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "Efficiency2DNumerator", true));
    fHEfficiency2DDenominator = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "Efficiency2DDenominator", true));
    fHEfficiency2DSingleTrackNumerator = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "Efficiency2DSingleTrackNumerator", true));
    fHEfficiency2DSingleTrackDenominator = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "Efficiency2DSingleTrackDenominator", true));

    fHEfficiency2DNoShowerNumerator = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "Efficiency2DNoShowerNumerator", true));
    fHEfficiency2DNoShowerDenominator = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "Efficiency2DNoShowerDenominator", true));
    fHEfficiency2DSingleTrackNoShowerNumerator = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "Efficiency2DSingleTrackNoShowerNumerator", true));
    fHEfficiency2DSingleTrackNoShowerDenominator = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "Efficiency2DSingleTrackNoShowerDenominator", true));
  }
}

void CHODEfficiency::Process(Int_t) {

  // run analyzer only in the data reading mode
  if (!fReadingData) return;

  fBurstID        = GetBurstID();
  Int_t  L0DataType    = GetL0Data()->GetDataType();
  Bool_t PhysicsData   = L0DataType    & 0x1;

  TRecoCHODEvent *CHODEvent = GetEvent<TRecoCHODEvent>();
  TRecoSpectrometerEvent *SpectrometerEvent = GetEvent<TRecoSpectrometerEvent>();
  Int_t ChambersInAcceptance;

  std::vector<DownstreamTrack> Tracks =
    *GetOutput<std::vector<DownstreamTrack>>("DownstreamTrackBuilder.Output");

  if(PhysicsData){
    for (UInt_t i = 0; i < Tracks.size(); i++) {
      // track geometric acceptance in STRAW
      ChambersInAcceptance = 0;
      for (Int_t iChamber=0; iChamber<4; iChamber++) {
          ChambersInAcceptance += GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[i], kSpectrometer, iChamber);
      }
      // track quality cuts
      if (ChambersInAcceptance!=4) continue;
      if(Tracks[i].GetNChambers()!=4) continue;
      if(Tracks[i].GetChi2()>20.) continue;
      if(fabs(Tracks[i].GetMomentumBeforeFit()-Tracks[i].GetMomentum())>20000.) continue;
      if(Tracks[i].GetMomentum()<10000. || Tracks[i].GetMomentum()>50000.) continue;
      // track in CHOD acceptance
      if(!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[i], kCHOD)) continue;
      // q>0
      if (Tracks[i].GetCharge()<0) continue;
      // vertex with nominal kaon
      if( Tracks[i].GetNominalBeamAxisCDA()>50. ||
              Tracks[i].GetNominalBeamAxisVertex().Z()<110000. || Tracks[i].GetNominalBeamAxisVertex().Z()>165000.) continue;
      // Spectrometer-NewCHOD association
      if(Tracks[i].GetNNewCHODAssociationRecords()!=1) continue;
      Double_t NewCHODTime = Tracks[i].GetNewCHODTime();

      Bool_t ShowerFlag = false;
      if ((CHODEvent->GetNHits()-2*SpectrometerEvent->GetNCandidates()) > 30) ShowerFlag = true;

      // denominator histograms (all events)
      fNExpectedPerBurst++;
      FillHisto("MomentumDenominator", Tracks[i].GetMomentum()*0.001);
      FillHisto("VertexDenominator", Tracks[i].GetNominalBeamAxisVertex().Z() );
      if(Tracks.size()==1){
          fNExpectedSingleTrackPerBurst++;
          FillHisto("MomentumSingleTrackDenominator", Tracks[i].GetMomentum()*0.001);
          FillHisto("VertexSingleTrackDenominator", Tracks[i].GetNominalBeamAxisVertex().Z() );
      }

      if (!ShowerFlag) {
          fNExpectedNoShowerPerBurst++;
          FillHisto("MomentumNoShowerDenominator", Tracks[i].GetMomentum()*0.001);
          FillHisto("VertexNoShowerDenominator", Tracks[i].GetNominalBeamAxisVertex().Z() );
          if(Tracks.size()==1){
              fNExpectedSingleTrackNoShowerPerBurst++;
              FillHisto("MomentumSingleTrackNoShowerDenominator", Tracks[i].GetMomentum()*0.001);
              FillHisto("VertexSingleTrackNoShowerDenominator", Tracks[i].GetNominalBeamAxisVertex().Z() );
          }
      }

      // numerator histograms ("efficient" events)
      Double_t CHODTime    = Tracks[i].GetCHODTime();
      if(Tracks[i].CHODAssociationExists() && fabs(CHODTime-NewCHODTime) < 4.){
          fNMatchedPerBurst++;
          FillHisto("MomentumNumerator", Tracks[i].GetMomentum()*0.001);
          FillHisto("VertexNumerator", Tracks[i].GetNominalBeamAxisVertex().Z() );
          if(Tracks.size()==1){
              fNMatchedSingleTrackPerBurst++;
              FillHisto("MomentumSingleTrackNumerator", Tracks[i].GetMomentum()*0.001);
              FillHisto("VertexSingleTrackNumerator", Tracks[i].GetNominalBeamAxisVertex().Z());
          }
      }

      if (!ShowerFlag && fabs(CHODTime-NewCHODTime) < 4.) {
          if(Tracks[i].CHODAssociationExists()){
              fNMatchedNoShowerPerBurst++;
              FillHisto("MomentumNoShowerNumerator", Tracks[i].GetMomentum()*0.001);
              FillHisto("VertexNoShowerNumerator", Tracks[i].GetNominalBeamAxisVertex().Z() );
              if(Tracks.size()==1){
                  fNMatchedSingleTrackNoShowerPerBurst++;
                  FillHisto("MomentumSingleTrackNoShowerNumerator", Tracks[i].GetMomentum()*0.001);
                  FillHisto("VertexSingleTrackNoShowerNumerator", Tracks[i].GetNominalBeamAxisVertex().Z());
              }
          }
      }

      Double_t Z_CHODVPlane = GeometricAcceptance::GetInstance()->GetZCHODVPlane();
      Double_t Z_CHODHPlane = GeometricAcceptance::GetInstance()->GetZCHODHPlane();
      
      // offsets for the track projection coordinates
      // The track projections on the CHOD plane
      // allow to calculate the detector position seen by tracks.
      // To keep the CHOD center in (0,0), the track coordinates are corrected
      Double_t xt = Tracks[i].xAtAfterMagnet(Z_CHODVPlane);
      Double_t yt = Tracks[i].yAtAfterMagnet(Z_CHODHPlane);
      Double_t xt_with_offset = xt -fXOffset;
      Double_t yt_with_offset = yt - fYOffset;

      FillHisto("Efficiency2DDenominator", xt_with_offset, yt_with_offset, 1);
      if (Tracks.size()==1) {
	FillHisto("Efficiency2DSingleTrackDenominator", xt_with_offset, yt_with_offset, 1);
      }
      if(Tracks[i].CHODAssociationExists() && fabs(CHODTime-NewCHODTime) < 4.){
	FillHisto("Efficiency2DNumerator", xt_with_offset, yt_with_offset, 1);
	if (Tracks.size()==1) {
	  FillHisto("Efficiency2DSingleTrackNumerator", xt_with_offset, yt_with_offset, 1);
	}
      }

      if (!ShowerFlag) {
	FillHisto("Efficiency2DNoShowerDenominator", xt_with_offset, yt_with_offset, 1);
	if (Tracks.size()==1) {
	  FillHisto("Efficiency2DSingleTrackNoShowerDenominator", xt_with_offset, yt_with_offset, 1);
	}
	if(Tracks[i].CHODAssociationExists() && fabs(CHODTime-NewCHODTime) < 4.){
	  FillHisto("Efficiency2DNoShowerNumerator", xt_with_offset, yt_with_offset, 1);
	  if (Tracks.size()==1) {
	    FillHisto("Efficiency2DSingleTrackNoShowerNumerator", xt_with_offset, yt_with_offset, 1);
	  }
	}    
      }  
      
    } // end cycle over tracks
  } // end physics triggers
}

void CHODEfficiency::EndOfJobUser(){
  // Data mode: save output
  if (fReadingData) {
    SaveAllPlots();
    return;
  }

  // Histo mode: evaluate efficiency
  fHMomentum                = static_cast<TH1F*>(fHMomentumNumerator->Clone("HMomentum"));
  fHMomentumSingleTrack     = static_cast<TH1F*>(fHMomentumSingleTrackNumerator->Clone("HMomentumSingleTrack"));
  fHMomentumNoShower                = static_cast<TH1F*>(fHMomentumNoShowerNumerator->Clone("HMomentumNoShower"));
  fHMomentumSingleTrackNoShower     = static_cast<TH1F*>(fHMomentumSingleTrackNoShowerNumerator->Clone("HMomentumSingleTrackNoShower"));
  fHVertex                  = static_cast<TH1F*>(fHVertexNumerator->Clone("HVertex"));
  fHVertexSingleTrack       = static_cast<TH1F*>(fHVertexSingleTrackNumerator->Clone("HVertexSingleTrack"));
  fHVertexNoShower                  = static_cast<TH1F*>(fHVertexNoShowerNumerator->Clone("HVertexNoShower"));
  fHVertexSingleTrackNoShower       = static_cast<TH1F*>(fHVertexSingleTrackNoShowerNumerator->Clone("HVertexSingleTrackNoShower"));
  fHEfficiency2D            = static_cast<TH2F*>(fHEfficiency2DNumerator->Clone("HEfficiency2D"));
  fHEfficiency2DSingleTrack = static_cast<TH2F*>(fHEfficiency2DSingleTrackNumerator->Clone("HEfficiency2DSingleTrack"));
  fHEfficiency2DNoShower            = static_cast<TH2F*>(fHEfficiency2DNoShowerNumerator->Clone("HEfficiency2DNoShower"));
  fHEfficiency2DSingleTrackNoShower = static_cast<TH2F*>(fHEfficiency2DSingleTrackNoShowerNumerator->Clone("HEfficiency2DSingleTrackNoShower"));

  fHMomentum->SetTitle("Efficiency vs momentum histogram");
  fHMomentumSingleTrack->SetTitle("Single track efficiency vs momentum histogram");
  fHMomentumNoShower->SetTitle("Efficiency vs momentum histogram (shower flag = 0)");
  fHMomentumSingleTrackNoShower->SetTitle("Single track efficiency vs momentum histogram (shower flag = 0)");
  fHVertex->SetTitle("Efficiency vs vertex histogram");
  fHVertexSingleTrack->SetTitle("Single track efficiency vs vertex histogram");
  fHVertexNoShower->SetTitle("Efficiency vs vertex histogram (shower flag = 0)");
  fHVertexSingleTrackNoShower->SetTitle("Single track efficiency vs vertex histogram (shower flag = 0)");
  fHEfficiency2D->SetTitle("Efficiency 2D");
  fHEfficiency2DSingleTrack->SetTitle("Single track efficiency 2D");
  fHEfficiency2DNoShower->SetTitle("Efficiency 2D (shower flag = 0)");
  fHEfficiency2DSingleTrackNoShower->SetTitle("Single track efficiency 2D (shower flag = 0)");

  fHMomentum->GetXaxis()->SetTitle("P [GeV/c]");
  fHMomentumSingleTrack->GetXaxis()->SetTitle("P [GeV/c]");
  fHMomentumNoShower->GetXaxis()->SetTitle("P [GeV/c]");
  fHMomentumSingleTrackNoShower->GetXaxis()->SetTitle("P [GeV/c]");
  fHVertex->GetXaxis()->SetTitle("z_{vertex} [mm]");
  fHVertexSingleTrack->GetXaxis()->SetTitle("z_{vertex} [mm]");
  fHVertexNoShower->GetXaxis()->SetTitle("z_{vertex} [mm]");
  fHVertexSingleTrackNoShower->GetXaxis()->SetTitle("z_{vertex} [mm]");
  fHEfficiency2D->GetXaxis()->SetTitle("x [mm]");
  fHEfficiency2D->GetYaxis()->SetTitle("y [mm]");
  fHEfficiency2DSingleTrack->GetXaxis()->SetTitle("x [mm]");
  fHEfficiency2DSingleTrack->GetYaxis()->SetTitle("y [mm]");
  fHEfficiency2DNoShower->GetXaxis()->SetTitle("x [mm]");
  fHEfficiency2DNoShower->GetYaxis()->SetTitle("y [mm]");
  fHEfficiency2DSingleTrackNoShower->GetXaxis()->SetTitle("x [mm]");
  fHEfficiency2DSingleTrackNoShower->GetYaxis()->SetTitle("y [mm]");

  fHMomentumNumerator->Sumw2();
  fHMomentumDenominator->Sumw2();
  fHMomentumSingleTrackNumerator->Sumw2();
  fHMomentumSingleTrackDenominator->Sumw2();
  fHVertexNumerator->Sumw2();
  fHVertexDenominator->Sumw2();
  fHVertexSingleTrackNumerator->Sumw2();
  fHVertexSingleTrackDenominator->Sumw2();
  fHEfficiency2DNumerator->Sumw2();
  fHEfficiency2DDenominator->Sumw2();
  fHEfficiency2DSingleTrackNumerator->Sumw2();
  fHEfficiency2DSingleTrackDenominator->Sumw2();

  fHMomentumNoShowerNumerator->Sumw2();
  fHMomentumNoShowerDenominator->Sumw2();
  fHMomentumSingleTrackNoShowerNumerator->Sumw2();
  fHMomentumSingleTrackNoShowerDenominator->Sumw2();
  fHVertexNoShowerNumerator->Sumw2();
  fHVertexNoShowerDenominator->Sumw2();
  fHVertexSingleTrackNoShowerNumerator->Sumw2();
  fHVertexSingleTrackNoShowerDenominator->Sumw2();
  fHEfficiency2DNoShowerNumerator->Sumw2();
  fHEfficiency2DNoShowerDenominator->Sumw2();
  fHEfficiency2DSingleTrackNoShowerNumerator->Sumw2();
  fHEfficiency2DSingleTrackNoShowerDenominator->Sumw2();

  fHMomentum->Divide(fHMomentumNumerator, fHMomentumDenominator, 1., 1., "B");
  fHMomentumSingleTrack->Divide(fHMomentumSingleTrackNumerator, fHMomentumSingleTrackDenominator, 1., 1., "B");
  fHVertex->Divide(fHVertexNumerator, fHVertexDenominator, 1., 1., "B");
  fHVertexSingleTrack->Divide(fHVertexSingleTrackNumerator, fHVertexSingleTrackDenominator, 1., 1., "B");
  fHEfficiency2D->Divide(fHEfficiency2DNumerator, fHEfficiency2DDenominator, 1., 1., "B");
  fHEfficiency2DSingleTrack->Divide(fHEfficiency2DSingleTrackNumerator, fHEfficiency2DSingleTrackDenominator, 1., 1., "B");
  
  fHMomentumNoShower->Divide(fHMomentumNoShowerNumerator, fHMomentumNoShowerDenominator, 1., 1., "B");
  fHMomentumSingleTrackNoShower->Divide(fHMomentumSingleTrackNoShowerNumerator, fHMomentumSingleTrackNoShowerDenominator, 1., 1., "B");
  fHVertexNoShower->Divide(fHVertexNoShowerNumerator, fHVertexNoShowerDenominator, 1., 1., "B");
  fHVertexSingleTrackNoShower->Divide(fHVertexSingleTrackNoShowerNumerator, fHVertexSingleTrackNoShowerDenominator, 1.,1., "B");
  fHEfficiency2DNoShower->Divide(fHEfficiency2DNoShowerNumerator, fHEfficiency2DNoShowerDenominator, 1., 1., "B");
  fHEfficiency2DSingleTrackNoShower->Divide(fHEfficiency2DSingleTrackNoShowerNumerator,fHEfficiency2DSingleTrackNoShowerDenominator, 1., 1., "B");

  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
  fCanvas = new TCanvas("Canvas", "Canvas", 800, 400);

  TLine a;
  a.SetLineColor(kRed);

  if(fHIllumination){
    fHIllumination->Draw("colz");
  }
  fCanvas->Print(Form(fOutputPDFFileName + "("), "pdf");

  if(fHEfficiencyVsBurstID && fHEfficiencyVsBurstID->GetN()){
    fHEfficiencyVsBurstID->SetTitle(Form("efficiency Vs BurstID for run %d",GetRunID()));
    fHEfficiencyVsBurstID->Draw("AP");
    fHEfficiencyVsBurstID->SetLineColor(kRed);
    fHEfficiencyVsBurstID->SetMarkerColor(kRed);
    fHEfficiencyVsBurstID->SetMarkerStyle(20);
    fHEfficiencyVsBurstID->SetMarkerSize(0.3);
    fHEfficiencyVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHEfficiencyVsBurstID->GetYaxis()->SetTitle("Efficiency");
    fHEfficiencyVsBurstID->GetYaxis()->SetRangeUser(0.,1.1);
    fCanvas->Print(fOutputPDFFileName, "pdf");
  }
  if(fHEfficiencySingleTrackVsBurstID && fHEfficiencySingleTrackVsBurstID->GetN()){
    fHEfficiencySingleTrackVsBurstID->SetTitle(Form("efficiency Vs BurstID for run %d (single track)",GetRunID()));
    fHEfficiencySingleTrackVsBurstID->Draw("AP");
    fHEfficiencySingleTrackVsBurstID->SetLineColor(kRed);
    fHEfficiencySingleTrackVsBurstID->SetMarkerColor(kRed);
    fHEfficiencySingleTrackVsBurstID->SetMarkerStyle(20);
    fHEfficiencySingleTrackVsBurstID->SetMarkerSize(0.3);
    fHEfficiencySingleTrackVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHEfficiencySingleTrackVsBurstID->GetYaxis()->SetTitle("Efficiency");
    fHEfficiencySingleTrackVsBurstID->GetYaxis()->SetRangeUser(0.,1.1);
    fCanvas->Print(fOutputPDFFileName, "pdf");
  }
  if(fHEfficiencyNoShowerVsBurstID && fHEfficiencyNoShowerVsBurstID->GetN()){
    fHEfficiencyNoShowerVsBurstID->SetTitle(Form("efficiency Vs BurstID for run %d (no shower)",GetRunID()));
    fHEfficiencyNoShowerVsBurstID->Draw("AP");
    fHEfficiencyNoShowerVsBurstID->SetLineColor(kRed);
    fHEfficiencyNoShowerVsBurstID->SetMarkerColor(kRed);
    fHEfficiencyNoShowerVsBurstID->SetMarkerStyle(20);
    fHEfficiencyNoShowerVsBurstID->SetMarkerSize(0.3);
    fHEfficiencyNoShowerVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHEfficiencyNoShowerVsBurstID->GetYaxis()->SetTitle("Efficiency");
    fHEfficiencyNoShowerVsBurstID->GetYaxis()->SetRangeUser(0.,1.1);
    fCanvas->Print(fOutputPDFFileName, "pdf");
  }
  if(fHEfficiencySingleTrackNoShowerVsBurstID && fHEfficiencySingleTrackNoShowerVsBurstID->GetN()){
    fHEfficiencySingleTrackNoShowerVsBurstID->SetTitle(Form("efficiency Vs BurstID for run %d (single track, no shower)",GetRunID()));
    fHEfficiencySingleTrackNoShowerVsBurstID->Draw("AP");
    fHEfficiencySingleTrackNoShowerVsBurstID->SetLineColor(kRed);
    fHEfficiencySingleTrackNoShowerVsBurstID->SetMarkerColor(kRed);
    fHEfficiencySingleTrackNoShowerVsBurstID->SetMarkerStyle(20);
    fHEfficiencySingleTrackNoShowerVsBurstID->SetMarkerSize(0.3);
    fHEfficiencySingleTrackNoShowerVsBurstID->GetXaxis()->SetTitle("BurstID");
    fHEfficiencySingleTrackNoShowerVsBurstID->GetYaxis()->SetTitle("Efficiency");
    fHEfficiencySingleTrackNoShowerVsBurstID->GetYaxis()->SetRangeUser(0.,1.1);
    fCanvas->Print(fOutputPDFFileName, "pdf");
  }

  gStyle->SetOptStat(11);
  gStyle->SetStatW(0.15);
  gStyle->SetStatH(0.12);
  gStyle->SetStatY(0.4);

  Double_t YaxisMinValue = 1.;
  for (Int_t i=1; i<=70; i++) {
    if (fHMomentum->GetBinContent(i) == 0 && fHMomentum->GetBinError(i) == 0) continue;
    if (fHMomentum->GetBinContent(i) - fHMomentum->GetBinError(i) < YaxisMinValue)
      YaxisMinValue = fHMomentum->GetBinContent(i) - fHMomentum->GetBinError(i);
  }
  fHMomentum->SetMinimum(YaxisMinValue);
  fHMomentum->Draw();
  fCanvas->Print(fOutputPDFFileName, "pdf");

  YaxisMinValue = 1.;
  for (Int_t i=1; i<=70; i++) {
    if (fHMomentumSingleTrack->GetBinContent(i) == 0 && fHMomentumSingleTrack->GetBinError(i) == 0) continue;
    if (fHMomentumSingleTrack->GetBinContent(i) - fHMomentumSingleTrack->GetBinError(i) < YaxisMinValue)
      YaxisMinValue = fHMomentumSingleTrack->GetBinContent(i) - fHMomentumSingleTrack->GetBinError(i);
  }
  fHMomentumSingleTrack->SetMinimum(YaxisMinValue);
  fHMomentumSingleTrack->Draw();
  fCanvas->Print(fOutputPDFFileName, "pdf");

  YaxisMinValue = 1.;
  for (Int_t i=1; i<=70; i++) {
    if (fHMomentumNoShower->GetBinContent(i) == 0 && fHMomentumNoShower->GetBinError(i) == 0) continue;
    if (fHMomentumNoShower->GetBinContent(i) - fHMomentumNoShower->GetBinError(i) < YaxisMinValue)
      YaxisMinValue = fHMomentumNoShower->GetBinContent(i) - fHMomentumNoShower->GetBinError(i);
  }
  fHMomentumNoShower->SetMinimum(YaxisMinValue);
  fHMomentumNoShower->Draw();
  fCanvas->Print(fOutputPDFFileName, "pdf");

  YaxisMinValue = 1.;
  for (Int_t i=1; i<=70; i++) {
    if (fHMomentumSingleTrackNoShower->GetBinContent(i) == 0 && fHMomentumSingleTrackNoShower->GetBinError(i) == 0) continue;
    if (fHMomentumSingleTrackNoShower->GetBinContent(i) - fHMomentumSingleTrackNoShower->GetBinError(i) < YaxisMinValue)
      YaxisMinValue = fHMomentumSingleTrackNoShower->GetBinContent(i) - fHMomentumSingleTrackNoShower->GetBinError(i);
  }
  fHMomentumSingleTrackNoShower->SetMinimum(YaxisMinValue);
  fHMomentumSingleTrackNoShower->Draw();
  fCanvas->Print(fOutputPDFFileName, "pdf");

  gStyle->SetStatY(0.95);

  YaxisMinValue = 1.;
  for (Int_t i=1; i<=70; i++) {
    if (fHVertex->GetBinContent(i) == 0 && fHVertex->GetBinError(i) == 0) continue;
    if (fHVertex->GetBinContent(i) - fHVertex->GetBinError(i) < YaxisMinValue)
      YaxisMinValue = fHVertex->GetBinContent(i) - fHVertex->GetBinError(i);
  }
  fHVertex->SetMinimum(YaxisMinValue);
  fHVertex->Draw();
  fCanvas->Print(fOutputPDFFileName, "pdf");

  YaxisMinValue = 1.;
  for (Int_t i=1; i<=70; i++) {
    if (fHVertexSingleTrack->GetBinContent(i) == 0 && fHVertexSingleTrack->GetBinError(i) == 0) continue;
    if (fHVertexSingleTrack->GetBinContent(i) - fHVertexSingleTrack->GetBinError(i) < YaxisMinValue)
      YaxisMinValue = fHVertexSingleTrack->GetBinContent(i) - fHVertexSingleTrack->GetBinError(i);
  }
  fHVertexSingleTrack->SetMinimum(YaxisMinValue);
  fHVertexSingleTrack->Draw();
  fCanvas->Print(fOutputPDFFileName, "pdf");

  YaxisMinValue = 1.;
  for (Int_t i=1; i<=70; i++) {
    if (fHVertexNoShower->GetBinContent(i) == 0 && fHVertexNoShower->GetBinError(i) == 0) continue;
    if (fHVertexNoShower->GetBinContent(i) - fHVertexNoShower->GetBinError(i) < YaxisMinValue)
      YaxisMinValue = fHVertexNoShower->GetBinContent(i) - fHVertexNoShower->GetBinError(i);
  }
  fHVertexNoShower->SetMinimum(YaxisMinValue);
  fHVertexNoShower->Draw();
  fCanvas->Print(fOutputPDFFileName, "pdf");

  YaxisMinValue = 1.;
  for (Int_t i=1; i<=70; i++) {
    if (fHVertexSingleTrackNoShower->GetBinContent(i) == 0 && fHVertexSingleTrackNoShower->GetBinError(i) == 0) continue;
    if (fHVertexSingleTrackNoShower->GetBinContent(i) - fHVertexSingleTrackNoShower->GetBinError(i) < YaxisMinValue)
      YaxisMinValue = fHVertexSingleTrackNoShower->GetBinContent(i) - fHVertexSingleTrackNoShower->GetBinError(i);
  }
  fHVertexSingleTrackNoShower->SetMinimum(YaxisMinValue);
  fHVertexSingleTrackNoShower->Draw();
  fCanvas->Print(fOutputPDFFileName, "pdf");

  gStyle->SetStatX(0.9);
  gStyle->SetPaintTextFormat("4.2f");
  a.SetLineColor(kBlack);

  Double_t Red[3]   = {0.00, 0.00, 1.00};
  Double_t Green[3] = {0.00, 0.00, 1.00};
  Double_t Blue[3]  = {0.20, 1.00,  0.00};
  Double_t Stops[3] = {0.00, 0.60, 1.00};
  Int_t nb = 32;
  TColor::CreateGradientColorTable(3,Stops,Red,Green,Blue,nb);

  fHEfficiency2D->SetMinimum(0.5);
  fHEfficiency2D->SetContour(nb);
  fHEfficiency2D->Draw("colztext");
  a.DrawLine(-1210., 0., 1210., 0.);
  a.DrawLine(0., -1210., 0., 1210.);
  fCanvas->Print(fOutputPDFFileName, "pdf");

  fHEfficiency2DSingleTrack->SetMinimum(0.5);
  fHEfficiency2DSingleTrack->SetContour(nb);
  fHEfficiency2DSingleTrack->Draw("colztext");
  a.DrawLine(-1210., 0., 1210., 0.);
  a.DrawLine(0., -1210., 0., 1210.);
  fCanvas->Print(fOutputPDFFileName, "pdf");

  fHEfficiency2DNoShower->SetMinimum(0.5);
  fHEfficiency2DNoShower->SetContour(nb);
  fHEfficiency2DNoShower->Draw("colztext");
  a.DrawLine(-1210., 0., 1210., 0.);
  a.DrawLine(0., -1210., 0., 1210.);
  fCanvas->Print(fOutputPDFFileName, "pdf");

  fHEfficiency2DSingleTrackNoShower->SetMinimum(0.5);
  fHEfficiency2DSingleTrackNoShower->SetContour(nb);
  fHEfficiency2DSingleTrackNoShower->Draw("colztext");
  a.DrawLine(-1210., 0., 1210., 0.);
  a.DrawLine(0., -1210., 0., 1210.);
  fCanvas->Print(Form(fOutputPDFFileName + ")"), "pdf");

  delete fCanvas;
  gErrorIgnoreLevel = -1; // restore the default

  ofstream BadBurstFile;
  BadBurstFile.open(Form("CHODEfficiency.BadBursts.thr%.3f.dat", fMinEff));
  for(Int_t iPoint=0;iPoint<fHEfficiencySingleTrackNoShowerVsBurstID->GetN();iPoint++){
    double BurstID=0., Efficiency=0., Nexp=0., Nmatch=0.;
    if (fHEfficiencySingleTrackNoShowerVsBurstID) fHEfficiencySingleTrackNoShowerVsBurstID->GetPoint(iPoint,BurstID,Efficiency);
    if (fHNExpectedSingleTrackNoShowerVsBurstID)  fHNExpectedSingleTrackNoShowerVsBurstID->GetPoint(iPoint,BurstID,Nexp);
    if (fHNMatchedSingleTrackNoShowerVsBurstID)   fHNMatchedSingleTrackNoShowerVsBurstID->GetPoint(iPoint,BurstID,Nmatch);
    if (Nexp<0.5)           BadBurstFile << Form("BadBurst %06d %04d LOWSTAT\n", GetRunID(), (Int_t)BurstID);
    if (Nmatch<fMinStat)    BadBurstFile << Form("BadBurst %06d %04d LOWSTAT\n", GetRunID(), (Int_t)BurstID);
    if (Efficiency<fMinEff) BadBurstFile << Form("BadBurst %06d %04d LOWEFF\n",  GetRunID(), (Int_t)BurstID);
  }
  BadBurstFile.close();
}

void CHODEfficiency::StartOfBurstUser() {
  fBurstID = GetBurstID();
  fNMatchedPerBurst = 0.;
  fNExpectedPerBurst = 0.;
  fNMatchedSingleTrackPerBurst = 0.;
  fNExpectedSingleTrackPerBurst = 0.;
  fNMatchedNoShowerPerBurst = 0.;
  fNExpectedNoShowerPerBurst = 0.;
  fNMatchedSingleTrackNoShowerPerBurst = 0.;
  fNExpectedSingleTrackNoShowerPerBurst = 0.;
}

void CHODEfficiency::EndOfBurstUser() {
  if (fReadingData) {
    double Efficiency=0., eEfficiency=0.;
    fHNExpectedVsBurstID->Set(fHNExpectedVsBurstID->GetN()+1);
    fHNExpectedVsBurstID->SetPoint(fHNExpectedVsBurstID->GetN()-1,fBurstID,fNExpectedPerBurst);
    fHNExpectedVsBurstID->SetPointError(fHNExpectedVsBurstID->GetN()-1,0,sqrt(fNExpectedPerBurst));
    if(fNExpectedPerBurst){
      Efficiency  = fNMatchedPerBurst/fNExpectedPerBurst;
      eEfficiency = sqrt(Efficiency*(1.-Efficiency)/fNExpectedPerBurst);
    }
    fHEfficiencyVsBurstID->Set(fHEfficiencyVsBurstID->GetN()+1);
    fHEfficiencyVsBurstID->SetPoint(fHEfficiencyVsBurstID->GetN()-1,fBurstID,Efficiency);
    fHEfficiencyVsBurstID->SetPointError(fHEfficiencyVsBurstID->GetN()-1,0,eEfficiency);

    fHNExpectedSingleTrackVsBurstID->Set(fHNExpectedSingleTrackVsBurstID->GetN()+1);
    fHNExpectedSingleTrackVsBurstID->SetPoint(fHNExpectedSingleTrackVsBurstID->GetN()-1,fBurstID,fNExpectedSingleTrackPerBurst);
    fHNExpectedSingleTrackVsBurstID->SetPointError(fHNExpectedSingleTrackVsBurstID->GetN()-1,0,sqrt(fNExpectedSingleTrackPerBurst));
    if(fNExpectedSingleTrackPerBurst){
      Efficiency  = fNMatchedSingleTrackPerBurst/fNExpectedSingleTrackPerBurst;
      eEfficiency = sqrt(Efficiency*(1.-Efficiency)/fNExpectedSingleTrackPerBurst);
    }
    fHEfficiencySingleTrackVsBurstID->Set(fHEfficiencySingleTrackVsBurstID->GetN()+1);
    fHEfficiencySingleTrackVsBurstID->SetPoint(fHEfficiencySingleTrackVsBurstID->GetN()-1,fBurstID,Efficiency);
    fHEfficiencySingleTrackVsBurstID->SetPointError(fHEfficiencySingleTrackVsBurstID->GetN()-1,0,eEfficiency);

    fHNExpectedNoShowerVsBurstID->Set(fHNExpectedNoShowerVsBurstID->GetN()+1);
    fHNExpectedNoShowerVsBurstID->SetPoint(fHNExpectedNoShowerVsBurstID->GetN()-1,fBurstID,fNExpectedNoShowerPerBurst);
    fHNExpectedNoShowerVsBurstID->SetPointError(fHNExpectedNoShowerVsBurstID->GetN()-1,0,sqrt(fNExpectedNoShowerPerBurst));
    if(fNExpectedNoShowerPerBurst){
      Efficiency  = fNMatchedNoShowerPerBurst/fNExpectedNoShowerPerBurst;
      eEfficiency = sqrt(Efficiency*(1.-Efficiency)/fNExpectedNoShowerPerBurst);
    }
    fHEfficiencyNoShowerVsBurstID->Set(fHEfficiencyNoShowerVsBurstID->GetN()+1);
    fHEfficiencyNoShowerVsBurstID->SetPoint(fHEfficiencyNoShowerVsBurstID->GetN()-1,fBurstID,Efficiency);
    fHEfficiencyNoShowerVsBurstID->SetPointError(fHEfficiencyNoShowerVsBurstID->GetN()-1,0,eEfficiency);

    fHNMatchedSingleTrackNoShowerVsBurstID->Set(fHNMatchedSingleTrackNoShowerVsBurstID->GetN()+1);
    fHNMatchedSingleTrackNoShowerVsBurstID->SetPoint(fHNMatchedSingleTrackNoShowerVsBurstID->GetN()-1,fBurstID,fNMatchedSingleTrackNoShowerPerBurst);
    fHNMatchedSingleTrackNoShowerVsBurstID->SetPointError(fHNMatchedSingleTrackNoShowerVsBurstID->GetN()-1,0,sqrt(fNMatchedSingleTrackNoShowerPerBurst));
    fHNExpectedSingleTrackNoShowerVsBurstID->Set(fHNExpectedSingleTrackNoShowerVsBurstID->GetN()+1);
    fHNExpectedSingleTrackNoShowerVsBurstID->SetPoint(fHNExpectedSingleTrackNoShowerVsBurstID->GetN()-1,fBurstID,fNExpectedSingleTrackNoShowerPerBurst);
    fHNExpectedSingleTrackNoShowerVsBurstID->SetPointError(fHNExpectedSingleTrackNoShowerVsBurstID->GetN()-1,0,sqrt(fNExpectedSingleTrackNoShowerPerBurst));
    if(fNExpectedSingleTrackNoShowerPerBurst){
      Efficiency  = fNMatchedSingleTrackNoShowerPerBurst/fNExpectedSingleTrackNoShowerPerBurst;
      eEfficiency = sqrt(Efficiency*(1.-Efficiency)/fNExpectedSingleTrackNoShowerPerBurst);
    }
    fHEfficiencySingleTrackNoShowerVsBurstID->Set(fHEfficiencySingleTrackNoShowerVsBurstID->GetN()+1);
    fHEfficiencySingleTrackNoShowerVsBurstID->SetPoint(fHEfficiencySingleTrackNoShowerVsBurstID->GetN()-1,fBurstID,Efficiency);
    fHEfficiencySingleTrackNoShowerVsBurstID->SetPointError(fHEfficiencySingleTrackNoShowerVsBurstID->GetN()-1,0,eEfficiency);
  }
}

void CHODEfficiency::ExportPlot() {
  if (!fReadingData) {
    if(fHIllumination) fHIllumination->Write();
    fHEfficiencyVsBurstID->Write();
    fHEfficiencySingleTrackVsBurstID->Write();
    fHEfficiencyNoShowerVsBurstID->Write();
    fHEfficiencySingleTrackNoShowerVsBurstID->Write();
    fHMomentum->Write();
    fHMomentumSingleTrack->Write();
    fHMomentumNoShower->Write();
    fHMomentumSingleTrackNoShower->Write();
    fHVertex->Write();
    fHVertexSingleTrack->Write();
    fHVertexNoShower->Write();
    fHVertexSingleTrackNoShower->Write();
    fHEfficiency2D->Write();
    fHEfficiency2DSingleTrack->Write();
    fHEfficiency2DNoShower->Write();
    fHEfficiency2DSingleTrackNoShower->Write();
  }
}

CHODEfficiency::~CHODEfficiency(){}
