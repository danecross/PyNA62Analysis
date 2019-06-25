//------------------------------------------------------------
//History:
//
//Created by Zuzana Kucerova (zukucero@cern.ch) 2016-02-28
//
//------------------------------------------------------------

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "SpectrometerMCTester.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "functions.hh"

#include "TPaveStats.h"
#include "TList.h"
#include "TText.h"
#include "TAxis.h"
#include "TCanvas.h"
#include "TPolyLine.h"
#include "TSpectrometerHit.hh"
#include <numeric>

/// \class SpectrometerMCTester
/// \Brief
/// A test tool to check Spectrometer simulation in NA62MC
/// \EndBrief
///
/// \Detailed
/// A test tool to check the output of the Spectrometer part of NA62MC.
/// It has two parts - Geantino and Standard.
/// Standard: contains Channel Activity plot (as in Online Monitor),
/// Hit profiles per each plane, activity in each plane and integrated activity of each chamber.
/// Channel Activity canvas is included with predrawn borders of each view.
/// Geantino: [will be added] contains plots of RZ and XZ coordinates of end points for whole spectrometer
/// and for each chamber separately. Chamber Activity plot for each chamber.
/// \n
/// \author Zuzana Kucerova (zukucero@cern.ch)
/// \EndDetailed


namespace ChannelActivityBorders {
  //arrays for ChannelActivity canvas
  double ch1uLx[9] = {1., 61., 61., 62., 62., 61., 61., 1., 1.};
  double ch1uy[9] = {1., 1., 2., 2., 4., 4., 5., 5., 1.};
  double ch1uRx[9] = {120., 68., 68., 69., 69., 68., 68., 120., 120.};

  double ch1vLx[9] = {1., 61., 61., 62., 62., 61., 61., 1., 1.};
  double ch1vy[9] = {6., 6., 7., 7., 9., 9., 10., 10., 6.};
  double ch1vRx[9] = {120., 68., 68., 69., 69., 68., 68., 120., 120.};

  double ch1xLx[9] = {1., 63., 63., 64., 64., 63., 63., 1., 1.};
  double ch1xy[9] = {11., 11., 13., 13., 14., 14., 15., 15., 11.};
  double ch1xRx[9] = {120., 70., 70., 71., 71., 70., 70., 120., 120.};

  double ch1yLx[9] = {2., 58., 58., 59., 59., 58., 58., 2., 2.};
  double ch1yy[9] = {16., 16., 17., 17., 19., 19., 20., 20., 16.};
  double ch1yRx[9] = {121., 65., 65., 66., 66., 65., 65., 121., 121.};

  double ch2uLx[5] = {1., 62., 62., 1., 1.};
  double ch2uy[5] = {22., 22., 26., 26., 22.};
  double ch2uRx[5] = {120., 69., 69., 120., 120.};

  double ch2vLx[5] = {1., 62., 62., 1., 1.};
  double ch2vy[5] = {27., 27., 31., 31., 27.};
  double ch2vRx[5] = {120., 69., 69., 120., 120.};

  double ch2xLx[5] = {1., 64., 64., 1., 1.};
  double ch2xy[5] = {32., 32., 36., 36., 32.};
  double ch2xRx[5] = {120., 71., 71., 120., 120.};

  double ch2yLx[9] = {2., 58., 58., 59., 59., 58., 58., 2., 2.};
  double ch2yy[9] = {37., 37., 38., 38., 40., 40., 41., 41., 37.};
  double ch2yRx[9] = {121., 65., 65., 66., 66., 65., 65., 121., 121.};

  double ch3uLx[9] = {1., 61., 61., 62., 62., 61., 61., 1., 1.};
  double ch3uy[9] = {43., 43., 45., 45., 46., 46., 47., 47., 43.};
  double ch3uRx[9] = {120., 68., 68., 69., 69., 68., 68., 120., 120.};

  double ch3vLx[9] = {1., 61., 61., 62., 62., 61., 61., 1., 1.};
  double ch3vy[9] = {48., 48., 49., 49., 50., 50., 52., 52., 48.};
  double ch3vRx[9] = {120., 68., 68., 69., 69., 68., 68., 120., 120.};

  double ch3xLx[7] = {1., 62., 62., 63., 63., 1., 1.};
  double ch3xy[7] = {53., 53., 54., 54., 57., 57., 53.};
  double ch3xRx[7] = {120., 69., 69., 70., 70., 120., 120.};

  double ch3yLx[9] = {2., 58., 58., 59., 59., 58., 58., 2., 2.};
  double ch3yy[9] = {58., 58., 59., 59., 61., 61., 62., 62., 58.};
  double ch3yRx[9] = {121., 65., 65., 66., 66., 65., 65., 121., 121.};

  double ch4uLx[7] = {1., 59., 59., 60., 60., 1., 1.};
  double ch4uy[7] = {64., 64., 65., 65., 68., 68., 64.};
  double ch4uRx[7] = {120., 66., 66., 67., 67., 120., 120.};

  double ch4vLx[7] = {1., 60., 60., 59., 59., 1., 1.};
  double ch4vy[7] = {69., 69., 72., 72., 73., 73., 69.};
  double ch4vRx[7] = {120., 67., 67., 66., 66., 120., 120.};

  double ch4xLx[9] = {1., 60., 60., 61., 61., 60., 60., 1., 1.};
  double ch4xy[9] = {74., 74., 75., 75., 77., 77., 78., 78., 74.};
  double ch4xRx[9] = {120., 67., 67., 68., 68., 67., 67., 120., 120.};

  double ch4yLx[9] = {2., 58., 58., 59., 59., 58., 58., 2., 2.};
  double ch4yy[9] = {79., 79., 80., 80., 82., 82., 83., 83., 79.};
  double ch4yRx[9] = {121., 65., 65., 66., 66., 65., 65., 121., 121.};
}

using namespace ChannelActivityBorders;
using namespace NA62Analysis;
using namespace NA62Constants;

SpectrometerMCTester::SpectrometerMCTester(Core::BaseAnalysis *ba) : Analyzer(ba, "SpectrometerMCTester") {
  RequestTree("Spectrometer", new TSpectrometerEvent);

  //constants for geantino histos [mm]
  double chz[4] = {183507.8, 194062.8, 204449.8, 218875.7};
  double vdeltaz[4] = {-173.5, -116.5, 116.5, 173.5};
  double hvdeltaz[2] = {-13., 13.};
  double ldeltaz = 5.5;
  double halfStrawD = 4.912;
  for(int i=0; i<32; i++){
    fHVminZ[i] = chz[i/8] + vdeltaz[(i%8)/2] + hvdeltaz[i%2] - ldeltaz - halfStrawD - 0.3;
    fHVmaxZ[i] = chz[i/8] + vdeltaz[(i%8)/2] + hvdeltaz[i%2] + ldeltaz + halfStrawD + 0.3;
 };
}

void SpectrometerMCTester::InitHist() {

  //Geantino histograms

  //Standard histograms
  BookHisto(new TH1F("NHits", "Number of Spectrometer Hits per event", 400, 0, 400), 1, "Standard");
  BookHisto(new TH2F("ChannelActivity", "Channel Activity", 122, 0, 122, 84, 0, 84), 1, "Standard");
  for(int ch=1; ch<5; ch++){
    BookHisto(new TH1F(Form("HitProfilePerPlane_CH%i", ch), Form("Hit Profile per Plane, Chamber %i",ch), 1952, 0, 1952), 1, "Standard");
    BookHisto(new TH2F(Form("ActivityPerChamber_CH%i", ch), Form("Integrated Activity in Chamber %i", ch), 3400, -1700, 1700, 3400, -1700, 1700), 1, "Standard");
    for(int v=1; v<5; v++){
      for(int p=1; p<5; p++){
  	BookHisto(new TH2F(Form("ActivityPerPlane_CH%i_V%i_P%i", ch, v, p), "title", 340, -1700, 1700, 340, -1700, 1700), 1, "Standard");
      }
    }
  }
  BookHisto(new TH1F("PlIDAllHits", "Plane Activity, all", 69, 0 , 68), 1, "Standard");
}

void SpectrometerMCTester::Process(int) {
  if (!GetWithMC()) return;
  if (!GetIsTree()) return;

  //Standard histograms
  TSpectrometerEvent *SpecEvent = GetEvent<TSpectrometerEvent>();
  int NHits = SpecEvent->GetNHits();
  FillHisto("NHits", NHits);
  for(int ihit = 0; ihit<NHits; ihit++){
    TSpectrometerHit *Hit = static_cast<TSpectrometerHit*>(SpecEvent->GetHits()->At(ihit));
    int chID = Hit->GetChamberID();
    int vID = Hit->GetViewID();
    int hvID = Hit->GetHalfViewID();
    int pID = Hit->GetPlaneID();
    int sID = Hit->GetStrawID();
    int specChID = 1 + 21*chID + 5*vID + 2*hvID + pID;
    FillHisto("ChannelActivity", sID, specChID);
    FillHisto("PlIDAllHits", chID*17 + vID*4 + hvID*2 + pID + 1);
    int hbin = vID*(122*4) + hvID*(122*2) + pID*122 + sID;
    FillHisto(Form("HitProfilePerPlane_CH%i", chID+1), hbin);
    FillHisto(Form("ActivityPerChamber_CH%i",chID+1), Hit->GetPosition().X(), Hit->GetPosition().Y());
    FillHisto(Form("ActivityPerPlane_CH%i_V%i_P%i", chID+1, vID+1, 2*hvID + pID + 1), Hit->GetPosition().X(), Hit->GetPosition().Y());
  };

  //Geantino histograms

}

void SpectrometerMCTester::EndOfJobUser() {
  TString view[4] = {"U", "V", "X", "Y"};

  //Channel Activity
  fChActivity = static_cast<TH2F*>(fHisto.GetTH2("ChannelActivity"));
  fChActivity->GetYaxis()->SetTickLength(0.);
  fChActivity->SetStats(0);
  fChActivity->SetOption("colz");
  for(int k=0; k<16; k++){
    fChActivity->GetYaxis()->SetBinLabel(3+k*5+(k>=4)+(k>=8)+(k>=12),Form("%i - "+view[k%4],(k/4)+1));
  };

  TCanvas *c = new TCanvas("ChannelActivity");
  fChActivity->Draw();
  TPolyLine* poly[32] = {new TPolyLine(9, ch1uLx, ch1uy), new TPolyLine(9, ch1uRx, ch1uy),
  			new TPolyLine(9, ch1vLx, ch1vy), new TPolyLine(9, ch1vRx, ch1vy),
  			new TPolyLine(9, ch1xLx, ch1xy), new TPolyLine(9, ch1xRx, ch1xy),
  			new TPolyLine(9, ch1yLx, ch1yy), new TPolyLine(9, ch1yRx, ch1yy),
  			new TPolyLine(5, ch2uLx, ch2uy), new TPolyLine(5, ch2uRx, ch2uy),
  			new TPolyLine(5, ch2vLx, ch2vy), new TPolyLine(5, ch2vRx, ch2vy),
  			new TPolyLine(5, ch2xLx, ch2xy), new TPolyLine(5, ch2xRx, ch2xy),
  			new TPolyLine(9, ch2yLx, ch2yy), new TPolyLine(9, ch2yRx, ch2yy),
  			new TPolyLine(9, ch3uLx, ch3uy), new TPolyLine(9, ch3uRx, ch3uy),
  			new TPolyLine(9, ch3vLx, ch3vy), new TPolyLine(9, ch3vRx, ch3vy),
  			new TPolyLine(7, ch3xLx, ch3xy), new TPolyLine(7, ch3xRx, ch3xy),
  			new TPolyLine(9, ch3yLx, ch3yy), new TPolyLine(9, ch3yRx, ch3yy),
  			new TPolyLine(7, ch4uLx, ch4uy), new TPolyLine(7, ch4uRx, ch4uy),
  			new TPolyLine(7, ch4vLx, ch4vy), new TPolyLine(7, ch4vRx, ch4vy),
  			new TPolyLine(9, ch4xLx, ch4xy), new TPolyLine(9, ch4xRx, ch4xy),
  			new TPolyLine(9, ch4yLx, ch4yy), new TPolyLine(9, ch4yRx, ch4yy)};

  for(int t=0; t<32; t++){
    poly[t]->SetLineWidth(2.2);
    poly[t]->Draw("same");
  };
  c->Update();

  //Hit Profile per Plane
  for(int i=1; i<5; i++){
    fHitProf = static_cast<TH1F*>(fHisto.GetTH1(Form("HitProfilePerPlane_CH%i",i)));
    fHitProf->GetXaxis()->SetTickLength(0.);
    fHitProf->SetStats(0);
    fHitProf->GetXaxis()->SetTitle("Plane");
    fHitProf->GetXaxis()->SetTitleOffset(1.7);
    for(int j=0; j<16; j++){
      fHitProf->GetXaxis()->SetBinLabel(64+j*122 , Form("%i -"+view[j/4]+"- %i",i,(j%4)+1));
    };
  };

  //Activity per Plane
  for(int ch=1; ch<5; ch++){
    for(int v=1; v<5; v++){
      for(int p=1; p<5; p++){
  	fPlActivity = static_cast<TH2F*>(fHisto.GetTH2(Form("ActivityPerPlane_CH%i_V%i_P%i", ch, v, p)));
	fPlActivity->SetTitle(Form("Activity per Plane %i-"+view[v-1]+"-%i, global coordinates", ch, p));
    fPlActivity->GetXaxis()->SetTickLength(0.01);
  	fPlActivity->GetYaxis()->SetTickLength(0.01);
	fPlActivity->GetXaxis()->SetTitle("Global X Coordinate");
	fPlActivity->GetYaxis()->SetTitle("Global Y Coordinate");
	fPlActivity->SetStats(0);
	fPlActivity->SetOption("colz");
      }
    }
  }

  //Activity per Chamber
  for(int i=1; i<5; i++){
    fChamberActivity = static_cast<TH2F*>(fHisto.GetTH2(Form("ActivityPerChamber_CH%i", i)));
    fChamberActivity->SetOption("colz");
    fChamberActivity->SetStats(0);
    fChamberActivity->GetXaxis()->SetTitle("Global X Coordinate");
    fChamberActivity->GetYaxis()->SetTitle("Global Y Coordinate");
  };

  //Number of Hits
  fNHits = static_cast<TH1F*>(fHisto.GetTH1("NHits"));
  fNHits->SetStats(0);

  //Plane Activity, all hits
  fPlID = static_cast<TH1F*>(fHisto.GetTH1("PlIDAllHits"));
  fPlID->GetXaxis()->SetTitle("Plane");
  for(int i=0; i<4; i++){
    fPlID->GetXaxis()->SetBinLabel(8+i*17, Form("Chamber %i", i+1));
  };
  fPlID->SetStats(0);

  //Geantino histograms

  //Save plots
  SaveAllPlots();

  gDirectory->cd("Standard");
  c->Write();
  gDirectory->cd("..");
}
