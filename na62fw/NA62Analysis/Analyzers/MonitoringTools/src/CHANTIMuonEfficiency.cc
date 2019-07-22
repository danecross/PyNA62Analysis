#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "CHANTIMuonEfficiency.hh"
#include <TLegend.h>
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "BaseAnalysis.hh"
#include "containers.hh"
#include "ConfigSettings.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

using Array3D_I = Array3D<Int_t>;

/// \class CHANTIMuonEfficiency
/// \Brief
/// Check the efficiency of the CHANTI detector using the muon halo.
/// The efficiency is evaluated for each X(Y) layer.
/// \EndBrief

/// \Detailed
/// Just CHANTI detector is used for this macro and no particular trigger mask is selected.
/// The efficiency (evaluated burst by burst) is studied for each station and for each view,
/// both for coordinates X (view with vertical bars) and Y (view with horizontal bars)
/// separately. Let us consider, for example, the study of efficiency as a function of X.
/// A "muon" is defined as an event that gives at least one hit in two vertical
/// coupled bars of a plane and in the corresponding bars of four other planes;
/// the spatial region defined by the corresponding coupled bars in the sixth plane is defined efficient
/// if it presents at least one hit in that bars or in one of
/// the two immediately nearby bars. Also the total efficiency of each plane is defined: if a muon crosses
/// 5 planes, the sixth is considered efficient if it presents at least one hit in corresponding bars
/// where you expect the muon or in one of the two immediately nearby bars.
/// It is possible to evaluate also the efficiency in different time slice (just one is used now).
/// The following input parameters are used:
/// - fLowBoundEff[i][j]: efficiency cut for a particular region in the i CHANTI plane (j=0/1 if the region
///                       in which the efficiency is evaluated is internal/at the edge of the plane);
///                       this is useful to spot local problem in CHANTI plane;
/// - fLayerEffCut[i]: efficiency cut for entire i CHANTI plan; this defines good/bad burst
/// - fMaxNBursts: max number of bins in burstID histograms
/// Usage:
/// The analyzer should be run in two modes:
/// 1) Read the reconstructed data and produce intermediate output;
/// 2) read its own output (using the --histo command line option) and produce the
/// histograms of efficiencies (as a root file and a PDF report)
/// and 3 text files:
/// - file RunXXXX_CHANTIMuonEfficiencyBadBursts.dat with a list of the bursts defined bad for CHANTI.
/// - file RunXXXX_CHANTIMuonEfficiencyBadLayers.dat where for each bad burst
///   the layers(0=X,1=Y), planes, time slices and efficiency values are reported.
/// - file RunXXXX_CHANTIMuonEfficiencyBadPositions.dat where, for each bad burst,
///   the layers(0=X,1=Y), planes, bin coordinates, time slices and efficiency values are reported.
/// \author Paolo Massarotti (paolo.massarotti@cern.ch)
/// \author Marco Mirra (marco.mirra@cern.ch)
/// \EndDetailed


CHANTIMuonEfficiency::CHANTIMuonEfficiency(Core::BaseAnalysis *ba) : Analyzer(ba, "CHANTIMuonEfficiency")
{

	RequestTree("CHANTI",new TRecoCHANTIEvent);
	Configuration::ConfigSettings::SetNoSkipBadBurst(true);// do not skip bad bursts

	fNPlanes = 6;
	fNRings = 12;
	fNBins = 9;
	NTimeSlice = 1;

	fGeometry = CHANTIGeometry::GetInstance();
  	fStep           = fGeometry->GetTriangleBase()/2;
  	fGlobalShift    = fGeometry->GetSquareLength()/2 - fStep;
	fOutPDFFileName = fAnalyzerName + ".pdf";
	fX = 0;
  	fY = 0;
  	fErrX = 0;
  	fEfX = 0;
  	fEfY = 0;
  	fEfHighX = 0;
  	fEfHighY = 0;
  	fErrEfX = 0;
  	fErrEfY = 0;
  	fErrEfHighX = 0;
  	fErrEfHighY = 0;
	fEfficiencyEvalX = 0;
	fEfficiencyEvalY = 0;
	fEfficiencyEvalHighX = 0;
	fEfficiencyEvalHighY = 0;
	fCumEffiX = 0;
	fCumEffiY = 0;
	fCumEffiHighX = 0;
	fCumEffiHighY = 0;
	fEffiNumX_VS_BurstID= 0;
	fEffiHighNumX_VS_BurstID = 0;
	fEffiDenX_VS_BurstID = 0;
	fEffiNumY_VS_BurstID = 0;
	fEffiHighNumY_VS_BurstID = 0;
	fEffiDenY_VS_BurstID = 0;
	fEffiX_VS_BurstID = 0;
	fEffiHighX_VS_BurstID = 0;
	fEffiY_VS_BurstID = 0;
	fEffiHighY_VS_BurstID = 0;
	fInPlane_1Up = 0;
  	fInPlane_1Down = 0;
	fInPlane_1SideUp = 0;
	fInPlane_2SideUp = 0;
	fInPlane_1SideDown = 0;
	fInPlane_2SideDown = 0;
	fInPlane_High1Up = 0;
	fInPlane_High1Down = 0;
	fInPlane_High1SideUp = 0;
	fInPlane_High2SideUp = 0;
	fInPlane_High1SideDown = 0;
	fInPlane_High2SideDown = 0;
	fX_NumEffBurst = 0;
	fX_NumHighEffBurst = 0;
	fX_DenEffBurst = 0;
	fY_NumEffBurst = 0;
	fY_NumHighEffBurst = 0;
	fY_DenEffBurst = 0;
	fX_EffBurst = 0;
	fX_HighEffBurst = 0;
	fY_EffBurst = 0;
	fY_HighEffBurst = 0;
	fTotEfficiencyForPlaneX = 0;
	fTotEfficiencyForPlaneHighX = 0;
	fTotEfficiencyForPlaneY = 0;
	fTotEfficiencyForPlaneHighY = 0;
	fLimitsSlice = 0;
	fNEventsInBurstAnalyzed = 0;
	fTotalEffX = 0;
	fTotalEffY = 0;
	fTotalEffHighX = 0;
	fTotalEffHighY = 0;
      	fhCHANTItiming = 0;

	AddParam("fLowBoundEff_St1_Internal", &fLowBoundEff[0][0], 0.88);
	AddParam("fLowBoundEff_St1_Edge", &fLowBoundEff[0][1], 0.88);
	AddParam("fLowBoundEff_St2_Internal", &fLowBoundEff[1][0], 0.88);
	AddParam("fLowBoundEff_St2_Edge", &fLowBoundEff[1][1], 0.88);
	AddParam("fLowBoundEff_St3_Internal", &fLowBoundEff[2][0], 0.88);
	AddParam("fLowBoundEff_St3_Edge", &fLowBoundEff[2][1], 0.88);
	AddParam("fLowBoundEff_St4_Internal", &fLowBoundEff[3][0], 0.88);
	AddParam("fLowBoundEff_St4_Edge", &fLowBoundEff[3][1], 0.88);
	AddParam("fLowBoundEff_St5_Internal", &fLowBoundEff[4][0], 0.88);
	AddParam("fLowBoundEff_St5_Edge", &fLowBoundEff[4][1], 0.88);
	AddParam("fLowBoundEff_St6_Internal", &fLowBoundEff[5][0], 0.79);
	AddParam("fLowBoundEff_St6_Edge", &fLowBoundEff[5][1], 0.69);
	AddParam("fMaxNBursts", &fMaxNBursts, 3000); // max number of bins in histograms

	AddParam("fLayerEffCut_St1", &fLayerEffCut[0], 0.85);
	AddParam("fLayerEffCut_St2", &fLayerEffCut[1], 0.85);
	AddParam("fLayerEffCut_St3", &fLayerEffCut[2], 0.85);
	AddParam("fLayerEffCut_St4", &fLayerEffCut[3], 0.85);
	AddParam("fLayerEffCut_St5", &fLayerEffCut[4], 0.85);
	AddParam("fLayerEffCut_St6", &fLayerEffCut[5], 0.7);
}

void CHANTIMuonEfficiency::InitOutput(){


}

void CHANTIMuonEfficiency::InitHist(){


	fReadingData = GetIsTree();

	fEfficiencyEvalX = new TEfficiency**[fNPlanes];
	fEfficiencyEvalY = new TEfficiency**[fNPlanes];
	fEfficiencyEvalHighX = new TEfficiency**[fNPlanes];
	fEfficiencyEvalHighY = new TEfficiency**[fNPlanes];
	fCumEffiX = new TGraphAsymmErrors**[fNPlanes];
	fCumEffiY = new TGraphAsymmErrors**[fNPlanes];
	fCumEffiHighX = new TGraphAsymmErrors**[fNPlanes];
	fCumEffiHighY = new TGraphAsymmErrors**[fNPlanes];
	fX_EffBurst = new TGraphAsymmErrors**[fNPlanes];
	fX_HighEffBurst = new TGraphAsymmErrors**[fNPlanes];
	fY_EffBurst = new TGraphAsymmErrors**[fNPlanes];
	fY_HighEffBurst = new TGraphAsymmErrors**[fNPlanes];
	//Allocate memory for histo and graph pointers
	for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
		fEfficiencyEvalX[iPlane] = new TEfficiency*[NTimeSlice];
		fEfficiencyEvalY[iPlane] = new TEfficiency*[NTimeSlice];
		fEfficiencyEvalHighX[iPlane] = new TEfficiency*[NTimeSlice];
		fEfficiencyEvalHighY[iPlane] = new TEfficiency*[NTimeSlice];
		fCumEffiX[iPlane] = new TGraphAsymmErrors*[NTimeSlice];
		fCumEffiY[iPlane] = new TGraphAsymmErrors*[NTimeSlice];
		fCumEffiHighX[iPlane] = new TGraphAsymmErrors*[NTimeSlice];
		fCumEffiHighY[iPlane] = new TGraphAsymmErrors*[NTimeSlice];
		fX_EffBurst[iPlane] = new TGraphAsymmErrors*[NTimeSlice];
		fX_HighEffBurst[iPlane] = new TGraphAsymmErrors*[NTimeSlice];
		fY_EffBurst[iPlane] = new TGraphAsymmErrors*[NTimeSlice];
		fY_HighEffBurst[iPlane] = new TGraphAsymmErrors*[NTimeSlice];
		for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
			fEfficiencyEvalX[iPlane][iTslice] = new TEfficiency(Form("XEff_Plane%d_TimeSlice%d",iPlane,iTslice),Form("XEff_Plane%d_TimeSlice%d",iPlane,iTslice),9,-fStep*9.5,fStep*8.5);
			fEfficiencyEvalY[iPlane][iTslice] = new TEfficiency(Form("YEff_Plane%d_TimeSlice%d",iPlane,iTslice),Form("YEff_Plane%d_TimeSlice%d",iPlane,iTslice),9,-fStep*9.5,fStep*8.5);
			fEfficiencyEvalHighX[iPlane][iTslice] = new TEfficiency(Form("HighXEff_Plane%d_TimeSlice%d",iPlane,iTslice),Form("HighXEff_Plane%d_TimeSlice%d",iPlane,iTslice),9,-fStep*9.5,fStep*8.5);
			fEfficiencyEvalHighY[iPlane][iTslice] = new TEfficiency(Form("HighYEff_Plane%d_TimeSlice%d",iPlane,iTslice),Form("HighYEff_Plane%d_TimeSlice%d",iPlane,iTslice),9,-fStep*9.5,fStep*8.5);
			fCumEffiX[iPlane][iTslice] = new TGraphAsymmErrors();
			fCumEffiY[iPlane][iTslice] = new TGraphAsymmErrors();
			fCumEffiHighX[iPlane][iTslice] = new TGraphAsymmErrors();
			fCumEffiHighY[iPlane][iTslice] = new TGraphAsymmErrors();
		}
	}
	fLimitsSlice = new Double_t[NTimeSlice+1];
	fLimitsSlice[0] = 15.;
	fLimitsSlice[NTimeSlice] = 120.;

	if (fReadingData) {
    		std::cout << user_normal() << "Reading reconstructed data" << std::endl;

		BookHisto(new TH2F("CHANTIVsRefTime","CHANTI-RefTime vs burstID",fMaxNBursts,-0.5,fMaxNBursts-0.5,750,-100,200));
		BookHisto(new TH1F("NEventsInBurstAnalyzed","NEventsInBurstAnalyzed",fMaxNBursts,-0.5,fMaxNBursts-0.5));
		fInPlane_1Up = new TH1F**[fNRings];
  		fInPlane_1Down = new TH1F**[fNRings];
		fInPlane_1SideUp = new TH1F**[fNRings];
		fInPlane_2SideUp = new TH1F**[fNRings];
		fInPlane_1SideDown = new TH1F**[fNRings];
		fInPlane_2SideDown = new TH1F**[fNRings];
		fInPlane_High1Up = new TH1F**[fNRings];
		fInPlane_High1Down = new TH1F**[fNRings];
		fInPlane_High1SideUp = new TH1F**[fNRings];
		fInPlane_High2SideUp = new TH1F**[fNRings];
		fInPlane_High1SideDown = new TH1F**[fNRings];
		fInPlane_High2SideDown = new TH1F**[fNRings];

		for (int iRing=0; iRing<fNRings; iRing++){
			fInPlane_1Up[iRing] = new TH1F*[NTimeSlice];
  			fInPlane_1Down[iRing] = new TH1F*[NTimeSlice];
			fInPlane_1SideUp[iRing] = new TH1F*[NTimeSlice];
			fInPlane_2SideUp[iRing] = new TH1F*[NTimeSlice];
			fInPlane_1SideDown[iRing] = new TH1F*[NTimeSlice];
			fInPlane_2SideDown[iRing] = new TH1F*[NTimeSlice];
			fInPlane_High1Up[iRing] = new TH1F*[NTimeSlice];
			fInPlane_High1Down[iRing] = new TH1F*[NTimeSlice];
			fInPlane_High1SideUp[iRing] = new TH1F*[NTimeSlice];
			fInPlane_High2SideUp[iRing] = new TH1F*[NTimeSlice];
			fInPlane_High1SideDown[iRing] = new TH1F*[NTimeSlice];
			fInPlane_High2SideDown[iRing] = new TH1F*[NTimeSlice];
		}

		fTotEfficiencyForPlaneX = new TEfficiency**[fNPlanes];
		fTotEfficiencyForPlaneHighX = new TEfficiency**[fNPlanes];
		fTotEfficiencyForPlaneY = new TEfficiency**[fNPlanes];
		fTotEfficiencyForPlaneHighY = new TEfficiency**[fNPlanes];


		for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			fTotEfficiencyForPlaneX[iPlane] = new TEfficiency*[NTimeSlice];
			fTotEfficiencyForPlaneHighX[iPlane] = new TEfficiency*[NTimeSlice];
			fTotEfficiencyForPlaneY[iPlane] = new TEfficiency*[NTimeSlice];
			fTotEfficiencyForPlaneHighY[iPlane] = new TEfficiency*[NTimeSlice];
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				//Definition of efficiency graphs for plane and time slice (cumulative for all the analyzed bursts)
				fCumEffiX[iPlane][iTslice]->SetNameTitle(Form("CumulativeEfficiencyX%i_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Cumulative Efficiency X %i  SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]));
				BookHisto(fCumEffiX[iPlane][iTslice]);
				fCumEffiY[iPlane][iTslice]->SetNameTitle(Form("CumulativeEfficiencyY%i_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Cumulative Efficiency Y %i SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]));
				BookHisto(fCumEffiY[iPlane][iTslice]);
				fCumEffiHighX[iPlane][iTslice]->SetNameTitle(Form("CumulativeEfficiencyHighX%i_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Cumulative Efficiency high thr X %i SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]));
				BookHisto(fCumEffiHighX[iPlane][iTslice]);
				fCumEffiHighY[iPlane][iTslice]->SetNameTitle(Form("CumulativeEfficiencyHighY%i_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Cumulative Efficiency high thr Y %i SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]));
				BookHisto(fCumEffiHighY[iPlane][iTslice]);
				fCumEffiX[iPlane][iTslice]->Set(fNBins);
				fCumEffiY[iPlane][iTslice]->Set(fNBins);
				fCumEffiHighX[iPlane][iTslice]->Set(fNBins);
				fCumEffiHighY[iPlane][iTslice]->Set(fNBins);

				//Definition of efficiency histrograms for plane and time slice
				BookHisto(new TH2F(Form("EffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Den Efficiency X %d SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5,9,-fStep*9.5,fStep*8.5));
	      			BookHisto(new TH2F(Form("EffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Den Efficiency Y %d SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5,9,-fStep*9.5,fStep*8.5));
      				BookHisto(new TH2F(Form("EffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Num Efficiency X %d SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5,9,-fStep*9.5,fStep*8.5));
      				BookHisto(new TH2F(Form("EffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Num Efficiency Y %d SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5,9,-fStep*9.5,fStep*8.5));
      				BookHisto(new TH2F(Form("EffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Num Efficiency HighX %d SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5,9,-fStep*9.5,fStep*8.5));
      				BookHisto(new TH2F(Form("EffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Num Efficiency HighY %d SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5,9,-fStep*9.5,fStep*8.5));

				BookHisto(new TH2F(Form("EffiX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Efficiency X %d SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5,9,-fStep*9.5,fStep*8.5));
	      			BookHisto(new TH2F(Form("EffiY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Efficiency Y %d SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5,9,-fStep*9.5,fStep*8.5));
      				BookHisto(new TH2F(Form("EffiHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Efficiency HighX %d SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5,9,-fStep*9.5,fStep*8.5));
      				BookHisto(new TH2F(Form("EffiHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Efficiency HighY %d SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5,9,-fStep*9.5,fStep*8.5));
				BookHisto(new TH1F(Form("TotalEffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("TotalEffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5));
				BookHisto(new TH1F(Form("TotalEffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("TotalEffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5));
				BookHisto(new TH1F(Form("TotalEffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("TotalEffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5));
				BookHisto(new TH1F(Form("TotalEffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("TotalEffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5));
				BookHisto(new TH1F(Form("TotalEffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("TotalEffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5));
				BookHisto(new TH1F(Form("TotalEffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("TotalEffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5));

				//Definition of efficiency histrograms for plane and time slice (cumulative for all bursts analyzed)
				BookHisto(new TH1F(Form("CumulativeEffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Cumulative Den Efficiency X %d SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5));
	      			BookHisto(new TH1F(Form("CumulativeEffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Cumulative Den Efficiency Y %d SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5));
      				BookHisto(new TH1F(Form("CumulativeEffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Cumulative Num Efficiency X %d SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5));
      				BookHisto(new TH1F(Form("CumulativeEffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Cumulative Num Efficiency Y %d SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5));
      				BookHisto(new TH1F(Form("CumulativeEffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Cumulative Num Efficiency HighX %d SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5));
      				BookHisto(new TH1F(Form("CumulativeEffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Cumulative Num Efficiency HighY %d SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5));

				fTotEfficiencyForPlaneX[iPlane][iTslice] = new TEfficiency(Form("TotEfficiencyForPlaneX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("TotEfficiencyForPlaneX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5);
				fTotEfficiencyForPlaneHighX[iPlane][iTslice] = new TEfficiency(Form("TotEfficiencyForPlaneHighX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("TotEfficiencyForPlaneHighX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5);
				fTotEfficiencyForPlaneY[iPlane][iTslice] = new TEfficiency(Form("TotEfficiencyForPlaneY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("TotEfficiencyForPlaneY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5);
				fTotEfficiencyForPlaneHighY[iPlane][iTslice] = new TEfficiency(Form("TotEfficiencyForPlaneHighY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("TotEfficiencyForPlaneHighY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),fMaxNBursts,-0.5,fMaxNBursts-0.5);
			}
		}


		//Definition of histrograms for ring and time slice used in each event
		for(int iRing = 0; iRing < fNRings; iRing++){
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				if(iRing%2 == 0){
					fInPlane_1Up[iRing][iTslice] = new TH1F(Form("XInPlane%d_1Up_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("X In Plane%d per event Up 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
					fInPlane_1Down[iRing][iTslice] = new TH1F(Form("XInPlane%d_1Down_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("X In Plane%d per event Down 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
					fInPlane_1SideUp[iRing][iTslice] = new TH1F(Form("XInPlane%d_1SideUp_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("X In Plane%d per event Side Up 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5);
					fInPlane_2SideUp[iRing][iTslice] = new TH1F(Form("XInPlane%d_2SideUp_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("X In Plane%d per event Side Up 2 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*8.5,fStep*9.5);
					fInPlane_1SideDown[iRing][iTslice] = new TH1F(Form("XInPlane%d_1SideDown_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("X In Plane%d per event Side Down 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5);
        				fInPlane_2SideDown[iRing][iTslice] = new TH1F(Form("XInPlane%d_2SideDown_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("X In Plane%d per event 2 Side Down SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*8.5,fStep*9.5);
        				fInPlane_High1Up[iRing][iTslice] = new TH1F(Form("XInPlane%d_High1Up_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("HighX In Plane%d per event Up 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
					fInPlane_High1Down[iRing][iTslice] = new TH1F(Form("XInPlane%d_High1Down_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("HighX In Plane%d per event Down 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
					fInPlane_High1SideUp[iRing][iTslice] = new TH1F(Form("XInPlane%d_High1SideUp_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("HighX In Plane%d per event Side Up 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5);
					fInPlane_High2SideUp[iRing][iTslice] = new TH1F(Form("XInPlane%d_High2SideUp_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("HighX In Plane%d per event Side Up 2 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*8.5,fStep*9.5);
        				fInPlane_High1SideDown[iRing][iTslice] = new TH1F(Form("XInPlane%d_High1SideDown_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("HighX In Plane%d per event Side Down 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5);
					fInPlane_High2SideDown[iRing][iTslice] = new TH1F(Form("XInPlane%d_High2SideDown_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("HighX In Plane%d per event Side Down 2 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*8.5,fStep*9.5);
				} else {
					fInPlane_1Up[iRing][iTslice] = new TH1F(Form("YInPlane%d_1Up_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Y In Plane%d per event Up 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
					fInPlane_1Down[iRing][iTslice] = new TH1F(Form("YInPlane%d_1Down_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Y In Plane%d per event Down 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
					fInPlane_1SideUp[iRing][iTslice] = new TH1F(Form("YInPlane%d_1SideUp_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Y In Plane%d per event Side Up 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5);
					fInPlane_2SideUp[iRing][iTslice] = new TH1F(Form("YInPlane%d_2SideUp_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Y In Plane%d per event Side Up 2 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*8.5,fStep*9.5);
					fInPlane_1SideDown[iRing][iTslice] = new TH1F(Form("YInPlane%d_1SideDown_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Y In Plane%d per event Side Down 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5);
					fInPlane_2SideDown[iRing][iTslice] = new TH1F(Form("YInPlane%d_2SideDown_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Y In Plane%d per event 2 Side Down SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*8.5,fStep*9.5);
					fInPlane_High1Up[iRing][iTslice] = new TH1F(Form("YInPlane%d_High1Up_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("HighY In Plane%d per event Up 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
					fInPlane_High1Down[iRing][iTslice] = new TH1F(Form("YInPlane%d_High1Down_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("HighY In Plane%d per event Down 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),17,-fGlobalShift-fStep*.5,fStep*16.5-fGlobalShift);
					fInPlane_High1SideUp[iRing][iTslice] = new TH1F(Form("YInPlane%d_High1SideUp_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("HighY In Plane%d per event Side Up 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5);
					fInPlane_High2SideUp[iRing][iTslice] = new TH1F(Form("YInPlane%d_High2SideUp_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("HighY In Plane%d per event Side Up 2 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*8.5,fStep*9.5);
					fInPlane_High1SideDown[iRing][iTslice] = new TH1F(Form("YInPlane%d_High1SideDown_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("HighY In Plane%d per event Side Down 1 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5);
					fInPlane_High2SideDown[iRing][iTslice] = new TH1F(Form("YInPlane%d_High2SideDown_SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("HighY In Plane%d per event Side Down 2 SliceStart%3.0f_SliceEnd%3.0f",iRing/2+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*8.5,fStep*9.5);
				}
			}
		}
	} else {

    	std::cout << user_normal() << "Reading my own output" << std::endl;
      	fNEventsInBurstAnalyzed = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,"NEventsInBurstAnalyzed", true));
      	fhCHANTItiming = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,"CHANTIVsRefTime", true));
		fX = new Double_t** [fNPlanes];
  		fY = new Double_t** [fNPlanes];
  		fErrX = new Double_t** [fNPlanes];
  		fEfX = new Double_t** [fNPlanes];
  		fEfY = new Double_t** [fNPlanes];
  		fEfHighX = new Double_t** [fNPlanes];
  		fEfHighY = new Double_t** [fNPlanes];
  		fErrEfX = new Double_t** [fNPlanes];
  		fErrEfY = new Double_t** [fNPlanes];
  		fErrEfHighX = new Double_t** [fNPlanes];
  		fErrEfHighY = new Double_t** [fNPlanes];

		fTotalEffX = new Double_t* [fNPlanes];
		fTotalEffY = new Double_t* [fNPlanes];
		fTotalEffHighX = new Double_t* [fNPlanes];
		fTotalEffHighY = new Double_t* [fNPlanes];

		fEffiNumX_VS_BurstID = new TH2F**[fNPlanes];
		fEffiHighNumX_VS_BurstID = new TH2F**[fNPlanes];
		fEffiDenX_VS_BurstID = new TH2F**[fNPlanes];
		fEffiNumY_VS_BurstID = new TH2F**[fNPlanes];
		fEffiHighNumY_VS_BurstID = new TH2F**[fNPlanes];
		fEffiDenY_VS_BurstID = new TH2F**[fNPlanes];
		fEffiX_VS_BurstID = new TH2F**[fNPlanes];
		fEffiHighX_VS_BurstID = new TH2F**[fNPlanes];
		fEffiY_VS_BurstID = new TH2F**[fNPlanes];
		fEffiHighY_VS_BurstID = new TH2F**[fNPlanes];

		fX_NumEffBurst = new TH1F**[fNPlanes];
		fX_NumHighEffBurst = new TH1F**[fNPlanes];
		fX_DenEffBurst = new TH1F**[fNPlanes];
		fY_NumEffBurst = new TH1F**[fNPlanes];
		fY_NumHighEffBurst = new TH1F**[fNPlanes];
		fY_DenEffBurst = new TH1F**[fNPlanes];

		for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			fEffiNumX_VS_BurstID[iPlane] = new TH2F*[NTimeSlice];
			fEffiHighNumX_VS_BurstID[iPlane] = new TH2F*[NTimeSlice];
			fEffiDenX_VS_BurstID[iPlane] = new TH2F*[NTimeSlice];
			fEffiNumY_VS_BurstID[iPlane] = new TH2F*[NTimeSlice];
			fEffiHighNumY_VS_BurstID[iPlane] = new TH2F*[NTimeSlice];
			fEffiDenY_VS_BurstID[iPlane] = new TH2F*[NTimeSlice];
			fEffiX_VS_BurstID[iPlane] = new TH2F*[NTimeSlice];
			fEffiHighX_VS_BurstID[iPlane] = new TH2F*[NTimeSlice];
			fEffiY_VS_BurstID[iPlane] = new TH2F*[NTimeSlice];
			fEffiHighY_VS_BurstID[iPlane] = new TH2F*[NTimeSlice];
			fX[iPlane] = new Double_t* [fNBins];
  			fY[iPlane] = new Double_t* [fNBins];
  			fErrX[iPlane] = new Double_t* [fNBins];
  			fEfX[iPlane] = new Double_t* [fNBins];
  			fEfY[iPlane] = new Double_t* [fNBins];
  			fEfHighX[iPlane] = new Double_t* [fNBins];
  			fEfHighY[iPlane] = new Double_t* [fNBins];
  			fErrEfX[iPlane] = new Double_t* [fNBins];
  			fErrEfY[iPlane] = new Double_t* [fNBins];
  			fErrEfHighX[iPlane] = new Double_t* [fNBins];
  			fErrEfHighY[iPlane] = new Double_t* [fNBins];
			fX_NumEffBurst[iPlane] = new TH1F*[NTimeSlice];
			fX_NumHighEffBurst[iPlane] = new TH1F*[NTimeSlice];
			fX_DenEffBurst[iPlane] = new TH1F*[NTimeSlice];
			fY_NumEffBurst[iPlane] = new TH1F*[NTimeSlice];
			fY_NumHighEffBurst[iPlane] = new TH1F*[NTimeSlice];
			fY_DenEffBurst[iPlane] = new TH1F*[NTimeSlice];
			fTotalEffX[iPlane] = new Double_t[NTimeSlice];
			fTotalEffY[iPlane] = new Double_t[NTimeSlice];
			fTotalEffHighX[iPlane] = new Double_t[NTimeSlice];
			fTotalEffHighY[iPlane] = new Double_t[NTimeSlice];
			for (int ibin = 0; ibin<fNBins; ibin++){
				fX[iPlane][ibin] = new Double_t[NTimeSlice];
  				fY[iPlane][ibin] = new Double_t[NTimeSlice];
  				fErrX[iPlane][ibin] = new Double_t[NTimeSlice];
  				fEfX[iPlane][ibin] = new Double_t[NTimeSlice];
  				fEfY[iPlane][ibin] = new Double_t[NTimeSlice];
  				fEfHighX[iPlane][ibin] = new Double_t[NTimeSlice];
  				fEfHighY[iPlane][ibin] = new Double_t[NTimeSlice];
  				fErrEfX[iPlane][ibin] = new Double_t[NTimeSlice];
  				fErrEfY[iPlane][ibin] = new Double_t[NTimeSlice];
  				fErrEfHighX[iPlane][ibin] = new Double_t[NTimeSlice];
  				fErrEfHighY[iPlane][ibin] = new Double_t[NTimeSlice];
			}

			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){

				fTotalEffX[iPlane][iTslice] = -1.;
				fTotalEffY[iPlane][iTslice] = -1.;
				fTotalEffHighX[iPlane][iTslice] = -1.;
				fTotalEffHighY[iPlane][iTslice] = -1.;
				fEffiNumX_VS_BurstID[iPlane][iTslice] = new TH2F();
				fEffiHighNumX_VS_BurstID[iPlane][iTslice] = new TH2F();
				fEffiDenX_VS_BurstID[iPlane][iTslice] = new TH2F();
				fEffiNumY_VS_BurstID[iPlane][iTslice] = new TH2F();
				fEffiHighNumY_VS_BurstID[iPlane][iTslice] = new TH2F();
				fEffiDenY_VS_BurstID[iPlane][iTslice] = new TH2F();
				fX_NumEffBurst[iPlane][iTslice] = new TH1F();
				fX_NumHighEffBurst[iPlane][iTslice] = new TH1F();
				fX_DenEffBurst[iPlane][iTslice] = new TH1F();
				fY_NumEffBurst[iPlane][iTslice] = new TH1F();
				fY_NumHighEffBurst[iPlane][iTslice] = new TH1F();
				fY_DenEffBurst[iPlane][iTslice] = new TH1F();
				fEffiX_VS_BurstID[iPlane][iTslice] = new TH2F();
				fEffiHighX_VS_BurstID[iPlane][iTslice] = new TH2F();
				fEffiY_VS_BurstID[iPlane][iTslice] = new TH2F();
				fEffiHighY_VS_BurstID[iPlane][iTslice] = new TH2F();
				fCumEffiX[iPlane][iTslice] = (TGraphAsymmErrors*)RequestHistogram(fAnalyzerName, Form("CumulativeEfficiencyX%i_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true);
				fCumEffiY[iPlane][iTslice] = (TGraphAsymmErrors*)RequestHistogram(fAnalyzerName, Form("CumulativeEfficiencyY%i_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true);
				fCumEffiHighX[iPlane][iTslice] = (TGraphAsymmErrors*)RequestHistogram(fAnalyzerName, Form("CumulativeEfficiencyHighX%i_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true);
				fCumEffiHighY[iPlane][iTslice] = (TGraphAsymmErrors*)RequestHistogram(fAnalyzerName, Form("CumulativeEfficiencyHighY%i_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true);
				fEffiNumX_VS_BurstID[iPlane][iTslice] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("EffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));
				fEffiHighNumX_VS_BurstID[iPlane][iTslice] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("EffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));
				fEffiDenX_VS_BurstID[iPlane][iTslice] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("EffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));
				fEffiNumY_VS_BurstID[iPlane][iTslice] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("EffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));
				fEffiHighNumY_VS_BurstID[iPlane][iTslice] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("EffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));
				fEffiDenY_VS_BurstID[iPlane][iTslice] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("EffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));
				fEffiX_VS_BurstID[iPlane][iTslice] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("EffiX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));
				fEffiHighX_VS_BurstID[iPlane][iTslice] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("EffiHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));
				fEffiY_VS_BurstID[iPlane][iTslice] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("EffiY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));
				fEffiHighY_VS_BurstID[iPlane][iTslice] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName,Form("EffiHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));

				fX_EffBurst[iPlane][iTslice] = new TGraphAsymmErrors();
				fX_HighEffBurst[iPlane][iTslice] = new TGraphAsymmErrors();
				fY_EffBurst[iPlane][iTslice] = new TGraphAsymmErrors();
				fY_HighEffBurst[iPlane][iTslice] = new TGraphAsymmErrors();
				fX_EffBurst[iPlane][iTslice] = (TGraphAsymmErrors*)RequestHistogram(fAnalyzerName,Form("TotalEffiX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true);
				fX_HighEffBurst[iPlane][iTslice] = (TGraphAsymmErrors*)RequestHistogram(fAnalyzerName,Form("TotalHighEffiX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true);
				fX_NumEffBurst[iPlane][iTslice] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("TotalEffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));
				fX_NumHighEffBurst[iPlane][iTslice] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("TotalEffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));
				fX_DenEffBurst[iPlane][iTslice]  = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("TotalDenX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));
				fY_EffBurst[iPlane][iTslice] = (TGraphAsymmErrors*)RequestHistogram(fAnalyzerName,Form("TotalEffiY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true);
				fY_HighEffBurst[iPlane][iTslice] = (TGraphAsymmErrors*)RequestHistogram(fAnalyzerName,Form("TotalHighEffiY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true);
				fY_NumEffBurst[iPlane][iTslice]  = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("TotalEffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));
				fY_NumHighEffBurst[iPlane][iTslice]  = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("TotalEffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));
				fY_DenEffBurst[iPlane][iTslice] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName,Form("TotalEffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]), true));

				//Definition of efficiency histrograms for plane and time slice (monitoring for different bursts)
      				BookHisto(new TH2F(Form("MonitorEffiX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Monitor Efficiency X %d SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5,1100,0.,1.1));
      				BookHisto(new TH2F(Form("MonitorEffiY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Monitor Efficiency Y %d SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5,1100,0.,1.1));
      				BookHisto(new TH2F(Form("MonitorEffiHighX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Monitor Efficiency HighX %d SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5,1100,0.,1.1));
      				BookHisto(new TH2F(Form("MonitorEffiHighY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Monitor Efficiency HighY %d SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5,1100,0.,1.1));
				BookHisto(new TH2F(Form("MonitorEffiErrX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Monitor EfficiencyErr X %d SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5,1000,0.,.2));
      				BookHisto(new TH2F(Form("MonitorEffiErrY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Monitor EfficiencyErr Y %d SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5,1000,0.,.2));
      				BookHisto(new TH2F(Form("MonitorEffiHighErrX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Monitor EfficiencyErr HighX %d SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5,1000,0.,.2));
      				BookHisto(new TH2F(Form("MonitorEffiHighErrY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("Monitor EfficiencyErr HighY %d SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),9,-fStep*9.5,fStep*8.5,1000,0.,0.2));
			}
		}
	}
}

void CHANTIMuonEfficiency::DefineMCSimple(){}

void CHANTIMuonEfficiency::StartOfRunUser(){}

void CHANTIMuonEfficiency::StartOfBurstUser(){}

void CHANTIMuonEfficiency::Process(int){

	if (!fReadingData) return;

  	TRecoCHANTIEvent *CHANTIEvent = GetEvent<TRecoCHANTIEvent>();
	fEventHeader = GetEventHeader();
  	Double_t EventTime = fEventHeader->GetFineTime()*TdcCalib;
	Int_t BurstID = GetBurstID();
	fHisto.GetTH1("NEventsInBurstAnalyzed")->Fill(BurstID);
	//Reset and initialization of histrograms and arrays used in each event
  	for (int iRing=0; iRing<fNRings; iRing++){
		for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
  			fInPlane_1Up[iRing][iTslice]->Reset();
  			fInPlane_1Down[iRing][iTslice]->Reset();
			fInPlane_1SideUp[iRing][iTslice]->Reset();
			fInPlane_2SideUp[iRing][iTslice]->Reset();
			fInPlane_1SideDown[iRing][iTslice]->Reset();
			fInPlane_2SideDown[iRing][iTslice]->Reset();
			fInPlane_High1Up[iRing][iTslice]->Reset();
			fInPlane_High1Down[iRing][iTslice]->Reset();
			fInPlane_High1SideUp[iRing][iTslice]->Reset();
			fInPlane_High2SideUp[iRing][iTslice]->Reset();
			fInPlane_High1SideDown[iRing][iTslice]->Reset();
			fInPlane_High2SideDown[iRing][iTslice]->Reset();
		}
	}

  	Int_t nHits = CHANTIEvent->GetNHits();
	if (nHits == 0) return;
  	Int_t NBins = fNBins;
  		Array3D_I EffiNumXUp       = CreateArray3D(fNPlanes, NBins, NTimeSlice, 0);
    	Array3D_I EffiNumYUp       = CreateArray3D(fNPlanes, NBins, NTimeSlice, 0);
    	Array3D_I NormSampleXUp    = CreateArray3D(fNPlanes, NBins, NTimeSlice, 0);
    	Array3D_I NormSampleYUp    = CreateArray3D(fNPlanes, NBins, NTimeSlice, 0);
    	Array3D_I EffiNumXDown     = CreateArray3D(fNPlanes, NBins, NTimeSlice, 0);
    	Array3D_I EffiNumYDown     = CreateArray3D(fNPlanes, NBins, NTimeSlice, 0);
    	Array3D_I NormSampleXDown  = CreateArray3D(fNPlanes, NBins, NTimeSlice, 0);
    	Array3D_I NormSampleYDown  = CreateArray3D(fNPlanes, NBins, NTimeSlice, 0);
    	Array3D_I EffiNumHighXUp   = CreateArray3D(fNPlanes, NBins, NTimeSlice, 0);
    	Array3D_I EffiNumHighYUp   = CreateArray3D(fNPlanes, NBins, NTimeSlice, 0);
    	Array3D_I EffiNumHighXDown = CreateArray3D(fNPlanes, NBins, NTimeSlice, 0);
    	Array3D_I EffiNumHighYDown = CreateArray3D(fNPlanes, NBins, NTimeSlice, 0);
    	Int_t RingID;
    	Double_t X, Y, Time;
    	// Following is useless now. Array are all initialized to 0 by default
    	//for(int iPlane = 0; iPlane < fNPlanes; iPlane++) {
    	//	for(int iBin = 0; iBin < NBins; iBin++) {
		//	for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
        //			EffiNumXUp[iPlane][iBin][iTslice] = 0;
        //			EffiNumYUp[iPlane][iBin][iTslice] = 0;
        //			EffiNumXDown[iPlane][iBin][iTslice] = 0;
        //			EffiNumYDown[iPlane][iBin][iTslice] = 0;
       	//			EffiNumHighXUp[iPlane][iBin][iTslice] = 0;
        //			EffiNumHighYUp[iPlane][iBin][iTslice] = 0;
        //			EffiNumHighXDown[iPlane][iBin][iTslice] = 0;
        //			EffiNumHighYDown[iPlane][iBin][iTslice]  = 0;
        //			NormSampleXUp[iPlane][iBin][iTslice]  = 0;
        //			NormSampleYUp[iPlane][iBin][iTslice]  = 0;
        //			NormSampleXDown[iPlane][iBin][iTslice]  = 0;
        //			NormSampleYDown[iPlane][iBin][iTslice]  = 0;
		//	}
		//}
    //}



	//Filling histograms for the evaluation of efficiency
    TClonesArray & RecoHits = (* (CHANTIEvent->GetHits()));
    for(Int_t iHit = 0; iHit<nHits; iHit++){
    	TRecoCHANTIHit *RecoHit = static_cast<TRecoCHANTIHit*>(RecoHits[iHit]);
      	if (RecoHit->GetQualityFlag()==5 || RecoHit->GetThresholdFlag()==1) continue;
      	X = RecoHit->GetX();
      	Y = RecoHit->GetY();
      	Time = RecoHit->GetTime();
      	Double_t dt = Time-EventTime;
		fHisto.GetTH2("CHANTIVsRefTime")->Fill(GetBurstID(),dt);
		Int_t TSlice = -1;
		Bool_t TSliceFound = false;
		for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				if  (dt < fLimitsSlice[iTslice+1] && dt > fLimitsSlice[iTslice]){
					TSlice = iTslice;
					TSliceFound = true;
					break;
				}
			}
			if (!TSliceFound) continue;
			RingID = RecoHit->GetRingID();
			if(RecoHit->GetRingType() == kX) {
				if(RecoHit->GetSideID() == kPositive)  {
					fInPlane_1Up[RingID][TSlice]->Fill(X);
				}
				else{
					fInPlane_1Down[RingID][TSlice]->Fill(X);
				}
			}
			else{
				if(RecoHit->GetSideID() == kPositive)  {
					fInPlane_1Up[RingID][TSlice]->Fill(Y);
				}
				else{
					fInPlane_1Down[RingID][TSlice]->Fill(Y);
				}
			}
				if (RecoHit->GetThresholdFlag()==2 ){
					if(RecoHit->GetRingType() == kX) {
						if(RecoHit->GetSideID() == kPositive)  {
							fInPlane_High1Up[RingID][TSlice]->Fill(X);
						}
						else{
							fInPlane_High1Down[RingID][TSlice]->Fill(X);
						}
					}
					else{
						if(RecoHit->GetSideID() == kPositive)  {
							fInPlane_High1Up[RingID][TSlice]->Fill(Y);
						}
						else{
							fInPlane_High1Down[RingID][TSlice]->Fill(Y);
						}
					}
				}

	}

	Double_t Position;
	for(int iRing = 0; iRing < fNRings; iRing++) {
		for(int iStep = 0; iStep < 17; iStep++) {
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				Position = iStep*fStep - fGlobalShift;
				if(fInPlane_1Up[iRing][iTslice]->GetBinContent(iStep+1) > 0) {
					fInPlane_1SideUp[iRing][iTslice]->Fill(Position);
					fInPlane_2SideUp[iRing][iTslice]->Fill(Position);
				}
				if(fInPlane_1Down[iRing][iTslice]->GetBinContent(iStep+1) > 0) {
					fInPlane_1SideDown[iRing][iTslice]->Fill(Position);
					fInPlane_2SideDown[iRing][iTslice]->Fill(Position);
				}
				if(fInPlane_High1Up[iRing][iTslice]->GetBinContent(iStep+1) > 0) {
					fInPlane_High1SideUp[iRing][iTslice]->Fill(Position);
					fInPlane_High2SideUp[iRing][iTslice]->Fill(Position);
				}
				if(fInPlane_High1Down[iRing][iTslice]->GetBinContent(iStep+1) > 0) {
					fInPlane_High1SideDown[iRing][iTslice]->Fill(Position);
					fInPlane_High2SideDown[iRing][iTslice]->Fill(Position);
				}

			}
		}
	}


	//Normalization sample evaluation
	Int_t PlaneID;
	Double_t Step = fInPlane_1SideUp[0][0]->GetBinWidth(1);
	Double_t Minimum1 = fInPlane_1SideUp[0][0]->GetBinLowEdge(1);
	for(int iBin = 0; iBin < NBins; iBin++) {
		for(int iRing = 0; iRing < fNRings; iRing++) {
			PlaneID = iRing/2;
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				if((PlaneID != 0) && (iRing%2 == 0) && (fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleXUp[0][iBin][iTslice] += 1;
				if((PlaneID != 0) && (iRing%2 == 1) && (fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleYUp[0][iBin][iTslice] += 1;
				if((PlaneID != 0) && (iRing%2 == 0) && (fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleXDown[0][iBin][iTslice] += 1;
				if((PlaneID != 0) && (iRing%2 == 1) && (fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleYDown[0][iBin][iTslice] += 1;
				if((PlaneID != 1) && (iRing%2 == 0) && (fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleXUp[1][iBin][iTslice] += 1;
				if((PlaneID != 1) && (iRing%2 == 1) && (fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleYUp[1][iBin][iTslice] += 1;
				if((PlaneID != 1) && (iRing%2 == 0) && (fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleXDown[1][iBin][iTslice] += 1;
				if((PlaneID != 1) && (iRing%2 == 1) && (fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleYDown[1][iBin][iTslice] += 1;
				if((PlaneID != 2) && (iRing%2 == 0) && (fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleXUp[2][iBin][iTslice] += 1;
				if((PlaneID != 2) && (iRing%2 == 1) && (fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleYUp[2][iBin][iTslice] += 1;
				if((PlaneID != 2) && (iRing%2 == 0) && (fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleXDown[2][iBin][iTslice] += 1;
				if((PlaneID != 2) && (iRing%2 == 1) && (fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleYDown[2][iBin][iTslice] += 1;
				if((PlaneID != 3) && (iRing%2 == 0) && (fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleXUp[3][iBin][iTslice] += 1;
				if((PlaneID != 3) && (iRing%2 == 1) && (fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleYUp[3][iBin][iTslice] += 1;
				if((PlaneID != 3) && (iRing%2 == 0) && (fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleXDown[3][iBin][iTslice] += 1;
				if((PlaneID != 3) && (iRing%2 == 1) && (fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleYDown[3][iBin][iTslice] += 1;
				if((PlaneID != 4) && (iRing%2 == 0) && (fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleXUp[4][iBin][iTslice] += 1;
				if((PlaneID != 4) && (iRing%2 == 1) && (fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleYUp[4][iBin][iTslice] += 1;
				if((PlaneID != 4) && (iRing%2 == 0) && (fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleXDown[4][iBin][iTslice] += 1;
				if((PlaneID != 4) && (iRing%2 == 1) && (fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleYDown[4][iBin][iTslice] += 1;
				if((PlaneID != 5) && (iRing%2 == 0) && (fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleXUp[5][iBin][iTslice] += 1;
				if((PlaneID != 5) && (iRing%2 == 1) && (fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleYUp[5][iBin][iTslice] += 1;
				if((PlaneID != 5) && (iRing%2 == 0) && (fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleXDown[5][iBin][iTslice] += 1;
				if((PlaneID != 5) && (iRing%2 == 1) && (fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) NormSampleYDown[5][iBin][iTslice] += 1;
				if(iBin == 0){
					if(iRing%2 == 0){
					    // cppcheck-suppress constArgument
						if ((fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0))EffiNumXUp[PlaneID][iBin][iTslice] = 1;
						// cppcheck-suppress constArgument
						if ((fInPlane_High1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumHighXUp[PlaneID][iBin][iTslice] = 1;
					}
					if(iRing%2 == 1) {
					    // cppcheck-suppress constArgument
						if ((fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumYUp[PlaneID][iBin][iTslice] = 1;
						// cppcheck-suppress constArgument
						if ((fInPlane_High1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumHighYUp[PlaneID][iBin][iTslice] = 1;
					}
				}
				else if(iBin == 2){
					if(iRing%2 == 0){
						if ((fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0
						)) EffiNumXUp[PlaneID][iBin][iTslice] = 1;
						if ((fInPlane_High1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0
						)) EffiNumHighXUp[PlaneID][iBin][iTslice] = 1;
					}
					if(iRing%2 == 1){
						if ((fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0
						)) EffiNumYUp[PlaneID][iBin][iTslice] = 1;
						if ((fInPlane_High1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0
						)) EffiNumHighYUp[PlaneID][iBin][iTslice] = 1;
					}
				}
				else if(iBin == 6){
					if(iRing%2 == 0){
						if ((fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_2SideDown[iRing][iTslice]->GetBinContent(iBin) > 0  ) ) EffiNumXUp[PlaneID][iBin][iTslice] = 1;
						if ((fInPlane_High1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_High2SideDown[iRing][iTslice]->GetBinContent(iBin) > 0 )) EffiNumHighXUp[PlaneID][iBin][iTslice] = 1;
					}
					if(iRing%2 == 1) {
						if ((fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_2SideDown[iRing][iTslice]->GetBinContent(iBin) > 0  )) EffiNumYUp[PlaneID][iBin][iTslice] = 1;
						if ((fInPlane_High1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_High2SideDown[iRing][iTslice]->GetBinContent(iBin) > 0)) EffiNumHighYUp[PlaneID][iBin][iTslice] = 1;
					}
				}
				else if(iBin+1 == NBins){
					if(iRing%2 == 0) {
						if ((fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumXUp[PlaneID][iBin][iTslice] = 1;
						if ((fInPlane_High1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumHighXUp[PlaneID][iBin][iTslice] = 1;
					}
					if(iRing%2 == 1) {
						if ((fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumYUp[PlaneID][iBin][iTslice] = 1;
						if ((fInPlane_High1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumHighYUp[PlaneID][iBin][iTslice] = 1;
					}
				}else{
					if(iRing%2 == 0){
						if ((fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumXUp[PlaneID][iBin][iTslice] = 1;
						if ((fInPlane_High1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumHighXUp[PlaneID][iBin][iTslice] = 1;
					}
					if(iRing%2 == 1) {
						if ((fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumYUp[PlaneID][iBin][iTslice] = 1;
						if ((fInPlane_High1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumHighYUp[PlaneID][iBin][iTslice] = 1;
					}
				}
				if(iBin == 3){
					if(iRing%2 == 0){
						if ((fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_2SideDown[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_2SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumXDown[PlaneID][iBin][iTslice] = 1;
						if ((fInPlane_High1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_High2SideDown[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_High2SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumHighXDown[PlaneID][iBin][iTslice] = 1;
					}
					if(iRing%2 == 1) {
						if ((fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_2SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumYDown[PlaneID][iBin][iTslice] = 1;
						if ((fInPlane_High1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_High2SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0 )) EffiNumHighYDown[PlaneID][iBin][iTslice] = 1;
					}
				}
				else if(iBin >3 && iBin <6){
					if(iRing%2 == 0) {
						if ((fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideDown[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_2SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumXDown[PlaneID][iBin][iTslice] = 1;
						if ((fInPlane_High1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideDown[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_High2SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumHighXDown[PlaneID][iBin][iTslice] = 1;
					}
					if(iRing%2 == 1) {
						if ((fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideDown[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_2SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumYDown[PlaneID][iBin][iTslice] = 1;
						if ((fInPlane_High1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideDown[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_High2SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumHighYDown[PlaneID][iBin][iTslice] = 1;
					}
				}
				else if(iBin == 6){
					if(iRing%2 == 0) {
						if ((fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideDown[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumXDown[PlaneID][iBin][iTslice] = 1;
						if ((fInPlane_High1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideDown[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumHighXDown[PlaneID][iBin][iTslice] = 1;
					}
					if(iRing%2 == 1) {
						if ((fInPlane_1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_2SideDown[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)) EffiNumYDown[PlaneID][iBin][iTslice] = 1;
						if (fInPlane_High1SideDown[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High1SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0 ||
						fInPlane_High2SideDown[iRing][iTslice]->GetBinContent(iBin) > 0 ||
						fInPlane_High2SideUp[iRing][iTslice]->GetBinContent(iBin+1) > 0)  EffiNumHighYDown[PlaneID][iBin][iTslice] = 1;
					}
				}
			}
		}
	}



	for(int iBin = 0; iBin < NBins; iBin++) {
		for(int iPlane = 0; iPlane < fNPlanes; iPlane++) {
        		for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				if(NormSampleXUp[iPlane][iBin][iTslice] == 5){
					fHisto.GetTH2(Form("EffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID,(iBin+0.5)*Step + Minimum1);
					if(EffiNumXUp[iPlane][iBin][iTslice] == 1) fHisto.GetTH2(Form("EffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID,(iBin+0.5)*Step + Minimum1);
					if(EffiNumHighXUp[iPlane][iBin][iTslice] == 1) fHisto.GetTH1(Form("EffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID,(iBin+0.5)*Step + Minimum1);
				}
				if(NormSampleYUp[iPlane][iBin][iTslice] == 5) {
					fHisto.GetTH2(Form("EffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID,(iBin+0.5)*Step + Minimum1);
					if(EffiNumYUp[iPlane][iBin][iTslice] == 1) fHisto.GetTH2(Form("EffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID,(iBin+0.5)*Step + Minimum1);
					if(EffiNumHighYUp[iPlane][iBin][iTslice] == 1) fHisto.GetTH2(Form("EffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID,(iBin+0.5)*Step + Minimum1);
				}
				if(NormSampleXDown[iPlane][iBin][iTslice] == 5){
					fHisto.GetTH2(Form("EffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID,(iBin+0.5)*Step + Minimum1);
					if(EffiNumXDown[iPlane][iBin][iTslice] == 1) fHisto.GetTH1(Form("EffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID,(iBin+0.5)*Step + Minimum1);
					if(EffiNumHighXDown[iPlane][iBin][iTslice] == 1) fHisto.GetTH1(Form("EffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID,(iBin+0.5)*Step + Minimum1);
				}
				if(NormSampleYDown[iPlane][iBin][iTslice] == 5) {
					fHisto.GetTH2(Form("EffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID,(iBin+0.5)*Step + Minimum1);
					if(EffiNumYDown[iPlane][iBin][iTslice] == 1)  fHisto.GetTH1(Form("EffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID,(iBin+0.5)*Step + Minimum1);
					if(EffiNumHighYDown[iPlane][iBin][iTslice] == 1) fHisto.GetTH1(Form("EffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID,(iBin+0.5)*Step + Minimum1);
				}
			}
		}
	}

}

void CHANTIMuonEfficiency::PostProcess(){


}

void CHANTIMuonEfficiency::EndOfBurstUser(){


}

void CHANTIMuonEfficiency::EndOfJobUser(){

	gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed

	////////////
  	// DATA mode
	if (fReadingData) {

		for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				fHisto.GetTH1(Form("CumulativeEffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Add(fHisto.GetTH2(Form("EffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->ProjectionY());
				fHisto.GetTH1(Form("CumulativeEffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Add(fHisto.GetTH2(Form("EffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->ProjectionY());
				fHisto.GetTH1(Form("CumulativeEffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Add(fHisto.GetTH2(Form("EffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->ProjectionY());
				fHisto.GetTH1(Form("CumulativeEffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Add(fHisto.GetTH2(Form("EffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->ProjectionY());
				fHisto.GetTH1(Form("CumulativeEffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Add(fHisto.GetTH2(Form("EffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->ProjectionY());
				fHisto.GetTH1(Form("CumulativeEffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Add(fHisto.GetTH2(Form("EffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->ProjectionY());

			}
		}

		Double_t XErrorAxis= fHisto.GetTH1(Form("CumulativeEffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f",1,fLimitsSlice[0],fLimitsSlice[1]))->GetBinWidth(1)/2;
		for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				fEfficiencyEvalX[iPlane][iTslice]->SetPassedHistogram(*fHisto.GetTH1(Form("CumulativeEffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");
				fEfficiencyEvalX[iPlane][iTslice]->SetTotalHistogram(*fHisto.GetTH1(Form("CumulativeEffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");
				for (int i = 0; i<fNBins; i++){
					Double_t TempEff = -0.01;
					Double_t TempErrorUp = 0;
					if (fHisto.GetTH1(Form("CumulativeEffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->GetBinContent(i+1)>0.){
						TempErrorUp = fEfficiencyEvalX[iPlane][iTslice]->GetEfficiencyErrorUp(i+1);
						TempEff = fEfficiencyEvalX[iPlane][iTslice]->GetEfficiency(i+1);
					}
					fCumEffiX[iPlane][iTslice]->SetPoint(i,fHisto.GetTH1(Form("CumulativeEffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->GetBinCenter(i+1),TempEff);
					fCumEffiX[iPlane][iTslice]->SetPointError(i,XErrorAxis,XErrorAxis,fEfficiencyEvalX[iPlane][iTslice]->GetEfficiencyErrorLow(i+1),TempErrorUp);
				}


				fEfficiencyEvalY[iPlane][iTslice]->SetPassedHistogram(*fHisto.GetTH1(Form("CumulativeEffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");
				fEfficiencyEvalY[iPlane][iTslice]->SetTotalHistogram(*fHisto.GetTH1(Form("CumulativeEffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");
				for (int i = 0; i<fNBins; i++){
					Double_t TempEff = -0.01;
					Double_t TempErrorUp = 0;
					if (fHisto.GetTH1(Form("CumulativeEffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->GetBinContent(i+1)>0.){
						TempErrorUp = fEfficiencyEvalY[iPlane][iTslice]->GetEfficiencyErrorUp(i+1);
						TempEff = fEfficiencyEvalY[iPlane][iTslice]->GetEfficiency(i+1);
					}
					fCumEffiY[iPlane][iTslice]->SetPoint(i,fHisto.GetTH1(Form("CumulativeEffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->GetBinCenter(i+1),TempEff);
					fCumEffiY[iPlane][iTslice]->SetPointError(i,XErrorAxis,XErrorAxis,fEfficiencyEvalY[iPlane][iTslice]->GetEfficiencyErrorLow(i+1),TempErrorUp);
				}

				fEfficiencyEvalHighX[iPlane][iTslice]->SetPassedHistogram(*fHisto.GetTH1(Form("CumulativeEffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");
				fEfficiencyEvalHighX[iPlane][iTslice]->SetTotalHistogram(*fHisto.GetTH1(Form("CumulativeEffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");
				for (int i = 0; i<fNBins; i++){
					Double_t TempEff = -0.01;
					Double_t TempErrorUp = 0;
					if (fHisto.GetTH1(Form("CumulativeEffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->GetBinContent(i+1)>0.){
						TempErrorUp = fEfficiencyEvalHighX[iPlane][iTslice]->GetEfficiencyErrorUp(i+1);
						TempEff = fEfficiencyEvalHighX[iPlane][iTslice]->GetEfficiency(i+1);
					}
					fCumEffiHighX[iPlane][iTslice]->SetPoint(i,fHisto.GetTH1(Form("CumulativeEffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->GetBinCenter(i+1),TempEff);
					fCumEffiHighX[iPlane][iTslice]->SetPointError(i,XErrorAxis,XErrorAxis,fEfficiencyEvalHighX[iPlane][iTslice]->GetEfficiencyErrorLow(i+1),TempErrorUp);
				}

				fEfficiencyEvalHighY[iPlane][iTslice]->SetPassedHistogram(*fHisto.GetTH1(Form("CumulativeEffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");
				fEfficiencyEvalHighY[iPlane][iTslice]->SetTotalHistogram(*fHisto.GetTH1(Form("CumulativeEffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");

				for (int i = 0; i<fNBins; i++){
					Double_t TempEff = -0.01;
					Double_t TempErrorUp = 0;
					if (fHisto.GetTH1(Form("CumulativeEffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->GetBinContent(i+1)>0.){
						TempErrorUp = fEfficiencyEvalHighY[iPlane][iTslice]->GetEfficiencyErrorUp(i+1);
						TempEff = fEfficiencyEvalHighY[iPlane][iTslice]->GetEfficiency(i+1);
					}
					fCumEffiHighY[iPlane][iTslice]->SetPoint(i,fHisto.GetTH1(Form("CumulativeEffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->GetBinCenter(i+1),TempEff);
					fCumEffiHighY[iPlane][iTslice]->SetPointError(i,XErrorAxis,XErrorAxis,fEfficiencyEvalHighY[iPlane][iTslice]->GetEfficiencyErrorLow(i+1),TempErrorUp);
				}

				fHisto.GetTH2(Form("EffiX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Divide(fHisto.GetTH2(Form("EffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),fHisto.GetTH2(Form("EffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),1.,1., "B");
				fHisto.GetTH2(Form("EffiX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->SetMaximum(1.0);
				fHisto.GetTH2(Form("EffiX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->SetMinimum(0.7);

				fHisto.GetTH2(Form("EffiY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Divide(fHisto.GetTH2(Form("EffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),fHisto.GetTH2(Form("EffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),1.,1., "B");
				fHisto.GetTH2(Form("EffiY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->SetMaximum(1.0);
				fHisto.GetTH2(Form("EffiY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->SetMinimum(0.7);

				fHisto.GetTH2(Form("EffiHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Divide(fHisto.GetTH2(Form("EffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),fHisto.GetTH2(Form("EffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),1.,1., "B");
				fHisto.GetTH2(Form("EffiHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->SetMaximum(1.0);
				fHisto.GetTH2(Form("EffiHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->SetMinimum(0.6);



				fHisto.GetTH2(Form("EffiHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Divide(fHisto.GetTH2(Form("EffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),fHisto.GetTH2(Form("EffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),1.,1., "B");
				fHisto.GetTH2(Form("EffiHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->SetMaximum(1.0);
				fHisto.GetTH2(Form("EffiHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->SetMinimum(0.6);


			}
		}

		for (Int_t iBurst=1; iBurst<=fMaxNBursts; iBurst++) {
			Int_t BurstID = -1;
			BurstID = fHisto.GetTH1("NEventsInBurstAnalyzed")->GetBinCenter(iBurst);
			if ( fHisto.GetTH1("NEventsInBurstAnalyzed")->GetBinContent(iBurst)==0 ) continue;
			for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
				for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
					TH1D* ProjNumX = fHisto.GetTH2(Form("EffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->ProjectionY("NumX",iBurst,iBurst);
					TH1D* ProjNumHighX =  fHisto.GetTH2(Form("EffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->ProjectionY("NumHighX",iBurst,iBurst);
					TH1D* ProjDenX = fHisto.GetTH2(Form("EffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->ProjectionY("DenX",iBurst,iBurst);
					TH1D* ProjNumY = fHisto.GetTH2(Form("EffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->ProjectionY("NumY",iBurst,iBurst);
					TH1D* ProjNumHighY = fHisto.GetTH2(Form("EffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->ProjectionY("NumHighY",iBurst,iBurst);
					TH1D* ProjDenY = fHisto.GetTH2(Form("EffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->ProjectionY("DenY",iBurst,iBurst);
					Int_t NumX = ProjNumX->GetEntries();
					Int_t NumHighX = ProjNumHighX->GetEntries();
					Int_t DenX = ProjDenX->GetEntries();
					Int_t NumY = ProjNumY->Integral(2,9);
					Int_t NumHighY = ProjNumHighY->Integral(2,9);
					Int_t DenY = ProjDenY->Integral(2,9);
					for (int i=0; i<NumX; i++) 	fHisto.GetTH1(Form("TotalEffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID);
					for (int i=0; i<NumHighX; i++) 	fHisto.GetTH1(Form("TotalEffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID);
					for (int i=0; i<DenX; i++) 	fHisto.GetTH1(Form("TotalEffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID);
					for (int i=0; i<NumY; i++) 	fHisto.GetTH1(Form("TotalEffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID);
					for (int i=0; i<NumHighY; i++) 	fHisto.GetTH1(Form("TotalEffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID);
					for (int i=0; i<DenY; i++) 	fHisto.GetTH1(Form("TotalEffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(BurstID);
					delete ProjNumX;
					delete ProjNumHighX;
					delete ProjDenX;
					delete ProjNumY;
					delete ProjNumHighY;
					delete ProjDenY;
				}
			}

		}

		for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				fTotEfficiencyForPlaneX[iPlane][iTslice]->SetPassedHistogram(*fHisto.GetTH1(Form("TotalEffiNumX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");
				fTotEfficiencyForPlaneX[iPlane][iTslice]->SetTotalHistogram(*fHisto.GetTH1(Form("TotalEffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");
				fTotEfficiencyForPlaneHighX[iPlane][iTslice]->SetPassedHistogram(*fHisto.GetTH1(Form("TotalEffiNumHighX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");
				fTotEfficiencyForPlaneHighX[iPlane][iTslice]->SetTotalHistogram(*fHisto.GetTH1(Form("TotalEffiDenX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");
				fTotEfficiencyForPlaneY[iPlane][iTslice]->SetPassedHistogram(*fHisto.GetTH1(Form("TotalEffiNumY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");
				fTotEfficiencyForPlaneY[iPlane][iTslice]->SetTotalHistogram(*fHisto.GetTH1(Form("TotalEffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");
				fTotEfficiencyForPlaneHighY[iPlane][iTslice]->SetPassedHistogram(*fHisto.GetTH1(Form("TotalEffiNumHighY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");
				fTotEfficiencyForPlaneHighY[iPlane][iTslice]->SetTotalHistogram(*fHisto.GetTH1(Form("TotalEffiDenY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1])),"f");

				fX_EffBurst[iPlane][iTslice]=fTotEfficiencyForPlaneX[iPlane][iTslice]->CreateGraph();
				fY_EffBurst[iPlane][iTslice]=fTotEfficiencyForPlaneY[iPlane][iTslice]->CreateGraph();
				fX_HighEffBurst[iPlane][iTslice]=fTotEfficiencyForPlaneHighX[iPlane][iTslice]->CreateGraph();
				fY_HighEffBurst[iPlane][iTslice]=fTotEfficiencyForPlaneHighY[iPlane][iTslice]->CreateGraph();
				fX_EffBurst[iPlane][iTslice]->SetNameTitle(Form("TotalEffiX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("TotalEffiX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]));
				fX_HighEffBurst[iPlane][iTslice]->SetNameTitle(Form("TotalHighEffiX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("TotalHighEffiX%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]));
				fY_EffBurst[iPlane][iTslice]->SetNameTitle(Form("TotalEffiY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("TotalEffiY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]));
				fY_HighEffBurst[iPlane][iTslice]->SetNameTitle(Form("TotalHighEffiY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]),Form("TotalHighEffiY%d_SliceStart%3.0f_SliceEnd%3.0f_VSBurstID",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]));
			}
		}
	}


	/////////////
  	// HISTO mode

  	if (!fReadingData) {
    		if (!fCumEffiX) {
      			std::cout << user_normal() << "Asked to read my own output but cannot found it" << std::endl;
      			return;
    		}
		/////////////////////////
   		// Produce the PDF output

		TString Planes[6];
		Planes[0]="A";Planes[1]="B";Planes[2]="C";Planes[3]="D";Planes[4]="E";Planes[5]="F";

    		fCanvas = new TCanvas("Canvas");

    		// Efficiency vs X
    		TString DrawOption = "";
    		TLegend* Legend = new TLegend(0.83,0.13,0.95,0.4);
    		Legend->SetFillColor(kWhite);
		for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			if(iPlane) DrawOption = "P&same";
			else {
				fCumEffiX[iPlane][0]->GetYaxis()->SetRangeUser(0.65,1.05);
				DrawOption = "AP";
			}
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				fCumEffiX[iPlane][iTslice]->SetTitle(Form("Cumulative Efficiency X"));
				fCumEffiX[iPlane][iTslice]->SetLineColor(iPlane+1);
      				fCumEffiX[iPlane][iTslice]->SetMarkerColor(iPlane+1);
      				fCumEffiX[iPlane][iTslice]->SetMarkerStyle(20+iPlane*4);
      				fCumEffiX[iPlane][iTslice]->SetMarkerSize(0.8);
      				fCumEffiX[iPlane][iTslice]->Draw(DrawOption);
      				fCumEffiX[iPlane][iTslice]->GetXaxis()->SetTitle("X (mm)");
				fCumEffiX[iPlane][iTslice]->GetYaxis()->SetTitle("Efficiency");
			}
			Legend->AddEntry(fCumEffiX[iPlane][0],Form("Plane %s",Planes[iPlane].Data()));
		}
		Legend->Draw();
		fCanvas->Print(Form(fOutPDFFileName + "("), "pdf"); // open and print the canvas
		delete Legend;

		// Efficiency vs Y
		fCanvas->Clear();
   	 	Legend = new TLegend(0.83,0.13,0.95,0.4);
    		Legend->SetFillColor(kWhite);
    		for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			if(iPlane) DrawOption = "P&same";
			else {
				fCumEffiY[iPlane][0]->GetYaxis()->SetRangeUser(0.65,1.05);
				DrawOption = "AP";
			}
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				fCumEffiY[iPlane][iTslice]->SetTitle(Form("Cumulative Efficiency Y"));
				fCumEffiY[iPlane][iTslice]->SetLineColor(iPlane+1);
      				fCumEffiY[iPlane][iTslice]->SetMarkerColor(iPlane+1);
      				fCumEffiY[iPlane][iTslice]->SetMarkerStyle(20+iPlane*4);
      				fCumEffiY[iPlane][iTslice]->SetMarkerSize(0.8);
      				fCumEffiY[iPlane][iTslice]->Draw(DrawOption);
      				fCumEffiY[iPlane][iTslice]->GetXaxis()->SetTitle("Y (mm)");
				fCumEffiY[iPlane][iTslice]->GetYaxis()->SetTitle("Efficiency");
    			}
			Legend->AddEntry(fCumEffiY[iPlane][0],Form("Plane %s",Planes[iPlane].Data()));
		}
		Legend->Draw();
		fCanvas->Print(fOutPDFFileName, "pdf");
		delete Legend;

		// Efficiency high threshold vs X
		fCanvas->Clear();
   	 	Legend = new TLegend(0.83,0.13,0.95,0.4);
    		Legend->SetFillColor(kWhite);
    		for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			if(iPlane) DrawOption = "P&same";
			else {
				fCumEffiHighX[iPlane][0]->GetYaxis()->SetRangeUser(0.5,1.05);
				DrawOption = "AP";
			}
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				fCumEffiHighX[iPlane][iTslice]->SetTitle(Form("Cumulative Efficiency High Threshold X"));
				fCumEffiHighX[iPlane][iTslice]->SetLineColor(iPlane+1);
      				fCumEffiHighX[iPlane][iTslice]->SetMarkerColor(iPlane+1);
      				fCumEffiHighX[iPlane][iTslice]->SetMarkerStyle(20+iPlane*4);
      				fCumEffiHighX[iPlane][iTslice]->SetMarkerSize(0.8);
      				fCumEffiHighX[iPlane][iTslice]->Draw(DrawOption);
      				fCumEffiHighX[iPlane][iTslice]->GetXaxis()->SetTitle("X (mm)");
				fCumEffiHighX[iPlane][iTslice]->GetYaxis()->SetTitle("Efficiency");

    			}
			Legend->AddEntry(fCumEffiHighX[iPlane][0],Form("Plane %s",Planes[iPlane].Data()));
		}
		Legend->Draw();
		fCanvas->Print(fOutPDFFileName, "pdf"); // open and print the canvas
		delete Legend;

		// Efficiency high threshold vs Y
		fCanvas->Clear();
   	 	Legend = new TLegend(0.83,0.13,0.95,0.4);
    		Legend->SetFillColor(kWhite);
    		for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			if(iPlane) DrawOption = "P&same";
			else {
				fCumEffiHighY[iPlane][0]->GetYaxis()->SetRangeUser(0.5,1.05);
				DrawOption = "AP";
			}
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				fCumEffiHighY[iPlane][iTslice]->SetTitle(Form("Cumulative Efficiency High Threshold Y"));
				fCumEffiHighY[iPlane][iTslice]->SetLineColor(iPlane+1);
      				fCumEffiHighY[iPlane][iTslice]->SetMarkerColor(iPlane+1);
      				fCumEffiHighY[iPlane][iTslice]->SetMarkerStyle(20+iPlane*4);
      				fCumEffiHighY[iPlane][iTslice]->SetMarkerSize(0.8);
      				fCumEffiHighY[iPlane][iTslice]->Draw(DrawOption);
      				fCumEffiHighY[iPlane][iTslice]->GetXaxis()->SetTitle("Y (mm)");
				fCumEffiHighY[iPlane][iTslice]->GetYaxis()->SetTitle("Efficiency");

    			}
			Legend->AddEntry(fCumEffiHighY[iPlane][0],Form("Plane %s",Planes[iPlane].Data()));
		}
		Legend->Draw();
		fCanvas->Print(fOutPDFFileName, "pdf");
		delete Legend;

		//Plot total efficiency in X layers VS burstID
		fCanvas->Clear();
   	 	Legend = new TLegend(0.83,0.13,0.95,0.4);
    		Legend->SetFillColor(kWhite);
    		for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			if(iPlane) DrawOption = "P&same";
			else {
				fX_EffBurst[iPlane][0]->GetYaxis()->SetRangeUser(0.,1.05);
				DrawOption = "AP";
			}
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				fX_EffBurst[iPlane][iTslice]->SetTitle(Form("Total efficiency in X layers VS burstID"));
				fX_EffBurst[iPlane][iTslice]->SetLineColor(iPlane+1);
      				fX_EffBurst[iPlane][iTslice]->SetMarkerColor(iPlane+1);
      				fX_EffBurst[iPlane][iTslice]->SetMarkerStyle(20+iPlane*4);
      				fX_EffBurst[iPlane][iTslice]->SetMarkerSize(0.8);
      				fX_EffBurst[iPlane][iTslice]->Draw(DrawOption);
      				fX_EffBurst[iPlane][iTslice]->GetXaxis()->SetTitle("BurstID");
				fX_EffBurst[iPlane][iTslice]->GetYaxis()->SetTitle("Efficiency");

    			}
			Legend->AddEntry(fX_EffBurst[iPlane][0],Form("Plane %s",Planes[iPlane].Data()));
		}
		Legend->Draw();
		fCanvas->Print(fOutPDFFileName, "pdf");
		delete Legend;

		//Plot total efficiency in Y layers VS burstID
		fCanvas->Clear();
   	 	Legend = new TLegend(0.83,0.13,0.95,0.4);
    		Legend->SetFillColor(kWhite);
    		for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			if(iPlane) DrawOption = "P&same";
			else {
				fY_EffBurst[iPlane][0]->GetYaxis()->SetRangeUser(0.0,1.05);
				DrawOption = "AP";
			}
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				fY_EffBurst[iPlane][iTslice]->SetTitle(Form("Total efficiency in Y layers VS burstID"));
				fY_EffBurst[iPlane][iTslice]->SetLineColor(iPlane+1);
      				fY_EffBurst[iPlane][iTslice]->SetMarkerColor(iPlane+1);
      				fY_EffBurst[iPlane][iTslice]->SetMarkerStyle(20+iPlane*4);
      				fY_EffBurst[iPlane][iTslice]->SetMarkerSize(0.8);
      				fY_EffBurst[iPlane][iTslice]->Draw(DrawOption);
      				fY_EffBurst[iPlane][iTslice]->GetXaxis()->SetTitle("BurstID");
				fY_EffBurst[iPlane][iTslice]->GetYaxis()->SetTitle("Efficiency");

    			}
			Legend->AddEntry(fY_EffBurst[iPlane][0],Form("Plane %s",Planes[iPlane].Data()));
		}
		Legend->Draw();
		fCanvas->Print(fOutPDFFileName, "pdf");
		delete Legend;

		//Plot total efficiency in X (high threshold) layers VS burstID
		fCanvas->Clear();
   	 	Legend = new TLegend(0.83,0.13,0.95,0.4);
    		Legend->SetFillColor(kWhite);
    		for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			if(iPlane) DrawOption = "P&same";
			else {
				fX_HighEffBurst[iPlane][0]->GetYaxis()->SetRangeUser(0.0,1.05);
				DrawOption = "AP";
			}
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				fX_HighEffBurst[iPlane][iTslice]->SetTitle(Form("Total efficiency in X (high threshold) layers VS burstID"));
				fX_HighEffBurst[iPlane][iTslice]->SetLineColor(iPlane+1);
      				fX_HighEffBurst[iPlane][iTslice]->SetMarkerColor(iPlane+1);
      				fX_HighEffBurst[iPlane][iTslice]->SetMarkerStyle(20+iPlane*4);
      				fX_HighEffBurst[iPlane][iTslice]->SetMarkerSize(0.8);
      				fX_HighEffBurst[iPlane][iTslice]->Draw(DrawOption);
      				fX_HighEffBurst[iPlane][iTslice]->GetXaxis()->SetTitle("BurstID");
				fX_HighEffBurst[iPlane][iTslice]->GetYaxis()->SetTitle("Efficiency");

    			}
			Legend->AddEntry(fX_HighEffBurst[iPlane][0],Form("Plane %s",Planes[iPlane].Data()));
		}
		Legend->Draw();
		fCanvas->Print(fOutPDFFileName, "pdf");
		delete Legend;

		//Plot total efficiency in Y (high threshold) layers VS burstID
		fCanvas->Clear();
   	 	Legend = new TLegend(0.83,0.13,0.95,0.4);
    		Legend->SetFillColor(kWhite);
    		for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			if(iPlane) DrawOption = "P&same";
			else {
				fY_HighEffBurst[iPlane][0]->GetYaxis()->SetRangeUser(0.0,1.05);
				DrawOption = "AP";
			}
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				fY_HighEffBurst[iPlane][iTslice]->SetTitle(Form("Total efficiency in Y (high threshold) layers VS burstID"));
				fY_HighEffBurst[iPlane][iTslice]->SetLineColor(iPlane+1);
      				fY_HighEffBurst[iPlane][iTslice]->SetMarkerColor(iPlane+1);
      				fY_HighEffBurst[iPlane][iTslice]->SetMarkerStyle(20+iPlane*4);
      				fY_HighEffBurst[iPlane][iTslice]->SetMarkerSize(0.8);
      				fY_HighEffBurst[iPlane][iTslice]->Draw(DrawOption);
      				fY_HighEffBurst[iPlane][iTslice]->GetXaxis()->SetTitle("BurstID");
				fY_HighEffBurst[iPlane][iTslice]->GetYaxis()->SetTitle("Efficiency");

    			}
			Legend->AddEntry(fY_HighEffBurst[iPlane][0],Form("Plane %s",Planes[iPlane].Data()));
		}
		Legend->Draw();
		fCanvas->Print(fOutPDFFileName, "pdf");
		delete Legend;

		// Efficiency X VS BurstID
		fCanvas->Clear();
		DrawOption = "colz";
   	 	for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				fEffiX_VS_BurstID[iPlane][iTslice]->SetStats(0);
      				fEffiX_VS_BurstID[iPlane][iTslice]->Draw(DrawOption);
      				fEffiX_VS_BurstID[iPlane][iTslice]->GetXaxis()->SetTitle("Burst ID");
				fEffiX_VS_BurstID[iPlane][iTslice]->GetYaxis()->SetTitle("X (mm)");
				fCanvas->Print(fOutPDFFileName, "pdf");
    			}
		}

		// Efficiency Y VS BurstID
		fCanvas->Clear();
		DrawOption = "colz";
   	 	for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				fEffiY_VS_BurstID[iPlane][iTslice]->SetStats(0);
      				fEffiY_VS_BurstID[iPlane][iTslice]->Draw(DrawOption);
      				fEffiY_VS_BurstID[iPlane][iTslice]->GetXaxis()->SetTitle("Burst ID");
				fEffiY_VS_BurstID[iPlane][iTslice]->GetYaxis()->SetTitle("Y (mm)");
				fCanvas->Print(fOutPDFFileName, "pdf");
    			}
		}

		// Efficiency of high threshold X VS BurstID
		fCanvas->Clear();
		DrawOption = "colz";
   	 	for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				fEffiHighX_VS_BurstID[iPlane][iTslice]->SetStats(0);
      				fEffiHighX_VS_BurstID[iPlane][iTslice]->Draw(DrawOption);
      				fEffiHighX_VS_BurstID[iPlane][iTslice]->GetXaxis()->SetTitle("Burst ID");
				fEffiHighX_VS_BurstID[iPlane][iTslice]->GetYaxis()->SetTitle("X (mm)");
				fCanvas->Print(fOutPDFFileName, "pdf");
    			}
		}

		// Efficiency of high threshold Y VS BurstID
		fCanvas->Clear();
		DrawOption = "colz";
   	 	for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
			for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
				fEffiHighY_VS_BurstID[iPlane][iTslice]->SetStats(0);
      				fEffiHighY_VS_BurstID[iPlane][iTslice]->Draw(DrawOption);
      				fEffiHighY_VS_BurstID[iPlane][iTslice]->GetXaxis()->SetTitle("Burst ID");
				fEffiHighY_VS_BurstID[iPlane][iTslice]->GetYaxis()->SetTitle("Y (mm)");
				if ( iPlane==fNPlanes-1 && iTslice==NTimeSlice-1 ) fCanvas->Print(Form(fOutPDFFileName + ")"), "pdf");
				else fCanvas->Print(fOutPDFFileName, "pdf");
    			}
		}


		for (Int_t iBurst=1; iBurst<=fMaxNBursts; iBurst++) {
			Int_t BurstID = -1;
			Bool_t BadBurst_FewEntries = false;
			BurstID = fHisto.GetTH1("NEventsInBurstAnalyzed")->GetBinCenter(iBurst);
			if ( fHisto.GetTH1("NEventsInBurstAnalyzed")->GetBinContent(iBurst)==0 ) continue;
			for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
				for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){

					BurstID = fEffiNumX_VS_BurstID[iPlane][iTslice]->ProjectionX()->GetBinCenter(iBurst);
					TH1D* ProjNumX = fEffiNumX_VS_BurstID[iPlane][iTslice]->ProjectionY("NumX",iBurst,iBurst);
					TH1D* ProjNumHighX =  fEffiHighNumX_VS_BurstID[iPlane][iTslice]->ProjectionY("NumHighX",iBurst,iBurst);
					TH1D* ProjDenX = fEffiDenX_VS_BurstID[iPlane][iTslice]->ProjectionY("DenX",iBurst,iBurst);
					TH1D* ProjNumY = fEffiNumY_VS_BurstID[iPlane][iTslice]->ProjectionY("NumY",iBurst,iBurst);
					TH1D* ProjNumHighY = fEffiHighNumY_VS_BurstID[iPlane][iTslice]->ProjectionY("NumHighY",iBurst,iBurst);
					TH1D* ProjDenY = fEffiDenY_VS_BurstID[iPlane][iTslice]->ProjectionY("DenY",iBurst,iBurst);
					if (ProjDenX->GetEntries()==0 || ProjDenY->GetEntries()==0 ) {
						BadBurst_FewEntries = true;
						continue;
					}
					Double_t x;
					Int_t NPoints = fX_EffBurst[iPlane][iTslice]->GetN();
					Int_t BurstPoint = -1;
					for (int iPoint = 0; iPoint < NPoints; iPoint++){
						fX_EffBurst[iPlane][iTslice]->GetPoint(iPoint,x,fTotalEffX[iPlane][iTslice]);
						if ((Int_t)x==BurstID){
							BurstPoint = iPoint;
							break;
						}
					}
					if (BurstPoint<0){
						cout<< user_normal() << "DEBUG ERROR - NO BURST FOUND FOR TOTAL EFFICIENCY"<<endl;
						cout<< user_normal() << "BurstID = "<<BurstID;
						continue;
					}
					fX_EffBurst[iPlane][iTslice]->GetPoint(BurstPoint,x,fTotalEffX[iPlane][iTslice]);
					fY_EffBurst[iPlane][iTslice]->GetPoint(BurstPoint,x,fTotalEffY[iPlane][iTslice]);
					fX_HighEffBurst[iPlane][iTslice]->GetPoint(BurstPoint,x,fTotalEffHighX[iPlane][iTslice]);
					fY_HighEffBurst[iPlane][iTslice]->GetPoint(BurstPoint,x,fTotalEffHighY[iPlane][iTslice]);
					for (int i = 0; i<fNBins; i++){
						fX[iPlane][i][iTslice] = 0.0;
  						fY[iPlane][i][iTslice] = 0.0;
  						fErrX[iPlane][i][iTslice] = 0.0;
  						fEfX[iPlane][i][iTslice] = 0.0;
  						fEfY[iPlane][i][iTslice] = 0.0;
  						fEfHighX[iPlane][i][iTslice] = 0.0;
  						fEfHighY[iPlane][i][iTslice] = 0.0;
  						fErrEfX[iPlane][i][iTslice] = 0.0;
  						fErrEfY[iPlane][i][iTslice] = 0.0;
  						fErrEfHighX[iPlane][i][iTslice] = 0.0;
						fErrEfHighY[iPlane][i][iTslice] = 0.0;

      						if(ProjNumX->GetBinContent(i+1) > 0) {
		    					fEfX[iPlane][i][iTslice] = ProjNumX->GetBinContent(i+1)/ProjDenX->GetBinContent(i+1);
							fEfHighX[iPlane][i][iTslice] = ProjNumHighX->GetBinContent(i+1)/ProjDenX->GetBinContent(i+1);
      						} else {
		    					fEfX[iPlane][i][iTslice] = -0.01;
								fEfHighX[iPlane][i][iTslice] = -0.01;
      						}
		  				if(ProjNumY->GetBinContent(i+1) > 0) {
		    					fEfY[iPlane][i][iTslice] = ProjNumY->GetBinContent(i+1)/ProjDenY->GetBinContent(i+1);
        						fEfHighY[iPlane][i][iTslice] = ProjNumHighY->GetBinContent(i+1)/ProjDenY->GetBinContent(i+1);
		  				} else {
		    					fEfY[iPlane][i][iTslice] = -0.01;
		    					fEfHighY[iPlane][i][iTslice] = -0.01;
      						}
						fErrEfX[iPlane][i][iTslice] = sqrt(fEfX[iPlane][i][iTslice]*(1 - fEfX[iPlane][i][iTslice])/ProjDenX->GetBinContent(i+1));
      						fErrEfY[iPlane][i][iTslice] = sqrt(fEfY[iPlane][i][iTslice]*(1 - fEfY[iPlane][i][iTslice])/ProjDenY->GetBinContent(i+1));
						fErrEfHighX[iPlane][i][iTslice] = sqrt(fEfHighX[iPlane][i][iTslice]*(1 - fEfHighX[iPlane][i][iTslice])/ProjDenX->GetBinContent(i+1));
      						fErrEfHighY[iPlane][i][iTslice] = sqrt(fEfHighY[iPlane][i][iTslice]*(1 - fEfHighY[iPlane][i][iTslice])/ProjDenY->GetBinContent(i+1));
      						fX[iPlane][i][iTslice] = ProjDenX->GetBinCenter(i+1);
      						fY[iPlane][i][iTslice] = ProjDenY->GetBinCenter(i+1);
     	 					fErrX[iPlane][i][iTslice] = ProjDenX->GetBinWidth(i+1)/2;
						fHisto.GetTH2(Form("MonitorEffiX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(fX[iPlane][i][iTslice],fEfX[iPlane][i][iTslice]);
						fHisto.GetTH2(Form("MonitorEffiY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(fY[iPlane][i][iTslice],fEfY[iPlane][i][iTslice]);
						fHisto.GetTH2(Form("MonitorEffiHighX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(fX[iPlane][i][iTslice],fEfHighX[iPlane][i][iTslice]);
						fHisto.GetTH2(Form("MonitorEffiHighY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(fY[iPlane][i][iTslice],fEfHighY[iPlane][i][iTslice]);
						fHisto.GetTH2(Form("MonitorEffiErrX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(fX[iPlane][i][iTslice],fErrEfX[iPlane][i][iTslice]);
						fHisto.GetTH2(Form("MonitorEffiErrY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(fY[iPlane][i][iTslice],fErrEfY[iPlane][i][iTslice]);
						fHisto.GetTH2(Form("MonitorEffiHighErrX%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(fX[iPlane][i][iTslice],fErrEfHighX[iPlane][i][iTslice]);
						fHisto.GetTH2(Form("MonitorEffiHighErrY%d_SliceStart%3.0f_SliceEnd%3.0f",iPlane+1,fLimitsSlice[iTslice],fLimitsSlice[iTslice+1]))->Fill(fY[iPlane][i][iTslice],fErrEfHighY[iPlane][i][iTslice]);

     	 				}

				}

			}

			Int_t RunID = GetRunID();
			fBurstFile.open(Form("CHANTIMuonEfficiency_BadBursts.dat"), ios::app);
			fBadLayersFile.open(Form("CHANTIMuonEfficiency_BadLayers.dat"), ios::app);
			fBadPositionsFile.open(Form("CHANTIMuonEfficiency_BadPositions.dat"), ios::app);
			vector <double> BadPlane, BadTslice, BadBin, BadLayer, BadEff, BadTotalEffPlane,BadTotalEffTslice,BadTotalEffLayer,BadTotalEff;
			BadPlane.clear();
			BadTslice.clear();
			BadBin.clear();
			BadLayer.clear();
			BadEff.clear();
			BadTotalEffPlane.clear();
			BadTotalEffTslice.clear();
			BadTotalEffLayer.clear();
			BadTotalEff.clear();
			for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
				for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
					for (int iBin = 0; iBin<fNBins; iBin++){
						Int_t Internal = (iBin!=0 && iBin!=(fNBins-1) ) ? 0:1;
						if (fEfX[iPlane][iBin][iTslice]<fLowBoundEff[iPlane][Internal]){
							BadPlane.push_back(iPlane);
							BadTslice.push_back(iTslice);
							BadBin.push_back(iBin);
							BadLayer.push_back(0);
							BadEff.push_back(fEfX[iPlane][iBin][iTslice]);
						}
						if (fEfY[iPlane][iBin][iTslice]<fLowBoundEff[iPlane][Internal] && fEfY[iPlane][iBin][iTslice]>=0) {
							if (! (iBin==0) ){
								BadPlane.push_back(iPlane);
								BadTslice.push_back(iTslice);
								BadBin.push_back(iBin);
								BadLayer.push_back(1);
								BadEff.push_back(fEfY[iPlane][iBin][iTslice]);
							}
						}
					}
					if (fTotalEffX[iPlane][iTslice] < fLayerEffCut[iPlane]){
						BadTotalEffPlane.push_back(iPlane);
						BadTotalEffTslice.push_back(iTslice);
						BadTotalEffLayer.push_back(0);
						BadTotalEff.push_back(fTotalEffX[iPlane][iTslice]);
					}
					if (fTotalEffY[iPlane][iTslice] < fLayerEffCut[iPlane] ){
						BadTotalEffPlane.push_back(iPlane);
						BadTotalEffTslice.push_back(iTslice);
						BadTotalEffLayer.push_back(1);
						BadTotalEff.push_back(fTotalEffY[iPlane][iTslice]);
					}
				}
			}
			if (BadPlane.size()>0 || BadBurst_FewEntries){
				if (BadPlane.size()>0){
					fBadPositionsFile << "[CHANTIMuonEfficiency]"<<Form("BadBurst %06d %04d",RunID,(Int_t)BurstID)<< "\n";
					for (unsigned int iBad=0; iBad<BadPlane.size(); iBad++)
       						fBadPositionsFile << BadLayer[iBad] << " " << BadPlane[iBad] << " " << BadBin[iBad] << " " << BadTslice[iBad] << " " << BadEff[iBad] << "\n";
				} else if (BadBurst_FewEntries){
					fBadPositionsFile << "[CHANTIMuonEfficiency]"<<Form("BadBurst %06d %04d FewEntries",RunID,(Int_t)BurstID)<< "\n";
				}
			}

			if (BadTotalEffPlane.size()>0 || BadBurst_FewEntries){
				fBurstFile << Form("BadBurst %06d %04d",RunID,(Int_t)BurstID)<<"\n";
				if (BadTotalEffPlane.size()>0) {
					fBadLayersFile << "[CHANTIMuonEfficiency]"<<Form("BadBurst %06d %04d",RunID,(Int_t)BurstID)<<"\n";
					for (unsigned int iBad=0; iBad<BadTotalEffPlane.size(); iBad++)
       						fBadLayersFile << BadTotalEffLayer[iBad] << " " << BadTotalEffPlane[iBad] << " " << BadTotalEffTslice[iBad] << " " << BadTotalEff[iBad] << "\n";
				} else if (BadBurst_FewEntries){
					fBadLayersFile << "[CHANTIMuonEfficiency]"<<Form("BadBurst FewEntries %05d %04d",RunID,(Int_t)BurstID)<<"\n";
				}
			}

			fBurstFile.close();
			fBadPositionsFile.close();
			fBadLayersFile.close();
		}

	}

	for(int iPlane = 0; iPlane < fNPlanes; iPlane++){
		for (int iTslice = 0; iTslice < NTimeSlice; iTslice++){
			fX_EffBurst[iPlane][iTslice]->Write();
			fY_EffBurst[iPlane][iTslice]->Write();
			fX_HighEffBurst[iPlane][iTslice]->Write();
			fY_HighEffBurst[iPlane][iTslice]->Write();
		}
	}
	SaveAllPlots();
	gErrorIgnoreLevel = -1; // restore the default
}

void CHANTIMuonEfficiency::EndOfRunUser(){


}


void CHANTIMuonEfficiency::DrawPlot(){

}

CHANTIMuonEfficiency::~CHANTIMuonEfficiency(){

}
