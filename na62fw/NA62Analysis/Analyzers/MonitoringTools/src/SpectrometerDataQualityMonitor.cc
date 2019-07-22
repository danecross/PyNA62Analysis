#include <stdlib.h>
#include <iostream>
#include <vector>
#include <array>
#include <algorithm>
#include <numeric>
#include <TSystem.h>
#include <TChain.h>
#include <TStyle.h>
#include <TMath.h>
#include <TLatex.h>
#include <TPad.h>
#include <THStack.h>
#include <TPolyLine.h>
#include <TGraphErrors.h>
#include "CoarseT0Evaluation.hh" // StrawDrift function
#include "SpectrometerDataQualityMonitor.hh"
#include "functions.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class SpectrometerDataQualityMonitor
/// \Brief
/// Monitoring of Spectrometer data quality based on Reconstruction histograms.
/// \EndBrief
///
/// \Detailed
/// The analyzer should be run in the usual two-step processing. In the first
/// step, burst quality is checked for each burst, burst quality and empty
/// covers and straws are saved in TGraphs to be used in second step of
/// analyzer run. Monitoring plots are produced and saved in a PDF file output.
///
/// \author Michal Koval (michal.koval@cern.ch)
/// \EndDetailed

SpectrometerDataQualityMonitor::SpectrometerDataQualityMonitor(Core::BaseAnalysis *ba) :
    Analyzer(ba, "SpectrometerDataQualityMonitor"), fCoverT0s{},
    fDigiCover{nullptr}, fDigiStraw{nullptr}, fHChannelActivity{nullptr},
    fHLeadingTotal{nullptr}, fHTrailingTotal{nullptr}, fHRadiusTotal{nullptr},
    fHStaggeringTotal{nullptr}, fHDecoderErrors{nullptr}, fHResidual{nullptr},
    fGBurstQuality{nullptr}, fGMissingCovers{nullptr}, fGMissingStraws{nullptr}
{
    fRecoSubDirName = "SpectrometerMonitor"; // name of the histogram directory in the Reco file
    AddParam("fOutDirName", &fOutDirName, ".");
    AddParam("fBadBurstFileName", &fBadBurstFileName, "SpectrometerDataQualityMonitor_BadBursts.dat");
    AddParam("fOutT0Name", &fOutT0Name, "SpectrometerDataQualityMonitor_ROMezzaninesT0.new.dat");
    AddParam("fOutResidualsName", &fOutResidualsName, "SpectrometerDataQualityMonitor_Alignment_Residuals.dat");
    AddParam("fMagicT0", &fMagicT0, 25.);
    AddParam("fOutPdfName", &fOutPdfName, fAnalyzerName + ".pdf");
    AddParam("fPrintRecomputedT0s", &fPrintRecomputedT0s, true);
    AddParam("fPrintResiduals", &fPrintResiduals, true);
    fBurstQuality = 0;          // default output = 0 = good burst
    fBurstID = 0;
    fNProcessedBursts = 0;
    for (int ich = 0; ich < 4; ich++) {
        fHRecoChamberHit4Total[ich] = nullptr;
        fHRecoChamberHit3Total[ich] = nullptr;
        fHRecoChamberHit2Total[ich] = nullptr;
        fHProjectedMissedHit[ich]   = nullptr;
    }
}

void SpectrometerDataQualityMonitor::InitOutput()
{
    RegisterOutput("BurstQuality", &fBurstQuality);
    RegisterOutput("MissingCoverIDs", &fMissingCoverIDs);
    RegisterOutput("MissingStrawIDs", &fMissingStrawIDs);
    if (!GetIsTree() && GetIsHisto()) {
        ofstream badBurstFile;
        badBurstFile.open(fBadBurstFileName, ios::out);
        badBurstFile << "### Spectrometer bad bursts identified by SpectrometerDataQualityMonitor analyzer" << endl;
        badBurstFile.close();
    }
}

void SpectrometerDataQualityMonitor::InitHist()
{
    const bool kHistoAppend  = true;  // accumulate over all processed bursts
    const bool kHistoReplace = false; // kHistoReplace the histogram after each burst
    if (GetIsTree()){ // first step
        // Reco histograms needed for burst-by-burst analysis
        fDigiStraw = static_cast<TH2F*>(RequestHistogram(fRecoSubDirName, "DigiTimeRawFineVsROChannel", kHistoReplace));
        fDigiCover = static_cast<TH2F*>(RequestHistogram(fRecoSubDirName, "DigiTimeRawFine", kHistoReplace));
        BookHisto("burstQualityPerBurst", new TGraph());
        BookHisto("missingCoversPerBurst", new TGraph());
        BookHisto("missingStrawsPerBurst", new TGraph());
        fGBurstQuality  = fHisto.GetTGraph("burstQualityPerBurst");
        fGMissingCovers = fHisto.GetTGraph("missingCoversPerBurst");
        fGMissingStraws = fHisto.GetTGraph("missingStrawsPerBurst");
        fGBurstQuality->SetName("burstQualityPerBurst");
        fGBurstQuality->SetTitle(";Burst ID; Quality flag");
        fGMissingCovers->SetName("missingCoversPerBurst");
        fGMissingCovers->SetTitle(";Burst ID; Missing Cover IDs");
        fGMissingStraws->SetName("missingStrawsPerBurst");
        fGMissingStraws->SetTitle(";Burst ID; Missing Straw IDs");

        // Request reco histograms for use in second step
        RequestHistogram(fRecoSubDirName, "strawChannelActivity", kHistoAppend);
        RequestHistogram(fRecoSubDirName, "strawLeadingTotal", kHistoAppend);
        RequestHistogram(fRecoSubDirName, "strawTrailingTotal", kHistoAppend);
        RequestHistogram(fRecoSubDirName, "strawRadius", kHistoAppend);
        RequestHistogram(fRecoSubDirName, "RecoHitWireSum2", kHistoAppend);
        RequestHistogram(fRecoSubDirName, "srbIDvsAllLeadingTime", kHistoAppend);
        RequestHistogram(fRecoSubDirName, "coverIDvsAllLeadingTime", kHistoAppend);
        RequestHistogram(fRecoSubDirName, "DigiTimeRawFine", kHistoAppend);
        RequestHistogram(fRecoSubDirName, "HResidual", kHistoAppend);
        RequestHistogram(fRecoSubDirName, "DecoderErrors", kHistoAppend);
        const TString namesProjected[4] = {"XY123_ch0", "XY023_ch1", "XY013_ch2", "XY012_ch3"};
        for (int ich = 0; ich < 4; ich++) {
            RequestHistogram(fRecoSubDirName, Form("RecoHit4Views_ch%d", ich), kHistoAppend);
            RequestHistogram(fRecoSubDirName, Form("RecoHit3Views_ch%d", ich), kHistoAppend);
            RequestHistogram(fRecoSubDirName, Form("RecoHit2Views_ch%d", ich), kHistoAppend);
            RequestHistogram(fRecoSubDirName, namesProjected[ich], kHistoAppend);
        }
    } else {
        fGBurstQuality  = reinterpret_cast<TGraph*>(RequestHistogram(fAnalyzerName,"burstQualityPerBurst", kHistoAppend));
        fGMissingCovers = reinterpret_cast<TGraph*>(RequestHistogram(fAnalyzerName,"missingCoversPerBurst", kHistoAppend));
        fGMissingStraws = reinterpret_cast<TGraph*>(RequestHistogram(fAnalyzerName,"missingStrawsPerBurst", kHistoAppend));
        fHChannelActivity = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "strawChannelActivity", kHistoAppend));
        fHLeadingTotal = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "strawLeadingTotal", kHistoAppend));
        fHTrailingTotal = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "strawTrailingTotal", kHistoAppend));
        fHRadiusTotal = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "strawRadius", kHistoAppend));
        fHStaggeringTotal = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "RecoHitWireSum2", kHistoAppend));
        fHCoverIDvsAllLeadingTimeTotal = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "coverIDvsAllLeadingTime", kHistoAppend));
        fHResidual = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "HResidual", kHistoAppend));
        RequestHistogram(fAnalyzerName, "srbIDvsAllLeadingTime", kHistoAppend);

        const TString namesProjected[4] = {"XY123_ch0", "XY023_ch1", "XY013_ch2", "XY012_ch3"};
        for (int ich = 0; ich < 4; ich++) {
            fHRecoChamberHit4Total[ich] =
                static_cast<TH2F*>(RequestHistogram(fAnalyzerName, Form("RecoHit4Views_ch%d", ich), kHistoAppend));
            fHRecoChamberHit3Total[ich] =
                static_cast<TH2F*>(RequestHistogram(fAnalyzerName, Form("RecoHit3Views_ch%d", ich), kHistoAppend));
            fHRecoChamberHit2Total[ich] =
                static_cast<TH2F*>(RequestHistogram(fAnalyzerName, Form("RecoHit2Views_ch%d", ich), kHistoAppend));
            fHProjectedMissedHit[ich] =
                static_cast<TH2F*>(RequestHistogram(fAnalyzerName, namesProjected[ich], kHistoAppend));
        }
        fHDecoderErrors = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "DecoderErrors", kHistoAppend));
    }
}

void SpectrometerDataQualityMonitor::StartOfBurstUser()
{
    fBurstID = GetBurstID();
    fBurstQuality = 0;
    fMissingCoverIDs.clear();
    fMissingStrawIDs.clear();
    SetOutputState("BurstQuality", kOValid);
    SetOutputState("MissingCoverIDs", kOValid);
    SetOutputState("MissingStrawIDs", kOValid);
    if (!GetIsTree()) return;   // evaluate burst quality only in the first step of processing

    if (!fDigiStraw || !fDigiCover) {
        fBurstQuality |= kHistoMissing;
    } else {
        auto nHitsTotal = fDigiStraw->Integral();
        if (nHitsTotal < kMinimumNHits) {
            fBurstQuality |= kEmptyBurst;      // too few hits per burst to analyze
        } else {
            auto hc = fDigiCover->ProjectionX();
            auto hs = fDigiStraw->ProjectionX();
            // find missing covers
            for (int icover = 0; icover < hc->GetNbinsX(); icover++) {
                int isrb = icover / 16;
                if ((isrb % 2 == 0) && (icover % 16 == 11 || icover % 16 == 15)) continue;
                if (kCoverKnownIssue.count(icover) > 0) continue;
                auto nhits = hc->GetBinContent(icover+1);
                if (nhits < nHitsTotal/10000)
                    fMissingCoverIDs.push_back(icover);
            }
            if (fMissingCoverIDs.size() != 0) {
                fBurstQuality |= kCoverMissing;
                for (const auto &c : fMissingCoverIDs)
                    fGMissingCovers->SetPoint(fGMissingCovers->GetN(), fBurstID, c);
            }
            // find missing straws
            if (nHitsTotal > 10*kMinimumNHits) { //  sufficient statistics for straw check
                for (int istraw = 0; istraw < hs->GetNbinsX(); istraw++) {
                    int icover = istraw / 16;
                    int isrb = icover / 16;
                    if ((isrb % 2 == 0) && (icover % 16 == 11 || icover % 16 == 15)) continue;
                    if (kStrawNonexist.count(istraw) > 0) continue;
                    auto nhits = hs->GetBinContent(istraw+1);
                    if (nhits < 50)
                        fMissingStrawIDs.push_back(istraw);
                }
            }
            if (fMissingStrawIDs.size() != 0) {
                if (fMissingStrawIDs.size() >= 3) fBurstQuality |= kStrawsMissing;
                for (const auto &s : fMissingStrawIDs)
                    fGMissingStraws->SetPoint(fGMissingStraws->GetN(), fBurstID, s);
            }
        }
    }
    fGBurstQuality->SetPoint(fGBurstQuality->GetN(), fBurstID, fBurstQuality);
}


void SpectrometerDataQualityMonitor::EndOfJobUser()
{
    gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
    SaveAllPlots();
    fNProcessedBursts = fGBurstQuality->GetN();

    if (!GetIsTree() && GetIsHisto()) {
        // print bad burst list
        PrintSpectrometerDQBadBurstList();
        // pdf output
        gStyle->SetTextFont(42);
        gStyle->SetTitleX(0.5);
        gStyle->SetTitleAlign(23);
        gStyle->SetPalette(55);
        gStyle->SetStatX(0.98);
        gStyle->SetStatY(0.935);
        gStyle->SetNumberContours(99);
        TString fOutPdfPath = gSystem->ConcatFileName(fOutDirName, fOutPdfName);
        // First page: channel activity plot
        GetChannelActivityCanvas()->Print(fOutPdfPath + "(", "pdf");
        // Accumulated cover asymmetry plot
        GetCoverAsymmetryCanvas()->Print(fOutPdfPath, "pdf");
        // Accumulated timing distribution plots overview
        GetTotalTimingCanvas()->Print(fOutPdfPath, "pdf");
        // Reconstructed leading timing peaks of Covers
        GetCoverTimingCanvas()->Print(fOutPdfPath, "pdf");
        if (fPrintRecomputedT0s) {
            TString outT0Path = gSystem->ConcatFileName(fOutDirName, fOutT0Name);
            ofstream outT0;
            outT0.open(outT0Path, ios::out);
            outT0.setf(ios::fixed);
            outT0.setf(ios::right);
            outT0.precision(0);
            outT0 << "# Spectrometer ROMezzanines T0 file" << endl;
            outT0 << "# These T0 were evaluated on top of the automatic T0s and should be added to them in the re-reprocessing" << endl;
            outT0 << "# Format: Starting from SRB ID 0, grouped by 16 Covers" << endl;
            for (int isrb = 0; isrb < 32; isrb++) {
                outT0 << Form("MezzaninesT0_%02d=", isrb);
                for (int icover = 0; icover < 16; icover++) {
                    outT0 << Form(" %.2f", fCoverT0s[isrb][icover]);
                }
                outT0 << endl;
            }
            outT0.close();
        }
        // Reconstructed hits and projected track points
        for (int ich = 0; ich < 4; ich++) {
            GetHitCanvas(ich)->Print(fOutPdfPath, "pdf");
        }
        // Straws geometrical alignment
        fHResidual->FitSlicesX(0,0,-1,5);
        TH1F* hOffset = static_cast<TH1F*>(gDirectory->Get("HResidual_1"));
        if (fPrintResiduals) {
            TString outResidualsPath = gSystem->ConcatFileName(fOutDirName, fOutResidualsName);
            ofstream outResiduals;
            outResiduals.open(outResidualsPath, ios::out);
            outResiduals.setf(ios::fixed);
            outResiduals.setf(ios::right);
            outResiduals.precision(5);
            for (int i = 1; i < hOffset->GetNbinsX(); i++) {
                if (hOffset->GetBinError(i) < 0.01)
                    outResiduals << hOffset->GetBinContent(i);
                else
                    outResiduals << 0;
                outResiduals << std::endl;
            }
            outResiduals.close();
        }
        GetResidualsCanvas(hOffset)->Print(fOutPdfPath, "pdf");
        for (int ich = 0; ich < 4; ich++) {
            GetResidualsCanvas(hOffset, ich)->Print(fOutPdfPath, "pdf");
        }
        // Last page: Decoder errors
        GetDecoderErrorCanvas()->Print(fOutPdfPath + ")", "pdf");
    }
}

unique_ptr<TCanvas> SpectrometerDataQualityMonitor::GetChannelActivityCanvas()
{
    auto c = unique_ptr<TCanvas>(new TCanvas("SpectrometerCanvasActivity"));
    c->SetLogz(kTRUE);
    gStyle->SetOptStat("e");
    gPad->SetRightMargin(0.15);

    if (fHChannelActivity->GetEntries() > 1e5) {
        auto histMin = TMath::Infinity();
        for (int ibinx = 1; ibinx <= fHChannelActivity->GetNbinsX(); ibinx++) {
            for (int ibiny = 1; ibiny <= fHChannelActivity->GetNbinsY(); ibiny++) {
                auto act = fHChannelActivity->GetBinContent(ibinx, ibiny);
                if (act > 10*fNProcessedBursts && act < histMin)
                    histMin = act - 1;
            }
        }
        // set higher minimum to filter electronic noise in display
        fHChannelActivity->SetMinimum(histMin);
    }
    fHChannelActivity->Draw("colz");
    fHChannelActivity->GetXaxis()->SetTitle("Straw ID");

    TText t;
    t.SetTextColor(kRed+2);
    t.SetTextSize(0.05);
    t.DrawTextNDC(0.05, 0.91, Form("Run %d", GetRunID()));
    t.SetTextSize(0.03);
    t.SetTextColor(kBlack);
    for (int i = 0; i < 4; i++) {
        t.SetTextAngle(0);
        const double sep = 21;
        t.DrawText(-8, 2 + sep*i, "U");
        t.DrawText(-8, 7 + sep*i, "V");
        t.DrawText(-8, 12 + sep*i, "X");
        t.DrawText(-8, 17 + sep*i, "Y");
        t.SetTextAngle(-90);
        t.DrawText(140, 18 + sep*i, Form("Chamber %i", i+1));
    }

    return c;
}

unique_ptr<TCanvas> SpectrometerDataQualityMonitor::GetTotalTimingCanvas()
{
    auto c = unique_ptr<TCanvas>(new TCanvas("SpectrometerCanvasTiming"));
    gStyle->SetOptStat("emr");
    gStyle->SetStatW(0.3);
    c->Divide(2,2,0.001,0.001);
    c->cd(1);
    fHLeadingTotal->Draw();
    c->cd(2);
    fHTrailingTotal->Draw();
    c->cd(3);
    fHRadiusTotal->Draw();
    c->cd(4);
    fHStaggeringTotal->SetTitle("Reco Straw Radius Sum in Two Consecutive Planes (staggering)");
    fHStaggeringTotal->Draw();
    fHStaggeringTotal->GetXaxis()->SetRangeUser(0., 10.);
    return c;
}

unique_ptr<TCanvas> SpectrometerDataQualityMonitor::GetCoverTimingCanvas()
{
    auto c = unique_ptr<TCanvas>(new TCanvas("SpectrometerCanvasT0s"));
    gPad->SetGridx();
    gPad->SetGridy();

    auto stackT0 = new THStack(fHCoverIDvsAllLeadingTimeTotal, "x");
    auto histoList = stackT0->GetHists();

    vector<double> covers, t0s;
    for (int i = 0; i < stackT0->GetNhists(); i++) {
        int iSRB = i/16;
        int iCover = i%16;
        bool nonExist = (iSRB % 2 == 0) && (iCover == 11 || iCover == 15);
        if (nonExist) continue;

        TH1F *h = (TH1F*)histoList->At(i);
        if (h->GetEntries() == 0)
            continue; // do not try to fit empty histograms

        auto fStraw = unique_ptr<TF1>(
            new TF1("fStraw", CoarseT0Evaluation::StrawDrift, -100, 400, 8));
        fStraw->SetParameters(h->GetBinContent(h->GetMaximumBin()),
                              h->GetBinCenter(h->GetMaximumBin()),
                              7,20,0.65,130,24,0);
        fStraw->SetParLimits(2,5,10);
        fStraw->SetParLimits(3,15,30);
        fStraw->SetParLimits(4,0.5,0.8);
        fStraw->SetParLimits(5,120,140);
        fStraw->SetParLimits(6,15,30);
        auto fitStatus = h->Fit("fStraw","RQN");
        if (fitStatus == 0) {
            covers.push_back(i);
            t0s.push_back(fStraw->GetParameter(1));
        }
        fCoverT0s[iSRB][iCover] = fStraw->GetParameter(1) - fMagicT0;
        h->SetName(Form("T0/SRB_%02d-Cover%02d", iSRB, iCover));
        h->Write();
    }
    if (covers.size() == 0) return c;

    TGraph timeAlignment(covers.size(), &(covers[0]), &(t0s[0]));
    timeAlignment.SetTitle("Leading Time Peak Position vs. Cover ID; Cover ID; Leading Time Peak [ns]");
    timeAlignment.SetMarkerStyle(20);
    timeAlignment.SetMarkerSize(0.5);
    timeAlignment.SetMarkerColor(kRed+2);
    timeAlignment.SetLineColor(kRed+2);
    double minimum = *min_element(t0s.cbegin(), t0s.cend());
    double maximum = *max_element(t0s.cbegin(), t0s.cend());
    auto ngoodFits = t0s.size();
    double average = accumulate(t0s.cbegin(), t0s.cend(),0.)/ngoodFits;
    if (minimum > 10)
        timeAlignment.SetMinimum(10);
    if (maximum < 40)
        timeAlignment.SetMaximum(40);
    timeAlignment.DrawClone("AP");

    TLine l(0, average, covers.back(), average);
    l.SetLineWidth(2);
    l.SetLineColor(kGreen+2);
    l.DrawClone();
    c->Update();
    return c;
}

const array<vector<double>, 4> SpectrometerDataQualityMonitor::ComputeAsymmetries()
{
    TH1F* coverActivity;
    coverActivity = (TH1F*)fHCoverIDvsAllLeadingTimeTotal->ProjectionY();

    vector<double> index, asymmetry, dasymm, dx;
    for (int ibin = 1; ibin <= coverActivity->GetNbinsX(); ibin++) {
        int iCoverGlobal = ibin-1;
        int iSRB = iCoverGlobal/16;
        int iCover = iCoverGlobal%16;
        bool nonExist = (iSRB % 2 == 0) && (iCover == 11 || iCover == 15);
        int iCoverNeighbour=
            (iCover < 4 || (iCover > 7 && iCover < 12)) ? iCoverGlobal + 4 : iCoverGlobal - 4;

        auto nHitsTotal = coverActivity->GetBinContent(ibin);
        auto nhits2 =  coverActivity->GetBinContent(iCoverNeighbour+1);
        if (nonExist) continue;
        if (kAsymmetryCorrection.count(iCoverGlobal) > 0)
            nHitsTotal*= kAsymmetryCorrection.at(iCoverGlobal);
        if (nHitsTotal == 0 && nhits2 == 0)
            nHitsTotal = nhits2 = 1;
        double a = (nHitsTotal - nhits2) / (nHitsTotal + nhits2);
        double da = (sqrt(nHitsTotal)*nhits2 + nHitsTotal*sqrt(nhits2))/pow((nHitsTotal + nhits2),2);
        index.push_back(iCoverGlobal);
        dx.push_back(0.0001);
        asymmetry.push_back(a);
        dasymm.push_back(da);
    }
    return array<vector<double>, 4> {index, asymmetry, dasymm, dx};
}

unique_ptr<TCanvas> SpectrometerDataQualityMonitor::GetCoverAsymmetryCanvas()
{
    auto c = unique_ptr<TCanvas>(new TCanvas("SpectrometerCanvasAsymmetry"));
    gPad->SetGridx();
    gPad->SetGridy();

    auto data = ComputeAsymmetries();
    TGraphErrors asymm(data[0].size(),
                       &(data[0][0]), &(data[1][0]),
                       &(data[2][0]), &(data[3][0]));
    double minimum = *min_element(data[1].cbegin(), data[1].cend());
    double maximum = *max_element(data[1].cbegin(), data[1].cend());
    if (minimum > -0.1)
        asymm.SetMinimum(-0.1);
    if (maximum < 0.1)
        asymm.SetMaximum(0.1);
    asymm.SetTitle("Cover asymmetry: A(cover) = #frac{N_{hits}(cover) - N_{hits}(neighbor)}{N_{hits}(cover) + N_{hits}(neighbor)}; Cover ID; A");
    gPad->SetTopMargin(0.15);
    gPad->SetRightMargin(0.03);
    asymm.SetMarkerStyle(20);
    asymm.SetMarkerSize(0.5);
    asymm.SetMarkerColor(kGreen+2);
    asymm.SetLineColor(kGreen+2);
    asymm.DrawClone("AP");

    c->Update();
    return c;
}

unique_ptr<TCanvas> SpectrometerDataQualityMonitor::GetDecoderErrorCanvas()
{
    auto c = unique_ptr<TCanvas>(new TCanvas("SpectrometerCanvasDecoder"));
    gStyle->SetOptStat("e");
    gPad->SetLeftMargin(0.2);
    if(fHDecoderErrors) fHDecoderErrors->Draw("colz");
    return c;
}


unique_ptr<TCanvas> SpectrometerDataQualityMonitor::GetHitCanvas(int ichamber)
{
    auto c = unique_ptr<TCanvas>(new TCanvas(Form("SpectrometerHitCanvasChamber%d", ichamber)));
    gStyle->SetOptStat("");
    gPad->SetMargin(0.02, 0.02, 0.02, 0.02);

    TText t;
    t.SetTextFont(43);
    c->Divide(2,2,0.001,0.001);
    t.SetTextSize(15);

    c->cd(1);
    gPad->SetLogz(kTRUE);
    fHRecoChamberHit4Total[ichamber]->Draw("colz");
    t.DrawTextNDC(0.3, 0.92, "Four-view space-points");

    c->cd(2);
    gPad->SetLogz(kTRUE);
    fHRecoChamberHit3Total[ichamber]->Draw("colz");
    t.DrawTextNDC(0.3, 0.92, "Three-view space-points");

    c->cd(3);
    gPad->SetLogz(kTRUE);
    fHRecoChamberHit2Total[ichamber]->Draw("colz");
    t.DrawTextNDC(0.3, 0.92, "Two-view space-points");

    c->cd(4);
    gPad->SetLogz(kTRUE);
    if(fHProjectedMissedHit[ichamber]->GetMinimum() < 1)
        fHProjectedMissedHit[ichamber]->SetMinimum(1);
    fHProjectedMissedHit[ichamber]->Draw("colz");
    t.DrawTextNDC(0.15, 0.92, "Extrapolated tracks: missing space-points");

    c->cd();
    t.SetTextSize(20);
    t.SetTextColor(kBlue+2);
    t.DrawTextNDC(0.43, 0.96, Form("Chamber %d", ichamber+1));

    c->Update();
    return c;
}


unique_ptr<TCanvas> SpectrometerDataQualityMonitor::GetResidualsCanvas(TH1F* hOffset, int ichamber)
{
    TString canName = (ichamber < 0) ?
        "SpectrometerResiduals" : Form("SpectrometerResidualsChamber%d", ichamber);
    auto c = make_unique<TCanvas>(canName);
    c->SetGrid(0,1);
    TLatex t(0.7, 0.905, Form("Run %d", GetRunID()));
    t.SetTextColor(kRed+2);
    t.SetTextSize(0.04);
    t.SetNDC(true);
    t.DrawClone();
    if (!hOffset) {
        t.DrawLatex(0.105, 0.905, "Alignment histo empty");
        c->Update();
        return c;
    }

    hOffset->SetStats(0);
    hOffset->SetMarkerStyle(20);
    hOffset->SetMarkerSize(0.35);
    hOffset->SetMarkerColor(kBlack);
    hOffset->SetLineColor(kBlue+2);
    hOffset->SetLineWidth(1);
    hOffset->GetYaxis()->SetRangeUser(-0.1, 0.1);

    if (ichamber < 0) {
        hOffset->SetTitle("Geometric straw alignment: hit-track residual mean value (gaus fit);Straw ID; Residual [mm]");
        hOffset->Draw();
    } else {
        hOffset->SetTitle(";Straw ID; Residual [mm]");
        const char* vname[4] = {"U", "V", "X", "Y"};
        t.SetTextColor(kBlue+2);
        const int Nstraw = 122;
        c->Divide(2,2, 0.001, 0.001);
        for (int iview = 0; iview < 4; iview++) {
            c->cd(iview+1);
            gPad->SetGrid(0,1);
            gPad->SetRightMargin(0.001);
            const int low = 0 + (ichamber*4 + iview)*4*Nstraw;
            const int high = 4*Nstraw + (ichamber*4 + iview)*4*Nstraw;
            hOffset->GetXaxis()->SetRangeUser(low, high);
            hOffset->DrawClone();
            t.DrawLatex(0.105, 0.905, Form("Chamber %d View %s", ichamber + 1, vname[iview]));
            gPad->Update();
        }
        c->cd();
    }
    c->Update();
    return c;
}

// function to be called in the end of job routine in the second step of processing
void SpectrometerDataQualityMonitor::PrintSpectrometerDQBadBurstList()
{
    if (GetIsTree()) return;      // do nothing in the first step
    ofstream badBurstFile;
    badBurstFile.open(fBadBurstFileName, ios::app);
    for (int i = 0; i < fGBurstQuality->GetN(); i++) { // loop over bursts
        fMissingCoverIDs.clear();
        fMissingStrawIDs.clear();
        double burst = 0, quality = 0;
        fGBurstQuality->GetPoint(i, burst, quality);
        fBurstID = burst;
        fBurstQuality = quality;
        for (int j = 0; j < fGMissingCovers->GetN(); j++) {
            double b = 0, c = 0;
            fGMissingCovers->GetPoint(j, b, c);
            if (b == fBurstID) fMissingCoverIDs.push_back(c);
        }
        for (int j = 0; j < fGMissingStraws->GetN(); j++) {
            double b = 0, s = 0;
            fGMissingStraws->GetPoint(j, b, s);
            if (b == fBurstID) fMissingStrawIDs.push_back(s);
        }
        if (fBurstQuality > kEmptyBurst) {
            TString output{Form("BadBurst %06d %04d ", GetRunID(), fBurstID)};
            if (fBurstQuality & kCoverMissing) {
                output+= "[ Missing covers: ";
                for (const auto &c : fMissingCoverIDs)
                    output+= Form("%d ", c);
                output+= "]";
            }
            if (fBurstQuality & kStrawsMissing) {
                output+= "[ Missing straws: ";
                for (const auto &s : fMissingStrawIDs)
                    output+= Form("%d ", s);
                output+= "]";
            }
            badBurstFile << output << endl;
        }
    }
    badBurstFile.close();
}
