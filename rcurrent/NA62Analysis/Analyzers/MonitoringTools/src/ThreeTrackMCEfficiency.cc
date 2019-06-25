#include <stdlib.h>
#include <iostream>
#include <set>
#include <TChain.h>
#include <TVector3.h>
#include <TLorentzVector.h>
#include <TEfficiency.h>
#include <TF1.h>
#include <TLegend.h>
#include <TPaveStats.h>
#include <TLatex.h>
#include <TGraphAsymmErrors.h>
#include "ThreeTrackMCEfficiency.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "GeometricAcceptance.hh"
#include "SpectrometerTrackVertex.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class ThreeTrackMCEfficiency
/// \Brief
/// General straw spectrometer efficiency analyzer for 3-track MC events
/// \EndBrief
/// \Detailed
/// Straw spectrometer efficiency for 3-track MC events is evaluated by
/// matching reconstructed tracks to the true MC tracks using KinePart.
/// The analyzer works for any kind of K+ -> t1+ t2+ t3- decay.
///
/// This is a two-step analyzer. Necessary histograms with results of the
/// reco-true matching are built in the first step while the efficiency
/// evaluation itself is performed in the second step (--histo mode) of the
/// analyzer running on the output files of step 1. A PDF summary report is
/// also produced, the location of the pdf file is by default in the same
/// directory as the $PWD of the analysis job. TEfficiency objects are in the
/// output root file of the step 2.
///
/// In the denominator for efficiency estimates there are events with all 3
/// true MC tracks from a kaon decay in the acceptance of all 4 straw chambers.
/// A true MC track is considered as efficient if a reconstructed track is
/// found and matched both in position (in all 4 chambers) and in the
/// reconstructed momentum to a reconstructed track. Tracks can be
/// reconstructed using information either from 3 or 4 straw chambers.
/// Efficiency is evaluated separately for the 2 types of tracks.
///
/// An event is considered to be efficient if all 3 tracks are matched to 3
/// different reconstructed tracks. Event efficiency is also evaluated for
/// events with all 3 tracks having momentum above 10 GeV/c, since straw
/// efficiency is worse for tracks with lower momentum and usually cut of
/// momentum larger than 10GeV/c is applied in an analysis.
/// \EndDetailed

ThreeTrackMCEfficiency::ThreeTrackMCEfficiency(Core::BaseAnalysis *ba) :
    Analyzer(ba, "ThreeTrackMCEfficiency")
{
    fReadingTree = true;
    fOutputPDFFileName = fAnalyzerName + ".pdf";
}

void ThreeTrackMCEfficiency::InitOutput() {}

void ThreeTrackMCEfficiency::InitHist() {
    fReadingTree = GetIsTree();
    if (fReadingTree) {
        cout << user_normal() << "Reading reconstructed data" << endl;

        // Histograms for monitoring selection and true-reco matching
        BookHisto(new TH1I("kaonDecayed", "Number of decayed kaons; N", 2, -0.5, 1.5));
        BookHisto(new TH1F("ZvtxAll", "True K^{+} decay vertex position, all kaons; z_{vtx} [m]", 120, 80, 200));
        BookHisto(new TH1I("NposTracks", "Number of positive tracks from kaon decay;N[tracks]", 10, -0.5, 9.5));
        BookHisto(new TH1I("NnegTracks", "Number of negative tracks from kaon decay;N[tracks]", 10, -0.5, 9.5));
        BookHisto(new TH1I("Nneutrals", "Number of neutral particles from kaon decay;N[particles]", 10, -0.5, 9.5));
        BookHisto(new TH1I("NtracksInAcc", "Number of tracks in Straw acceptance;N[tracks]", 10, -0.5, 9.5));
        BookHisto(new TH1F("ZvtxAcc", "True K^{+} decay vertex position, all tracks in Straw acceptance; z_{vtx} [m]", 120, 80, 200));
        BookHisto(new TH1F("NrecoTracks", "Number of reconstructed tracks", 10, -0.5, 9.5));
        BookHisto(new TH1F("NrecoVertices", "Number of reconstructed 3-track vertices", 10, -0.5, 9.5));
        BookHisto(new TH1F("momDiff", "Momentum KinePart Reco difference", 1000, -10000, 10000));
        BookHisto(new TH1F("posDiff", "Position KinePart Reco difference", 1000, 0, 2000));
        BookHisto(new TH1I("NchambersTracks", "Number of chambers used to reconstructed matched tracksks", 6, -0.5, 5.5));
        BookHisto(new TH1I("NmatchedTracks", "Number of true MC tracks matched to reconstructed tracks", 6, -0.5, 5.5));
        BookHisto(new TH1I("NmatchedTracks4Ch", "Number of true MC tracks matched to reconstructed tracks (4 chambers)", 6, -0.5, 5.5));

        // Full event reconstruction efficiency
        BookHisto(new TH1F("eventEfficiencyNumerator", "", 4, -0.5, 3.5));
        BookHisto(new TH1F("eventEfficiencyDenominator", "", 4, -0.5, 3.5));

        // Track momenta and efficiencies
        for (const auto &ch : fCharges) {
            for (const auto &pdgid : fPDGids) {
                Int_t id = pdgid * ch;  // revert PDG id for negative tracks
                BookHisto(new TH1F("mom"+ GetPartName(id, ch).first + "All", "Momentum of " +  GetPartName(id, ch).second + " tracks ; p[GeV]; Entries / 0.5GeV",
                                   150, 0, 75));
                BookHisto(new TH1F("mom"+ GetPartName(id, ch).first + "Acc", "Momentum of " +  GetPartName(id, ch).second + " tracks in acceptance; p[GeV]; Entries / 0.5GeV",
                                   150, 0, 75));
                BookHisto(new TH1F("effMomNum" + GetPartName(id, ch).first, "", 150, 0, 75));
                BookHisto(new TH1F ("effMomNum" + GetPartName(id, ch).first + "4Ch", "",150, 0, 75));
            }
        }

        // Inefficient regions in Straw chambers
        for (int iCh = 0; iCh < 4; iCh++) {
            BookHisto(new TH2F(Form("xyNoMatch_%d", iCh), Form("XY position of not matched true track in CH_%d", iCh+1),
                               1000, -1000, 1000, 1000, -1000, 1000));
            BookHisto(new TH2F(Form("xy3Chambers_%d", iCh), Form("XY position of matched 3 chamber tracks in CH_%d", iCh+1),
                               1000, -1000, 1000, 1000, -1000, 1000));
            BookHisto(new TH2F(Form("xyMiss_%d", iCh), Form("XY matched 3CH tracks with missing space point in CH_%d", iCh+1),
                               1000, -1000, 1000, 1000, -1000, 1000));
        }

    } else {                    // --histo mode request histograms from the tree mode for final display
        cout << user_normal() << "Reading my own output" << endl;
        fHkaonDecayed  = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "kaonDecayed", true));
        fHNtracksInAcc = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "NtracksInAcc", true));
        fHZvtxAll      = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "ZvtxAll", true));
        fHZvtxAcc      = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "ZvtxAcc", true));

        // event efficiency
        fHeffEvent    = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "eventEfficiencyNumerator", true));
        fHeffEventDen = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "eventEfficiencyDenominator", true));

        // track momenta and efficiencies
        for (const auto &ch : fCharges) {
            for (const auto &pdgid : fPDGids) {
                Int_t id = pdgid * ch; // revert PDG id for negative tracks
                fHmomAll[make_pair(id, ch)] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "mom"+ GetPartName(id, ch).first + "All", true));
                fHmomAcc[make_pair(id, ch)] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "mom"+ GetPartName(id, ch).first + "Acc", true));
                fHeffMom[make_pair(id, ch)] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "effMomNum" + GetPartName(id, ch).first, true));
                fHeffMom4Ch[make_pair(id, ch)] = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "effMomNum" + GetPartName(id, ch).first + "4Ch", true));
            }
        }

        for (int iCh = 0; iCh < 4; iCh++) {
            fHxyNoMatch[iCh]   = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, Form("xyNoMatch_%d", iCh), true));
            fHxy3Chambers[iCh] = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, Form("xy3Chambers_%d", iCh), true));
            fHxyMiss[iCh]      = static_cast<TH2F*>(RequestHistogram(fAnalyzerName, Form("xyMiss_%d", iCh), true));
        }
    }
}

void ThreeTrackMCEfficiency::StartOfRunUser() {}

void ThreeTrackMCEfficiency::StartOfBurstUser() {}

void ThreeTrackMCEfficiency::ProcessSpecialTriggerUser(int iEvent, unsigned int trigger_type) {
    (void)iEvent;
    (void)trigger_type;
}

void ThreeTrackMCEfficiency::Process(Int_t) {
    // run first step only in the tree reading mode
    if (!fReadingTree) return;
    // run only on MC
    if (!GetWithMC()) {
      static bool userWarned = false;
      if (!userWarned)
	cout << user_normal() << "Analyzer enabled only for MC, skip event" << endl;
      userWarned = true;
      return;
    }
    auto geomAcc = GeometricAcceptance::GetInstance();
    ReconfigureAnalyzer("SpectrometerVertexBuilder", "Build3TrackVtx", true);
    ReconfigureAnalyzer("SpectrometerVertexBuilder", "Build2TrackVtx", false);

    auto tracks = *static_cast<const vector<DownstreamTrack>*>(
        GetOutput("DownstreamTrackBuilder.Output"));
    auto vertices = *static_cast<const vector<SpectrometerTrackVertex>*>(
        GetOutput("SpectrometerVertexBuilder.Output"));

    Event* mcevt = GetMCEvent();
    KinePart* kaonKine(nullptr);
    vector<KinePart*> tracksKine;
    int npos = 0, nneg = 0, nneut = 0;
    // identify K^+ -> p1^+ p2^+ p3^- + (neutral particles) events
    for (int ikine = 0; ikine < mcevt->GetNKineParts(); ikine++) {
        KinePart *kp = static_cast<KinePart*>(mcevt->GetKineParts()->At(ikine));
        if (kp->GetPDGcode() == 321 && kp->GetParentIndex() == -1) {
            kaonKine = kp;      // store kaon KinePart pointer
            continue;
        }
        if (kp->GetParentIndex() != 0 || kp->GetProdProcessName() != "Decay") {
            continue;           // KinePart not originating from the kaon
        }
        if (kp->GetCharge() == 1) { // positive track from the decay
            npos++;
            tracksKine.push_back(kp);       // store charged track KinePart pointer
        } else if (kp->GetCharge() == -1) { // negative track from the decay
            nneg++;
            tracksKine.push_back(kp);      // store charged track KinePart pointer
        } else if (kp->GetCharge() == 0) { // neutral particle from the decay
            nneut++;
        } else {
            continue;
        }
    }
    // select only events with a kaon decay
    bool kaonDecayed = kaonKine && (kaonKine->GetEndProcessName() == "Decay");
    FillHisto("kaonDecayed", kaonDecayed);
    if (!kaonDecayed) return;

    FillHisto("ZvtxAll", kaonKine->GetEndPos().Z() / 1e3);
    FillHisto("NposTracks", npos);
    FillHisto("NnegTracks", nneg);
    FillHisto("Nneutrals",  nneut);
    if (npos != 2 || nneg != 1) return; // not a 3-track event

    int nInAcc = 0;
    int nAbove10 = 0;
    for (const auto &p : tracksKine) {
        double momentum = 1e-3 * p->GetInitialMomentum().Mag();
        TString hname = "mom" + GetPartName(p->GetPDGcode(), p->GetCharge()).first + "All";
        FillHisto(hname, momentum);
        // require end position after 4th chamber
        bool kineInAcc = (geomAcc->GetZStraw(3) < p->GetEndPos().Z());
        // require true track positions to be in acceptance of all 4 chambers
        for (int iCh = 0; iCh < 4; iCh++) {
            TVector3 truePos = p->GetPosAtZ(geomAcc->GetZStraw(iCh));
            kineInAcc = kineInAcc && geomAcc->InAcceptance(truePos.X(), truePos.Y(),
                                                           kSpectrometer, iCh);
        }
        if (kineInAcc) nInAcc++;
        if (momentum > 10) nAbove10++;
    }
    FillHisto("NtracksInAcc", nInAcc);
    if (nInAcc < 3) return;

    FillHisto("ZvtxAcc", kaonKine->GetEndPos().Z() / 1e3);
    FillHisto("NrecoTracks", tracks.size());
    FillHisto("NrecoVertices", vertices.size());

    int nMatched = 0, nMatched4Ch = 0;
    std::set<Int_t> matchedTrackIDs;
    for (const auto &p : tracksKine) {
        bool kineMatched = false;
        bool kineMatched4Ch = false;
        int missingChamber = -1;
        double momentum = 1e-3 * p->GetMomSpectrometerEntry().Rho();
        TString hname = "mom" + GetPartName(p->GetPDGcode(), p->GetCharge()).first + "Acc";
        FillHisto(hname, momentum);
        // True-Reco matching
        for (auto &t : tracks) {
            if (t.GetCharge() != p->GetCharge())
                continue;       // wrong charge
            if (matchedTrackIDs.find(t.GetTrackID()) != matchedTrackIDs.end())
                continue;       // track already matched

            double momdiff = p->GetMomSpectrometerEntry().Rho() - t.GetMomentum();
            FillHisto("momDiff", momdiff);

            double posdiffMax = 0.; // position difference in 4 Straw chamber z-plances
            for (int iCh = 0; iCh < 4; iCh++) {
                double zref = geomAcc->GetZStraw(iCh);
                double posdiff = sqrt(pow(p->GetPosAtZ(zref).X() - t.xAt(zref), 2) +
                                      pow(p->GetPosAtZ(zref).Y() - t.yAt(zref), 2));
                if (posdiff > posdiffMax) posdiffMax = posdiff;
            }
            FillHisto("posDiff", posdiffMax);

            if (momdiff < 1000 && posdiffMax < 25) {
                kineMatched = true;
                matchedTrackIDs.insert(t.GetTrackID());
                nMatched++;
                FillHisto("NchambersTracks", t.GetNChambers());
                if (t.GetNChambers() == 4) {
                    kineMatched4Ch = true;
                    nMatched4Ch++;
                } else {        // identify missing chamber for 3-chamber tracks
                    for (int chid = 0; chid < 4; chid++)
                        if (t.GetSpectrometerCandidate()->GetChamberId(chid) == -1)
                            missingChamber = chid;
                }
                break;
            }
        }
        TString effname    = "effMomNum" + GetPartName(p->GetPDGcode(), p->GetCharge()).first;
        if (kineMatched) FillHisto(effname, momentum);
        TString effname4Ch = "effMomNum" + GetPartName(p->GetPDGcode(), p->GetCharge()).first + "4Ch";
        if (kineMatched4Ch) FillHisto(effname4Ch, momentum);

        if (!kineMatched) {
            for (int iCh = 0; iCh < 4; iCh++) {
                TVector3 truePos = p->GetPosAtZ(geomAcc->GetZStraw(iCh));
                FillHisto(Form("xyNoMatch_%d", iCh), truePos.X(), truePos.Y());
            }
        } else if (!kineMatched4Ch) {
            for (int iCh = 0; iCh < 4; iCh++) {
                TVector3 truePos = p->GetPosAtZ(geomAcc->GetZStraw(iCh));
                FillHisto(Form("xy3Chambers_%d", iCh), truePos.X(), truePos.Y());
            }
            if (missingChamber >= 0 && missingChamber <=3) {
                TVector3 truePos = p->GetPosAtZ(geomAcc->GetZStraw(missingChamber));
                FillHisto(Form("xyMiss_%d", missingChamber), truePos.X(), truePos.Y());
            }
        }
    }
    FillHisto("eventEfficiencyDenominator", 0);
    FillHisto("eventEfficiencyDenominator", 1);
    if (nMatched == 3) FillHisto("eventEfficiencyNumerator", 0);
    if (nMatched4Ch == 3) FillHisto("eventEfficiencyNumerator", 1);
    if (nAbove10 == 3) {
        FillHisto("eventEfficiencyDenominator", 2);
        FillHisto("eventEfficiencyDenominator", 3);
        if (nMatched == 3) FillHisto("eventEfficiencyNumerator", 2);
        if (nMatched4Ch == 3) FillHisto("eventEfficiencyNumerator", 3);
    }
    FillHisto("NmatchedTracks", nMatched);
    FillHisto("NmatchedTracks4Ch", nMatched4Ch);
}

void ThreeTrackMCEfficiency::PostProcess()
{}

void ThreeTrackMCEfficiency::EndOfBurstUser()
{}

void ThreeTrackMCEfficiency::EndOfRunUser()
{}

void ThreeTrackMCEfficiency::EndOfJobUser()
{
    if (fReadingTree) {
        SaveAllPlots();
    } else {
        gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
        auto can = unique_ptr<TCanvas>(new TCanvas("Canvas", "Canvas", 1200, 800));

        can->Divide(2,2);
        can->cd(1);
        fHkaonDecayed->Draw();
        can->cd(2);
        fHZvtxAll->Draw();
        can->cd(3);
        fHNtracksInAcc->Draw();
        fHNtracksInAcc->GetXaxis()->SetRangeUser(0, 5);
        can->cd(4);
        fHZvtxAcc->Draw();
        can->Print(Form(fOutputPDFFileName + "("), "pdf");

        can->Clear();
        can->SetGrid(1,1);
        TEfficiency effEvent(*fHeffEvent, *fHeffEventDen);
        effEvent.SetName("eventEfficiency");
        effEvent.SetTitle("Full event reconstruction efficiency - all 3 tracks");
        effEvent.Write();
        effEvent.SetLineWidth(2);
        effEvent.SetLineColor(kBlue+2);
        effEvent.Draw();
        gPad->Update();
        effEvent.GetPaintedGraph()->SetMinimum(0.);
        effEvent.GetPaintedGraph()->SetMaximum(1.);
        gPad->Update();
        TLatex l;
        l.SetTextSize(0.025);
        l.SetTextColor(kBlue+2);
        l.DrawLatexNDC(0.18, 0.3, "3/4 Chamber tracks");
        l.DrawLatexNDC(0.35, 0.3, "4 Chamber tracks");
        l.DrawLatexNDC(0.52, 0.3, "#splitline{3/4 Chamber tracks}{all with p>10GeV/c}");
        l.DrawLatexNDC(0.68, 0.3, "#splitline{4 Chamber tracks}{all with p>10GeV/c}");
        l.SetTextSize(0.035);
        l.DrawLatexNDC(0.18, 0.13, Form("#varepsilon = %.3f ^{+%.3f}_{- %.3f}", effEvent.GetEfficiency(1),effEvent.GetEfficiencyErrorUp(1), effEvent.GetEfficiencyErrorLow(1)));
        l.DrawLatexNDC(0.35, 0.13, Form("#varepsilon = %.3f ^{+%.3f}_{- %.3f}", effEvent.GetEfficiency(2),effEvent.GetEfficiencyErrorUp(2), effEvent.GetEfficiencyErrorLow(2)));
        l.DrawLatexNDC(0.52, 0.13, Form("#varepsilon = %.3f ^{+%.3f}_{- %.3f}", effEvent.GetEfficiency(3),effEvent.GetEfficiencyErrorUp(3), effEvent.GetEfficiencyErrorLow(3)));
        l.DrawLatexNDC(0.68, 0.13, Form("#varepsilon = %.3f ^{+%.3f}_{- %.3f}", effEvent.GetEfficiency(4),effEvent.GetEfficiencyErrorUp(4), effEvent.GetEfficiencyErrorLow(4)));

        can->Print(fOutputPDFFileName, "pdf");

        for (const auto &ch : fCharges) {
            for (const auto &pdgid : fPDGids) {
                Int_t id = pdgid * ch; // revert PDG id for negative tracks
                if (fHmomAll[make_pair(id, ch)]->GetEntries() == 0) continue; // skip empty histograms
                can->Clear();
                can->SetGrid(1,1);
                fHmomAll[make_pair(id, ch)]->SetLineColor(kBlue+2);
                fHmomAll[make_pair(id, ch)]->Draw();
                gPad->Update();
                auto stAll = (TPaveStats*)fHmomAll[make_pair(id, ch)]->FindObject("stats");
                if (stAll) {
                    stAll->SetX1NDC(0.75);
                    stAll->SetX2NDC(0.98);
                    stAll->SetY1NDC(0.82);
                    stAll->SetY2NDC(0.98);
                } else {
                    cerr << "Statbox stAll not found" << endl;
                }
                fHmomAcc[make_pair(id, ch)]->SetLineColor(kRed+2);
                fHmomAcc[make_pair(id, ch)]->Draw("sames");
                gPad->Update();
                auto stAcc = (TPaveStats*)fHmomAcc[make_pair(id, ch)]->FindObject("stats");
                if (stAcc) {
                    stAcc->SetX1NDC(0.75);
                    stAcc->SetX2NDC(0.98);
                    stAcc->SetY1NDC(0.66);
                    stAcc->SetY2NDC(0.82);
                } else {
                    cerr << "Statbox stAcc not found" << endl;
                }
                TLegend legMom(0.75, 0.45, 0.98, 0.66);
                legMom.AddEntry(fHmomAll[make_pair(id, ch)], "#splitline{All MC events}{with a kaon decay}");
                legMom.AddEntry(fHmomAcc[make_pair(id, ch)], "#splitline{Events with all tracks}{in Straw acceptance}");
                legMom.Draw("same");
                can->Print(fOutputPDFFileName, "pdf");

                can->Clear();
                can->SetGrid(1,1);
                TEfficiency effMom(*fHeffMom[make_pair(id, ch)], *fHmomAcc[make_pair(id, ch)]);
                effMom.SetName("trackEfficiency_"+GetPartName(id, ch).first);
                effMom.SetTitle("Track reconstruction efficiency for " + GetPartName(id, ch).second + " tracks; p[GeV]");
                effMom.Write();
                TEfficiency effMom4Ch(*fHeffMom4Ch[make_pair(id, ch)], *fHmomAcc[make_pair(id, ch)]);
                effMom4Ch.SetName("trackEfficiency_"+GetPartName(id, ch).first + "4Ch");
                effMom4Ch.SetTitle("Track reconstruction efficiency for " + GetPartName(id, ch).second + " tracks - 4 Chambers; p[GeV]");
                effMom4Ch.Write();
                effMom.SetMarkerColor(kBlue+2);
                effMom.SetLineColor(kBlue+2);
                effMom.Draw();
                effMom4Ch.SetMarkerColor(kRed+2);
                effMom4Ch.SetLineColor(kRed+2);
                effMom4Ch.Draw("same");
                TLegend legEff(0.3, 0.11, 0.7, 0.25);
                legEff.AddEntry(&effMom, "Track reco efficiency");
                legEff.AddEntry(&effMom4Ch, "Track reco efficiency - 4 chambers");
                legEff.Draw("same");
                can->Print(fOutputPDFFileName, "pdf");
            }
        }

        can->Clear();
        can->Divide(2,2);
        for (int iCh = 0; iCh < 4; iCh++) {
            can->cd(iCh+1);
            fHxyNoMatch[iCh]->Rebin2D(10,10);
            fHxyNoMatch[iCh]->Draw("colz");
        }
        can->Print(fOutputPDFFileName, "pdf");

        can->Clear();
        can->Divide(2,2);
        for (int iCh = 0; iCh < 4; iCh++) {
            can->cd(iCh+1);
            fHxy3Chambers[iCh]->Rebin2D(10,10);
            fHxy3Chambers[iCh]->Draw("colz");
        }
        can->Print(fOutputPDFFileName, "pdf");

        can->Clear();
        can->Divide(2,2);
        for (int iCh = 0; iCh < 4; iCh++) {
            can->cd(iCh+1);
            fHxyMiss[iCh]->Rebin2D(10,10);
            fHxyMiss[iCh]->Draw("colz");
        }
        can->Print(fOutputPDFFileName + ")", "pdf");

        gErrorIgnoreLevel = -1; // restore the default
    }
}

void ThreeTrackMCEfficiency::DrawPlot()
{}

ThreeTrackMCEfficiency::~ThreeTrackMCEfficiency()
{}

pair<TString, TString> ThreeTrackMCEfficiency::GetPartName(Int_t pdgID, Int_t charge) const
{
    switch (pdgID) {
    case 211:
        return make_pair("PosPion", "#pi^{+}");
    case -211:
        return make_pair("NegPion", "#pi^{-}");
    case -13:
        return make_pair("PosMuon", "#mu^{+}");
    case 13:
        return make_pair("NegMuon", "#mu^{-}");
    case -11:
        return make_pair("PosElectron", "e^{+}");
    case 11:
        return make_pair("NegElectron", "e^{-}");
    default:
        if (charge > 0)
            return make_pair("Pos", "positive");
        else if (charge < 0)
            return make_pair("Neg", "negative");
        else
            return make_pair("","");
        break;
    }
}
