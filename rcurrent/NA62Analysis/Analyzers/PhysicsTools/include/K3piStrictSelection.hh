#ifndef K3PISTRICTSELECTION_HH
#define K3PISTRICTSELECTION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include "SpectrometerTrackVertex.hh"
#include "DownstreamTrack.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class K3piStrictSelection : public NA62Analysis::Analyzer {	
  public:
    explicit K3piStrictSelection(NA62Analysis::Core::BaseAnalysis *ba);
    ~K3piStrictSelection();
    void InitHist();
    void InitOutput();
    void DefineMCSimple();
    void Process(int);
    void StartOfBurstUser();
    void EndOfBurstUser();
    void StartOfRunUser();
    void EndOfRunUser();
    void EndOfJobUser();
    void PostProcess();
    void DrawPlot() {};

  private:

    // event info
    Double_t fVertexTime;
    Int_t fVtxIndex;
    Double_t fTriggerTime;
    UInt_t fNSTRAWTracks;
    UInt_t fNVertices;
    TVector3 fBeamThreeMomt;
    std::array<Int_t, 3> fTrackID;
    std::array<Int_t, 3> fTrackCharge;
    std::array<Double_t, 3> fTrackTime;
    std::array<TVector3, 3> fTrackThreeMomt;
   
    // outputs
    Bool_t fEventSelected;
    Double_t fKaonMass;
    Int_t fChosenVtxIndex;
    Double_t fChosenVertexTime;
    std::array<Int_t, 3> fChosenTrackID;
    std::array<Double_t, 3> fChosenTrackTime;
    TVector3 fVertexPos;
    TVector3 fKaonMomt;
    TVector3 fPosPionLowMomt_3Momt;
    TVector3 fPosPionHighMomt_3Momt;
    TVector3 fNegPion_3Momt;
    Double_t fPosPionLowMomt_Time;
    Double_t fPosPionHighMomt_Time;
    Double_t fNegPion_Time;
    Int_t fPosPionLowMomt_ID;
    Int_t fPosPionHighMomt_ID;
    Int_t fNegPion_ID;

    // cut parameters
    Double_t fCutKaonMass;
    Double_t fCutTrackLowMomt;
    Double_t fCutTrackHighMomt;
    Double_t fCutTrackChi2;
    Double_t fCutTrackAfterBeforeFitMomDiff;
    Double_t fCutVertexChi2;
    Double_t fCutZVertexMin;
    Double_t fCutZVertexMax;
    Double_t fCutTotalVertexMomtWrtBeam;
    Double_t fCutVertexPt2;
    Int_t    fCutNTracksTimeCorr;
    Double_t fCutVertexTriggerTime;
    Double_t fCutVertexKTAGTime;
    Double_t fCutTriggerKTAGTime;
    Double_t fCutTrackVertexTime;
    Double_t fCutEoPMaximal;
    
    // Vertexing tool and Downstream track builder outputs
    std::vector<SpectrometerTrackVertex> fVertices;
    std::vector<DownstreamTrack> fDownTrack;

    // user functions
    Bool_t tracksPassCuts();
    Bool_t atLeastOneGoodKTAG();
};
#endif
