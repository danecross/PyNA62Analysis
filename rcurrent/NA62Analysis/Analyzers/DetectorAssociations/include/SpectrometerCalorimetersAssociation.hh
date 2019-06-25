#ifndef SPECTROMETERCALORIMETERSASSOCIATION_HH
#define SPECTROMETERCALORIMETERSASSOCIATION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include "TF1.h"
#include "TClonesArray.h"
#include "CalorimeterCluster.hh"
#include "MUV12Corrections.hh"
#include "TMVA/Reader.h"
#include <sstream>      // std::stringstream

class TH1I;
class TH2F;
class TGraph;
class TTree;

class TRecoMUV1Hit;
class TRecoMUV2Hit;

class SpectrometerCalorimetersAssociation : public NA62Analysis::Analyzer
{
	public:
		explicit SpectrometerCalorimetersAssociation(NA62Analysis::Core::BaseAnalysis *ba);
		~SpectrometerCalorimetersAssociation();
		void InitHist();
		void InitOutput();
		void DefineMCSimple();
    void PreProcess();
		void Process(Int_t);
		void StartOfBurstUser();
		void EndOfBurstUser();
		void StartOfRunUser();
		void EndOfJobUser();
		void PostProcess();
		void DrawPlot();


  protected:
    void MatchLKrCandidate  (double TrackTime, TVector2 PosAtLKr, CalorimeterCluster *RecoCluster);
    void ReconstructLKrCandidate  (double TrackTime, TVector2 PosAtLKr, CalorimeterCluster *RecoCluster);
    void ReconstructMUV1Candidate (double TrackTime, TVector2 PosAtMUV1, CalorimeterCluster *RecoCluster, bool dry=false);
    void ReconstructMUV2Candidate (double TrackTime, TVector2 PosAtMUV2, CalorimeterCluster *RecoCluster);

  protected:
    double LKrCellEnergyFraction (double ClusterEnergy, double Distance);
    void LKrProfInit ();


  private:
    TClonesArray *fLKrCandidates;
    TClonesArray *fMUV1Candidates;
    TClonesArray *fMUV2Candidates;
    TClonesArray *fReconstructedCluster;

    double MUV1ScintillatorPosition[44], MUV2ScintillatorPosition[22];
    double MUV1ChannelEnergy[4][44], MUV2ChannelEnergy[4][22], HACChannelEnergy[2][22];
    double MUV1ChannelTime[4][44], MUV2ChannelTime[4][22];
    bool MUV1IsLongScintillator[4][44];

  private:
    MUV12Corrections *fMUV12CorrHandler;

    static const int fLKrProfNbins=121;
    static const int fLKrProfNEbins=6;

    double fLKrProf[fLKrProfNEbins][fLKrProfNbins];
    double fLKrProfEbin[fLKrProfNEbins];
    const double fLKrProfDx=25./120.;

    std::vector <TRecoLKrHit*> LKrMatchingHits;
    std::vector <TRecoMUV1Hit*> MUV1MatchingHits_H, MUV1MatchingHits_V;
    std::vector <TRecoMUV2Hit*> MUV2MatchingHits_H, MUV2MatchingHits_V;

    TMVA::Reader *fTMVAReader;
    TMVA::Reader *fTMVAReaderNew;
    float fTMVAVariables[16], fNewTMVAVariables[12];
    bool fUseLKrStandard;
    bool fUseLKrRefTime;

    bool fDebug;
    vector<string> fDebugStream;
    std::stringstream thisstream;

};
#endif
