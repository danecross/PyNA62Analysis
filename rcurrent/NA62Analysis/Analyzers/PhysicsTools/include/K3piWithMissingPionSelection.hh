#ifndef K3PIWITHMISSINGPIONSELECTION_HH
#define K3PIWITHMISSINGPIONSELECTION_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include "DownstreamTrack.hh"
#include "LAVMatching.hh"
#include "SAVMatching.hh"
#include "K3piWithMissingPionInfo.hh"

class TH1I;
class TH2F;
class TGraph;
class TTree;

class K3piWithMissingPionSelection : public NA62Analysis::Analyzer {
public:

    struct GoodTrack {
        Int_t fDownstreamTrackId;
        Int_t fChodId;
    };

    struct MissingPion {

        void operator=(std::pair<GoodTrack, GoodTrack> &trackPair) {
            fDownstreamTrackId = make_pair(trackPair.first.fDownstreamTrackId, 
                    trackPair.second.fDownstreamTrackId);
            fChodId = make_pair(trackPair.first.fChodId, trackPair.second.fChodId);
        }

        std::pair<Int_t, Int_t> fDownstreamTrackId; ///< IDs of the two detected pions as DownstreamTrack objects 
        std::pair<Int_t, Int_t> fChodId; ///< IDs of the CHODAssociationRecords for the two DownstreamTrack objects
        Int_t fKtagId; ///< ID of the TRecoCedarCandidate in time with the pair
        Int_t fGtkId; ///< ID of the TRecoGigaTrackerCandidate matched with the pair

        Double_t fPairTime; ///< Average of the times of CHOD candidates matched with each track
        Double_t fKaonTime; ///< Time of the TRecoCedarCandidate matched with the pair
        std::pair<TLorentzVector, TLorentzVector> fTrackFourMomenta; ///< four momenta of the two tracks in the Pi+ Pi- hypotesys
        TLorentzVector fKaonMomentum; ///< Matched GTK candidate four momentum
        TLorentzVector fMissingPionMomentum; ///< Difference between kaon 4-momentum and tracks 4-momenta
        TVector3 fPairKaonVertex; ///< Position of the common vertex of the kaon and the two tracks. Starting point for the missing pion
    };

    explicit K3piWithMissingPionSelection(NA62Analysis::Core::BaseAnalysis *ba);
    ~K3piWithMissingPionSelection();
    void InitHist();
    void InitOutput();
    void DefineMCSimple();
    void ProcessSpecialTriggerUser(int iEvent, unsigned int triggerType);
    void Process(int iEvent);
    void PostProcess();
    void StartOfBurstUser();
    void EndOfBurstUser();
    void StartOfRunUser();
    void EndOfRunUser();
    void EndOfJobUser();
    void DrawPlot();

public:
    std::vector<std::pair<GoodTrack, GoodTrack> > GetTrackPairs();
    std::vector<GoodTrack> GetGoodTracks();
    void GetMissingPion(std::vector<std::pair<GoodTrack, GoodTrack> > &pairs);
    Int_t MatchKaon(MissingPion &missPi);
    Int_t MatchCedarCandidate(MissingPion &missPi);
    Int_t MatchGtkCandidate(MissingPion &missPi);
    Int_t PhotonVeto(const Double_t &timeRef);

    TVector3 GetMultiTrackVertex(std::vector<TVector3>& positions, std::vector<TVector3>& momenta, Double_t *cda);
protected:

private:
    //Pointers to Reco events and usefull analyzers
    TRecoCedarEvent *fCedarEvent;
    TRecoGigaTrackerEvent *fGigaTrackerEvent;
    TRecoCHODEvent *fChodEvent;
    TRecoLAVEvent *fLavEvent;
    TRecoSACEvent *fSacEvent;
    TRecoIRCEvent *fIrcEvent;
    
    LAVMatching *fLavMatching; //photon veto
    SAVMatching *fSavMatching; //photon veto
    
    //Input data
    std::vector<DownstreamTrack> fDownstreamTracks;
    
    //Output data
    Bool_t fEventSelected;
    std::vector<MissingPion> fMissingPions;
    std::vector<K3piWithMissingPionInfo> fMissingPionsOutput;

    //Flag that determines type of data on which the analyzer is currently running
    Bool_t fReadingData;
    
    //Parameters used for cuts
    Double_t fPiPlusMass; //In MeV/c^2
    Double_t fKaonMass; //In MeV/c^2
    Double_t fCutPiPlusMassLow; //In MeV/c^2
    Double_t fCutPiPlusMassHigh;// In MeV/c^2
    
    Int_t fCutNStrawChambers;
    Double_t fCutTrackChi2;
    Double_t fCutTrackChodMinDiscr;
    Double_t fCutRichPionLH;
    Double_t fCutTrackMuvMaxTimeDiff;
    
    Double_t fCutPairMinTimeDiff;
    Double_t fCutPairMaxTimeDiff;
    Double_t fCutPairCda;
    Double_t fCutPairMinZVtx;
    Double_t fCutPairMaxZVtx;
    
    Double_t fCutPairCedarMinTimeDiff;
    Double_t fCutPairCedarMaxTimeDiff;
    
    Double_t fCutPairGtkMatchMinTimeDiff;
    Double_t fCutPairGtkMatchMaxTimeDiff;
    Double_t fCutPairGtkMatchCda;
    Double_t fCutPairGtkMatchMinZVtx;
    Double_t fCutPairGtkMatchMaxZVtx;
    
    Double_t fCutPhotonVetoLavMinTimeDiff;
    Double_t fCutPhotonVetoLavMaxTimeDiff;
    Double_t fCutPhotonVetoSacMinTimeDiff;
    Double_t fCutPhotonVetoSacMaxTimeDiff;
    Double_t fCutPhotonVetoIrcMinTimeDiff;
    Double_t fCutPhotonVetoIrcMaxTimeDiff;
};
#endif
