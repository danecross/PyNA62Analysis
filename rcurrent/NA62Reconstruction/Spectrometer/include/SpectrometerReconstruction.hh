#ifndef SpectrometerReconstruction_H
#define SpectrometerReconstruction_H 1

#include "TROOT.h"
#include "TMinuit.h"
#include "TH2F.h"
#include "TFile.h"
#include <iostream>

#include "NA62VReconstruction.hh"
#include "Event.hh"
#include "SRBEvent.hh"
#include "TSpectrometerEvent.hh"
#include "TRecoCHODEvent.hh"
#include "TRecoSpectrometerEvent.hh"
#include "TRecoSpectrometerCandidate.hh"

#include "SpectrometerParameters.hh"
#include "SpectrometerGeometry.hh"
#include "SpectrometerDigiManager.hh"
#include "ChamberHitCollector.hh"
#include "TrackCollector.hh"

class SpectrometerReconstruction : public NA62VReconstruction {
public:
    SpectrometerReconstruction(TFile*, TString);
    ~SpectrometerReconstruction();

    virtual TRecoVEvent * ProcessEvent(TDetectorVEvent*, Event*);
    virtual TDetectorVEvent* Trigger(TDetectorVEvent*, Event*);
    virtual void EndProcessing();
    virtual void FillTimes(Double_t);

    virtual void Init(NA62VReconstruction*);
    virtual void StartOfBurst();
    virtual void EndOfBurst();

    TrackCollector* GetTrackCollector() {
        /// \MemberDescr
        /// \return SpectrometerReconstruction::fTrackCollector.
        ///
        /// \EndMemberDescr
        return fTrackCollector;
    };
    ChamberHitCollector *GetChamber(Int_t jChamber) {
        /// \MemberDescr
        /// \param jChamber Id of the chamber.
        /// \return The element jChamber of SpectrometerReconstruction::fChamberHitCollector.
        ///
        /// \EndMemberDescr
        return fChamberHitCollector.at(jChamber);
    };
    Int_t GetNTracks() {
        /// \MemberDescr
        /// \return The total number of reconstructed tracks.
        ///
        /// \EndMemberDescr
        return fRecoEvent->GetNCandidates();
    };
    TRecoSpectrometerCandidate *GetCandidate(Int_t j) {
        /// \MemberDescr=
        /// \param j Id of the track candidate.
        /// \return The pointer to the track candidate j of the class TRecoSpectrometerCandidate.
        ///
        /// \EndMemberDescr
        return static_cast<TRecoSpectrometerCandidate *>(fRecoEvent->GetCandidates()->At(j));
    };
    TH1F* GetHPic_momentum_fit()                { return fHPic_momentum_fit;            };
    TH1F* GetHPic_mmiss_fit_4Chambers()         { return fHPic_mmiss_fit_4Chambers;     };
    TH2F* GetHIllum4Chambers_ch1()              { return fHIllum4Chambers[0];           };
    TH2F* GetHIllum4Chambers_ch4()              { return fHIllum4Chambers[3];           };
    TH2F* GetHMissMassTime()                    { return fHMissMassTime;                };
    TH1F* GetHRecoHitWireSum2()                 { return fRecoHitWireSum2;              };
    TH1F* GetHStep()                            { return fHStep;                        };
    void  SetHStep(TH1F * value)                { fHStep = value;                       };
    TH1F* GetHNIonPairs()                       { return fHNIonPairs;                   };
    void  SetHNIonPairs(TH1F * value)           { fHNIonPairs = value;                  };
    TH2F* GetHMaxClusterDelay()                 { return fHMaxClusterDelay;             };
    void  SetHMaxClusterDelay(TH2F * value)     { fHMaxClusterDelay = value;            };
    SpectrometerDigiManager* GetDigiManager()   { return fSpectrometerDigiManager;      };
    static int GetRebinFactor_DigiTimeRaw()     { return kRebinFactorDigiTimeRaw;       };
    void  SetRecoCHODEvent(TRecoCHODEvent* val) { fRecoCHODEvent = val;                 };

private:
    SpectrometerDigiManager *fSpectrometerDigiManager; ///< Pointer to SpectrometerDigiManager
    SpectrometerParameters *fPar;       ///< Pointer to SpectrometerParameters instance.
    SpectrometerGeometry *fGeo;         ///< Pointer to SpectrometerGeometry instance.
    TRecoCHODEvent * fRecoCHODEvent;    ///< Pointer to the reconstructed CHOD event.
    std::vector<ChamberHitCollector *> fChamberHitCollector; ///< Vector of the pointers to the ensambles of reconstructed chamber-hits.
    TrackCollector* fTrackCollector; ///< Pointer to the collection of reconstructed tracks.

    Int_t fNEvent;    ///< Number of events processed.
    Int_t fNChambers; ///< Number of chambers in the detector.
    Int_t fNViews;    ///< Number of views per chamber.
    Int_t fNPlanes;   ///< Number of planes per view.
    UInt_t fNCHODHitsInTime;
    static constexpr int kRebinFactorDigiTimeRaw = 8;

    void ParseConfFile(TString s);
    void SeparateHitPerViewPlane();
    Bool_t ReconstructHitPerView();
    Double_t ReferenceTime();

    // Histogram functions
    void InitHistograms();
    void MakeHistograms(SRBEvent *TdcEvent);
    void SaveHistograms();
    // Histograms
    TH2F *fDetectedEdge;
    TH2F *fDetectedEdge2;

    TH2F *fTrailingVSDrift;
    TH2F *fTrailingVSDrift2;
    TH1F *fOverflowHisto;
    TH2F *fNGoodHitPerPlane;
    TH2F *fNGoodHitPerEvent;
    TH2F *fRecoChamberHitTotal[4];
    TH2F *fRecoChamberHit4Total[4];
    TH1F *fRecoChamberHitQuality4[4];
    TH2F *fRecoChamberHit3Total[4];
    TH1F *fRecoChamberHitQuality3[4];
    TH2F *fRecoChamberHit2Total[4];
    TH1F *fRecoChamberHitTime;
    TH1F *fRecoChamberDHitTime;
    TH1F *fRecoHitWireSum2;
    TH2F *fRecoHitWireSum22;
    TH2F *fRecoHitWireSum23;
    TH2F *fRecoHitWireSum24;
    TH2F *fRecoHitWireSum25;
    TH2F *fHSlope;
    TH2F *fRecoHitWireSum2VsMagicT0;
    TH2F *fRecoHitWireSum2VsROMezzanine;
    TH2F *fViewClusterQuality;
    TH1F *fViewClusterDT;
    TH1F *fViewCluster3TT;

    TH2F *fHCombQuality;
    TH2F *fHCombHough;
    TH1F *fHPic_mmiss;
    TH1F *fHPic_mmiss_le;
    TH1F *fHPic_mmiss_zoom;
    TH1F *fHPic_momentum;
    TH2F *fHPvsVertex;
    TH1F *fHPic_mmiss2;
    TH1F *fHPic_momentum_fit;
    TH2F *fHPic_momentum_fit_trig;
    TH1F *fHPic_mmiss_fit;
    TH1F *fHPic_mmiss_fit_4Chambers;
    TH2F *fHPic_mmiss_fit_4Chambers_trig;
    TH2F *fHPic_mmiss_fit_4Chambers_chi2;
    TH2F *fHPic_mmiss_fit_4Chambers_pmom;
    TH2F *fHPic_mmiss_fit_4Chambers_pmom_trig[8];
    TH2F *fHMissMassTime;
    TH2F *fHIllumTotal[4];
    TH2F *fHIllum4Chambers[4];
    TH2F *fHIllum3Chambers[4];
    TH2F *fHIllum123[4];
    TH2F *fHIllum023[4];
    TH2F *fHIllum013[4];
    TH2F *fHIllum012[4];

    TH1F* fHStep;
    TH1F* fHNIonPairs;
    TH2F* fHMaxClusterDelay;
};


#endif
