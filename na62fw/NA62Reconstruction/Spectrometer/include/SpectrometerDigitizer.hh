#ifndef SpectrometerDigitizer_H
#define SpectrometerDigitizer_H 1

#include "SRBEvent.hh"
#include "NA62VDigitizer.hh"
#include "StrawResponse.hh"
#include "TSpectrometerHit.hh"

#define PARAMETRIZEDDIGI

class SpectrometerDigitizer : public NA62VDigitizer {

  public:
    explicit SpectrometerDigitizer(NA62VReconstruction*);
    virtual ~SpectrometerDigitizer();
    virtual TDetectorVEvent * ProcessEvent(TDetectorVEvent *);
    virtual void StartOfBurst();
    virtual void EndOfBurst();
#ifndef PARAMETRIZEDDIGI
    void AddIonizationClusters(TSpectrometerHit*);
    Int_t Discriminate(Int_t, Float_t, Float_t);
    void AddNoise(Float_t, Float_t, Int_t);
    Double_t CariocaTransferFunction(Double_t t);
    Double_t StrawSignal(Double_t t, Double_t t0);
    Float_t Diffusion(Float_t Distance);
    Float_t DriftTime(Float_t Distance);
#endif

  private:
    Double_t* fChambersMCToF;
    Double_t* fViewsMCToF;
    Double_t* fPlanesMCToF;
#ifdef PARAMETRIZEDDIGI
    StrawResponse * Response;
#else
    Float_t fMaxTime;
    Float_t fTimeStep;
    Float_t fStrawT0;
    Float_t *fSingleCluster;
    Float_t *fSignalPre;
    Float_t *fSignalPost;
    Float_t *fSignal;
    Float_t fNClustersPermm;
    Float_t fDiscrSetTime;
    Float_t fEdgeDeadTime;
    Float_t fSameEdgeDeadTime;
    Float_t fThreshold;
    Float_t fGain;
    Float_t fCARIOCASlope; 
    Float_t fEquivalentNoiseCharge; 
    Float_t fIonizationEnergy;
    Float_t fNTotalPermm;
    Bool_t fNoiseSimu;
    Bool_t fSavePulseShapes;
    Bool_t fNoBulkHits;
    Float_t * fNoise;
    SRBEvent * fFakeDigiEvent;
#endif

};

#endif
