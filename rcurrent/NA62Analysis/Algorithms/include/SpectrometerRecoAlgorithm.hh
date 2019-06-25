#ifndef SPECTROMETERRECOALGORITHM_H
#define SPECTROMETERRECOALGORITHM_H

#include <memory>
#include <string>

#include "Algorithm.hh"
#include "TRecoSpectrometerEvent.hh"
#include "SpectrometerReconstruction.hh"
#include "SpectrometerGeometry.hh"
#include "SpectrometerParameters.hh"
#include "ChamberHitCollector.hh"
#include "TrackCollector.hh"

class SpectrometerRecoAlgorithm : public Algorithm {
public:
  SpectrometerRecoAlgorithm(BaseAnalysis *ba, Analyzer* ana,
                            const std::string &name = "SpectrometerRecoAlgorithm");
  virtual ~SpectrometerRecoAlgorithm();
  void Init();
  void Process(TRecoSpectrometerEvent*, Double_t refTime = -999.);
  void  SetConfFileName(TString value) {fConfFileName = value;}
  TString GetConfFileName() const {return fConfFileName;}
  void  SetUseCustomChamberCollector(Bool_t value) {fUseCustomChamberCollector = value;};
  Bool_t  GetUseCustomChamberCollector() {return fUseCustomChamberCollector;};
  void  SetUseCustomViewCollector(Bool_t value) {fUseCustomViewCollector = value;};
  Bool_t  GetUseCustomViewCollector() {return fUseCustomViewCollector;};
  void  SetUseCustomTrackCollector(Bool_t value) {fUseCustomTrackCollector = value;};
  Bool_t  GetUseCustomTrackCollector() {return fUseCustomTrackCollector;};
  void  SetApplyAlphaBetaCorrections(Bool_t value) {fApplyAlphaBetaCorrections = value;};
  Bool_t  GetApplyAlphaBetaCorrections() {return fApplyAlphaBetaCorrections;};
  void  SetDebugMode(Bool_t value) {fDebugMode = value;};
  Bool_t  GetDebugMode() {return fDebugMode;};

  void SetUpdateWireDistance(Bool_t value) {fUpdateWireDistance=value;};
  Bool_t GetUpdateWireDistance() {return fUpdateWireDistance;};

private:
  Bool_t SeparateHitPerViewPlane(TRecoSpectrometerEvent *event);
  void PrintRecoCandidates(TClonesArray *candidates);

  TString fConfFileName;        ///< Path to Spectrometer config file
  SpectrometerParameters* fPar;
  SpectrometerGeometry* fGeo;
  Bool_t fIsInitialized;
  Int_t fRunID;
  Int_t fBurstID;
  Bool_t fIsMC;

  // Spectrometer reconstruction flags
  Bool_t fUpdateWireDistance;   ///< Recompute wire distances for all hits using new reference time
  Bool_t fUseCustomChamberCollector; ///< Use custom chamber hit collector class
  Bool_t fUseCustomViewCollector; ///< Use custom view hit collector class
  Bool_t fUseCustomTrackCollector; ///< Use custom track collector class
  Bool_t fApplyAlphaBetaCorrections; ///< Apply alpha/beta corrections to reconstructed tracks

  // variables for debugging
  Bool_t fDebugMode;
  TClonesArray *fRecoCandidatesCopy;

  std::vector<ChamberHitCollector*> fChamberHitCollector; ///< Vector of the pointers to the ensambles of reconstructed chamber-hits.
  std::unique_ptr<TrackCollector> fTrackCollector; ///< Pointer to the collection of reconstructed tracks.
};

class ChamberHitCollectorCustom : public ChamberHitCollector
{
public:
    ChamberHitCollectorCustom(Int_t iChamber, Bool_t useCustomViewCollector);
    virtual ~ChamberHitCollectorCustom() {};
    void ReconstructHit();
};

class ViewHitCollectorCustom : public ViewHitCollector
{
public:
    ViewHitCollectorCustom(Int_t ichamber, Int_t iview);
    virtual ~ViewHitCollectorCustom() {};
    void ReconstructHitPositions(TRecoSpectrometerEvent *event);
};

class TrackCollectorCustom : public TrackCollector
{
public:
	explicit TrackCollectorCustom(std::vector<ChamberHitCollector *> *c) : TrackCollector(c) {};
    virtual ~TrackCollectorCustom() {};
    void Reconstruct(TRecoSpectrometerEvent *);
};



#endif /* SPECTROMETERRECOALGORITHM_H */
