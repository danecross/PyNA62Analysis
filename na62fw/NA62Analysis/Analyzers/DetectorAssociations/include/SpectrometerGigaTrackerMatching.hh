// ----------------------------------------------------------------------
// //
// // History:
// //
// // Created by Joel Swallow (joel.christopher.swallow@cern.ch) 24/03/17
// // Modified by Joel Swallow 21/06/17
// // Modified by Joel Swallow 14/09/17
// // ----------------------------------------------------------------------

#ifndef SPECTROMETERGIGATRACKERMATCHING_HH
#define SPECTROMETERGIGATRACKERMATCHING_HH

#include <stdlib.h>
#include <vector>
#include "Analyzer.hh"
#include <TCanvas.h>
#include "UserMethods.hh"
#include "TRecoGigaTrackerEvent.hh"
#include "TRecoSpectrometerCandidate.hh"
#include "DownstreamTrack.hh"
#include "SpectrometerGigaTrackerMatchingOutput.hh"

class SpectrometerGigaTrackerMatching : public NA62Analysis::Analyzer{

  public:

    explicit SpectrometerGigaTrackerMatching(NA62Analysis::Core::BaseAnalysis *ba); //constructor
    virtual ~SpectrometerGigaTrackerMatching();	//destructor

    //standard analyzer methods................
    void InitHist();
    void InitOutput();
    void DefineMCSimple();
    void Process(int iEvent);
    void StartOfBurstUser();
    void EndOfBurstUser();
    void StartOfRunUser();
    void EndOfRunUser();
    void PostProcess();
    void DrawPlot();

    //Other Methods
    void ApplyTimeCorrections();
    void DiscriminantNormalization();
    void Clear();
    Bool_t BuildTrack(int nid, TRecoGigaTrackerEvent*);
    Bool_t BuildBackgroundTrack(int nidBack, TRecoGigaTrackerEvent*);
    void CorrectHitTime(TRecoGigaTrackerHit*);
    void LinearLeastSquareFit(Double_t *x, Double_t *y, Int_t Nsample, Double_t *sigma, Double_t &a, Double_t &b, Double_t &rho, Double_t &chi2);

    //Dmatch method for correction of hit positions
    void GTKStationXYPosCorrection(TRecoGigaTrackerCandidate*);

    //get and set ref time
    Double_t GetRefTime() {return fRefTime; }
    void SetRefTime(double time) {fRefTime=time;}

    void ResetOutputs(){
      fVertexPosition.clear();	
      fBeamParticleMomentum.clear();
      fCorrectedBeamParticleMomentum.clear();
      fTrackMomentum.clear();
      fCorrectedTrackMomentum.clear();
      fcda.clear();
      fMDDiscriminant.clear();
    }

    void SetK3piEventVariables();

    //PDF functions................................................................
    Double_t CDASignalPDF(Double_t CDA);
    Double_t CDABackgroundPDF(Double_t CDA);
    Double_t dtSignalPDF(Double_t dt);
    Double_t dtBackgroundPDF(Double_t dt);	

    Double_t FulldtCDASignalPDFIntegral();
    Double_t FulldtCDABackgroundPDFIntegral();

    Double_t CDASignalPDF_new(Double_t CDA);
    Double_t CDABackgroundPDF_new(Double_t CDA);
    Double_t dtSignalPDF_new(Double_t dt);
    Double_t dtBackgroundPDF_new(Double_t dt);	

    Double_t dt_KPi_SignalPDF(Double_t dt);
    Double_t dt_KPi_BackgroundPDF(Double_t dt);

  private:

    TRecoGigaTrackerEvent* fGTKevt; ///<Pointer to TRecoGigaTrackerEvent (see https://na62-sw.web.cern.ch/sites/na62-sw.web.cern.ch/files/doxygen/d5/d1c/classTRecoGigaTrackerEvent.html)

    //Reference time to be used in reconstruction
    Double_t fRefTime;

    Double_t fGTKOffset;
    Double_t fZGTK[3];

    Int_t fReferenceDetector;

    Double_t fTimeWindow;
    Double_t fTimeWindowTrigger;
    Double_t fXWindow;
    Double_t fYWindow;
    Double_t fDt;	
    Double_t fChi2X;
    Double_t fChi2T;
    Bool_t fRefineTimeStation;
    Bool_t fRefineTimeChip;
    Bool_t fRemoveHit;

    //Save variables for efficiency calculation
    Int_t fNRecoCand;	///<count total number of reconstructed candidates
    Int_t fNRecoNoCand;	///<count the number of times 0 candidates are reconstructed
    Int_t fNRecoMoreThanOne;     ///<count the number of times more than 1 GTK candidates are reconstructed
    Double_t fEfficiency;	///<Calculate the efficiency based on current prediections

    Int_t fNK3piCandidates;
    Int_t fNCandidateEvents;

    Int_t fNK3piCandidatesNotPassingDiscCut;
    Int_t fNK3piCandidatesPassingDiscCut;

    //GTK XY offsets:
    Double_t fOffset_X_GTK[3];
    Double_t fOffset_Y_GTK[3];

    //Matching results
    Int_t fDmatchMatch_NMatch[3];
    Int_t fDmatchMatch_NMismatch[3];
    Int_t fDmatchMatch_NNoMatch[3];

    //Linear Matching Discriminant matching 
    Int_t fMDMatch_NMatch[3];
    Int_t fMDMatch_NMismatch[3];
    Int_t fMDMatch_NNoMatch[3];

    //Standard Dmatch matching using D<20, but no restriction on Dmatch
    Int_t fDmatchMatchUNR_NMatch[3];
    Int_t fDmatchMatchUNR_NMismatch[3];
    Int_t fDmatchMatchUNR_NNoMatch[3];

    Int_t fNoCandidate;

    Int_t fMatchingMode; //Variable to determine choice of 

    //Dmatch procedure
    std::vector<Int_t> fMatchedpiIndex;
    std::vector<Int_t> fMatchedKIndex;
    std::vector<Double_t> fMatchingQuality;

    //MD procedure
    std::vector<Int_t> fMatchedpiIndex_MD;
    std::vector<Int_t> fMatchedKIndex_MD;
    std::vector<Double_t> fMatchingQuality_MD;

    //Dmatch Unrestricted procedure
    std::vector<Int_t> fMatchedpiIndex_DmUNR;
    std::vector<Int_t> fMatchedKIndex_DmUNR;
    std::vector<Double_t> fMatchingQuality_DmUNR;

    Int_t fTrackID; ///<DownstreamTrack ID / index.	

    std::vector<Bool_t> fMDMatchMade; ///< Boolean variable showing is a match is made or not when using the MD matching procedure. 
    std::vector<TVector3> fVertexPosition; ///< Estimate of vertex position (for GTK candidate and Spectrometer track).
    std::vector<TVector3> fBeamParticleMomentum; ///<Momentum of the upstream (beam) particle [effectively the GTK candidate].
    std::vector<TVector3> fCorrectedBeamParticleMomentum; ///<Corrected momentum of the upstream (beam) particle [effectively the GTK candidate] -- corrected for magnetic "BlueTube/Blue field" effects.
    std::vector<TVector3> fTrackMomentum; ///<DownstreamTrack momentum.
    std::vector<TVector3> fCorrectedTrackMomentum; ///<DownstreamTrack momentum -- corrected for magnetic "BlueTube/Blue field" effects.
    std::vector<Double_t> fcda; ///<Closest Distance of Approach between the upstream (GTK candidate) and downstream tracks.
    std::vector<Double_t> fMDDiscriminant; ///<Value of the MD matching discriminant calculated for the upstream GTK candidate and DowntstreamTrack pairs.

    std::vector<SpectrometerGigaTrackerMatchingOutput> fContainer; ///<Output container for the Spectrometer-GTK matching procedure.

    //Int_t fnRecoCandUpperLim; ///< Upper Limit set on the number of GTK candidates considered (sets size of arrays).

    // Variables previously taken as outputs from K3piStrictSelection but now re-calculated in this analyser
    Bool_t fIsK3pi = false;
    Double_t fKTAGTime = -999.9;
    Double_t fK3piTime = -999.9; // Previously called CHODsTime since it comes from CHODs times
    Double_t fp_K3pi = -999.9;
    Double_t fdxdz_K3pi = -999.9;
    Double_t fdydz_K3pi = -999.9;
    Double_t fXGTK3_K3pi = -999.9;
    Double_t fYGTK3_K3pi = -999.9;
    Double_t fp_K3piX = -999.9;
    Double_t fp_K3piY = -999.9;
    Double_t fp_K3piZ = -999.9;
    Int_t fVertexIndex;
    Int_t fNegativePionIndex = -1;
    Int_t fPositivePionIndex1 = -1; // Higher momentum pi+ track
    Int_t fPositivePionIndex2 = -1; // lower momentum pi+ track
    Double_t fNegativePionCHODsTime = -999.9;
    Double_t fPositivePion1CHODsTime = -999.9;
    Double_t fPositivePion2CHODsTime = -999.9;	

};
#endif

