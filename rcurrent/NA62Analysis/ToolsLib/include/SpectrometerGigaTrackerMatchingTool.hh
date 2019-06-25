// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-05-28
// Based on the code developed by Giuseppe Ruggiero
//
// ---------------------------------------------------------------

#ifndef SpectrometerGigaTrackerMatchingTool_HH
#define SpectrometerGigaTrackerMatchingTool_HH

#include <fstream>
#include <iostream>
#include "TRecoGigaTrackerEvent.hh"
#include "TRecoSpectrometerCandidate.hh"
#include "TwoLinesCDA.hh"

class SpectrometerGigaTrackerMatchingTool {

public:

  SpectrometerGigaTrackerMatchingTool();
  ~SpectrometerGigaTrackerMatchingTool();

  // Global settings
  void SetMatchingTimingCuts(Double_t val1, Double_t val2)
  { fMinTimingCut = val1; fMaxTimingCut = val2; }
  void Reset();

  // The matching algorithm
  void Match(TRecoGigaTrackerEvent*, TRecoSpectrometerCandidate*, Double_t time, Int_t RefDetector);

  // Retrieve outputs
  Int_t    GetNGTKCandidates()        { return fNGTKCandidates;        }
  Int_t    GetNInTimeGTKCandidates()  { return fNInTimeGTKCandidates;  }
  Int_t    GetNMatchedGTKCandidates() { return fNMatchedGTKCandidates; }
  Bool_t   MatchingGTKTrackFound()    { return (fBestIndex>=0);        } // same as BestGTKTrackFound()
  Bool_t   BestGTKTrackFound()        { return (fBestIndex>=0);        }
  Bool_t   SecondBestGTKTrackFound()  { return (fSecondBestIndex>=0);  }
  Int_t    GetBestIndex()             { return fBestIndex;             }
  Int_t    GetSecondBestIndex()       { return fSecondBestIndex;       }

  TVector3 GetBestVertex()
  { return fBestIndex>=0 ? fVertex[fBestIndex] : TVector3(0.0, 0.0, 0.0); }
  TVector3 GetBestBeamMomentum()
  { return fBestIndex>=0 ? fBeamMomentum[fBestIndex] : TVector3(0.0, 0.0, 0.0); }
  TVector3 GetBestCorrectedBeamMomentum()
  { return fBestIndex>=0 ? fCorrectedBeamMomentum[fBestIndex] : TVector3(0.0, 0.0, 0.0); }
  TVector3 GetBestCorrectedTrackMomentum()
  { return fBestIndex>=0 ? fCorrectedTrackMomentum[fBestIndex] : TVector3(0.0, 0.0, 0.0); }
  Double_t GetBestCDA()
  { return fBestIndex>=0 ? fCDA[fBestIndex] : 999.0; }
  Double_t GetBestDeltaTime() ///< Time of the track with the largest discriminant; this is not the closest time
  { return fBestIndex>=0 ? fDeltaTime[fBestIndex] : 999.0; }
  Double_t GetClosestDeltaTime() { return fClosestDeltaTime; }

  Double_t GetBestDiscriminant()              { return fBestDiscriminant;             }
  Double_t GetBestDiscriminantPileup()        { return fBestDiscriminantPileup;       }
  Double_t GetSecondBestDiscriminant()        { return fSecondBestDiscriminant;       }
  Double_t GetSecondBestDiscriminantPileup()  { return fSecondBestDiscriminantPileup; }

  Bool_t   GetCandidateIsMatched(Int_t i)     { return fCandidateIsMatched[i];     }
  TVector3 GetVertex(Int_t i)                 { return fVertex[i];                 }
  TVector3 GetBeamMomentum(Int_t i)           { return fBeamMomentum[i];           }
  TVector3 GetCorrectedBeamMomentum(Int_t i)  { return fCorrectedBeamMomentum[i];  }
  TVector3 GetCorrectedTrackMomentum(Int_t i) { return fCorrectedTrackMomentum[i]; }
  Double_t GetCDA(Int_t i)                    { return fCDA[i];                    }
  Double_t GetDeltaTime(Int_t i)              { return fDeltaTime[i];              }
  Double_t GetDiscriminant(Int_t i)           { return fDiscriminant[i];           }
  Double_t GetDiscriminantPileup(Int_t i)     { return fDiscriminantPileup[i];     }

  Double_t GetDiscriminantValue(Double_t, Double_t, Int_t); ///< Discriminant value for a (CDA, dT) pair

  void Print();

private:

  TwoLinesCDA* fCDAcomp;
  TVector3 fBeamMomentumTemp;
  TVector3 fTrackMomentumTemp;
  TVector3 fVertexTemp;
  Double_t fCDATemp;
  Double_t fMinTimingCut, fMaxTimingCut; ///< GTK candidate - reference timing cuts [ns]

  std::vector<Double_t> fPDFKaonCDA[2];
  std::vector<Double_t> fPDFPileCDA[2];
  std::vector<Double_t> fPDFKaonDT[2];
  std::vector<Double_t> fPDFPileDT[2];
  Double_t fIntPDFKaonCDA[2];
  Double_t fIntPDFKaonDT[2];
  Double_t fIntPDFPileCDA[2];
  Double_t fIntPDFPileDT[2];
  Double_t fDiscr[2];

  Double_t Chi2CandidateBeam(TRecoGigaTrackerCandidate*); ///< quantifies GTK track compatibility with the beam
  Double_t PDFKaonCDA(Double_t);
  Double_t PDFKaonDT(Double_t, Int_t);
  Double_t PDFPileCDA(Double_t);
  Double_t PDFPileDT(Double_t, Int_t);

  void ComputeVertex(TRecoGigaTrackerCandidate*, TRecoSpectrometerCandidate*);
  void DiscriminantNormalization(Int_t); ///< Initialise the discriminants
  void ComputeDiscriminant(Double_t, Double_t, Int_t);

  Double_t EvaluateCondition(Double_t var, Double_t cut, Double_t integral)
  { return (var>cut) ? integral : -1.0; }

  // Outputs

  Int_t    fNGTKCandidates; ///< Total number of GTK candidates in the event
  Int_t    fNInTimeGTKCandidates; ///< Number of GTK candidates in matching time window
  Int_t    fNMatchedGTKCandidates; ///< Number of in-time GTK candidates compatible with beam spectrum
  Int_t    fBestIndex; ///< Index of the best matched GTK track (i.e. of the one with the largest discriminant)
  Int_t    fSecondBestIndex; ///< Index of the second best matched GTK track
  Double_t fClosestDeltaTime; ///< The smallest time difference; not necessarily the one of the best matched track
  Double_t fBestDiscriminant;
  Double_t fSecondBestDiscriminant;
  Double_t fBestDiscriminantPileup;
  Double_t fSecondBestDiscriminantPileup;

  std::vector<Bool_t>fCandidateIsMatched; ///< For each GTK track, is it in-time and compatible with beam spectrum?
  std::vector<TVector3>fVertex; ///< For each GTK track, blue-tube corrected vertex coordinates
  std::vector<TVector3>fBeamMomentum;
  std::vector<TVector3>fCorrectedBeamMomentum;
  std::vector<TVector3>fCorrectedTrackMomentum;
  std::vector<Double_t>fCDA;
  std::vector<Double_t>fDeltaTime;
  std::vector<Double_t>fDiscriminant;
  std::vector<Double_t>fDiscriminantPileup;
};

#endif
