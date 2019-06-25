// ---------------------------------------------------------------
// History:
//
// Created by Joel Swallow (joelchristopherswallow@cern.ch) 2017-06-09
// modified by Joel Swallow 2017-07-06
// Modified by Joel Swallow 14/09/17
// ---------------------------------------------------------------

#ifndef INCLUDE_SpectrometerGigaTrackerMatchingOutput_HH
#define INCLUDE_SpectrometerGigaTrackerMatchingOutput_HH 1

#include "TMath.h"
#include "TVector3.h"

class SpectrometerGigaTrackerMatchingOutput {

public:

  SpectrometerGigaTrackerMatchingOutput();
  explicit SpectrometerGigaTrackerMatchingOutput(Int_t);
  ~SpectrometerGigaTrackerMatchingOutput() {}

  Int_t    GetTrackID()          { return fTrackID;          };
  Int_t    GetBestIndex()        { return fBestIndex;        };
  Double_t GetBestDiscriminant() { return fBestDiscriminant; };
  TVector3 GetBestVertex()
  { return fBestIndex>=0 ? fVertex[fBestIndex] : TVector3(0.0, 0.0, 0.0); };
  TVector3 GetBestBeamParticleMomentum()
  { return fBestIndex>=0 ? fBeamParticleMomentum[fBestIndex] : TVector3(0.0, 0.0, 0.0); };
  TVector3 GetBestCorrectedBeamParticleMomentum()
  { return fBestIndex>=0 ? fCorrectedBeamParticleMomentum[fBestIndex] : TVector3(0.0, 0.0, 0.0); };
  TVector3 GetBestTrackMomentum()
  { return fBestIndex>=0 ? fTrackMomentum[fBestIndex] : TVector3(0.0, 0.0, 0.0); };
  TVector3 GetBestCorrectedTrackMomentum()
  { return fBestIndex>=0 ? fCorrectedTrackMomentum[fBestIndex] : TVector3(0.0, 0.0, 0.0); };
  Double_t GetBestCDA()
  { return fBestIndex>=0 ? fCDA[fBestIndex] : -999.0; };

  Int_t    GetGTKCandidateIndex(Int_t i)      { return fGTKCandidateIndex[i];      };
  Bool_t   GetMatchMade(Int_t i)        { return fMatchMade[i];        };
  TVector3 GetVertex(Int_t i)                 { return fVertex[i];                 };
  TVector3 GetBeamParticleMomentum(Int_t i)           { return fBeamParticleMomentum[i];           };
  TVector3 GetCorrectedBeamParticleMomentum(Int_t i)  { return fCorrectedBeamParticleMomentum[i];  };
  TVector3 GetTrackMomentum(Int_t i)		      { return fTrackMomentum[i];	   };
  TVector3 GetCorrectedTrackMomentum(Int_t i) { return fCorrectedTrackMomentum[i]; };
  Double_t GetCDA(Int_t i)                    { return fCDA[i];                    };
  Double_t GetDiscriminant(Int_t i)           { return fDiscriminant[i];           };

  Int_t GetNGTKCandidates() { return fGTKCandidateIndex.size(); }; /// return the number of GTK candidtaes = size of the vectors fGTKCandidateIndex, fMatchMade ... etc. 

  void SetTrackID(Int_t val)          { fTrackID = val;          }
  void SetBestIndex(Int_t val)        { fBestIndex = val;        }
  void SetBestDiscriminant(Double_t val) { fBestDiscriminant = val; }
  void AddRecord(Int_t, Bool_t, TVector3, TVector3, TVector3, TVector3, TVector3, Double_t, Double_t);

  void Clear();
  void SetBestMatch(Int_t,Double_t);
  void Print(Int_t);

private:

  Int_t    fTrackID;
  Int_t    fBestIndex;
  Double_t fBestDiscriminant;

  std::vector<Int_t>   fGTKCandidateIndex; 			///<GTK candidate index.
  std::vector<Bool_t>  fMatchMade;				///<Spectrometer-GTK match is made (true/false).
  std::vector<TVector3>fVertex;					///<The Vertex position.
  std::vector<TVector3>fBeamParticleMomentum;			///<The momentum of the beam particle (upstream particle or GTK candidate).
  std::vector<TVector3>fCorrectedBeamParticleMomentum;		///<The momentum of the beam particle (upstream particle or GTK candidate) -- corrected for magnetic "BlueTube/Blue field" effects.
  std::vector<TVector3>fTrackMomentum;				///<The momentum of the DownsreamTrack.
  std::vector<TVector3>fCorrectedTrackMomentum;			///<The momentum of the DownsreamTrack -- corrected for magnetic "BlueTube/Blue field" effects.
  std::vector<Double_t>fCDA;					///<The Closest Distance of Approach between the upstream (GTK candidate) and downstream tracks.
  std::vector<Double_t>fDiscriminant;				///<Value of the matching discriminant calculated for the upstream GTK candidate and DowntstreamTrack pairs. This quantifies the matching quality.
};

#endif
