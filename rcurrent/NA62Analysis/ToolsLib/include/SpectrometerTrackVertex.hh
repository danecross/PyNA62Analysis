// ------------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)    2015-08-25
//
// ------------------------------------------------------------------

#ifndef SPECTROMETERTRACKVERTEX_HH
#define SPECTROMETERTRACKVERTEX_HH 1

#include <TVector3.h>
#include "TRecoSpectrometerCandidate.hh"

class SpectrometerTrackVertex {

public:

  SpectrometerTrackVertex();
  virtual ~SpectrometerTrackVertex() {}

  void     Clear();
  void     AddTrack(TRecoSpectrometerCandidate*, Int_t, Int_t, TVector3, TVector3);

  void     SetNTracks (Int_t val)               { fNTracks  = val;            }
  void     SetCharge  (Int_t val)               { fCharge   = val;            }
  void     SetPosition(TVector3 val)            { fPosition = val;            }
  void     SetChi2    (Double_t val)            { fChi2     = val;            }
  void     SetTime    (Double_t val)            { fTime = val;                }
  void     SetTotalThreeMomentum(TVector3 val)  { fTotalThreeMomentum = val;  }
  void     SetTotalThreeMomentum0(TVector3 val) { fTotalThreeMomentum0 = val; }

  Int_t    GetNTracks() const             { return fNTracks;                   }
  Int_t    GetCharge() const              { return fCharge;                    }
  TVector3 GetPosition() const            { return fPosition;                  }
  Double_t GetChi2() const                { return fChi2;                      }
  Double_t GetTime() const                { return fTime;                      }
  TVector3 GetTotalThreeMomentum() const  { return fTotalThreeMomentum;        }
  Double_t GetTotalMomentum() const       { return fTotalThreeMomentum.Mag();  }
  TVector3 GetTotalThreeMomentum0() const { return fTotalThreeMomentum0;       }
  Double_t GetTotalMomentum0() const      { return fTotalThreeMomentum0.Mag(); }

  std::vector<Int_t>& GetTrackIndexes()   { return fTrackIndices; }
  Int_t    GetTrackIndex (Int_t i); ///< Track index in the TRecoSpectrometerCandidate array
  Int_t    GetTrackCharge(Int_t i); ///< Track charge (+-1)

  // "Corrected" values (output from the vertex fit)
  TVector3 GetTrackThreeMomentum(Int_t i);
  Double_t GetTrackMomentum(Int_t i);
  Double_t GetTrackSlopeX(Int_t i);
  Double_t GetTrackSlopeY(Int_t i);

  // "Uncorrected" values (spectrometer reconstruction input to the vertex fit)
  TVector3 GetTrackThreeMomentum0(Int_t i);
  Double_t GetTrackMomentum0(Int_t i);
  Double_t GetTrackSlopeX0(Int_t i);
  Double_t GetTrackSlopeY0(Int_t i);
  TRecoSpectrometerCandidate* GetSpectrometerCandidate(Int_t i);

  void Print() const;

private:

  Int_t    fNTracks;                   ///< Number of tracks forming the vertex
  Int_t    fCharge;                    ///< Total electrical charge of the tracks
  TVector3 fPosition;                  ///< Vertex position
  Double_t fChi2;                      ///< Vertex fit quality: chi2
  Double_t fTime;                      ///< Average of the track Straw leading times
  TVector3 fTotalThreeMomentum;        ///< Total three-momentum of all vertex tracks (after vertex fit)
  TVector3 fTotalThreeMomentum0;       ///< Total three-momentum of all vertex tracks (input to vertex fit)
  std::vector<Int_t>fTrackIndices;     ///< Spectrometer track indices
  std::vector<Int_t>fTrackCharges;     ///< Spectrometer track charges (+-1)
  std::vector<TVector3>fThreeMomenta;  ///< Three-momenta (after vertex fit)
  std::vector<TVector3>fThreeMomenta0; ///< Three-momenta (input to vertex fit)
  std::vector<TRecoSpectrometerCandidate*>fSpectrometerCandidates; ///< Pointers to the spectrometer tracks
};

#endif
