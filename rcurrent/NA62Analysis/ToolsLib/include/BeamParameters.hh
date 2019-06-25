// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-03-08
//
// ---------------------------------------------------------------

#ifndef BEAMPARAMETERS_HH
#define BEAMPARAMETERS_HH

#include <fstream>
#include <iostream>
#include <TVector3.h>
#include "TObjString.h"
#include "TObjArray.h"

class BeamParameters {

public:
  static BeamParameters* GetInstance();
  void     ResetBeamParameters(); ///< Reset beam parameters to their nominal values
  Double_t GetBeamMomentum() { return fBeamMomentum; }
  Double_t GetBeamXSlope()   { return fBeamXSlope;   }
  Double_t GetBeamYSlope()   { return fBeamYSlope;   }
  Double_t GetBeamX()        { return fBeamX;        }
  Double_t GetBeamY()        { return fBeamY;        }
  Double_t GetBeamZ()        { return 102400.0;      }
  TVector3 GetBeamXYZ();
  Bool_t   GetBeamParametersFound() { return fBeamParametersFound; }
  TVector3 GetBeamThreeMomentum();
  TVector3 GetNominalBeamThreeMomentum(); ///< Nominal beam 3-momentum: p=75GeV/c, dx/dz=0.0012, dy/dz=0

  void InitBeamParameters(Int_t CurrentRunID, Bool_t IsMC);
  void Print();

private:
  BeamParameters();
  ~BeamParameters() {}

  Double_t fBeamMomentum;        ///< Beam central momentum [MeV/c] of the current run
  Double_t fBeamXSlope;          ///< Beam axis dx/dz of the current run
  Double_t fBeamYSlope;          ///< Beam axis dy/dz of the current run
  Double_t fBeamX;               ///< Beam axis x [mm] at z=102.4m of the current run
  Double_t fBeamY;               ///< Beam axis y [mm] at z=102.4m of the current run
  Bool_t   fBeamParametersFound; ///< Are beam parameters for data found in the DB?
};

#endif
