// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-03-08
//
// ---------------------------------------------------------------

/// \class BeamParameters
/// \Brief
/// Interface to the database of beam spectrometer and RICH calibration parameters
/// \EndBrief
/// \Detailed
/// This class reads 5 parameters from the database:
/// beam central momentum; beam axis parameters dx/dz, dy/dz, x and y (at z=102.4m).
/// The current granularity is one set of constants per run.
/// An example of use for a subsample of the Get...() methods:
/// \code
/// #include "BeamParameters.hh"
/// ...
/// TVector3 KaonThreeMomentum = BeamParameters::GetInstance()->GetBeamThreeMomentum();
/// Double_t Beam_dxdz = BeamParameters::GetInstance()->GetBeamXSlope();
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "BeamParameters.hh"
#include "NA62Exceptions.hh"
#include "NA62ConditionsService.hh"

using namespace std;
using namespace NA62Analysis;

static BeamParameters* fInstance = 0;

BeamParameters* BeamParameters::GetInstance() {
  if (!fInstance) fInstance = new BeamParameters();
  return fInstance;
}

BeamParameters::BeamParameters() {
  ResetBeamParameters();
}

void BeamParameters::ResetBeamParameters() {
  fBeamMomentum = 75000.0;
  fBeamXSlope = 1.200e-3;
  fBeamYSlope = 0.0;
  fBeamX = 0.0;
  fBeamY = 0.0;
  fBeamParametersFound = false;
}

void BeamParameters::InitBeamParameters(Int_t CurrentRunID, Bool_t IsMC) { // called from BaseAnalysis
  ResetBeamParameters(); // set all parameters to their nominal values

  TString BeamParametersFile = "BeamParameters.dat";
  if (IsMC) BeamParametersFile = "BeamParametersMC.dat";
  TString Line;
  NA62ConditionsService::GetInstance()->Open(BeamParametersFile);
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(BeamParametersFile))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    Int_t    Run  = ((TObjString*)(l->At(0)))->GetString().Atoi();
    Double_t Mom  = ((TObjString*)(l->At(1)))->GetString().Atof();
    Double_t dxdz = ((TObjString*)(l->At(2)))->GetString().Atof();
    Double_t dydz = ((TObjString*)(l->At(3)))->GetString().Atof();
    Double_t x0   = ((TObjString*)(l->At(4)))->GetString().Atof();
    Double_t y0   = ((TObjString*)(l->At(5)))->GetString().Atof();
    delete l;
    if (Run!=CurrentRunID) continue; // wrong run number

    fBeamMomentum = Mom;
    fBeamXSlope = dxdz;
    fBeamYSlope = dydz;
    fBeamX = x0;
    fBeamY = y0;
    fBeamParametersFound = true;
  }
  NA62ConditionsService::GetInstance()->Close(BeamParametersFile);
  if (fBeamParametersFound)
    cout << "[BeamParameters] " << (IsMC?"MC":"Data") << " run " << CurrentRunID <<
      ": PK=" << fBeamMomentum << " MeV/c, dx/dz="<< fBeamXSlope << ", dy/dz="<< fBeamYSlope <<
      ", x0=" << fBeamX << " mm, y0=" << fBeamY << " mm" << endl;
  else
    cout << "[BeamParameters] Parameters not found for " <<
      (IsMC?"MC":"data") << " run " << CurrentRunID << ", nominal values used" << endl;
}

//////////////////
// Derived methods

TVector3 BeamParameters::GetBeamXYZ() {
  return TVector3(fBeamX, fBeamY, 102400.0);
}

TVector3 BeamParameters::GetBeamThreeMomentum() {
  Double_t p    = GetBeamMomentum();
  Double_t dxdz = GetBeamXSlope();
  Double_t dydz = GetBeamYSlope();
  Double_t norm = 1.0 / sqrt(1.0 + dxdz*dxdz + dydz*dydz);
  return TVector3(p*dxdz*norm, p*dydz*norm, p*norm);
}

TVector3 BeamParameters::GetNominalBeamThreeMomentum() {
  Double_t p    = 75000.0;
  Double_t dxdz = 1.200e-3;
  Double_t norm = 1.0 / sqrt(1.0 + dxdz*dxdz);
  return TVector3(p*dxdz*norm, 0.0, p*norm);
}

////////////////////////////////////////////////
// Check if parameters are found for a given run

void BeamParameters::Print() {
  TVector3 mom = GetBeamThreeMomentum();
  cout << "[BeamParameters] Px,Py,Pz = " << mom.X()<<" "<<mom.Y()<<" "<<mom.Z()<<endl;
}
