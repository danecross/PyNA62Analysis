// ---------------------------------------------------------------
//
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-01-22
//
// ---------------------------------------------------------------

/// \class LKrClusterCorrections
/// \Brief
/// Application of LKr cluster energy and alignment corrections
/// \EndBrief
/// \Detailed
/// Corrections are applied to LKr cluster energies and positions.
/// The corrections to energy include corrections
/// for the overall energy scale, non-linearity and energy loss in the hole.
/// Starting from v1.0.5, the large run-independent energy correction is applied by NA62Reco.
/// The corrections are NOT identical for data and MC.
/// The original LKr candidate data are overwritten,
/// so this analyzer must be run as a pre-analyzer to make sure
/// the corrections are applied exactly once to each event before user processing.
/// For the data, the correction to the energy also includes run-dependent fine energy calibration
/// available via LKrFineCalibration and computed with LKrEopMonitor (EnableFineCalibration=1 by default).
/// Corrections based on pi0 mass are also available: to use them, set EnableFineCalibration=2.
/// In case EnableFineCalibration>0, the uncorrected energies, as well as energies corrected by
/// the two methods described above can be obtained for each LKr candidate LKrCand as follows:
/// \code
/// Double_t EnergyUncorrected       = LKrCand->GetEnergy77Cell(0);
/// Double_t EnergyCorrected_EoP     = LKrCand->GetEnergy77Cell(1);
/// Double_t EnergyCorrected_pi0mass = LKrCand->GetEnergy77Cell(2);
/// \endcode
/// Both the alignment and fine energy calibration corrections can be disabled
/// (simultaneously) via the command line argument:
/// \code
/// ./MyExec -l MyList -p "LKrClusterCorrections:EnableFineCalibration=0" ...
/// \endcode
/// Each of the two methods of correction can be enabled similarly.
/// Another possibility of switching between the correction methods is by reconfiguring this analyzer.
/// \author Giuseppe Ruggiero (Giuseppe.Ruggiero@cern.ch)
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \author Artur Shaikhiev (shaykhiev@inr.ru)
/// \author Francesco Brizioli (francesco.brizioli@cern.ch)
/// \EndDetailed

#include "LKrClusterCorrections.hh"
#include "LKrClusterCorrectionFunctions.hh"
#include "LKrFineCalibration.hh"
#include "NA62ConditionsService.hh"
#include "TRecoLKrEvent.hh"
#include "Stream.hh"
#include "EventHeader.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

LKrClusterCorrections::LKrClusterCorrections(Core::BaseAnalysis *ba) :
  Analyzer(ba, "LKrClusterCorrections"), fWarnOnce(false), fdX(0.0), fdY(0.0), fPhi(0.0), fRevision("") {
  RequestTree("LKr", new TRecoLKrEvent, "Reco");

  fLKrGeo = LKrGeometry::GetInstance();
  fRMSScaleFactor = fLKrGeo->GetLKrCellLength() / 10.0; // to fix old revisions

  // Fine calibration: enabled (E/p based) by default.
  // Valid values: 0 = disabled, 1 = enabled (E/p based), 2 = enabled (pi0 mass based).
  AddParam("EnableFineCalibration", &fFineCalibrationEnabled, 1);
}

LKrClusterCorrections::~LKrClusterCorrections() {
  if (fLKrGeo) delete fLKrGeo;
}

void LKrClusterCorrections::InitHist() {
  cout << user_normal() << "LKr fine calibration ";
  if      (fFineCalibrationEnabled==0) cout << "disabled";
  else if (fFineCalibrationEnabled==1) cout << "enabled (E/p based)";
  else if (fFineCalibrationEnabled==2) cout << "enabled (pi0 mass based)";
  else {
    fFineCalibrationEnabled=0;
    cout << "disabled (invalid user settings)";
  }
  cout << endl;
  BookHisto("hEnergy", new TH1F("Energy", "Cluster energy [GeV]", 200, 0, 100));
  BookHisto("hEnergyCorrected", new TH1F ("EnergyCorrected", "Cluster energy (corrected);[GeV]", 200, 0, 100));
}

void LKrClusterCorrections::StartOfRunUser() {
  fLKrPi0CalibFileName = GetWithMC() ? "LKr-Pi0CalibrationMC.dat" : "LKr-Pi0Calibration.dat";

  fdX = 1.44; fdY = 0.38; fPhi = 0.0; // MC constants
  if (!GetIsTree()) return;

  // Read alignment parameters for the data
  if (!GetWithMC() && fFineCalibrationEnabled) {
    fdX = fdY = fPhi = 0.0;
    Bool_t ParametersFound = false;
    TString filename = "LKr-Alignment.dat";
    if (NA62ConditionsService::GetInstance()->Open(filename)==kSuccess) {
      TString Line;
      while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(filename))) {
	if (Line.BeginsWith("#")) continue;
	TObjArray *l = Line.Tokenize(" ");
	Int_t    Run = ((TObjString*)(l->At(0)))->GetString().Atoi();
	Double_t dx  = ((TObjString*)(l->At(1)))->GetString().Atof();
	Double_t dy  = ((TObjString*)(l->At(2)))->GetString().Atof();
	Double_t phi = ((TObjString*)(l->At(3)))->GetString().Atof();
	delete l;
	if (Run!=GetRunID()) continue; // wrong run number
	fdX  = dx; // [mm]
	fdY  = dy; // [mm]
	fPhi = 0.001*phi; // [rad]
	ParametersFound = true;
      }
      NA62ConditionsService::GetInstance()->Close(filename);
    }
    if (!ParametersFound) {
      cout << user_normal() << "Warning: LKr alignment parameters for run " << GetRunID() << " not found" << endl;
    }
    else {
      cout << user_normal() << "Run " << GetRunID() <<
	": LKr alignment dx="<< fdX << " mm; dy=" << fdY <<" mm; Phi=" << 1e3*fPhi << " mrad" << endl;
    }
  }

  // Read parameters for LKrPi0Calibration
  if (fFineCalibrationEnabled) {
    Bool_t LKrPi0CalibFound = false;
    fFitPar[0] = 1.0;
    for (Int_t j=1; j<8; j++) fFitPar[j] = 0.0;
    fFitRange[0] = 0.0;
    fFitRange[1] = 100000.0;
    if (NA62ConditionsService::GetInstance()->Open(fLKrPi0CalibFileName)==kSuccess) {
      TString Line;
      while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fLKrPi0CalibFileName))) {
	if (Line.BeginsWith("#")) continue;
	TObjArray *l = Line.Tokenize(" ");
	fFitRange[0] = ((TObjString*)(l->At(0)))->GetString().Atof();
	fFitRange[1] = ((TObjString*)(l->At(1)))->GetString().Atof();
	for (Int_t j=0; j<8; j++) fFitPar[j] = ((TObjString*)(l->At(j+2)))->GetString().Atof();
	delete l;
	LKrPi0CalibFound = true;
      }
      NA62ConditionsService::GetInstance()->Close(fLKrPi0CalibFileName);
    }
    if (!LKrPi0CalibFound) cout << user_normal() << "LKrPi0Calibration parameters not found" << endl;
  }
}

void LKrClusterCorrections::StartOfBurstUser() {
  fRevision = GetStreamInfo()->GetRecoInfo().GetRevision();
}

//////////////////////////////////////////////////////////////
// Apply the corrections. The units are standard: [MeV], [mm].

void LKrClusterCorrections::Process(Int_t) {
  if (!GetIsTree()) return; // no actions in histo mode

  // Protection against multiple application of the corrections
  if (IsAnalyzerInHistory(GetAnalyzerName())) {
    if (!fWarnOnce) {
      cout << user_normal() << "Warning: corrections already applied" << endl;
      fWarnOnce = true;
    }
    return;
  }

  TRecoLKrEvent* LKrEvent = GetEvent<TRecoLKrEvent>();
  if (!LKrEvent) return;
  if (!LKrEvent->GetNCandidates()) return;

  for (Int_t iClus=0; iClus<LKrEvent->GetNCandidates(); iClus++) {
    TRecoLKrCandidate* Lcand = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(iClus));

    // Correction to cluster width for old reco revisions
    if (fRevision=="v0.11.1" || fRevision=="v0.11.2" || fRevision=="v0.11.3" ||
	fRevision=="v1.0.0"  || fRevision=="v1.0.1") {
      Lcand->SetClusterRMSX(Lcand->GetClusterRMSX()*fRMSScaleFactor);
      Lcand->SetClusterRMSY(Lcand->GetClusterRMSY()*fRMSScaleFactor);
    }

    Double_t Energy = Lcand->GetClusterEnergy();
    if (Energy<0.0)
      cout << user_normal() << "Warning: cluster #" << iClus << " energy E = " << Energy <<
	" MeV: run " << GetRunID() << ", burst " << GetBurstID() << ", event " <<
	GetEventHeader()->GetEventNumber() << endl;

    //////////////////////////////////////////////////////////////////////////////
    // A fixed energy scale and non-linearity correction (data and MC).
    // It is applied by NA62Reco starting from v1.0.5, and here for old data sets.

    if (fRevision=="v0.11.1" || fRevision=="v0.11.2" || fRevision=="v0.11.3" ||
	fRevision=="v1.0.0"  || fRevision=="v1.0.1" ||
	fRevision=="v1.0.2"  || fRevision=="v1.0.3" || fRevision=="v1.0.4") {
      Energy = LKrClusterCorrectionFunctions::CorrectedEnergy(Lcand, GetWithMC());
    }
    FillHisto("hEnergy", 0.001*Energy); // [GeV]

    Lcand->SetEnergy77Cell(0, Energy);
    Lcand->SetEnergy77Cell(1, Energy);
    Lcand->SetEnergy77Cell(2, Energy);

    if (fFineCalibrationEnabled && Energy>1.0) { // [MeV]

      ////////////////////////////////////////////////////////////
      // Fine calibration: a run-dependent correction based on E/p

      Double_t EnergyScaling = 1.0;
      Double_t EnergyOffset  = 0.0;
      if (!GetWithMC()) { // data: burst-dependent calibration
        EnergyScaling = LKrFineCalibration::GetInstance()->GetEnergyScaling(GetRunID(), GetBurstID());
        EnergyOffset  = LKrFineCalibration::GetInstance()->GetEnergyOffset(GetRunID(),  GetBurstID());
      }
      else { // MC: hard-coded values
        EnergyScaling = 1.00413;
        EnergyOffset  = -214.383; // [MeV]
      }
      Double_t NewEnergy = (Energy-EnergyOffset)/EnergyScaling;
      Lcand->SetEnergy77Cell(1, NewEnergy);

      /////////////////////////////////////////////////////////////////
      // Fine calibration: a run-dependent correction based on pi0 mass

      Double_t Ra    = fFitPar[0];
      Double_t RaMin = fFitPar[0];
      Double_t RaMax = fFitPar[0];
      for (Int_t iPol=1; iPol<8; iPol++) {
	Ra    += fFitPar[iPol]*pow(Energy,iPol);
	RaMin += fFitPar[iPol]*pow(fFitRange[0],iPol);
	RaMax += fFitPar[iPol]*pow(fFitRange[1],iPol);
      }
      Double_t Pi0CorrEnergy = Energy/Ra;
      if (Energy<fFitRange[0]) Pi0CorrEnergy = Energy/RaMin;
      if (Energy>fFitRange[1]) Pi0CorrEnergy = Energy/RaMax;
      Lcand->SetEnergy77Cell(2, Pi0CorrEnergy);

      if      (fFineCalibrationEnabled==1) Lcand->SetClusterEnergy(NewEnergy);
      else if (fFineCalibrationEnabled==2) Lcand->SetClusterEnergy(Pi0CorrEnergy);
      FillHisto("hEnergyCorrected", 0.001*Lcand->GetClusterEnergy());
    }

    /////////////////////////////////////////////////////
    // Fine calibration: correction for LKr mis-alignment

    if (fFineCalibrationEnabled) {
      Double_t x0 = Lcand->GetClusterX();
      Double_t y0 = Lcand->GetClusterY();
      Double_t x  = + x0*cos(fPhi) + y0*sin(fPhi) + fdX;
      Double_t y  = - x0*sin(fPhi) + y0*cos(fPhi) + fdY;
      Lcand->SetClusterX(x);
      Lcand->SetClusterY(y);
    }
  }

  /////////////////////////////////////
  // Recompute the total cluster energy

  if (fFineCalibrationEnabled) {
    Double_t TotalEnergy = 0.0;
    for (Int_t i=0; i<LKrEvent->GetNCandidates(); i++) {
      TRecoLKrCandidate* Lcand = static_cast<TRecoLKrCandidate*>(LKrEvent->GetCandidate(i));
      TotalEnergy += Lcand->GetClusterEnergy();
    }
    LKrEvent->SetEnergyTotal(TotalEnergy);
  }
}

void LKrClusterCorrections::EndOfJobUser() {
  SaveAllPlots();
}
