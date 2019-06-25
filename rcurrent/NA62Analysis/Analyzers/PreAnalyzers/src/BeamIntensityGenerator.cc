// ---------------------------------------------------------------
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2019-01-17
// ---------------------------------------------------------------

/// \class BeamIntensityGenerator
/// \Brief
/// Generation of a beam intensity value, picked at random from a template distribution
/// \EndBrief
/// \Detailed
/// Generation of an instantaneous beam intensity value for an MC event,
/// picked at random from a template distribution, in a reproducible way.
/// This value is to be used by the pileup generators.
/// The maximum and minimum intensity values can be set by the user, with parameters
/// "MinBeamIntensity" and "MaxBeamIntensity".
/// In case invalid values are chosen, the code will (silently) return -1.0.
/// The file name containing the beam intensity template can be set by the user, with
/// the parameter "BeamIntensityTemplateFileName". If no template histogram is found,
/// the intensity is generated uniformly between the min and max value.
/// The generated beam intensity can be accessed by the parameter "BeamIntensity".
/// The generated value is filled in a histogram named "BeamIntensity".
///
/// \author Chris Parkinson (chris.parkinson@cern.ch)
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "BeamIntensityGenerator.hh"
#include "NA62ConditionsService.hh"
#include "EventHeader.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

BeamIntensityGenerator::BeamIntensityGenerator(Core::BaseAnalysis *ba) :
  Analyzer(ba, "BeamIntensityGenerator"),
  fBeamIntensityTemplateHistoName("hBeamIntensityTemplateMCTruth"),
  fHBeamIntensityTemplate(nullptr), fBeamIntensity(-1.0) {

  TString DefaultTemplateFileName =
    NA62ConditionsService::GetInstance()->GetFullPath("BeamIntensityTemplate-2017A.root");

  AddParam("MinBeamIntensity", &fMinBeamIntensity,    0.0); // [MHz]
  AddParam("MaxBeamIntensity", &fMaxBeamIntensity, 5000.0); // [MHz]
  AddParam("BeamIntensityTemplateFileName", &fBeamIntensityTemplateFileName, DefaultTemplateFileName);
  AddParam("ForcedOnData", &fForcedOnData, false);

  fRandom = new TRandom2();
}

BeamIntensityGenerator::~BeamIntensityGenerator() {
  if (fRandom) delete fRandom;
}

void BeamIntensityGenerator::InitOutput() {
  RegisterOutput("BeamIntensity", &fBeamIntensity);
}

void BeamIntensityGenerator::InitHist() {
  if (!GetWithMC() && !fForcedOnData) return;
  if (!GetIsTree()) return;

  Double_t intstep = 1000.0/(3*45.0);
  BookHisto(new TH1F("BeamIntensity", "BeamIntensity;Intensity [MHz]", 340, -0.5*intstep, 339.5*intstep));

  if (fBeamIntensityTemplateFileName != "") {
    TFile* BeamIntensityTemplateFile = TFile::Open(fBeamIntensityTemplateFileName);
    if (BeamIntensityTemplateFile) {
      cout << user_normal() << "Reading intensity template from " << fBeamIntensityTemplateFileName << endl;
      BeamIntensityTemplateFile->GetObject(fBeamIntensityTemplateHistoName, fHBeamIntensityTemplate);
      if (fHBeamIntensityTemplate) {
	fHBeamIntensityTemplate->SetDirectory(0);
	BeamIntensityTemplateFile->Close();
      }
      else {
	cout << user_normal() << "Template " <<
	  fBeamIntensityTemplateHistoName << " not found, will generate uniform distribution" << endl;
      }
    }
    else { // ConditionsService normally does not allow this to happen
      cout << user_normal() << "Template file " <<
	fBeamIntensityTemplateFileName << " not found, will generate uniform distribution" << endl;
    }
  }
  cout << user_normal() << "Min/max beam intemsity to generate [MHz]: " <<
    fMinBeamIntensity << " " << fMaxBeamIntensity << endl;
}

void BeamIntensityGenerator::StartOfBurstUser() {
  if (!GetIsTree()) return;
  if (!GetEventHeader()) return;
  fRandom->SetSeed(GetEventHeader()->GetBurstID()); // to ensure reproducibility
}

void BeamIntensityGenerator::Process(Int_t) {

  fBeamIntensity = -1.0;
  if (!GetWithMC() && !fForcedOnData) return;
  if (!GetIsTree()) return;
  if (!GetEventHeader()) return; // do not run on NA62MC output

  // Sanity checks; intensities are in [MHz]
  if (fMaxBeamIntensity<=fMinBeamIntensity) return;
  if (fMinBeamIntensity<0.0) return;
  if (fMinBeamIntensity<1.0 && fMaxBeamIntensity<1.0) return;

  ///////////////////////////////////////////////////////////
  // Generate the instantaneous beam intensity for this event

  if (fHBeamIntensityTemplate) { // generation from template, between set minimum and maximum
    TRandom *r = gRandom;
    gRandom = fRandom;
    do {
      fBeamIntensity = fHBeamIntensityTemplate->GetRandom();
    } while (fBeamIntensity<fMinBeamIntensity || fBeamIntensity>fMaxBeamIntensity);
    gRandom = r;
  }
  else { // no template provided: flat distribution between minimum and maximum
    fBeamIntensity = fMinBeamIntensity + fRandom->Uniform(fMaxBeamIntensity-fMinBeamIntensity);
  }
  FillHisto("BeamIntensity", fBeamIntensity);
}

void BeamIntensityGenerator::EndOfJobUser() {
  if (!GetWithMC() && !fForcedOnData) return;
  if (!GetIsTree()) return;
  SaveAllPlots();
}
