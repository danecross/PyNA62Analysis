#include "SpectrometerParameters.hh"
#include "TMath.h"
#include "TObjString.h"
#include "TObjArray.h"
#include "TRegexp.h"
#include "TString.h"
#include "Riostream.h"
#include "CLHEP/Units/SystemOfUnits.h"
#include <iostream>
#include <string>
#include <sstream>
#include "NA62ConditionsService.hh"
#include "NA62Global.hh"
#include "NA62Utilities.hh"

using namespace std;
using namespace CLHEP;

/// \class SpectrometerParameters
/// \Brief
/// Spectrometer parameters.
/// \EndBrief
///
/// \Detailed
/// This class stores the spectrometer reconstruction parameters.
/// \EndDetailed

SpectrometerParameters::SpectrometerParameters() :
  fEC(TMath::C()*(eV/GeV)*(gauss/tesla)*(cm/m))
{
  for (Int_t j = 0; j < 4; j++) {
    fZChamberCorrection[j] = 0.;
    fChambersMCToF[j] = 0.;
  }
  for (int i = 0; i < 20; i++) {
    fViewsMCToF[i] = 0;
    fPlanesMCToF[i] = 0;
  }
  for (Int_t j = 0; j < 4; j++) {
    for (Int_t i = 0; i < 4; i++) {
      fXAlignment[j][i] = 0;
    }
  }
  for (Int_t j = 0; j<64; j++) {
    for (Int_t i = 0; i<123; i++) {
      fXStrawAlignment[j][i] = 0;
    }
  }
  for (Int_t j = 0; j < 64; j++) {
    for (Int_t i=0; i < 123; i++) {
      fChannelT0[j][i] = 0;
    }
  }
  for (Int_t k = 0; k < 702; k++) {
    fBxIntegral[k] = 0;
    fByIntegral[k] = 0;
  }
  for (Int_t k = 0; k < 169; k++) {
    for (Int_t i = 0; i < 26; i++) {
      for (Int_t j = 0; j < 26; j++) {
        fBxMNP33[i][j][k] = 0;
        fByMNP33[i][j][k] = 0;
        fBzMNP33[i][j][k] = 0;
      }
    }
    fZMNP33[k] = 0;
  }
  for (Int_t i = 0; i < 30; i++) {
    for (Int_t j = 0; j < 29; j++) {
      fbx[i][j] = 0;
      fby[i][j] = 0;
      fbz[i][j] = 0;
    }
  }
  for (Int_t jz=0; jz<30; jz++) {
    for (Int_t jx=0; jx<17; jx++) {
      for (Int_t jy=0; jy<17; jy++) {
        fBxFringe[jx][jy][jz] = 0.;
        fByFringe[jx][jy][jz] = 0.;
        fBzFringe[jx][jy][jz] = 0.;
      }
    }
    fZFringe[jz] = -99999.;
  }
  for (Int_t i = 0; i < 64*23; i++) {
    for (Int_t j = 0; j < 13; j++) {
      fRTH[i][j] = 0;
    }
  }
  for (Int_t i = 0; i < 16; i++) {
    for (Int_t j = 0; j < 7; j++) {
      fXTH[i][j] = 0;
    }
  }
}

void SpectrometerParameters::ParseConfFile(TString ConfFileName)
{
  ifstream confFile(ConfFileName.Data());
  if(!confFile.is_open()) {
    perror(Form("Configuration File : %s",ConfFileName.Data()));
    exit(kWrongConfiguration);
  }
  else cout << "[SpectrometerParameters] Reading config file: " << ConfFileName << endl;

  TString Line;
  Double_t units = 1;
  Int_t nItems=0;
  while(Line.ReadLine(confFile))
  {
    if(Line.BeginsWith("#")) continue;
    else if(Line.BeginsWith("RunType")){
      TObjArray * line = Line.Tokenize(" ");
      TString nameDataType = ((TObjString*)(line->At(2)))->GetString();
      if (nameDataType.CompareTo("muon")==0) fIsMuonRun = true;
      delete line;
    } else if(Line.BeginsWith("TimeReference")){
      TObjArray * line = Line.Tokenize(" ");
      fTimeReference = ((TObjString*)(line->At(2)))->GetString();
      delete line;
    } else if (Line.BeginsWith("StationsAlignmentFileInput")){
      TObjArray *l = Line.Tokenize(" ");
      fStationsAlignmentFileName = ((TObjString*)(l->At(2)))->GetString();
      delete l;
    } else if (Line.BeginsWith("AlignmentFileInput")){
      TObjArray *l = Line.Tokenize(" ");
      fAlignmentFileName = ((TObjString*)(l->At(2)))->GetString();
      delete l;
    } else if (Line.BeginsWith("ZOffsetFileInput")){
      TObjArray *l = Line.Tokenize(" ");
      fZCorrectionFileName = ((TObjString*)(l->At(2)))->GetString();
      delete l;
    } else if (Line.BeginsWith("BxIntegralFileInput")){
      TObjArray *l = Line.Tokenize(" ");
      fBxIntegralFileName  = ((TObjString*)(l->At(2)))->GetString();
      delete l;
    } else if (Line.BeginsWith("ByIntegralFileInput")){
      TObjArray *l = Line.Tokenize(" ");
      fByIntegralFileName = ((TObjString*)(l->At(2)))->GetString();
      delete l;
    } else if (Line.BeginsWith("BMNP33FileInput")){
      TObjArray *l = Line.Tokenize(" ");
      fBMNP33FileName = ((TObjString*)(l->At(2)))->GetString();
      delete l;
    } else if (Line.BeginsWith("BFringeFileInput")){
      TObjArray *l = Line.Tokenize(" ");
      fBFringeFileName = ((TObjString*)(l->At(2)))->GetString();
      delete l;
    } else if (Line.BeginsWith("RTFileInput")){
      TObjArray *l = Line.Tokenize(" ");
      fRTFileName = ((TObjString*)(l->At(2)))->GetString();
      delete l;
    } else if (Line.BeginsWith("XTFileInput")){
      TObjArray *l = Line.Tokenize(" ");
      fXTFileName = ((TObjString*)(l->At(2)))->GetString();
      delete l;
    } else if(Line.BeginsWith("MaxTime=")) {
      fMaxTime = TString(Line(TRegexp("[0-9]+.*[0-9]*"))).Atof();
    } else if(Line.BeginsWith("TimeStep=")) {
      fTimeStep = TString(Line(TRegexp("[0-9]+.*[0-9]*"))).Atof();
    } else if(Line.BeginsWith("StrawT0=")) {
      fStrawT0 = TString(Line(TRegexp(" [0-9]+.*[0-9]*"))).Atof();
    } else if(Line.BeginsWith("NClustersPermm=")) {
      fNClustersPermm = TString(Line(TRegexp("[0-9]+.*[0-9]*"))).Atof();
    } else if(Line.BeginsWith("DiscrSetTime=")) {
      fDiscrSetTime = TString(Line(TRegexp("[0-9]+.*[0-9]*"))).Atof();
    } else if(Line.BeginsWith("EdgeDeadTime=")) {
      fEdgeDeadTime = TString(Line(TRegexp("[0-9]+.*[0-9]*"))).Atof();
    } else if(Line.BeginsWith("SameEdgeDeadTime=")) {
      fSameEdgeDeadTime = TString(Line(TRegexp("[0-9]+.*[0-9]*"))).Atof();
    } else if(Line.BeginsWith("Threshold=")) {
      fThreshold = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof();
    } else if(Line.BeginsWith("Gain=")) {
      fGain = TString(Line(TRegexp("[0-9]+.*[0-9]*"))).Atof();
    } else if(Line.BeginsWith("CARIOCASlope=")) {
      fCARIOCASlope = TString(Line(TRegexp("[0-9]+.*[0-9]*"))).Atof();
    } else if(Line.BeginsWith("EquivalentNoiseCharge=")) {
      fEquivalentNoiseCharge = TString(Line(TRegexp("[0-9]+.*[0-9]*"))).Atof();
    } else if(Line.BeginsWith("IonizationEnergy=")) {
      fIonizationEnergy = TString(Line(TRegexp("[0-9]+.*[0-9]*"))).Atof();
    } else if(Line.BeginsWith("NTotalPermm=")) {
      fNTotalPermm = TString(Line(TRegexp("[0-9]+.*[0-9]*"))).Atof();
    } else if(Line.BeginsWith("NoiseSimu=")) {
      fNoiseSimu = TString(Line(TRegexp(" [0-9]+"))).Atoi();
    } else if(Line.BeginsWith("SavePulseShapes=")) {
      fSavePulseShapes = TString(Line(TRegexp("[0-9]+"))).Atoi();
    } else if(Line.BeginsWith("NoBulkHits=")) {
      fNoBulkHits = TString(Line(TRegexp("[0-9]+"))).Atoi();
    } else if(Line.BeginsWith("MagicT0=")) {
      Line.Remove(6,1);
      fMagicT0 = (TString(Line(TRegexp("[0-9]+.*[0-9]*"))).Atof());
    } else if(Line.BeginsWith("MagicT0FileInput=")) {
      TObjArray *l = Line.Tokenize(" ");
      fMagicT0FileName = ((TObjString*)(l->At(1)))->GetString();
      delete l;
    } else if(Line.BeginsWith("MagicT0ScanEnabled=")) {
      Line.Remove(6,1);
      fMagicT0ScanEnabled = (TString(Line(TRegexp("[0-9]+"))).Atoi());
    } else if(Line.BeginsWith("RTMode=")) {
      fRTMode = TString(Line(TRegexp("[0-9]+"))).Atoi();
    } else if(Line.BeginsWith("XTMode=")) {
      fXTMode = TString(Line(TRegexp("[0-9]+"))).Atoi();
    } else if(Line.BeginsWith("ChambersMCToF=")){
      TObjArray * l = Line.Tokenize(" ");
      for(int j = 0; j < l->GetEntries() - 1; j++) {
        fChambersMCToF[j] = ((TObjString*)(l->At(j+1)))->GetString().Atof();
      }
      delete l;
    } else if(Line.BeginsWith("ViewsMCToF=")){
      TObjArray * l = Line.Tokenize(" ");
      for(int j = 0; j < l->GetEntries() - 1; j++) {
        fViewsMCToF[j] = ((TObjString*)(l->At(j+1)))->GetString().Atof();
      }
      delete l;
    } else if(Line.BeginsWith("PlanesMCToF=")){
      TObjArray * l = Line.Tokenize(" ");
      for(int j = 0; j < l->GetEntries() - 1; j++)
      {
        fPlanesMCToF[j] = ((TObjString*)(l->At(j+1)))->GetString().Atof();
      }
      delete l;
    } else if(Line.BeginsWith("TWindow=")) { // -100 300
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>3) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(3)))->GetString());
      else { cout << "WARNING: TWindow in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      if (!units) exit(kWrongConfiguration);
      fTWindowMin = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/ns);
      fTWindowMax = ((TObjString*)(l->At(2)))->GetString().Atof()*(units/ns);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Null_coordinate=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: Null_coordinate in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      fNullCoordinate = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/mm);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Chambers,Views,Planes,Straws=")) {
      TObjArray * l = Line.Tokenize(" ");
      fNChambers = ((TObjString*)(l->At(1)))->GetString().Atoi();
      fNViews = ((TObjString*)(l->At(2)))->GetString().Atoi();
      fNPlanes = ((TObjString*)(l->At(3)))->GetString().Atoi();
      fNStraws = ((TObjString*)(l->At(4)))->GetString().Atoi();
      delete l;
    } else if(Line.BeginsWith("TDrift_max=")) { // 160
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: TDrift_max in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      fTDriftMax = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/ns);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Ttrailing_cut_2hits=")) { // 150
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: Ttrailing_cut_2hits in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      fTTrailingCut2Hits = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/ns);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Ttrailing_sigma_3hits=")) { // 20
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: Ttrailing_sigma_3hits in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      fTTrailingSigma3Hits = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/ns);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Ttrailing_sigma_2hits=")) { // 25
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: Ttrailing_sigma_2hits in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      fTTrailingSigma2Hits = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/ns);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("RadiusSum_2hits=")) { // 6.5
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: RadiusSum_2hits in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      fRadiusSum2HitsMax = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/mm);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Triplet_chi2=")) { // 7.5
      TObjArray * l = Line.Tokenize(" ");
      fChi2Triplet = ((TObjString*)(l->At(1)))->GetString().Atof();
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Triplet_parameters=")) { // 0.5 3.9 4.45 0.2
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>5) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(5)))->GetString());
      else { cout << "WARNING: Triplet_parameters in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      if (!units) exit(kWrongConfiguration);
      fTripletParameter1 = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/mm);
      fTripletParameter2 = ((TObjString*)(l->At(2)))->GetString().Atof()*(units/mm);
      fTripletParameter3 = ((TObjString*)(l->At(3)))->GetString().Atof()*(units/mm);
      fTripletParameter4 = ((TObjString*)(l->At(4)))->GetString().Atof()*(units/mm);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Doublet_parameters=")) { //
      TObjArray * l = Line.Tokenize(" ");
      fDoubletParameter1 = ((TObjString*)(l->At(1)))->GetString().Atof();
      fDoubletParameter2 = ((TObjString*)(l->At(2)))->GetString().Atof();
      fDoubletParameter3 = ((TObjString*)(l->At(3)))->GetString().Atof();
      nItems++;
      delete l;
    } else if(Line.BeginsWith("HalfView_staggering=")) { // 4.4
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: HalfView_staggering in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      fPlaneStaggering = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/mm);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Difference_StrawHit_position=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: Difference_StrawHit_position in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      fDeltaStrawPositionMax = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/mm);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Intersection_ttrailing_cut=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: Intersection_ttrailing_cut in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      fInterTtrailingCut = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/ns);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Intersection_distance_cut=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: Intersection_distance_cut in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      fInterDistanceCut = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/mm);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("4ViewIntersection_quality_cut=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>5) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(5)))->GetString());
      else { cout << "WARNING: 4ViewIntersection_quality_cut in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      if (!units) exit(kWrongConfiguration);
      fInterQuality4ViewsCut1 = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/mm);
      fInterQuality4ViewsCut2 = ((TObjString*)(l->At(2)))->GetString().Atof()*(units/mm);
      fInterQuality4ViewsCut3 = ((TObjString*)(l->At(3)))->GetString().Atof()*(units/mm);
      fInterQuality4ViewsCut4 = ((TObjString*)(l->At(4)))->GetString().Atof()*(units/mm);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("4ViewIntersection_ttrailing_sigma=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: 4ViewIntersection_ttrailing_sigma in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      fInter4ViewsTtrailingSigma = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/ns);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("3ViewIntersection_quality_cut=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>5) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(5)))->GetString());
      else { cout << "WARNING: 3ViewIntersection_quality_cut in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      if (!units) exit(kWrongConfiguration);
      fInterQuality3ViewsCutXYU = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/mm);
      fInterQuality3ViewsCutXYV = ((TObjString*)(l->At(2)))->GetString().Atof()*(units/mm);
      fInterQuality3ViewsCutXUV = ((TObjString*)(l->At(3)))->GetString().Atof()*(units/mm);
      fInterQuality3ViewsCutYUV = ((TObjString*)(l->At(4)))->GetString().Atof()*(units/mm);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("3ViewIntersection_ttrailing_sigma=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: 3ViewIntersection_ttrailing_sigma in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      fInter3ViewsTtrailingSigma = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/ns);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Combination_XCorridor=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>5) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(5)))->GetString());
      else { cout << "WARNING: Combination_XCorridor in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      if (!units) exit(kWrongConfiguration);
      fCombinationXCorridor0 = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/mm);
      fCombinationXCorridor1 = ((TObjString*)(l->At(2)))->GetString().Atof()*(units/mm);
      fCombinationXCorridor2 = ((TObjString*)(l->At(3)))->GetString().Atof()*(units/mm);
      fCombinationXCorridor3 = ((TObjString*)(l->At(4)))->GetString().Atof()*(units/mm);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Combination_YCorridor=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>5) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(5)))->GetString());
      else { cout << "WARNING: Combination_YCorridor in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      if (!units) exit(kWrongConfiguration);
      fCombinationYCorridor0 = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/mm);
      fCombinationYCorridor1 = ((TObjString*)(l->At(2)))->GetString().Atof()*(units/mm);
      fCombinationYCorridor2 = ((TObjString*)(l->At(3)))->GetString().Atof()*(units/mm);
      fCombinationYCorridor3 = ((TObjString*)(l->At(4)))->GetString().Atof()*(units/mm);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("3HitsCombination_radiusatmc=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: 3HitsCombination_radiusatmc in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      f3HitsCombRadiusAtMC = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/ns);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Momentum_max=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: Momentum_max in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      fMaxMomentum = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/MeV);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Angle_max=")) {
      TObjArray * l = Line.Tokenize(" ");
      fMaxAngle = ((TObjString*)(l->At(1)))->GetString().Atof();
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Combination_QualityCut=")) {
      TObjArray * l = Line.Tokenize(" ");
      fCombQualityCut = ((TObjString*)(l->At(1)))->GetString().Atof();
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Combination_HoughDeltaCut=")) {
      TObjArray * l = Line.Tokenize(" ");
      fCombHoughDeltaCut = ((TObjString*)(l->At(1)))->GetString().Atof();
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Guess_digi_sigma=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: Guess_digi_sigma in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      fGuessAverageSigma = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/mm);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Guess_digisinglet_sigma=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: Guess_digisinglet_sigma in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      if (!units) exit(kWrongConfiguration);
      fGuessSingletSigma = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/mm);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Z_reference_plane=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>2) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(2)))->GetString());
      else { cout << "WARNING: Z_reference_plane in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      fZRef = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/mm);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("X/X0_per_plane=")) {
      TObjArray * l = Line.Tokenize(" ");
      fXX0perPlane = ((TObjString*)(l->At(1)))->GetString().Atof();
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Fit_residual_cuts_per_chamber=")) {
      TObjArray * l = Line.Tokenize(" ");
      if (l->GetEntriesFast()>5) units = NA62Utilities::GetInstance()->GetUnitFromString((TString)((TObjString*)(l->At(5)))->GetString());
      else { cout << "WARNING: Fit_residual_cuts_per_chamber in Spectrometer.conf without units" << endl; exit(kWrongConfiguration); }
      if (!units) exit(kWrongConfiguration);
      fCutDyFit[0] = ((TObjString*)(l->At(1)))->GetString().Atof()*(units/mm);
      fCutDyFit[1] = ((TObjString*)(l->At(2)))->GetString().Atof()*(units/mm);
      fCutDyFit[2] = ((TObjString*)(l->At(3)))->GetString().Atof()*(units/mm);
      fCutDyFit[3] = ((TObjString*)(l->At(4)))->GetString().Atof()*(units/mm);
      nItems++;
      delete l;
    } else if(Line.BeginsWith("Histo_Monitor=")) {
      TObjArray * l = Line.Tokenize(" ");
      fHistoDebug = ((TObjString*)(l->At(1)))->GetString().Atoi();
      nItems++;
      delete l;
    }
  }
  confFile.close();

}

void SpectrometerParameters::ReadMagicT0(){
/// \MemberDescr
/// Read Magic T0 offset from the dedicated file
/// \EndMemberDescr
  if (NA62ConditionsService::GetInstance()->Open(fMagicT0FileName)!=kSuccess) return;
  TString Line;
      while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fMagicT0FileName))) {
    if (Line.BeginsWith("#")) continue;
    stringstream ss;
    ss << Line;
    Double_t MagicT0, RecoWire, RecoWireSigma;
    ss >> MagicT0 >> RecoWire >> RecoWireSigma;
    fMagicT0 = MagicT0;
  }
  NA62ConditionsService::GetInstance()->Close(fMagicT0FileName);
}

void SpectrometerParameters::SetAllT0s()
{
/// \MemberDescr
/// T0:
/// \n
/// 7808 Relative channel T0 (ns)
/// \EndMemberDescr

  if (fIsRawData) {
    // Straw relative t0
    if (NA62ConditionsService::GetInstance()->Open(fT0FileName)!=kSuccess) return;
    int format=0; // 0 - private format, 1 - T0 evaluation tool format
    string line;
    while(getline(NA62ConditionsService::GetInstance()->Get(fT0FileName),line)) {
      stringstream ss;
      ss.str(line);
      bool found = (line.find("#") != string::npos);
      if (found) format=1;
      Int_t planeid;
      Int_t strawid;
      if(format){
        if(!found){
          Double_t t0;
          int inro, index;
          if(ss >> inro >> index >> t0){
            planeid = index/122;
            strawid = (index%122) + 1;
            if(fabs(t0)<999.0){
              fChannelT0[planeid][strawid] = t0 - 7.875 ; // subtract the temporary magic number for the given RT
            }
          }
        }
      } else {
        Double_t coarset0;
        Double_t middlet0;
        Double_t refinet0;
        Double_t finalt0;
        if(ss >> planeid >> strawid >> coarset0 >> middlet0 >> refinet0 >> finalt0){
          fChannelT0[planeid][strawid] = coarset0+middlet0-refinet0-finalt0;
        }
      }
    }
    NA62ConditionsService::GetInstance()->Close(fT0FileName);
  }
}

void SpectrometerParameters::SetAlignment(){
  /// \MemberDescr
  /// Alignment constants.
  /// Alignment in x, files are NA62Reconstruction/config/Spectrometer-Alignment... and Spectrometer-StationsAlignment
  /// \EndMemberDescr
  // Alignments for data
  if (fIsRawData) {

    // Chamber alignment
    NA62ConditionsService::GetInstance()->Open(fStationsAlignmentFileName);
    while(!NA62ConditionsService::GetInstance()->Get(fStationsAlignmentFileName).eof()) {
      Int_t chid;
      Int_t vid;
      Double_t xoff1;
      Double_t xoff2;
      NA62ConditionsService::GetInstance()->Get(fStationsAlignmentFileName) >> chid >> vid >> xoff1 >> xoff2;
      fXAlignment[chid][vid] = xoff1+xoff2;
      if (chid==3&&vid==3) break;
    }
    NA62ConditionsService::GetInstance()->Close(fStationsAlignmentFileName);

    // Straw alignment
    NA62ConditionsService::GetInstance()->Open(fAlignmentFileName);
    while(!NA62ConditionsService::GetInstance()->Get(fAlignmentFileName).eof()) {
      Int_t planeid;
      Int_t strawid;
      Double_t align1 = 0;
      Double_t align2 = 0;
      Double_t align3 = 0;
      NA62ConditionsService::GetInstance()->Get(fAlignmentFileName) >> planeid >> strawid >> align1 >> align2 >> align3;
      fXStrawAlignment[planeid][strawid] = align1+align2+align3;
      if (planeid==63 && strawid==122) break;
    }
    NA62ConditionsService::GetInstance()->Close(fAlignmentFileName);
  }
}

void SpectrometerParameters::SetZPositionCorrection(){
/// \MemberDescr
/// Z Position corrections according to the survey measurements.
///
/// MNP33 center: Measurement from EDMS:
/// https://edms.cern.ch/file/1317176/1/NA62-MNP33_carriage_at_zero_position.pdf
/// Chambers: Measurement from EDMS survey straws under vacuum
/// https://edms.cern.ch/file/1421896/1/NA62_Measurements_of_the_STRAW_detectors_1-4.pdf
///
/// \EndMemberDescr
  NA62ConditionsService::GetInstance()->Open(fZCorrectionFileName);
  NA62ConditionsService::GetInstance()->Get(fZCorrectionFileName)>> fZMagnetCorrection;
  for (Int_t j=0; j<4; j++)
    NA62ConditionsService::GetInstance()->Get(fZCorrectionFileName) >> fZChamberCorrection[j];
  NA62ConditionsService::GetInstance()->Close(fZCorrectionFileName);
}

void SpectrometerParameters::SetBIntegral(){
/// \MemberDescr
/// MNP33 Integral Bxy (X,Y) derived from the 2013 B measurements
/// assuming straight tracks.
/// \EndMemberDescr

  // By integral
  NA62ConditionsService::GetInstance()->Open(fByIntegralFileName);
  Int_t k=0;
  while(!NA62ConditionsService::GetInstance()->Get(fByIntegralFileName).eof()) {
    Double_t xcor;
    Double_t ycor;
    NA62ConditionsService::GetInstance()->Get(fByIntegralFileName) >> xcor >> ycor >> fByIntegral[k];
    fByIntegral[k] *=1000.;
    k++; 
  }
  NA62ConditionsService::GetInstance()->Close(fByIntegralFileName);

  // Bx integral
  NA62ConditionsService::GetInstance()->Open(fBxIntegralFileName);
  k=0;
  while(!NA62ConditionsService::GetInstance()->Get(fBxIntegralFileName).eof()) {
    Double_t xcor;
    Double_t ycor;
    NA62ConditionsService::GetInstance()->Get(fBxIntegralFileName) >> xcor >> ycor >> fBxIntegral[k];
    fBxIntegral[k] *=1000.;
    k++; 
  }
  NA62ConditionsService::GetInstance()->Close(fBxIntegralFileName);
}

void SpectrometerParameters::SetRT(){
/// \MemberDescr
/// Parameters from fit of RT histos channel by channel derived as integral function of the
/// drift times measured on the 2015 muon run. Index 1 equals straw 1.
/// \EndMemberDescr

  NA62ConditionsService::GetInstance()->Open(fRTFileName);
  int q = 0;
  string b;
  char *pend[13];
  while(!NA62ConditionsService::GetInstance()->Get(fRTFileName).eof()){
    getline(NA62ConditionsService::GetInstance()->Get(fRTFileName),b,'\n');
    if(NA62ConditionsService::GetInstance()->Get(fRTFileName).eof()) break;
    fRTH[q][0] = strtod(b.c_str(),&pend[0]);
    for(int i = 1; i < 13; i++){
      fRTH[q][i] = strtod(pend[i-1],&pend[i]);
    }
    q++;
  }
  NA62ConditionsService::GetInstance()->Close(fRTFileName);
}

void SpectrometerParameters::SetXT(){
/// \MemberDescr
/// Parameters from piecewise fit of XT (HTimeCorPerPlane 0-15) histos, view by view
/// Fits are of zeroth and first order
/// \EndMemberDescr

  NA62ConditionsService::GetInstance()->Open(fXTFileName);
  int q = 0;
  string b;
  char *pend[7];
  while(!NA62ConditionsService::GetInstance()->Get(fXTFileName).eof()){
    getline(NA62ConditionsService::GetInstance()->Get(fXTFileName),b,'\n');
    if(NA62ConditionsService::GetInstance()->Get(fXTFileName).eof()) break;
    if (b.compare(0, 1, "#") == 0) cout << "Commented out line: " << b << endl;
    if(q>=16) {
      cerr << "[SpectrometerParameter] q overflow!" << endl;
      break;
    }
    fXTH[q][0] = strtod(b.c_str(),&pend[0]);
    for(int i = 1; i < 7; i++){
      fXTH[q][i] = strtod(pend[i-1],&pend[i]);
    }
    q++;
  }
  NA62ConditionsService::GetInstance()->Close(fXTFileName);
}

Double_t SpectrometerParameters::GetRTDependence(Double_t Time){
/// \MemberDescr
/// RT dependence for MC in case of full digitization 
/// \EndMemberDescr

    Double_t Time2 = Time * Time;
    if(Time < 0)
        return -(- 0.072766
                + 0.1665678   * Time
                - 0.005184744 * Time2
                + 9.83302e-05 * Time2 * Time
                - 6.97656e-07 * Time2 * Time2
                );
    else if(Time < 20)
        return (- 0.072766
                + 0.1665678   * Time
                - 0.005184744 * Time2
                + 9.83302e-05 * Time2 * Time
                - 6.97656e-07 * Time2 * Time2
               );
    else
        return (  0.861513
                + 0.0559724   * Time
                - 0.000197694 * Time2
               );
}

Double_t SpectrometerParameters::GetRTDependenceData(Double_t tt){
/// \MemberDescr
/// RT dependence for data derived from Garfield
/// \EndMemberDescr

  Double_t par[5] = {-2.29722e+00,
                      7.63831e+02,
                      8.07073e-03,
                      2.25652e+01,
                     -5.89754e+01};
  Double_t r = par[0]+sqrt(fabs(par[1]*(tt-par[2])+par[4]*(tt-par[2])*(tt-par[2])))-par[3]*tt;

  if (r<0) r = 0;

  return r;
}

Double_t SpectrometerParameters::GetRTParametricDependence(Double_t tt) {
/// \MemberDescr
/// RT dependence for MC in case of parametric digitization
/// \EndMemberDescr

  Double_t pol[10];
  pol[0] =      1.40694;
  pol[1] =     -288.673;
  pol[2] =      22372.4;
  pol[3] =      -724749;
  pol[4] =  1.38739e+07;
  pol[5] = -1.69746e+08;
  pol[6] =  1.34466e+09;
  pol[7] = -6.68268e+09;
  pol[8] =  1.89464e+10;
  pol[9] = -2.33756e+10;

  Double_t f1 = 0;
  Double_t d = 1;

  if (tt<0.02) {
    Double_t val = 0;
    for (Int_t j=0; j<10; j++) {
      val += pol[j]*d;
      d *= 0.02;
    }
    Double_t b = val/(0.02-0.0155);
    Double_t a = -0.0155*b;
    f1 = a+b*tt;
    if (f1<0) f1 = 0;
  }

  if (tt>=0.02 && tt<=0.09) {
    for (Int_t j=0; j<10; j++) {
      f1 += pol[j]*d;
      d *= tt;
    }
  }

  if (tt>0.09 && tt<=0.115) {
    Double_t val = 0;
    for (Int_t j=0; j<10; j++) {
      val += pol[j]*d;
      d *= 0.09;
    }
    Double_t b = (4.26-val)/(0.115-0.09);
    Double_t a = 4.26-0.115*b;
    f1 = a+b*tt;
  }

  if (tt>0.115 && tt<=0.15) {
    Double_t b = (4.82767-4.26)/(0.15-0.115);
    Double_t a = 4.82767-0.15*b;
    f1 = a+b*tt;
  }

  if (tt>0.15) {
    f1 = 4.82767 ;
  }

  Double_t r = f1-0.017;

  return r>0?r:0;
}

Double_t SpectrometerParameters::GetRTDependenceDataFull(Int_t sid, Double_t tt, Double_t /*t0*/)
{
/// \MemberDescr
/// NOTE! GLOBAL STRAW ID 1 to 7808
/// RT dependence for data measured on data from the integral distribution of the drifttime 
/// \n
//  To be debugged yet
/// \EndMemberDescr

  // Int_t bin = (Int_t)((tt+18.4)/2);
  // if (bin>=100) return 4.9;
  // if (bin<0) return 0.;
  // Float_t tgrid = -18.4+2*bin;
  // Float_t dt = tt-tgrid;
  // if (IsBadChannel(pid,sid)) {
  //   pid = 63;
  //   sid = 95;
  // }
  // Float_t rgrid = GetRTH(pid,sid,bin);
  // if (rgrid==-9999.) return GetRTDependenceData(tt/1000.);
  // Float_t rgridnext = bin<99 ? GetRTH(pid,sid,bin+1) : rgrid;
  // Float_t radius = rgrid*(1-dt)+rgridnext*dt;
  // Float_t weight = 0.85+0.03*radius;
  // radius *= weight;
  //
  // return rgrid;
  // double **fRTH = SetRT(); //Index 1 is first straw
  //Double_t t0ref= 1e-3; // microseconds, read this from t0 file of run 3809, 3821, 3810 or 3818
  //Double_t displacement = t0-t0ref; //if displacement > 0 graph R-T is moved right, if displacement < 0 R-T is moved left
  Double_t xv = tt/1000;
  if((sid >= 2758 && sid <= 2765) || (sid >= 2880 && sid <= 2887)){
    return ((xv<0.03)*(fRTH[sid][0]+xv*fRTH[sid][1]+xv*xv*fRTH[sid][2]+xv*xv*xv*fRTH[sid][3]+xv*xv*xv*xv*fRTH[sid][4])
            +(xv>=0.03 && xv<=0.09)*(fRTH[sid][5]+xv*fRTH[sid][6]+xv*xv*fRTH[sid][7]+xv*xv*xv*fRTH[sid][8])
            +(xv>0.09)*(fRTH[sid][9]+xv*fRTH[sid][10]+xv*xv*fRTH[sid][11]+xv*xv*xv*fRTH[sid][12]));
  } else if (fRTH[sid][0] == 0 && fRTH[sid][1] == 0) {
    return GetRTDependenceData(tt/1000);
  } else {
    return ((xv<0.02)*(fRTH[sid][0]+xv*fRTH[sid][1]+xv*xv*fRTH[sid][2]+xv*xv*xv*fRTH[sid][3]+xv*xv*xv*xv*fRTH[sid][4])
            +(xv>=0.02 && xv<=0.09)*(fRTH[sid][5]+xv*fRTH[sid][6]+xv*xv*fRTH[sid][7]+xv*xv*xv*fRTH[sid][8])
            +(xv>0.09)*(fRTH[sid][9]+xv*fRTH[sid][10]+xv*xv*fRTH[sid][11]+xv*xv*xv*fRTH[sid][12]));
  }
}

Double_t SpectrometerParameters::GetXTDependence(Int_t viewID, Double_t orthcoord) {
    // \MemberDescr
    // Correction of the orthogonal hit dependence on the drift time
    // \EndMemberDescr
  if (viewID == 0 || viewID == 4 || viewID == 8 || viewID == 12){
    return (orthcoord >= -1050 && orthcoord <= -200)*(fXTH[viewID][1]-fXTH[viewID][0] + fXTH[viewID][2]*orthcoord) + (orthcoord > -200 && orthcoord < 400)    *(fXTH[viewID][3]-fXTH[viewID][0] + fXTH[viewID][4]*orthcoord) + (orthcoord >= 400 && orthcoord <= 1050)  *(fXTH[viewID][5]-fXTH[viewID][0] + fXTH[viewID][6]*orthcoord);
  } else if (viewID == 1 || viewID == 5 || viewID == 9 || viewID == 13){
    return (orthcoord >= -1050 && orthcoord <= -900)*(fXTH[viewID][1]-fXTH[viewID][0]) + (orthcoord >= -900 && orthcoord <= 1050) *(fXTH[viewID][3]-fXTH[viewID][0] + fXTH[viewID][4]*orthcoord);
  } else if (viewID == 2 || viewID == 6 || viewID == 10 || viewID == 14){
    return (orthcoord >= -1050 && orthcoord <= -600)*(fXTH[viewID][1]-fXTH[viewID][0] + fXTH[viewID][2]*orthcoord) + (orthcoord > -600 && orthcoord < 500)    *(fXTH[viewID][3]-fXTH[viewID][0] + fXTH[viewID][4]*orthcoord) + (orthcoord >= 500 && orthcoord <= 1050)  *(fXTH[viewID][5]-fXTH[viewID][0] + fXTH[viewID][6]*orthcoord);
  } else {// if (viewID == 3 || viewID == 7 || viewID == 11 || viewID == 15){
    return (orthcoord >= -1050 && orthcoord <= -400)*(fXTH[viewID][1]-fXTH[viewID][0] + fXTH[viewID][2]*orthcoord) + (orthcoord > -400 && orthcoord < -100)   *(fXTH[viewID][3]-fXTH[viewID][0] + fXTH[viewID][4]*orthcoord) + (orthcoord >= -100 && orthcoord <= 1050) *(fXTH[viewID][5]-fXTH[viewID][0] + fXTH[viewID][6]*orthcoord);
  }
}
Bool_t SpectrometerParameters::IsBadChannel(Int_t pid, Int_t sid)
{
/// \MemberDescr
/// Bad channels for the RT dependence measured on data
/// \EndMemberDescr

  if (pid==44) {
    if (sid>=114&&sid<=120) return true;
  }
  if (pid==45) {
    if (sid>=115&&sid<=120) return true;
  }
  if (pid==48) {
    if (sid>=67&&sid<=73) return true;
  }
  if (pid==49) {
    if (sid>=68&&sid<=74) return true;
  }
  if (pid==60) {
    if (sid==57) return true;
    if (sid==58) return true;
  }
  if (pid==61) {
    if (sid==58) return true;
    if (sid==59) return true;
  }
  if (pid==62) {
    if (sid>=106&&sid<=113) return true;
  }
  if (pid==63) {
    if (sid>=106&&sid<=113) return true;
  }
  return false;
}

void SpectrometerParameters::SetBMNP33(){
  // Read the measurements
  NA62ConditionsService::GetInstance()->Open(fBMNP33FileName);
  //cout << "Loading MNP33 B measurements.............." << endl;
  Int_t k=0;
  while(!NA62ConditionsService::GetInstance()->Get(fBMNP33FileName).eof()) {
    Double_t xmnp33;
    Double_t ymnp33;
    Double_t zmnp33;
    Double_t bxmnp33;
    Double_t bymnp33;
    Double_t bzmnp33;
    NA62ConditionsService::GetInstance()->Get(fBMNP33FileName) >> xmnp33 >> ymnp33 >> zmnp33 >> bxmnp33 >> bymnp33 >> bzmnp33;
    Int_t jz = k%169;
    if (k<169) fZMNP33[jz] = zmnp33;
////    xmnp33 = -xmnp33;
////    ymnp33 = -ymnp33;
    Int_t jx = (xmnp33+1000)/80;
    Int_t jy = (ymnp33+1000)/80;
    fBxMNP33[jx][jy][jz] = 1000*bxmnp33; 
    fByMNP33[jx][jy][jz] = 1000*bymnp33;
    fBzMNP33[jx][jy][jz] = 1000*bzmnp33;
    if (k==93963) break;
    k++; 
  }
  NA62ConditionsService::GetInstance()->Close(fBMNP33FileName);
  //cout << "### MNP33 B field loaded in memory ###" << endl;
}

void SpectrometerParameters::SetBFringeField(){
  NA62ConditionsService::GetInstance()->Open(fBFringeFileName);
  Int_t kk=0;
  Int_t kz = 0;
  while(!NA62ConditionsService::GetInstance()->Get(fBFringeFileName).eof()) {
    Int_t kxy = kk%29;
    Double_t xpos;
    Double_t ypos;
    Double_t zpos;
    NA62ConditionsService::GetInstance()->Get(fBFringeFileName) >> xpos >> ypos >> zpos >> fby[kz][kxy] >> fbx[kz][kxy] >> fbz[kz][kxy];
    if (kxy==28) { 
      if (kz<15) fZFringe[kz] = zpos-196995;
      else fZFringe[kz] = 196995-zpos;
    }
    if (kxy==28 && kz==29) break;
    if (kxy==28) kz++;
    kk++;     
  }
  NA62ConditionsService::GetInstance()->Close(fBFringeFileName);

  for (Int_t jz=0; jz<30; jz++) {
    for (Int_t jx=0; jx<17; jx++) {
      for (Int_t jy=0; jy<17; jy++) {
        if (jx<  3 && jy<  3) continue;
        if (jx> 13 && jy<  3) continue;
        if (jx> 13 && jy> 13) continue;
        if (jx<  3 && jy> 13) continue; 

        if (jy>= 0 && jy<= 3) { if (jx>= 3 && jx<= 8) { Int_t idarray[9] = {jx,jy,jz, 3, 0,23,24,13,14}; FillFringeField(idarray,500,300); }  // External bottom
                                if (jx>= 8 && jx<=13) { Int_t idarray[9] = {jx,jy,jz, 8, 0,24,25,14,15}; FillFringeField(idarray,500,300); }}
        if (jy>=13 && jy<=16) { if (jx>= 3 && jx<= 8) { Int_t idarray[9] = {jx,jy,jz, 3,13,11,12,19,20}; FillFringeField(idarray,500,300); }  // External up
                                if (jx>= 8 && jx<=13) { Int_t idarray[9] = {jx,jy,jz, 8,13,10, 9,18,17}; FillFringeField(idarray,500,300); }}
        if (jx>= 0 && jx<= 3) { if (jy>= 3 && jy<= 8) { Int_t idarray[9] = {jx,jy,jz, 0, 3,22,13,21,12}; FillFringeField(idarray,300,500); }  // External left
                                if (jy>= 8 && jy<=13) { Int_t idarray[9] = {jx,jy,jz, 0, 8,21,12,20,11}; FillFringeField(idarray,300,500); }}
        if (jx>=13 && jx<=16) { if (jy>= 3 && jy<= 8) { Int_t idarray[9] = {jx,jy,jz,13, 3,15,26,16,27}; FillFringeField(idarray,300,500); }  // External right
                                if (jy>= 8 && jy<=13) { Int_t idarray[9] = {jx,jy,jz,13, 8,16,27, 9,28}; FillFringeField(idarray,300,500); }}
      }
    }
    for (Int_t jx=0; jx<17; jx++) {
      for (Int_t jy=0; jy<17; jy++) {
        if (jx>= 6 && jx<  8) { if (jy>= 6 && jy<  8) { Int_t idarray[9] = {jx,jy,jz, 6, 6, 4, 5, 3, 8}; FillFringeField(idarray,200,200); }  // Inner column left 
                                if (jy>= 8 && jy< 10) { Int_t idarray[9] = {jx,jy,jz, 6, 8, 3, 8, 2, 1}; FillFringeField(idarray,200,200); }
                                if (jy>  3 && jy<  6) { Int_t idarray[9] = {jx,jy,jz, 6, 3,-1,14, 4, 5}; FillFringeField(idarray,200,300); }
                                if (jy>=10 && jy< 13) { Int_t idarray[9] = {jx,jy,jz, 6,10, 2, 1,-1,10}; FillFringeField(idarray,200,300); }} 
        if (jx>= 8 && jx< 10) { if (jy>= 6 && jy<  8) { Int_t idarray[9] = {jx,jy,jz, 8, 6, 5, 6, 8, 7}; FillFringeField(idarray,200,200); }  // Inner column right
                                if (jy>= 8 && jy< 10) { Int_t idarray[9] = {jx,jy,jz, 8, 8, 8, 7, 1, 0}; FillFringeField(idarray,200,200); }
                                if (jy>  3 && jy<  6) { Int_t idarray[9] = {jx,jy,jz, 8, 3,14,-1, 5, 6}; FillFringeField(idarray,200,300); }
                                if (jy>=10 && jy< 13) { Int_t idarray[9] = {jx,jy,jz, 8,10, 1, 0,10,-1}; FillFringeField(idarray,200,300); }} 
        if (jx>= 3 && jx<  6) { if (jy>= 6 && jy<  8) { Int_t idarray[9] = {jx,jy,jz, 3, 6,-1, 4,12, 3}; FillFringeField(idarray,300,200); }  // Middle column left
                                if (jy>= 8 && jy< 10) { Int_t idarray[9] = {jx,jy,jz, 3, 8,12, 3,-1, 2}; FillFringeField(idarray,300,200); }
                                if (jy>  3 && jy<  6) { Int_t idarray[9] = {jx,jy,jz, 3, 3,13,-1,-1, 4}; FillFringeField(idarray,300,300); }
                                if (jy>=10 && jy< 13) { Int_t idarray[9] = {jx,jy,jz, 3,10,-1, 2,11,-1}; FillFringeField(idarray,300,300); }}
        if (jx>=10 && jx< 13) { if (jy>= 6 && jy<  8) { Int_t idarray[9] = {jx,jy,jz,10, 6, 6,-1, 7,16}; FillFringeField(idarray,300,200); }  // Middle column right
                                if (jy>= 8 && jy< 10) { Int_t idarray[9] = {jx,jy,jz,10, 8, 7,16, 0,-1}; FillFringeField(idarray,300,200); }
                                if (jy>  3 && jy<  6) { Int_t idarray[9] = {jx,jy,jz,10, 3,-1,15, 6,-1}; FillFringeField(idarray,300,300); }
                                if (jy>=10 && jy< 13) { Int_t idarray[9] = {jx,jy,jz,10,10, 0,-1,-1, 9}; FillFringeField(idarray,300,300); }}
      }
    }
  }

}

void SpectrometerParameters::FillFringeField(Int_t *idarray, Double_t passx, Double_t passy)
{
  Int_t jx = idarray[0];
  Int_t jy = idarray[1];
  Int_t jz = idarray[2];
  Int_t cx = idarray[3];
  Int_t cy = idarray[4];
  Int_t nid[4];
  nid[0] = idarray[5];
  nid[1] = idarray[6];
  nid[2] = idarray[7];
  nid[3] = idarray[8];
  Double_t b1x = nid[0]>=0 ? fbx[jz][nid[0]] : 0.; 
  Double_t b2x = nid[1]>=0 ? fbx[jz][nid[1]] : 0.; 
  Double_t b3x = nid[2]>=0 ? fbx[jz][nid[2]] : 0.; 
  Double_t b4x = nid[3]>=0 ? fbx[jz][nid[3]] : 0.; 
  Double_t b1y = nid[0]>=0 ? fby[jz][nid[0]] : 0.; 
  Double_t b2y = nid[1]>=0 ? fby[jz][nid[1]] : 0.; 
  Double_t b3y = nid[2]>=0 ? fby[jz][nid[2]] : 0.; 
  Double_t b4y = nid[3]>=0 ? fby[jz][nid[3]] : 0.; 
  Double_t b1z = nid[0]>=0 ? fbz[jz][nid[0]] : 0.; 
  Double_t b2z = nid[1]>=0 ? fbz[jz][nid[1]] : 0.; 
  Double_t b3z = nid[2]>=0 ? fbz[jz][nid[2]] : 0.; 
  Double_t b4z = nid[3]>=0 ? fbz[jz][nid[3]] : 0.; 
  if (nid[0]==-1) {
    b1x = fBxFringe[cx][cy][jz]*1000;
    b1y = fByFringe[cx][cy][jz]*1000;
    b1z = fBzFringe[cx][cy][jz]*1000;
  }
  if (nid[1]==-1) {
    if (cx==8 && cy==3) {
      b2x = fBxFringe[cx+2][cy][jz]*1000;
      b2y = fByFringe[cx+2][cy][jz]*1000;
      b2z = fBzFringe[cx+2][cy][jz]*1000;
    } else {
      b2x = fBxFringe[cx+3][cy][jz]*1000;
      b2y = fByFringe[cx+3][cy][jz]*1000;
      b2z = fBzFringe[cx+3][cy][jz]*1000;
    } 
  }
  if (nid[2]==-1) {
    if (cx==3 && cy==8) {
      b3x = fBxFringe[cx][cy+2][jz]*1000;
      b3y = fByFringe[cx][cy+2][jz]*1000;
      b3z = fBzFringe[cx][cy+2][jz]*1000;
    } else {
      b3x = fBxFringe[cx][cy+3][jz]*1000;
      b3y = fByFringe[cx][cy+3][jz]*1000;
      b3z = fBzFringe[cx][cy+3][jz]*1000;
    } 
  }
  if (nid[3]==-1) {
    if (cx==8 && cy==10) {
      b4x = fBxFringe[cx+2][cy+3][jz]*1000;
      b4y = fByFringe[cx+2][cy+3][jz]*1000;
      b4z = fBzFringe[cx+2][cy+3][jz]*1000;
    } else if (cx==10 && cy==8) {
      b4x = fBxFringe[cx+3][cy+2][jz]*1000;
      b4y = fByFringe[cx+3][cy+2][jz]*1000;
      b4z = fBzFringe[cx+3][cy+2][jz]*1000;
    } else {
      b4x = fBxFringe[cx+3][cy+3][jz]*1000;
      b4y = fByFringe[cx+3][cy+3][jz]*1000;
      b4z = fBzFringe[cx+3][cy+3][jz]*1000;
    } 
  } 

  Double_t xgrid = -800+100*jx;
  Double_t ygrid = -800+100*jy;
  Double_t xcorner = -800+100*cx;
  Double_t ycorner = -800+100*cy;
  Double_t dx = fabs(xgrid-xcorner)/passx;
  Double_t dy = fabs(ygrid-ycorner)/passy;
  fBxFringe[jx][jy][jz] = ((1-dx-dy+dx*dy)*b1x+(dx-dx*dy)*b2x+(dy-dx*dy)*b3x+dx*dy*b4x)/1000;
  fByFringe[jx][jy][jz] = ((1-dx-dy+dx*dy)*b1y+(dx-dx*dy)*b2y+(dy-dx*dy)*b3y+dx*dy*b4y)/1000;
  fBzFringe[jx][jy][jz] = ((1-dx-dy+dx*dy)*b1z+(dx-dx*dy)*b2z+(dy-dx*dy)*b3z+dx*dy*b4z)/1000;
//  if (jx==6 && jy==11) cout << "asdrubale " << nid[2] << " " << cx << " " << cy << " " << fByFringe[cx][cy][jz] << " " << dx << " " << dy << " " << b1y << " " << b2y << " " << b3y << " " << b4y << endl; 
}
