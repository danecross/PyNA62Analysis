// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-03-08
//
// ---------------------------------------------------------------

/// \class RICHParameters
/// \Brief
/// Interface to the RICH parameters database
/// \EndBrief
/// \Detailed
/// Interface to the RICH mirror positions and electron ring parameters database.
/// Example of use:
/// \code
/// #include "RICHParameters.hh"
/// ...
/// Double_t x  = RICHParameters::GetInstance()->GetMirrorCornerX(iMirror, iCorner);
/// Double_t x0 = RICHParameters::GetInstance()->GetMirrorCentreX(iMirror);
/// Bool_t saleve = RICHParameters::GetInstance()->SaleveMirror(iMirror);
/// Bool_t jura   = RICHParameters::GetInstance()->JuraMirror(iMirror);
/// \endcode
/// \author Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
/// \EndDetailed

#include "RICHParameters.hh"
#include "ConfigSettings.hh"
#include "NA62Exceptions.hh"
#include "NA62ConditionsService.hh"

using namespace std;
using namespace NA62Analysis;

static RICHParameters* fInstance = 0;

RICHParameters* RICHParameters::GetInstance() {
  if (!fInstance) fInstance = new RICHParameters();
  return fInstance;
}

RICHParameters::RICHParameters() :
  fMirrorPositionsInputFileName("RICH-MirrorPositions.dat"),
  fMirrorAlignmentInputFileName("RICH-MirrorAlignment.dat"),
  fElectronRingInputFileName("RICH-ElectronRing.dat") {
  for (Int_t i=0; i<25; i++) {
    for (Int_t j=0; j<6; j++) {
      fX[i][j] = fY[i][j] = 0.0;
    }
  }
  ParseMirrorPositionsInputFile();
  ParseMirrorAlignmentInputFile();
  ParseElectronRingInputFile();

  // Mapping of sequential to non-sequential mirror IDs.
  // Mirrors 0, 2, 7, 18, 19 in non-sequential numbering (0-24) do not exist.
  fMirrorNumber[0]  =  9;
  fMirrorNumber[1]  = 17;
  fMirrorNumber[2]  =  4;
  fMirrorNumber[3]  = 10;
  fMirrorNumber[4]  = 20;
  fMirrorNumber[5]  = 12;
  fMirrorNumber[6]  = 21;
  fMirrorNumber[7]  =  6;
  fMirrorNumber[8]  =  5;
  fMirrorNumber[9]  =  3;
  fMirrorNumber[10] =  1;
  fMirrorNumber[11] = 11;
  fMirrorNumber[12] = 14;
  fMirrorNumber[13] = 13;
  fMirrorNumber[14] = 16;
  fMirrorNumber[15] =  8;
  fMirrorNumber[16] = 22;
  fMirrorNumber[17] = 15;
  fMirrorNumber[18] = 23;
  fMirrorNumber[19] = 24;
}

////////////////////////////////
// Read nominal mirror positions

void RICHParameters::ParseMirrorPositionsInputFile() {

  TString Line;
  Int_t NEntriesRead = 0;
  NA62ConditionsService::GetInstance()->Open(fMirrorPositionsInputFileName);
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fMirrorPositionsInputFileName))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    Int_t    i = ((TObjString*)(l->At(0)))->GetString().Atoi();
    Int_t    j = ((TObjString*)(l->At(1)))->GetString().Atoi();
    Double_t x = ((TObjString*)(l->At(2)))->GetString().Atof();
    Double_t y = ((TObjString*)(l->At(3)))->GetString().Atof();
    delete l;
    fX[i][j] = x;
    fY[i][j] = y;
    NEntriesRead++;
  }
  NA62ConditionsService::GetInstance()->Close(fMirrorPositionsInputFileName);
  cout << "[RICHParameters] Found " << NEntriesRead << " entries in " << fMirrorPositionsInputFileName << endl;
}

//////////////////////////////////////////////////
// Read mirror alignment parameters for run ranges

void RICHParameters::ParseMirrorAlignmentInputFile() {
  fAlignmentParameters.clear();
  TString Line;
  RICHAlignmentParameters par;
  par.MirrorAlignment[0][0] = par.MirrorAlignment[0][1] = 0.0; // these are unused, resetting them
  NA62ConditionsService::GetInstance()->Open(fMirrorAlignmentInputFileName);
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fMirrorAlignmentInputFileName))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    par.FirstRun = ((TObjString*)(l->At(0)))->GetString().Atoi();
    par.LastRun  = ((TObjString*)(l->At(1)))->GetString().Atoi();
    l->Delete();
    delete l;
    Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fMirrorAlignmentInputFileName));
    l = Line.Tokenize(" ");
    par.ExtraOffsetX[0] = ((TObjString*)(l->At(0)))->GetString().Atof();
    par.ExtraOffsetY[0] = ((TObjString*)(l->At(1)))->GetString().Atof();
    par.ExtraOffsetX[1] = ((TObjString*)(l->At(2)))->GetString().Atof();
    par.ExtraOffsetY[1] = ((TObjString*)(l->At(3)))->GetString().Atof();
    l->Delete();
    delete l;
    Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fMirrorAlignmentInputFileName));
    l = Line.Tokenize(" ");
    for (Int_t m=1; m<25; m++) { // read 24 x-coordinates
      par.MirrorAlignment[m][0] = ((TObjString*)(l->At(m-1)))->GetString().Atof();
    }
    l->Delete();
    delete l;
    Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fMirrorAlignmentInputFileName));
    l = Line.Tokenize(" ");
    for (Int_t m=1; m<25; m++) { // read 24 y-coordinates
      par.MirrorAlignment[m][1] = ((TObjString*)(l->At(m-1)))->GetString().Atof();
    }
    l->Delete();
    delete l;
    fAlignmentParameters.push_back(par);
  }
  NA62ConditionsService::GetInstance()->Close(fMirrorAlignmentInputFileName);
  cout << "[RICHParameters] Found " << fAlignmentParameters.size() << " run ranges in " << fMirrorAlignmentInputFileName << endl;
}

void RICHParameters::ParseElectronRingInputFile() {
  fFirstRun.clear();
  fLastRun.clear();
  fRadiusOffset.clear();
  fRadiusSlope.clear();
  fNHitsOffset.clear();
  fNHitsSlope.clear();

  TString Line;
  Int_t NRunRanges = 0;
  NA62ConditionsService::GetInstance()->Open(fElectronRingInputFileName);
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fElectronRingInputFileName))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l1 = Line.Tokenize(" ");
    fFirstRun.push_back(((TObjString*)(l1->At(0)))->GetString().Atoi());
    fLastRun.push_back (((TObjString*)(l1->At(1)))->GetString().Atoi());
    delete l1;
    Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fElectronRingInputFileName));
    TObjArray *l2 = Line.Tokenize(" ");
    fRadiusOffset.push_back(((TObjString*)(l2->At(0)))->GetString().Atof());
    fRadiusSlope.push_back (((TObjString*)(l2->At(1)))->GetString().Atof());
    delete l2;
    Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fElectronRingInputFileName));
    TObjArray *l3 = Line.Tokenize(" ");
    fNHitsOffset.push_back(((TObjString*)(l3->At(0)))->GetString().Atof());
    fNHitsSlope.push_back (((TObjString*)(l3->At(1)))->GetString().Atof());
    delete l3;
    NRunRanges++;
  }
  NA62ConditionsService::GetInstance()->Close(fElectronRingInputFileName);
  cout <<"[RICHParameters] Found " << NRunRanges << " run ranges in " << fElectronRingInputFileName << endl;
}

///////////////////////////////////////////////////
// Functions to return the nominal mirror positions

Double_t RICHParameters::GetMirrorCentreX(Int_t iMirror) {
  if (iMirror!=23 && iMirror!=24) // hex mirrors
    return 0.25*(fX[iMirror][1]+fX[iMirror][2]+fX[iMirror][4]+fX[iMirror][5]);
  else // semihex mirrors
    return 0.25*(fX[iMirror][0]+fX[iMirror][1]+fX[iMirror][2]+fX[iMirror][3]);
}

Double_t RICHParameters::GetMirrorCentreY(Int_t iMirror) {
  if (iMirror!=23 && iMirror!=24) // hex mirrors
    return 0.25*(fY[iMirror][1]+fY[iMirror][2]+fY[iMirror][4]+fY[iMirror][5]);
  else // semihex mirrors
    return 0.25*(fY[iMirror][0]+fY[iMirror][1]+fY[iMirror][2]+fY[iMirror][3]);
}

Bool_t RICHParameters::SaleveMirror(Int_t iMirror) {
  return ((GetMirrorCentreX(iMirror)<0.0 && iMirror!=22 && iMirror!=23) ||
	  iMirror==17 || iMirror==24);
}

Bool_t RICHParameters::JuraMirror(Int_t iMirror) {
  return ((GetMirrorCentreX(iMirror)>0.0 && iMirror!=17 && iMirror!=24) ||
	  iMirror==22 || iMirror==23);
}

Bool_t RICHParameters::MirrorExists(Int_t iMirror) {
  return SaleveMirror(iMirror) || JuraMirror(iMirror);
}

///////////////////////////////////////
// Interface to the mirror alignment DB

Double_t RICHParameters::GetMirrorAlignmentConstant(Int_t RunNumber, Int_t i, Int_t j) {
  Int_t Range = -1;
  for (UInt_t iRange=0; iRange<fAlignmentParameters.size(); iRange++) {
    if (RunNumber>=fAlignmentParameters[iRange].FirstRun &&
	RunNumber<=fAlignmentParameters[iRange].LastRun) Range = iRange;
  }
  if (Range<0) {
    cout <<"[RICHParameters] Error: no mirror alignment info for run " << RunNumber << endl;
    return 0.0;
  }
  return fAlignmentParameters[Range].MirrorAlignment[i][j];
}

///////////////////////////////////////////
// Interface to electron ring parameters DB

Int_t RICHParameters::GetElectronRingRunRange(Int_t RunNumber) {
  Int_t RunRange = -1;
  for (UInt_t i=0; i<fFirstRun.size(); i++) {
    if (RunNumber>=fFirstRun[i] && RunNumber<=fLastRun[i]) RunRange = i;
  }
  return RunRange;
}

Double_t RICHParameters::GetElectronRingRadius(Int_t RunNumber, time_t BurstTime) {
  Int_t RunRange = GetElectronRingRunRange(RunNumber);
  if (RunRange<0) {
    cout <<"[RICHParameters] Error: no electron ring radius info for run " << RunNumber <<
      ", RICH association will fail" << endl;
    return 0.0;
  }
  tm *tms = localtime(&BurstTime);
  Double_t day = tms->tm_yday+(tms->tm_hour*60.+tms->tm_min)/1440.;
  return fRadiusOffset[RunRange] + fRadiusSlope[RunRange] * day;
}

Double_t RICHParameters::GetElectronRingNHits(Int_t RunNumber, time_t BurstTime) {
  Int_t RunRange = GetElectronRingRunRange(RunNumber);
  if (RunRange<0) {
    cout <<"[RICHParameters] Error: no electron ring N(hits) info for run " << RunNumber <<
      ", RICH association will fail" << endl;
    return 0.0;
  }
  tm *tms = localtime(&BurstTime);
  Double_t day = tms->tm_yday+(tms->tm_hour*60.+tms->tm_min)/1440.;
  return fNHitsOffset[RunRange] + fNHitsSlope[RunRange] * day;
}
