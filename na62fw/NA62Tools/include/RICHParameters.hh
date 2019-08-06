// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2016-10-13
//
// ---------------------------------------------------------------

#ifndef RICHPARAMETERS_HH
#define RICHPARAMETERS_HH

#include <fstream>
#include <iostream>
#include "TObjString.h"
#include "TObjArray.h"

struct RICHAlignmentParameters {
  Int_t FirstRun, LastRun;
  Double_t ExtraOffsetX[2];
  Double_t ExtraOffsetY[2];
  Double_t MirrorAlignment[25][2];
};

class RICHParameters {

public:

  static RICHParameters* GetInstance();

  Double_t GetMirrorCornerX (Int_t iMirror, Int_t iCorner) { return fX[iMirror][iCorner]; }
  Double_t GetMirrorCornerY (Int_t iMirror, Int_t iCorner) { return fY[iMirror][iCorner]; }
  Double_t GetMirrorCentreX (Int_t iMirror);
  Double_t GetMirrorCentreY (Int_t iMirror);

  Bool_t   SaleveMirror (Int_t iMirror);
  Bool_t   JuraMirror   (Int_t iMirror);
  Bool_t   MirrorExists (Int_t iMirror);
  Double_t GetMirrorNumber(Int_t i)
  { return (i>=0 && i<=19) ? fMirrorNumber[i] : -999; }

  Double_t GetMirrorAlignmentConstant(Int_t RunNumber, Int_t i, Int_t j);

  Double_t GetElectronRingRadius(Int_t, time_t);
  Double_t GetElectronRingNHits(Int_t, time_t);

  Double_t GetReferenceElectronRingRadius() { return 189.6457604861; } ///< CDB value for run 6610 [unit: mm]
  Double_t GetReferenceElectronRingNHits()  { return  12.8928556597; } ///< CDB value for run 6610

private:

  RICHParameters();
  ~RICHParameters() {}

  void ParseMirrorPositionsInputFile();
  void ParseMirrorAlignmentInputFile();
  void ParseElectronRingInputFile();

  TString fMirrorPositionsInputFileName; ///< Name of mirror positions DB file
  TString fMirrorAlignmentInputFileName; ///< Name of run-dependent mirror alignment DB file
  TString fElectronRingInputFileName;    ///< Name of electron ring DB file

  Double_t fX[25][6]; ///< X coordinates of the mirror corners [MirrorID, CornerID]
  Double_t fY[25][6]; ///< Y coordinates of the mirror corners [MirrorID, CornerID]
  Int_t    fMirrorNumber[20]; ///< Mapping of standard mirror numbers to contiguous numbers

  // Parameters of the mirror alignment database
  std::vector<RICHAlignmentParameters> fAlignmentParameters;

  // Parameters of the electron ring parameter database
  std::vector<Int_t> fFirstRun;
  std::vector<Int_t> fLastRun;
  std::vector<Double_t> fRadiusOffset;
  std::vector<Double_t> fRadiusSlope;
  std::vector<Double_t> fNHitsOffset;
  std::vector<Double_t> fNHitsSlope;
  Int_t GetElectronRingRunRange(Int_t);
};

#endif
