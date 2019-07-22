// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2015-10-27
//
// ---------------------------------------------------------------  

#ifndef NewCHODDigitizer_H
#define NewCHODDigitizer_H 1

#include "TNewCHODHit.hh"
#include "TNewCHODDigi.hh"
#include "TDCBRawDecoder.hh"
#include "NewCHODGeometry.hh"
#include "NewCHODReconstruction.hh"
#include "TDCEvent.hh"
#include "TSpecialTriggerEvent.hh"
#include "TString.h"
#include "TRegexp.h"
#include "NA62VDigitizer.hh"
#include "NewCHODChannel.hh"

class NewCHODDigitizer : public NA62VDigitizer {

public:

  explicit NewCHODDigitizer(NA62VReconstruction*);
  ~NewCHODDigitizer() {}
  virtual TDetectorVEvent* ProcessEvent(TDetectorVEvent *);
  void ParseConfFile(TString FileName);

private:

  Double_t fEnergyDepositThreshold;
  Double_t fChannelTimeResolution;
  Double_t fChannelMergeThreshold;
  NewCHODGeometry *fGeo;
};

#endif
