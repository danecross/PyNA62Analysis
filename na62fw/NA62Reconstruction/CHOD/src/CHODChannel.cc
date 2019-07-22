#include "TH1D.h"
#include "TH2F.h"
#include "TGraphErrors.h"
#include "TF1.h"
#include "TFile.h"
#include "TProfile.h"
#include <fstream>
#include "Riostream.h"
#include "TMath.h"

#include "TDCEvent.hh"
#include "TCHODHit.hh"
#include "CHODChannel.hh"


CHODChannel::CHODChannel(TVector2 Position,Int_t Plane,Int_t Counter):
  NA62VChannel(Counter,Counter,kFALSE,"CHOD"),
  fPosition(Position),
  fPlane(Plane),
  fCounter(Counter)
{ //WARNING: NA62VChannel is not initialised properly!
}

CHODChannel::~CHODChannel()
{}
