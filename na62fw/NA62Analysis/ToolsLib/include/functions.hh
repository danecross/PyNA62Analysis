#ifndef FUNCTIONS_HH
#define FUNCTIONS_HH

#include <signal.h>
#include <iostream>
#include <set>
#include <queue>

#include <TVector3.h>
#include <TChain.h>
#include <TLorentzVector.h>
#include <TH1I.h>
#include <TH2I.h>
#include <TH2F.h>

#include "TGigaTrackerEvent.hh"
#include "TIRCEvent.hh"
#include "TLAVEvent.hh"
#include "TLKrEvent.hh"
#include "TSACEvent.hh"
#include "TSpectrometerEvent.hh"
#include "TGigaTrackerHit.hh"
#include "TIRCHit.hh"
#include "TLAVHit.hh"
#include "TLKrHit.hh"
#include "TSACHit.hh"
#include "TSpectrometerHit.hh"
#include "TRecoSpectrometerCandidate.hh"
#include "TRecoGigaTrackerCandidate.hh"
#include "KinePart.hh"
#include "Event.hh"

#include "NA62Constants.hh"

TVector3 propagate(TVector3 pos, TVector3 p, double z);

TString printVector2(TVector2 v);

TString printVector3(TVector3 v);

TString printVector4(TLorentzVector v);

double distance3D(TVector3 p1, TVector3 p2);

double distance2D(TVector3 p1, TVector3 p2, TString plane);

void ApplyBlueTube(int, TVector3, TVector3, double, TVector3*, TVector3*);

TVector3 GetPositionAtZ(TVector3, TVector3, double);

TVector3 MomAfterKick(TVector3, double);

TVector3 GetVertexCDA(TVector3, TVector3, TVector3, TVector3, double&);

void GetSimpleVertex(int, TVector3, TVector3, TVector3, TVector3, TVector3*, TVector3*, TVector3*, TVector3*, TVector3*, double&);

TVector3 GetIterativeVertex(double, TVector3, TVector3, double, TVector3, TVector3, TVector3*, TVector3*, TVector3*, TVector3*, double&, double dist = 5000.);

TVector3 GetRadoVertex(double, TVector3, TVector3, double, TVector3, TVector3, TVector3*, TVector3*, TVector3*, TVector3*, double&);

TVector3 GetLSFVertex(double, TRecoSpectrometerCandidate*, double, TRecoGigaTrackerCandidate*, TVector3*, TVector3*, TVector3*, TVector3*, double&);

double GetCDA(TRecoSpectrometerCandidate*, TRecoSpectrometerCandidate*);

double GetCDA(TVector3, TVector3, TVector3, TVector3);

int WhichCHODQuadrant(TVector2);
#endif
