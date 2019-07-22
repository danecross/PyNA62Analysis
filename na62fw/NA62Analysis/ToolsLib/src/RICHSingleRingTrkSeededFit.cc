// ---------------------------------------------------------------
// Adapted from Giuseppe Ruggiero's private code
// RICHSingleRingFit::Chi2Fit  considers the hits from a PMTimeCandidate and perform a fit
// It has been validated for single track events
//----------------------------------------------------------------

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "RICHSingleRingTrkSeededFit.hh"
#include "TRecoRICHEvent.hh"
#include "TRecoRICHCandidate.hh"
#include "TRecoRICHHit.hh"
#include "BaseAnalysis.hh"
#include "BeamParameters.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "TMath.h"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;
std::vector<TVector2> hitPosFit;
std::vector<int> hitIDFit;
double hitMax;
std::vector<unsigned int> hitMaxBasket;

static RICHSingleRingTrkSeededFit* fInstance = 0;
RICHSingleRingTrkSeededFit* RICHSingleRingTrkSeededFit::GetInstance() {
  if (!fInstance) fInstance = new RICHSingleRingTrkSeededFit();
  return fInstance;
}

RICHSingleRingTrkSeededFit::RICHSingleRingTrkSeededFit() :
  fNPars(3), // Parameters for fit
  fIdBad(-1)
{
  fFitter = new TMinuit(fNPars);
  fFitter->SetFCN(RingChi2FCN);
  fjMax = -1; // new
 
}

/********************************************//**
 * Minuit minimization function
 ************************************************/
void RICHSingleRingTrkSeededFit::RingChi2FCN(Int_t &, Double_t *, Double_t &f, Double_t *par, Int_t iflag)
{
  if(1 || iflag == 4) {
    Int_t nHits = hitPosFit.size();
    f = 0.;
    for (Int_t iHit = 0; iHit < nHits; iHit++) {
      TVector2 PMPosition = hitPosFit[iHit];
      Double_t u = PMPosition.X() - par[0];
      Double_t v = PMPosition.Y() - par[1];
      Double_t d = TMath::Sqrt(u*u+v*v);
      Double_t dr = d - par[2];
      f += dr*dr/(4.7*4.7);
    }
  } // iflag=4 for Minuit
}

/********************************************//**
 * Ring Chi2
 ************************************************/
Double_t RICHSingleRingTrkSeededFit::RingChi2(Double_t *par)
{
  Int_t nHits = hitPosFit.size();
  Double_t f = 0.;
  double maxdr = 0;
  for (Int_t iHit = 0; iHit < nHits; iHit++) {
    TVector2 PMPosition = hitPosFit[iHit];
    Double_t u = PMPosition.X() - par[0];
    Double_t v = PMPosition.Y() - par[1];
    Double_t d = TMath::Sqrt(u*u+v*v);
    Double_t dr = par[2] - d;
    if (fabs(dr)>fabs(maxdr)) {
      maxdr = dr;
      fjMax = hitIDFit[iHit];
    }
    f += dr*dr/(4.7*4.7); 
  }
  hitMax = fjMax;
  hitMaxBasket.push_back(hitMax);
  return f;
}
/********************************************//**
 * Chi2 Fit
 ************************************************/

Bool_t RICHSingleRingTrkSeededFit::Chi2Fit(TRecoRICHEvent* event, TRecoRICHCandidate *thisRing, TVector3 pmtpos, Int_t &SRNHits, Double_t &SRCenterX, Double_t &SRCenterY, Double_t &SRRadius, Double_t &SRChi2, Double_t &SRTime)


//(Int_t /*RunNumber*/, TRecoRICHEvent* event, TRecoRICHCandidate *thisRing, TVector3 pmtpos)
{
  // Run number not used anymore
  Float_t Xcog = 0;
  Float_t Ycog = 0;
  Float_t Rmean = 0;
  TRecoRICHHit *richhit;
  // Extract the hits belonging to the ring candidate and compute the CoG

  unsigned int nHits = thisRing->GetNHits();
  std::vector<int> badHits(nHits, 1); // +1 if good hit, -1 if bad hit ; all initialized as good hit
  TVector2 CurrentRingCenter;
  TVector2 CurrentRingCenterError;
  Double_t CurrentRingRadius, CurrentRingChi2;

  Int_t niter=2; // official pinunu analysis is with niter=2
  fjMax = -1;
  hitMax = -1;
  Double_t TimeAverage = -99;
  Int_t goodNHits = -1;
  Int_t nFinIt = 0;
  

  for (int kiter=0; kiter<niter; kiter++) { 
    nFinIt++;
    goodNHits = 0;
    TimeAverage = 0;
    thisRing->SetEvent(event);
    hitPosFit.clear();
    hitIDFit.clear();   
    for (unsigned int jHit=0; jHit<nHits; jHit++) {
      if(!thisRing->GetHit(jHit)){
	badHits[jHit] = -1 ;
        continue;
      }
      richhit = static_cast<TRecoRICHHit *>(thisRing->GetHit(jHit));
      Double_t xpos = richhit->GetFitPosition().X();
      Double_t ypos = richhit->GetFitPosition().Y();
      Double_t hdist2 = (xpos-pmtpos.X())*(xpos-pmtpos.X())+(ypos-pmtpos.Y())*(ypos-pmtpos.Y());
      if (hdist2>240.*240.) {badHits[jHit] = -1 ;continue;}
      if (hdist2<80.*80.) {badHits[jHit] = -1 ; continue;}
      if (richhit->GetOrSuperCellID()==1) {badHits[jHit] = -1 ;continue;}
      if (fabs(thisRing->GetTime()-richhit->GetTime())>2.) {// ADDED according to the updated pinunu analysis	
	badHits[jHit] = -1 ; 
	continue;
      } 
      unsigned int NHitMax = hitMaxBasket.size();
      for(unsigned int ihitmax=0; ihitmax< NHitMax ; ihitmax++){
	if ( jHit == hitMaxBasket[ihitmax] ){
	  badHits[jHit] = -1 ; 
	  continue ; //
	}
      }
      if(badHits[jHit] == -1) continue; // also the worst hit of the iteration is discarded     
      hitPosFit.push_back(TVector2(xpos,ypos));
      hitIDFit.push_back(jHit);
      Xcog += hitPosFit[goodNHits].X();
      Ycog += hitPosFit[goodNHits].Y();
      TimeAverage  += richhit->GetTime();
      goodNHits++;
    }// loop over hits
    Xcog /= goodNHits;
    Ycog /= goodNHits;
    TimeAverage /= goodNHits;
    for (Int_t iHit=0; iHit<goodNHits; iHit++) {
      Rmean += sqrt(pow(Xcog-hitPosFit[iHit].X(),2)+pow(Ycog-hitPosFit[iHit].Y(),2));
    }
    Rmean /= goodNHits;
    if (goodNHits<4){
      return 0;
    }
    // Perform the fit
    Double_t amin,edm,errdef;
    Int_t nvpar,nparx,icstat,ierflag;
    Double_t pars[10],epars[10];
    Double_t arglist[1];
    arglist[0] = -1;
    fFitter->mnexcm("SET PRI", arglist, 1, ierflag); // Set MINUIT print level
    fFitter->mnexcm("SET NOW", arglist, 0, ierflag); // Set MINUIT warnings
    fFitter->mnparm(0, "x0", Xcog, 0.01, 0., 0., ierflag);
    fFitter->mnparm(1, "y0", Ycog, 0.01, 0., 0., ierflag);
    fFitter->mnparm(2, "R", Rmean, 0.01, 0., 0., ierflag);
    arglist[0] = 0;
    fFitter->mnexcm("MIGRAD", arglist, 1, ierflag); // Calls the minimization
    fFitter->mnstat(amin,edm,errdef,nvpar,nparx,icstat);
    for(Int_t iPar = 0; iPar < fNPars; iPar++) fFitter->GetParameter(iPar,pars[iPar],epars[iPar]);
    CurrentRingCenter.Set(pars[0],pars[1]);
    CurrentRingRadius = pars[2];
    CurrentRingChi2 = RingChi2(pars);
    CurrentRingCenterError.Set(epars[0],epars[1]);
    if (TMath::Prob(CurrentRingChi2,goodNHits-3)>0.005) {
      hitMaxBasket.clear();
      break; // Modified
    }
  }

  SRTime = TimeAverage;
  SRCenterX = CurrentRingCenter.X();
  SRCenterY = CurrentRingCenter.Y();
  SRRadius = CurrentRingRadius;
  SRChi2 =  CurrentRingChi2;
  SRNHits = goodNHits;
  //-----------------------------------------------------------------------
  return 1;
}


RICHSingleRingTrkSeededFit::~RICHSingleRingTrkSeededFit(){
	/// \MemberDescr
	/// Destructor of the Analyzer. If you allocated any memory for class
	/// members, delete them here.
	/// \EndMemberDescr
}
