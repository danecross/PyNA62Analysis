// -----------------------------------------------------------------------------------------------------------------
// Adapted from the RICHSingleRingTrkSeededFit method
// by Francesco Brizioli (francesco.brizioli@cern.ch)
// RICHSingleRingTrkCentredFit::Chi2Fit  considers the hits from a RICH PMTimeCandidate and perform a ring fit
// with the radius as the only fit parameter, while the centre coordinates are extrapolated from the spectrometer.
// Only hits in 80-240 mm from the extrapolated centre and not coming from supecells are taken into account.
// The worst hit is iteratively discarded if the chi2 probability is not good:
// number of iteration: Niter, minimum chi2 probability value to be good: MinProbability.
// -----------------------------------------------------------------------------------------------------------------

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "RICHSingleRingTrkCentredFit.hh"
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
std::vector<TVector2> hitPosFit_TrkCentred;
std::vector<int> hitIDFit_TrkCentred;
Int_t hitMax_TrkCentred;
Double_t xc , yc ;
static RICHSingleRingTrkCentredFit* fInstance = 0;
RICHSingleRingTrkCentredFit* RICHSingleRingTrkCentredFit::GetInstance() {
  if (!fInstance) fInstance = new RICHSingleRingTrkCentredFit();
  return fInstance;
}

RICHSingleRingTrkCentredFit::RICHSingleRingTrkCentredFit() :
  fNPars(1) // Parameters for fit
{
  fFitter = new TMinuit(fNPars);
  fFitter->SetFCN(RingChi2FCN);
  fjMax = -1; // new
}

// ******************************************
// Minuit minimization function
// ******************************************
void RICHSingleRingTrkCentredFit::RingChi2FCN(Int_t &, Double_t *, Double_t &f, Double_t *par, Int_t iflag)
{
  if(1 || iflag == 4) {
    Int_t nHits = hitPosFit_TrkCentred.size();
    f = 0.;
    for (Int_t iHit = 0; iHit < nHits; iHit++) {
      TVector2 PMPosition = hitPosFit_TrkCentred[iHit];
      Double_t u = PMPosition.X() - xc;
      Double_t v = PMPosition.Y() - yc;
      Double_t d = TMath::Sqrt(u*u+v*v);
      Double_t dr = d - par[0];
      f += dr*dr/(4.7*4.7); // why 4.7 ?
    }
  } // iflag=4 for Minuit
}

// ******************************************
// Ring Chi2
// ******************************************
Double_t RICHSingleRingTrkCentredFit::RingChi2(Double_t *par)
{
  Int_t nHits = hitPosFit_TrkCentred.size();
  Double_t f = 0.;
  Double_t maxdr = 0.;
  hitMax_TrkCentred = -1 ;
  for (Int_t iHit = 0; iHit < nHits; iHit++) {
    TVector2 PMPosition = hitPosFit_TrkCentred[iHit];
    Double_t u = PMPosition.X() - xc;
    Double_t v = PMPosition.Y() - yc;
    Double_t d = TMath::Sqrt(u*u+v*v);
    Double_t dr = par[0] - d;
    if (fabs(dr)>fabs(maxdr)) {
      maxdr = dr;
      fjMax = hitIDFit_TrkCentred[iHit];
    }
    f += dr*dr/(4.7*4.7); // why 4.7 ?
  }
  hitMax_TrkCentred = fjMax;
  return f ;
}

// ********************************************
// Chi2 Fit
// ********************************************
Bool_t RICHSingleRingTrkCentredFit::Chi2Fit(TRecoRICHEvent* event, TRecoRICHCandidate *thisRing, TRecoSpectrometerCandidate *thisTrack, Int_t &NHitsTrackCentred, Double_t &RadiusTrackCentred, Double_t &Chi2TrackCentred, Double_t &TimeTrackCentred )
{
  // Centre coordinates extrapolated from the spectrometer
  Double_t FocalLength = 17020.0 ;
  xc   = FocalLength * thisTrack->GetSlopeXAfterMagnet();
  yc   = FocalLength * thisTrack->GetSlopeYAfterMagnet();

  // Float_t Xcog = 0;
  // Float_t Ycog = 0;
  thisRing->SetEvent(event);
  TRecoRICHHit *richhit;
  Int_t nHits = thisRing->GetNHits();
  std::vector<int> badHits(nHits, 1); // +1 if good hit, -1 if bad hit ; all initialized as good hit

  Bool_t ConvergedFit = false ;
  fjMax = -1;
  hitMax_TrkCentred = -1;
  Int_t Niter = 10 ; // number of iterations for worst hit discarding
  Int_t MinProbability = 0.001 ; // probability threshold for worst hit discarding
  for (int kiter=0; kiter<Niter; kiter++) {
    NHitsTrackCentred = 0;
    hitPosFit_TrkCentred.clear();
    hitIDFit_TrkCentred.clear();
    TimeTrackCentred = 0.0 ;
    // Xcog = 0.0 ;
    // Ycog = 0.0 ;
    Float_t Rmean = 0.0 ;
    for (Int_t jHit=0; jHit<nHits; jHit++) {
      if(badHits[jHit]<0) continue ; // hit already classified as bad
      if(static_cast<TRecoRICHHit*>(thisRing->GetHit(jHit))) {
        richhit = static_cast<TRecoRICHHit*>(thisRing->GetHit(jHit));
      }
      else{
        badHits[jHit] = -1 ;
        continue;
      }
      Double_t xpos = richhit->GetFitPosition().X();
      Double_t ypos = richhit->GetFitPosition().Y();
      Double_t hdist2 = (xpos-xc)*(xpos-xc)+(ypos-yc)*(ypos-yc);
      if (hdist2>240.*240.){ badHits[jHit] = -1 ; continue; }
      if (hdist2<80.*80.){ badHits[jHit] = -1 ; continue; }
      if (richhit->GetOrSuperCellID()==1){badHits[jHit] = -1 ; continue;} // supercells discarded
      if ( jHit == hitMax_TrkCentred ){badHits[jHit] = -1 ; continue ; } // the worst hit is discarded
      hitPosFit_TrkCentred.push_back(TVector2(xpos,ypos));
      hitIDFit_TrkCentred.push_back(jHit);
      // Xcog += hitPosFit_TrkCentred[NHitsTrackCentred].X();
      // Ycog += hitPosFit_TrkCentred[NHitsTrackCentred].Y();
      TimeTrackCentred  += richhit->GetTime();
      Rmean += sqrt(pow(xc-hitPosFit_TrkCentred[NHitsTrackCentred].X(),2)+pow(yc-hitPosFit_TrkCentred[NHitsTrackCentred].Y(),2));
      NHitsTrackCentred++;
    }
    // Xcog /= NHitsTrackCentred;
    // Ycog /= NHitsTrackCentred;
    TimeTrackCentred /= NHitsTrackCentred;
    Rmean /= NHitsTrackCentred;
    if (NHitsTrackCentred<2){
      break ; // break iterations for worst hit discarding
    }
    // Perform the fit
    Double_t amin,edm,errdef;
    Int_t nvpar,nparx,icstat,ierflag;
    Double_t pars[1],epars[1];
    Double_t arglist[1];
    arglist[0] = -1;
    fFitter->mnexcm("SET PRI", arglist, 1, ierflag); // Set MINUIT print level
    fFitter->mnexcm("SET NOW", arglist, 0, ierflag); // Set MINUIT warnings
    // fFitter->mnparm(0, "x0", Xcog, 0.01, 0., 0., ierflag); // xc is not a fit parameter anymore
    // fFitter->mnparm(1, "y0", Ycog, 0.01, 0., 0., ierflag); // yc is not a fit parameter anymore
    fFitter->mnparm(0, "R", Rmean, 0.01, 0., 0., ierflag);
    arglist[0] = 0;
    fFitter->mnexcm("MIGRAD", arglist, 1, ierflag); // Calls the minimization
    fFitter->mnstat(amin,edm,errdef,nvpar,nparx,icstat);
    for(Int_t iPar = 0; iPar < fNPars; iPar++){ fFitter->GetParameter(iPar,pars[iPar],epars[iPar]); }
    RadiusTrackCentred = pars[0];
    Chi2TrackCentred = RingChi2(pars);
    if (TMath::Prob(Chi2TrackCentred,NHitsTrackCentred-1)>MinProbability){
      ConvergedFit = true ;
      break; // break iterations for worst hit discarding
    }
  } // end iterations for worst hit discarding

  Chi2TrackCentred = Chi2TrackCentred/(1.0*NHitsTrackCentred-1.0); // chi2/ndf

  return ConvergedFit;
}


RICHSingleRingTrkCentredFit::~RICHSingleRingTrkCentredFit(){
	/// \MemberDescr
	/// Destructor of the Analyzer. If you allocated any memory for class
	/// members, delete them here.
	/// \EndMemberDescr
}
