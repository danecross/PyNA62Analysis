#include "Riostream.h"

#include "TrackCollector.hh"
#include "SpectrometerGeometry.hh"
#include "SpectrometerParameters.hh"
#include "TMath.h"
////#define DEBUG00 
Int_t SelectSubType(Int_t, std::vector<Intersection>::iterator);

TrackCollector::TrackCollector(ChamberCollector *c) :
  fChamber(c),
  fRecoEvent(nullptr),
  fGenEvent(nullptr)
{
/// \MemberDescr
/// \param c Pointer to ChamberCollector class.
///
/// Steps:
/// - Memory allocation to the pointers: TrackCollector::fTrack (pointer to the Track class), matrices used in pattern recognition algorithm,
///   TClonesArray of Tracklets and of Combinations.
/// - Defition of some constants. 
///
/// The dimension of the matrices is fixed assuming 4 chamber-hits with all the xyuv coordinates measured: it correspond to the total number of allowed
/// chamber-hits per track in the spectrometer.
/// \EndMemberDescr

  fTrack = new Track();
  fTrack->InitHistograms();
  fDeg90 = 0.49*TMath::Pi();
  fPar = SpectrometerParameters::GetInstance();

  fThetaMS = 13.6/30000*sqrt(0.0005)*(1+0.0038*log(0.0005)); // multiple scattering
  dMag = SpectrometerGeometry::GetInstance()->GetMagnetZLength();
  fZMag = SpectrometerGeometry::GetInstance()->GetMagnetZPosition();
  fEC = fPar->GetEC();
  fBMag = SpectrometerGeometry::GetInstance()->GetMagnetFieldStrength()*1000.;
  fMomentumMax = fPar->GetMaxMomentum();
  fAngleMax = fPar->GetMaxAngle();
  fCombQualityCut = fPar->GetCombQualityCut();
  fCombHoughDeltaCut = fPar->GetCombHoughDeltaCut();
  fInterTtrailingCut = fPar->GetInterTtrailingCut();
  f3HitsCombRadiusAtMC = fPar->Get3HitsCombRadiusAtMC();
  fSigmaXY = 1.; // mm
  for (Int_t jChamber=0; jChamber<4; jChamber++) fZcoord[jChamber] = SpectrometerGeometry::GetInstance()->GetChamberZPosition(jChamber);    
  fXspread[0] = fPar->GetCombinationXCorridor0();
  fXspread[1] = fPar->GetCombinationXCorridor1();
  fXspread[2] = fPar->GetCombinationXCorridor2();
  fXspread[3] = fPar->GetCombinationXCorridor3();
  fYspread[0] = fPar->GetCombinationYCorridor0();
  fYspread[1] = fPar->GetCombinationYCorridor1();
  fYspread[2] = fPar->GetCombinationYCorridor2();
  fYspread[3] = fPar->GetCombinationYCorridor3();
  fNCouple[0] = 0;
  fNCouple[1] = 1;
  fNCouple[2] = 2;
  fNCouple[3] = 3;
  fIsMuonRun = fPar->GetIsMuonRun();
}

TrackCollector::~TrackCollector()
{
  Reset();
  delete fTrack;
}

void TrackCollector::Reset()
{
/// \MemberDescr
/// Reset of private variables.
/// \EndMemberDescr
  for (Combination* c : fCombination)
    delete c;
  fCombination.clear();
}

void TrackCollector::Reconstruct(TRecoSpectrometerEvent *fSpecEvent)
{
/// \MemberDescr
/// \param fMCEvent Pointer to the Event class.
/// \param fSpecEvent Pointer to the TRecoSpectrometerEvent class. 
///
/// Steering method for the track reconstruction flow. The steps are:
/// - Check the total number of chamber having one chamber-hit: TrackCollector::FewHits.
/// - Pattern recongition: TrackCollector::PatternRecognition.
/// - Track reconstruction: TrackCollector::TrackMeasurement.
/// \EndMemberDescr

  fRecoEvent = fSpecEvent;
  if (FewHits()) return; //ok
  PatternRecognition();
  ComputeTotalHitNumber(); // To be removed in future versions (backward compatibility)
  TrackMeasurement();
}

Int_t TrackCollector::FewHits() //ok
{
/// \MemberDescr
/// \return 1 not enough chambers, 0 enough.
///
/// Check on the number of chambers with at least one hit (>=3). 
/// \EndMemberDescr

  Int_t fNoHit=0;
  for (Int_t jChamber=0; jChamber<4; jChamber++)
  {
    if (!(fNChamberHit[jChamber]=GetChamber(jChamber)->GetNHit())) fNoHit++;
  }
  Int_t nChamberWithHit = 4-fNoHit;
  if (nChamberWithHit<3) return 1; 
  return 0;
}

void TrackCollector::TrackMeasurement()
{
/// \MemberDescr
///
/// Perform the final fit on the combination candidates
/// \EndMemebrDescr
  for (UInt_t j=0; j<fCombination.size(); j++) {
    Combination *comb = fCombination.at(j);
    if (comb->GetNChambers()<3) continue;
    TRecoSpectrometerCandidate *fCandidate = static_cast<TRecoSpectrometerCandidate*>(fRecoEvent->AddCandidate());
    if (!fIsMuonRun) { for (Int_t jiter=0; jiter<3; jiter++) fTrack->Reconstruct(comb,fChamber,fCandidate,jiter); }
    else fTrack->ReconstructMuonRun(comb,fChamber,fCandidate);
  }    
}


void TrackCollector::PatternRecognition()
{
/// \MemberDescr
///
/// Steering method for pattern recognition. 
/// \EndMemberDescr

////  cout << "chamber " << fNChamberHit[0] << " " <<
////          fNChamberHit[1] << " " <<
////          fNChamberHit[2] << " " <<
////          fNChamberHit[3] << endl;
  if (!fIsMuonRun) {
    FindCombinations(-1); // 4-chamber combination
    FindCombinations(0); // 3-chamber combination (no ch 0)
    FindCombinations(3); // 3-chamber combination (no ch 3)
    FindCombinations(1); // 3-chamber combination (no ch 1)
    FindCombinations(2); // 3-chamber combination (no ch 2)
  } else {
    FindCombinations(-1); // muon run
  }

#ifdef DEBUG00
  PrintoutCombination();
#endif
}

void TrackCollector::FindCombinations(Int_t skipChamber)
{
  Double_t xcoord[4] = {-999999., -999999., -999999., -999999.};
  Double_t ycoord[4] = {-999999., -999999., -999999., -999999.};
  std::vector<Intersection>::iterator thisChamberHit[4];

  // Loop on all the possible combinations of chamber-hits
  Combination combination;

  Int_t jHit[4] = {0,0,0,0};
  Int_t exit = 0;
  while(!exit) {
    // Initialization for multiplet selection 
    Double_t timet[4] = {-999999., -999999., -999999., -999999.};
    Bool_t istrailing[4] = {true, true, true, true};
    combination.Reset();
    jHit[1] = skipChamber!=0 ? 0 : jHit[1];
    jHit[2] = 0;
    jHit[3] = 0;
    Double_t yexpmax[4] = {99999., 99999., 99999., 99999.};
    Double_t yexpmin[4] = {-99999., -99999., -99999., -99999.};
    Double_t xexpmax[4] = {99999., 99999., 99999., 99999.};
    Double_t xexpmin[4] = {-99999., -99999., -99999., -99999.};

    // Select a multiplet of chamber hits
    for (Int_t jChamber= (skipChamber!=0?0:1); jChamber<(skipChamber!=3?4:3); jChamber++) {
      if (jChamber==skipChamber) continue;
      if (jHit[jChamber]==fNChamberHit[jChamber]) { exit = 1; break; } 

      // Select a hit in one chamber
      Int_t hitselected(0);

      while(jHit[jChamber]<fNChamberHit[jChamber] && !hitselected) {
        thisChamberHit[jChamber] = GetChamber(jChamber)->GetHit(jHit[jChamber]);
        if (thisChamberHit[jChamber]->GetType()==1) { jHit[jChamber]++; continue; } // Loop over chamber hits at 2, 3 or 4 views only

        //  Timing coincidence between chamber hits if trailing times are present
        timet[jChamber] = thisChamberHit[jChamber]->GetTrailingTime();
        istrailing[jChamber] = timet[jChamber]==-999999. ? 0 : 1;
        if (istrailing[jChamber]) {
          if (jChamber==1) {
            if (skipChamber!=0 && istrailing[0] && fabs(timet[0]-timet[1])>=fInterTtrailingCut) { jHit[jChamber]++; continue; } 
          } 
          if (jChamber==2) {
            if (skipChamber!=0 && istrailing[0] && fabs(timet[0]-timet[2])>=fInterTtrailingCut) { jHit[jChamber]++; continue; } 
            if (skipChamber!=1 && istrailing[1] && fabs(timet[1]-timet[2])>=fInterTtrailingCut) { jHit[jChamber]++; continue; } 
          }
          if (jChamber==3) {
            if (skipChamber!=0 && istrailing[0] && fabs(timet[0]-timet[3])>=fInterTtrailingCut) { jHit[jChamber]++; continue; } 
            if (skipChamber!=1 && istrailing[1] && fabs(timet[1]-timet[3])>=fInterTtrailingCut) { jHit[jChamber]++; continue; } 
            if (skipChamber!=2 && istrailing[2] && fabs(timet[2]-timet[3])>=fInterTtrailingCut) { jHit[jChamber]++; continue; } 
          }
        }

        // Define corridors 
        xcoord[jChamber] = thisChamberHit[jChamber]->GetXcoor(); 
        ycoord[jChamber] = thisChamberHit[jChamber]->GetYcoor(); 
        if (ycoord[jChamber]>=yexpmax[jChamber] || ycoord[jChamber]<=yexpmin[jChamber]) { jHit[jChamber]++; continue; }
        if (((jChamber-1)!=skipChamber) && (jChamber==1||jChamber==3) && (xcoord[jChamber]>=xexpmax[jChamber] || xcoord[jChamber]<=xexpmin[jChamber]) ) { jHit[jChamber]++; continue; }
        if ((jChamber == 0) || (jChamber == 1) || ((jChamber == 2) && (skipChamber != 3))) {
          Int_t next = jChamber==skipChamber-1 ? 2 : 1;
          Double_t yslope = 0;
          Double_t yexp = ycoord[jChamber]+yslope*(fZcoord[jChamber+next]-fZcoord[jChamber]);
          yexpmax[jChamber+next] = yexp+fSigmaXY+3*fThetaMS*fabs(fZcoord[jChamber+next]-fZcoord[jChamber])+fYspread[jChamber+next];
          yexpmin[jChamber+next] = yexp-fSigmaXY-3*fThetaMS*fabs(fZcoord[jChamber+next]-fZcoord[jChamber])-fYspread[jChamber+next];
          Double_t xslope = 0;
          Double_t xexp = xcoord[jChamber]+xslope*(fZcoord[jChamber+next]-fZcoord[jChamber]);
          xexpmax[jChamber+next] = xexp+fSigmaXY+3*fThetaMS*fabs(fZcoord[jChamber+next]-fZcoord[jChamber])+fXspread[jChamber+next];
          xexpmin[jChamber+next] = xexp-fSigmaXY-3*fThetaMS*fabs(fZcoord[jChamber+next]-fZcoord[jChamber])-fXspread[jChamber+next];
        }

        // select hits  
        hitselected = 1;
        jHit[jChamber]++;
      }

      if (!hitselected) jChamber = NextIteration(skipChamber,jChamber,jHit);
      if (jChamber<(skipChamber!=3?3:2) || !hitselected) continue;      

      // Create combinations
      Double_t minsigma = -99999.;
      Double_t deltax = -99999.;
      Double_t combtime = -99999.;
      Double_t ypar[3] = {-99999., -99999., -99999.};
      Double_t xpar[3] = {-99999., -99999., -99999.};

      if (skipChamber==-1) { // 4-hits
        combtime = 0;
        minsigma = HoughTransformation(4,fZcoord,ycoord,-9999.,ypar);
        if (!fIsMuonRun) deltax = XZPlaneRecognition(fZcoord,xcoord,xpar);
        else deltax = HoughTransformation(4,fZcoord,xcoord,-9999.,xpar);
        Int_t ntr = 0;
        for (Int_t jjch=0; jjch<4; jjch++) {
          if (timet[jjch]==-999999.) continue;
          combtime += timet[jjch];
          ntr++;
        } 
        combtime = ntr ? combtime/ntr : -99999.;
      } else { // 3-hits
        Double_t zcoord_sub[3] = {-99999., -99999., -99999.};
        Double_t ycoord_sub[3] = {-99999., -99999., -99999.};;
        Double_t xcoord_sub[3] = {-99999., -99999., -99999.};;
        Int_t ksub = 0;
        combtime = 0;
        Int_t ntr = 0;
        for (Int_t jjch=0; jjch<4; jjch++) {
          if (jjch==skipChamber) continue;
          zcoord_sub[ksub] = fZcoord[jjch];
          ycoord_sub[ksub] = ycoord[jjch];
          xcoord_sub[ksub] = xcoord[jjch];
          if (timet[jjch]>-999999.) { 
            combtime += timet[jjch];
            ntr++;
          }
          ksub++;
        }
        minsigma = HoughTransformation(3,zcoord_sub,ycoord_sub,fZcoord[skipChamber],ypar);
        if (!fIsMuonRun) deltax = XZPlaneRecognition(skipChamber,fZcoord,xcoord,ypar[2],xpar);
        else deltax = HoughTransformation(3,zcoord_sub,xcoord_sub,fZcoord[skipChamber],xpar);
        combtime = ntr ? combtime/ntr : -99999.;
      }

      // Fill combination variables
      Double_t xch1 = xpar[1]+xpar[0]*180000;
      Double_t ych1 = ypar[1]+ypar[0]*180000;
      Double_t distatch1 = xch1*xch1+ych1*ych1;
//      if (fabs(xpar[2])>100000 || xpar[0]>0.5 || ypar[0]>0.5) {
      if (fabs(xpar[2])>(fMomentumMax+10000) || fabs(xpar[2])<3000 || xpar[0]>0.5 || ypar[0]>0.5 || distatch1>=1100*1100) {
        jChamber = NextIteration(skipChamber,jChamber,jHit);
        combination.Reset();
        continue;
      }

      // Fill combination variables
      if (skipChamber==-1) combination.SetType(4);
      else combination.SetType(3);
      combination.SetQuality(minsigma,deltax);
      combination.SetX0(xpar[1]);
      combination.SetY0(ypar[1]);
      combination.SetZ0(0.);
      combination.SetThetaX(xpar[0]);
      combination.SetThetaY(ypar[0]);
      combination.SetP(xpar[2]);
      combination.SetTrailingTime(combtime);
      for (Int_t j=0; j<4; j++) // Loop over all the chambers 
      {
        combination.SetChamberHitId(jHit[j]-1);
        if (combination.GetChamberHitId(j)==-1) {
          combination.SetChamberId(-1); // id of the chamber
          continue; // 3-chamber-hit combinations
        }
        combination.SetChamberId(j);
      }

      // Conditions to store a combination
      // if a RemoveCombination is -1 or 1 store, otherwise not.
      if (!RemoveCombination(combination)) {
        jChamber = NextIteration(skipChamber,jChamber,jHit);
        combination.Reset();
        continue;
      }

      fCombination.emplace_back(new Combination(combination));
      combination.Reset();

      // Next iteration
      jChamber = NextIteration(skipChamber,jChamber,jHit);

      // Exit conditions
      if (jChamber==(skipChamber!=3?4:3)) exit = 1;
    }
    if (exit) continue;

  }  // end loop on combinations 

  RemoveCombination();
}

// Conditions for next iteration
Int_t TrackCollector::NextIteration(Int_t skipChamber, Int_t jChamber, Int_t *jHit)
{
  switch (jChamber)
  {
    case 3:
      switch (skipChamber)
      {
        case -1: case 0: case 3:
          if (jHit[3]<fNChamberHit[3]) return 2;
          if (jHit[2]<fNChamberHit[2]) 
          {
            jHit[3] = 0;
            return 1;
          } 
          if (jHit[1]<fNChamberHit[1]) 
          {
            jHit[2] = 0;
            jHit[3] = 0;
            return 0;
          } 
          return 4;
        break;

        case 1:
          if (jHit[3]<fNChamberHit[3]) return 2;
          if (jHit[2]<fNChamberHit[2]) 
          {
            jHit[3] = 0;
            return 0;
          } 
          return 4;
        break;

        case 2:
          if (jHit[3]<fNChamberHit[3]) return 2;
          if (jHit[1]<fNChamberHit[1]) 
          {
            jHit[3] = 0;
            return 0;
          } 
          return 4;
        break;
      }
    break;

    case 2: 
      switch (skipChamber)
      {
        case -1: case 0: case 2: case 3:
          if (jHit[2]<fNChamberHit[2]) return 1;
          if (jHit[1]<fNChamberHit[1]) 
          {
            jHit[2] = 0;
            return 0;
          } 
          return 3;
        break;

        case 1: 
          if (jHit[2]<fNChamberHit[2]) return 0;
          return 3;
        break;
      }
    break;

    case 1:
      if (jHit[1]<fNChamberHit[1]) return 0;
      return 3;
    break;

    case 0:
      if (jHit[0]==fNChamberHit[0]) return 3;
    break;
  }

  return jChamber;

}

// Hough transformation along y
Double_t TrackCollector::HoughTransformation(Int_t nHits, Double_t *zcoord, Double_t *ycoord, Double_t zskip, Double_t *par)
{
  // Coarse determination of the parameters
  Double_t minsigma(99999.);
  Int_t bestjh(-1);
  Double_t slope(-9999.);
  Double_t rmean(0);
  Double_t rsigma(0);
  Double_t rdist[4];
  for (Int_t jh=0; jh<20; jh++) // slope binning
  {
    slope = 0.001*(-20+2*jh);
    rmean = 0; 
    for (Int_t jhit=0; jhit<nHits; jhit++) 
    {
      rdist[jhit] = (ycoord[jhit]-slope*zcoord[jhit]);
      rmean += rdist[jhit];
    }
    rmean /= nHits;
    rsigma = 0.;
    for (Int_t jhit=0; jhit<nHits; jhit++) rsigma += (rdist[jhit]-rmean)*(rdist[jhit]-rmean)/(1+slope*slope);
    if (rsigma<minsigma)
    {
      minsigma = rsigma;
      bestjh = jh;
    }
  }
  if (minsigma==99999.) return minsigma;

  // Fine determination of the parameters
  minsigma = 99999.;
  slope = -9999.;
  rmean = 0;
  for (Int_t jh=0; jh<40; jh++) // slope binning
  {
    slope = 0.001*(-20+2*bestjh)+0.001*(-2+0.1*jh);
    rmean = 0; 
    for (Int_t jhit=0; jhit<nHits; jhit++) 
    {
      rdist[jhit] = (ycoord[jhit]-slope*zcoord[jhit]);
      rmean += rdist[jhit];
    }
    rmean /= nHits;
    rsigma = 0.;
    for (Int_t jhit=0; jhit<nHits; jhit++) rsigma += (rdist[jhit]-rmean)*(rdist[jhit]-rmean)/(1+slope*slope);
    if (rsigma<minsigma)
    {
      minsigma = rsigma;
      par[0] = slope;
      par[1] = ycoord[0]-par[0]*zcoord[0];
    }
  }

  if (nHits==3) par[2] = par[1]+par[0]*zskip; 
  else par[2] = -9999.;

  return sqrt(minsigma);
}

// Pattern recognition in the XZ plane (4-hits candidate)
Double_t TrackCollector::XZPlaneRecognition(Double_t *zcoord, Double_t *xcoord, Double_t *par)
{
  Double_t slopexbm = (xcoord[1]-xcoord[0])/(zcoord[1]-zcoord[0]);
  Double_t constxbm = xcoord[0]-slopexbm*zcoord[0];
  Double_t x1mag = slopexbm*fZMag+constxbm; 
  Double_t slopexam = (xcoord[3]-xcoord[2])/(zcoord[3]-zcoord[2]);
  Double_t constxam = xcoord[3]-slopexam*zcoord[3];
  Double_t x2mag = slopexam*fZMag+constxam; 
  Double_t pestimation = 1000*fEC*fBMag*dMag/(slopexbm-slopexam); // MeV/c
  Double_t deltax = x1mag-x2mag;
  par[0] = slopexbm;
  par[1] = constxbm;
  par[2] = pestimation;
  return deltax;
}

// Pattern recognition in the XZ plane (3-hits candidate)
Double_t TrackCollector::XZPlaneRecognition(Int_t skip, Double_t *zcoord, Double_t *xcoord, Double_t ycoord, Double_t *par) // Pattern recognition in XZ plane, 1 chamber missing
{
  Double_t slopexbm = 0;
  Double_t constxbm = 0;
  Double_t x1mag; 
  Double_t slopexam = 0;
  Double_t constxam = 0;
  Double_t x2mag; 
  Double_t deltax  = 0;

  if (skip==0 || skip==1) 
  {
    slopexam = (xcoord[3]-xcoord[2])/(zcoord[3]-zcoord[2]);
    constxam = xcoord[3]-slopexam*zcoord[3];
    x2mag = slopexam*fZMag+constxam; 
    Int_t chid = skip==0 ? 1 : 0;
    slopexbm = (x2mag-xcoord[chid])/(fZMag-zcoord[chid]);
    constxbm = xcoord[chid]-slopexbm*zcoord[chid];
    x1mag = slopexbm*zcoord[skip]+constxbm; 
    Double_t xhole = SpectrometerGeometry::GetInstance()->GetChamberHoleCenterX(skip);
    Double_t xeff = x1mag-xhole;
    Double_t reff = sqrt(xeff*xeff+ycoord*ycoord);
    deltax = reff<f3HitsCombRadiusAtMC ? 0. : reff-f3HitsCombRadiusAtMC;
  }

  if (skip==2 || skip==3) 
  {
    slopexbm = (xcoord[1]-xcoord[0])/(zcoord[1]-zcoord[0]);
    constxbm = xcoord[0]-slopexbm*zcoord[0];
    x1mag = slopexbm*fZMag+constxbm; 
    Int_t chid = skip==2 ? 3 : 2;
    slopexam = (xcoord[chid]-x1mag)/(zcoord[chid]-fZMag);
    constxam = xcoord[chid]-slopexam*zcoord[chid];
    x2mag = slopexam*zcoord[skip]+constxam; 
    Double_t xhole = SpectrometerGeometry::GetInstance()->GetChamberHoleCenterX(skip);
    Double_t xeff = x2mag-xhole;
    Double_t reff = sqrt(xeff*xeff+ycoord*ycoord);
    deltax = reff<f3HitsCombRadiusAtMC ? 0. : reff-f3HitsCombRadiusAtMC;
  }

  Double_t pestimation = 1000*fEC*fBMag*dMag/(slopexbm-slopexam); // MeV/c
  par[0] = slopexbm;
  par[1] = constxbm;
  par[2] = pestimation;
  return deltax; // Check the track position at the missing chamber
}

// Remove combinations during creation
Bool_t TrackCollector::RemoveCombination(const Combination &thisComb)
{
  Int_t remove = -1;

  // Reject this combination if unphysics
  if (fabs(thisComb.GetP())>fMomentumMax) remove = 0; //was 90000 hardcoded
  if (fabs(thisComb.GetThetaX())>fAngleMax) remove = 0; //was 0.02 hardcoded
  if (fabs(thisComb.GetThetaY())>fAngleMax) remove = 0; //was 0.02 hardcoded
  if (fabs(thisComb.GetP())<3000) remove = 0;
  if (!remove) return remove;
  // Compare this combination to an existing one
  for (auto combIter = fCombination.begin(); combIter != fCombination.end(); ) { // no increment, do at the end
    Combination *comb = (*combIter);
    // Count chamber hits in common
    Int_t nCommon = 0;   // Total chamber hits in common
    Int_t nCommonbm = 0; // Chamber hits in common before magnet 
    Int_t nCommonam = 0; // Chamber hits in common after magnet
    for (Int_t i=0; i<4; i++) {
      if (comb->GetChamberHitId(i)==-1 || thisComb.GetChamberHitId(i)==-1) continue;
      nCommon = comb->GetChamberHitId(i)==thisComb.GetChamberHitId(i) ? nCommon+1 : nCommon;
      if (i<2) nCommonbm = comb->GetChamberHitId(i)==thisComb.GetChamberHitId(i) ? nCommonbm+1 : nCommonbm;
      else nCommonam = comb->GetChamberHitId(i)==thisComb.GetChamberHitId(i) ? nCommonam+1 : nCommonam;
    }

    // Decide to remove a combination only if there are 2 or more chamber hits in common
    if (nCommon>=2) {
      if (nCommon>=3) {
        if (thisComb.GetType()==4 && comb->GetType()==4) remove = thisComb.GetQuality()<comb->GetQuality() ? 1 : 0;
        else if (thisComb.GetType()==3 && comb->GetType()==3) remove = thisComb.GetHDelta()<comb->GetHDelta() ? 1 : 0;
        else if (thisComb.GetType()==3 && comb->GetType()==4) {
          if (comb->GetQuality()>=fCombQualityCut) remove = thisComb.GetHDelta()<comb->GetHDelta() ? 1 : 0; // or HDelta>5
          else remove = 0;
        } else if (thisComb.GetType()==4 && comb->GetType()==3) {
          if (thisComb.GetQuality()>=fCombQualityCut) remove = thisComb.GetHDelta()<comb->GetHDelta() ? 1 : 0; // or HDelta>5
          else remove = 1;
        }
      }
      if (nCommon==2 && nCommonam==2) {
        if (thisComb.GetType()==4 && comb->GetType()==4) remove = thisComb.GetQuality()<comb->GetQuality() ? 1 : 0;
        else if (thisComb.GetType()==3 && comb->GetType()==3) {
          if (thisComb.GetHDelta()>=fCombHoughDeltaCut && comb->GetHDelta()>=fCombHoughDeltaCut) remove = thisComb.GetHDelta()<comb->GetHDelta() ? 1 : 0;
          if (thisComb.GetHDelta()<fCombHoughDeltaCut && comb->GetHDelta()>=fCombHoughDeltaCut) remove = 1;
          if (thisComb.GetHDelta()>=fCombHoughDeltaCut && comb->GetHDelta()<fCombHoughDeltaCut) remove = 0;
          if (thisComb.GetHDelta()<fCombHoughDeltaCut && comb->GetHDelta()<fCombHoughDeltaCut) remove = thisComb.GetHDelta()<comb->GetHDelta() ? 1 : 0;
        }
        else if (thisComb.GetType()==3 && comb->GetType()==4) {
          if (comb->GetQuality()>=fCombQualityCut) remove = thisComb.GetHDelta()<comb->GetHDelta() ? 1 : 0; // or HDelta>5
          else remove = 0;
        } else if  (thisComb.GetType()==4 && comb->GetType()==3) {
          if (thisComb.GetQuality()>=fCombQualityCut) remove = thisComb.GetHDelta()<comb->GetHDelta() ? 1 : 0; // or HDelta>5
          else remove = 1;
        }
      }
    }

    // Remove the existing combination  if remove is equal 1, continue if remove is -1 or 0
    if (remove == 1) {
      combIter = fCombination.erase(combIter);
      delete comb;
      break;
    } else {
      combIter++;
      continue;
    }
  }
  return remove;
}

// Remove residual fake combinations
void TrackCollector::RemoveCombination()
{
  for (UInt_t j1=0; j1<fCombination.size(); j1++) {
    Combination *comb1 = fCombination.at(j1);
    for (UInt_t j2=0; j2<fCombination.size(); j2++) {
      Int_t nCommon = 0;
      if (j2<=j1) continue;
      Combination *comb2 = fCombination.at(j2);
      for (Int_t i=0; i<4; i++)  {
        if (comb1->GetChamberHitId(i)==-1 || comb2->GetChamberHitId(i)==-1) continue;
        if (comb1->GetChamberHitId(i)==comb2->GetChamberHitId(i)) nCommon++;
        else {
          Int_t chid1 = comb1->GetChamberHitId(i);
          Int_t chid2 = comb2->GetChamberHitId(i);
          std::vector<Intersection>::iterator thisHit1 = GetChamber(i)->GetHit(chid1);
          std::vector<Intersection>::iterator thisHit2 = GetChamber(i)->GetHit(chid2);
          if (thisHit1->GetXcoor()==thisHit2->GetXcoor()) nCommon++;
          if ((comb1->GetType()==3 && comb2->GetType()==4 && comb1->GetChamberHitId(0)==-1) ||
              (comb2->GetType()==3 && comb1->GetType()==4 && comb2->GetChamberHitId(1)==-1) ) {
            if (thisHit1->GetYcoor()==thisHit2->GetYcoor()) nCommon++;
          }
          if ((comb1->GetType()==3 && comb2->GetType()==4 && comb1->GetChamberHitId(1)==-1) ||
              (comb2->GetType()==3 && comb1->GetType()==4 && comb2->GetChamberHitId(1)==-1) ) {
            if (thisHit1->GetYcoor()==thisHit2->GetYcoor()) nCommon++;
          }
        }
      }
      comb1->SetNCommon(nCommon);
      comb2->SetNCommon(nCommon);

      // 3 hits in common (or 3 X coordinates)
      if (nCommon>=3) {
        if (comb1->GetType()==4 && comb2->GetType()==4) {
          if (comb1->GetQuality()>comb2->GetQuality()) comb1->SetHDelta(-9999.);
          else comb2->SetHDelta(-9999.);
        } else if (comb1->GetType()==3 && comb2->GetType()==3) {
          if (comb1->GetHDelta()>comb2->GetHDelta()) comb1->SetHDelta(-9999.);
          else comb2->SetHDelta(-9999.);
        } else if (comb1->GetType()==3 && comb2->GetType()==4) {
//          comb1->SetHDelta(-9999.);
          if (comb2->GetQuality()>=fCombQualityCut && comb2->GetHDelta()>=fCombHoughDeltaCut) {
            if (comb1->GetHDelta()>comb2->GetHDelta()) comb1->SetHDelta(-9999.);
            else comb2->SetHDelta(-9999.);
          } else comb1->SetHDelta(-9999.);
        } else if  (comb1->GetType()==4 && comb2->GetType()==3) {
          if (comb1->GetQuality()>=fCombQualityCut && comb1->GetHDelta()>=fCombHoughDeltaCut) {
            if (comb1->GetHDelta()>comb2->GetHDelta()) comb1->SetHDelta(-9999.);
            else comb2->SetHDelta(-9999.);
          } else comb2->SetHDelta(-9999.);
//          comb2->SetHDelta(-9999.);
        }
      }

      // 2 hits in common (or 2 X coordinates)
      if (nCommon==2)
      {
        // 4 vs 3
        if (comb1->GetType()==4 && comb2->GetType()==3) {
//          comb2->SetHDelta(-9999.);
          if (comb2->GetChamberHitId(0)==-1 || comb2->GetChamberHitId(1)==-1 || comb2->GetChamberHitId(2)==-1 || comb2->GetChamberHitId(3)==-1) { // skip chamber 0 or 3
            if (comb1->GetQuality()>=fCombQualityCut && comb1->GetHDelta()>=fCombHoughDeltaCut) {
              if (comb1->GetHDelta()>comb2->GetHDelta()) comb1->SetHDelta(-9999.);
              else comb2->SetHDelta(-9999.);
            } else comb2->SetHDelta(-9999.);
          }
        }

        // 3 vs 3 
        if (comb1->GetType()==3 && comb2->GetType()==3) {
          if (comb1->GetHDelta()>=fCombQualityCut && comb2->GetHDelta()>=fCombHoughDeltaCut) {
            if (comb1->GetHDelta()>comb2->GetHDelta()) comb1->SetHDelta(-9999.);
            else comb2->SetHDelta(-9999.);
          }
          if (comb1->GetHDelta()<fCombHoughDeltaCut && comb2->GetHDelta()>=fCombHoughDeltaCut) comb2->SetDeltaX(-9999.);
          if (comb1->GetHDelta()>=fCombHoughDeltaCut && comb2->GetHDelta()<fCombHoughDeltaCut) comb1->SetDeltaX(-9999.);
          if (comb1->GetHDelta()<fCombHoughDeltaCut && comb2->GetHDelta()<fCombHoughDeltaCut) {
            if (comb1->GetHDelta()>comb2->GetHDelta()) comb1->SetDeltaX(-9999.);
            else comb2->SetDeltaX(-9999.);
          }
        }

      }

    }
  } 

  for (auto combIter = fCombination.begin(); combIter != fCombination.end(); ) { // no increment, do at the end
    Combination *comb = (*combIter);
    Bool_t removecomb = false;
    if (comb->GetHDelta()==-9999.) removecomb = true;
    if (comb->GetDeltaX()==-9999.) removecomb = true;
    if (removecomb) {
      combIter = fCombination.erase(combIter);
      delete comb;
    } else {
      combIter++;
    }
  }
}

// Printout combination
void TrackCollector::PrintoutCombination()
{
  // Check
  std::cout << "SELECTED COMBINATIONS: " << fCombination.size() << std::endl;
  for (UInt_t j=0; j<fCombination.size(); j++)
  {
    Combination *comb = fCombination.at(j);
    std::cout << " " << std::endl;
    std::cout << ">>> Combination " << j << std::endl;
    std::cout << "Slope X: "  << comb->GetThetaX() << std::endl; 
    std::cout << "Slope Y: "  << comb->GetThetaY() << std::endl; 
    std::cout << "Momentum: " << comb->GetP() << std::endl;
    std::cout << "X0: "       << comb->GetX0() << std::endl;
    std::cout << "Y0: "       << comb->GetY0() << std::endl;
    std::cout << "Time: "     << comb->GetTrailingTime() << std::endl;
    std::cout << "Quality: "  << comb->GetHDelta() << " " << comb->GetDeltaX() << " " << comb->GetQuality() << std::endl;
    std::cout << "" << comb->GetNCommon() << std::endl;
    
    // Extract coordinates of chamber hits
    for (Int_t jj=0; jj<4; jj++)
    {
      Int_t chamberHitId = comb->GetChamberHitId(jj); 
      if (chamberHitId==-1) continue;
      std::vector<Intersection>::iterator thisChamberHit = GetChamber(jj)->GetHit(chamberHitId);
      std::cout << "Chamber " << jj << " " << chamberHitId << " " << thisChamberHit->GetXcoor() << " " << thisChamberHit->GetYcoor() << std::endl;
    } 

  } 
}

Int_t SelectSubType(Int_t type, std::vector<Intersection>::iterator thisHit)
{
  switch (type)
  {
    case 4:
    return -1;
    break;

    case 3:
    if (thisHit->GetV()==-9999.) return 0; // uxy -> 14
    if (thisHit->GetU()==-9999.) return 1; // vxy -> 13
    if (thisHit->GetY()==-9999.) return 2; // xuv -> 11
    if (thisHit->GetX()==-9999.) return 3; // yuv -> 7
    break;
  
    case 2:
    if (thisHit->GetU()==-9999. && thisHit->GetV()==-9999.) return 0; // xy  
    if (thisHit->GetV()==-9999. && thisHit->GetY()==-9999.) return 1; // ux   
    if (thisHit->GetU()==-9999. && thisHit->GetY()==-9999.) return 2; // vx   
    if (thisHit->GetX()==-9999. && thisHit->GetV()==-9999.) return 3; // uy  
    if (thisHit->GetX()==-9999. && thisHit->GetU()==-9999.) return 4; // vy  
    if (thisHit->GetY()==-9999. && thisHit->GetX()==-9999.) return 5; // uv  
  }
  
  return -1; 
}


void TrackCollector::ComputeTotalHitNumber()
{
/// \MemberDescr
/// Compute the total numer of tube hits in this combination. 
/// \EndDescr

  for (UInt_t j=0; j<fCombination.size(); j++) {
    Int_t nTotalHits = 0;

    Combination *comb = fCombination.at(j);
    for (Int_t jj=0; jj<4; jj++) {
      Int_t chamberHitId = comb->GetChamberHitId(jj); // Get the Id of the chamber-hit of chamber jj (1 chamber-hit per chamber in each combination) 
      if (chamberHitId==-1) continue;
       
      std::vector<Intersection>::iterator thisChamberHit = GetChamber(jj)->GetHit(chamberHitId);
      for (UInt_t jclust=0; jclust<thisChamberHit->GetNCluster(); jclust++) {
        Int_t idview = thisChamberHit->GetViewId(jclust); // View ID of a chamber hit cluster (set in ChamberHitCollector)
        Int_t idcluster = thisChamberHit->GetClusterId(jclust); // Cluster ID of a chamber hit cluster (set in ChamberHitCollector) 
        Cluster *thisCluster = GetChamber(jj)->GetView(idview)->Getcluster(idcluster); // Pointer to this cluster
        nTotalHits += thisCluster->GetNHit();
      }
    }  
    comb->SetNTotalHits(nTotalHits);
  }

}

void TrackCollector::SaveHistograms()
{
  fTrack->SaveHistograms();
}
