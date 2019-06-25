/// \class RICHAcceptance
/// \Brief
/// RICH acceptance computaton for a track
/// \EndBrief
/// \Detailed
/// RICH acceptance is the product of three acceptances, namely the acceptance in the mirrors plane, the acceptance in the
/// PM plane and the beam pipe acceptance (if the cherenkov cone and the beam pipe intersect, beam pipe acceptance
/// returns false). These three acceptances can be evaluated separately by calling GetRICHMirrorPlaneAcceptance(),
/// GetRICHPMPlaneAcceptance() and GetRICHBeamPipeAcceptance().
/// Additional methods are provided in order to evaluate the ring fraction: the ring fraction in mirror plane
/// acceptance is evaluated by calling GetRICHRingFractionMirrorPlane(), GetRICHRingFractionMirrorPlaneJura() and
/// GetRICHRingFractionMirrorPlaneSaleve(); the ring fraction in the PM plane acceptance is evaluated by calling
/// GetRICHRingFractionPMPlane(), GetRICHRingFractionPMPlaneJura() and GetRICHRingFractionPMPlaneSaleve(); the ring fraction
/// in mirror and PM plane acceptance is evaluated by calling GetRICHRingFraction(), GetRICHRingFractionJura() and
/// GetRICHRingFractionSaleve(). Detector IDs are defined in NA62MC/Persistency/include/NA62Global.hh. An example of use is below.
/// \code
/// bool inRICHMirrorPlane = RICHAcceptance::GetInstance()->GetRICHMirrorPlaneAcceptance(Scand, 190., 20.);
/// \endcode
/// \author Riccardo Lollini (riccardo.lollini@pg.infn.it)
/// \EndDetailed

#include "RICHAcceptance.hh"
#include "RICHParameters.hh"

using namespace std;

static RICHAcceptance* fInstance = 0;

RICHAcceptance* RICHAcceptance::GetInstance() {
  if (!fInstance) fInstance = new RICHAcceptance();
  return fInstance;
}

RICHAcceptance::RICHAcceptance() :
  fZRICHFrontPlane        (219445.0),
  fZRICHMirror            (236873.0), // from Mauro Piccini
  fZRICHBackPlane         (237326.0),
  fXRICHFrontPlaneCentre  (    34.0),
  fXRICHBackPlaneCentre   (     2.0),
  fRICHMirrorAcceptance   (true), // acceptance of RICH mirror plane
  fRICHBeamPipeAcceptance (true), // if the BP and the cherenkov cone intersect, BP acceptance is false
  fRICHPMAcceptance       (true), // acceptance of RICH PM plane
  fRICHPMAcceptanceSaleve (true), // cherenkov ring is fully contained in Saleve side (PM plane)
  fRICHPMAcceptanceJura   (true), // cherenkov ring is fully contained in Jura side (PM plane)
  fXRICHBeamPipeHoleCentre((RICHParameters::GetInstance()->GetMirrorCornerX(24,0) + RICHParameters::GetInstance()->GetMirrorCornerX(23,0) + RICHParameters::GetInstance()->GetMirrorCornerX(24,3) + RICHParameters::GetInstance()->GetMirrorCornerX(23,3))/4.),
  fYRICHBeamPipeHoleCentre((RICHParameters::GetInstance()->GetMirrorCornerY(24,0) + RICHParameters::GetInstance()->GetMirrorCornerY(23,0) + RICHParameters::GetInstance()->GetMirrorCornerY(24,3) + RICHParameters::GetInstance()->GetMirrorCornerY(23,3))/4.),
  fRICHBeamPipeHoleRadius(101.),
  fRICHMirrorFocalLength(17000.),
  fInSaleve(1), // track is in Saleve side (mirror)
  fInJura(1), // track is in Jura side (mirror)
  fRICHPMAreaRadius(300.) // radius of the Saleve and Jura surfaces containing the PMTs.
{
  fPMReference[0] = 0.; // RICH mirror alignment global offset
  fPMReference[1] = 0.; // RICH mirror alignment global offset

  // RICH Mirror alignment global offsets for 2016 data
  fSaleveRotation[0] = 196.7;
  fSaleveRotation[1] = 9.5;
  fJuraRotation[0] = 146.8;
  fJuraRotation[1] = 19.8;

  // The following lines define an array fRICHMirrorExternalCorner containing
  // the coordinates for the external corners of mirror mosaic.
  // fRICHMirrorExternalCorner[0][] = (mirror 9, corner 0) and clockwise.
  Int_t fRICHAveragedExtCorner[12] = {1,3,6,8,11,13,16,18,21,23,26,28};
  Int_t fRICHExtMirror[13] = {9,17,4,21,1,16,15,22,8,11,6,10,9};
  Int_t k1 = 0, k2 = 0;
  Int_t imirr1 = 0, imirr2 = 0;
  Int_t oldcorner1 = 1, oldcorner2 = 0;
  Int_t oldcorner1_next = 5;

  for (Int_t corner=0; corner<30; corner++) {
    if (any_of(begin(fRICHAveragedExtCorner), end(fRICHAveragedExtCorner), [&](int i){return i==corner;})){
      fRICHMirrorExternalCorner[corner][0] =
	(RICHParameters::GetInstance()->GetMirrorCornerX(fRICHExtMirror[imirr1],oldcorner1) +
	 RICHParameters::GetInstance()->GetMirrorCornerX(fRICHExtMirror[imirr1+1],oldcorner1_next))/2.;
      fRICHMirrorExternalCorner[corner][1] =
	(RICHParameters::GetInstance()->GetMirrorCornerY(fRICHExtMirror[imirr1],oldcorner1) +
	 RICHParameters::GetInstance()->GetMirrorCornerY(fRICHExtMirror[imirr1+1],oldcorner1_next))/2.;
      if (k1==1) {
	(oldcorner1 != 5) ? (oldcorner1++) : (oldcorner1 = 0);
	(oldcorner1 < 2) ? (oldcorner1_next = oldcorner1 + 4) : (oldcorner1_next = oldcorner1 - 2);
	k1 = 0;
	imirr1++;
      }
      else {
	k1++;
	imirr1++;
      }
    }
    else{
      fRICHMirrorExternalCorner[corner][0] =
	RICHParameters::GetInstance()->GetMirrorCornerX(fRICHExtMirror[imirr2],oldcorner2);
      fRICHMirrorExternalCorner[corner][1] =
	RICHParameters::GetInstance()->GetMirrorCornerY(fRICHExtMirror[imirr2],oldcorner2);
      if (k2==2){
        (oldcorner2 != 5) ? (oldcorner2++) : (oldcorner2 = 0);
        k2 = 0;
      }
      else {
        k2++;
        imirr2++;
      }
    }
  }
  fRICHMirrorExternalCorner[30][0] = fRICHMirrorExternalCorner[0][0];
  fRICHMirrorExternalCorner[30][1] = fRICHMirrorExternalCorner[0][1];

  // Boundary line between Saleve and Jura side of RICH mirror mosaic.
  fRICHBoundaryLine_SaleveJura[0][0] = (RICHParameters::GetInstance()->GetMirrorCornerX(17,1) +
					RICHParameters::GetInstance()->GetMirrorCornerX(4,5))/2.;
  fRICHBoundaryLine_SaleveJura[0][1] = (RICHParameters::GetInstance()->GetMirrorCornerY(17,1) +
					RICHParameters::GetInstance()->GetMirrorCornerY(4,5))/2.;

  fRICHBoundaryLine_SaleveJura[1][0] = (RICHParameters::GetInstance()->GetMirrorCornerX(17,2) +
					RICHParameters::GetInstance()->GetMirrorCornerX(4,4) +
					RICHParameters::GetInstance()->GetMirrorCornerX(12,0))/3.;
  fRICHBoundaryLine_SaleveJura[1][1] = (RICHParameters::GetInstance()->GetMirrorCornerY(17,2) +
					RICHParameters::GetInstance()->GetMirrorCornerY(4,4) +
					RICHParameters::GetInstance()->GetMirrorCornerY(12,0))/3.;

  fRICHBoundaryLine_SaleveJura[2][0] = (RICHParameters::GetInstance()->GetMirrorCornerX(17,3) +
					RICHParameters::GetInstance()->GetMirrorCornerX(20,1) +
					RICHParameters::GetInstance()->GetMirrorCornerX(12,5))/3.;
  fRICHBoundaryLine_SaleveJura[2][1] = (RICHParameters::GetInstance()->GetMirrorCornerY(17,3) +
					RICHParameters::GetInstance()->GetMirrorCornerY(20,1) +
					RICHParameters::GetInstance()->GetMirrorCornerY(12,5))/3.;

  fRICHBoundaryLine_SaleveJura[3][0] = (RICHParameters::GetInstance()->GetMirrorCornerX(14,2) +
					RICHParameters::GetInstance()->GetMirrorCornerX(13,4) +
					RICHParameters::GetInstance()->GetMirrorCornerX(22,0))/3.;
  fRICHBoundaryLine_SaleveJura[3][1] = (RICHParameters::GetInstance()->GetMirrorCornerY(14,2) +
					RICHParameters::GetInstance()->GetMirrorCornerY(13,4) +
					RICHParameters::GetInstance()->GetMirrorCornerY(22,0))/3.;

  fRICHBoundaryLine_SaleveJura[4][0] = (RICHParameters::GetInstance()->GetMirrorCornerX(14,3) +
					RICHParameters::GetInstance()->GetMirrorCornerX(8,1) +
					RICHParameters::GetInstance()->GetMirrorCornerX(22,5))/3.;
  fRICHBoundaryLine_SaleveJura[4][1] = (RICHParameters::GetInstance()->GetMirrorCornerY(14,3) +
					RICHParameters::GetInstance()->GetMirrorCornerY(8,1) +
					RICHParameters::GetInstance()->GetMirrorCornerY(22,5))/3.;

  fRICHBoundaryLine_SaleveJura[5][0] = (RICHParameters::GetInstance()->GetMirrorCornerX(22,4) +
					RICHParameters::GetInstance()->GetMirrorCornerX(8,2))/2.;
  fRICHBoundaryLine_SaleveJura[5][1] = (RICHParameters::GetInstance()->GetMirrorCornerY(22,4) +
					RICHParameters::GetInstance()->GetMirrorCornerY(8,2))/2.;
}

Bool_t RICHAcceptance::GetRICHMirrorPlaneAcceptance
(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut) {
  fRICHMirrorAcceptance = 1;

  TVector3 TrackMomentum;
  TrackMomentum.SetZ(Candidate->GetMomentum()/sqrt(pow(Candidate->GetSlopeXAfterMagnet(),2)+pow(Candidate->GetSlopeYAfterMagnet(),2)+1.));
  TrackMomentum.SetX(TrackMomentum.Z()*Candidate->GetSlopeXAfterMagnet());
  TrackMomentum.SetY(TrackMomentum.Z()*Candidate->GetSlopeYAfterMagnet());

  TVector3 TrackPositionAfterMagnet;
  TrackPositionAfterMagnet.SetX(Candidate->GetPositionAfterMagnet().X());
  TrackPositionAfterMagnet.SetY(Candidate->GetPositionAfterMagnet().Y());
  TrackPositionAfterMagnet.SetZ(Candidate->GetPositionAfterMagnet().Z());

  // Mirror external corners minus (RingRadius+EdgeCut)
  Double_t cint[31][2];
  Double_t ngroup,alphagroup;
  for (Int_t vertex=0; vertex<30; vertex++){
    if (vertex <= 4) ngroup = 1.;
    else if (vertex >= 5 && vertex <= 9) ngroup = 2.;
    else if (vertex >= 10 && vertex <= 14) ngroup = 3.;
    else if (vertex >= 15 && vertex <= 19) ngroup = 4.;
    else if (vertex >= 20 && vertex <= 24) ngroup = 5.;
    else  ngroup = 6.;
    alphagroup = ((ngroup-4.)*TMath::Pi())/3.;
    cint[vertex][0] = fRICHMirrorExternalCorner[vertex][0]+(RingRadius+EdgeCut)*sin(alphagroup)/cos(TMath::Pi()/6.);
    cint[vertex][1] = fRICHMirrorExternalCorner[vertex][1]+(RingRadius+EdgeCut)*cos(alphagroup)/cos(TMath::Pi()/6.);
  }
  cint[30][0] = cint[0][0];
  cint[30][1] = cint[0][1];

  // Track position on mirrors plane
  Double_t thetaXZ = TrackMomentum.X()/TrackMomentum.Z();
  Double_t thetaYZ = TrackMomentum.Y()/TrackMomentum.Z();
  TVector3 posAtRICHMirrors;
  posAtRICHMirrors.SetZ(fZRICHMirror);
  posAtRICHMirrors.SetX(TrackPositionAfterMagnet.X()+thetaXZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));
  posAtRICHMirrors.SetY(TrackPositionAfterMagnet.Y()+thetaYZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));

  Double_t BeamPipeCut = 5.; // Cut on the Ring - beampipe distance (mm)
  // Cut: beam pipe hole on mirror plane
  if (pow(posAtRICHMirrors.X()-fXRICHBeamPipeHoleCentre,2)+pow(posAtRICHMirrors.Y()-fYRICHBeamPipeHoleCentre,2) <
      pow(fRICHBeamPipeHoleRadius+RingRadius+BeamPipeCut,2)) fRICHMirrorAcceptance = fRICHMirrorAcceptance && 0;
  else fRICHMirrorAcceptance = fRICHMirrorAcceptance && 1;

  //Cut: mirrors acceptance
  Int_t NIntersect;
  Double_t x1,y1,x2,y2;
  Double_t m,q;
  NIntersect = 0;
  for (Int_t j=0; j<30; j++){
    if (cint[j][1] <= cint[j+1][1]){
      x1 = cint[j][0];
      y1 = cint[j][1];
      x2 = cint[j+1][0];
      y2 = cint[j+1][1];
    }
    else {
      x1 = cint[j+1][0];
      y1 = cint[j+1][1];
      x2 = cint[j][0];
      y2 = cint[j][1];
    }

    if (posAtRICHMirrors.Y() <= y1 || posAtRICHMirrors.Y() > y2) continue;
    else {
      m = (y1-y2)/(x1-x2);
      q = y1 - m*x1;
      if ((posAtRICHMirrors.Y()-q)/m >= posAtRICHMirrors.X()) NIntersect = NIntersect+1;
    }
  }
  if (NIntersect%2 == 0) fRICHMirrorAcceptance = fRICHMirrorAcceptance && 0;
  else fRICHMirrorAcceptance = fRICHMirrorAcceptance && 1;

  return fRICHMirrorAcceptance;
}

Double_t RICHAcceptance::GetRICHRingFractionMirrorPlaneJura
(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut) {
  TVector3 TrackMomentum;
  TrackMomentum.SetZ(Candidate->GetMomentum()/sqrt(pow(Candidate->GetSlopeXAfterMagnet(),2)+pow(Candidate->GetSlopeYAfterMagnet(),2)+1.));
  TrackMomentum.SetX(TrackMomentum.Z()*Candidate->GetSlopeXAfterMagnet());
  TrackMomentum.SetY(TrackMomentum.Z()*Candidate->GetSlopeYAfterMagnet());

  TVector3 TrackPositionAfterMagnet;
  TrackPositionAfterMagnet.SetX(Candidate->GetPositionAfterMagnet().X());
  TrackPositionAfterMagnet.SetY(Candidate->GetPositionAfterMagnet().Y());
  TrackPositionAfterMagnet.SetZ(Candidate->GetPositionAfterMagnet().Z());

  // Track position on mirrors plane
  Double_t thetaXZ = TrackMomentum.X()/TrackMomentum.Z();
  Double_t thetaYZ = TrackMomentum.Y()/TrackMomentum.Z();
  TVector3 posAtRICHMirrors;
  posAtRICHMirrors.SetZ(fZRICHMirror);
  posAtRICHMirrors.SetX(TrackPositionAfterMagnet.X()+thetaXZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));
  posAtRICHMirrors.SetY(TrackPositionAfterMagnet.Y()+thetaYZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));

  // Mirrors external corners minus (RingRadius+EdgeCut)
  Double_t cint[31][2];
  Double_t ngroup,alphagroup;
  for (Int_t vertex=0; vertex<30; vertex++){
    if (vertex <= 4) ngroup = 1.;
    else if (vertex >= 5 && vertex <= 9) ngroup = 2.;
    else if (vertex >= 10 && vertex <= 14) ngroup = 3.;
    else if (vertex >= 15 && vertex <= 19) ngroup = 4.;
    else if (vertex >= 20 && vertex <= 24) ngroup = 5.;
    else  ngroup = 6.;
    alphagroup = ((ngroup-4.)*TMath::Pi())/3.;
    cint[vertex][0] = fRICHMirrorExternalCorner[vertex][0]+(RingRadius+EdgeCut)*sin(alphagroup)/cos(TMath::Pi()/6.);
    cint[vertex][1] = fRICHMirrorExternalCorner[vertex][1]+(RingRadius+EdgeCut)*cos(alphagroup)/cos(TMath::Pi()/6.);
  }
  cint[30][0] = cint[0][0];
  cint[30][1] = cint[0][1];

  Double_t b0 = (fRICHBoundaryLine_SaleveJura[5][0] + fRICHBoundaryLine_SaleveJura[4][0])/2.;
  Double_t b1 = (fRICHBoundaryLine_SaleveJura[3][0] + fRICHBoundaryLine_SaleveJura[2][0])/2.;
  Double_t b2 = (fRICHBoundaryLine_SaleveJura[1][0] + fRICHBoundaryLine_SaleveJura[0][0])/2.;
  Double_t h1 = fRICHBoundaryLine_SaleveJura[1][1];
  Double_t h2 = fRICHBoundaryLine_SaleveJura[2][1];
  Double_t h3 = fRICHBoundaryLine_SaleveJura[3][1];
  Double_t h4 = fRICHBoundaryLine_SaleveJura[4][1];

  Long64_t N_MirrorPlaneJura = 0;
  const Long64_t N_angles = 100, N_rad = 100;
  Double_t P[2];
  for (Long64_t iAngle = 0; iAngle < N_angles; iAngle++){
    Double_t angle = iAngle*2*TMath::Pi()/N_angles;
    for (Long64_t iRadius = 0; iRadius <= N_rad; iRadius++){
      Double_t rad = iRadius*RingRadius/N_rad;
      P[0] = posAtRICHMirrors.X()+rad*TMath::Cos(angle);
      P[1] = posAtRICHMirrors.Y()+rad*TMath::Sin(angle);

      if (P[0] >= b2)  fInJura = 1;
      else if (P[0] < b2 && P[0] >= b1){
	if ( (P[0]-b1)*(h1-h2) - (P[1]-h2)*(b2-b1) > 0. ) fInJura = 1;
	else fInJura = 0;
      }
      else if (P[0] < b1 && P[0] >= b0){
	if ( (P[0]-b0)*(h3-h4) - (P[1]-h4)*(b1-b0) > 0. ) fInJura = 1;
	else fInJura = 0;
      }
      else fInJura = 0;

      //Cut: mirrors acceptance
      Int_t NIntersect;
      Double_t x1,y1,x2,y2;
      Double_t m,q;
      NIntersect = 0;
      for (Int_t j=0; j<30; j++){
	if (cint[j][1] <= cint[j+1][1]){
	  x1 = cint[j][0];
	  y1 = cint[j][1];
	  x2 = cint[j+1][0];
	  y2 = cint[j+1][1];
	}
	else {
	  x1 = cint[j+1][0];
	  y1 = cint[j+1][1];
	  x2 = cint[j][0];
	  y2 = cint[j][1];
	}

	if (P[1] <= y1 || P[1] > y2) continue;
	else {
	  m = (y1-y2)/(x1-x2);
	  q = y1 - m*x1;
	  if ((P[1]-q)/m >= P[0]) NIntersect = NIntersect+1;
	}
      }

      if (NIntersect%2 != 0 && fInJura == 1) N_MirrorPlaneJura++;
    }//end for iRadius
  }//end for iAngle
  return (double)N_MirrorPlaneJura/(double)(N_angles*N_rad);
}

Double_t RICHAcceptance::GetRICHRingFractionMirrorPlaneSaleve
(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut) {
  TVector3 TrackMomentum;
  TrackMomentum.SetZ(Candidate->GetMomentum()/sqrt(pow(Candidate->GetSlopeXAfterMagnet(),2)+pow(Candidate->GetSlopeYAfterMagnet(),2)+1.));
  TrackMomentum.SetX(TrackMomentum.Z()*Candidate->GetSlopeXAfterMagnet());
  TrackMomentum.SetY(TrackMomentum.Z()*Candidate->GetSlopeYAfterMagnet());

  TVector3 TrackPositionAfterMagnet;
  TrackPositionAfterMagnet.SetX(Candidate->GetPositionAfterMagnet().X());
  TrackPositionAfterMagnet.SetY(Candidate->GetPositionAfterMagnet().Y());
  TrackPositionAfterMagnet.SetZ(Candidate->GetPositionAfterMagnet().Z());

  // Track position on mirrors plane
  Double_t thetaXZ = TrackMomentum.X()/TrackMomentum.Z();
  Double_t thetaYZ = TrackMomentum.Y()/TrackMomentum.Z();
  TVector3 posAtRICHMirrors;
  posAtRICHMirrors.SetZ(fZRICHMirror);
  posAtRICHMirrors.SetX(TrackPositionAfterMagnet.X()+thetaXZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));
  posAtRICHMirrors.SetY(TrackPositionAfterMagnet.Y()+thetaYZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));

  // Mirrors external corners minus (RingRadius+EdgeCut)
  Double_t cint[31][2];
  Double_t ngroup,alphagroup;
  for (Int_t vertex=0; vertex<30; vertex++){
    if (vertex <= 4) ngroup = 1.;
    else if (vertex >= 5 && vertex <= 9) ngroup = 2.;
    else if (vertex >= 10 && vertex <= 14) ngroup = 3.;
    else if (vertex >= 15 && vertex <= 19) ngroup = 4.;
    else if (vertex >= 20 && vertex <= 24) ngroup = 5.;
    else  ngroup = 6.;
    alphagroup = ((ngroup-4.)*TMath::Pi())/3.;
    cint[vertex][0] = fRICHMirrorExternalCorner[vertex][0]+(RingRadius+EdgeCut)*sin(alphagroup)/cos(TMath::Pi()/6.);
    cint[vertex][1] = fRICHMirrorExternalCorner[vertex][1]+(RingRadius+EdgeCut)*cos(alphagroup)/cos(TMath::Pi()/6.);
  }
  cint[30][0] = cint[0][0];
  cint[30][1] = cint[0][1];

  Double_t b0 = (fRICHBoundaryLine_SaleveJura[5][0] + fRICHBoundaryLine_SaleveJura[4][0])/2.;
  Double_t b1 = (fRICHBoundaryLine_SaleveJura[3][0] + fRICHBoundaryLine_SaleveJura[2][0])/2.;
  Double_t b2 = (fRICHBoundaryLine_SaleveJura[1][0] + fRICHBoundaryLine_SaleveJura[0][0])/2.;
  Double_t h1 = fRICHBoundaryLine_SaleveJura[1][1];
  Double_t h2 = fRICHBoundaryLine_SaleveJura[2][1];
  Double_t h3 = fRICHBoundaryLine_SaleveJura[3][1];
  Double_t h4 = fRICHBoundaryLine_SaleveJura[4][1];

  Long64_t N_MirrorPlaneSaleve = 0;
  const Long64_t N_angles = 100, N_rad = 100;
  Double_t P[2];
  for (Long64_t iAngle = 0; iAngle < N_angles; iAngle++){
    Double_t angle = iAngle*2*TMath::Pi()/N_angles;
    for (Long64_t iRadius = 0; iRadius <= N_rad; iRadius++){
      Double_t rad = iRadius*RingRadius/N_rad;
      P[0] = posAtRICHMirrors.X()+rad*TMath::Cos(angle);
      P[1] = posAtRICHMirrors.Y()+rad*TMath::Sin(angle);

      if (P[0] <= b0)  fInSaleve = 1;
      else if (P[0] > b0 && P[0] <= b1){
	if ( (P[0]-b0)*(h3-h4) - (P[1]-h4)*(b1-b0) < 0. ) fInSaleve = 1;
	else fInSaleve = 0;
      }
      else if (P[0] > b1 && P[0] <= b2){
	if ( (P[0]-b1)*(h1-h2) - (P[1]-h2)*(b2-b1) < 0. ) fInSaleve = 1;
	else fInSaleve = 0;
      }
      else fInSaleve = 0;

      //Cut: mirrors acceptance
      Int_t NIntersect;
      Double_t x1,y1,x2,y2;
      Double_t m,q;
      NIntersect = 0;
      for (Int_t j=0; j<30; j++){
	if (cint[j][1] <= cint[j+1][1]){
          x1 = cint[j][0];
          y1 = cint[j][1];
          x2 = cint[j+1][0];
          y2 = cint[j+1][1];
        }
	else {
          x1 = cint[j+1][0];
          y1 = cint[j+1][1];
          x2 = cint[j][0];
          y2 = cint[j][1];
        }

        if (P[1] <= y1 || P[1] > y2) continue;
        else {
          m = (y1-y2)/(x1-x2);
          q = y1 - m*x1;
          if ((P[1]-q)/m >= P[0]) NIntersect = NIntersect+1;
        }
      }

      if (NIntersect%2 != 0 && fInSaleve == 1) N_MirrorPlaneSaleve++;
    }//end for iRadius
  }//end for iAngle
  return (double)N_MirrorPlaneSaleve/(double)(N_angles*N_rad);
}

Double_t RICHAcceptance::GetRICHRingFractionMirrorPlane
(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut) {
  return GetRICHRingFractionMirrorPlaneJura(Candidate, RingRadius, EdgeCut) + GetRICHRingFractionMirrorPlaneSaleve(Candidate, RingRadius, EdgeCut);
}

Bool_t RICHAcceptance::RICHMirrorPlaneIntersectionByTrack
(TVector3 TrackPosition, TVector3 TrackMomentum) {
  fRICHMirrorAcceptance = 1;

  // Mirrors external corners minus (RingRadius+EdgeCut)
  Double_t cint[31][2];
  //Double_t ngroup;
  for (Int_t vertex=0; vertex<30; vertex++){
    //if (vertex <= 4) ngroup = 1.;
    //else if (vertex >= 5 && vertex <= 9) ngroup = 2.;
    //else if (vertex >= 10 && vertex <= 14) ngroup = 3.;
    //else if (vertex >= 15 && vertex <= 19) ngroup = 4.;
    //else if (vertex >= 20 && vertex <= 24) ngroup = 5.;
    //else  ngroup = 6.;
    //alphagroup = ((ngroup-4.)*TMath::Pi())/3.;
    cint[vertex][0] = fRICHMirrorExternalCorner[vertex][0];
    cint[vertex][1] = fRICHMirrorExternalCorner[vertex][1];
  }
  cint[30][0] = cint[0][0];
  cint[30][1] = cint[0][1];

  // Track position on mirrors plane
  Double_t thetaXZ = TrackMomentum.X()/TrackMomentum.Z();
  Double_t thetaYZ = TrackMomentum.Y()/TrackMomentum.Z();
  TVector3 posAtRICHMirrors;
  posAtRICHMirrors.SetZ(fZRICHMirror);
  posAtRICHMirrors.SetX(TrackPosition.X()+thetaXZ*(posAtRICHMirrors.Z()-TrackPosition.Z()));
  posAtRICHMirrors.SetY(TrackPosition.Y()+thetaYZ*(posAtRICHMirrors.Z()-TrackPosition.Z()));

  //Double_t BeamPipeCut = 5.; // Cut on the Ring - beampipe distance (mm)
  // Cut: beam pipe hole on mirror plane
  if (pow(posAtRICHMirrors.X()-fXRICHBeamPipeHoleCentre,2)+pow(posAtRICHMirrors.Y()-fYRICHBeamPipeHoleCentre,2) <
      pow(fRICHBeamPipeHoleRadius,2)) fRICHMirrorAcceptance = fRICHMirrorAcceptance && 0;
  else fRICHMirrorAcceptance = fRICHMirrorAcceptance && 1;

  //Cut: mirrors acceptance
  Int_t NIntersect;
  Double_t x1,y1,x2,y2;
  Double_t m,q;
  NIntersect = 0;
  for (Int_t j=0; j<30; j++){
    if (cint[j][1] <= cint[j+1][1]){
      x1 = cint[j][0];
      y1 = cint[j][1];
      x2 = cint[j+1][0];
      y2 = cint[j+1][1];
    }
    else {
      x1 = cint[j+1][0];
      y1 = cint[j+1][1];
      x2 = cint[j][0];
      y2 = cint[j][1];
    }

    if (posAtRICHMirrors.Y() <= y1 || posAtRICHMirrors.Y() > y2) continue;
    else {
      m = (y1-y2)/(x1-x2);
      q = y1 - m*x1;
      if ((posAtRICHMirrors.Y()-q)/m >= posAtRICHMirrors.X()) NIntersect = NIntersect+1;
    }
  }
  if (NIntersect%2 == 0) fRICHMirrorAcceptance = fRICHMirrorAcceptance && 0;
  else fRICHMirrorAcceptance = fRICHMirrorAcceptance && 1;

  return fRICHMirrorAcceptance;
}

Bool_t RICHAcceptance::GetRICHBeamPipeAcceptance
(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius) {
  fRICHBeamPipeAcceptance = 1;

  TVector3 TrackMomentum;
  TrackMomentum.SetZ(Candidate->GetMomentum()/sqrt(pow(Candidate->GetSlopeXAfterMagnet(),2)+pow(Candidate->GetSlopeYAfterMagnet(),2)+1.));
  TrackMomentum.SetX(TrackMomentum.Z()*Candidate->GetSlopeXAfterMagnet());
  TrackMomentum.SetY(TrackMomentum.Z()*Candidate->GetSlopeYAfterMagnet());

  TVector3 TrackPositionAfterMagnet;
  TrackPositionAfterMagnet.SetX(Candidate->GetPositionAfterMagnet().X());
  TrackPositionAfterMagnet.SetY(Candidate->GetPositionAfterMagnet().Y());
  TrackPositionAfterMagnet.SetZ(Candidate->GetPositionAfterMagnet().Z());

  // Track position on mirrors plane
  Double_t thetaXZ = TrackMomentum.X()/TrackMomentum.Z();
  Double_t thetaYZ = TrackMomentum.Y()/TrackMomentum.Z();
  TVector3 posAtRICHMirrors;
  posAtRICHMirrors.SetZ(fZRICHMirror);
  posAtRICHMirrors.SetX(TrackPositionAfterMagnet.X()+thetaXZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));
  posAtRICHMirrors.SetY(TrackPositionAfterMagnet.Y()+thetaYZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));

  TVector3 posAtRICHEntrance;
  posAtRICHEntrance.SetZ(219875.);
  posAtRICHEntrance.SetX(TrackPositionAfterMagnet.X()+thetaXZ*(posAtRICHEntrance.Z()-TrackPositionAfterMagnet.Z()));
  posAtRICHEntrance.SetY(TrackPositionAfterMagnet.Y()+thetaYZ*(posAtRICHEntrance.Z()-TrackPositionAfterMagnet.Z()));

  Double_t BeamPipeCut = 5.; // Cut on the cone - beampipe distance (mm)

  // CDA (beam pipe axis, track)
  Double_t RCDA;
  TVector3 dist, vert, CDA;
  Double_t Q1, Q2;
  TVector3 point_1, point_2;
  TVector3 beam_Pos;
  beam_Pos.SetXYZ(0.,0.,102400.0);
  point_1 = beam_Pos;
  point_2 = TrackPositionAfterMagnet;
  dist = point_2 - point_1;
  TVector3 vec_1,vec_2; // beam direction and track direction
  vec_1.SetXYZ(0.,0.,1.);
  vec_2.SetXYZ(TrackMomentum.X()/TrackMomentum.Mag(), TrackMomentum.Y()/TrackMomentum.Mag(), TrackMomentum.Z()/TrackMomentum.Mag());

  Double_t ACDA=pow(vec_1[0],2)+pow(vec_1[1],2)+pow(vec_1[2],2);
  Double_t BCDA=pow(vec_2[0],2)+pow(vec_2[1],2)+pow(vec_2[2],2);
  Double_t CCDA=vec_2[0]*vec_1[0]+vec_2[1]*vec_1[1]+vec_2[2]*vec_1[2];
  Double_t DCDA=dist[0]*vec_1[0]+dist[1]*vec_1[1]+dist[2]*vec_1[2];
  Double_t ECDA=dist[0]*vec_2[0]+dist[1]*vec_2[1]+dist[2]*vec_2[2];
  Double_t DET=pow(CCDA,2)-ACDA*BCDA;
  if (DET!=0.){
    Double_t T1=(BCDA*DCDA-CCDA*ECDA)/DET;
    Double_t T2=(CCDA*DCDA-ACDA*ECDA)/DET;
    Q1=point_2[0]+T1*vec_1[0];
    Q2=point_1[0]+T2*vec_2[0];
    vert.SetX((Q1+Q2)/2);
    CDA.SetX(Q1-Q2);
    Q1=point_2[1]+T1*vec_1[1];
    Q2=point_1[1]+T2*vec_2[1];
    vert.SetY((Q1+Q2)/2);
    CDA.SetY(Q1-Q2);
    Q1=point_2[2]+T1*vec_1[2];
    Q2=point_1[2]+T2*vec_2[2];
    vert.SetZ((Q1+Q2)/2);
    CDA.SetZ(Q1-Q2);
    RCDA=sqrt(pow(CDA.X(),2)+pow(CDA.Y(),2)+pow(CDA.Z(),2));

    if (vert.Z() < 219875.) {
      if (fRICHBeamPipeHoleRadius + BeamPipeCut < sqrt(pow(posAtRICHEntrance.X(),2)+pow(posAtRICHEntrance.Y(),2)) ) fRICHBeamPipeAcceptance = 1;
      else fRICHBeamPipeAcceptance = 0;
    }
    else if (vert.Z() > fZRICHMirror) {
      Double_t RingRadius_MAX = RingRadius;
      if (RingRadius_MAX + fRICHBeamPipeHoleRadius + BeamPipeCut < sqrt(pow(posAtRICHMirrors.X(),2)+pow(posAtRICHMirrors.Y(),2)) ) fRICHBeamPipeAcceptance = 1;
      else fRICHBeamPipeAcceptance = 0;
    }
    else {
      Double_t r_cone_at_CDA = (vert.Z()-219875.)*RingRadius/fRICHMirrorFocalLength;
      if (r_cone_at_CDA + fRICHBeamPipeHoleRadius + BeamPipeCut < RCDA) fRICHBeamPipeAcceptance = 1;
      else fRICHBeamPipeAcceptance = 0;
    }
  }

  else {
    Double_t RingRadius_MAX = RingRadius;
    if (RingRadius_MAX + fRICHBeamPipeHoleRadius + BeamPipeCut < sqrt(pow(posAtRICHMirrors.X(),2)+pow(posAtRICHMirrors.Y(),2)) ) fRICHBeamPipeAcceptance = 1;
    else fRICHBeamPipeAcceptance = 0;
  }
  return fRICHBeamPipeAcceptance;
}

Bool_t RICHAcceptance::RICHBeamPipeIntersectionByTrack
(TVector3 TrackPosition, TVector3 TrackMomentum){
  fRICHBeamPipeAcceptance = 1;

  // Track position on mirrors plane
  Double_t thetaXZ = TrackMomentum.X()/TrackMomentum.Z();
  Double_t thetaYZ = TrackMomentum.Y()/TrackMomentum.Z();
  TVector3 posAtRICHMirrors;
  posAtRICHMirrors.SetZ(fZRICHMirror);
  posAtRICHMirrors.SetX(TrackPosition.X()+thetaXZ*(posAtRICHMirrors.Z()-TrackPosition.Z()));
  posAtRICHMirrors.SetY(TrackPosition.Y()+thetaYZ*(posAtRICHMirrors.Z()-TrackPosition.Z()));

  Double_t BeamPipeCut = 5.; // Cut on the cone - beampipe distance (mm)

  // CDA (beam pipe axis, track)
  Double_t RCDA;
  TVector3 dist, vert, CDA;
  Double_t Q1, Q2;
  TVector3 point_1, point_2;
  TVector3 beam_Pos;
  beam_Pos.SetXYZ(0.,0.,102400.0);
  point_1 = beam_Pos;
  point_2 = TrackPosition;
  dist = point_2 - point_1;
  TVector3 vec_1,vec_2; // beam direction and track direction
  vec_1.SetXYZ(0.,0.,1.);
  vec_2.SetXYZ(TrackMomentum.X()/TrackMomentum.Mag(), TrackMomentum.Y()/TrackMomentum.Mag(), TrackMomentum.Z()/TrackMomentum.Mag());

  Double_t ACDA=pow(vec_1[0],2)+pow(vec_1[1],2)+pow(vec_1[2],2);
  Double_t BCDA=pow(vec_2[0],2)+pow(vec_2[1],2)+pow(vec_2[2],2);
  Double_t CCDA=vec_2[0]*vec_1[0]+vec_2[1]*vec_1[1]+vec_2[2]*vec_1[2];
  Double_t DCDA=dist[0]*vec_1[0]+dist[1]*vec_1[1]+dist[2]*vec_1[2];
  Double_t ECDA=dist[0]*vec_2[0]+dist[1]*vec_2[1]+dist[2]*vec_2[2];
  Double_t DET=pow(CCDA,2)-ACDA*BCDA;
  if (DET!=0.){
    Double_t T1=(BCDA*DCDA-CCDA*ECDA)/DET;
    Double_t T2=(CCDA*DCDA-ACDA*ECDA)/DET;
    Q1=point_2[0]+T1*vec_1[0];
    Q2=point_1[0]+T2*vec_2[0];
    vert.SetX((Q1+Q2)/2);
    CDA.SetX(Q1-Q2);
    Q1=point_2[1]+T1*vec_1[1];
    Q2=point_1[1]+T2*vec_2[1];
    vert.SetY((Q1+Q2)/2);
    CDA.SetY(Q1-Q2);
    Q1=point_2[2]+T1*vec_1[2];
    Q2=point_1[2]+T2*vec_2[2];
    vert.SetZ((Q1+Q2)/2);
    CDA.SetZ(Q1-Q2);
    RCDA=sqrt(pow(CDA.X(),2)+pow(CDA.Y(),2)+pow(CDA.Z(),2));

    if (fRICHBeamPipeHoleRadius + BeamPipeCut < RCDA) fRICHBeamPipeAcceptance = 1;
    else fRICHBeamPipeAcceptance = 0;
  }
  else {
    if ( fRICHBeamPipeHoleRadius + BeamPipeCut < sqrt(pow(posAtRICHMirrors.X(),2)+pow(posAtRICHMirrors.Y(),2)) ) fRICHBeamPipeAcceptance = 1;
    else fRICHBeamPipeAcceptance = 0;
  }
  return !fRICHBeamPipeAcceptance;
}

Bool_t RICHAcceptance::GetRICHPMPlaneAcceptance
(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut) {
  fRICHPMAcceptance = 0;
  fRICHPMAcceptanceSaleve = 0;
  fRICHPMAcceptanceJura = 0;
  fInSaleve = 0;
  fInJura = 0;

  TVector3 TrackMomentum;
  TrackMomentum.SetZ(Candidate->GetMomentum()/sqrt(pow(Candidate->GetSlopeXAfterMagnet(),2)+pow(Candidate->GetSlopeYAfterMagnet(),2)+1.));
  TrackMomentum.SetX(TrackMomentum.Z()*Candidate->GetSlopeXAfterMagnet());
  TrackMomentum.SetY(TrackMomentum.Z()*Candidate->GetSlopeYAfterMagnet());

  TVector3 TrackPositionAfterMagnet;
  TrackPositionAfterMagnet.SetX(Candidate->GetPositionAfterMagnet().X());
  TrackPositionAfterMagnet.SetY(Candidate->GetPositionAfterMagnet().Y());
  TrackPositionAfterMagnet.SetZ(Candidate->GetPositionAfterMagnet().Z());

  // Track position on mirrors plane
  Double_t thetaXZ = TrackMomentum.X()/TrackMomentum.Z();
  Double_t thetaYZ = TrackMomentum.Y()/TrackMomentum.Z();
  TVector3 posAtRICHMirrors;
  posAtRICHMirrors.SetZ(fZRICHMirror);
  posAtRICHMirrors.SetX(TrackPositionAfterMagnet.X()+thetaXZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));
  posAtRICHMirrors.SetY(TrackPositionAfterMagnet.Y()+thetaYZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));

  Double_t b0 = (fRICHBoundaryLine_SaleveJura[5][0] + fRICHBoundaryLine_SaleveJura[4][0])/2.;
  Double_t b1 = (fRICHBoundaryLine_SaleveJura[3][0] + fRICHBoundaryLine_SaleveJura[2][0])/2.;
  Double_t b2 = (fRICHBoundaryLine_SaleveJura[1][0] + fRICHBoundaryLine_SaleveJura[0][0])/2.;
  Double_t bs0 = b0 - RingRadius*cos(TMath::Pi()/6.);
  Double_t bs1 = b1 - RingRadius*cos(TMath::Pi()/6.);
  Double_t bs2 = b2 - RingRadius*cos(TMath::Pi()/6.);
  Double_t bj0 = b0 + RingRadius*cos(TMath::Pi()/6.);
  Double_t bj1 = b1 + RingRadius*cos(TMath::Pi()/6.);
  Double_t bj2 = b2 + RingRadius*cos(TMath::Pi()/6.);
  Double_t hs1 = fRICHBoundaryLine_SaleveJura[1][1] + RingRadius*sin(TMath::Pi()/6.);
  Double_t hs2 = fRICHBoundaryLine_SaleveJura[2][1] + RingRadius*sin(TMath::Pi()/6.);
  Double_t hs3 = fRICHBoundaryLine_SaleveJura[3][1] + RingRadius*sin(TMath::Pi()/6.);
  Double_t hs4 = fRICHBoundaryLine_SaleveJura[4][1] + RingRadius*sin(TMath::Pi()/6.);
  Double_t hj1 = fRICHBoundaryLine_SaleveJura[1][1] - RingRadius*sin(TMath::Pi()/6.);
  Double_t hj2 = fRICHBoundaryLine_SaleveJura[2][1] - RingRadius*sin(TMath::Pi()/6.);
  Double_t hj3 = fRICHBoundaryLine_SaleveJura[3][1] - RingRadius*sin(TMath::Pi()/6.);
  Double_t hj4 = fRICHBoundaryLine_SaleveJura[4][1] - RingRadius*sin(TMath::Pi()/6.);

  // fInSaleve = 1, fInJura = 0: ring is fully contained in Saleve side (mirror plane).
  // fInSaleve = 0, fInJura = 1: ring is fully contained in Jura side (mirror plane).
  // fInSaleve = 1 and fInJura = 1: part of the ring is in Saleve side, part of the ring is in Jura side (mirror plane).
  // (The ring doesn't necessarily have to be in mirror acceptance).
  if (posAtRICHMirrors.X() <= bs0)  fInSaleve = 1;
  else if (posAtRICHMirrors.X() > bs0 && posAtRICHMirrors.X() <= bs1){
    if ( (posAtRICHMirrors.X()-bs0)*(hs3-hs4) - (posAtRICHMirrors.Y()-hs4)*(bs1-bs0) < 0. ) fInSaleve = 1;
    else fInSaleve = 0;
  }
  else if (posAtRICHMirrors.X() > bs1 && posAtRICHMirrors.X() <= bs2){
    if ( (posAtRICHMirrors.X()-bs1)*(hs1-hs2) - (posAtRICHMirrors.Y()-hs2)*(bs2-bs1) < 0. ) fInSaleve = 1;
    else fInSaleve = 0;
  }
  else fInSaleve = 0;

  if (posAtRICHMirrors.X() >= bj2)  fInJura = 1;
  else if (posAtRICHMirrors.X() < bj2 && posAtRICHMirrors.X() >= bj1){
    if ( (posAtRICHMirrors.X()-bj1)*(hj1-hj2) - (posAtRICHMirrors.Y()-hj2)*(bj2-bj1) > 0. ) fInJura = 1;
    else fInJura = 0;
  }
  else if (posAtRICHMirrors.X() < bj1 && posAtRICHMirrors.X() >= bj0){
    if ( (posAtRICHMirrors.X()-bj0)*(hj3-hj4) - (posAtRICHMirrors.Y()-hj4)*(bj1-bj0) > 0. ) fInJura = 1;
    else fInJura = 0;
  }
  else fInJura = 0;

  // Track projection on PM plane
  TVector3 NablaF;
  NablaF.SetXYZ(posAtRICHMirrors.X(),posAtRICHMirrors.Y(),fRICHMirrorFocalLength*2.);
  NablaF.Unit();
  TVector3 TrajRefPhot(TMath::Sin(TrackMomentum.Theta())*TMath::Cos(TrackMomentum.Phi()),TMath::Sin(TrackMomentum.Theta())*TMath::Sin(TrackMomentum.Phi()),TMath::Cos(TrackMomentum.Theta()));
  long double projection = (TrajRefPhot(0)*NablaF(0)+TrajRefPhot(1)*NablaF(1)+TrajRefPhot(2)*NablaF(2));
  TVector3 S = projection/(NablaF(0)*NablaF(0)+NablaF(1)*NablaF(1)+NablaF(2)*NablaF(2))*NablaF;
  TVector3 d;
  d.SetXYZ(S(0)-TrajRefPhot(0),S(1)-TrajRefPhot(1),S(2)-TrajRefPhot(2));
  TrajRefPhot.SetXYZ(S(0)+d(0),S(1)+d(1),S(2)+d(2)); // Mirror Vector
  TrajRefPhot = (1/sqrt(TrajRefPhot(0)*TrajRefPhot(0)+TrajRefPhot(1)*TrajRefPhot(1)+TrajRefPhot(2)*TrajRefPhot(2)))*TrajRefPhot; // normalize vector
  long double lambda = -fRICHMirrorFocalLength/TrajRefPhot(2);
  TVector3 PMTPos = posAtRICHMirrors+lambda*TrajRefPhot;

  if (!fInSaleve && !fInJura){
    fPMReference[0] = fSaleveRotation[0];
    fPMReference[1] = fSaleveRotation[1];
    if ( sqrt(pow(PMTPos.X()+fPMReference[0],2)+pow(PMTPos.Y()+fPMReference[1],2)) < fRICHPMAreaRadius-(RingRadius+EdgeCut) ) fRICHPMAcceptanceSaleve = 1;
    else fRICHPMAcceptanceSaleve = 0;
    fPMReference[0] = fJuraRotation[0];
    fPMReference[1] = fJuraRotation[1];
    if ( sqrt(pow(PMTPos.X()+fPMReference[0],2)+pow(PMTPos.Y()+fPMReference[1],2)) < fRICHPMAreaRadius-(RingRadius+EdgeCut) ) fRICHPMAcceptanceJura = 1;
    else fRICHPMAcceptanceJura = 0;
    fRICHPMAcceptance = fRICHPMAcceptanceSaleve && fRICHPMAcceptanceJura;
  }
  else if (fInSaleve && !fInJura){
    fPMReference[0] = fSaleveRotation[0];
    fPMReference[1] = fSaleveRotation[1];
    if ( sqrt(pow(PMTPos.X()+fPMReference[0],2)+pow(PMTPos.Y()+fPMReference[1],2)) < fRICHPMAreaRadius-(RingRadius+EdgeCut) ) fRICHPMAcceptanceSaleve = 1;
    else fRICHPMAcceptanceSaleve = 0;
    fRICHPMAcceptance = fRICHPMAcceptanceSaleve;
  }
  else if (!fInSaleve && fInJura){
    fPMReference[0] = fJuraRotation[0];
    fPMReference[1] = fJuraRotation[1];
    if ( sqrt(pow(PMTPos.X()+fPMReference[0],2)+pow(PMTPos.Y()+fPMReference[1],2)) < fRICHPMAreaRadius-(RingRadius+EdgeCut) ) fRICHPMAcceptanceJura = 1;
    else fRICHPMAcceptanceJura = 0;
    fRICHPMAcceptance = fRICHPMAcceptanceJura;
  }
  return fRICHPMAcceptance;
}

Double_t RICHAcceptance::GetRICHRingFractionPMPlaneJura
(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut) {
  TVector3 TrackMomentum;
  TrackMomentum.SetZ(Candidate->GetMomentum()/sqrt(pow(Candidate->GetSlopeXAfterMagnet(),2)+pow(Candidate->GetSlopeYAfterMagnet(),2)+1.));
  TrackMomentum.SetX(TrackMomentum.Z()*Candidate->GetSlopeXAfterMagnet());
  TrackMomentum.SetY(TrackMomentum.Z()*Candidate->GetSlopeYAfterMagnet());

  TVector3 TrackPositionAfterMagnet;
  TrackPositionAfterMagnet.SetX(Candidate->GetPositionAfterMagnet().X());
  TrackPositionAfterMagnet.SetY(Candidate->GetPositionAfterMagnet().Y());
  TrackPositionAfterMagnet.SetZ(Candidate->GetPositionAfterMagnet().Z());

  // Track position on mirrors plane
  Double_t thetaXZ = TrackMomentum.X()/TrackMomentum.Z();
  Double_t thetaYZ = TrackMomentum.Y()/TrackMomentum.Z();
  TVector3 posAtRICHMirrors;
  posAtRICHMirrors.SetZ(fZRICHMirror);
  posAtRICHMirrors.SetX(TrackPositionAfterMagnet.X()+thetaXZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));
  posAtRICHMirrors.SetY(TrackPositionAfterMagnet.Y()+thetaYZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));

  // Track projection on PM plane
  TVector3 NablaF;
  NablaF.SetXYZ(posAtRICHMirrors.X(),posAtRICHMirrors.Y(),fRICHMirrorFocalLength*2.);
  NablaF.Unit();
  TVector3 TrajRefPhot(TMath::Sin(TrackMomentum.Theta())*TMath::Cos(TrackMomentum.Phi()),TMath::Sin(TrackMomentum.Theta())*TMath::Sin(TrackMomentum.Phi()),TMath::Cos(TrackMomentum.Theta()));
  long double projection = (TrajRefPhot(0)*NablaF(0)+TrajRefPhot(1)*NablaF(1)+TrajRefPhot(2)*NablaF(2));
  TVector3 S = projection/(NablaF(0)*NablaF(0)+NablaF(1)*NablaF(1)+NablaF(2)*NablaF(2))*NablaF;
  TVector3 d;
  d.SetXYZ(S(0)-TrajRefPhot(0),S(1)-TrajRefPhot(1),S(2)-TrajRefPhot(2));
  TrajRefPhot.SetXYZ(S(0)+d(0),S(1)+d(1),S(2)+d(2)); // Mirror Vector
  TrajRefPhot = (1/sqrt(TrajRefPhot(0)*TrajRefPhot(0)+TrajRefPhot(1)*TrajRefPhot(1)+TrajRefPhot(2)*TrajRefPhot(2)))*TrajRefPhot; // normalize vector
  long double lambda = -fRICHMirrorFocalLength/TrajRefPhot(2);
  TVector3 PMTPos = posAtRICHMirrors+lambda*TrajRefPhot;

  const Long64_t N_angles = 10000;
  Double_t P_PM[2];
  Double_t AcceptancePMJura[N_angles];
  Double_t AcceptancePMJura_TOT = 0.;

  for (Long64_t iAngle = 0; iAngle < N_angles; iAngle++){
    Double_t angle = iAngle*2.*TMath::Pi()/N_angles;

    //P_mirr projection on PM plane
    fPMReference[0] = fJuraRotation[0];
    fPMReference[1] = fJuraRotation[1];
    P_PM[0] = PMTPos.X()+RingRadius*TMath::Cos(angle);
    P_PM[1] = PMTPos.Y()+RingRadius*TMath::Sin(angle);
    if (sqrt(pow(fPMReference[0]+P_PM[0],2)+pow(fPMReference[1]+P_PM[1],2)) <= fRICHPMAreaRadius-EdgeCut){
      AcceptancePMJura[iAngle] = 1.;
    }
    else {
      AcceptancePMJura[iAngle] = 0.;
    }

    AcceptancePMJura_TOT = AcceptancePMJura_TOT+AcceptancePMJura[iAngle];
  }// end loop iAngle
  AcceptancePMJura_TOT = AcceptancePMJura_TOT/(double)N_angles;
  return AcceptancePMJura_TOT;
}

Double_t RICHAcceptance::GetRICHRingFractionPMPlaneSaleve
(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut) {
  TVector3 TrackMomentum;
  TrackMomentum.SetZ(Candidate->GetMomentum()/sqrt(pow(Candidate->GetSlopeXAfterMagnet(),2)+pow(Candidate->GetSlopeYAfterMagnet(),2)+1.));
  TrackMomentum.SetX(TrackMomentum.Z()*Candidate->GetSlopeXAfterMagnet());
  TrackMomentum.SetY(TrackMomentum.Z()*Candidate->GetSlopeYAfterMagnet());

  TVector3 TrackPositionAfterMagnet;
  TrackPositionAfterMagnet.SetX(Candidate->GetPositionAfterMagnet().X());
  TrackPositionAfterMagnet.SetY(Candidate->GetPositionAfterMagnet().Y());
  TrackPositionAfterMagnet.SetZ(Candidate->GetPositionAfterMagnet().Z());

  // Track position on mirrors plane
  Double_t thetaXZ = TrackMomentum.X()/TrackMomentum.Z();
  Double_t thetaYZ = TrackMomentum.Y()/TrackMomentum.Z();
  TVector3 posAtRICHMirrors;
  posAtRICHMirrors.SetZ(fZRICHMirror);
  posAtRICHMirrors.SetX(TrackPositionAfterMagnet.X()+thetaXZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));
  posAtRICHMirrors.SetY(TrackPositionAfterMagnet.Y()+thetaYZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));

  // Track projection on PM plane
  TVector3 NablaF;
  NablaF.SetXYZ(posAtRICHMirrors.X(),posAtRICHMirrors.Y(),fRICHMirrorFocalLength*2.);
  NablaF.Unit();
  TVector3 TrajRefPhot(TMath::Sin(TrackMomentum.Theta())*TMath::Cos(TrackMomentum.Phi()),TMath::Sin(TrackMomentum.Theta())*TMath::Sin(TrackMomentum.Phi()),TMath::Cos(TrackMomentum.Theta()));
  long double projection = (TrajRefPhot(0)*NablaF(0)+TrajRefPhot(1)*NablaF(1)+TrajRefPhot(2)*NablaF(2));
  TVector3 S = projection/(NablaF(0)*NablaF(0)+NablaF(1)*NablaF(1)+NablaF(2)*NablaF(2))*NablaF;
  TVector3 d;
  d.SetXYZ(S(0)-TrajRefPhot(0),S(1)-TrajRefPhot(1),S(2)-TrajRefPhot(2));
  TrajRefPhot.SetXYZ(S(0)+d(0),S(1)+d(1),S(2)+d(2)); // Mirror Vector
  TrajRefPhot = (1/sqrt(TrajRefPhot(0)*TrajRefPhot(0)+TrajRefPhot(1)*TrajRefPhot(1)+TrajRefPhot(2)*TrajRefPhot(2)))*TrajRefPhot; // normalize vector
  long double lambda = -fRICHMirrorFocalLength/TrajRefPhot(2);
  TVector3 PMTPos = posAtRICHMirrors+lambda*TrajRefPhot;

  const Long64_t N_angles = 10000;
  Double_t P_PM[2];
  Double_t AcceptancePMSaleve[N_angles];
  Double_t AcceptancePMSaleve_TOT = 0.;

  for (Long64_t iAngle = 0; iAngle < N_angles; iAngle++){
    Double_t angle = iAngle*2.*TMath::Pi()/N_angles;

    //P_mirr projection on PM plane
    fPMReference[0] = fSaleveRotation[0];
    fPMReference[1] = fSaleveRotation[1];
    P_PM[0] = PMTPos.X()+RingRadius*TMath::Cos(angle);
    P_PM[1] = PMTPos.Y()+RingRadius*TMath::Sin(angle);
    if (sqrt(pow(fPMReference[0]+P_PM[0],2)+pow(fPMReference[1]+P_PM[1],2)) <= fRICHPMAreaRadius-EdgeCut){
      AcceptancePMSaleve[iAngle] = 1.;
    }
    else {
      AcceptancePMSaleve[iAngle] = 0.;
    }

    AcceptancePMSaleve_TOT = AcceptancePMSaleve_TOT+AcceptancePMSaleve[iAngle];
  }//end loop iAngle
  AcceptancePMSaleve_TOT = AcceptancePMSaleve_TOT/(double)N_angles;
  return AcceptancePMSaleve_TOT;
}

Double_t RICHAcceptance::GetRICHRingFractionJura
(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut) {
  TVector3 TrackMomentum;
  TrackMomentum.SetZ(Candidate->GetMomentum()/sqrt(pow(Candidate->GetSlopeXAfterMagnet(),2)+pow(Candidate->GetSlopeYAfterMagnet(),2)+1.));
  TrackMomentum.SetX(TrackMomentum.Z()*Candidate->GetSlopeXAfterMagnet());
  TrackMomentum.SetY(TrackMomentum.Z()*Candidate->GetSlopeYAfterMagnet());

  TVector3 TrackPositionAfterMagnet;
  TrackPositionAfterMagnet.SetX(Candidate->GetPositionAfterMagnet().X());
  TrackPositionAfterMagnet.SetY(Candidate->GetPositionAfterMagnet().Y());
  TrackPositionAfterMagnet.SetZ(Candidate->GetPositionAfterMagnet().Z());

  // Track position on mirrors plane
  Double_t thetaXZ = TrackMomentum.X()/TrackMomentum.Z();
  Double_t thetaYZ = TrackMomentum.Y()/TrackMomentum.Z();
  TVector3 posAtRICHMirrors;
  posAtRICHMirrors.SetZ(fZRICHMirror);
  posAtRICHMirrors.SetX(TrackPositionAfterMagnet.X()+thetaXZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));
  posAtRICHMirrors.SetY(TrackPositionAfterMagnet.Y()+thetaYZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));

  // Track projection on PM plane
  TVector3 NablaF;
  NablaF.SetXYZ(posAtRICHMirrors.X(),posAtRICHMirrors.Y(),fRICHMirrorFocalLength*2.);
  NablaF.Unit();
  TVector3 TrajRefPhot(TMath::Sin(TrackMomentum.Theta())*TMath::Cos(TrackMomentum.Phi()),TMath::Sin(TrackMomentum.Theta())*TMath::Sin(TrackMomentum.Phi()),TMath::Cos(TrackMomentum.Theta()));
  long double projection = (TrajRefPhot(0)*NablaF(0)+TrajRefPhot(1)*NablaF(1)+TrajRefPhot(2)*NablaF(2));
  TVector3 S = projection/(NablaF(0)*NablaF(0)+NablaF(1)*NablaF(1)+NablaF(2)*NablaF(2))*NablaF;
  TVector3 d;
  d.SetXYZ(S(0)-TrajRefPhot(0),S(1)-TrajRefPhot(1),S(2)-TrajRefPhot(2));
  TrajRefPhot.SetXYZ(S(0)+d(0),S(1)+d(1),S(2)+d(2)); // Mirror Vector
  TrajRefPhot = (1/sqrt(TrajRefPhot(0)*TrajRefPhot(0)+TrajRefPhot(1)*TrajRefPhot(1)+TrajRefPhot(2)*TrajRefPhot(2)))*TrajRefPhot; // normalize vector
  long double lambda = -fRICHMirrorFocalLength/TrajRefPhot(2);
  TVector3 PMTPos = posAtRICHMirrors+lambda*TrajRefPhot;

  // Mirrors external corners minus (RingRadius+EdgeCut)
  Double_t cint[31][2];
  Double_t ngroup,alphagroup;
  for (Int_t vertex=0; vertex<30; vertex++){
    if (vertex <= 4) ngroup = 1.;
    else if (vertex >= 5 && vertex <= 9) ngroup = 2.;
    else if (vertex >= 10 && vertex <= 14) ngroup = 3.;
    else if (vertex >= 15 && vertex <= 19) ngroup = 4.;
    else if (vertex >= 20 && vertex <= 24) ngroup = 5.;
    else  ngroup = 6.;
    alphagroup = ((ngroup-4.)*TMath::Pi())/3.;
    cint[vertex][0] = fRICHMirrorExternalCorner[vertex][0]+(RingRadius+EdgeCut)*sin(alphagroup)/cos(TMath::Pi()/6.);
    cint[vertex][1] = fRICHMirrorExternalCorner[vertex][1]+(RingRadius+EdgeCut)*cos(alphagroup)/cos(TMath::Pi()/6.);
  }
  cint[30][0] = cint[0][0];
  cint[30][1] = cint[0][1];

  Double_t b0 = (fRICHBoundaryLine_SaleveJura[5][0] + fRICHBoundaryLine_SaleveJura[4][0])/2.;
  Double_t b1 = (fRICHBoundaryLine_SaleveJura[3][0] + fRICHBoundaryLine_SaleveJura[2][0])/2.;
  Double_t b2 = (fRICHBoundaryLine_SaleveJura[1][0] + fRICHBoundaryLine_SaleveJura[0][0])/2.;
  Double_t h1 = fRICHBoundaryLine_SaleveJura[1][1];
  Double_t h2 = fRICHBoundaryLine_SaleveJura[2][1];
  Double_t h3 = fRICHBoundaryLine_SaleveJura[3][1];
  Double_t h4 = fRICHBoundaryLine_SaleveJura[4][1];

  const Long64_t N_angles = 100, N_rad = 100;
  Double_t P_PM[2];
  Double_t AcceptancePMJura[N_angles];
  Double_t AcceptanceMirrorJura[N_angles];
  Double_t AcceptanceJura = 0.;

  for (Long64_t iAngle = 0; iAngle < N_angles; iAngle++){
    Double_t angle = iAngle*2*TMath::Pi()/N_angles;

    //P_mirr projection on PM plane
    fPMReference[0] = fJuraRotation[0];
    fPMReference[1] = fJuraRotation[1];
    P_PM[0] = PMTPos.X()+RingRadius*TMath::Cos(angle);
    P_PM[1] = PMTPos.Y()+RingRadius*TMath::Sin(angle);
    if (sqrt(pow(fPMReference[0]+P_PM[0],2)+pow(fPMReference[1]+P_PM[1],2)) <= fRICHPMAreaRadius-EdgeCut){
      AcceptancePMJura[iAngle] = 1;
    }
    else {
      AcceptancePMJura[iAngle] = 0;
    }

    Long64_t N_MirrorPlaneJura = 0;
    for (Long64_t iRad = 0; iRad < N_rad; iRad++){
      Double_t rad = iRad*RingRadius/N_rad;
      Double_t P_mirr_rad[2];
      P_mirr_rad[0] = posAtRICHMirrors.X()+rad*TMath::Cos(angle);
      P_mirr_rad[1] = posAtRICHMirrors.Y()+rad*TMath::Sin(angle);

      if (P_mirr_rad[0] >= b2)  fInJura = 1;
      else if (P_mirr_rad[0] < b2 && P_mirr_rad[0] >= b1){
        if ( (P_mirr_rad[0]-b1)*(h1-h2) - (P_mirr_rad[1]-h2)*(b2-b1) > 0. ) fInJura = 1;
        else fInJura = 0;
      }
      else if (P_mirr_rad[0] < b1 && P_mirr_rad[0] >= b0){
        if ( (P_mirr_rad[0]-b0)*(h3-h4) - (P_mirr_rad[1]-h4)*(b1-b0) > 0. ) fInJura = 1;
        else fInJura = 0;
      }
      else fInJura = 0;

      //Cut: mirrors acceptance
      Int_t NIntersect;
      Double_t x1,y1,x2,y2;
      Double_t m,q;
      NIntersect = 0;
      for (Int_t j=0; j<30; j++){
	if (cint[j][1] <= cint[j+1][1]){
          x1 = cint[j][0];
          y1 = cint[j][1];
          x2 = cint[j+1][0];
          y2 = cint[j+1][1];
        }
	else {
          x1 = cint[j+1][0];
          y1 = cint[j+1][1];
          x2 = cint[j][0];
          y2 = cint[j][1];
        }

        if (P_mirr_rad[1] <= y1 || P_mirr_rad[1] > y2) continue;
        else {
          m = (y1-y2)/(x1-x2);
          q = y1 - m*x1;
          if ((P_mirr_rad[1]-q)/m >= P_mirr_rad[0]) NIntersect = NIntersect+1;
        }
      }

      if (NIntersect%2 != 0 && fInJura == 1) N_MirrorPlaneJura++;
    }//end loop iRad
    AcceptanceMirrorJura[iAngle] = (double)N_MirrorPlaneJura/(double)N_rad;
    AcceptanceJura = AcceptanceJura+AcceptanceMirrorJura[iAngle]*AcceptancePMJura[iAngle];
  }//end loop iAngle
  AcceptanceJura = AcceptanceJura/N_angles;
  return AcceptanceJura;
}

Double_t RICHAcceptance::GetRICHRingFractionSaleve
(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut) {
  TVector3 TrackMomentum;
  TrackMomentum.SetZ(Candidate->GetMomentum()/sqrt(pow(Candidate->GetSlopeXAfterMagnet(),2)+pow(Candidate->GetSlopeYAfterMagnet(),2)+1.));
  TrackMomentum.SetX(TrackMomentum.Z()*Candidate->GetSlopeXAfterMagnet());
  TrackMomentum.SetY(TrackMomentum.Z()*Candidate->GetSlopeYAfterMagnet());

  TVector3 TrackPositionAfterMagnet;
  TrackPositionAfterMagnet.SetX(Candidate->GetPositionAfterMagnet().X());
  TrackPositionAfterMagnet.SetY(Candidate->GetPositionAfterMagnet().Y());
  TrackPositionAfterMagnet.SetZ(Candidate->GetPositionAfterMagnet().Z());

  // Track position on mirrors plane
  Double_t thetaXZ = TrackMomentum.X()/TrackMomentum.Z();
  Double_t thetaYZ = TrackMomentum.Y()/TrackMomentum.Z();
  TVector3 posAtRICHMirrors;
  posAtRICHMirrors.SetZ(fZRICHMirror);
  posAtRICHMirrors.SetX(TrackPositionAfterMagnet.X()+thetaXZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));
  posAtRICHMirrors.SetY(TrackPositionAfterMagnet.Y()+thetaYZ*(posAtRICHMirrors.Z()-TrackPositionAfterMagnet.Z()));

  // Track projection on PM plane
  TVector3 NablaF;
  NablaF.SetXYZ(posAtRICHMirrors.X(),posAtRICHMirrors.Y(),fRICHMirrorFocalLength*2.);
  NablaF.Unit();
  TVector3 TrajRefPhot(TMath::Sin(TrackMomentum.Theta())*TMath::Cos(TrackMomentum.Phi()),TMath::Sin(TrackMomentum.Theta())*TMath::Sin(TrackMomentum.Phi()),TMath::Cos(TrackMomentum.Theta()));
  long double projection = (TrajRefPhot(0)*NablaF(0)+TrajRefPhot(1)*NablaF(1)+TrajRefPhot(2)*NablaF(2));
  TVector3 S = projection/(NablaF(0)*NablaF(0)+NablaF(1)*NablaF(1)+NablaF(2)*NablaF(2))*NablaF;
  TVector3 d;
  d.SetXYZ(S(0)-TrajRefPhot(0),S(1)-TrajRefPhot(1),S(2)-TrajRefPhot(2));
  TrajRefPhot.SetXYZ(S(0)+d(0),S(1)+d(1),S(2)+d(2)); // Mirror Vector
  TrajRefPhot = (1/sqrt(TrajRefPhot(0)*TrajRefPhot(0)+TrajRefPhot(1)*TrajRefPhot(1)+TrajRefPhot(2)*TrajRefPhot(2)))*TrajRefPhot; // normalize vector
  long double lambda = -fRICHMirrorFocalLength/TrajRefPhot(2);
  TVector3 PMTPos = posAtRICHMirrors+lambda*TrajRefPhot;

  // Mirrors external corners minus (RingRadius+EdgeCut)
  Double_t cint[31][2];
  Double_t ngroup,alphagroup;
  for (Int_t vertex=0; vertex<30; vertex++){
    if (vertex <= 4) ngroup = 1.;
    else if (vertex >= 5 && vertex <= 9) ngroup = 2.;
    else if (vertex >= 10 && vertex <= 14) ngroup = 3.;
    else if (vertex >= 15 && vertex <= 19) ngroup = 4.;
    else if (vertex >= 20 && vertex <= 24) ngroup = 5.;
    else  ngroup = 6.;
    alphagroup = ((ngroup-4.)*TMath::Pi())/3.;
    cint[vertex][0] = fRICHMirrorExternalCorner[vertex][0]+(RingRadius+EdgeCut)*sin(alphagroup)/cos(TMath::Pi()/6.);
    cint[vertex][1] = fRICHMirrorExternalCorner[vertex][1]+(RingRadius+EdgeCut)*cos(alphagroup)/cos(TMath::Pi()/6.);
  }
  cint[30][0] = cint[0][0];
  cint[30][1] = cint[0][1];

  Double_t b0 = (fRICHBoundaryLine_SaleveJura[5][0] + fRICHBoundaryLine_SaleveJura[4][0])/2.;
  Double_t b1 = (fRICHBoundaryLine_SaleveJura[3][0] + fRICHBoundaryLine_SaleveJura[2][0])/2.;
  Double_t b2 = (fRICHBoundaryLine_SaleveJura[1][0] + fRICHBoundaryLine_SaleveJura[0][0])/2.;
  Double_t h1 = fRICHBoundaryLine_SaleveJura[1][1];
  Double_t h2 = fRICHBoundaryLine_SaleveJura[2][1];
  Double_t h3 = fRICHBoundaryLine_SaleveJura[3][1];
  Double_t h4 = fRICHBoundaryLine_SaleveJura[4][1];

  const Long64_t N_angles = 100, N_rad = 100;
  Double_t P_PM[2];
  Double_t AcceptancePMSaleve[N_angles];
  Double_t AcceptanceMirrorSaleve[N_angles];
  Double_t AcceptanceSaleve = 0.;

  for (Long64_t iAngle = 0; iAngle < N_angles; iAngle++){
    Double_t angle = iAngle*2*TMath::Pi()/N_angles;

    //P_mirr projection on PM plane
    fPMReference[0] = fSaleveRotation[0];
    fPMReference[1] = fSaleveRotation[1];
    P_PM[0] = PMTPos.X()+RingRadius*TMath::Cos(angle);
    P_PM[1] = PMTPos.Y()+RingRadius*TMath::Sin(angle);
    if (sqrt(pow(fPMReference[0]+P_PM[0],2)+pow(fPMReference[1]+P_PM[1],2)) <= fRICHPMAreaRadius-EdgeCut){
      AcceptancePMSaleve[iAngle] = 1;
    }
    else {
      AcceptancePMSaleve[iAngle] = 0;
    }

    Long64_t N_MirrorPlaneSaleve = 0;
    for (Long64_t iRad = 0; iRad < N_rad; iRad++){
      Double_t rad = iRad*RingRadius/N_rad;
      Double_t P_mirr_rad[2];
      P_mirr_rad[0] = posAtRICHMirrors.X()+rad*TMath::Cos(angle);
      P_mirr_rad[1] = posAtRICHMirrors.Y()+rad*TMath::Sin(angle);

      if (P_mirr_rad[0] <= b0)  fInSaleve = 1;
      else if (P_mirr_rad[0] > b0 && P_mirr_rad[0] <= b1){
        if ( (P_mirr_rad[0]-b0)*(h3-h4) - (P_mirr_rad[1]-h4)*(b1-b0) < 0. ) fInSaleve = 1;
        else fInSaleve = 0;
      }
      else if (P_mirr_rad[0] > b1 && P_mirr_rad[0] <= b2){
        if ( (P_mirr_rad[0]-b1)*(h1-h2) - (P_mirr_rad[1]-h2)*(b2-b1) < 0. ) fInSaleve = 1;
        else fInSaleve = 0;
      }
      else fInSaleve = 0;

      //Cut: mirrors acceptance
      Int_t NIntersect;
      Double_t x1,y1,x2,y2;
      Double_t m,q;
      NIntersect = 0;
      for (Int_t j=0; j<30; j++){
        if (cint[j][1] <= cint[j+1][1]){
          x1 = cint[j][0];
          y1 = cint[j][1];
          x2 = cint[j+1][0];
          y2 = cint[j+1][1];
        }
        else {
          x1 = cint[j+1][0];
          y1 = cint[j+1][1];
          x2 = cint[j][0];
          y2 = cint[j][1];
        }

        if (P_mirr_rad[1] <= y1 || P_mirr_rad[1] > y2) continue;
        else {
          m = (y1-y2)/(x1-x2);
          q = y1 - m*x1;
          if ((P_mirr_rad[1]-q)/m >= P_mirr_rad[0]) NIntersect = NIntersect+1;
        }
      }

      if (NIntersect%2 != 0 && fInSaleve == 1) N_MirrorPlaneSaleve++;
    }//end loop iRad
    AcceptanceMirrorSaleve[iAngle] = (double)N_MirrorPlaneSaleve/(double)N_rad;
    AcceptanceSaleve = AcceptanceSaleve+AcceptanceMirrorSaleve[iAngle]*AcceptancePMSaleve[iAngle];
  }//end loop iAngle
  AcceptanceSaleve = AcceptanceSaleve/N_angles;
  return AcceptanceSaleve;
}

Double_t RICHAcceptance::GetRICHRingFraction
(TRecoSpectrometerCandidate *Candidate, Double_t RingRadius, Double_t EdgeCut) {
  return GetRICHRingFractionJura(Candidate, RingRadius, EdgeCut) + GetRICHRingFractionSaleve(Candidate, RingRadius, EdgeCut);
}
