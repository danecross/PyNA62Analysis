#include "TMath.h"
#include "SpectrometerGeometry.hh"
#include "Riostream.h"
#include <iostream>

using namespace std;

/// \class SpectrometerGeometry
/// \Brief
/// Parameters of the spectrometer geometry.
/// \EndBrief
///
/// \Detailed
/// Parameters of the spectrometer geometry.
/// \EndDetailed

SpectrometerGeometry::SpectrometerGeometry() :
  fPar(SpectrometerParameters::GetInstance())
{
/// \MemberDescr
/// Initialization of the variables and creation of the the geometry through the method SpectrometerGeometry::CreateGeometry.
/// \EndMemberDescr


  // DATA
  fViewRotationAngle[0] = 0.25 * TMath::Pi();
  fViewRotationAngle[1] = 1.25 * TMath::Pi();
  fViewRotationAngle[2] = 0.00 * TMath::Pi();
  fViewRotationAngle[3] = 0.50 * TMath::Pi();

  fChamberZPosition[0] = 0.5 * (183311.1 + 183704.9);
  fChamberZPosition[1] = 0.5 * (193864.1 + 194262.9)+2.5; // correction (ok for both data and mc)
  fChamberZPosition[2] = 0.5 * (204262.1 + 204655.9);
  fChamberZPosition[3] = 0.5 * (218688.1 + 219081.9);

  Double_t StrawInnerRadius = 4.875;
  Double_t CopperThickness  = 0.00005;
  Double_t MylarThickness   = 0.036;
  Double_t GoldThickness    = 0.00002;
  Double_t StrawRadius      = StrawInnerRadius+2*CopperThickness+MylarThickness+GoldThickness;

  fStrawDiameter        = 2*StrawRadius;
  fViewSize             = 2100.0;
  fStrawSpacing         = 17.6;
  fLayerSpacing         = 11.0;
  fLayerDisplacement    = fStrawSpacing/2.;
  fHalfViewSpacing      = fLayerSpacing+15.00;
  fHalfViewDisplacement = fStrawSpacing/4.;
  fViewZLength          = fHalfViewSpacing+fLayerSpacing+fStrawDiameter;
  fViewSpacing[0]       = 57.0;
  fViewSpacing[1]       = 233.0;
  fChamberZLength       = 393.8;
  fViewHoleRadius       = 63.8;

  fChambernXRminY        = -63.8;
  fChambernXRmaxY        = +63.8;
  fChamberHoleCenterX[0] = 101.2;
  fChamber1XRmaxU        = 134.2;
  fChamber1XRminU        = 6.6;
  fChamber1XRmaxX        = 165.0;
  fChamber1XRminX        = 37.4;
  fChamberHoleCenterX[1] = 114.4;
  fChamber2XRmaxU        = 143.0;
  fChamber2XRminU        = 15.4;
  fChamber2XRmaxX        = 178.2;
  fChamber2XRminX        = 50.6;
  fChamberHoleCenterX[2] = 92.4;
  fChamber3XRmaxU        = 129.8;
  fChamber3XRminU        = 2.2;
  fChamber3XRmaxX        = 156.2;
  fChamber3XRminX        = 28.6;
  fChamberHoleCenterX[3] = 52.8;
  fChamber4XRmaxU        = 103.4;
  fChamber4XRminU        = -24.2;
  fChamber4XRmaxX        = 116.6;
  fChamber4XRminX        = -11.0;

  fMagnetZLength = 1300;
  fMagnetZPosition = 0.5*(196350+197650)-5.;
  fMagnetFieldStrength = 0.6928;

  fHoleChamberMax[0][0] = 134.2;
  fHoleChamberMax[0][1] = 134.2;
  fHoleChamberMax[0][2] = 165.0;
  fHoleChamberMax[0][3] = 63.8;
  fHoleChamberMin[0][0] = 6.6;
  fHoleChamberMin[0][1] = 6.6;
  fHoleChamberMin[0][2] = 37.4;
  fHoleChamberMin[0][3] = -63.8;
  fHoleChamberMax[1][0] = 143.0;
  fHoleChamberMax[1][1] = 143.0;
  fHoleChamberMax[1][2] = 178.2;
  fHoleChamberMax[1][3] = 63.8;
  fHoleChamberMin[1][0] = 15.4;
  fHoleChamberMin[1][1] = 15.4;
  fHoleChamberMin[1][2] = 50.6;
  fHoleChamberMin[1][3] = -63.8;
  fHoleChamberMax[2][0] = 129.8;
  fHoleChamberMax[2][1] = 129.8;
  fHoleChamberMax[2][2] = 156.2;
  fHoleChamberMax[2][3] = 63.8;
  fHoleChamberMin[2][0] = 2.2;
  fHoleChamberMin[2][1] = 2.2;
  fHoleChamberMin[2][2] = 28.6;
  fHoleChamberMin[2][3] = -63.8;
  fHoleChamberMax[3][0] = 103.4;
  fHoleChamberMax[3][1] = 103.4;
  fHoleChamberMax[3][2] = 116.6;
  fHoleChamberMax[3][3] = 63.8;
  fHoleChamberMin[3][0] = -24.2;
  fHoleChamberMin[3][1] = -24.2;
  fHoleChamberMin[3][2] = -11.0;
  fHoleChamberMin[3][3] = -63.8;

  // X coordinate offset for the planes
  fXPlaneOffset[0][0] = -1058.2;
  fXPlaneOffset[0][1] = -1067.0;
  fXPlaneOffset[0][2] = -1071.4;
  fXPlaneOffset[0][3] = -1062.6;
  fXPlaneOffset[1][0] = -1062.6;
  fXPlaneOffset[1][1] = -1071.4;
  fXPlaneOffset[1][2] = -1067.0;
  fXPlaneOffset[1][3] = -1058.2;
  fXPlaneOffset[2][0] = -1058.2;
  fXPlaneOffset[2][1] = -1067.0;
  fXPlaneOffset[2][2] = -1071.4;
  fXPlaneOffset[2][3] = -1062.6;
  fXPlaneOffset[3][0] = -1080.2;
  fXPlaneOffset[3][1] = -1089.0;
  fXPlaneOffset[3][2] = -1084.6;
  fXPlaneOffset[3][3] = -1075.8;

  // Z coordinate for the views
  fZLocalView[0] = -fViewSpacing[0] - fViewSpacing[1]/2;
  fZLocalView[1] = -fViewSpacing[1]/2.;
  fZLocalView[2] = +fViewSpacing[1]/2.;
  fZLocalView[3] = +fViewSpacing[0] + fViewSpacing[1]/2;

  // Z coordinate for the planes
  fZLocalPlane[0]  = -18.5;
  fZLocalPlane[1]  = -7.5;
  fZLocalPlane[2]  = +7.5;
  fZLocalPlane[3]  = +18.5;

  fViewPlaneTransverseSize = (120-1)*fStrawSpacing+fStrawDiameter;
}

SpectrometerGeometry::~SpectrometerGeometry()
{
  for (Straw* s : fStraws) delete s;
  fStraws.clear();
}

void SpectrometerGeometry::CreateGeometry()
{
/// \MemberDescr
/// Creation of the geometry by defining the numbers and the positions of each straw tube.
/// Method is valid for both Data and MC setup, the only difference is in the x-alignment offsets.
/// \EndMemberDescr


  // protection against multiple data geometry creation calls: clear existing vector of Straw*
  for (Straw* s : fStraws) delete s;
  fStraws.clear();
  Int_t nTotStraws = fPar.GetNChambers()*fPar.GetNViews()*fPar.GetNPlanes()*fPar.GetNStraws();
  fStraws.reserve(nTotStraws);
  for (int is = 0; is < nTotStraws; is++) {
    fStraws.emplace_back(new Straw);
  }

  // Straw Positions
  for (Int_t ich=0; ich < fPar.GetNChambers(); ich++) { // loop over chambers
    for (Int_t iv=0; iv < fPar.GetNViews(); iv++) {     // loop over views
      Double_t xOffChamber = fPar.GetIsRawData() ? fPar.GetXAlignment(ich, iv) : 0;
      Double_t zOffChamber = fPar.GetZChamberCorrection(ich);
      for (Int_t ip=0; ip < fPar.GetNPlanes(); ip++) {  // loop over planes
        Double_t zPos = fChamberZPosition[ich] + zOffChamber + fZLocalView[iv] + fZLocalPlane[ip];
        for (Int_t is=0; is<fPar.GetNStraws(); is++) {  // loop over straws
          Int_t viewid  = ich     * fPar.GetNViews()  + iv;
          Int_t planeid = viewid  * fPar.GetNPlanes() + ip;
          Int_t strawid = planeid * fPar.GetNStraws() + is;
          Double_t xalign = fPar.GetIsRawData() ? fPar.GetXStrawAlignment(planeid, is+1) : 0;
          Double_t localx = fStrawSpacing * is + fXPlaneOffset[iv][ip] + xOffChamber - xalign;
          Double_t localz = fZLocalView[iv] + fZLocalPlane[ip];
          fStraws[strawid]->SetLocalXPosition(localx);
          fStraws[strawid]->SetLocalZPosition(localz);
          fStraws[strawid]->SetGlobalZPosition(zPos);
        }
      }
    }
  }

  // Magnet position correction
  // if (fPar.GetIsRawData()) fMagnetZPosition += fPar.GetZMagnetCorrection();
}

Straw* SpectrometerGeometry::GetStraw(Int_t chID){
  if (chID < 0 || chID > (Int_t)fStraws.size()) return nullptr;
  return fStraws[chID];
}

void SpectrometerGeometry::Printout()
{
/// \MemberDescr
/// Printout used for debugging purposes.
/// \EndMemberDescr

  std::cout << "### Spectrometer geometry for reconstruction ###" << std::endl;
  std::cout <<  "Z chamber position: ";
  for (Int_t j=0;j<4;j++) std::cout << GetChamberZPosition(j) << " ";
  std::cout << " " << std::endl;
  std::cout <<  "X chamber hole shift: ";
  for (Int_t j=0;j<4;j++) std::cout << GetChamberHoleCenterX(j) << " ";
  std::cout << " " << std::endl;
  std::cout <<  "Sequence of the views forming one chamber (XYUV for increasing Z): ";
  for (Int_t j=0;j<4;j++) std::cout << GetViewRotationAngle(j) << " ";
  std::cout << " " << std::endl;
  std::cout <<  "Distance between the uv, xy views: " << GetViewSpacing(0) << std::endl;
  std::cout <<  "Distance between the vx views: " << GetViewSpacing(1) << std::endl;
  std::cout <<  "Distance between two half parts of one view: " << GetHalfViewSpacing() << std::endl;
  std::cout <<  "Shift along the transverse direction between two half parts of one view: " << GetHalfViewDisplacement() << std::endl;
  std::cout <<  "Length of the straws: " << GetViewSize() << std::endl;
  std::cout <<  "Distance between straws of one plane along the transverse direction: " << GetStrawSpacing() << std::endl;
  std::cout <<  "Size of the central aperture: " << GetViewHoleRadius() << std::endl;
  std::cout <<  "Distance between the two straw planes of the same half view along the Z direction: " << GetLayerSpacing() << std::endl;
  std::cout <<  "Shift between the two straw planes of the same half view along transverse direction: " << GetLayerDisplacement() << std::endl;
  std::cout <<  "Magnet position along beam axis: " << GetMagnetZPosition() << std::endl;
  std::cout <<  "Thickness of the magnet: " << GetMagnetZLength() << std::endl;
  std::cout <<  "Field strength: "<< GetMagnetFieldStrength() << std::endl;
}

Int_t SpectrometerGeometry::StrawAcceptance(Int_t iChamber, TVector2 vPos, Int_t Zone)
{
  // View definition
  Double_t sq2 = TMath::Sqrt2();
  Double_t invsq2 = 1./sq2;
  Double_t a[4] = {invsq2,invsq2,1,0};
  Double_t b[4] = {-invsq2,invsq2,0,1};
  Double_t c[4] = {invsq2,-invsq2,0,1};
  Double_t d[4] = {invsq2,invsq2,1,0};
  Int_t viewflag[4] = {0,0,0,0};
  for (Int_t iv=0; iv<4; iv++) {
    Double_t posView = a[iv]*vPos.X() + b[iv]*vPos.Y();
    Double_t posAlongStraw = c[iv]*vPos.X() + d[iv]*vPos.Y();
    if (((posView>fHoleChamberMax[iChamber][iv] && posView<0.5*fViewPlaneTransverseSize)
         || (posView<fHoleChamberMin[iChamber][iv] && posView>-0.5*fViewPlaneTransverseSize)) && fabs(posAlongStraw)<0.5*fViewSize) viewflag[iv] = 1;
  }
  Int_t Vu = viewflag[0];
  Int_t Vv = viewflag[1];
  Int_t Vx = viewflag[2];
  Int_t Vy = viewflag[3];

  // Zones
  switch (Zone)
  {
    case 1:  // At least 1 view
             if (Vx || Vy || Vu || Vv)                      return 1;
             return 0;
    case 2:  // At least 2 views
             if ( (Vx && Vy) || (Vx && Vu) || (Vx && Vv) ||
                  (Vy && Vu) || (Vy && Vv) || (Vu && Vv) )  return 1;
             return 0;
    case 3:  // At least 3 views
             if ( (Vx && Vy && Vu) || (Vx && Vy && Vv) ||
                  (Vx && Vu && Vv) || (Vy && Vu && Vv))     return 1;
             return 0;
    case 4:  // Four views only
             if (Vx && Vy && Vu && Vv)                      return 1;
             return 0;
    case 11: // One view only
             if ( ((Vx && !Vy && !Vu && !Vv) ||
                   (!Vx && Vy && !Vu && !Vv) ||
                   (!Vx && !Vy && Vu && !Vv) ||
                   (!Vx && !Vy && !Vu && Vv)))              return 1;
             return 0;
    case 12: // Two views only
             if ( ((Vx && Vy && !Vu && !Vv) ||
                   (Vx && !Vy && Vu && !Vv) ||
                   (Vx && !Vy && !Vu && Vv) ||
                   (!Vx && Vy && Vu && !Vv) ||
                   (!Vx && Vy && !Vu && Vv) ||
                   (!Vx && !Vy && Vu && Vv)))               return 1;
             return 0;
    case 13: // Three views only
             if ( ((Vx && Vy && Vu && !Vv) ||
                   (Vx && Vy && !Vu && Vv) ||
                   (Vx && !Vy && Vu && Vv) ||
                   (!Vx && Vy && Vu && Vv)))                return 1;
             return 0;
    default:
             return 0;
  }

  return 0;
}
