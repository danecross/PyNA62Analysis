#include "Riostream.h"

#include "Intersection.hh"

/// \class Intersection 
/// \Brief
/// Chamber-hit class.
/// \EndBrief
/// 
/// \Detailed
/// Class which returns the variables defining an intersection or a chamber-hit. 
/// \EndDetailed

Intersection::Intersection() :
  fSQ2(TMath::Sqrt2()),
  fX         (.0),
  fY         (.0),
  fU         (.0),
  fV         (.0),
  fXcoor     (.0),
  fYcoor     (.0),
  fSlopeX    (.0),
  fSlopeY    (.0),
  fSlopeU    (.0),
  fSlopeV    (.0),
  fSlopeXcoor(.0),
  fSlopeYcoor(.0),
  fFlag      (0),
  fPhysical  (0),
  fUsed      (0),
  fQuality     (.0),
  fTrailingTime(.0),
  fSubType   (0)
{ 
}

Intersection::~Intersection()
{ 
}

void Intersection::Reset()
{
/// \MemberDescr
/// Clear the data member fIntersection::fClusterId and initialize the other data members.
/// \EndMemberDescr

  fClusterId.clear();
  fX=fY=fU=fV=fXcoor=fYcoor=-9999.;
  fSlopeX=fSlopeY=fSlopeU=fSlopeV=fSlopeXcoor=fSlopeYcoor=0.;
  fFlag=fUsed=0;
  fPhysical=1;
  fQuality=9999.;
  fSubType=-1;
  fViewId.clear();
}

Int_t Intersection::SetTheView(Int_t jView)
{
/// \MemberDescr
///\param jView Id of the view.
///\return Flag defining if jView belongs or not to the intersection.
/// \EndMemberDescr

  Int_t fViewNoHit[4] = {0,0,0,0};
  if (GetX()==-9999) fViewNoHit[0] = 1;
  if (GetY()==-9999) fViewNoHit[1] = 1;
  if (GetU()==-9999) fViewNoHit[2] = 1;
  if (GetV()==-9999) fViewNoHit[3] = 1;
  return fViewNoHit[jView];
}

#include "TMath.h"
void Intersection::SetSlope()
{
  Double_t slopeX = 0.; 
  Double_t slopeY = 0.;
  Double_t sq2 = TMath::Sqrt2();

  switch (GetType())
  {
    case 4:
      slopeX = fSlopeXcoor && fSlopeU && fSlopeV ? 0.5*(fSlopeXcoor+(fSlopeU+fSlopeV)/sq2) : fSlopeXcoor;
      slopeY = fSlopeYcoor && fSlopeU && fSlopeV ? 0.5*(fSlopeYcoor+(fSlopeU-fSlopeV)/sq2) : fSlopeYcoor;
    break;
  
    case 3:
      if (fV==-9999.) 
      {
        slopeX = fSlopeXcoor && fSlopeY && fSlopeU ? 0.5*(fSlopeXcoor+(-fSlopeY+sq2*fSlopeU)) : fSlopeXcoor; 
        slopeY = fSlopeYcoor && fSlopeX && fSlopeU ? 0.5*(fSlopeYcoor+(-fSlopeX+sq2*fSlopeU)) : fSlopeYcoor; 
      }
      if (fU==-9999.) 
      {
        slopeX = fSlopeXcoor && fSlopeY && fSlopeV ? 0.5*(fSlopeXcoor+(fSlopeY+sq2*fSlopeV)) : fSlopeXcoor; 
        slopeY = fSlopeYcoor && fSlopeX && fSlopeV ? 0.5*(fSlopeYcoor+(fSlopeX-sq2*fSlopeV)) : fSlopeYcoor; 
      }
      if (fY==-9999.)
      {
        slopeX = fSlopeXcoor && fSlopeU && fSlopeV ? 0.5*(fSlopeXcoor+(fSlopeU+fSlopeV)/sq2) : fSlopeXcoor;
        slopeY = fSlopeYcoor && fSlopeU && fSlopeV ? 0.5*(fSlopeYcoor+(fSlopeU-fSlopeV)/sq2) : fSlopeYcoor;
      }
      if (fX==-9999.)
      {
        slopeX = fSlopeXcoor && fSlopeU && fSlopeV ? 0.5*(fSlopeXcoor+(fSlopeU+fSlopeV)/sq2) : fSlopeXcoor;
        slopeY = fSlopeYcoor && fSlopeU && fSlopeV ? 0.5*(fSlopeYcoor+(fSlopeU-fSlopeV)/sq2) : fSlopeYcoor;
      }
    break;
  
    case 2:
    break;
  }

  fSlopeXcoor = slopeX;
  fSlopeYcoor = slopeY; 
}

void Intersection::SetCoordinate(Bool_t datatype, Double_t *xycoor)
{
  fXcoor = xycoor[0]; 
  fYcoor = xycoor[1]; 
  fX = xycoor[0]; 
  fY = xycoor[1]; 
  fU = xycoor[2]; 
  fV = xycoor[3]; 

  switch (fSubType)
  {
    case 20:
    fV = -9999;
    fX = -9999;
    fY = -9999;
    fXcoor = -9999;
    fYcoor = -9999;
    break;

    case 21:
    fU = -9999;
    fX = -9999;
    fY = -9999;
    fXcoor = -9999;
    fYcoor = -9999;
    break;

    case 22:
    fV = -9999;
    fU = -9999;
    fY = -9999;
    fYcoor = -9999;
    break;

    case 23:
    fU = -9999;
    fV = -9999;
    fX = -9999;
    fXcoor = -9999;
    break;

    case 14:
    fV = -9999; 
    break;

    case 13:
    fU = -9999; 
    break;

    case 11:
    fY = -9999; 
    fYcoor = datatype ? (-fU+fV)/fSQ2 : (fU-fV)/fSQ2;
    break;
  
    case 7:
    fX = -9999; 
    fXcoor = (fU+fV)/fSQ2;
    break;

    case 5:
    fX = -9999;
    fY = -9999;
    fXcoor = (fU+fV)/fSQ2;
    fYcoor = datatype ? (-fU+fV)/fSQ2 : (fU-fV)/fSQ2;
    break;

    case 4:
    fX = -9999;
    fU = -9999;
    fXcoor = datatype ? -fY+fV*fSQ2 : fY+fV*fSQ2;
    break;

    case 3:
    fX = -9999;
    fV = -9999;
    fXcoor = datatype ? fY+fU*fSQ2 : -fY+fU*fSQ2;
    break;

    case 2:
    fY = -9999;
    fU = -9999;
    fYcoor = datatype ? -fX+fV*fSQ2 : fX-fV*fSQ2;
    break;

    case 1:
    fY = -9999;
    fV = -9999;
    fYcoor = datatype ? fX-fU*fSQ2 : -fX+fU*fSQ2;
    break;

    case 0:
    fU = -9999;
    fV = -9999;
    break;

    default:
    break;
  }
}
