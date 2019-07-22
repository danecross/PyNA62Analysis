#ifndef RICHMirrorWindow_H
#define RICHMirrorWindow_H 1

#include "NA62VComponent.hh"
#include "RICHGeometryParameters.hh"
#include "globals.hh"
#include "G4OpticalSurface.hh"


class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class RICHMirrorWindow : public NA62VComponent
{

public:
  
  RICHMirrorWindow(G4Material*, G4LogicalVolume*);
  ~RICHMirrorWindow();
  void ReadGeometryParameters();
  void CreateGeometry();
  void SetProperties();
  void DefineOpticalSurface();

public:
        G4double  GetZLength(){return fZLength;}
        void SetZLength(G4double  value){fZLength=value;}
        G4double  GetThickness(){return fThickness;}
        void SetThickness(G4double  value){fThickness=value;}
        G4double  GetInnerRadius(){return fInnerRadius;}
        void SetInnerRadius(G4double  value){fInnerRadius=value;}
        G4double  GetOuterRadius(){return fOuterRadius;}
        void SetOuterRadius(G4double  value){fOuterRadius=value;}
        G4double  GetZPosition(){return fZPosition;}
        void SetZPosition(G4double  value){fZPosition=value;}

	G4double  GetInnerFlangeZLength(){return fInnerFlangeZLength;}
        void SetInnerFlangeZLength(G4double  value){fInnerFlangeZLength=value;}
        G4double  GetInnerFlangeInnerRadius(){return fInnerFlangeInnerRadius;}
        void SetInnerFlangeInnerRadius(G4double  value){fInnerFlangeInnerRadius=value;}
        G4double  GetInnerFlangeRadialThickness(){return fInnerFlangeRadialThickness;}
        void SetInnerFlangeRadialThickness(G4double  value){fInnerFlangeRadialThickness=value;}
        G4double  GetInnerFlangeZPosition(){return fInnerFlangeZPosition;}
        void SetInnerFlangeZPosition(G4double  value){fInnerFlangeZPosition=value;}

        G4double  GetOuterFlangeZLength(){return fOuterFlangeZLength;}
        void SetOuterFlangeZLength(G4double  value){fOuterFlangeZLength=value;}
        G4double  GetOuterFlangeInnerRadius(){return fOuterFlangeInnerRadius;}
        void SetOuterFlangeInnerRadius(G4double  value){fOuterFlangeInnerRadius=value;}
        G4double  GetOuterFlangeRadialThickness(){return fOuterFlangeRadialThickness;}
        void SetOuterFlangeRadialThickness(G4double  value){fOuterFlangeRadialThickness=value;}
        G4double  GetOuterFlangeZPosition(){return fOuterFlangeZPosition;}
        void SetOuterFlangeZPosition(G4double  value){fOuterFlangeZPosition=value;}

        G4double  GetInterfaceRingZLength(){return fInterfaceRingZLength;}
        void SetInterfaceRingZLength(G4double  value){fInterfaceRingZLength=value;}
        G4double  GetInterfaceRingInnerRadius(){return fInterfaceRingInnerRadius;}
        void SetInterfaceRingInnerRadius(G4double  value){fInterfaceRingInnerRadius=value;}
        G4double  GetInterfaceRingRadialThickness(){return fInterfaceRingRadialThickness;}
        void SetInterfaceRingRadialThickness(G4double  value){fInterfaceRingRadialThickness=value;}
        G4double  GetInterfaceRingZPosition(){return fInterfaceRingZPosition;}
        void SetInterfaceRingZPosition(G4double  value){fInterfaceRingZPosition=value;}

        G4OpticalSurface * GetOpticalSurface(){return fOpticalSurface;};
        void SetOpticalSurface(G4OpticalSurface * value){fOpticalSurface=value;};


private:

        G4double  fZLength; 
        G4double  fThickness; 
        G4double  fInnerRadius; 
        G4double  fOuterRadius; 
        G4double  fZPosition; 

        G4double  fInnerFlangeZLength;
        G4double  fInnerFlangeInnerRadius;
        G4double  fInnerFlangeRadialThickness;
        G4double  fInnerFlangeZPosition;       

        G4double  fOuterFlangeZLength;
        G4double  fOuterFlangeInnerRadius;
        G4double  fOuterFlangeRadialThickness;
        G4double  fOuterFlangeZPosition;

        G4double  fInterfaceRingZLength;
        G4double  fInterfaceRingInnerRadius;
        G4double  fInterfaceRingRadialThickness;
        G4double  fInterfaceRingZPosition;

        G4VPhysicalVolume* fPhysicalInnerFlange;
        G4VPhysicalVolume* fPhysicalOuterFlange;
        G4VPhysicalVolume* fPhysicalInterfaceRing;
        
        G4VPhysicalVolume* fPhysicalNeon;


        G4OpticalSurface* fOpticalSurface;

};

#endif
