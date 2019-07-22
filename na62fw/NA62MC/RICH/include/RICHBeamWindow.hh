#ifndef RICHBeamWindow_H
#define RICHBeamWindow_H 1

#include "NA62VComponent.hh"
#include "RICHGeometryParameters.hh"
#include "globals.hh"
#include "G4OpticalSurface.hh"

class G4LogicalVolume;
class G4VPhysicalVolume;
class G4Material;
class G4VisAttributes;

class RICHBeamWindow : public NA62VComponent
{

    public:

        RICHBeamWindow(G4Material*, G4LogicalVolume*);
        ~RICHBeamWindow();
        void ReadGeometryParameters();
        void CreateGeometry();
        void SetProperties();
        void DefineOpticalSurface();

    public:

        G4double  GetOuterFlangeZLength()                      {return fOuterFlangeZLength;      };
        void SetOuterFlangeZLength(G4double value)             {fOuterFlangeZLength=value;       };
        G4double  GetOuterFlangeInnerRadius()                  {return fOuterFlangeInnerRadius;  };
        void SetOuterFlangeInnerRadius(G4double value)         {fOuterFlangeInnerRadius=value;   };
        G4double  GetOuterFlangeRadialThickness()              {return fOuterFlangeRadialThickness;  };
        void SetOuterFlangeRadialThickness(G4double value)     {fOuterFlangeRadialThickness=value;   };
        G4double  GetOuterFlangeZPosition()                    {return fOuterFlangeZPosition;        };
        void SetOuterFlangeZPosition(G4double value)           {fOuterFlangeZPosition=value;         };
        G4double  GetOuterFlangeZShift()                       {return fOuterFlangeZShift;        };
        void SetOuterFlangeZShift(G4double value)           {fOuterFlangeZShift=value;         };


        G4double  GetSphericalWindowZLength()                  {return fSphericalWindowZLength;      };
        void SetSphericalWindowZLength(G4double value)         {fSphericalWindowZLength=value;       };
        G4double  GetSphericalWindowOuterRadius()              {return fSphericalWindowOuterRadius;  };
        void SetSphericalWindowOuterRadius(G4double value)     {fSphericalWindowOuterRadius=value;   };
        G4double  GetSphericalWindowInnerRadius()              {return fSphericalWindowInnerRadius;  };
        void SetSphericalWindowInnerRadius(G4double value)     {fSphericalWindowInnerRadius=value;   };
        G4double  GetSphericalWindowThickness()                {return fSphericalWindowThickness;    };
        void SetSphericalWindowThickness(G4double value)       {fSphericalWindowThickness=value;     };
        G4double  GetSphericalWindowZPosition()                {return fSphericalWindowZPosition;    };
        void SetSphericalWindowZPosition(G4double value)       {fSphericalWindowZPosition=value;     };

        G4double  GetInnerFlangeZLength()                      {return fInnerFlangeZLength;      };
        void SetInnerFlangeZLength(G4double value)             {fInnerFlangeZLength=value;       };
        G4double  GetInnerFlangeInnerRadius()                  {return fInnerFlangeInnerRadius;  };
        void SetInnerFlangeInnerRadius(G4double value)         {fInnerFlangeInnerRadius=value;   };
        G4double  GetInnerFlangeRadialThickness()              {return fInnerFlangeRadialThickness;    };
        void SetInnerFlangeRadialThickness(G4double value)     {fInnerFlangeRadialThickness=value;     };
        G4double  GetInnerFlangeZShift()                       {return fInnerFlangeZShift;    };
        void SetInnerFlangeZShift(G4double value)              {fInnerFlangeZShift=value;     };
        G4double  GetInnerFlangeZPosition()                    {return fInnerFlangeZPosition;    };
        void SetInnerFlangeZPosition(G4double value)           {fInnerFlangeZPosition=value;     };

        G4OpticalSurface * GetOpticalSurface(){return fOpticalSurface;};
        void SetOpticalSurface(G4OpticalSurface * value){fOpticalSurface=value;};

    private:

       G4double fOuterFlangeZLength;
       G4double fOuterFlangeInnerRadius;
       G4double fOuterFlangeRadialThickness;
       G4double fOuterFlangeZPosition;  
       G4double fOuterFlangeZShift;

       G4double fSphericalWindowZLength;
       G4double fSphericalWindowOuterRadius;
       G4double fSphericalWindowInnerRadius;
       G4double fSphericalWindowThickness;
       G4double fSphericalWindowZPosition;

       G4double fInnerFlangeZLength;
       G4double fInnerFlangeInnerRadius;
       G4double fInnerFlangeRadialThickness;
       G4double fInnerFlangeZShift;
       G4double fInnerFlangeZPosition;

       G4LogicalVolume* fLogicNeon;
       G4VPhysicalVolume* fPhysicalNeon;
       G4OpticalSurface* fOpticalSurface;

};

#endif
