#ifndef RICHPMTsWindow_H
#define RICHPMTsWindow_H 1

#include "NA62VComponent.hh"
#include "RICHGeometryParameters.hh"
#include "globals.hh"

#include "G4OpticalSurface.hh"

#include "RICHOpticalDetector.hh"

class RICHPMTsWindow : public NA62VComponent
{

    public:


        RICHPMTsWindow(G4Material*, G4LogicalVolume*);
        ~RICHPMTsWindow();
        void ReadGeometryParameters();
        void CreateGeometry();
        void SetProperties();
        void DefineOpticalSurface();

    public:

        G4double GetInnerRadius()                               {return fInnerRadius;         };
        void     SetInnerRadius(G4double value)                 {fInnerRadius = value;        };

        G4double GetOuterFlangeZLength()                        {return fOuterFlangeZLength;         };          
        void     SetOuterFlangeZLength(G4double value)          {fOuterFlangeZLength = value;        };
        G4double GetOuterFlangeInnerRadius()                    {return fOuterFlangeInnerRadius;     };  
        void     SetOuterFlangeInnerRadius(G4double value)      {fOuterFlangeInnerRadius = value;    };
        G4double GetOuterFlangeRadialThickness()                {return fOuterFlangeRadialThickness; };
        void     SetOuterFlangeRadialThickness(G4double value)  {fOuterFlangeRadialThickness = value;};
        G4double GetOuterFlangeZPosition()                      {return fOuterFlangeZPosition;        };
        void     SetOuterFlangeZPosition(G4double value)        {fOuterFlangeZPosition = value;       };

        G4double GeConicalWindowZLength()                       {return fConicalWindowZLength;         };
        void     SetConicalWindowZLength(G4double value)        {fConicalWindowZLength = value;        };
        G4double GetConicalWindowInnerRadius()                  {return fConicalWindowInnerRadius;     };
        void     SetConicalWindowInnerRadius(G4double value)    {fConicalWindowInnerRadius = value;    };
        G4double GetConicalWindowOuterRadius()                  {return fConicalWindowOuterRadius;     };
        void     SetConicalWindowOuterRadius(G4double value)    {fConicalWindowOuterRadius = value;    };
        G4double GetConicalWindowThickness()                    {return fConicalWindowThickness;       };
        void     SetConicalWindowThickness(G4double value)      {fConicalWindowThickness = value;      };
        G4double GetConicalWindowZPosition()                    {return fConicalWindowZPosition;       };
        void     SetConicalWindowZPosition(G4double value)      {fConicalWindowZPosition = value;      };

        G4double GetInnerFlangeZLength()                        {return fInnerFlangeZLength;         };
        void     SetInnerFlangeZLength(G4double value)          {fInnerFlangeZLength = value;        };
        G4double GetInnerFlangeInnerRadius()                    {return fInnerFlangeInnerRadius;     };
        void     SetInnerFlangeInnerRadius(G4double value)      {fInnerFlangeInnerRadius = value;    };
        G4double GetInnerFlangeRadialThickness()                {return fInnerFlangeRadialThickness; };
        void     SetInnerFlangeRadialThickness(G4double value)  {fInnerFlangeRadialThickness = value;};
        G4double GetInnerFlangeZPosition()                      {return fInnerFlangeZPosition;        };
        void     SetInnerFlangeZPosition(G4double value)        {fInnerFlangeZPosition = value;       };

        G4double GetPMTsTubeZLength()                            {return fPMTsTubeZLength;             };
        void     SetPMTsTubeZLength(G4double value)              {fPMTsTubeZLength = value;            };
        G4TwoVector* GetPMTsTubeCenter()                         {return fPMTsTubeCenter;              };
        void     SetPMTsTubeCenter(G4TwoVector * value)          { fPMTsTubeCenter = value;            };
        G4double GetPMTsTubeInnerRadius()                        {return fPMTsTubeInnerRadius;         };
        void     SetPMTsTubeInnerRadius(G4double value)          {fPMTsTubeInnerRadius = value;        };
        G4double GetPMTsTubeRadialThickness()                    {return fPMTsTubeRadialThickness;     };
        void     SetPMTsTubeRadialThickness(G4double value)      {fPMTsTubeRadialThickness = value;    };
        G4double GetPMTsTubeZPosition()                          {return fPMTsTubeZPosition;           };
        void     SetPMTsTubeZPosition(G4double value)            {fPMTsTubeZPosition = value;          };

        G4double GetPMTsDiskInnerRadius()                        {return fPMTsDiskInnerRadius;         };
        void     SetPMTsDiskInnerRadius(G4double value)          {fPMTsDiskInnerRadius = value;        };
        G4double GetPMTsDiskOuterRadius()                        {return fPMTsDiskOuterRadius;         };
        void     SetPMTsDiskOuterRadius(G4double value)          {fPMTsDiskOuterRadius = value;        };
        G4double GetPMTsDiskZLength()                            {return fPMTsDiskZLength;             };
        void     SetPMTsDiskZLength(G4double value)              {fPMTsDiskZLength = value;            };
        G4TwoVector* GetPMTsDiskCenter()                         {return fPMTsDiskCenter;              };
        void     SetPMTsDiskCenter(G4TwoVector * value)          { fPMTsDiskCenter = value;            };
        G4double GetPMTsDiskZPosition()                          {return fPMTsDiskZPosition;           };
        void     SetPMTsDiskZPosition(G4double value)            {fPMTsDiskZPosition = value;          };


 
        RICHOpticalDetector ** GetOpticalDetector()                               { return fOpticalDetector;              };
        void                   SetOpticalDetector(RICHOpticalDetector ** value)   { fOpticalDetector = value;             };

        G4OpticalSurface *   GetOpticalSurface()                                { return fOpticalSurface;               };
        void                 SetOpticalSurface(G4OpticalSurface * value)        { fOpticalSurface = value;              };

        G4double GetNPMs()                          {return fNPMs;           };
        void     SetNPMs(G4double value)            {fNPMs = value;          };



    private:

    G4double   fInnerRadius;

    G4double   fOuterFlangeZLength;
    G4double   fOuterFlangeInnerRadius;
    G4double   fOuterFlangeRadialThickness;
    G4double   fOuterFlangeZPosition;
        
    G4double   fConicalWindowZLength;
    G4double   fConicalWindowThickness;
    G4double   fConicalWindowOuterRadius;
    G4double   fConicalWindowInnerRadius;
    G4double   fConicalWindowZPosition;
 
    G4double   fInnerFlangeZLength;
    G4double   fInnerFlangeInnerRadius;
    G4double   fInnerFlangeRadialThickness;
    G4double   fInnerFlangeZPosition;

    G4double   fPMTsTubeZLength;
    G4TwoVector* fPMTsTubeCenter;
    G4double   fPMTsTubeInnerRadius;     
    G4double   fPMTsTubeRadialThickness;
    G4double   fPMTsTubeZPosition;

    G4double   fPMTsDiskInnerRadius;
    G4double   fPMTsDiskOuterRadius;
    G4double   fPMTsDiskZLength;
    G4TwoVector* fPMTsDiskCenter;
    G4double   fPMTsDiskZPosition;

   
    RICHOpticalDetector** fOpticalDetector;
    G4OpticalSurface* fOpticalSurface;
   
    G4double fNPMs;

    G4VPhysicalVolume * fPhysicalNeon;
};

#endif
