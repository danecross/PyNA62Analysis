#ifndef CHANTIGeometry_H
#define CHANTIGeometry_H 1

#include "TString.h"

#include "vector"

using namespace std;

class CHANTIGeometry
{

public:

  CHANTIGeometry();
  ~CHANTIGeometry();
  static CHANTIGeometry* GetInstance();

private:

  static CHANTIGeometry* fInstance;

private:

  void CreateGeometry();

public:

  Double_t             GetWorldZLength()                                  { return fWorldZLength;                 }
  void                 SetWorldZLength(Double_t value)                    { fWorldZLength = value;                }
  Double_t             GetWorldXLength()                                  { return fWorldXLength;                 }
  void                 SetWorldXLength(Double_t value)                    { fWorldXLength = value;                }
  Double_t             GetWorldYLength()                                  { return fWorldYLength;                 }
  void                 SetWorldYLength(Double_t value)                    { fWorldYLength = value;                }

  Double_t             GetDetectorZPosition()                         { return fDetectorZPosition;        }
  void                 SetDetectorZPosition(Double_t value)           { fDetectorZPosition = value;       }

  Double_t             GetDetectorZLength()                           { return fDetectorZLength;          }
  void                 SetDetectorZLength(Double_t value)             { fDetectorZLength = value;         }
  Double_t             GetDetectorXLength()                           { return fDetectorXLength;          }
  void                 SetDetectorXLength(Double_t value)             { fDetectorXLength = value;         }
  Double_t             GetDetectorYLength()                           { return fDetectorYLength;          }
  void                 SetDetectorYLength(Double_t value)             { fDetectorYLength = value;         }

  Double_t GetInnerRadius()                                      {return fInnerRadius;      }
  void     SetInnerRadius(Double_t value)                        {fInnerRadius = value;      }
  Double_t GetOuterRadius()                                      {return fOuterRadius; }
  void     SetOuterRadius(Double_t value)                        {fOuterRadius = value; }
   
  std::vector<int> GetStripIDRing(int value)                               {return fStripID.at(value); }     
  							             
  Double_t GetTheta()                                            {return fTheta;  }
  void     SetTheta(Double_t value)                              {fTheta = value; }
  						                      
  Double_t GetRingThickness()                                    {return fRingThickness;        } 
  void     SetRingThickness(Double_t value)                      {fRingThickness = value;       } 
  Double_t GetSquareLength()                                     {return fSquareLength;         } 
  void     SetSquareLength(Double_t value)                       {fSquareLength = value;        } 
  Double_t GetTriangleBase()                                     {return fTriangleBase;         } 
  void     SetTriangleBase(Double_t value)                       {fTriangleBase = value;        } 
  
  // rings infos
  Int_t    GetNofRings()                                                 { return fNofRings;  }
  void     SetNofRings(Int_t value)                                       { fNofRings=value;   }
  Double_t GetZPos_Ring(Int_t N)                                      { return fZPos_Ring[N]; }   
  void     SetZPos_Ring(Double_t value, Int_t N)                          { fZPos_Ring[N]=value; }   

private:

  Double_t fTriangleBase     ;
  Double_t fTriangleAltitude ;
  Double_t fSquareLength     ;
  Double_t fXInnerHoleLength ;
  Double_t fYInnerHoleLength ;


  Double_t  fWorldZLength;
  Double_t  fWorldXLength;
  Double_t  fWorldYLength;

  TString   fSensitiveDetectorName;
  TString   fCollectionName;

  Double_t  fDetectorZPosition;

  Double_t  fDetectorZLength;
  Double_t  fDetectorXLength;
  Double_t  fDetectorYLength;

  Int_t fNofTiles;
  Double_t fSuperpositionGap; // superposition of two tiles
  
  Double_t fInnerRadius;
  Double_t fOuterRadius;


  Double_t fTileThickness ;

  // DERIVED parameters
  Double_t fTheta;
  Double_t fRingThickness; 
  Double_t fRingOuterRadius;

  Int_t     fNofRings;
  Double_t* fZPos_Ring;

  std::vector< vector<int> > fStripID;
 
};
#endif
