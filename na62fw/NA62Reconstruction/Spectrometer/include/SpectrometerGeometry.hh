#ifndef SpectrometerGeometry_H
#define SpectrometerGeometry_H 1

#include "Straw.hh"
#include "SpectrometerParameters.hh"

#include "TROOT.h"
#include "TVector2.h"
#include "TVector3.h"

#include <vector>

class SpectrometerGeometry
{

public:

  SpectrometerGeometry();
  ~SpectrometerGeometry();
  // delete copy constructor and copy assignment
  SpectrometerGeometry(SpectrometerGeometry const&) = delete;
  void operator=(SpectrometerGeometry const&) = delete;
  // singleton class
  static SpectrometerGeometry& GetInstance() {
    static SpectrometerGeometry instance; // instantiated on first use, guaranteed to be destroyed
    return instance;
  }
  Straw * GetStraw(Int_t);
  void CreateGeometry();
  Int_t StrawAcceptance(Int_t iChamber, TVector2 vPos, Int_t zone);
  Int_t StrawAcceptance(Int_t iChamber, TVector3 vPos, Int_t zone) { // overload for TVector3
    return StrawAcceptance(iChamber, TVector2(vPos.X(), vPos.Y()), zone);
  }
  Int_t StrawAcceptance(Int_t iChamber, Double_t *xyuv, Int_t zone) { // overload for arrays
    return StrawAcceptance(iChamber, TVector2(xyuv[0], xyuv[1]), zone);
  }

  Double_t * GetChamberZPosition()                              { return fChamberZPosition;             };
  Double_t   GetChamberZPosition(Int_t i)                       { return fChamberZPosition[i];          };
  void       SetChamberZPosition(Int_t i, Double_t value)       { fChamberZPosition[i] = value;         };
  Double_t * GetChamberHoleCenterX()                            { return fChamberHoleCenterX;           };
  Double_t   GetChamberHoleCenterX(Int_t i)                     { return fChamberHoleCenterX[i];        };
  void       SetChamberHoleCenterX(Int_t i, Double_t value)     { fChamberHoleCenterX[i] = value;       };
  Double_t * GetViewRotationAngle()                             { return fViewRotationAngle;            };
  Double_t   GetViewRotationAngle(Int_t i)                      { return fViewRotationAngle[i];         };
  void       SetViewRotationAngle(Int_t i, Double_t value)      { fViewRotationAngle[i] = value;        };
  Double_t   GetViewZPosition(Int_t jChamber, Int_t i)          { return fViewZPosition[jChamber][i];   };
  void       SetViewZPosition(Int_t jChamber, Int_t i, Double_t value) { fViewZPosition[jChamber][i] = value; };
  Double_t   GetStrawDiameter()                                 { return fStrawDiameter;                };
  void       SetStrawDiameter(Double_t value)                   { fStrawDiameter = value;               };
  Double_t   GetViewSize()                                      { return fViewSize;                     };
  void       SetViewSize(Double_t value)                        { fViewSize = value;                    };
  Double_t   GetStrawSpacing()                                  { return fStrawSpacing;                 };
  void       SetStrawSpacing(Double_t value)                    { fStrawSpacing = value;                };
  Double_t   GetLayerSpacing()                                  { return fLayerSpacing;                 };
  void       SetLayerSpacing(Double_t value)                    { fLayerSpacing = value;                };
  Double_t   GetLayerDisplacement()                             { return fLayerDisplacement;            };
  void       SetLayerDisplacement(Double_t value)               { fLayerDisplacement = value;           };
  Double_t   GetHalfViewSpacing()                               { return fHalfViewSpacing;              };
  void       SetHalfViewSpacing(Double_t value)                 { fHalfViewSpacing = value;             };
  Double_t   GetHalfViewDisplacement()                          { return fHalfViewDisplacement;         };
  void       SetHalfViewDisplacement(Double_t value)            { fHalfViewDisplacement = value;        };
  Double_t   GetViewZLength()                                   { return fViewZLength;                  };
  void       SetViewZLength(Double_t value)                     { fViewZLength = value;                 };
  Double_t   GetViewSpacing(Int_t j)                            { return fViewSpacing[j];               };
  void       SetViewSpacing(Double_t value, Int_t j)            { fViewSpacing[j] = value;              };
  Double_t   GetChamberZLength()                                { return fChamberZLength;               };
  void       SetChamberZLength(Double_t value)                  { fChamberZLength = value;              };

  Double_t   GetChambernXRminY()                                { return fChambernXRminY;               };
  void       SetChambernXRminY(Double_t value)                  { fChambernXRminY = value;              };
  Double_t   GetChambernXRmaxY()                                { return fChambernXRmaxY;               };
  void       SetChambernXRmaxY(Double_t value)                  { fChambernXRmaxY = value;              };
  Double_t   GetChamber1XRminU()                                { return fChamber1XRminU;               };
  void       SetChamber1XRminU(Double_t value)                  { fChamber1XRmaxU = value;              };
  Double_t   GetChamber1XRmaxU()                                { return fChamber1XRmaxU;               };
  void       SetChamber1XRmaxU(Double_t value)                  { fChamber1XRminU = value;              };
  Double_t   GetChamber1XRminX()                                { return fChamber1XRminX;               };
  void       SetChamber1XRminX(Double_t value)                  { fChamber1XRmaxX = value;              };
  Double_t   GetChamber1XRmaxX()                                { return fChamber1XRmaxX;               };
  void       SetChamber1XRmaxX(Double_t value)                  { fChamber1XRminX = value;              };
  Double_t   GetChamber2XRminU()                                { return fChamber2XRminU;               };
  void       SetChamber2XRminU(Double_t value)                  { fChamber2XRmaxU = value;              };
  Double_t   GetChamber2XRmaxU()                                { return fChamber2XRmaxU;               };
  void       SetChamber2XRmaxU(Double_t value)                  { fChamber2XRminU = value;              };
  Double_t   GetChamber2XRminX()                                { return fChamber2XRminX;               };
  void       SetChamber2XRminX(Double_t value)                  { fChamber2XRmaxX = value;              };
  Double_t   GetChamber2XRmaxX()                                { return fChamber2XRmaxX;               };
  void       SetChamber2XRmaxX(Double_t value)                  { fChamber2XRminX = value;              };
  Double_t   GetChamber3XRminU()                                { return fChamber3XRminU;               };
  void       SetChamber3XRminU(Double_t value)                  { fChamber3XRmaxU = value;              };
  Double_t   GetChamber3XRmaxU()                                { return fChamber3XRmaxU;               };
  void       SetChamber3XRmaxU(Double_t value)                  { fChamber3XRminU = value;              };
  Double_t   GetChamber3XRminX()                                { return fChamber3XRminX;               };
  void       SetChamber3XRminX(Double_t value)                  { fChamber3XRmaxX = value;              };
  Double_t   GetChamber3XRmaxX()                                { return fChamber3XRmaxX;               };
  void       SetChamber3XRmaxX(Double_t value)                  { fChamber3XRminX = value;              };
  Double_t   GetChamber4XRminU()                                { return fChamber4XRminU;               };
  void       SetChamber4XRminU(Double_t value)                  { fChamber4XRmaxU = value;              };
  Double_t   GetChamber4XRmaxU()                                { return fChamber4XRmaxU;               };
  void       SetChamber4XRmaxU(Double_t value)                  { fChamber4XRminU = value;              };
  Double_t   GetChamber4XRminX()                                { return fChamber4XRminX;               };
  void       SetChamber4XRminX(Double_t value)                  { fChamber4XRminX = value;              };
  Double_t   GetChamber4XRmaxX()                                { return fChamber4XRmaxX;               };
  void       SetChamber4XRmaxX(Double_t value)                  { fChamber4XRmaxX = value;              };
  Double_t   GetViewHoleRadius()                                { return fViewHoleRadius;               };
  void       SetViewHoleRadius(Double_t value)                  { fViewHoleRadius = value;              };

  Double_t   GetMagnetZLength()                                 { return fMagnetZLength;                };
  void       SetMagnetZLength(Double_t value)                   { fMagnetZLength = value;               };
  Double_t   GetMagnetZPosition()                               { return fMagnetZPosition;              };
  void       SetMagnetZPosition(Double_t value)                 { fMagnetZPosition = value;             };
  Double_t   GetMagnetFieldStrength()                           { return fMagnetFieldStrength;          };
  void       SetMagnetFieldStrength(Double_t value)             { fMagnetFieldStrength = value;         };

  void Printout();

private:
  SpectrometerParameters &fPar;
  Double_t fChamberZPosition[4]; ///< z coordinate of the chambers (half position)
  Double_t fChamberHoleCenterX[4]; ///< x shift of the center of the chambers.
  Double_t fViewRotationAngle[4]; ///< Rotation angle of the views (sequence xyuv for increasing z).
  Double_t fStrawDiameter; ///< Diameter of one straw.
  Double_t fViewSize; ///< Length of the straws.
  Double_t fStrawSpacing; ///< Distance between straws of one plane along the transverse direction.
  Double_t fLayerSpacing; ///< Distance between the two straw planes of the same half view along the Z direction.
  Double_t fLayerDisplacement; ///< Shift between the two straw planes of the same half view along transverse direction.
  Double_t fHalfViewSpacing; ///< Distance between two half parts of one view.
  Double_t fHalfViewDisplacement; ///< Shift along the transverse direction between two half parts of one view.
  Double_t fViewZLength; ///< Thickness of one view.
  Double_t fViewSpacing[2]; ///< Distance between the last end of one views and the beginning of the next view.
  Double_t fChamberZLength; ///< Length of the chamber (from beatch file).
  Double_t fChambernXRminY; ///< Innermost coordinate to the z axis (negative side) for Y views.
  Double_t fChambernXRmaxY; ///< Innermost coordinate to the z axis (positive side) for Y views.
  Double_t fChamber1XRminU; ///< Innermost coordinate to the z axis (negative side) for the U view of Chamber 1.
  Double_t fChamber1XRmaxU; ///< Innermost coordinate to the z axis (positive side) for the U view of Chamber 1.
  Double_t fChamber1XRminX; ///< Innermost coordinate to the z axis (negative side) for the X view of Chamber 1.
  Double_t fChamber1XRmaxX; ///< Innermost coordinate to the z axis (positive side) for the X view of Chamber 1.
  Double_t fChamber2XRminU; ///< Innermost coordinate to the z axis (negative side) for the U view of Chamber 2.
  Double_t fChamber2XRmaxU; ///< Innermost coordinate to the z axis (positive side) for the U view of Chamber 2.
  Double_t fChamber2XRminX; ///< Innermost coordinate to the z axis (negative side) for the X view of Chamber 2.
  Double_t fChamber2XRmaxX; ///< Innermost coordinate to the z axis (positive side) for the X view of Chamber 2.
  Double_t fChamber3XRminU; ///< Innermost coordinate to the z axis (negative side) for the U view of Chamber 3.
  Double_t fChamber3XRmaxU; ///< Innermost coordinate to the z axis (positive side) for the U view of Chamber 3.
  Double_t fChamber3XRminX; ///< Innermost coordinate to the z axis (negative side) for the X view of Chamber 3.
  Double_t fChamber3XRmaxX; ///< Innermost coordinate to the z axis (positive side) for the X view of Chamber 3.
  Double_t fChamber4XRminU; ///< Innermost coordinate to the z axis (negative side) for the U view of Chamber 4.
  Double_t fChamber4XRmaxU; ///< Innermost coordinate to the z axis (positive side) for the U view of Chamber 4.
  Double_t fChamber4XRminX; ///< Innermost coordinate to the z axis (negative side) for the X view of Chamber 4.
  Double_t fChamber4XRmaxX; ///< Innermost coordinate to the z axis (positive side) for the X view of Chamber 4.
  Double_t fViewHoleRadius; ///< Average size of the central aperture.
  Double_t fMagnetZLength; ///< Thickness of the magnet.
  Double_t fMagnetZPosition; ///< Position of the middle of the magnet along Z.
  Double_t fMagnetFieldStrength; ///< Field strength.
  Double_t fViewZPosition[4][4]; ///< Position along Z of the chamber views.
  Double_t fHoleChamberMax[4][4];
  Double_t fHoleChamberMin[4][4];
  Double_t fXPlaneOffset[4][4];
  Double_t fZLocalView[4];
  Double_t fZLocalPlane[4];
  Double_t fViewPlaneTransverseSize;

  std::vector<Straw*> fStraws;   ///< Pointer to the straw class.
};
#endif
