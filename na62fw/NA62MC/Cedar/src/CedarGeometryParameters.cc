// --------------------------------------------------------------
// History:
//
// 2014-04-14 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - Cedar misalignment simulation introduced
//
// 2012-12-21 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - parameters are read from a configuration file
//
// 2012-08-16 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - various geometry updates
//
// 2012-06-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - various geometry updates
//
// 2012-02-22 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - more photodetector configurations added
//
// 2011-11-18 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - a series of geometry updates
//
// 2011-07-08 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - lightguide parameters; more realistic geometry
//
// 2011-06-10 Evgueni Goudzovski (eg@hep.ph.bham.ac.uk)
// - vessel optics parameters
//
// 2009-11-16 Evgueni Goudzovski
// - parameters of passive Cedar geometry + two quadrupoles (QFS077,079)
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------

/// \class CedarGeometryParameters
/// \Brief
/// Positions and dimensions of objects in the CEDAR responsibility region
/// \EndBrief

#include "TString.h"
#include "TRegexp.h"
#include "TObjString.h"
#include "CedarGeometryParameters.hh"
#include "NA62Global.hh"
#include "NA62ConditionsService.hh"

CedarGeometryParameters* CedarGeometryParameters::fInstance = 0;

CedarGeometryParameters::CedarGeometryParameters() :
  NA62VGeometryParameters(G4String("Cedar")),
  fNSectors(8)
{

  // The number of octants: must not be modified

  fZSphericalMirrorCapCentreMisalignmentX = new G4double[fNSectors];
  fZSphericalMirrorCapCentreMisalignmentZ = new G4double[fNSectors];
  fZSphericalMirrorCapCentre              = new G4double[fNSectors];
  fZSphericalMirrorCentre                 = new G4double[fNSectors];
  fSphericalMirrorPosition                = new G4ThreeVector[fNSectors];

  ParseConfFile("Cedar-MCSettings.dat");

  // The beam is passed from TURTLE to Geant4 at z=69.175m.
  // Definition of the Cedar responsibility region (in the NA62 frame):
  // it starts just downstream to allow the definition of the first checkpoint in front of the Cedar.
  // The start of the responsibility region is NOT the origin of the Cedar reference frame.
  // The origin of the Cedar reference frame is at = 69.278m, as specified later.

  fZminCedarRegionLab      = 69.200*m;
  fZmaxCedarRegionLab      = 79.440*m;
  fCedarRegionZPositionLab = 0.5 * (fZminCedarRegionLab+fZmaxCedarRegionLab);
  fCedarRegionZLength      = fZmaxCedarRegionLab - fZminCedarRegionLab;
  fCedarRegionXLength      = 1.2*m;
  fCedarRegionYLength      = 1.2*m;

  fResponsibilityRegion.push_back
    (new ResponsibilityRegion(fZminCedarRegionLab, fZmaxCedarRegionLab));

  // Some radial offsets (used below)
  fQuartzWindowRadialOffset     = 103.00*mm;
  fQuartzWindowRadius           =  22.50*mm;
  fLightGuideCentreRadialOffset =  50.00*mm;
  if (iLGType==1 || iLGType==3) { // pre-2012 PMT arrangements
    fLightGuideCentreRadialOffset = fQuartzWindowRadialOffset;
  }

  /////////////////////////////////////////////////////////////////
  //
  // External lenses ("optical caps"): UV quality lenses from Edmund Optics
  //
  // Plano-convex, D=50mm.
  // Stock numbers: NT67-xxx. Price: ~ GBP 140.
  //
  // Effective focal length (mm): 75, 100, 150, 200, 250.
  // R(curvature) (mm) = 34.39, 45.85, 68.77, 91.69, 114.62.
  //
  // NB: fused quartz refractivity n=1.4585.
  // ==> For a plano-convex lens, R = 0.4585*f.
  //
  // Ordered for run 2012:
  // plano-convex, D=50mm, R=114.62mm, thickness centre/edge= 7.00mm/4.24mm.

  fExternalLensDiameter      =  50.00*mm;
  fExternalLensMinThickness  =   4.24*mm;
  fExternalLensSurfaceRadius = 114.62*mm;

  // Spherical mirrors: non-UV quality lenses from Edmund Optics,
  // coated at CERN to make mirrors.
  //
  // Plano-convex lenses, D=50mm. Stock numbers: NT69-628 ... NA62-636.
  // 
  // R(curvature) (mm) =
  // 25.84 (?), 39.24, 51.68, 64.60, 77.52, 90.44, 103.36, 129.21, 258.40.
  //
  // For run 2012, the following mirror type was used:
  //      R=77.52mm, D=50mm, thickness centre/edge =  9.00/4.86 mm
  // For run 2014, the following mirror types will be available:
  //   1) R=39.24mm, D=50mm, thickness centre/edge = 12.00/3.01 mm
  //   2) R=51.68mm, D=50mm, thickness centre/edge = 10.00/3.55 mm (currently the default)
  //   3) R=64.40mm, D=50mm, thickness centre/edge = 10.00/4.97 mm
  //
  // A spherical cap, rather than a plano-convex lens (=cylinder+cap) is simulated.

  fSphericalMirrorSurfaceRadius = 77.52*mm;
  if (iLGType==48) fSphericalMirrorSurfaceRadius = 51.68*mm;
  if (iLGType==64) fSphericalMirrorSurfaceRadius = 51.68*mm;
  fSphericalMirrorDiameter      = 50.00*mm;
  fSphericalMirrorCentralAngle  = 45.00*deg;

  /////////////////////////////////////////////////////////////////////

  fAtexCylinderMinRadius          = 184*mm;
  fAtexCylinderMaxRadius          = 185*mm;
  fAtexCylinderHoleDiameter       =  75*mm;
  fSphericalMirrorCapRadialOffset = 106*mm; // + 1.41*mm; // @@
  if (iLGType==32) fSphericalMirrorCapRadialOffset = 107*mm;

  // centre of the mirror sphere
  fSphericalMirrorCentreRadialOffset =
    fSphericalMirrorCapRadialOffset - fSphericalMirrorSurfaceRadius/sqrt(2.0);

  // Origin of the Cedar reference frame:
  // start of the old Cedar nose = start of the OLD entrance vacuum window.
  // The positions are defined below with respect to this origin.

  fZCedarOriginLab = 69.278*m;

  // Positions of elements with respect to the above origin
  fZRotBoxStart                     =    0*mm;
  fZFrontPipeStart                  =  379*mm;
  fZAtexCylinderStart               =  521*mm;
  fZSphericalMirrorMountStart       =  524*mm;
  fZLightGuideCentre                =  701*mm;
  fZSphericalMirrorCapCentreNominal =  701*mm; // + 1.41*mm; // @@
  fZExternalLensStart               =  829*mm;
  fZQuartzWindowStart               =  851*mm;
  fZMainVesselStart                 = 1200*mm;
  fZCondenserStart                  = 1211*mm;
  fZDiaphragmCentre                 = 1251*mm;
  fZRotCentre                       = 1259*mm;
  fZChromaticCorrectorStart         = 2234*mm;
  fZManginMirrorStart               = 5732*mm;
  fZExitWindowStart                 = 6021*mm;
  fZRotBoxEnd                       = 6022*mm;
  fZExitPipeStart                   = 6025*mm; // the gap allows CEDAR rotations

  fRotBoxXLength = 1.0*m;
  fRotBoxYLength = fRotBoxXLength;
  fRotBoxZLength = fZRotBoxEnd - fZRotBoxStart;
  fRotBoxZCentre = 0.5*(fZRotBoxEnd + fZRotBoxStart);

  // Cedar misalignment: rotation
  fDistanceBetweenFeet = 4347*mm;
  fXRotCentre          =    0*mm;
  fYRotCentre          = -448*mm;
  fRotAngleX           = +atan(fAlignmentMotorX/fDistanceBetweenFeet);
  fRotAngleY           = -atan(fAlignmentMotorY/fDistanceBetweenFeet);

  // Spherical mirror misalignment
  for (int i=0; i<fNSectors; i++) {
    fZSphericalMirrorCapCentre[i] =  fZSphericalMirrorCapCentreNominal;
    fZSphericalMirrorCapCentre[i] += fZSphericalMirrorCapCentreMisalignmentZ[i];
  }

  // Pre-2011 PMTs: entrance to PMTs
  if (iLGType==1) fZLightGuideCentre = 706*mm;

  // Middle of the 3-PMT holder in 2011: Z(QW) - (extension, std=331) - (half-depth)
  if (iLGType==3) fZLightGuideCentre = (851-331-20)*mm;

  for (int i=0; i<fNSectors; i++) {
    fZSphericalMirrorCentre[i] =
      fZSphericalMirrorCapCentre[i] - fSphericalMirrorSurfaceRadius/sqrt(2.0);
  }

  // Longitudunal sizes of elements

  fAtexCylinderZLength                    = 293.0*mm;
  fSphericalMirrorMountZLength            = 290.0*mm;
  fSphericalMirrorMountSupportRingZLength =  10.0*mm;
  fSphericalMirrorMountSupportRingRin     =  68.0*mm;
  fSphericalMirrorMountSupportRingRout    = 245.0*mm;
  fSphericalMirrorMountSupportRingHoleDia =  45.0*mm;
  fEntranceWindowZLength                  = 150.0*um;
  fExitWindowZLength                      = 200.0*um;

  fFrontPipeZLength           = fZQuartzWindowStart - fZFrontPipeStart;
  fQuartzWindowZLength        = 10*mm;
  fFrontVesselZLength         = fZMainVesselStart - fZQuartzWindowStart - fQuartzWindowZLength;
  fMainVesselCylinderZLength  = 4500*mm;
  fExitPipeZLength            = fZmaxCedarRegionLab - fZCedarOriginLab - fZExitPipeStart;
  fManginMirrorZLength        = 40*mm;
  fManginMirrorCoatingZLength = 50*um;
  fChromaticCorrectorZLength  = 20*mm;
  fDiaphragmZLength           = 0.5*mm;
  fCondenserZLength           = 10*mm;

  // Radii of elements
  fFrontPipeInnerRadius          =  52.5*mm;
  fFrontPipeOuterRadius          =  62.5*mm;
  fFrontVesselInnerRadius        = 139.0*mm;
  fFrontVesselOuterRadius        = 150.0*mm;
  fMainVesselInnerRadius         = 267.0*mm;
  fMainVesselOuterRadius         = 279.0*mm;
  fExitPipeInnerRadius1          =  37.5*mm; // upstream
  fExitPipeOuterRadius1          =  39.0*mm;
  fExitPipeInnerRadius2          =  47.5*mm; // downstream
  fExitPipeOuterRadius2          =  49.0*mm;
  fManginMirrorInnerRadius       =  50.0*mm;
  fManginMirrorOuterRadius       = 150.0*mm;
  fChromaticCorrectorInnerRadius =  75.0*mm;
  fChromaticCorrectorOuterRadius = 160.0*mm;
  fDiaphragmInnerRadius          =  50.0*mm;
  fDiaphragmOpeningRadius        = 100.0*mm;
  fDiaphragmAperturePhi          =  42.6*deg;
  if (fDiaphragmApertureR<0) fDiaphragmApertureR = (iGasH2) ? 3.0*mm : 2.0*mm;

  fInterCondenserAngle           =  1e-3;   // a gap of about 0.1mm

  fLightGuideInnerRadius         = 285.0*mm;
  fLightGuideOuterRadius         = 302.0*mm;
  fLightGuideDiameter            = 250.0*mm;

  // Test run 2011: Outer radius - Inner radius = Cylinder height.
  // NB: cone depth = 18mm, PMT length = 11.5mm.
  if (iLGType==3) {
    fLightGuideInnerRadius       =   0.0*mm;
    fLightGuideOuterRadius       =  40.0*mm;
    fLightGuideDiameter          =  73.0*mm;
  }

  fManginMirrorReflectingSurfaceRadius = 8610*mm;
  fManginMirrorRefractingSurfaceRadius = 6615*mm;
  fChromaticCorrectorRearSurfaceRadius = 1385*mm;
  fCondenserFrontSurfaceRadius         =  300*mm;
  fCondenserRadialOffset               =  130*mm;
  fCondenserOuterRadius                =   60*mm;
  fCondenserDistanceToCentre           =   17*mm;

  // Centres of the quadrupole magnets (LAB frame)
  fQFS077ZPositionLab = 76.705*m;
  fQFS079ZPositionLab = 78.705*m;
  fQFSMagnetZLength   = 0.80*m;

  //////////////////////////////////////////////////////////////////////////////////////////
  // Gradients of the quadrupole magnetic fields (QUAD 9, QUAD 10).
  // The upstream QUAD 9 defocuses the beam in x (focuses in y).
  // The downstream QUAD 10 focuses in x (defocuses in y).
  // The beam width is increased from +-27 mm at CEDAR to +-30 mm at GTK1,
  // and the beam height is decreased from +-15mm at CEDAR to +-13.5mm at GTK1 by the QUADs. 
  // Run-dependent scale factor can be applied to the nominal gradients at CedarDetector level.
  //
  // Source: /afs/cern.ch/user/e/eagroup/database/turtlin/k12hika+
  // See also Turtle, Halo and Transport datacards in NA62MC/Beam/datacard

  fQFSMagnet077NominalGradient = -9.19941*kilogauss / (50.0*mm); // QUAD  9
  fQFSMagnet079NominalGradient = +9.19941*kilogauss / (50.0*mm); // QUAD 10

  ////////////////////////////////////////////////////////
  // Positions wrt the centre of the responsibility region

  // Cedar origin in the RR frame
  G4double fZCedarOrigin = fZCedarOriginLab - fCedarRegionZPositionLab;

  // Cedar Origin in RotBox frame
  G4double fZCedarOriginRB = -fRotBoxZCentre;

  fRotBoxPosition = G4ThreeVector (0.0, 0.0, fZCedarOrigin + 0.5*fRotBoxZLength);

  //////////////////////////////////////
  // Positions in the rotation box frame

  fRotCentrePosition = G4ThreeVector
    (fXRotCentre, fYRotCentre, fZCedarOriginRB + fZRotCentre);

  fEntranceWindowPosition = G4ThreeVector
    (0.0, 0.0, fZCedarOriginRB + fZFrontPipeStart - 0.5*fEntranceWindowZLength);

  fFrontPipePosition = G4ThreeVector
    (0.0, 0.0, fZCedarOriginRB + fZFrontPipeStart + 0.5*fFrontPipeZLength);

  fQuartzWindowDiskPosition = G4ThreeVector
    (0.0, 0.0, fZCedarOriginRB + fZQuartzWindowStart + 0.5*fQuartzWindowZLength);

  fFrontVesselPosition = G4ThreeVector
    (0.0, 0.0, fZCedarOriginRB + fZQuartzWindowStart + fQuartzWindowZLength + 0.5*fFrontVesselZLength);

  fMainVesselCylinderPosition = G4ThreeVector
    (0.0, 0.0, fZCedarOriginRB + fZMainVesselStart + 0.5*fMainVesselCylinderZLength);

  fExitWindowPosition = G4ThreeVector
    (0.0, 0.0, fZCedarOriginRB + fZExitWindowStart + 0.5*fExitWindowZLength);

  fManginMirrorPosition = G4ThreeVector
    (0.0, 0.0, fZCedarOriginRB + fZManginMirrorStart + 0.5*fManginMirrorZLength);

  fChromaticCorrectorPosition = G4ThreeVector
    (0.0, 0.0, fZCedarOriginRB + fZChromaticCorrectorStart + 0.5*fChromaticCorrectorZLength);

  fDiaphragmPosition = G4ThreeVector
    (0.0, 0.0, fZCedarOriginRB + fZDiaphragmCentre);

  fCondenserPosition = G4ThreeVector
    (0.0, 0.0, fZCedarOriginRB + fZCondenserStart + 0.5*fCondenserZLength);

  //////////////////////////////////////////////////////////////////////

  fExitPipePosition = G4ThreeVector
    (0.0, 0.0, fZCedarOrigin + fZExitPipeStart + 0.5*fExitPipeZLength);

  fQFSMagnet077Position = G4ThreeVector
    (0.0, 0.0, fQFS077ZPositionLab - fCedarRegionZPositionLab);

  fQFSMagnet079Position = G4ThreeVector
    (0.0, 0.0, fQFS079ZPositionLab - fCedarRegionZPositionLab);

  /****************************************/
  /* Lightguide geometry                  */
  /****************************************/

  /////////////////////////////////////////////////////////////////////////////
  // Definition of lightguide angles.
  //
  // Theta is the polar angle between the Z axis and
  // the vector normal to the LG surface. In the LG centre, theta = pi/2.
  // A row is defined by cones with the same theta.
  //
  // Phi is the polar angle between the Y axis (in the octant reference frame)
  // and the vector normal to the LG surface.
  // In the LG centre, phi=0.
  // LG geometrical centre on the Y axis in the octant reference frame.
  /////////////////////////////////////////////////////////////////////////////

  fLightGuideConesPhiShift    =  3.654*deg;
  fLightGuideConesThetaShift  =  3.224*deg;

  fLightGuideConeOuterRadius  =  4.0*mm;
  fLightGuideConeLength       = 15.0*mm;
  fLightGuideConeOpeningAngle = 36.25*deg; // nominal=37deg leads to geom.conflicts (NARKD-1086)

  if (iLGType==3) { // 3-PMT prototype in 2011
    fLightGuideConeOuterRadius  =  4.0*mm;
    fLightGuideConeLength       = 18.0*mm;
    fLightGuideConeOpeningAngle = 37.0*deg;
  }

  fLightGuideConeInnerRadius  =
    fLightGuideConeOuterRadius +
    fLightGuideConeLength*tan(0.5*fLightGuideConeOpeningAngle);

  fPMTDiameter                = 15.9*mm;
  fPMTWindowDiameter          =  9.4*mm;
  fPMTPhotoCathodeDiameter    =  8.0*mm;
  fPMTLength                  =  1.5*mm; // the full 11.5mm PMT length is truncated to avoid geometry conflicts
  fPMTPreWindowLength         =  0.5*mm;
  fPMTWindowLength            =  0.5*mm;
  fPMTPhotoCathodeLength      =  0.1*mm;

  fOldPMTRadius               = 2.5*cm;
  fOldPMTWindowRadius         = fOldPMTRadius;
  fOldPMTPhotoCathodeRadius   = fOldPMTRadius;
  fOldPMTWindowLength         = 1.0*mm;
  fOldPMTPhotoCathodeLength   = 0.1*mm;
  fOldPMTLength               = fOldPMTWindowLength + fOldPMTPhotoCathodeLength + 0.1*mm;

  if (iLGType==0) { // no cones and PMTs
    fLightGuideNofRows      = 1;
    fLightGuideNofCones     = new G4int[fLightGuideNofRows];
    fLightGuideInstrumented = new G4int[fLightGuideNofRows];
    fLightGuideRowsPhiShift = new G4double[fLightGuideNofRows];
    for (int iRow=0; iRow<fLightGuideNofRows; iRow++) {
      fLightGuideNofCones[iRow]     = 0;
      fLightGuideInstrumented[iRow] = 0;
      fLightGuideRowsPhiShift[iRow] = 0.0;
    }
  }

  if (iLGType==32 || iLGType==48 || iLGType==64) {
    // 5+8+9+8+9+8+9+8=64 cones
    fLightGuideNofRows      = 8;
    fLightGuideNofCones     = new G4int[fLightGuideNofRows];
    fLightGuideInstrumented = new G4int[fLightGuideNofRows];
    fLightGuideRowsPhiShift = new G4double[fLightGuideNofRows];
    fLightGuideNofCones[0]  = 5;
    fLightGuideNofCones[1]  = 8;
    fLightGuideNofCones[2]  = 9;
    fLightGuideNofCones[3]  = 8;
    fLightGuideNofCones[4]  = 9;
    fLightGuideNofCones[5]  = 8;
    fLightGuideNofCones[6]  = 9;
    fLightGuideNofCones[7]  = 8;
    for (int iRow=0; iRow<fLightGuideNofRows; iRow++) {
      fLightGuideRowsPhiShift[iRow] = 0.0;
      fLightGuideInstrumented[iRow] = 0777; // 111111111
    }
  }

  // Arrangements with partically instrumented cones (bit-encoded)

  if (iLGType==32) {
    fLightGuideInstrumented[0] =    0; //     00000
    fLightGuideInstrumented[1] =  074; //  00111100
    fLightGuideInstrumented[2] = 0376; // 011111110
    fLightGuideInstrumented[3] = 0377; //  11111111
    fLightGuideInstrumented[4] = 0376; // 011111110
    fLightGuideInstrumented[5] = 0176; //  01111110
    fLightGuideInstrumented[6] =    0; // 000000000
    fLightGuideInstrumented[7] =    0; //  00000000
  }

  if (iLGType==48) {
    fLightGuideInstrumented[0] =   04; //     00100
    fLightGuideInstrumented[1] = 0176; //  01111110
    fLightGuideInstrumented[2] = 0376; // 011111110
    fLightGuideInstrumented[3] = 0377; //  11111111
    fLightGuideInstrumented[4] = 0777; // 111111111
    fLightGuideInstrumented[5] = 0377; //  11111111
    fLightGuideInstrumented[6] = 0376; // 011111110
    fLightGuideInstrumented[7] = 0102; //  01000010
  }

  // Total number of cones and PMTs in the lightguide
  fLightGuideNConesTotal = fLightGuideNPMTsTotal = 0;
  for (G4int iRow=0; iRow<fLightGuideNofRows; iRow++) {
    fLightGuideNConesTotal += fLightGuideNofCones[iRow];
    for (int iCone=0; iCone<fLightGuideNofCones[iRow]; iCone++) {
      fLightGuideNPMTsTotal += ((fLightGuideInstrumented[iRow]>>iCone)&1);
    }
  }

  //////////////////////////////////////////////////////////////////
  // For objects placed in the octant sub-regions:
  // compute positions wrt the (common) centre of these regions.
  // --> The octant centre coincides with the front pipe centre.
  // --> The central axis of each octant is its Y axis.

  fAtexCylinderPosition = G4ThreeVector
    (0.0, 0.0,
     fZAtexCylinderStart + 0.5*fAtexCylinderZLength
     - fZFrontPipeStart
     - 0.5*fFrontPipeZLength);

  fExternalLensPosition = G4ThreeVector
    (0.0, fQuartzWindowRadialOffset,
     fZExternalLensStart + 0.5*fExternalLensMinThickness
     - fZFrontPipeStart
     - 0.5*fFrontPipeZLength);

  for (int i=0; i<fNSectors; i++) {
    fSphericalMirrorPosition[i] = G4ThreeVector
      (-fZSphericalMirrorCapCentreMisalignmentX[i], // transverse misalignment
       fSphericalMirrorCentreRadialOffset,
       fZSphericalMirrorCentre[i] // longitudinal misalignment added here
       - fZFrontPipeStart
       - 0.5*fFrontPipeZLength);
  }

  fSphericalMirrorMountPosition = G4ThreeVector
    (0.0, 0.0,
     fZSphericalMirrorMountStart + 0.5*fSphericalMirrorMountZLength
     - fZFrontPipeStart - 0.5*fFrontPipeZLength);

  fLightGuidePosition = G4ThreeVector
    (0.0, fLightGuideCentreRadialOffset,
     fZLightGuideCentre
     - fZFrontPipeStart
     - 0.5*fFrontPipeZLength);

  fOldPMTPosition = G4ThreeVector
    (0.0, fLightGuideCentreRadialOffset,
     fZLightGuideCentre - 0.5*fOldPMTLength
     - fZFrontPipeStart
     - 0.5*fFrontPipeZLength);

  //////////////////////////////////////////////////////////////////
  // For objects placed in the octant sub-regions:
  // compute positions wrt the (common) centre of these regions.

  fAtexCylinderHolePosition = G4ThreeVector
    (0.0, 0.5*(fAtexCylinderMinRadius+fAtexCylinderMaxRadius),
     fZSphericalMirrorCapCentreNominal -
     (fZAtexCylinderStart + 0.5*fAtexCylinderZLength));

  /////////////////////////////////
  // Print configuration parameters

  G4cout << "[CedarGeometryParameters] Cedar/KTAG configuration: ";
  if (iGasH2) G4cout << "H2"; else G4cout << "N2";
  G4cout << "; diaphragm[mm] = "<<fDiaphragmApertureR/mm<<"; LightguideType = "<<iLGType<<"; alignment dx,dy[mm]="<<fAlignmentMotorX<<","<<fAlignmentMotorX<<G4endl;
}

//////////////////////////////////////////////////////////////////

void CedarGeometryParameters::ParseConfFile(TString ConfFileName) {

  // Set defaults
  iGasH2              = 0;
  fDiaphragmApertureR = 2.0*mm;
  iLGType             = 48;
  iOctantsEnabled     = 0xff; // all octants are enabled

  fAlignmentMotorX = fAlignmentMotorY = 0.0;
  for (int i=0; i<fNSectors; i++) {
    fZSphericalMirrorCapCentreMisalignmentX[i] = 0.0;
    fZSphericalMirrorCapCentreMisalignmentZ[i] = 0.0;
  }

  ///////////////////////////////
  // Read vales of the parameters

  TString Line;
  NA62ConditionsService::GetInstance()->Open(ConfFileName);
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(ConfFileName))) {
    if (Line.BeginsWith("#")) continue;
    else if (Line.BeginsWith("RadiatorGas")) {
      TObjArray *l = Line.Tokenize(" ");
      if (((TObjString*)(l->At(1)))->GetString().EqualTo("H2")) iGasH2 = 1;
      else if (((TObjString*)(l->At(1)))->GetString().EqualTo("N2")) iGasH2 = 0;
      else {
	G4cout << "[CedarGeometryParameters] Invalid gas requested" << G4endl;
	exit(kWrongConfiguration);
      }
    }
    else if (Line.BeginsWith("DiaphragmApertureR")) {
      fDiaphragmApertureR = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * mm;
    }
    else if (Line.BeginsWith("LightGuideType")) {
      iLGType = TString(Line(TRegexp("[0-9]+"))).Atoi();
      if (iLGType!=0 && iLGType!=1 && iLGType!=3 && iLGType!=32 && iLGType!=48 && iLGType!=64) {
	G4cout << "[CedarGeometryParameters] Invalid lightguide type " << iLGType << G4endl;
	exit(kWrongConfiguration);
      }
    }
    else if (Line.BeginsWith("OctantsEnabled")) {
      iOctantsEnabled = 0;
      TObjArray *l = Line.Tokenize(" ");
      for (int i=0; i<fNSectors; i++) {
	if (((TObjString*)(l->At(i+1)))->GetString().Atoi()) {
	  iOctantsEnabled |= (0x1<<i);
	}
      }
    }
    else if (Line.BeginsWith("MisalignmentX")) {
      fAlignmentMotorX = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * mm;
    }
    else if (Line.BeginsWith("MisalignmentY")) {
      fAlignmentMotorY = TString(Line(TRegexp("-*[0-9]+.*[0-9]*"))).Atof() * mm;
    }
    else if (Line.BeginsWith("SphericalMirrordZ")) {
      TObjArray *l = Line.Tokenize(" ");
      for (int i=0; i<fNSectors; i++) {
	G4double value = ((TObjString*)(l->At(i+1)))->GetString().Atof() * mm;
	fZSphericalMirrorCapCentreMisalignmentZ[i] = value;
      }
    }
    else if (Line.BeginsWith("SphericalMirrordX")) {
      TObjArray *l = Line.Tokenize(" ");
      for (int i=0; i<fNSectors; i++) {
	G4double value = ((TObjString*)(l->At(i+1)))->GetString().Atof() * mm;
	fZSphericalMirrorCapCentreMisalignmentX[i] = value;
      }
    }
  }
  NA62ConditionsService::GetInstance()->Close(ConfFileName);

  ////////////////
  // Sanity checks

  if (fabs(fAlignmentMotorX) > 50*mm) {
    G4cout << "[CedarGeometryParameters] Cedar X misalignment out of range: " <<
      fAlignmentMotorX << "mm (range: +-50mm)" << G4endl;
    exit(kWrongConfiguration);
  }
  if (fabs(fAlignmentMotorY) > 50*mm) {
    G4cout << "[CedarGeometryParameters] Cedar Y misalignment out of range: " <<
      fAlignmentMotorY << "mm (range: +-50mm)" << G4endl;
    exit(kWrongConfiguration);
  }

  for (G4int i=0; i<fNSectors; i++) {
    if (fabs(fZSphericalMirrorCapCentreMisalignmentZ[i])>20*mm) {
      G4cout << "[CedarGeometryParameters] Cedar mirror Z misalignment out of range: " <<
	fZSphericalMirrorCapCentreMisalignmentZ[i] << "mm (range: +-20mm)" << G4endl;
      exit(kWrongConfiguration);
    }
    if (fabs(fZSphericalMirrorCapCentreMisalignmentX[i])>4*mm) {
      G4cout << "[CedarGeometryParameters] Cedar mirror X misalignment out of range: " <<
	fZSphericalMirrorCapCentreMisalignmentX[i] << "mm (range: +-4mm)" << G4endl;
      exit(kWrongConfiguration);
    }
  }
}

/////////////////////////////////////////////////////////////

CedarGeometryParameters* CedarGeometryParameters::GetInstance() {
  if (!fInstance) fInstance = new CedarGeometryParameters();
  return fInstance;
}

/////////////////////////////////////////////////////////
// PMT numbering: "position ID" vs "sequential ID"

G4int CedarGeometryParameters::GetPMTSequentialID (G4int fPositionID) {
  fPositionID  -= 111; // numbering starting from zero
  G4int fOctant = fPositionID/100;
  G4int fRow    = (fPositionID%100)/10;
  G4int fCone   = fPositionID%10;
  G4int fSeqID  = fOctant*fLightGuideNConesTotal;
  for (int iRow=0; iRow<fRow; iRow++) fSeqID += fLightGuideNofCones[iRow];
  fSeqID += fCone;
  return fSeqID;
}

//////////////////////////////////////////////////////////////

TObjArray CedarGeometryParameters::GetHashTable() {
  return 0;
}

void CedarGeometryParameters::Print() {
  G4cout << "Cedar geometry printout" << G4endl;
  G4cout << "fCedarRegionZPosition [m]   = "<< fCedarRegionZPositionLab/m << G4endl
	 << "fCedarRegionZLength [m]     = "<< fCedarRegionZLength/m << G4endl
	 << "fCedarRegionXLength [m]     = "<< fCedarRegionXLength/m << G4endl
	 << "fCedarRegionYLength [m]     = "<< fCedarRegionYLength/m << G4endl;
  G4cout << "Cedar region Zmin, Zmax [m] = "<<
    fCedarRegionZPositionLab/m - 0.5*fCedarRegionZLength/m<<" "<<
    fCedarRegionZPositionLab/m + 0.5*fCedarRegionZLength/m<<G4endl;
}
