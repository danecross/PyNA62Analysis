// LAVGeometryParameters.hh
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
// 2009-03-02 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it) 
//   - First implementation of LAV geometry
// 2010-11-03 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Updated LAV stations positions
//   - Blocks geometry is now correct
//   - Added (part of) block support structure
// 2010-11-10 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Cleaned variables and methods names and logic
// 2010-11-23 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Added banana structure (slabs, columns, c-shapes)
// 2011-01-24 Domenico Di Filippo (difilippo@na.infn.it)
//   - Added pothocathode diameter
//   - Different sensitive detector names
//   - Added matrix file path
//   - Added some wrapping and phototube properties
// 2015-05-01 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Added blue tube structure
// 2019-06-11 Emanuele Leonardi (emanuele.leonardi@roma1.infn.it)
//   - Added parameters for displacement of central part of banana volume
//     to avoid block/banana overlaps
//
// --------------------------------------------------------------

#ifndef LAVGeometryParameters_H
#define LAVGeometryParameters_H 1

#include "globals.hh"
#include "G4TwoVector.hh"
#include "TObjArray.h"
#include "G4ThreeVector.hh"

#include "NA62VGeometryParameters.hh"

// Number of Responsibility Regions
#define NUMBER_OF_RESPONSIBILITY_REGIONS 5

// Number of different sections of the blue tube in the LAV RRs
#define NUMBER_OF_BLUETUBE_ZONES 14

// Number of rails in blue tube
#define NUMBER_OF_BTRAILS 2

// Number of vertices for blue tube rails
#define N_OF_BTRAIL_VERTICES 8

// Number of LAV stations
#define NUMBER_OF_VETOES 12

// Maximum Number of rings in a station
#define NUMBER_OF_RINGS 5

// Maximum Number of bananas in a ring
#define NUMBER_OF_BANANAS 16

// Number of different block shapes
#define NUMBER_OF_BLOCK_SHAPES 9

// Number of different banana shapes
#define NUMBER_OF_BANANA_SHAPES 4

// Number of vertices for banana's aluminum slab
#define N_OF_ALSLAB_VERTICES 11

// Number of aluminum columns in banana structure
#define N_OF_ALCOLUMNS 12

// Number of vertices for banana's aluminum C-shape
#define N_OF_ALCSHAPE_VERTICES 8

class LAVGeometryParameters : public NA62VGeometryParameters
{

public:

  ~LAVGeometryParameters();
  static LAVGeometryParameters* GetInstance();
  TObjArray GetHashTable();
  void Print();

private:

  static LAVGeometryParameters* fInstance;

protected:

  LAVGeometryParameters();

public:

  G4double GetWorldXLength() { return fWorldXLength; };
  G4double GetWorldYLength() { return fWorldYLength; };
  G4double GetWorldZLength() { return fWorldZLength; };
  void SetWorldXLength(G4double value) { fWorldXLength = value; };
  void SetWorldYLength(G4double value) { fWorldYLength = value; };
  void SetWorldZLength(G4double value) { fWorldZLength = value; };

  // Return info about LAV sensitive detector and hit collection
  G4String GetLAVFastSensitiveDetectorName()      { return fLAVFastSensitiveDetectorName; };
  G4String GetLAVLeadglassSensitiveDetectorName() { return fLAVLeadglassSensitiveDetectorName; };
  G4String GetLAVGuideSensitiveDetectorName()     { return fLAVGuideSensitiveDetectorName; };
  G4String GetLAVCathodeSensitiveDetectorName()   { return fLAVCathodeSensitiveDetectorName; };
  G4String GetLAVCollectionName()        { return fLAVCollectionName; };

  // Return info about responsibility regions
  G4int    GetNumberOfResponsibilityRegions()                  { return NUMBER_OF_RESPONSIBILITY_REGIONS; };
  G4double GetResponsibilityRegionZofFrontFace(const G4int id) { return fLAV_RR_ZofFrontFace[id]; };
  G4double GetResponsibilityRegionZofBackFace(const G4int id)  { return fLAV_RR_ZofBackFace[id]; };
  G4double GetResponsibilityRegionRadius(const G4int id)       { return fLAV_RR_Radius[id]; };

  // Return position and radius of blue tube zones
  G4int    GetNumberOfBlueTubeZones() { return NUMBER_OF_BLUETUBE_ZONES; };
  G4double GetBlueTubeZofFrontFace(const G4int id) { return fLAV_BT_ZofFrontFace[id]; };
  G4double GetBlueTubeZofBackFace(const G4int id)  { return fLAV_BT_ZofBackFace[id]; };
  G4double GetBlueTubeInnerRadius(const G4int id)  { return fLAV_BT_InnerRadius[id]; };
  G4double GetBlueTubeOuterRadius(const G4int id)  { return fLAV_BT_OuterRadius[id]; };
  G4int    BlueTubeHasRails(const G4int id)        { return fLAV_BT_HasRails[id]; };

  // Return number and vertices XY coordinates of blue tube rails
  G4int       GetNumberOfBTRails() { return NUMBER_OF_BTRAILS; };
  G4int       GetNumberOfBTRailVertices(const G4int id) { return fBTRailNVertices[id]; };
  G4TwoVector GetBTRailVertex(const G4int id, const G4int iv) { return fBTRailVertex[id][iv]; }

  G4int    GetTotalNumberOfVetoStations() { return NUMBER_OF_VETOES; };

  // Return responsibility region of stations and blue tube sections
  G4int    GetStationResponsibilityRegion(const G4int id) { return fLAV_Station_ResponsibilityRegion[id]; };
  G4int    GetBlueTubeResponsibilityRegion(const G4int id) { return fLAV_BT_ResponsibilityRegion[id]; };

  // Return block orientation/position in the absolute system;
  // Calling F the face of the leadglass on witch there
  // the lightguide interface:
  // LeadglassCenter - Center of Leadglass
  //                   (without the lightguide)
  // ZVersor - Normal versor of F
  //           directed inside the leadglass
  // YVersor - Normal versor of the face of
  //           the leadglass orthogonal to F
  //           directed inside the leadglass.
  // XVersor - Versor such as X,Y,Z is a 
  //           left-handed ortho-normal base
  G4double GetBlockPhi(const G4int chid);
  G4ThreeVector GetBlockXVersor(const G4int chid);
  G4ThreeVector GetBlockYVersor(const G4int chid);
  G4ThreeVector GetBlockZVersor(const G4int chid);
  G4ThreeVector GetLeadglassCenter(const G4int chid);

  // Return positions and dimensions of LAV stations
  G4double GetStationZofFrontFace(const G4int id) { return fLAV_Station_ZofFrontFace[id]; };
  G4double GetStationZofBackFace(const G4int id)  { return fLAV_Station_ZofBackFace[id]; };
  G4double GetStationZLength(const G4int id)      { return fLAV_Station_ZLength[id]; };
  G4double GetStationInnerRadius(const G4int id)  { return fLAV_Station_InnerRadius[id]; };
  G4double GetStationOuterRadius(const G4int id)  { return fLAV_Station_OuterRadius[id]; };

  // Return info about steel vessel structure
  // N.B. vessel global dimensions are those of the station
  G4double GetVesselOuterRadius(const G4int id) { return fLAV_Vessel_OuterRadius[id]; };
  G4double GetVesselInnerRadius(const G4int id) { return fLAV_Vessel_InnerRadius[id]; };
  G4double GetVesselThickness(const G4int id) { return fLAV_Vessel_Thickness[id]; };

  // Return info about position and numbers of bananas in station
  G4double GetStationFirstRingZPos(const G4int id)   { return fLAV_Station_FirstRingZPos[id]; };
  G4int    GetStationNRings(const G4int id)          { return fLAV_Station_NRings[id]; };
  G4int    GetStationNBlocksPerRing(const G4int id)  { return fLAV_Station_NBlocksPerRing[id]; };
  G4int    GetStationNBananasPerRing(const G4int id) { return fLAV_Station_NBananasPerRing[id]; };
  G4int    GetStationBananaType(const G4int id)      { return fLAV_Station_BananaType[id]; };
  G4double GetStationRingGap(const G4int id)         { return fLAV_Station_RingGap[id]; };
  G4double GetStationPhiReference(const G4int id)    { return fLAV_Station_PhiReference[id]; };
  G4double GetStationPhiRotationBetweenLayers(const G4int id) { return fLAV_Station_PhiRotationBetweenLayers[id]; };

  // Return type of crystal used for given station/layer
  G4int    GetTypeOfCrystal(const G4int is,const G4int il) { return fLAV_TypeOfCrystal[is][il]; };

  // Return geometrical values about single PbGl blocks

  G4int    GetTotalNumberOfBlockShapes()         { return NUMBER_OF_BLOCK_SHAPES; };

  G4int    GetBlockOpalId(const G4int id)        { return fBlockOpalId[id]; };
  G4int    GetBlockIdFromOpalId(const G4int);

  G4double GetBlockZLength(const G4int id)       { return fBlockZLength[id]; };
  G4double GetBlockL1Length(const G4int id)      { return fBlockL1Length[id]; };
  G4double GetBlockL2Length(const G4int id)      { return fBlockL2Length[id]; };
  G4double GetBlockL3Length(const G4int id)      { return fBlockL3Length[id]; };
  G4double GetBlockL4Length(const G4int id)      { return fBlockL4Length[id]; };
  G4double GetBlockW1Length(const G4int id)      { return fBlockW1Length[id]; };
  G4double GetBlockW2Length(const G4int id)      { return fBlockW2Length[id]; };

  G4double GetLightGuideZLength(const G4int id)  { return fLightGuideZLength[id]; };
  G4double GetLightGuideDiameter(const G4int id) { return fLightGuideDiameter[id]; };

  G4double GetCathodeDiameter(const G4int id)    { return GetLightGuideDiameter(id)*0.95; }
  G4double GetWrapFrontHole(const G4int)         { return fWrapFrontHole; }
  G4double GetWrapThick(const G4int)             { return fWrapThick; }
  G4double GetGlueThick(const G4int)             { return fGlueThick; }
  G4double GetAirThick(const G4int)              { return fAirThick; }

  G4double GetMuMetalZLength(const G4int id)     { return fMuMetalZLength[id]; };
  G4double GetMuMetalDiameter(const G4int id)    { return fMuMetalDiameter[id]; };
  G4double GetMuMetalThickness(const G4int id)   { return fMuMetalThickness[id]; };

  G4double GetSteelSlabThickness(const G4int id) { return fSteelSlabThickness[id]; };

  // Return geometrical values about bananas

  G4int    GetTotalNumberOfBananaShapes()   { return NUMBER_OF_BANANA_SHAPES; };

  G4int    GetBananaNBlocks(const G4int id)     { return fBananaNBlocks[id]; };
  G4double GetBananaPhiSpan(const G4int id)     { return fBananaPhiSpan[id]; };
  G4double GetBananaOuterRadius(const G4int id) { return fBananaOuterRadius[id]; };
  G4double GetBananaInnerRadius(const G4int id) { return fBananaInnerRadius[id]; };
  G4double GetBananaNotchLength(const G4int id) { return fBananaNotchLength[id]; };
  G4double GetBananaNotchAngle(const G4int id) { return fBananaNotchAngle[id]; };
  G4double GetBananaThickness(const G4int id)   { return fBananaThickness[id]; };
  G4double GetBananaThicknessTolerance(const G4int id) { return fBananaThicknessTolerance[id]; };
  G4double GetBananaExtraRadiusTolerance(const G4int id) { return fBananaExtraRadiusTolerance[id]; };

  G4int    GetAlSlabNVertices(const G4int id)   { return fAlSlabNVertices[id]; };
  G4TwoVector GetAlSlabVertex(const G4int id,const G4int iv) { return fAlSlabVertex[id][iv]; };
  G4double GetAlSlabThickness(const G4int id)   { return fAlSlabThickness[id]; };

  G4double GetAlColumnLength(const G4int id)    { return fAlColumnLength[id]; };
  G4double GetAlColumnDiameter(const G4int id)  { return fAlColumnDiameter[id]; };
  G4int    GetBananaNAlColumns(const G4int id)  { return fBananaNAlColumns[id]; };
  G4TwoVector GetAlColumnPosition(const G4int id,const G4int ic) { return fAlColumnPosition[id][ic]; };

  G4int    GetAlCShapeNVertices(const G4int id)    { return fAlCShapeNVertices[id]; };
  G4TwoVector GetAlCShapeVertex(const G4int id,const G4int iv) { return fAlCShapeVertex[id][iv]; };
  G4double GetAlCShapeHeight(const G4int id)       { return fAlCShapeHeight[id]; };
  G4double GetAlCShapeHoleDiameter(const G4int id) { return fAlCShapeHoleDiameter[id]; };
  G4TwoVector GetBananaAlCShapeScrewsPosition(const G4int id,const G4int ic,const G4int ih) {
    return fBananaAlCShapeScrewsPosition[id][ic][ih]; };

  // Path to matrices used in the fast simulation

  G4String GetLAVEfficiencyMatrix() {return fLAVEfficiencyMatrix;}
  void SetLAVEfficiencyMatrix(G4String path) {fLAVEfficiencyMatrix = path;}

  G4String GetLAVTimeMatrix() {return fLAVTimeMatrix;}
  void SetLAVTimeMatrix(G4String path) {fLAVTimeMatrix = path;}

private:

  G4double  fWorldZLength;
  G4double  fWorldXLength;
  G4double  fWorldYLength;

  // LAV sensitive detector and hit collection names
  G4String fLAVFastSensitiveDetectorName;
  G4String fLAVLeadglassSensitiveDetectorName;
  G4String fLAVGuideSensitiveDetectorName;
  G4String fLAVCathodeSensitiveDetectorName;
  G4String fLAVCollectionName;

  // LAV responsibility regions
  G4double fLAV_RR_ZofFrontFace[NUMBER_OF_RESPONSIBILITY_REGIONS];
  G4double fLAV_RR_ZofBackFace[NUMBER_OF_RESPONSIBILITY_REGIONS];
  G4double fLAV_RR_Radius[NUMBER_OF_RESPONSIBILITY_REGIONS];

  // Radius and position of blue tube in LAV responsibility regions
  // N.B. positions of front and back face are in the "world" coordinate system
  G4int fLAV_BT_ResponsibilityRegion[NUMBER_OF_BLUETUBE_ZONES];
  G4double fLAV_BT_ZofFrontFace[NUMBER_OF_BLUETUBE_ZONES];
  G4double fLAV_BT_ZofBackFace[NUMBER_OF_BLUETUBE_ZONES];
  G4double fLAV_BT_InnerRadius[NUMBER_OF_BLUETUBE_ZONES];
  G4double fLAV_BT_OuterRadius[NUMBER_OF_BLUETUBE_ZONES];
  G4int  fLAV_BT_HasRails[NUMBER_OF_BLUETUBE_ZONES];

  // Number and XY coordinates of blue tube rails' vertices
  G4int       fBTRailNVertices[NUMBER_OF_BTRAILS];    // Number of vertices of BT rail
  G4TwoVector fBTRailVertex[NUMBER_OF_BTRAILS][N_OF_BTRAIL_VERTICES];

  // LAV stations positions and dimensions
  // N.B. positions of front and back face are in the "world" coordinate system
  G4int fLAV_Station_ResponsibilityRegion[NUMBER_OF_VETOES];
  G4double fLAV_Station_ZofFrontFace[NUMBER_OF_VETOES];
  G4double fLAV_Station_ZofBackFace[NUMBER_OF_VETOES];
  G4double fLAV_Station_ZLength[NUMBER_OF_VETOES];

  // LAV inner and outer radius
  // Inner radius is the radius of the circle ~tangent to the internal face of the PbGl blocks
  G4double fLAV_Station_InnerRadius[NUMBER_OF_VETOES];
  // Outer radius is (for the moment) the radius at the external surface of the vessel
  G4double fLAV_Station_OuterRadius[NUMBER_OF_VETOES];

  // Steel vessel information
  G4double fLAV_Vessel_OuterRadius[NUMBER_OF_VETOES];
  G4double fLAV_Vessel_InnerRadius[NUMBER_OF_VETOES];
  G4double fLAV_Vessel_Thickness[NUMBER_OF_VETOES];

  // Number of rings in LAV station
  G4int fLAV_Station_NRings[NUMBER_OF_VETOES];

  // Position along Z of first ring wrt front face of station
  G4double fLAV_Station_FirstRingZPos[NUMBER_OF_VETOES];

  // Number of bananas in each ring
  G4int fLAV_Station_NBananasPerRing[NUMBER_OF_VETOES];
  G4int fLAV_Station_BananaType[NUMBER_OF_VETOES];

  // Number of blocks in each ring
  G4int fLAV_Station_NBlocksPerRing[NUMBER_OF_VETOES];

  // Gap between rings
  G4double fLAV_Station_RingGap[NUMBER_OF_VETOES];

  // Phi reference point for banana 0 of layer 0
  G4double fLAV_Station_PhiReference[NUMBER_OF_VETOES];

  // Phi rotation between consecutive layers
  G4double fLAV_Station_PhiRotationBetweenLayers[NUMBER_OF_VETOES];

  // Map OPAL type of crystals used for each ring of each station
  G4int fLAV_TypeOfCrystal[NUMBER_OF_VETOES][NUMBER_OF_RINGS];

  // Geometrical parameters for PbGl blocks
  G4int    fBlockOpalId[NUMBER_OF_BLOCK_SHAPES];        // Opal Id of block
  G4double fBlockZLength[NUMBER_OF_BLOCK_SHAPES];       // Z length (PbGl only)
  G4double fBlockL1Length[NUMBER_OF_BLOCK_SHAPES];      // L1 length (long base of back face)
  G4double fBlockL2Length[NUMBER_OF_BLOCK_SHAPES];      // L2 length (short base of back face)
  G4double fBlockL3Length[NUMBER_OF_BLOCK_SHAPES];      // L3 length (long base of front face)
  G4double fBlockL4Length[NUMBER_OF_BLOCK_SHAPES];      // L4 length (short base of front face)
  G4double fBlockW1Length[NUMBER_OF_BLOCK_SHAPES];      // W1 length (height of back face)
  G4double fBlockW2Length[NUMBER_OF_BLOCK_SHAPES];      // W2 length (height of front face)
  G4double fLightGuideZLength[NUMBER_OF_BLOCK_SHAPES];  // Light Guide Z length
  G4double fLightGuideDiameter[NUMBER_OF_BLOCK_SHAPES]; // Light Guide diameter
  G4double fMuMetalZLength[NUMBER_OF_BLOCK_SHAPES];     // u-metal cylinder Z length
  G4double fMuMetalDiameter[NUMBER_OF_BLOCK_SHAPES];    // u-metal cylinder external diameter
  G4double fMuMetalThickness[NUMBER_OF_BLOCK_SHAPES];   // u-metal cylinder thickness
  G4double fSteelSlabThickness[NUMBER_OF_BLOCK_SHAPES]; // Steel slab thickness
  G4double fWrapFrontHole;   // diameter of the hole in the small wrapping face
  G4double fWrapThick;       // wrap thickness
  G4double fAirThick;        // air thickness between leadglass and wrapping
  G4double fGlueThick;       // glue thickness betwen leadglass and metal plate

  // Geometrical parameters for bananas
  G4int    fBananaNBlocks[NUMBER_OF_BANANA_SHAPES];     // Number of blocks in banana (always 4)
  G4double fBananaPhiSpan[NUMBER_OF_BANANA_SHAPES];     // Angular span of banana
  G4double fBananaOuterRadius[NUMBER_OF_BANANA_SHAPES]; // External radius of banana (at vessel internal surface)
  G4double fBananaInnerRadius[NUMBER_OF_BANANA_SHAPES]; // Internal radius of banana (circle tangent to block front face)
  G4double fBananaNotchLength[NUMBER_OF_BANANA_SHAPES]; // Length along R of central notch to avoid block-banana overlap
  G4double fBananaNotchAngle[NUMBER_OF_BANANA_SHAPES]; // Angular displacement of central notch to avoid block-banana overlap
  G4double fBananaThickness[NUMBER_OF_BANANA_SHAPES];   // Thickness of banana at aluminum slab
  G4double fBananaThicknessTolerance[NUMBER_OF_BANANA_SHAPES]; // Thickness tolerance
  G4double fBananaExtraRadiusTolerance[NUMBER_OF_BANANA_SHAPES]; // Extra tolerance in radial position of each block

  G4double fAlSlabThickness[NUMBER_OF_BANANA_SHAPES];   // Thickness of aluminum slabs
  G4int    fAlSlabNVertices[NUMBER_OF_BANANA_SHAPES]; // Number of vertices of aluminum slab
  G4TwoVector fAlSlabVertex[NUMBER_OF_BANANA_SHAPES][N_OF_ALSLAB_VERTICES]; // Vertices of aluminum slab

  G4double fAlColumnLength[NUMBER_OF_BANANA_SHAPES];    // Length of aluminum columns
  G4double fAlColumnDiameter[NUMBER_OF_BANANA_SHAPES];  // Diameter of aluminum columns
  G4int    fBananaNAlColumns[NUMBER_OF_BANANA_SHAPES];  // Number of aluminum columns in banana
  G4TwoVector fAlColumnPosition[NUMBER_OF_BANANA_SHAPES][N_OF_ALCOLUMNS]; // Number of aluminum columns in banana

  G4int    fAlCShapeNVertices[NUMBER_OF_BANANA_SHAPES];    // Number of vertices of C-shape
  G4TwoVector fAlCShapeVertex[NUMBER_OF_BANANA_SHAPES][N_OF_ALCSHAPE_VERTICES]; // Vertices of C-shape
  G4double fAlCShapeHeight[NUMBER_OF_BANANA_SHAPES];       // Height at center of C-shape
  G4double fAlCShapeHoleDiameter[NUMBER_OF_BANANA_SHAPES]; // Diameter of hole at center of C-shape

  // Position on banana of two holes for C-Shape screws (2 C-Shapes per banana, 2 screws per C-Shape)
  G4TwoVector fBananaAlCShapeScrewsPosition[NUMBER_OF_BANANA_SHAPES][2][2];

  // Path to matrices used in the fast simulation
  G4String fLAVEfficiencyMatrix, fLAVTimeMatrix;

};
#endif
