#ifndef LAVGeometry_H
#define LAVGeometry_H 1

#include "iostream"

#include "TMath.h"

#include "TF1.h"
#include "TVector3.h"
#include "LAVDefinitions.hh"
#include "CLHEP/Units/SystemOfUnits.h"

using namespace CLHEP;


class LAVGeometry
{

public:

  static LAVGeometry* GetInstance();

private:

  LAVGeometry();
  ~LAVGeometry();
  static LAVGeometry* fInstance;
 
private:

  void CreateGeometry();

public:

  Int_t GetNumberOfStations()           { return fNofStations; }
				     
  Int_t GetNumberOfLayers(Int_t Station)  { return fNofLayersPerStation[Station]; }
  Int_t GetNumberOfBananas(Int_t Station) { return fNofBananasPerLayer[Station];  }

  Int_t* GetNumberOfLayers()   { return fNofLayersPerStation; }
  Int_t* GetNumberOfBananas()    { return fNofBananasPerLayer ;  }
  
  Double_t GetBinWidth() { return  0.2; }
  
  // N bins
  Int_t GetNbins()         { return 2000; }

  // Quantum efficiency
  TF1* GetQuantumEfficiency() { return fgPhotocathode; }
  
  // Number of dynodes
  Int_t GetNdynodes()  { return 12; }

  // FFT limit
  Int_t FFTlimit() { return 4; }
  
  // Dynode efficiency collection
  Double_t GetFirstDynodeCollectionEfficiency() { return 0.85; }
  Double_t GetDynodeCollectionEfficiency() { return 0.99; }
  //Double_t GetDynodeCollectionEfficiency() { return 1.; }
  
  // Transit time
  //Double_t GetTransitTime()  { return (15./(Double_t)GetNdynodes()); }
  Double_t GetTransitTime()              { return 17.; }                                  // ns
  Double_t GetTransitTime_FirstDynode()  { return (GetTransitTime()/(Double_t)GetNdynodes()); }         
    
  // Transit time spread
//  Double_t GetTransitTimeSpread_FirstDynode() { return 0.5; }    // ns 
//  Double_t GetTransitTimeSpread() { return ( 0.880459 ); }       // ns 
  Double_t GetTransitTimeSpread_FirstDynode() { return 0.6; }    // ns 
  Double_t GetTransitTimeSpread() { return ( 0.8 ); }       // ns 
  Double_t GetTransitTimeSpread_FFT() { return ( GetTransitTimeSpread()*TMath::Sqrt(LAVGeometry::GetNdynodes()-LAVGeometry::FFTlimit()-1) ); }
  
  // Gain
  Double_t GetGain() { return 1.5e6; }
  
  // alpha
  Double_t GetAlpha() { return 0.7; }
  
  // Dynode Gain (1.0e6)
  Double_t GetDynodeGain() { return ( TMath::Power( GetGain() / ( TMath::Power(2., GetAlpha())*TMath::Power( (Double_t)GetDynodeCollectionEfficiency(), (Double_t)( GetNdynodes()-1) )*GetFirstDynodeCollectionEfficiency()), 1/(Double_t)GetNdynodes() ) ); }
  Double_t GetFirstDynodeGain() { return TMath::Power(2., GetAlpha())*GetDynodeGain(); }
  //Double_t GetDynodeGain() { return (2.6);}///LAVGeometry::GetDynodeCollectionEfficiency()); }
  
  // PMT capacitance (nF)
  
  Double_t GetCableLength() { return 6.15; }                // cable lenght in m
  Double_t GetCableCapacitance() { return 96.46e-12; }      // capacitance in F/m
  Double_t GetCableInductance()  { return 2.4e-7; }         // inductance  in H/m
  Double_t GetCableResistance()  { return 0.086; }          // resistance  in Ohm/m
  Double_t GetCableConductance() { return 0.; }             // conductance in Siemens/m
  
  Double_t GetPMTCapacitance() { return 157e-3; } // pF
  
  // PMT impedence (OHM)
  Double_t GetPMTImpedence()    { return 50;}     // Ohm
  Double_t GetAnodeResistence() { return 33.; }   // Ohm

  // Ntau
  Int_t GetNtau() { return 6; }  // original value 6

  // Poisson limit
  Int_t GetBinomialLimit() { return 100; }

  // Leading, trailing th and hysteresi
  Double_t GetHysteresiTimeConstant(){ return 300.; }   // ns

  Double_t GetLeadingThresholdLow()  { return 7.125e-3; }    // volt
  Double_t GetTrailingThresholdLow() { return TMath::Max( (GetLeadingThresholdLow()-GetHysteresiLow()), 0.); }
  Double_t GetHysteresiLow()           { return 1.25e-3; }   // volt
  
  Double_t GetLeadingThresholdHigh()  { return 40.e-3; }    // volt
  Double_t GetTrailingThresholdHigh() { return TMath::Max( (GetLeadingThresholdHigh()-GetHysteresiHigh()), 0.); }
  Double_t GetHysteresiHigh()           { return 1.25e-3; }   // volt

  // Nsigma and N tau in order to define signal range 
  Double_t GetNsigmaTTS()          { return 60.; }
  Double_t GetNtauSignal()         { return 20.; }
  
  // If 1 FFT is performed for each stage if 0 only at the end
  Bool_t FFTisON() { return 0; }

  // normalization to right charge
  Double_t GetChargeNormalization() { return 1/(1-TMath::Exp(-LAVGeometry::GetBinWidth()/(LAVGeometry::GetPMTImpedence()*LAVGeometry::GetPMTCapacitance()))); }

  // 

  Double_t GetLeadingSafetyMargin(){ return 0.2;}   
  Double_t GetRiseTimeSafetyMargin(){ return 15;}

  Double_t GetRiseTime(){return 6.1;}

  void Print();

private:

  Int_t fNofStations; 	   
  Int_t* fNofLayersPerStation;
  Int_t* fNofBananasPerLayer; 
  
  TF1* fgPhotocathode;

  //
  // MC information: will be removed when DB is available
  //

public:
  // Return info about responsibility regions
  Int_t    GetNumberOfResponsibilityRegions()                  { return NUMBER_OF_RESPONSIBILITY_REGIONS; };
  Double_t GetResponsibilityRegionZofFrontFace(const Int_t id) { return fLAV_RR_ZofFrontFace[id]; };
  Double_t GetResponsibilityRegionZofBackFace(const Int_t id)  { return fLAV_RR_ZofBackFace[id]; };
  Double_t GetResponsibilityRegionRadius(const Int_t id)       { return fLAV_RR_Radius[id]; };

  Int_t    GetTotalNumberOfVetoStations() { return NUMBER_OF_VETOES; };

  // Return responsibility region of stations
  Int_t    GetStationResponsibilityRegion(const Int_t id) { return fLAV_Station_ResponsibilityRegion[id]; };

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
  Double_t GetBlockPhi(const Int_t chid);
  TVector3 GetBlockXVersor(const Int_t chid);
  TVector3 GetBlockYVersor(const Int_t chid);
  TVector3 GetBlockZVersor(const Int_t chid);
  TVector3 GetLeadglassCenter(const Int_t chid);

  // Return positions and dimensions of LAV stations
  Double_t GetStationZofFrontFace(const Int_t id) { return fLAV_Station_ZofFrontFace[id]; };
  Double_t GetStationZofBackFace(const Int_t id)  { return fLAV_Station_ZofBackFace[id]; };
  Double_t GetStationZLength(const Int_t id)      { return fLAV_Station_ZLength[id]; };
  Double_t GetStationInnerRadius(const Int_t id)  { return fLAV_Station_InnerRadius[id]; };
  Double_t GetStationOuterRadius(const Int_t id)  { return fLAV_Station_OuterRadius[id]; };

  // Return info about steel vessel structure
  // N.B. vessel global dimensions are those of the station
  Double_t GetVesselOuterRadius(const Int_t id) { return fLAV_Vessel_OuterRadius[id]; };
  Double_t GetVesselInnerRadius(const Int_t id) { return fLAV_Vessel_InnerRadius[id]; };
  Double_t GetVesselThickness(const Int_t id) { return fLAV_Vessel_Thickness[id]; };

  // Return info about position and numbers of bananas in station
  Double_t GetStationFirstRingZPos(const Int_t id)   { return fLAV_Station_FirstRingZPos[id]; };
  Int_t    GetStationNRings(const Int_t id)          { return fLAV_Station_NRings[id]; };
  Int_t    GetStationNBlocksPerRing(const Int_t id)  { return fLAV_Station_NBlocksPerRing[id]; };
  Int_t    GetStationNBananasPerRing(const Int_t id) { return fLAV_Station_NBananasPerRing[id]; };
  Int_t    GetStationBananaType(const Int_t id)      { return fLAV_Station_BananaType[id]; };
  Double_t GetStationRingGap(const Int_t id)         { return fLAV_Station_RingGap[id]; };
  Double_t GetStationPhiReference(const Int_t id)    { return fLAV_Station_PhiReference[id]; };
  Double_t GetStationPhiRotationBetweenLayers(const Int_t id) { return fLAV_Station_PhiRotationBetweenLayers[id]; };

  // Return type of crystal used for given station/layer
  Int_t    GetTypeOfCrystal(const Int_t is,const Int_t il) { return fLAV_TypeOfCrystal[is][il]; };

  // Return geometrical values about single PbGl blocks

  Int_t    GetTotalNumberOfBlockShapes()         { return NUMBER_OF_BLOCK_SHAPES; };

  Int_t    GetBlockOpalId(const Int_t id)        { return fBlockOpalId[id]; };
  Int_t    GetBlockIdFromOpalId(const Int_t);

  Double_t GetBlockZLength(const Int_t id)       { return fBlockZLength[id]; };
  Double_t GetBlockL1Length(const Int_t id)      { return fBlockL1Length[id]; };
  Double_t GetBlockL2Length(const Int_t id)      { return fBlockL2Length[id]; };
  Double_t GetBlockL3Length(const Int_t id)      { return fBlockL3Length[id]; };
  Double_t GetBlockL4Length(const Int_t id)      { return fBlockL4Length[id]; };
  Double_t GetBlockW1Length(const Int_t id)      { return fBlockW1Length[id]; };
  Double_t GetBlockW2Length(const Int_t id)      { return fBlockW2Length[id]; };

  Double_t GetLightGuideZLength(const Int_t id)  { return fLightGuideZLength[id]; };
  Double_t GetLightGuideDiameter(const Int_t id) { return fLightGuideDiameter[id]; };

  Double_t GetCathodeDiameter(const Int_t id)    { return GetLightGuideDiameter(id)*0.95; }
  Double_t GetWrapFrontHole(const Int_t)         { return fWrapFrontHole; }
  Double_t GetWrapThick(const Int_t)             { return fWrapThick; }
  Double_t GetGlueThick(const Int_t)             { return fGlueThick; }
  Double_t GetAirThick(const Int_t)              { return fAirThick; }

  Double_t GetMuMetalZLength(const Int_t id)     { return fMuMetalZLength[id]; };
  Double_t GetMuMetalDiameter(const Int_t id)    { return fMuMetalDiameter[id]; };
  Double_t GetMuMetalThickness(const Int_t id)   { return fMuMetalThickness[id]; };

  Double_t GetSteelSlabThickness(const Int_t id) { return fSteelSlabThickness[id]; };

  // Return geometrical values about bananas

  Int_t    GetTotalNumberOfBananaShapes()   { return NUMBER_OF_BANANA_SHAPES; };

  Int_t    GetBananaNBlocks(const Int_t id)     { return fBananaNBlocks[id]; };
  Double_t GetBananaPhiSpan(const Int_t id)     { return fBananaPhiSpan[id]; };
  Double_t GetBananaOuterRadius(const Int_t id) { return fBananaOuterRadius[id]; };
  Double_t GetBananaInnerRadius(const Int_t id) { return fBananaInnerRadius[id]; };
  Double_t GetBananaThickness(const Int_t id)   { return fBananaThickness[id]; };
  Double_t GetBananaThicknessTolerance(const Int_t id) { return fBananaThicknessTolerance[id]; };

  Int_t    GetAlSlabNVertices(const Int_t id)   { return fAlSlabNVertices[id]; };

  Double_t GetAlSlabThickness(const Int_t id)   { return fAlSlabThickness[id]; };

  Double_t GetAlColumnLength(const Int_t id)    { return fAlColumnLength[id]; };
  Double_t GetAlColumnDiameter(const Int_t id)  { return fAlColumnDiameter[id]; };
  Int_t    GetBananaNAlColumns(const Int_t id)  { return fBananaNAlColumns[id]; };


  Int_t    GetAlCShapeNVertices(const Int_t id)    { return fAlCShapeNVertices[id]; };

  Double_t GetAlCShapeHeight(const Int_t id)       { return fAlCShapeHeight[id]; };
  Double_t GetAlCShapeHoleDiameter(const Int_t id) { return fAlCShapeHoleDiameter[id]; };


private:
  // LAV responsibility regions
  Double_t fLAV_RR_ZofFrontFace[NUMBER_OF_RESPONSIBILITY_REGIONS];
  Double_t fLAV_RR_ZofBackFace[NUMBER_OF_RESPONSIBILITY_REGIONS];
  Double_t fLAV_RR_Radius[NUMBER_OF_RESPONSIBILITY_REGIONS];

  // LAV stations positions and dimensions
  // N.B. positions of front and back face are in the "world" coordinate system
  Int_t fLAV_Station_ResponsibilityRegion[NUMBER_OF_VETOES];
  Double_t fLAV_Station_ZofFrontFace[NUMBER_OF_VETOES];
  Double_t fLAV_Station_ZofBackFace[NUMBER_OF_VETOES];
  Double_t fLAV_Station_ZLength[NUMBER_OF_VETOES];

  // LAV inner and outer radius
  // Inner radius is the radius of the circle ~tangent to the internal face of the PbGl blocks
  Double_t fLAV_Station_InnerRadius[NUMBER_OF_VETOES];
  // Outer radius is (for the moment) the radius at the external surface of the vessel
  Double_t fLAV_Station_OuterRadius[NUMBER_OF_VETOES];

  // Steel vessel information
  Double_t fLAV_Vessel_OuterRadius[NUMBER_OF_VETOES];
  Double_t fLAV_Vessel_InnerRadius[NUMBER_OF_VETOES];
  Double_t fLAV_Vessel_Thickness[NUMBER_OF_VETOES];

  // Number of rings in LAV station
  Int_t fLAV_Station_NRings[NUMBER_OF_VETOES];

  // Position along Z of first ring wrt front face of station
  Double_t fLAV_Station_FirstRingZPos[NUMBER_OF_VETOES];

  // Number of bananas in each ring
  Int_t fLAV_Station_NBananasPerRing[NUMBER_OF_VETOES];
  Int_t fLAV_Station_BananaType[NUMBER_OF_VETOES];

  // Number of blocks in each ring
  Int_t fLAV_Station_NBlocksPerRing[NUMBER_OF_VETOES];

  // Gap between rings
  Double_t fLAV_Station_RingGap[NUMBER_OF_VETOES];

  // Phi reference point for banana 0 of layer 0
  Double_t fLAV_Station_PhiReference[NUMBER_OF_VETOES];

  // Phi rotation between consecutive layers
  Double_t fLAV_Station_PhiRotationBetweenLayers[NUMBER_OF_VETOES];

  // Map OPAL type of crystals used for each ring of each station
  Int_t fLAV_TypeOfCrystal[NUMBER_OF_VETOES][NUMBER_OF_RINGS];

  // Geometrical parameters for PbGl blocks
  Int_t    fBlockOpalId[NUMBER_OF_BLOCK_SHAPES];        // Opal Id of block
  Double_t fBlockZLength[NUMBER_OF_BLOCK_SHAPES];       // Z length (PbGl only)
  Double_t fBlockL1Length[NUMBER_OF_BLOCK_SHAPES];      // L1 length (long base of back face)
  Double_t fBlockL2Length[NUMBER_OF_BLOCK_SHAPES];      // L2 length (short base of back face)
  Double_t fBlockL3Length[NUMBER_OF_BLOCK_SHAPES];      // L3 length (long base of front face)
  Double_t fBlockL4Length[NUMBER_OF_BLOCK_SHAPES];      // L4 length (short base of front face)
  Double_t fBlockW1Length[NUMBER_OF_BLOCK_SHAPES];      // W1 length (height of back face)
  Double_t fBlockW2Length[NUMBER_OF_BLOCK_SHAPES];      // W2 length (height of front face)
  Double_t fLightGuideZLength[NUMBER_OF_BLOCK_SHAPES];  // Light Guide Z length
  Double_t fLightGuideDiameter[NUMBER_OF_BLOCK_SHAPES]; // Light Guide diameter
  Double_t fMuMetalZLength[NUMBER_OF_BLOCK_SHAPES];     // u-metal cylinder Z length
  Double_t fMuMetalDiameter[NUMBER_OF_BLOCK_SHAPES];    // u-metal cylinder external diameter
  Double_t fMuMetalThickness[NUMBER_OF_BLOCK_SHAPES];   // u-metal cylinder thickness
  Double_t fSteelSlabThickness[NUMBER_OF_BLOCK_SHAPES]; // Steel slab thickness
  Double_t fWrapFrontHole;   // diameter of the hole in the small wrapping face
  Double_t fWrapThick;       // wrap thickness
  Double_t fAirThick;        // air thickness between leadglass and wrapping
  Double_t fGlueThick;       // glue thickness betwen leadglass and metal plate

  // Geometrical parameters for bananas
  Int_t    fBananaNBlocks[NUMBER_OF_BANANA_SHAPES];     // Number of blocks in banana (always 4)
  Double_t fBananaPhiSpan[NUMBER_OF_BANANA_SHAPES];     // Angular span of banana
  Double_t fBananaOuterRadius[NUMBER_OF_BANANA_SHAPES]; // External radius of banana (at vessel internal surface)
  Double_t fBananaInnerRadius[NUMBER_OF_BANANA_SHAPES]; // Internal radius of banana (circle tangent to block front face)
  Double_t fBananaThickness[NUMBER_OF_BANANA_SHAPES];   // Thickness of banana at aluminum slab
  Double_t fBananaThicknessTolerance[NUMBER_OF_BANANA_SHAPES]; // Thickness tolerance

  Double_t fAlSlabThickness[NUMBER_OF_BANANA_SHAPES];   // Thickness of aluminum slabs
  Int_t    fAlSlabNVertices[NUMBER_OF_BANANA_SHAPES]; // Number of vertices of aluminum slab

  Double_t fAlColumnLength[NUMBER_OF_BANANA_SHAPES];    // Length of aluminum columns
  Double_t fAlColumnDiameter[NUMBER_OF_BANANA_SHAPES];  // Diameter of aluminum columns
  Int_t    fBananaNAlColumns[NUMBER_OF_BANANA_SHAPES];  // Number of aluminum columns in banana

  Int_t    fAlCShapeNVertices[NUMBER_OF_BANANA_SHAPES];    // Number of vertices of C-shape
  Double_t fAlCShapeHeight[NUMBER_OF_BANANA_SHAPES];       // Height at center of C-shape
  Double_t fAlCShapeHoleDiameter[NUMBER_OF_BANANA_SHAPES]; // Diameter of hole at center of C-shape


};
#endif
