// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-05-05
//
// --------------------------------------------------------------
#include "MUV1Geometry.hh"

MUV1Geometry* MUV1Geometry::fInstance = 0;

MUV1Geometry::MUV1Geometry(){ // All sizes in mm !
  // Insert here all the parameters you need to define the geometry
  // waiting for reading everything from DataBase

  fScintillatorPosition[1] = -1278;
  for (int i = 2; i < 45; i++) {
    if (i < 21 || i > 25) {
      fScintillatorPosition[i] = fScintillatorPosition[i - 1] + 60;
    }
    if (i == 21 || i == 25) {
      fScintillatorPosition[i] = fScintillatorPosition[i - 1] + 57;
    }
    if (i > 21 && i < 25) {
      fScintillatorPosition[i] = fScintillatorPosition[i - 1] + 54;
    }
  }

  // Unused World Parameters for stand-alone configurations
  fWorldZLength = 22.*1000;
  fWorldXLength = 10.*1000;
  fWorldYLength = 10.*1000;

  // Responsibility region (from BEATCH file 29/11/10)
  fMUV1ResponsibilityRegionXLength    =   5.000*1000;
  fMUV1ResponsibilityRegionYLength    =   5.000*1000;


  fMUV1ResponsibilityRegionZBeginning = 243.224*1000;
  fMUV1ResponsibilityRegionZEnd       = 244.385*1000;
  fMUV1DetectorFrontZPosition = 243.418*1000;  // = fMUV1ResponsibilityRegionZBeginning


  /*
  // FOR TESTING!
  fMUV1ResponsibilityRegionZBeginning = 0*1000;
  fMUV1ResponsibilityRegionZEnd       = 1.161*1000;
  fMUV1DetectorFrontZPosition = 0*1000;
  */

  // thickness of each scintillator (zsci)
  fScintillatorThickness  =  1.0*10;
  // thickness of each iron plate (zfe)
  fIronThickness  =  2.5*10;
  // Thickness of the rubber plate
  fRubberThickness=  0.2*10;


  // number of iron planes in the MUV1 (anfe)
  fNIronPlate = 25;
  //Size of iron plates
  fIronPlateSizeX = 2730;
  fIronPlateSizeY = 2630;
  fIronPlateOuterSizeX = 3200;
  fIronPlateOuterSizeY = 3200;
  // number of rubber plates
  fNRubberPlate = 24;
  // Radius for connection holes
  fConnectionHoleRadius = 45;



  // COUNTER VARIABLES

  // Number of Standard Counters in one MUV1 top-plane(nsvv)
  fNMUV1CounterStandard = 17;
  // Number of Middle Counters Standard in one MUV1 quadrant-plane
  fNMUV1CounterMiddleStd = 1;
  // Number of Middle Counters  Outer in one MUV1 Quadrant Plane
  fNMUV1CounterMiddleOuter = 1;
  // Total Number of Middle Counters in one MUV1 quadrant-plane
  fNMUV1CounterMiddle = fNMUV1CounterMiddleStd +fNMUV1CounterMiddleOuter ;
  // Number of Outer Counters in one MUV1 quadrant-plane
  fNMUV1CounterOuter = 3;

  // SCINTILLATOR VARIABLES

  // number of scintillators for each vertical counter in the MUV1 (0.5*strf)
  fNScintVertical = 12;
  // number of scintillators for each horizontal counter in the MUV1 (0.5*strf)
  fNScintHorizontal = 12;

  // Scintillator width standard (xnvv)
  fScintWidthStandard = 6.*10;
  // Scintillator width middle (xmvv)
  fScintWidthMiddle = 5.4*10;
  // Scintillator width outer (Back) (xsvv)
  fScintWidthOuter = 6.*10;

  // Scintillator length standard (ynvv)
  fScintLengthStandard = 261.6*10;
  // Scintillator length middle (ymvv)
  fScintLengthMiddleStd = 120.*10;
  /*
     fScintLengthMiddleOuter = new G4double[fNMUV1CounterMiddleOuter];
     fScintLengthMiddleOuter[0] = 123.6*10;
     */
  fScintLengthMiddleOuter = 123.6*10;


  // Scintillator length outer (Back) (ysvv)
  fScintLengthOuter = new double[fNMUV1CounterOuter];
  fScintLengthOuter[0] = 225.6*10;
  fScintLengthOuter[1] = 237.6*10;
  fScintLengthOuter[2] = 249.6*10;


  //SkinWidth of the wrapping volumes

  fSkinWidth = 0.001*10;

  // FIBER VARIABLES

  // Number of fibers in a scintillator
  fNFiber = 2;
  // Radius of Each Fiber
  fFiberRadius = 0.6;
  // Sepretation between the centers of the two fibers
  fFiberSeperation = 2.9*10;
  // First position of fiber w.r.t to Scintillator
  fFirstFiberXPosition = -1.45*10;
  // Depth of box cut in scintillator
  fCutDepth = 22;
  fCutWidth = 27;
  // height of the cut in the middle outer counter Logical Volume ----->??
  fmOCounterCutHeight = 2.2*10;
  // Length of the outer fibers (between scintillator and PMT)
  fFiberLengthOut = 70*10;
  // Cut of photon energy
  fFiberPhotonEnergyCut = 2 / 1000000. ; // MeV  2 eV


  // Groove


  fGrooveWidth = 1.6;
  fGrooveDepth = 1.6;
  fGroovePositionInStandardScintillator = 1.5*10;

  // PMT

  fPMTSize=TVector3(1.*10,1.*10,1*10);
  // Gap between fibers and PMT
  fPMTAirGap = 0.1;
  // Thickness of glass window of PMT
  fPMTGlassGap = 1;

  // POSITION VARIABLES

  //  overall transverse dimensions (xall,yall)
  fMUV1XSize = 273.0*10;
  fMUV1YSize = 263.0*10;

  // MUV1 length
  fMUV1Length = (fNScintVertical + fNScintHorizontal)*fScintillatorThickness +
    fNIronPlate*fIronThickness + fNRubberPlate*fRubberThickness;

  // MUV1 position in z
  // fMUV1ZPosition = 0*10; // set to zero for testing, so you can rotate it properly
  fMUV1ZPosition = fMUV1DetectorFrontZPosition + 0.5 * fMUV1Length;  // beatch file

  // MUV1 hole diameter (dchol)
  fHoleDiameter = 21.6*10;

  // Scintillator Spacer
  fScintillatorSpacerSize = TVector3((216./2), (108./2),(10./2));
  fCutBoxOuterSize = TVector3((108./2),(14./2),(10./2));
  fCutBoxMiddleOuterSize = TVector3((144./2),(13./2),(10./2));
  fCutBoxMiddleInnerSize = TVector3((162./2),(9./2),(10./2));
  fCutBoxInnerSize = TVector3((188./2),(18./2),(10./2));
  fInnerRadiusSpacer = 105;

  //Bolt
  fBoltPositionX = 1280;
  fBoltPositionY = 1250;

  // beam pipe inner and outer radius

  fInnerBeamPipeRadius = 9.25*10;
  fOuterBeamPipeRadius = 9.85*10;

  fLongitudinalLengthBeamPipe =  fMUV1ResponsibilityRegionZEnd - fMUV1ResponsibilityRegionZBeginning;


  //	  fResponsibilityRegion.push_back(new ResponsibilityRegion(fMUV1ResponsibilityRegionZBeginning,
  //								   fMUV1ResponsibilityRegionZEnd));



  CreateGeometry();
}

MUV1Geometry * MUV1Geometry::GetInstance(){
  if ( fInstance == 0 ) { fInstance = new MUV1Geometry(); }
  return fInstance;
}

void MUV1Geometry::CreateGeometry(){
  // Reproduce the geometry as it is defined in the MC
  // to be able to access it by the reconstruction class
}


int MUV1Geometry :: GetScintillatorAt (double X){
    
    double HalfWidth = 0.;
    double Xabs = TMath::Abs(X);
    
    if (Xabs>fScintillatorPosition[44] + 30.) return -1; // X is out of the range
    
    for (int i=1; i<=44; i++) {
        
        if (i<21 || i > 24) HalfWidth = 30.; // large strips
        else HalfWidth = 27.; // narrow strips
        
        if (TMath::Abs(X - fScintillatorPosition[i]) <= HalfWidth) return i;
        
    }
    
    return -1;
}
