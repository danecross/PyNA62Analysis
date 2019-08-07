//
// ********************************************************************
// * License and Disclaimer                                           *
// *                                                                  *
// * The  Geant4 software  is  copyright of the Copyright Holders  of *
// * the Geant4 Collaboration.  It is provided  under  the terms  and *
// * conditions of the Geant4 Software License,  included in the file *
// * LICENSE and available at  http://cern.ch/geant4/license .  These *
// * include a list of copyright holders.                             *
// *                                                                  *
// * Neither the authors of this software system, nor their employing *
// * institutes,nor the agencies providing financial support for this *
// * work  make  any representation or  warranty, express or implied, *
// * regarding  this  software system or assume any liability for its *
// * use.  Please see the license in the file  LICENSE  and URL above *
// * for the full disclaimer and the limitation of liability.         *
// *                                                                  *
// * This  code  implementation is the result of  the  scientific and *
// * technical work of the GEANT4 collaboration.                      *
// * By using,  copying,  modifying or  distributing the software (or *
// * any work based  on the software)  you  agree  to acknowledge its *
// * use  in  resulting  scientific  publications,  and indicate your *
// * acceptance of all terms of the Geant4 Software license.          *
// ********************************************************************
//
// --------------------------------------------------------------
// History:
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//            Evelina Marinova(Evelina.Marinova@cern.ch)  
//
// --------------------------------------------------------------
#include "TVector.h"

#include "LKrGeometryParameters.hh"
#include "DetectorParameter.hh"
#include "G4ThreeVector.hh"

/// \class LKrGeometryParameters 
/// \Brief
/// LKrGeometryParameters class.
/// \EndBrief   
///
/// \Detailed
/// All dimensions, positions and related to the LKr data are stored here.
/// \EndDetailed


LKrGeometryParameters* LKrGeometryParameters::fInstance = 0;

LKrGeometryParameters::LKrGeometryParameters() : NA62VGeometryParameters(G4String("LKr")) {
  // Define all the geometrical parameters and build the
  // responsibility region accordingly

  // Unused World Parameters for stand-alone configurations

  fWorldZLength = 22.*m;
  fWorldXLength = 10.*m;
  fWorldYLength = 10.*m;

  //use 4 of the reference points to get the length of the cones connecting the warm windows to the outer vessel, and the length of the outer vessel itself

  // front
  // responsible region starts at 240390 mm and ends at 243224 mm. !!!!! do not go out of these limits!!!!
  // NEW : 2 mm shift towards the beam target in the LKr position  ~ 15.02.2011 according to the new BEATCH file measurements.
  //  ==> shift ALL BEATCH numbers by 2 mm, independently of the numbers in the BEATCH file, 
  // only the cryostat measurements matter.

  fStartLKrResponsibleRegion = 240390* mm - 2*mm;
  fEndLKrResponsibleRegion = 243224 * mm - 2*mm; 

  //point between the warm window and the cone

  fBEATCHPositionFrontWarmWindowEdge = 240408 * mm - 2*mm; /// final, BEATCH


  //point at the start of the first outer warm flange

  fBEATCHPositionOuterWarmFlange1 = 240635 - 2*mm;

  // point between the cone and the outer vessel

  fBEATCHPositionFrontEdgeOuterVessel = 240745 * mm - 2*mm;

  // back

  //point between the outer vessel and the cone

  fBEATCHPositionBackEdgeOuterVessel = 242869 * mm - 2*mm;

  //point between the warm window and the cone

  fBEATCHPositionBackWarmWindowEdge = 243206 * mm - 2*mm;  /// final, BEATCH

  fDistanceWWtoValve = 18 * mm; // distance from the most outer warm window flange to the guilotine valve (which is also at the end of the responsible region);    

  // center of the LKr detector (Z position)
  fLKrDetectorZPosition = 241807 *mm - 2*mm;

  // fLKrDetectorZPosition=0.5*(240.757+242.857)*m; // initial by Antonino
  // adjusted according to the BEATCH file according to the Cryostat drawing . Up to date?
  fLKrDetectorZLength =  fBEATCHPositionBackWarmWindowEdge - fBEATCHPositionFrontWarmWindowEdge + 2*fDistanceWWtoValve; 

  fLKrDetectorXLength = 5.*m;
  fLKrDetectorYLength = 5.*m;

  fResponsibilityRegion.push_back(new ResponsibilityRegion(fLKrDetectorZPosition-0.5*fLKrDetectorZLength,
        fLKrDetectorZPosition+0.5*fLKrDetectorZLength)
      );

  // Define Spacer plate holes size

  //thermal contraction of the stesalit


  fThermalContractionConstantG10 =  0.9983; //apply to hole sizes and cell sizes. The longitudinal contractions depends on the stainless steel contracton.

  fSizeHoleX = 0.5 *cm * fThermalContractionConstantG10;
  fSizeHoleY = 1.85 *cm * fThermalContractionConstantG10;

  //irregular cell
  fSizeHoleYHalfCell = 0.55 *cm * fThermalContractionConstantG10;

  // sizes from Giullaume in the old simulation

  //fSizeHoleX = 0.509133 *cm;
  //fSizeHoleY = 1.86682 *cm;
  //fSizeHoleYHalfCell = 0.559048 *cm;

  // Start building the walls and the spacer plates
  // Structure: one vertical block; 2 horizontal blocks

  // Thickness of the walls
  // Front wall thickness == Spacer plate thickness  --->correct it later

  fHalfSizeFrontWallSegmentZ = 0.5 * 0.5 *cm;
  fHalfSizeSpacerWallSegmentZ = 0.5 * 0.5 *cm;
  fHalfSizeBackWallSegmentZ = 0.5 * 0.5 *cm;

  // Half size in Y of the Vertical segment

  fHalfSizeWallVSegmentY = 0.5 * (fSizeHoleY);
  fHalfSizeIrregularWallVSegmentY = 0.5 * (fSizeHoleYHalfCell);

  // Define the space between the spacer plates

  SpaceToNextPlate = 21.25 *cm; // 20.75 space between two plates + 0.5 thickness of one plate -- space between the enters of two plates

  //half lengths in x,y,z of the normal zigzags

  fZigHalfSizeX = 0.004 / 2.*cm;
  fZigHalfSizeY = 1.8 / 2.*cm;
  fZigHalfSizeZ = (SpaceToNextPlate - 2 * fHalfSizeBackWallSegmentZ)  / 2;

  //half lengths in x,y,z of the half zigzags - irregular electrodes.

  fIrrZigHalfSizeX = 0.005/2.*cm;
  fIrrZigHalfSizeY = 0.5/2.*cm;
  fIrrZigHalfSizeZ = (SpaceToNextPlate  - 2 * fHalfSizeBackWallSegmentZ) /2;

  //Max num of  Electrodes in one half of the LKr (in a full line)

  fHalfNElectrodesX = 127; //non-symmetric
  fHalfNElectrodesY = 64;  //symmetric

  //Number of rows in the octagons which have the maximum number of electrodes
  fNRowsFullNumberElectrodes = 27;


  //***************************************
  //            NHOD
  //

  //    add a thin layer of G10 and a thin layer of Scintillator BEFORE the second spacer plate , BEFORE the vertical wall pieces
  //(in reallity - a tube of G10, inner d =7mm, outer d=8mm, full of 20 scintillating fibres with d = 1mm.All is dipped in lKr
  //In the simulation, two planes, about 0.5 mm (the size of the second spacer plane) wide in X, the depth in Z is effective so the surface of the planes is approx equal to the surface of the tube and the fibres.).
  //

  fNHODHalfLenghtG10Z = 0.5 * mm;
  fNHODHalfLenghtScintillatorZ = 1.85 * mm;

  //Radius of a Hole to be cut for accomodatng the beam pipe
  fRHoleX = 10.*cm;
  fRHoleY = 5.*cm;

  //Inner cryostat

  fThicknessOfInnerCryo = 0.5 * cm; //taken from cryostat technical drawing ?? old ?

  // taken from front cold window drawing 
  // to a first approximation the inner cryostat is a cilinder under the lower part of the outer flanges. 
  // In real life, the inner cryostat has some shape, moreover there's the entrance of the tube via which the lkr is exchanged.

  fInnerCryostatMinRadius = 3008 * mm  / 2 - fThicknessOfInnerCryo; 

  fLongitudinalLengthInnerCryo = 153.0 * cm; //rough estimation from cryostat assembly near beam axis (1820 - 131 front -57 to the back of flange1 - 144 to the back of the cold window (taken from cold rear window))


  //Outer cryostat 

  fVerticalRadiusWarmWindow = 2658 /2 *mm;

  fThicknessOfOuterCryo = 1 * cm; // taken from cryostat technical drawing
  fOuterCryostatMinRadius = 3474/2 *mm - fThicknessOfOuterCryo; // taken from cryostat technical drawing 

  fLongitudinalLengthOuterCryo = fBEATCHPositionBackEdgeOuterVessel - fBEATCHPositionFrontEdgeOuterVessel;  //taken from cryostat technical drawing

  //distance between the convex of the front warm window and the convex of the back warm window
  fInnerDistanceFrontBackWarmWindow = 233 * cm;//taken from cryostat technical drawing (1852 + 258 front +220 back)

  //distances between the convex of the warm windows to the LKr Volume 
  fDistanceFrontWarmWindowLKr = 39 * cm;  //rough value from the "cryostat assembly around the beam pipe"

  // this value is not used -- clean!
  fDistanceBackWarmWindowLKr = 28 * cm;

  // beam pipe inner and outer radius

  fInnerBeamPipeRadius = 8.0 * cm;
  fOuterBeamPipeRadius = 8.3 * cm;

  //radius of the hole in each spacer plate around the beam pipe
  fRadiusHoleSpacerPlate = 8.5 * cm;
  //   fLongitudinalLengthBeamPipe = fLongitudinalLengthOuterCryo + (fBEATCHPositionFrontEdgeOuterVessel - fBEATCHPositionFrontWarmWindowEdge) 
  //  + ( fBEATCHPositionBackWarmWindowEdge -  fBEATCHPositionBackEdgeOuterVessel);

  fLongitudinalLengthBeamPipe =  fEndLKrResponsibleRegion -  fStartLKrResponsibleRegion;

  //  thickness of the thin stainless steel foil of the cold plate, positioned right behind the honey comb
  fThicknessSteelFoilColdWindow = 0.5*mm;

  //radius (vertical) of the thin stainless steel foil of the cold plate, positioned right behind the honey comb
  fRadiusSteelFoilColdWindow = (1465) * mm;

  // thickness of the slice of the front cold window
  fThicknessSliceColdWindow = 268.5 * mm;

  //cold window
  fRadiusFrontColdWindow = fRadiusSteelFoilColdWindow/ (sin(3.14 - 2*atan(fRadiusSteelFoilColdWindow / fThicknessSliceColdWindow) ));

  // radius around the beam pipe

  fRadiusFrontColdWindowBeamPipe =  286 *mm /2;

  // thickness of the slice of the front cold window
  fThicknessSliceBackColdWindow = 200 * mm;

  fRadiusBackColdWindow = (fInnerCryostatMinRadius + fThicknessOfInnerCryo) / (sin(3.14 - 2*atan(  (fInnerCryostatMinRadius + fThicknessOfInnerCryo)/ fThicknessSliceBackColdWindow) ));

  fThicknesConvexColdWindow = 3 * mm;

  //thickness of cold back window, taken from "cold rear window drawing"
  fThicknesConvexColdBackWindow = 6 * mm;

  //warm window
  fRadiusWarmWindow = 387.5 *cm; 
  fThicknesConvexWarmWindow = 4 * mm; 

  //length of the distance between the electrode structure and the thin stainless steel foil of the cold plate, positioned right behind the honey comb

  fLengthOfColdWindowPlate =  0.6 * mm; //info by Sandro Palestini
  // the front plate would deform so additional lense-like material has been added to the front, the steps are in 0.33 mm
  //the minimum is 2.7 mm at the edges , and it reaches 5 mm in the centre. Let's add the minimum

  fMinimumAddedMaterialFrontPlate = 2.7 *mm; //info by Sandro Palestini 

  //There's a minimum amount of passive LKr in the holes where the electrodes are plugged in the front wall.
  //effective thickness is 0.25 mm (info by Sandro Palestini). Put it between the active cells and the front octagon.

  fPassiveLKrInsideOctagon = 0.25 * mm;

  //***************  Flanges

  // Distance from the center of the cold front window to the start of the monotube
  // the center of the cold front window + filler is not very even, the monotube starts almost in the middle
  // see drawings: "cryostat assembly near beam axis PPE 005N0173" + "Final Monotube 005N02181" + "cold front window with filler 005N02590" 
  // all flanges needed, the ones around the beam pipe fall in the SAC acceptance

  //
  //flanges at cold front window
  //

  fDistanceColdWindowToMonotube = 130 * mm;

  //1. tooth flange

  //distance from the start of the monotube to the tooth flange //the distance is to the center

  fDistanceMonotubeToothFlange = 112 * mm;
  // its height

  fHightToothFlange=7 * mm;

  //its length 
  fLengthToothFlange = 4 * mm;

  //2. bellow tooth

  //distance from the start of the monotube to the tooth flange //the distance is to the center

  fDistanceMonotubeBellowToothFlange = 64.8 * mm;
  // its hight

  fHightBellowToothFlange = 4.5 * mm;

  //it's length 
  fLengthBellowToothFlange = 3.5 * mm;

  //distance from  the bellow tooth flange at the back to the rear cold window (very rough)  //the distance is to the center

  fDistanceBellowToothFlangeAtBackToRareWindow = fDistanceMonotubeBellowToothFlange;

  // four parts of the flange at the cold front window around the beam pipe

  //part 1

  fHightFlange1AtColdWindowBeam = 60 * mm;//24 * mm;
  fLengthFlange1AtColdWindowBeam = 40 * mm; //actually it's ~45 but there's a curve so I reduced it effectively

  fMinRadiusFlange1AtColdWindowBeam = fOuterBeamPipeRadius; // + 36 * mm;

  // part 1 contains 12 bolts sinking in, 12 bolts with heads out ==> make them daughter volumes of the flange 1

  // holes before the bolts on cold front window // make them out of vacuum and cut them out from flange 1

  fRadiusHolesBoltsFrontCold = 10 * mm;
  fLengthHolesBoltsFrontCold = 42 * mm;

  fAngleCentralAxisBolt = 6.3 * deg; // check rear cold window drawing

  // heads before the bolts on cold front window (stainless steel)

  fRadiusHeadsBoltsFrontCold = 9 * mm;
  fLengthHeadsBoltsFrontCold = 8 * mm;

  // distance between the center of the bolts to the outer beam pipe radius 
  fDistanceBoltToBeamPipe = 12 * mm;

  //part 2

  fHightFlange2AtColdWindowBeam = 60 * mm;
  fLengthFlange2AtColdWindowBeam = 50 * mm;

  //part 3

  fHightFlange3AtColdWindowBeam = 50 * mm;
  fLengthFlange3AtColdWindowBeam = 37 * mm;

  //part 4

  fHightFlange4AtColdWindowBeam = 7 * mm;
  fLengthFlange4AtColdWindowBeam = 130 * mm;
  fMinRadiusFlange4AtColdWindowBeam = fOuterBeamPipeRadius + 43 * mm;

  // outer flange at the cold window, separated in 2 parts

  // part 1

  fHightFlange1AtColdWindowOut = 160 * mm;
  fLengthFlange1AtColdWindowOut = 67 * mm;
  fMinRadiusFlange1AtColdWindowOut = 1433 * mm;

  // part 2

  fHightFlange2AtColdWindowOut = 89 * mm;
  fLengthFlange2AtColdWindowOut = 83 * mm;
  fMinRadiusFlange2AtColdWindowOut = 1504 * mm;

  //distance from the face of part2 outerflange to the stainless steel foil

  fDistanceFlange2AtColdWindowOutFaceToSteelFoil = 19 * mm;

  //
  // flanges at cold back window
  //

  //around the beam pipe. The flange is split in 5 parts. Look at "cold rear window PPE DC PD" for details 

  //(a piece of part 4 and the 5th part is visible only on "cryostat assembly around the beam pipe PPE 005N 0173 0")

  //part 1

  fHightBackFlange1AtColdWindowBeam = 73 *mm;
  fLengthBackFlange1AtColdWindowBeam = 15 * mm;
  fMinRadiusBackFlange1AtColdWindowBeam =  120 * mm;

  //part 2

  fHightBackFlange2AtColdWindowBeam = 27 *mm;
  fLengthBackFlange2AtColdWindowBeam = 37 * mm;
  fMinRadiusBackFlange2AtColdWindowBeam =  120 * mm;

  //part 3

  fHightBackFlange3AtColdWindowBeam = 27 *mm;
  fLengthBackFlange3AtColdWindowBeam = 86 * mm + fThicknesConvexColdBackWindow;
  fMinRadiusBackFlange3AtColdWindowBeam =  120 * mm;

  //part 4

  fHightBackFlange4AtColdWindowBeam = 37 *mm;
  fLengthBackFlange4AtColdWindowBeam = 63 * mm;
  fMinRadiusBackFlange4AtColdWindowBeam =  fOuterBeamPipeRadius;

  // part #4 and part #3 back edges have a shift of 10 mm. used for the positioning.part 4 sticks out at the back.

  fZDistanceBetweeBackEdges3and4 = 13 *mm;

  //part 5

  fHightBackFlange5AtColdWindowBeam = 25 *mm;
  fLengthBackFlange5AtColdWindowBeam = 21 * mm; 
  fMinRadiusBackFlange5AtColdWindowBeam =  fOuterBeamPipeRadius;

  // bolts at the back .......
  // part 1 contains 12 bolts sinking in, 12 bolts with heads out
  // holes before the bolts on cold rear window // make them out of vacuum and cut them out from flange 4 and 5

  fLengthHolesBoltsBackColdInFlange5 = 21 * mm;
  fLengthHolesBoltsBackColdInFlange4 = 16 * mm;

  //outer flange. 1 very rough piece 

  fHightBackFlangeAtColdWindowOut = 132.5 *mm;
  fLengthBackFlangeAtColdWindowOut = 165 * mm;
  fMinRadiusBackFlangeAtColdWindowOut =   fInnerCryostatMinRadius + fThicknessOfInnerCryo;  //2977 *mm / 2;

  // the distance between the face of the outer flange at the rear cold window to the rear cold window inner face

  fDistanceFaceBackFlangeOutToColdWindow = 90 *mm;

  //
  //flanges at warm window. they are mirrored for front and back window
  // 

  //
  //flange around the beam pipe is separated in 5 pieces
  //see cryostat assembly near beam pipe
  //

  // part 1

  fHightFlangeAtWarmWindow1Beam = 59 *mm;
  fLengthFlangeAtWarmWindow1Beam = 20 * mm;
  fMinRadiusFlangeAtWarmWindow1Beam = fOuterBeamPipeRadius;

  // part 2

  fHightFlangeAtWarmWindow2Beam = 29.5 *mm;
  fLengthFlangeAtWarmWindow2Beam = 20 * mm;
  fMinRadiusFlangeAtWarmWindow2Beam = 227 / 2 * mm;

  // part 3

  fHightFlangeAtWarmWindow3Beam = 20 *mm;
  fLengthFlangeAtWarmWindow3Beam = 106 * mm;
  fMinRadiusFlangeAtWarmWindow3Beam = 227 / 2 * mm - fHightFlangeAtWarmWindow3Beam;

  // elliptical cut in part 3 of the warm window flange around the beam pipe; half axis length.

  fElipticalCutFlange3LongAxis = 20*mm;
  fElipticalCutFlange3ShortAxis = 10*mm;

  //distance from tha back face of flange 2 to the frong face of front 3 (enough to fit a bolt in the gap)

  fDistanceWarmWindowFlange2EndFlange3Front = 10* mm;
  // part 4

  fHightFlangeAtWarmWindow4Beam = (260 - 227) /2  *mm;
  fLengthFlangeAtWarmWindow4Beam = 101 * mm - 31 * mm;
  fMinRadiusFlangeAtWarmWindow4Beam = 227 / 2 * mm;

  //distance between the back face of front warm window back face
  //respectively from the face front of the flange4 in the back to the front face of the back warm window

  fDistanceWarmWindowFlange4 = 43 * mm;
  fDistanceWarmWindowFlange3 = 58 * mm;
  // part 5

  fHightFlangeAtWarmWindow5Beam = 12 *mm + fHightFlangeAtWarmWindow4Beam;
  fLengthFlangeAtWarmWindow5Beam = 31 * mm;
  fMinRadiusFlangeAtWarmWindow5Beam = 227 /2 * mm;

  //
  // Outer flanges
  //

  //at the end of the outer vessel, upper part of the cone connecting outer vessel to the warm window

  //part 1

  fHightFlangeAtWarmWindow1Out = 132 *mm;
  fLengthFlangeAtWarmWindow1Out = 120 * mm;
  fMinRadiusFlangeAtWarmWindow1Out = 3640 / 2 - fHightFlangeAtWarmWindow1Out;

  // part 2 - at the lower edge ot the cone, at the upper edge of the warm window

  fHightFlangeAtWarmWindow2Out = 100 *mm;
  fLengthFlangeAtWarmWindow2Out = 120 * mm;
  fMinRadiusFlangeAtWarmWindow2Out = 2658 /2 * mm;

  // warm window bolts

  // upper bolts

  // angle between the center of the bolt and the central axis

  fAngleCentralAxisBoltUpperWarm = 0 * deg;
  fRadiusHolesBoltsUpperWarm = 5 *mm;
  fLengthHolesBoltsUpperWarmInFlange5 = 25 * mm;

  // distance to the outer R of the beam pipe
  fDistanceUpperWarmBoltToBeamPipe = 47 *mm;

  fRadiusHeadsBoltsUpperWarm = 7 *mm;
  fLengthHeadsBoltsUpperWarm = 11 * mm;

  // lower bolts

  // angle between the center of the bolt and the central axis

  fAngleCentralAxisBoltLowerWarm = 15 * deg;
  fRadiusHolesBoltsLowerWarm = 4 *mm;
  fLengthHolesBoltsLowerWarm = 101 * mm;

  // distance to the center of flange 3

  fDistanceLowerWarmBoltToFlange3 = 0 *mm;

  fRadiusHeadsBoltsLowerWarm = 7 *mm;
  fLengthHeadsBoltsLowerWarm = 8 * mm;

  //
  //front plate, back plate
  //

  // add an octagon made of epoxy fiber glass in front of the Electrode cells to compensate the thicker front plate. 
  // No holes are needed.Subtract 0.5 cm which are already included in the Electrode cells (total front plate from epoxy 5.0 * cm)

  fHalfLengthOfFrontPlateOctagon = (5.0 * cm - 0.5 * cm + fMinimumAddedMaterialFrontPlate - fPassiveLKrInsideOctagon)/2; //taken from technical drawing

  //add an octagon made of epoxy fiber glass at the back of the Electrode cells to compensate the thicker back plate. 
  // No holes are needed.Subtract 0.5 cm which are already included in the Electrode cells (total back plate from epoxy 6.4 * cm)

  fHalfLengthOfBackPlateOctagon = (6.4 * cm - 0.5 * cm)/2; //taken from technical drawing

  //*******************************
  //additional amount of material 
  //behind the LKr back plate
  //
  fElectronicsBackG10HalfLength = 73.6 / 2 * mm;
  fElectronicsBackCuHalfLength = 2.2 * mm;
  fElectronicsBackBrassHalfLength = 1 * mm;
  fElectronicsBackTeflonHalfLength = 6 * mm;

  // Front and Back wall between which the zig zag electrodes are
  // positioned

  // front surface of the front plate

  fDistanceFrontPlateBackPlate = 128 *cm;

  fFrontWallPositionZ = - fDistanceFrontPlateBackPlate / 2.;
  fBackWallPositionZ = fFrontWallPositionZ + fDistanceFrontPlateBackPlate;

  // Define longitudinal size of the generic trapezoid

  fHalfZWidth = (fBackWallPositionZ - fFrontWallPositionZ) * 0.5;

  // Define position of the centre of the generic trapezoid

  fZtr = (fBackWallPositionZ + fFrontWallPositionZ) * 0.5;

  // LKr Volume 
  fLongitudinalLengthLKrVolume = fDistanceFrontPlateBackPlate + 2 * fHalfLengthOfFrontPlateOctagon 
    + 2 * fHalfLengthOfBackPlateOctagon + fLengthOfColdWindowPlate;     //138.2 * cm + + 0.6 mm + ...; //according technical drawing 

  // Longitudinal Position of the very front of the Front wall segment
  // wrt the center of the generic trapezoid

  fPositionOfWallZ0 = -fHalfZWidth + fHalfSizeFrontWallSegmentZ;

  // Define a reference electrode in the center; define X and Y coordinates at the Back wall
  fXbackReferenceCell = 1.*cm * fThermalContractionConstantG10; //middle of central trapezoid
  fYbackReferenceCell = 1.*cm * fThermalContractionConstantG10;
  //gap between the two halves of the LKr
  fHalfSizeCentralGap = 0.15*mm;   //maybe in reality it is only 0.1 mm

  //define physical center of the LKr, used as projectivity center: coincides with the center of the zigzag for the central cathod.

  fXbackProjectivityReferenceCell = 1. *cm * fThermalContractionConstantG10; 
  fYbackProjectivityReferenceCell = 1. *cm * fThermalContractionConstantG10;

  // Define projectivity point position along Z (X=0,Y=0 wrt beam line) -- only up to the front plate!!!

  fProjectivityPointPositionZ  = fFrontWallPositionZ - (110 *m - fHalfSizeFrontWallSegmentZ); 
  // The thickness of front wall as defined in this parametrisation -- fHalfSizeFrontWallSegmentZ
  // -- is subtracted from 110 m because according to the technical drawing the projectivity point is at the back of the front plate.

  // Distance between Back plate of LKr to projectivity point - define projectivity axis
  // the mother cell of the reference cell;

  fProjectivityAxisProjectionZ = fBackWallPositionZ - fProjectivityPointPositionZ;

  // Calculate the X and Y position of the reference electrode at Front Wall (along the
  // Projectivity axis)

  fXfrontReferenceCell = (fFrontWallPositionZ - fProjectivityPointPositionZ) * 
    fXbackReferenceCell / fProjectivityAxisProjectionZ;

  //G4cout << " fXfrontReferenceCell " << fXfrontReferenceCell <<G4endl;

  fYfrontReferenceCell = (fFrontWallPositionZ - fProjectivityPointPositionZ) * 
    fYbackReferenceCell / fProjectivityAxisProjectionZ;

  //---------------half cell sizes-------

  // Set parameter of increase for successive spacer plates
  // 0.19% cite NA48 detector paper

  fIncr = 1.+SpaceToNextPlate/(fFrontWallPositionZ-fProjectivityPointPositionZ); // = 1.0019318620878

  // Set size of a half cell at the Front wall
  fHalfCellSizeAtFrontWall = 0.98859 *cm * fThermalContractionConstantG10;

  //double space from center of the trapezoid to the position of the electrode
  // 0.5 is the maximal width of the vertical bar (at the back plate) "fZigHalfSizeX"  allowance 
  //so you can account for the angles od the zigzags and avoid overlapping

  fSpaceFromWallToElectrodeAtWallX = fHalfCellSizeAtFrontWall -  5 * fZigHalfSizeX;

  // Calculate the distance between the centers of two mother cells containing one electrode
  // each - in X,Y plane, at Front and Back Wall.

  fDistanceToNextElectrodeFrontX  = fHalfCellSizeAtFrontWall;
  fDistanceToNextElectrodeBackX = fHalfCellSizeAtFrontWall * pow(fIncr, 6);
  fDistanceToNextElectrodeFrontY  = 2 * fDistanceToNextElectrodeFrontX;
  fDistanceToNextElectrodeBackY = 2 * fDistanceToNextElectrodeBackX;

  // Define size of the section of the generic trapezoid at Front wall and at Back wall 
  fHalfXWidthF  =  fDistanceToNextElectrodeFrontX * 0.5;
  fHalfYWidthF  =  fDistanceToNextElectrodeFrontY * 0.5;
  fHalfXWidthB  =  fDistanceToNextElectrodeBackX * 0.5;
  fHalfYWidthB  =  fDistanceToNextElectrodeBackY * 0.5;

  // irregular cells - size at back wall leave same tollerance like for a regular cell (1.85 hole + 0.15 wall) 
  //    --> irregular cell (0.55 hole + 0.15 wall) at the back plate

  fHalfIrregularCellSizeAtBackWallY = 0.7 *cm;
  fHalfIrregularCellSizeAtFrontWallY = fHalfIrregularCellSizeAtBackWallY * pow(fIncr, -6);

  fHalfYWidthIrregularB  = fHalfIrregularCellSizeAtBackWallY * 0.5;
  fHalfYWidthIrregularF  = fHalfIrregularCellSizeAtFrontWallY * 0.5;

  //define positions of the Stainless steel bars supporting the octagon

  fRadiusSteelBars = 4.3/2 *cm;

  fPositionSteelBars[0] = G4ThreeVector(131.5 *cm,5.5 *cm,0);
  fPositionSteelBars[1] = G4ThreeVector(131.5 *cm,- 5.5 *cm,0);
  fPositionSteelBars[2] = G4ThreeVector(131.5 *cm,21.5 *cm,0);
  fPositionSteelBars[3] = G4ThreeVector(131.5 *cm,-21.5 *cm,0);
  fPositionSteelBars[4] = G4ThreeVector(131.5 *cm,37.5 *cm,0);
  fPositionSteelBars[5] = G4ThreeVector(131.5 *cm,-37.5 *cm,0);
  fPositionSteelBars[6] = G4ThreeVector(-131.5 *cm,5.5 *cm,0);
  fPositionSteelBars[7] = G4ThreeVector(-131.5 *cm,-5.5 *cm,0);
  fPositionSteelBars[8] = G4ThreeVector(-131.5 *cm,21.5 *cm,0);
  fPositionSteelBars[9] = G4ThreeVector(-131.5 *cm,-21.5 *cm,0);
  fPositionSteelBars[10] = G4ThreeVector(-131.5 *cm,37.5 *cm,0);
  fPositionSteelBars[11] = G4ThreeVector(-131.5 *cm,-37.5 *cm,0);
  fPositionSteelBars[12] = G4ThreeVector(93.6 *cm,92.7 *cm,0);
  fPositionSteelBars[13] = G4ThreeVector(93.6 *cm,-92.7 *cm,0);
  fPositionSteelBars[14] = G4ThreeVector(-93.6 *cm,92.7 *cm,0);
  fPositionSteelBars[15] = G4ThreeVector(-93.6 *cm,-92.7 *cm,0);
  fPositionSteelBars[16] = G4ThreeVector(68.9 *cm,117.4 *cm,0);
  fPositionSteelBars[17] = G4ThreeVector(68.9 *cm,-117.4 *cm,0);
  fPositionSteelBars[18] = G4ThreeVector(-68.9 *cm,117.4 *cm,0);
  fPositionSteelBars[19] = G4ThreeVector(-68.9 *cm,-117.4 *cm,0);
  fPositionSteelBars[20] = G4ThreeVector(35.0 *cm,131.5 *cm,0);
  fPositionSteelBars[21] = G4ThreeVector(35.0 *cm,-131.5 *cm,0);
  fPositionSteelBars[22] = G4ThreeVector(-35.0 *cm,131.5 *cm,0);
  fPositionSteelBars[23] = G4ThreeVector(-35. *cm,-131.5 *cm,0);
  fPositionSteelBars[24] = G4ThreeVector(0. *cm,131.5 *cm,0);
  fPositionSteelBars[25] = G4ThreeVector(0. *cm,-131.5 *cm,0);
  fPositionSteelBars[26] = G4ThreeVector(118.3 *cm,68. *cm,0);
  fPositionSteelBars[27] = G4ThreeVector(118.3 *cm,-68. *cm,0);
  fPositionSteelBars[28] = G4ThreeVector(-118.3 *cm,68. *cm,0);
  fPositionSteelBars[29] = G4ThreeVector(-118.3 *cm,-68. *cm,0);

  // GeV to current files: to be read form the macro file
  fGeVtoCurrent1 = "LKrGeVtoCurr1.dat";
  fGeVtoCurrent2 = "LKrGeVtoCurr2.dat";
  fGeVtoCurrent3 = "LKrGeVtoCurr3.dat";

  fLKrCellLength = 2 * fHalfCellSizeAtFrontWall;

  // 16*8 = 128 but we have 127 LKr Cells. The first cathod index is 125. 
  // The mismatch between the position of electrode cell 125 and LKr cell 0 is exactly the length of one LKr cell at the front of LKr. 
  // So, we shift the position of the top right corner. Top right corner has a negative X!!!!
  // TOP RIGHT CORNER OF THE TECHNICAL DRAWING -->corresponds to negative values in X   !!!
  fTopRightCornerX = -64*fLKrCellLength + fXfrontReferenceCell - fLKrCellLength;
  fTopRightCornerY = 64*fLKrCellLength;
}

LKrGeometryParameters::~LKrGeometryParameters(){}

LKrGeometryParameters* LKrGeometryParameters::GetInstance(){
  if ( fInstance == 0 ) { fInstance = new LKrGeometryParameters(); }
  return fInstance;
}

TObjArray LKrGeometryParameters::GetHashTable(){
  TObjArray LKrGeometryParameters;
  std::ostringstream Buffer;
  TString Value;
  TObjArray ParameterData;

  Buffer << fWorldZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldZLength));
  LKrGeometryParameters.Add(new DetectorParameter("fWorldZLength",Value.Data(),
        "World Z Length", ParameterData));
  ParameterData.Clear();

  Buffer << fWorldXLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldXLength));
  LKrGeometryParameters.Add(new DetectorParameter("fWorldXLength",Value.Data(),
        "World X Length", ParameterData));
  ParameterData.Clear();

  Buffer << fWorldYLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fWorldYLength));
  LKrGeometryParameters.Add(new DetectorParameter("fWorldYLength",Value.Data(),
        "World Y Length", ParameterData));
  ParameterData.Clear();

  Buffer << fLKrDetectorZPosition;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fLKrDetectorZPosition));
  LKrGeometryParameters.Add(new DetectorParameter("fLKrDetectorZPosition",Value.Data(),
        "LKr Detector Z Position", ParameterData));
  ParameterData.Clear();

  Buffer << fLKrDetectorZLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fLKrDetectorZLength));
  LKrGeometryParameters.Add(new DetectorParameter("fLKrDetectorZLength",Value.Data(),
        "LKr Detector Z Length", ParameterData));
  ParameterData.Clear();

  Buffer << fLKrDetectorXLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fLKrDetectorXLength));
  LKrGeometryParameters.Add(new DetectorParameter("fLKrDetectorXLength",Value.Data(),
        "LKr Detector X Length", ParameterData));
  ParameterData.Clear();

  Buffer << fLKrDetectorYLength;
  Value = Buffer.str();
  Buffer.str("");
  ParameterData.Add(new TVectorT<G4double>(1, &fLKrDetectorYLength));
  LKrGeometryParameters.Add(new DetectorParameter("fLKrDetectorYLength",Value.Data(),
        "LKr Detector Y Length", ParameterData));

  return LKrGeometryParameters;
}

void LKrGeometryParameters::Print(){
  G4cout << "fWorldZLength= "<< fWorldZLength << G4endl
    << "fWorldXLength= "<< fWorldXLength << G4endl
    << "fWorldYLength= "<< fWorldYLength << G4endl
    << "fLKrDetectorZPosition= "<< fLKrDetectorZPosition << G4endl
    << "fLKrDetectorZLength= "<< fLKrDetectorZLength << G4endl
    << "fLKrDetectorXLength= "<< fLKrDetectorXLength << G4endl
    << "fLKrDetectorYLength= "<< fLKrDetectorYLength << G4endl;
}
