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
// --------------------------------------------------------------------
// History:
//
//            Antonino Sergi (Antonino.Sergi@cern.ch)
//            Evelina Marinova(Evelina.Marinova@cern.ch) 
//
// --------------------------------------------------------------------

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4Trap.hh"
#include "G4Para.hh"
#include "G4Sphere.hh"
#include "G4Cons.hh"
#include "G4EllipticalTube.hh"

#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4UnionSolid.hh"
#include "G4IntersectionSolid.hh"
#include "G4SubtractionSolid.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "LKrGeometryParameters.hh"
#include "LKrMaterialParameters.hh"
#include "LKrFlange.hh"
#include "LKrFlangesBolts.hh"
#include "LKrElectrodes.hh"
#include "LKrWarmWindow.hh"
#include "LKrWarmWindowFlangesBeam.hh"

#include "G4SDManager.hh"
#include "LKrLKrVolume.hh"
#include "LKrFrontBackPlate.hh"

/// \class LKrWarmWindowFlangesBeam
/// \Brief
/// All the flanges around the beam pipe at the warm windows of the LKr.
/// \EndBrief
///
/// \Detailed
/// This class stores and provides the information about the geometry and position of the flanges at the front and back warm window around the beam pipe.
/// \EndDetailed


LKrWarmWindowFlangesBeam::LKrWarmWindowFlangesBeam (G4String Name, G4Material * Material, G4LogicalVolume * MotherVolume, G4double ZPositionWarmWindow, G4int Sign):
    NA62VComponent(Material,MotherVolume)
{
    ReadGeometryParameters();

    fName = Name;
    fZPositionWarmWindow = ZPositionWarmWindow;
    fSign = Sign;

    // Mandatory here to Find or Build the needed materials
    LKrMaterialParameters::GetInstance();
    CreateGeometry();
    SetProperties();
}

LKrWarmWindowFlangesBeam::~LKrWarmWindowFlangesBeam(){}

void LKrWarmWindowFlangesBeam::ReadGeometryParameters()
{
    // Read all the geometrical parameters and copy them to private members
    LKrGeometryParameters* GeoPars = LKrGeometryParameters::GetInstance();
    
  
    fThicknesConvexWarmWindow = GeoPars->GetThicknesConvexWarmWindow();

    fHightFlangeAtWarmWindow1Beam = GeoPars->GetHightFlangeAtWarmWindow1Beam() ;
    fLengthFlangeAtWarmWindow1Beam = GeoPars->GetLengthFlangeAtWarmWindow1Beam() ;
    fMinRadiusFlangeAtWarmWindow1Beam  = GeoPars->GetMinRadiusFlangeAtWarmWindow1Beam();
    
    fHightFlangeAtWarmWindow2Beam = GeoPars->GetHightFlangeAtWarmWindow2Beam() ;
    fLengthFlangeAtWarmWindow2Beam = GeoPars->GetLengthFlangeAtWarmWindow2Beam() ;
    fMinRadiusFlangeAtWarmWindow2Beam = GeoPars->GetMinRadiusFlangeAtWarmWindow2Beam() ;
      
    fHightFlangeAtWarmWindow3Beam = GeoPars->GetHightFlangeAtWarmWindow3Beam() ;
    fLengthFlangeAtWarmWindow3Beam = GeoPars->GetLengthFlangeAtWarmWindow3Beam() ;
    fMinRadiusFlangeAtWarmWindow3Beam = GeoPars->GetMinRadiusFlangeAtWarmWindow3Beam() ;
    
    fHightFlangeAtWarmWindow4Beam = GeoPars->GetHightFlangeAtWarmWindow4Beam() ;
    fLengthFlangeAtWarmWindow4Beam = GeoPars->GetLengthFlangeAtWarmWindow4Beam() ;
    fMinRadiusFlangeAtWarmWindow4Beam = GeoPars->GetMinRadiusFlangeAtWarmWindow4Beam() ;
    
    fHightFlangeAtWarmWindow5Beam = GeoPars->GetHightFlangeAtWarmWindow5Beam() ;
    fLengthFlangeAtWarmWindow5Beam = GeoPars->GetLengthFlangeAtWarmWindow5Beam() ;
    fMinRadiusFlangeAtWarmWindow5Beam = GeoPars->GetMinRadiusFlangeAtWarmWindow5Beam() ;
    
    fOuterBeamPipeRadius = GeoPars->GetOuterBeamPipeRadius();
    fDistanceWarmWindowFlange4 = GeoPars->GetDistanceWarmWindowFlange4();
    fDistanceWarmWindowFlange3 = GeoPars->GetDistanceWarmWindowFlange3();

    fDistanceWarmWindowFlange2EndFlange3Front = GeoPars->GetDistanceWarmWindowFlange2EndFlange3Front();
    
    fVerticalRadiusWarmWindow = GeoPars->GetVerticalRadiusWarmWindow();
    
    fAngleCentralAxisBoltUpperWarm = GeoPars->GetAngleCentralAxisBoltUpperWarm();
    fRadiusHolesBoltsUpperWarm = GeoPars->GetRadiusHolesBoltsUpperWarm() ;
    fLengthHolesBoltsUpperWarmInFlange5 = GeoPars->GetLengthHolesBoltsUpperWarmInFlange5() ;
    
    fDistanceUpperWarmBoltToBeamPipe = GeoPars->GetDistanceUpperWarmBoltToBeamPipe() ;
    
    fRadiusHeadsBoltsUpperWarm = GeoPars->GetRadiusHeadsBoltsUpperWarm() ;
    fLengthHeadsBoltsUpperWarm = GeoPars->GetLengthHeadsBoltsUpperWarm() ;
    
    fAngleCentralAxisBoltLowerWarm = GeoPars->GetAngleCentralAxisBoltLowerWarm() ;
    fRadiusHolesBoltsLowerWarm = GeoPars->GetRadiusHolesBoltsLowerWarm() ;
    fLengthHolesBoltsLowerWarm = GeoPars->GetLengthHolesBoltsLowerWarm();
    
    fDistanceLowerWarmBoltToFlange3 = GeoPars->GetDistanceLowerWarmBoltToFlange3() ;
    
    fRadiusHeadsBoltsLowerWarm = GeoPars->GetRadiusHeadsBoltsLowerWarm() ;
    fLengthHeadsBoltsLowerWarm = GeoPars->GetLengthHeadsBoltsLowerWarm();

    fElipticalCutFlange3LongAxis = GeoPars->GetElipticalCutFlange3LongAxis() ;
    fElipticalCutFlange3ShortAxis = GeoPars->GetElipticalCutFlange3ShortAxis() ;

}

void LKrWarmWindowFlangesBeam::CreateGeometry()
{

  G4ThreeVector Ta;
 
  //
  // Create a cylindric volume which hosts all the flanges and bolts at the warm windows
 
  //one in the front
  
  G4double LenghtGlobalWarmWindowFlange =  fLengthFlangeAtWarmWindow1Beam + fLengthFlangeAtWarmWindow2Beam 
    + fLengthFlangeAtWarmWindow3Beam  + fDistanceWarmWindowFlange2EndFlange3Front + fLengthHeadsBoltsUpperWarm; 
  
  Ta = G4ThreeVector(0.,0.,fZPositionWarmWindow + fSign * fThicknesConvexWarmWindow /2 
		     + fSign * fDistanceWarmWindowFlange3 - fSign * LenghtGlobalWarmWindowFlange / 2);
  
  fGlobalWarmWindowFlange = new LKrFlange("GlobalWarmWindowFlange",G4Material::GetMaterial("G4_Galactic"), 
					  fMotherVolume, fMinRadiusFlangeAtWarmWindow1Beam, fHightFlangeAtWarmWindow1Beam, 
					  LenghtGlobalWarmWindowFlange , Ta );   
  
  
  
  //************************************
  //******************* create FLANGES 
  
  
  // put pieces of the mirror in the general flange

  //front
  
  G4double HightMirrorSegment = fHightFlangeAtWarmWindow1Beam + fMinRadiusFlangeAtWarmWindow1Beam 
    - (fHightFlangeAtWarmWindow4Beam + fMinRadiusFlangeAtWarmWindow4Beam);
  
  Ta = G4ThreeVector(0.,0.,fSign * LenghtGlobalWarmWindowFlange/2 
		     - fSign * fThicknesConvexWarmWindow/2 - fSign * fDistanceWarmWindowFlange3);
  fWarmWindowFrontSegment = new LKrFlange("WarmWindowFrontSegment ", fMaterial, fGlobalWarmWindowFlange->GetLogicalVolume(), 
					  fMinRadiusFlangeAtWarmWindow4Beam + fHightFlangeAtWarmWindow4Beam, 
					  HightMirrorSegment,fThicknesConvexWarmWindow, Ta );     
  //
  // Front window
  
  //
  // flanges around the beam pipe
  
  // part 4
    
 Ta = G4ThreeVector(0.,0.,fSign * LenghtGlobalWarmWindowFlange/2 - fSign * fLengthFlangeAtWarmWindow4Beam/2 
		    - fSign *( fDistanceWarmWindowFlange3 - fDistanceWarmWindowFlange4));
 fFlangeAtWarmWindow4Beam = new LKrFlange("FlangeAtWarmWindow4Beam", fMaterial, fGlobalWarmWindowFlange->GetLogicalVolume(), 
					  fMinRadiusFlangeAtWarmWindow4Beam , fHightFlangeAtWarmWindow4Beam, 
					  fLengthFlangeAtWarmWindow4Beam , Ta );     
 // part 5
 
 Ta = G4ThreeVector(0.,0., Ta.z()  - fSign * fLengthFlangeAtWarmWindow5Beam/2  - fSign * fLengthFlangeAtWarmWindow4Beam/2 );
 fFlangeAtWarmWindow5Beam = new LKrFlange("FlangeAtWarmWindow5Beam",fMaterial, 
					  fGlobalWarmWindowFlange->GetLogicalVolume(), fMinRadiusFlangeAtWarmWindow5Beam , 
					  fHightFlangeAtWarmWindow5Beam, fLengthFlangeAtWarmWindow5Beam , Ta );     
  
  // part 2
 
 Ta = G4ThreeVector(0.,0.,Ta.z()  - fSign * fLengthFlangeAtWarmWindow5Beam/2  - fSign * fLengthFlangeAtWarmWindow2Beam/2);
 G4double ZpositionWarmFlange2 = Ta.z(); 
 fFlangeAtWarmWindow2Beam = new LKrFlange("FlangeAtWarmWindow2Beam", G4Material::GetMaterial("G4_POLYVINYL_CHLORIDE"), 
					  fGlobalWarmWindowFlange->GetLogicalVolume(), fMinRadiusFlangeAtWarmWindow2Beam , 
					  fHightFlangeAtWarmWindow2Beam, fLengthFlangeAtWarmWindow2Beam , Ta  );     
 
 // part 1
 
 Ta = G4ThreeVector(0.,0.,Ta.z()  - fSign * fLengthFlangeAtWarmWindow2Beam/2  - fSign * fLengthFlangeAtWarmWindow1Beam/2);
 G4double ZpositionWarmFlange1 = Ta.z();
 fFlangeAtWarmWindow1Beam = new LKrFlange("FlangeAtWarmWindow1Beam",G4Material::GetMaterial("LKr_StainlessSteel"), 
					  fGlobalWarmWindowFlange->GetLogicalVolume(), fMinRadiusFlangeAtWarmWindow1Beam , 
					  fHightFlangeAtWarmWindow1Beam, fLengthFlangeAtWarmWindow1Beam , Ta );     
 
  // part 3
 
 Ta = G4ThreeVector(0.,0., ZpositionWarmFlange2 + fSign * fLengthFlangeAtWarmWindow2Beam/2 
		    + fSign * fLengthFlangeAtWarmWindow3Beam/2 + fSign * fDistanceWarmWindowFlange2EndFlange3Front);
 G4double ZpositionWarmFlange3 = Ta.z(); 
 fFlangeAtWarmWindow3Beam = new LKrFlange("FlangeAtWarmWindow3Beam",fMaterial, fGlobalWarmWindowFlange->GetLogicalVolume(), 
					  fMinRadiusFlangeAtWarmWindow3Beam , fHightFlangeAtWarmWindow3Beam, fLengthFlangeAtWarmWindow3Beam , Ta );     
 
 
  //*********************************
  // *** eliptical cuts
  //********************************

  // make a section of the elipses with flange 3 / then put them as daughter volumes of flange 3
  // 12 half elipses cut along the longer axis, long axis is 40 mm, short axis is 2*10 mm
  // put the first elipse at 0 degrees.
  // make the elipses out of vacuum
  // rotate them as you go around the clock
  
  
  solidEllipticalOpeningInWarmFlange = new G4EllipticalTube("EllipticalOpeningInWarmFlange", fElipticalCutFlange3LongAxis, 
							    fElipticalCutFlange3ShortAxis,fLengthFlangeAtWarmWindow3Beam/2);
  
  // dummy solid tube for the intersection with the elliptical openings in the flange
  solidTube = new G4Tubs("solidTube",                        
			 fMinRadiusFlangeAtWarmWindow3Beam,
			 fMinRadiusFlangeAtWarmWindow3Beam+fHightFlangeAtWarmWindow3Beam,
			 fLengthFlangeAtWarmWindow3Beam/2,
			 0*deg,
			 360*deg);
  
  G4RotationMatrix Ra;
  G4RotationMatrix Rotation[12];
  G4double ThetaEllipse[12];
  
  for(G4int iOpening=0; iOpening<12; iOpening++ ) {
    
    ThetaEllipse[iOpening] = iOpening * (30 * deg);
    Rotation[iOpening].rotateZ(ThetaEllipse[iOpening]+90*deg);
        
    fsolidEllipticalOpeningInWarmFlange[iOpening] = new G4IntersectionSolid("IntersectedSolid", 
									    solidTube,
									    new G4DisplacedSolid("intersected",solidEllipticalOpeningInWarmFlange,
												 G4Transform3D(Rotation[iOpening],
													       G4ThreeVector(fMinRadiusFlangeAtWarmWindow3Beam * cos(ThetaEllipse[iOpening]), 
															     fMinRadiusFlangeAtWarmWindow3Beam * sin(ThetaEllipse[iOpening]),0))));
    
    logicEllipticalOpeningInWarmFlange[iOpening] = new G4LogicalVolume(fsolidEllipticalOpeningInWarmFlange[iOpening],
								       G4Material::GetMaterial("G4_Galactic"),"EllipticalOpeningInWarmFlange",0,0,0);
     
    physyEllipticalOpeningInWarmFlange[iOpening] = new G4PVPlacement(G4Transform3D (Ra, G4ThreeVector(0,0,0)),
								     logicEllipticalOpeningInWarmFlange[iOpening],    // its logical volume
								     "EllipticalOpeningInWarmFlange",       // its name
								     fFlangeAtWarmWindow3Beam->GetLogicalVolume(),      // its mother  volume
								     false,           // no boolean operations
								     0);              // copy number
  }
    
  
  //**************************************
  //********** bolts
  //************************************   
  
  // 12 upper and 12 lower bolts
  //the upper ones pass trough flages 2 and 5 (first bolt at 0 degrees) (flange one is anyway made of inox)
  //the lower ones are in flange 3 (first bolt at 15 degrees)
  //in addition, there are heads sticking out
    
  //front (the back is mirrored) 
    
  // upper bolts
  
  // in flange 2
    
  G4double ROfHolesBolts =  fDistanceUpperWarmBoltToBeamPipe +  fOuterBeamPipeRadius  ;
    
  // 12 equidistant bolts ==> at 30 deg interval
  
  for (G4int iHoleBolt = 0; iHoleBolt < 12 ; iHoleBolt++){
    
    G4double ThetaOfHolesBolts = fAngleCentralAxisBoltUpperWarm + iHoleBolt * (30 * deg);
    
    fHolesBoltsUpperWarmFlange2 = new LKrFlangesBolts("HolesBoltsUpperWarmFlange2", G4Material::GetMaterial("LKr_StainlessSteel"),
						      fFlangeAtWarmWindow2Beam->GetLogicalVolume(), fRadiusHolesBoltsUpperWarm, 
						      fLengthFlangeAtWarmWindow2Beam, ROfHolesBolts,  ThetaOfHolesBolts,0 );
   }
  
  // in flange 5
  
  // 12 equidistant bolts ==> at 30 deg interval
  
  for (G4int iHoleBolt = 0; iHoleBolt < 12 ; iHoleBolt++){
    
    G4double ThetaOfHolesBolts = fAngleCentralAxisBoltUpperWarm + iHoleBolt * (30 * deg);
    
    fHolesBoltsUpperWarmFlange5 = new LKrFlangesBolts("HolesBoltsUpperWarmFlange5",G4Material::GetMaterial("LKr_StainlessSteel"),
						      fFlangeAtWarmWindow5Beam->GetLogicalVolume(), fRadiusHolesBoltsUpperWarm, 
						      fLengthHolesBoltsUpperWarmInFlange5, ROfHolesBolts,  ThetaOfHolesBolts, 
						      - fSign * fLengthFlangeAtWarmWindow5Beam / 2 + fSign * fLengthHolesBoltsUpperWarmInFlange5 / 2);
  }   
  
  // upper heads
  
  for (G4int iHoleBolt = 0; iHoleBolt < 12 ; iHoleBolt++){
    
    G4double ThetaOfHolesBolts = fAngleCentralAxisBoltUpperWarm + iHoleBolt * (30 * deg);
    
    fHolesBoltsUpperHeads = new LKrFlangesBolts("HolesBoltsUpperHeads",G4Material::GetMaterial("LKr_StainlessSteel"),
						fGlobalWarmWindowFlange->GetLogicalVolume(), fRadiusHeadsBoltsUpperWarm, 
						fLengthHeadsBoltsUpperWarm, ROfHolesBolts,  ThetaOfHolesBolts,
						ZpositionWarmFlange1 - fSign * fLengthFlangeAtWarmWindow1Beam / 2 - fSign * fLengthHeadsBoltsUpperWarm / 2);
  }   
  
  // lower bolts in flange 3
    
  ROfHolesBolts =  (fMinRadiusFlangeAtWarmWindow3Beam + fHightFlangeAtWarmWindow3Beam / 2) ;
  
  // 12 equidistant bolts ==> at 30 deg interval
  
  for (G4int iHoleBolt = 0; iHoleBolt < 12 ; iHoleBolt++){
    
    G4double ThetaOfHolesBolts = fAngleCentralAxisBoltLowerWarm + iHoleBolt * (30 * deg);
    
    fHolesBoltsLowerWarm = new LKrFlangesBolts("HolesBoltsLowerWarm",G4Material::GetMaterial("LKr_StainlessSteel"),
					       fFlangeAtWarmWindow3Beam->GetLogicalVolume(), fRadiusHolesBoltsLowerWarm, 
					       fLengthHolesBoltsLowerWarm, ROfHolesBolts,  ThetaOfHolesBolts,
					       -  fSign * fLengthFlangeAtWarmWindow3Beam / 2 + fSign * fLengthHolesBoltsLowerWarm / 2);
  }
  
  //lower heads
    
  for (G4int iHoleBolt = 0; iHoleBolt < 12 ; iHoleBolt++){
    
    G4double ThetaOfHolesBolts = fAngleCentralAxisBoltLowerWarm + iHoleBolt * (30 * deg);
    
    fHolesBoltsLowerHeads = new LKrFlangesBolts("HolesBoltsLowerHeads",G4Material::GetMaterial("LKr_StainlessSteel"),
						fGlobalWarmWindowFlange->GetLogicalVolume(), fRadiusHeadsBoltsLowerWarm, 
						fLengthHeadsBoltsLowerWarm, ROfHolesBolts,  ThetaOfHolesBolts,
						ZpositionWarmFlange3 - fSign * fLengthFlangeAtWarmWindow3Beam / 2 - fSign * fLengthHeadsBoltsLowerWarm / 2);
    
  }   
    
  
}

void LKrWarmWindowFlangesBeam::SetProperties()
{
  // Set visualization properties
    fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
    fVisAtt -> SetVisibility(true);
    //logicWarmWindow ->SetVisAttributes(fVisAtt);
}
