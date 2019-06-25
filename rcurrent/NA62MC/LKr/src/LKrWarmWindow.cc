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

/// \class LKrWarmWindow
/// \Brief
/// The LKr warm windows and the related flanges.
/// \EndBrief
///
/// \Detailed
/// This class stores and provides the information about the geometry and position of the front and back warm window.The outer flanges are created here. The flange around the beam pipe is added.
/// \EndDetailed


LKrWarmWindow::LKrWarmWindow (G4Material * Material, G4LogicalVolume * MotherVolume):
    NA62VComponent(Material,MotherVolume)
{
    ReadGeometryParameters();

    // Mandatory here to Find or Build the needed materials
    LKrMaterialParameters::GetInstance();
    CreateGeometry();
    SetProperties();
}

LKrWarmWindow::~LKrWarmWindow(){}

void LKrWarmWindow::ReadGeometryParameters()
{
    // Read all the geometrical parameters and copy them to private members
    LKrGeometryParameters* GeoPars = LKrGeometryParameters::GetInstance();

    fOuterCryostatMinRadius = GeoPars->GetOuterCryostatMinRadius();
    fThicknessOfOuterCryo = GeoPars->GetThicknessOfOuterCryo(); 
    fLongitudinalLengthOuterCryo = GeoPars->GetLongitudinalLengthOuterCryo();


    fRadiusHoleSpacerPlate = GeoPars->GetRadiusHoleSpacerPlate();

    fRadiusWarmWindow = GeoPars->GetRadiusWarmWindow();
    fThicknesConvexWarmWindow = GeoPars->GetThicknesConvexWarmWindow();

    
    fHightFlangeAtWarmWindow1Beam = GeoPars->GetHightFlangeAtWarmWindow1Beam() ;
  
           
    fHightFlangeAtWarmWindow1Out = GeoPars->GetHightFlangeAtWarmWindow1Out() ;
    fLengthFlangeAtWarmWindow1Out = GeoPars->GetLengthFlangeAtWarmWindow1Out()  ;
    fMinRadiusFlangeAtWarmWindow1Out = GeoPars->GetMinRadiusFlangeAtWarmWindow1Out() ;
    
    fHightFlangeAtWarmWindow2Out = GeoPars->GetHightFlangeAtWarmWindow2Out() ;
    fLengthFlangeAtWarmWindow2Out = GeoPars->GetLengthFlangeAtWarmWindow2Out() ;
    fMinRadiusFlangeAtWarmWindow2Out = GeoPars->GetMinRadiusFlangeAtWarmWindow2Out();

    fOuterBeamPipeRadius = GeoPars->GetOuterBeamPipeRadius();
     

    fBEATCHPositionFrontWarmWindowEdge = GeoPars->GetBEATCHPositionFrontWarmWindowEdge();
    fBEATCHPositionFrontEdgeOuterVessel = GeoPars->GetBEATCHPositionFrontEdgeOuterVessel();
    fBEATCHPositionBackEdgeOuterVessel = GeoPars->GetBEATCHPositionBackEdgeOuterVessel();
    fBEATCHPositionBackWarmWindowEdge = GeoPars->GetBEATCHPositionBackWarmWindowEdge();
    fBEATCHPositionOuterWarmFlange1 = GeoPars->GetBEATCHPositionOuterWarmFlange1();


    fVerticalRadiusWarmWindow = GeoPars->GetVerticalRadiusWarmWindow();
    
   
}

void LKrWarmWindow::CreateGeometry()
{


  G4double startPhiAngle=0;
  G4double deltaPhiAngle=360*deg;
  
  G4double CurvatureRadius = fRadiusWarmWindow;
  
  G4double MinTheta =  asin(( fOuterBeamPipeRadius +  fHightFlangeAtWarmWindow1Beam)/CurvatureRadius);
  G4double MaxTheta =  asin(fVerticalRadiusWarmWindow/CurvatureRadius);
  
  solidWarmWindow= new G4Sphere("LKrWarmWindow",
				CurvatureRadius - fThicknesConvexWarmWindow,
				CurvatureRadius,
				startPhiAngle,
				deltaPhiAngle,
				MinTheta,
				MaxTheta - MinTheta);
  
  logicalWarmWindow = new G4LogicalVolume(solidWarmWindow,        // solid
					  fMaterial,             // material
					  "LKrWarmWindow",           // name
					  0,                    // field manager
					  0,                    // sensitive detector
					  0);                   // user limits
  
  
  G4RotationMatrix Ra =  G4RotationMatrix(G4ThreeVector(0., 1.,0.),-180*deg);
  
  
  //calculate the thickness of the missing segment to complete sphere at the front
  G4double MissingPieceSphereFront = CurvatureRadius - (fOuterBeamPipeRadius +  fHightFlangeAtWarmWindow1Beam)/ tan(MinTheta); 
  
  //Longitudinal thickness of the slice of the sphere, used as Warm Window
  
  G4double ThicknessSliceWarmWindow = (fOuterBeamPipeRadius +  fHightFlangeAtWarmWindow1Beam) / tan(MinTheta) - (fVerticalRadiusWarmWindow)/tan(MaxTheta); 
  G4ThreeVector Ta = G4ThreeVector(0,0, fRadiusWarmWindow - fLongitudinalLengthOuterCryo/2 - ThicknessSliceWarmWindow 
				   + fLongitudinalLengthOuterCryo + fBEATCHPositionBackWarmWindowEdge -  fBEATCHPositionBackEdgeOuterVessel);
  
  phisyWarmBackWindow = new G4PVPlacement(G4Transform3D(Ra,Ta),
					  logicalWarmWindow,             // its logical volume
					  "LKrWarmBackWindow",                // its name
					  fMotherVolume,              // its mother  volume
					  false,                     // no boolean operations
					  0);                        // copy number
  
  
  //*****************************************************************************//
  //********************* create warm front window
  
  Ra =  G4RotationMatrix(G4ThreeVector(0., 1.,0.),0*deg);
  Ta = G4ThreeVector(0,0,- fRadiusWarmWindow + fLongitudinalLengthOuterCryo/2 + ThicknessSliceWarmWindow - fLongitudinalLengthOuterCryo 
		     - (fBEATCHPositionFrontEdgeOuterVessel - fBEATCHPositionFrontWarmWindowEdge));
  
  phisyWarmFrontWindow = new G4PVPlacement(G4Transform3D(Ra,Ta),
					   logicalWarmWindow,             // its logical volume
					   "LKrWarmFrontWindow",                // its name
					   fMotherVolume,              // its mother  volume
					   false,                     // no boolean operations
					   0);                        // copy number
  
  
  
  //******************************************************************************//
  //********    Make the cones connecting the warm windows to the outer vessel
  //********
  
  solidCone = new G4Cons("solidWarmFrontCone",                        
			 fVerticalRadiusWarmWindow + fHightFlangeAtWarmWindow2Out ,
			 fVerticalRadiusWarmWindow + fHightFlangeAtWarmWindow2Out + fThicknessOfOuterCryo,
			 fOuterCryostatMinRadius,
			 fOuterCryostatMinRadius + fThicknessOfOuterCryo,
			 (fBEATCHPositionOuterWarmFlange1 - fBEATCHPositionFrontWarmWindowEdge) / 2,
			 0 *deg,
			 360 * deg);
  
  
  logicalCone = new G4LogicalVolume(solidCone,        // solid
				    fMaterial,             // material
				    "LKrWarmFrontCone",           // name
				    0,                    // field manager
				    0,                    // sensitive detector
				    0);                   // user limits
  
  
  Ta = G4ThreeVector(0,0,-  fLongitudinalLengthOuterCryo/2 - (fBEATCHPositionFrontEdgeOuterVessel - fBEATCHPositionOuterWarmFlange1 ) 
		     - (fBEATCHPositionOuterWarmFlange1 - fBEATCHPositionFrontWarmWindowEdge ) / 2);
  phisyCone = new G4PVPlacement(G4Transform3D(Ra,Ta),
				logicalCone,             // its logical volume
				"LKrWarmFrontCone",                // its name
				fMotherVolume,              // its mother  volume
				false,                     // no boolean operations
				0);
  
  //  
  // create the cone in the back
  //
  
  
  solidBackCone = new G4Cons("solidWarmBackCone",
			     fOuterCryostatMinRadius,
			     fOuterCryostatMinRadius + fThicknessOfOuterCryo,                        
			     fVerticalRadiusWarmWindow + fHightFlangeAtWarmWindow2Out ,
			     fVerticalRadiusWarmWindow + fHightFlangeAtWarmWindow2Out + fThicknessOfOuterCryo,
			     (fBEATCHPositionOuterWarmFlange1 - fBEATCHPositionFrontWarmWindowEdge ) / 2,
			     0 *deg,
			     360 * deg);
  
  
  logicalBackCone = new G4LogicalVolume(solidBackCone,        // solid
					fMaterial,             // material
					"LKrWarmBackCone",           // name
					0,                    // field manager
					0,                    // sensitive detector
					0);                   // user limits
  
  
  Ta = G4ThreeVector(0,0, fLongitudinalLengthOuterCryo/2 + (fBEATCHPositionFrontEdgeOuterVessel - fBEATCHPositionOuterWarmFlange1 ) 
		     + (fBEATCHPositionOuterWarmFlange1 - fBEATCHPositionFrontWarmWindowEdge) / 2);
  
  phisyBackCone = new G4PVPlacement(G4Transform3D(Ra,Ta),
				    logicalBackCone,             // its logical volume
				    "LKrWarmFrontCone",                // its name
				    fMotherVolume,              // its mother  volume
				    false,                     // no boolean operations
				    0);  
    
  G4double ZPositionFrontWarmWindow = - fLongitudinalLengthOuterCryo/2 + ThicknessSliceWarmWindow  
                                      - (fBEATCHPositionFrontEdgeOuterVessel - fBEATCHPositionFrontWarmWindowEdge) - MissingPieceSphereFront;
  
  G4double ZPositionBackWarmWindow = fLongitudinalLengthOuterCryo/2 - ThicknessSliceWarmWindow 
                                     + fBEATCHPositionBackWarmWindowEdge -  fBEATCHPositionBackEdgeOuterVessel + MissingPieceSphereFront;  
  
  //*****************************************************************************************
  //******** create a cylindric volume which hosts all the flanges and bolts at the warm windoow

  fGlobalFlangeFrontWarmWindow = new LKrWarmWindowFlangesBeam(" GlobalFlangeFrontWarmWindow", fMaterial, fMotherVolume,ZPositionFrontWarmWindow, 1); 
  fGlobalFlangeBackWarmWindow = new LKrWarmWindowFlangesBeam(" GlobalFlangeBackWarmWindow", fMaterial, fMotherVolume,ZPositionBackWarmWindow, - 1);
  
  
  //*****************************************************************************************
  //************************************  create FLANGES 
  //****************************************************************************************
 
  //
  // Front window
  
  //
  //outer flanges
  
  
  // part 1
  
   Ta = G4ThreeVector(0.,0., -  fLongitudinalLengthOuterCryo/2 - fLengthFlangeAtWarmWindow1Out/2 );
   fFlangeAtWarmWindow1Out = new LKrFlange("FlangeAtWarmWindow1Out",fMaterial, fMotherVolume, 
					   fMinRadiusFlangeAtWarmWindow1Out , fHightFlangeAtWarmWindow1Out, fLengthFlangeAtWarmWindow1Out , Ta );     
   // part 2
    
  Ta = G4ThreeVector(0.,0., -  fLongitudinalLengthOuterCryo/2 - (fBEATCHPositionFrontEdgeOuterVessel - fBEATCHPositionFrontWarmWindowEdge) + fLengthFlangeAtWarmWindow2Out/2);
  fFlangeAtWarmWindow2Out = new LKrFlange("FlangeAtWarmWindow2Out",fMaterial, fMotherVolume, 
					  fMinRadiusFlangeAtWarmWindow2Out , fHightFlangeAtWarmWindow2Out, fLengthFlangeAtWarmWindow2Out , Ta );     
  
  
  
  //*******************************************************************
  //******************************  create the flanges in the Back
   
  //
  //outer flanges
    
  // part 1
    
  Ta = G4ThreeVector(0.,0., +  fLongitudinalLengthOuterCryo/2 + fLengthFlangeAtWarmWindow1Out/2 );
  fFlangeAtWarmWindow1OutBack = new LKrFlange("FlangeAtWarmWindow1OutBack", fMaterial, fMotherVolume, 
					      fMinRadiusFlangeAtWarmWindow1Out , fHightFlangeAtWarmWindow1Out, fLengthFlangeAtWarmWindow1Out , Ta );     

  // part 2
    
  Ta = G4ThreeVector(0.,0., +  fLongitudinalLengthOuterCryo/2 + fBEATCHPositionBackWarmWindowEdge 
		     -  fBEATCHPositionBackEdgeOuterVessel - fLengthFlangeAtWarmWindow2Out/2);
  fFlangeAtWarmWindow2OutBack = new LKrFlange("FlangeAtWarmWindow2OutBack", fMaterial, fMotherVolume, 
					      fMinRadiusFlangeAtWarmWindow2Out , fHightFlangeAtWarmWindow2Out, 
					      fLengthFlangeAtWarmWindow2Out , Ta );     
      
}

void LKrWarmWindow::SetProperties()
{
  // Set visualization properties
    fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
    fVisAtt -> SetVisibility(true);
    //logicWarmWindow ->SetVisAttributes(fVisAtt);
}
