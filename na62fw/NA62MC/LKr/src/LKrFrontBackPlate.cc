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
//            Evelina Marinova (Evelina.MArinova@cern.ch)
//
// --------------------------------------------------------------------

#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4Trap.hh"
#include "G4Para.hh"
#include "G4Polyhedra.hh"

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

#include "LKrFrontBackPlate.hh"
#include "G4SDManager.hh"

/// \class LKrFrontBackPlate 
/// \Brief
/// Stesalit front and back octagon plate (outside the the electrode cells), LKr.
/// \EndBrief   
///
/// \Detailed
/// This class is responsible for the correct thickness of the front and back plate. The electrode cell contain only a part of the front 
/// and back plate (as much as the spacer plates) for symmetry reasons. Two additional octagons of G10 are built here.
/// \EndDetailed



LKrFrontBackPlate::LKrFrontBackPlate (G4Material * Material, G4LogicalVolume * MotherVolume):
    NA62VComponent(Material,MotherVolume)
{
    ReadGeometryParameters();

    //fFrontBackPlateTransform = FrontBackPlateTransform;

    // Mandatory here to Find or Build the needed materials
    LKrMaterialParameters::GetInstance();
    CreateGeometry();
    SetProperties();
}

LKrFrontBackPlate::~LKrFrontBackPlate(){}

void LKrFrontBackPlate::ReadGeometryParameters()
{
    // Read all the geometrical parameters and copy them to private members
    LKrGeometryParameters* GeoPars = LKrGeometryParameters::GetInstance();

    fInnerBeamPipeRadius = GeoPars->GetInnerBeamPipeRadius();
    fOuterBeamPipeRadius = GeoPars->GetOuterBeamPipeRadius();
    fLongitudinalLengthBeamPipe =  GeoPars->GetLongitudinalLengthBeamPipe();

    fHalfLengthOfFrontPlateOctagon =  GeoPars->GetHalfLengthOfFrontPlateOctagon();
    fHalfLengthOfBackPlateOctagon =  GeoPars->GetHalfLengthOfBackPlateOctagon();

    fRadiusHoleSpacerPlate = GeoPars->GetRadiusHoleSpacerPlate();

    fHalfNElectrodesY = GeoPars->GetHalfNElectrodesY();
    fDistanceToNextElectrodeBackY = GeoPars->GetDistanceToNextElectrodeBackY(); 
    fLongitudinalLengthInnerCryo = GeoPars->GetLongitudinalLengthInnerCryo();
    fDistanceFrontPlateBackPlate = GeoPars->GetDistanceFrontPlateBackPlate();
    fLongitudinalLengthLKrVolume = GeoPars->GetLongitudinalLengthLKrVolume();
    fPassiveLKrInsideOctagon = GeoPars->GetPassiveLKrInsideOctagon(); 
    fLengthOfColdWindowPlate = GeoPars->GetLengthOfColdWindowPlate(); 
}

void LKrFrontBackPlate::CreateGeometry()
{
 // create solid beam pipe so this volume can later be subtracted of the windows and the filler

  G4double  pSPhi = 0*deg;
  G4double  pDPhi = 360*deg;
  
  G4Tubs* solidBeamPipe = new G4Tubs("solid_BeamPipe",                        
				     0,
				     fRadiusHoleSpacerPlate,
				     fLongitudinalLengthBeamPipe/2,
				     pSPhi,
				     pDPhi);

  // start with the octagon in the front


    G4double phiStart = 0*deg;
    G4double phiTotal= 360*deg; 
    G4int numSide = 8; 
  

    G4int numZPlanes = 2; 
    
    // add octagon of stesalite in front of the electrode cells to compensate for the thicker front plate
    G4double OuterRadiusOctagon = fHalfNElectrodesY * fDistanceToNextElectrodeBackY ;
    
    G4double LengthOfOctagon = fHalfLengthOfFrontPlateOctagon * 2;
    
    G4double rInner[2] = { 0 , 0}; 

    G4double rOuter[2] = { OuterRadiusOctagon, OuterRadiusOctagon}; 

    G4double zPlane[2] = {-LengthOfOctagon/2 ,LengthOfOctagon/2 } ;
    solidOct = new G4Polyhedra("oct",phiStart,phiTotal,numSide,numZPlanes,zPlane, rInner,rOuter);


    G4VSolid * OctagonMinusBeamPipe = new G4SubtractionSolid("OctagonMinusBeamPipe",
							     solidOct,
							     solidBeamPipe
							     );
     logicOct = new G4LogicalVolume(OctagonMinusBeamPipe , fMaterial, "oct",0,0,0);  




    G4RotationMatrix Ra;
    Ra.rotateZ(22.5*deg);
    G4ThreeVector Ta = G4ThreeVector(0,0,- fLongitudinalLengthLKrVolume / 2  + fHalfLengthOfFrontPlateOctagon + fLengthOfColdWindowPlate);
    physiOct = new G4PVPlacement(G4Transform3D(Ra,Ta),              // no rotation
            logicOct,    // its logical volume				  
            "Oct",       // its name
            fMotherVolume,      // its mother  volume
            false,           // no boolean operations
            0);              // copy number 


    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    // add octagon of stesalite at the back of the electrode cells to compensate for the thicker back plate

      
    LengthOfOctagon = fHalfLengthOfBackPlateOctagon * 2;


    G4double rInnerBack[2] = { 0, 0}; 

    G4double rOuterBack[2] = { OuterRadiusOctagon, OuterRadiusOctagon}; 

    G4double zPlaneBack[2] = {- LengthOfOctagon/2 ,LengthOfOctagon/2 } ;
    solidOctBack = new G4Polyhedra("octback",phiStart,phiTotal,numSide,numZPlanes,zPlaneBack, rInnerBack,rOuterBack);

 G4VSolid * BackOctagonMinusBeamPipe = new G4SubtractionSolid("BackOctagonMinusBeamPipe",
							     solidOctBack,
							     solidBeamPipe
							     );

    logicOctBack = new G4LogicalVolume(BackOctagonMinusBeamPipe , fMaterial, "oct",0,0,0);  

    //G4RotationMatrix Ra;
    //Ra.rotateZ(22.5*deg);

    Ta = G4ThreeVector(0,0, fLongitudinalLengthLKrVolume / 2 - fHalfLengthOfBackPlateOctagon);
    physiOctBack = new G4PVPlacement(G4Transform3D(Ra,Ta),              // no rotation
            logicOctBack,    // its logical volume				  
            "OctBack",       // its name
            fMotherVolume,      // its mother  volume
            false,           // no boolean operations
            0);              // copy number 
     
}

void LKrFrontBackPlate::SetProperties()
{
    // Set visualization properties
    fVisAtt= new G4VisAttributes(G4Colour(0.0,1.0,1.0));
    fVisAtt->SetForceSolid(true);
    fVisAtt -> SetVisibility(true);
    logicOct ->SetVisAttributes(fVisAtt);
    // logicOctBack ->SetVisAttributes(fVisAtt);
}
