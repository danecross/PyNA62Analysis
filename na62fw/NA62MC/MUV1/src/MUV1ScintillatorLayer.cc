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
// Created by Massimo Lenti (Massimo.Lenti@cern.ch) 2008-03-11
//            Francesca Bucci (Francesca.Bucci@cern.ch)
//            Antonino Sergi (Antonino.Sergi@cern.ch)
//
// --------------------------------------------------------------------
//
//Copy from MUVIronPlate
//Changes to MUV1 by ykohl in March 2010
//
// Modified by Gia Khoriauli (gia.khoriauli@cern.ch) 2017-11-10
//
// --------------------------------------------------------------------
#include "G4Box.hh"
#include "G4Tubs.hh"
#include "G4LogicalVolume.hh"
#include "G4PVPlacement.hh"
#include "globals.hh"

#include "G4UnionSolid.hh"
#include "G4SubtractionSolid.hh"

#include "G4VisAttributes.hh"
#include "G4Colour.hh"
#include "G4Material.hh"

#include "MUV1GeometryParameters.hh"
#include "MUV1MaterialParameters.hh"

#include "MUV1ScintillatorLayer.hh"
#include "MUV1Scintillator.hh"
#include "MUV1ScintillatorSpacer.hh"
#include "MUV1OuterSpacer.hh"
#include "MUV1Fiber.hh"
#include "MUV1PMT.hh"


/// \class MUV1ScintillatorLayer
/// \Brief
/// MUV1ScintillatorLayer class.
/// \EndBrief
///	A layer with scintillators is build. Also the outer fibers and PMT for read out are placed
/// \Detailed

/// \EndDetailed


MUV1ScintillatorLayer::MUV1ScintillatorLayer(G4Material * Material,
                                             G4LogicalVolume * MotherVolume,
                                             G4ThreeVector ScintLayerPosition,
                                             G4int iCopy, G4int Offset, G4String FastSimualtion) : NA62VComponent(Material,MotherVolume)
{
    ReadGeometryParameters();

    fiCopy = iCopy;
    fOffset = Offset;

    fScintLayerPosition = ScintLayerPosition;
    fFastSimulation = FastSimualtion;

    // Mandatory here to Find or Build the needed materials
    MUV1MaterialParameters::GetInstance();
    CreateGeometry();
    SetProperties();
}

MUV1ScintillatorLayer::~MUV1ScintillatorLayer(){}

void MUV1ScintillatorLayer::ReadGeometryParameters()
{
    // Read all the geometrical parameters and copy them to private members
    MUV1GeometryParameters* GeoPars = MUV1GeometryParameters::GetInstance();

    fScintMotherWidth     = GeoPars->GetScintMotherWidth();
    fScintMotherLength    = GeoPars->GetScintMotherLength();
    fScintMotherThickness = GeoPars->GetScintMotherThickness();
    fScintLayerThickness  = GeoPars->GetScintLayerThickness();

    fBareScintillatorThickness = GeoPars->GetBareScintillatorThickness();
    fRubberThickness           = GeoPars->GetRubberThickness();
    fIronThickness             = GeoPars->GetIronThickness();
    fRubberRealThickness       = GeoPars->GetRubberRealThickness();

    // thickness of each scintillator (zsci)
    fScintillatorThickness = GeoPars->GetScintillatorThickness();
    //Length of scintillators
    fScintLengthStandard    = GeoPars->GetScintLengthStandard();
    fScintLengthOuter       = GeoPars->GetScintLengthOuter();
    fScintLengthMiddleStd   = GeoPars->GetScintLengthMiddleStd();
    fScintLengthMiddleOuter = GeoPars->GetScintLengthMiddleOuter();
    fScintWidthStandard     = GeoPars->GetScintWidthStandard();
    fScintWidthMiddle       = GeoPars->GetScintWidthMiddle();

    fHoleInnerRadius = GeoPars->GetHoleInnerRadius(); // MUV1 hole inner radius
    fGrooveDepth = GeoPars->GetGrooveDepth();
    fGroovePositionInStandardScintillator = GeoPars->GetGroovePositionInStandardScintillator();

    fFiberOuterRadius = GeoPars -> GetFiberRadius();
    fFiberHightOut = GeoPars -> GetFiberLengthOut();
    fSkinWidth = GeoPars->GetSkinWidth();
    fAirGapWidth = GeoPars->GetAirGapWidth();
    fPMTSize = GeoPars->GetPMTSize();

    fHoleDimeter = GeoPars->GetHoleDiameter();
    fOuterSpacerOuterRadius = GeoPars->GetOuterSpacerOuterRadius();
    fConnectionHoleRadius = GeoPars->GetConnectionHoleRadius();
    fBoltPositionX = GeoPars->GetBoltPositionX();
    fBoltPositionY = GeoPars->GetBoltPositionY();

}

void MUV1ScintillatorLayer::CreateGeometry()
{
    // Build one or more boxes that will contain all the
    // detector sections, up to fill the responsibility region

    // Value of the loop variables bellow which cutting of normal strips into two halves shoudl start
    // 10 corresponds of the vertical layer, which has 3 normal scintillator strips cut into halves on each side (left and right)
    G4double CentralScint     = 27.5;
    G4int    Nscintillators   = 54;
    G4int    StartCuttingIndx = 10;
    G4int    EndShortIndx     = 23;
    G4int    EndSpecShortIndx = 25;
    G4int    EndShortPosShift = 0;
    G4int    Parity           = 1;
    G4int    Channel          = 0;

    // Space between the closest surfaces of two neighbour iron plates
    G4double GapBetweenTwoIronPlates = fBareScintillatorThickness + fRubberThickness;

    if(fiCopy%2==0)	{ //horizontal layer is under construction
    	//swap X-Y coordinates of connection holes in the even (horizontal) layers of scintillators, since they are rotated wrt the odd (vertical) layers by 90 deg
    	G4double xy    = fBoltPositionX;
    	fBoltPositionX = fBoltPositionY ;
    	fBoltPositionY = xy ;

    	//in the horizontal layers, which has only 2 normal scintillator strips cut into halves on each side (left and right), there are 54-2 scintillator strips only.
        CentralScint     = 26.5;
        Nscintillators   = 52;
    	StartCuttingIndx = 8 ;
    	EndShortIndx     = 22;
    	EndSpecShortIndx = 24;
        EndShortPosShift = 1;
        Parity           = 0;

    	//restore fiCopy value to be compatible to what is expected in the sensitive detector. see a in comment MUV1Detector::CreateGeometry().
    	fiCopy = (fiCopy+2)/2;
    }
    else	{//vertical layer is under construction
    	//restore fiCopy value to be compatible to what is expected in the sensitive detector. see a in comment MUV1Detector::CreateGeometry().
    	fiCopy = (fiCopy+1)/2;
    }

    // Define Scint layer blank volume (a box)
    G4Box * LayerBox = new G4Box("MUV1ScintillatorLayerBlankSV",
                            0.5*fScintMotherLength + 0.5 * mm,
                            0.5*fScintMotherLength + 0.5 * mm,
                            0.5*fScintLayerThickness );




    // ***  VOLUMES TO BE ADDED ***

    // Extra volume for inner iron spacer
    G4Box * SpacerBox = new G4Box("MUV1ScintillatorLayerSpacerBoxSV",
    								fHoleDimeter/2. + 0.5 * mm,
    								fHoleDimeter/2. + 0.5 * mm,
    								(GapBetweenTwoIronPlates - 0.1 * fAirGapWidth)/2.);

    // Extra volume for OuterSpacers of connection bolts
    G4Tubs * OuterSpacerTube = new G4Tubs("MUV1ScintillatorLayerOuterSpacerTubeSV",
  				     	 	 	 	 	  0,
  				     	 	 	 	 	  fOuterSpacerOuterRadius + 0.5 * mm,
  				     	 	 	 	 	  (GapBetweenTwoIronPlates - 0.1 * fAirGapWidth)/2.,
  				     	 	 	 	 	  0.*deg,
  				     	 	 	 	 	  360.*deg);

    // ! NOTE ! Relies on the auxiliary knowledge that the scintillator layer logical volume center Z position is by
    //    				fAirGapWidth + fRubberRealThickness + 1./3. * fAirGapWidth + 0.5 * fScintLayerThickness
    //          away from the iron plate surface
    G4double posZ = GapBetweenTwoIronPlates/2.  -   (fAirGapWidth + fRubberRealThickness + 1./3. * fAirGapWidth + 0.5 * fScintLayerThickness) ;

    //    G4cout<<" fAirGapWidth "<<fAirGapWidth<<G4endl;
    //    G4cout<<" fRubberRealThickness "<<fRubberRealThickness				<<"     sum "<<fAirGapWidth+fRubberRealThickness<<G4endl;
    //    G4cout<<" 1./3. * fAirGapWidth "<<1./3. * fAirGapWidth				<<"     sum "<<fAirGapWidth+fRubberRealThickness + 1./3. * fAirGapWidth<<G4endl;
    //    G4cout<<" 0.5 * fScintLayerThickness "<<0.5 * fScintLayerThickness	<<"     sum "<<fAirGapWidth+fRubberRealThickness + 1./3. * fAirGapWidth +0.5 * fScintLayerThickness <<G4endl;
    //    G4cout<<" GapBetweenTwoIronPlates/2. "<<GapBetweenTwoIronPlates/2.<<G4endl;
    //    G4cout<<" posZ "<<posZ<<G4endl;

    G4ThreeVector TransSpacer              (  0,               0,              posZ);
    G4ThreeVector TransOuterSpacerLeftUp   ( -fBoltPositionX,  fBoltPositionY, posZ);
    G4ThreeVector TransOuterSpacerRightUp  (  fBoltPositionX,  fBoltPositionY, posZ);
    G4ThreeVector TransOuterSpacerRightDown(  fBoltPositionX, -fBoltPositionY, posZ);
    G4ThreeVector TransOuterSpacerLeftDown ( -fBoltPositionX, -fBoltPositionY, posZ);

    G4UnionSolid* ScintLayerSpacer                = new G4UnionSolid("MUV1ScintillatorLayerWithSpacerBoxSV",            LayerBox,                       SpacerBox,        0, TransSpacer);
    G4UnionSolid* ScintLayerOuterSpacerLeftUp     = new G4UnionSolid("MUV1ScintillatorLayerOuterSpacerLeftUpSV",   		ScintLayerSpacer,               OuterSpacerTube,  0, TransOuterSpacerLeftUp);
    G4UnionSolid* ScintLayerOuterSpacerRightUp    = new G4UnionSolid("MUV1ScintillatorLayerOuterSpacerRightUpSV",  		ScintLayerOuterSpacerLeftUp,    OuterSpacerTube,  0, TransOuterSpacerRightUp);
    G4UnionSolid* ScintLayerOuterSpacerRightDown  = new G4UnionSolid("MUV1ScintillatorLayerOuterSpacerRighDownSV", 		ScintLayerOuterSpacerRightUp,   OuterSpacerTube,  0, TransOuterSpacerRightDown);
    G4UnionSolid* ScintLayerOuterSpacerLeftDown   = new G4UnionSolid("MUV1ScintillatorLayerOuterSpacerLeftDownSV", 		ScintLayerOuterSpacerRightDown, OuterSpacerTube,  0, TransOuterSpacerLeftDown);

    fSolidVolume = ScintLayerOuterSpacerLeftDown;


    // ***  VOLUMES TO BE SUBTRACTED ***

    // Define Beam Pipe Hole volume (a cylinder); enlarge by 1.01 to see a clean hole in Vis
    G4Tubs * BeamPipeHole = new G4Tubs("MUV1ScintillatorLayerBeamPipeHoleSV",
                                       0.,
                                       fHoleInnerRadius-0.05*fAirGapWidth, 	//a little bit smaller radius of the hole
									   	   	   	   	   	   	   	   	   	   //in order to contain the daughter ScintillatorSpacer volume w/o overlapping
                                       GapBetweenTwoIronPlates/2.,
                                       0.*deg,
                                       360.*deg);

    G4Tubs * ConnectionHole = new G4Tubs("MUV1ScintillatorLayerBoltHoleSV",
  				     	 	 	 	   0.,
  				     	 	 	 	   fConnectionHoleRadius,
  				     	 	 	 	   GapBetweenTwoIronPlates/2.,
  				     	 	 	 	   0.*deg,
  				     	 	 	 	   360.*deg);


    //Subtract the connection holes (4 times)
    G4VSolid * ScintillatorLayerConnectionHoleLeftUp = new G4SubtractionSolid("MUV1ScintillatorLayerBoltHoleLeftUpSV",
    							fSolidVolume,
    							ConnectionHole,
                                0,
                                G4ThreeVector(-fBoltPositionX,fBoltPositionY, posZ));

    G4VSolid * ScintillatorLayerConnectionHoleRightUp = new G4SubtractionSolid("MUV1ScintillatorLayerBoltHoleRightUpSV",
  		  	  	  	  	  		ScintillatorLayerConnectionHoleLeftUp,
    							ConnectionHole,
                                0,
                                G4ThreeVector(fBoltPositionX,fBoltPositionY, posZ));

    G4VSolid * ScintillatorLayerConnectionHoleRightDown = new G4SubtractionSolid("MUV1ScintillatorLayerBoltHoleRightDownSV",
  		  	  	  	  	    	ScintillatorLayerConnectionHoleRightUp,
    							ConnectionHole,
                                0,
                                G4ThreeVector(fBoltPositionX,-fBoltPositionY,posZ));

    G4VSolid * ScintillatorLayerConnectionHoleLeftDown = new G4SubtractionSolid("MUV1ScintillatorLayerBoltHoleLeftDownSV",
  		  	  	  	  	  		ScintillatorLayerConnectionHoleRightDown,
    							ConnectionHole,
                                0,
                                G4ThreeVector(-fBoltPositionX,-fBoltPositionY,posZ));


    // Subtract the Pipe Hole - the final volume
    G4VSolid * ScintLayerWithHole = new G4SubtractionSolid("MUV1ScintillatorLayerSV",
														   ScintillatorLayerConnectionHoleLeftDown,
                                                           BeamPipeHole,
                                                           0,
                                                           G4ThreeVector(0.,0.,posZ));

    // Scint layer logical volume
    fLogicalVolume= new G4LogicalVolume(ScintLayerWithHole,           // solid
                                        fMaterial,                   // material
                                        "MUV1ScintillatorLayer",              // name
                                        0,                           // field manager
                                        0,                           // sensitive detector
                                        0);                          // user limits

    // Scint layer physical volume
    fPhysicalVolume = new G4PVPlacement(0,                           // no rotation
                                        fScintLayerPosition,          // position
                                        fLogicalVolume,              // its logical volume
                                        "MUV1ScintillatorLayerPV",              // its name
                                        fMotherVolume,               // its mother  volume
                                        false,                       // no boolean operations
                                        fiCopy);                     // copy number


    // Material change for fast/full simulation
    G4Material* ScintMat = G4Material::GetMaterial("NonScint");
    //    G4Material* WLSMat = G4Material::GetMaterial("DummyWLSMatCore"); //TODO: see the comment bellow from 02.11.2017
    if(fFastSimulation=="false"){
        ScintMat = G4Material::GetMaterial("Scint");
        //        WLSMat = G4Material::GetMaterial("WLSMatCore"); //TODO: see the comment bellow from 02.11.2017
    }



    // Loop over all scintillators in this layer

    for (int scintillatorNumber = 1; scintillatorNumber <= Nscintillators; scintillatorNumber++){

        Channel++;

        G4double ScintDistanceFromCenter = fabs(scintillatorNumber - CentralScint);

        G4double ScintPosX = (scintillatorNumber < CentralScint ? -1. : 1.) * ( (2 * fScintWidthMiddle + 1.5 * fAirGapWidth) +
        																		(EndShortPosShift + ScintDistanceFromCenter - 4 - 3)* fScintWidthStandard +
        																		(EndShortPosShift + ScintDistanceFromCenter + 0.5 - 4 - 3)* fAirGapWidth      );

        G4double ScintPosY = 0 ; // Used for all scintillators with double-sided readout

        // Scintillators of type 1
        G4ThreeVector ScintillatorSize = G4ThreeVector(0.5*fScintWidthStandard,
                                                       0.5*fScintLengthStandard,
                                                       0.5*fScintillatorThickness);
        G4bool boolOpp = false;
        G4int Logical = 0;


        // Scintillators of type 2d, 2c,2b,2a on left side and right side
        if(ScintDistanceFromCenter > EndShortIndx)	{

        	ScintillatorSize = G4ThreeVector(0.5*fScintWidthStandard,
                                             0.5*fScintLengthOuter[(Nscintillators/2-1) - (G4int)ScintDistanceFromCenter],
                                             0.5*fScintillatorThickness);

        	if(ScintDistanceFromCenter > EndSpecShortIndx)	{//two shortest strips on the side, 2d and 2c, require special mother volumes
        		Logical = -2;
        	}
        }

        // scintillators with normal width but cut into two halves
        if (ScintDistanceFromCenter > 4 && ScintDistanceFromCenter < StartCuttingIndx ){

        	G4int scintMultiplier = (G4int)(ScintDistanceFromCenter/2) - 2;

            ScintPosX = (scintillatorNumber < CentralScint ? -1. : 1.) * (  (2. * fScintWidthMiddle + 1.5* fAirGapWidth) +
            																(fScintWidthStandard/2. + fAirGapWidth)      +
            																(fScintWidthStandard + fAirGapWidth) * scintMultiplier	);

            ScintPosY =  (scintillatorNumber%2 == Parity ?  -1. : 1.) * (0.25*fScintLengthStandard + (fAirGapWidth + fSkinWidth)/2. + fAirGapWidth/2.);

            ScintillatorSize = G4ThreeVector(0.5*fScintWidthStandard,
                                             0.25*fScintLengthStandard + (fAirGapWidth + fSkinWidth)/2.,
                                             0.5*fScintillatorThickness);

            if (scintillatorNumber%2 == Parity) Channel--;
            boolOpp = true;

        }

        //central narrow scintillators with the trapezoidal ending on the inner side
        if(ScintDistanceFromCenter > 2 && ScintDistanceFromCenter < 4){ //

            ScintPosX = (scintillatorNumber < CentralScint ? -1. : 1.) * (2* fScintWidthMiddle     - 0.5 * fScintWidthMiddle + fAirGapWidth + fAirGapWidth/2.);

            ScintPosY = (scintillatorNumber%2 == Parity ?  -1. : 1. )       * (0.5*fScintLengthStandard - 0.5 * fScintLengthMiddleOuter + (fAirGapWidth + fSkinWidth) + fAirGapWidth);

            ScintillatorSize = G4ThreeVector(0.5*fScintWidthMiddle,
                                             0.5*fScintLengthMiddleOuter,
                                             0.5*fScintillatorThickness);

            boolOpp = true;        // cut for beam hole
            Logical = (scintillatorNumber < CentralScint ? 2*(scintillatorNumber - (G4int)(CentralScint-3)) + 1 : 2*(scintillatorNumber - (G4int)(CentralScint+2)));         // cut corner

            if (scintillatorNumber%2 == Parity) Channel--;

        }

        //central narrow scintillators
        if(ScintDistanceFromCenter < 2){ //

            ScintPosX = (scintillatorNumber < CentralScint ? -1. : 1.) * (0.5 * fScintWidthMiddle + fAirGapWidth/2.);
            ScintPosY =  (scintillatorNumber%2 == Parity ? -1. : 1.) * ( 0.5*fScintLengthStandard - 0.5*fScintLengthMiddleStd + (fAirGapWidth + fSkinWidth) + fAirGapWidth);
            ScintillatorSize = G4ThreeVector(0.5*fScintWidthMiddle,
                                             0.5*fScintLengthMiddleStd,
                                             0.5*fScintillatorThickness);
            boolOpp = true;
            Logical = -1;

            if (scintillatorNumber%2 == Parity) Channel--;

        }


        G4double CopyNo = Channel +100*fiCopy+fOffset ; // Copy Number for scintillators


        G4ThreeVector ScintillatorPosition = G4ThreeVector(ScintPosX, ScintPosY, 0);

        G4ThreeVector BareScintillatorSize = G4ThreeVector(
        					ScintillatorSize.x()-fSkinWidth-fAirGapWidth,
							ScintillatorSize.y()-fSkinWidth-fAirGapWidth,
							ScintillatorSize.z()-fSkinWidth-fAirGapWidth);

        new MUV1Scintillator(0,
                             ScintMat,
                             fLogicalVolume,
                             BareScintillatorSize,
                             ScintillatorSize,
                             ScintillatorPosition,
                             boolOpp,
                             CopyNo,
                             Logical);

//02.11.2017
//        TODO: the stuff commented out bellow needs major improvements in order to fix the related geometrical problems with overlaps and extrusion.
//        Since it only matters for the full simulation, we can disable it for the moment because we have only fast simulation model of MUV1 working at the moment.
//        Improvements can/should be done later.

 /* ===========================================================================================================================>>
        G4RotationMatrix* g4rot = new G4RotationMatrix(CLHEP::HepRotationX(90.*deg)); //by gkh
        *g4rot = g4rot->inverse();

        G4ThreeVector fiberPosition;

        if(!boolOpp){

            G4cout <<"   !boolOpp"<<G4endl;

            for(int z=1;z<5;z++){

                fiberPosition[0] = ScintillatorPosition[0]+ ((z==1 || z==4) ? 1 : -1)* fGroovePositionInStandardScintillator ;
                fiberPosition[1] = ScintillatorPosition[1]+ ((z==3 || z==4) ? 1 : -1)* (ScintillatorSize.y()+ 0.5*fFiberHightOut);
                fiberPosition[2] = ScintillatorPosition[2]+ (ScintillatorSize.z() -1*fSkinWidth ) - 0.5*fGrooveDepth;

                G4cout <<"   z="<<z<<"   fiberPosition[0]="<<fiberPosition[0]<<"   fiberPosition[1]="<<fiberPosition[1]<<"   fiberPosition[2]="<<fiberPosition[2]<< G4endl;

                new  MUV1Fiber(WLSMat,
                               fLogicalVolume,
                               g4rot,
                               fFiberOuterRadius,
                               fFiberHightOut,
                               fiberPosition,
                               0);


                fiberPosition[1] = ScintillatorPosition[1]+ ((z==1 || z==2) ? 1 : -1)* (ScintillatorSize.y() -fSkinWidth + fFiberHightOut +  fPMTSize[1]) ;

                G4cout <<"      "<<"   MUV1PMT[1](fiberPosition[1])="<<fiberPosition[1]<< G4endl;

                new MUV1PMT(G4Material::GetMaterial("Air"),
                            fLogicalVolume,
                            fiberPosition,
                            fPMTSize,
                            z*10000 +  CopyNo);

            }

        }
        else{

            G4cout <<"   boolOpp"<<G4endl;

            for(int z=1;z<3;z++){

                fiberPosition[0] = ScintillatorPosition[0] + ((z==1) ? -1 : 1)* fGroovePositionInStandardScintillator ;
                fiberPosition[1] = ScintillatorPosition[1]+ ((ScintillatorPosition[1]>0) ? 1 : -1)* (ScintillatorSize.y() -fSkinWidth + 0.5*fFiberHightOut);
                fiberPosition[2] = ScintillatorPosition[2]+ (ScintillatorSize.z()-2*fSkinWidth) - 0.5*fGrooveDepth;

                new  MUV1Fiber(WLSMat,
                               fLogicalVolume,
                               g4rot,
                               fFiberOuterRadius,
                               fFiberHightOut,
                               fiberPosition,
                               0);

                G4cout <<"   z="<<z<<"   fiberPosition[0]="<<fiberPosition[0]<<"   fiberPosition[1]="<<fiberPosition[1]<<"   fiberPosition[2]="<<fiberPosition[2]<< G4endl;

                fiberPosition[1] = ScintillatorPosition[1]+ ((ScintillatorPosition[1]>0) ? 1 : -1)* (ScintillatorSize.y() -fSkinWidth + fFiberHightOut + fPMTSize[1]);

                G4cout <<"      "<<"   fiberPosition[1]="<<fiberPosition[1]<< G4endl;

                new MUV1PMT(G4Material::GetMaterial("Air"),
                            fLogicalVolume,
                            fiberPosition,
                            fPMTSize,
                            ((ScintillatorPosition[1]>0) ? z : z+2)*10000 + CopyNo);

            }

        }

    <<==============================================================================================================================================================
 */

    }



    //        ***         SPACERS         ***

    //Central Spacer
    new MUV1ScintillatorSpacer(G4Material::GetMaterial("G4_Fe"),
                               fLogicalVolume,
                               G4ThreeVector(0, 0, posZ),
                               0);

    //Left Up Spacer
    new MUV1OuterSpacer(G4Material::GetMaterial("G4_Fe"),
            			fLogicalVolume,
            			G4ThreeVector(-fBoltPositionX,fBoltPositionY, posZ),
            			 0);

    //Right Up Spacer
    new MUV1OuterSpacer(G4Material::GetMaterial("G4_Fe"),
            			fLogicalVolume,
            			G4ThreeVector(fBoltPositionX,fBoltPositionY, posZ),
            			 1);

    //Right Down Spacer
    new MUV1OuterSpacer(G4Material::GetMaterial("G4_Fe"),
            			fLogicalVolume,
            			G4ThreeVector(fBoltPositionX,-fBoltPositionY, posZ),
            			 2);

    //Left Down Spacer
    new MUV1OuterSpacer(G4Material::GetMaterial("G4_Fe"),
            			fLogicalVolume,
            			G4ThreeVector(-fBoltPositionX,-fBoltPositionY, posZ),
            			 3);

}

void MUV1ScintillatorLayer::SetProperties()
{
    // Set visualization properties
    fVisAtt= new G4VisAttributes(G4Colour(1.0, 0.0, 0.0, 1.0));
    fVisAtt -> SetVisibility(true);
    fLogicalVolume ->SetVisAttributes(fVisAtt);
}
