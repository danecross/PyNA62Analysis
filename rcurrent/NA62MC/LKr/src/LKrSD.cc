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
// Created by Francesca Bucci (Francesca.Bucci@cern.ch) 2008-04-29
//            Antonino Sergi (Antonino.Sergi@cern.ch) 
//            Evelina Marinova (Evelina.Marinova@cern.ch)
//
// --------------------------------------------------------------------
#include "LKrSD.hh"
#include "LKrHit.hh"
#include "G4VPhysicalVolume.hh"
#include "G4LogicalVolume.hh"
#include "G4Track.hh"
#include "G4ParticleDefinition.hh"
#include "G4VTouchable.hh"
#include "G4ios.hh"
#include "G4SDManager.hh"
#include "G4ParticleTypes.hh"
#include "G4VPVParameterisation.hh"
#include "LKrGeometryParameters.hh"
#include "NA62Global.hh"
#include "NA62ConditionsService.hh"

/// \class LKrSD 
/// \Brief
/// LKr sensitive detector class
/// \EndBrief
/// \Detailed
/// The information from the sensitive detectors (i.e. the single electrode cells) is stored in this class. The signal from the separate
/// geant4 hits is transformed into a signal of each cell here.
/// \EndDetailed

LKrSD::LKrSD(G4String name,G4String colName) :
  G4VSensitiveDetector(name), Collection(nullptr), nHits(0), HCID(0) {
    G4String HCname;
    collectionName.insert(HCname=colName);
    LKrGeometryParameters* GeoPars = LKrGeometryParameters::GetInstance();
    fZtr = GeoPars->GetZtr();
    fXbackReferenceCell = GeoPars->GetXbackReferenceCell();
    fYbackReferenceCell = GeoPars->GetYbackReferenceCell();
    fXfrontReferenceCell = GeoPars->GetXfrontReferenceCell();
    fYfrontReferenceCell = GeoPars->GetYfrontReferenceCell();
    fProjectivityPointPositionZ = GeoPars->GetProjectivityPointPositionZ();
    fProjectivityAxisProjectionZ = GeoPars->GetProjectivityAxisProjectionZ();
    fDistanceToNextElectrodeFrontX = GeoPars->GetDistanceToNextElectrodeFrontX();
    fDistanceToNextElectrodeBackX = GeoPars->GetDistanceToNextElectrodeBackX();
    fDistanceToNextElectrodeFrontY = GeoPars->GetDistanceToNextElectrodeFrontY();
    fDistanceToNextElectrodeBackY = GeoPars->GetDistanceToNextElectrodeBackY();
    fHalfNElectrodesX = GeoPars->GetHalfNElectrodesX();
    fHalfNElectrodesY  = GeoPars->GetHalfNElectrodesY();
    fNRowsFullNumberElectrodes = GeoPars->GetNRowsFullNumberElectrodes();
    fRHoleX = GeoPars->GetRHoleX();
    fRHoleY = GeoPars->GetRHoleY();
    fHalfZWidth   = GeoPars->GetHalfZWidth();
    fHalfYWidthF = GeoPars->GetHalfYWidthF();
    fHalfXWidthF = GeoPars->GetHalfXWidthF();
    fHalfYWidthB = GeoPars->GetHalfYWidthB();
    fHalfXWidthB = GeoPars->GetHalfXWidthB();
    fBackWallPositionZ = GeoPars->GetBackWallPositionZ();
    fDistanceFrontPlateBackPlate = GeoPars->GetDistanceFrontPlateBackPlate();
    fSpaceToNextPlate = GeoPars->GetSpaceToNextPlate();
    fHalfSizeFrontWallSegmentZ =  GeoPars->GetHalfSizeFrontWallSegmentZ();
    fHalfCellSizeAtFrontWall =  GeoPars->GetHalfCellSizeAtFrontWall();
    fTopRightCornerX = GeoPars->GetTopRightCornerX();
    fTopRightCornerY = GeoPars->GetTopRightCornerY();

    fLKrCellLength = GeoPars->GetLKrCellLength();

    fLKrCell0X= GeoPars->GetLKrCell0X();
    fLKrCell0Y= GeoPars->GetLKrCell0Y();
    fIncr= GeoPars->GetIncr();
    SpaceToNextPlate= GeoPars->GetSpaceToNextPlate();

    fParametrizedTrapezoid = new G4Trap("ParameterisedTrapezoid",1*cm, 0, 0, 1*cm, 1*cm,1*cm, 0, 1*cm, 1*cm,1*cm, 0);

    for(G4int i = 0; i < 16384; i++)
      if(HitMap[i] != 0)
        HitMap[i] = 0;
    // Tables from the old simulation, for the simple solution with no time dependence
    G4cout << "[LKrSD] Loading LKr signal formation files" << G4endl;
    Int_t iEntryGeVtoCurr1=0;
    std::ifstream GeVtoCurr1Constants
      (NA62ConditionsService::GetInstance()->GetFullPath(GeoPars->GetGeVtoCurrent1()));
    while(GeVtoCurr1Constants >> fGevtoCurr1[iEntryGeVtoCurr1]) iEntryGeVtoCurr1++;
    GeVtoCurr1Constants.close();
    Int_t iEntryGeVtoCurr2=0;
    std::ifstream GeVtoCurr2Constants
      (NA62ConditionsService::GetInstance()->GetFullPath(GeoPars->GetGeVtoCurrent2()));
    while(GeVtoCurr2Constants >> fGevtoCurr2[iEntryGeVtoCurr2]) iEntryGeVtoCurr2++;
    GeVtoCurr2Constants.close();
    Int_t iEntryGeVtoCurr3=0;
    std::ifstream GeVtoCurr3Constants
      (NA62ConditionsService::GetInstance()->GetFullPath(GeoPars->GetGeVtoCurrent3()));
    while(GeVtoCurr3Constants >> fGevtoCurr3[iEntryGeVtoCurr3]) iEntryGeVtoCurr3++;
    GeVtoCurr3Constants.close();

    if (!iEntryGeVtoCurr1) {
      G4cout << "[LKrSD] Error: Cannot read file "<<GeoPars->GetGeVtoCurrent1() << G4endl;
      exit(kGenericError);
    }
    if (!iEntryGeVtoCurr2) {
      G4cout << "[LKrSD] Error: Cannot read file "<<GeoPars->GetGeVtoCurrent2() << G4endl;
      exit(kGenericError);
    }
    if (!iEntryGeVtoCurr3) {
      G4cout << "[LKrSD] Error: Cannot read file "<<GeoPars->GetGeVtoCurrent3() << G4endl;
      exit(kGenericError);
    }
  }

LKrSD::~LKrSD() {}

void LKrSD::Initialize(G4HCofThisEvent*HCE) {
  static int HCID = -1;
  Collection = new LKrHitsCollection(SensitiveDetectorName,collectionName[0]);
  verboseLevel = 0;
  nHits=0;
  if(HCID<0)
  { HCID = GetCollectionID(0); }
  HCE->AddHitsCollection( HCID, Collection );
  for(G4int i = 0; i < 16384; i++)
    if(HitMap[i] != 0)
      HitMap[i] = 0;
}

G4bool LKrSD::ProcessHits(G4Step*aStep,G4TouchableHistory*) {
  G4double edep = aStep->GetTotalEnergyDeposit();
  if(edep == 0.) return false;

  G4TouchableHistory* TouchableHistory = (G4TouchableHistory*)(aStep->GetPreStepPoint()->GetTouchable());

  G4int TrackID = -1;
  if (aStep->GetTrack() != NULL)
    TrackID =  aStep->GetTrack()->GetTrackID();

  G4VPhysicalVolume * Channel = TouchableHistory->GetVolume(0);

  if(edep < 0.00005 *keV) {
    return false;}

  /// HITS
  G4ThreeVector HitPosition = aStep->GetPreStepPoint()->GetPosition();

  // G4cout << "edep " << edep << "  dist  "<< sqrt((HitPosition.x() - 317.07299551978)*(HitPosition.x() - 317.07299551978)+  (HitPosition.y() - 252.83939736958)* (HitPosition.y() - 252.83939736958) )<< G4endl;
  //translation of the trapezoids in the laboratory frame ///check how many parent volumes exist if we change the mother-volume-scheme

  G4ThreeVector Translation_Center_Trapezoid = Channel->GetObjectTranslation();
  G4ThreeVector Total_Translation_Center_Trapezoid = Channel->GetObjectTranslation() + TouchableHistory->GetVolume(1)->GetObjectTranslation() 
    + TouchableHistory->GetVolume(2)->GetObjectTranslation() + TouchableHistory->GetVolume(3)->GetObjectTranslation();

  //Check how the shower develops longitudinally
  G4double LongitudinalSliceThickness = 1 *cm;
  G4int LongitudinalIndex = (HitPosition.z() -  Total_Translation_Center_Trapezoid.z() + fDistanceFrontPlateBackPlate/2 -  fHalfSizeFrontWallSegmentZ) 
    /  LongitudinalSliceThickness;

  // find the position of the hits wrt the zigzags - left or right?
  G4LogicalVolume * LogicalChannel = Channel->GetLogicalVolume();
  G4int IndexDaughterVolumes;

  //get solid,
  //-------------------------------------------------------
  G4Trap * Trapezoid =(G4Trap*) LogicalChannel->GetSolid();
  G4int IndexElectrode = (HitPosition.z() -  Total_Translation_Center_Trapezoid.z() + fDistanceFrontPlateBackPlate/2 -  fHalfSizeFrontWallSegmentZ) /  (fSpaceToNextPlate);

  IndexElectrode -= (IndexElectrode == 6 ? 1 : 0);

  // the daughter volumes follow the order: first 7 are vertical, horizontal up - 7, horizontal down - 7, 
  // 7 small segments in the spacer plate holes, last 6 are zig zag electrodes (34 elements in total)

  IndexDaughterVolumes = IndexElectrode + 28;
  //------------------------------------ IndexDaughterVolumes---------------------------

  fParametrizedTrapezoid = (G4Trap*) LogicalChannel->GetDaughter(0)->GetLogicalVolume()->GetSolid(); 

  // use the total translation of the electrodes to compare with the hit numbers...
  G4Trap & ElectrodeRef = *fParametrizedTrapezoid;
  G4Trap * Electrode = fParametrizedTrapezoid;

  //compute dimensions and translations of the zig zags
  LogicalChannel->GetDaughter(0)->GetParameterisation()->ComputeDimensions(ElectrodeRef, IndexDaughterVolumes, LogicalChannel->GetDaughter(0));
  LogicalChannel->GetDaughter(0)->GetParameterisation()->ComputeTransformation(IndexDaughterVolumes, LogicalChannel->GetDaughter(0));

  //---------------------------------------------------------------
  // translation of the electrode in laboratory frame 

  G4ThreeVector TotalTranslationElectrode =  LogicalChannel->GetDaughter(0)->GetObjectTranslation() + Channel->GetObjectTranslation() 
    + TouchableHistory->GetVolume(1)->GetObjectTranslation() + TouchableHistory->GetVolume(2)->GetObjectTranslation() 
    + TouchableHistory->GetVolume(3)->GetObjectTranslation();

  G4double XofElectrodeAtZofHit = TotalTranslationElectrode.x() + Electrode->GetSymAxis().x() / Electrode->GetSymAxis().z() 
    * ( HitPosition.z() - TotalTranslationElectrode.z()); 

  G4double YofElectrodeAtZofHit = TotalTranslationElectrode.y() + Electrode->GetSymAxis().y() / Electrode->GetSymAxis().z() 
    * ( HitPosition.z() - TotalTranslationElectrode.z());
  //if the electrode is anode - project the hit at the center of the front face, if the electrode is a cathod - 
  // project the hit at the center of the front face of the neighbouring anode - left or right depending on which side of the zigzag is the hit

  // project the hits on the front plate(center of the anode) , between a vertical wall bar and a spacer plate hole (= center of zig - zags)

  G4double XatFrontOfLKrCell = Channel->GetObjectTranslation().x() + TouchableHistory->GetVolume(1)->GetObjectTranslation().x() 
    + Trapezoid ->GetSymAxis().x() / Trapezoid->GetSymAxis().z() * (Channel->GetObjectTranslation().z() - fDistanceFrontPlateBackPlate * 0.5);

  //start building a fine grid inside one LKR cell
  // size of cell at Z of the hit
  G4double SizeCellatZofHit = 2 * fHalfCellSizeAtFrontWall * pow(fIncr,(fHalfSizeFrontWallSegmentZ +  LongitudinalIndex/SpaceToNextPlate));

  //  distance of the hit to the electrode.
  G4double  DistanceHitToElectrode = HitPosition.x() -  XofElectrodeAtZofHit; 
  G4double  DistanceHitToElectrodeY = HitPosition.y() -  YofElectrodeAtZofHit;

  ///------------------make a map of the anode X coordinates-----------      
  //the reference cell is the central cathode:

  G4double XatFrontRefCell = (fZtr - 0.5 * fDistanceFrontPlateBackPlate - fProjectivityPointPositionZ) * fXbackReferenceCell / fProjectivityAxisProjectionZ;

  //  now, find the distance of the hit to the appropriate anode!!
  //  pay attention in which cathode you are.

  G4double DistanceHitToAnode = 0.;

  // even - anode, odd - cathode
  G4double Tolerance = 0.3; 
  G4int AnodeCathodeSeparation = abs (XatFrontOfLKrCell - XatFrontRefCell ) /  fDistanceToNextElectrodeFrontX + Tolerance;

  ///------------------------------------------------------------------

  if( AnodeCathodeSeparation %2 == 0){ 
    if ( HitPosition.x() - XofElectrodeAtZofHit > 0.00000)
    {
      // go to left volume number grows on the left 
      XatFrontOfLKrCell = XatFrontOfLKrCell + fDistanceToNextElectrodeFrontX ;
      //  G4cout << " left" <<G4endl;
      DistanceHitToAnode =  -(0.5*SizeCellatZofHit - DistanceHitToElectrode) ; 
      //this is the absolute value if you're inside a cathode. Pay attention what sign you'll assign
    }
    else
    {
      // go to right volume number decreases on the right ;
      XatFrontOfLKrCell = XatFrontOfLKrCell  - fDistanceToNextElectrodeFrontX; 
      //  G4cout << " right " <<G4endl;
      DistanceHitToAnode =  (0.5*SizeCellatZofHit + DistanceHitToElectrode);
    }
  }
  if( AnodeCathodeSeparation %2 == 1)
  {
    //	G4cout << " anode " <<G4endl;
    DistanceHitToAnode =  DistanceHitToElectrode ; // for anode cells	
  }

  //  build a fine grid in X and Y inside the LKr cell - from cathode to cathode
  //  as the cell size varies with Z position due to projective geometry
  //  lets divide the cell in 100 slices in X and in 50 slices in Y like it was done in the old simulation.
  //  The idea is to create a fine grid with the energy deposition in fine grid.
  // 
  //  XGapOfCellAtZofHit = fSizeCellatZofHit / 100;
  //  YGapOfCellAtZofHit = fSizeCellatZofHit / 50;
  // 
  // starting from small to big X and Y COUNT UP ....testing..... 

  Double_t SignOfDistance = DistanceHitToAnode/fabs(DistanceHitToAnode);

  if(fabs(DistanceHitToAnode) > 0.5*SizeCellatZofHit) DistanceHitToAnode = SignOfDistance* 0.5*SizeCellatZofHit;

  G4int  indY = (G4int)(0.5 * SizeCellatZofHit + DistanceHitToElectrodeY)/ SizeCellatZofHit*50;
  // 
  G4int  indX = (G4int)(0.5 * SizeCellatZofHit +  DistanceHitToAnode )/ SizeCellatZofHit*100;

  // starting from small to big X and Y COUNT DOWN ....testing.....
  //    G4int  indY = (G4int)((0.5 * SizeCellatZofHit - HitPositionInCell.y())/ SizeCellatZofHit*50);
  // 
  //    G4int  indX = (G4int)((0.5 * SizeCellatZofHit -  DistanceHitToAnode )/ SizeCellatZofHit*100);
  //
  // organize container for the energies in the grid -- > longitudinal contains 3d matrix now

  // G4double  EnergyInCellGrid[100][50];

  // EnergyInCellGrid[indX][indY] =  edep; // check if this is summed.

  //-------------calculate coordinates at front plate of the anode ------------------
  G4double XatFrontOfLKrCell_Anode = XatFrontOfLKrCell;
  G4double YatFrontOfLKrCell_Anode = Channel->GetObjectTranslation().y() + TouchableHistory->GetVolume(1)->GetObjectTranslation().y()
    + Trapezoid->GetSymAxis().y() / Trapezoid->GetSymAxis().z() * (Channel->GetObjectTranslation().z() - fDistanceFrontPlateBackPlate * 0.5); 

  G4double ZatFrontOfLKrCell_Anode = Channel->GetObjectTranslation().z() + TouchableHistory->GetVolume(1)->GetObjectTranslation().z() 
    + TouchableHistory->GetVolume(2)->GetObjectTranslation().z() + TouchableHistory->GetVolume(3)->GetObjectTranslation().z() - fDistanceFrontPlateBackPlate*0.5;

  //----------------------remove fake solutions for anodes close to the borders-----------
  // around the beam hole  
  G4double  rHitAtFront = sqrt((XatFrontOfLKrCell_Anode - fXfrontReferenceCell) * (XatFrontOfLKrCell_Anode - fXfrontReferenceCell) + YatFrontOfLKrCell_Anode * YatFrontOfLKrCell_Anode);
  G4double RadiusAnodesHole = 10.5*cm; //effective value to cut fake values for anodes around the beam pipe !check what happens when adding the material contraction

  if ( (rHitAtFront ) < RadiusAnodesHole ) 
  {
    return false;
  }     

  // outside the regular lines ? keep that condition for the central lines, otherwise the short lines around the beam pipe might create problems!!!
  if(XatFrontOfLKrCell_Anode > ((fHalfNElectrodesX-1) * fDistanceToNextElectrodeFrontX + XatFrontRefCell ) || XatFrontOfLKrCell_Anode < (- (fHalfNElectrodesX-1) * fDistanceToNextElectrodeFrontX + XatFrontRefCell ) ){
    return false;
  }

  G4Trap * Line = (G4Trap*) Channel->GetMotherLogical()->GetSolid();
  G4double HalfNElectrodesX  = Line->GetXHalfLength4()/10;

  /// cut the edges of the octagon
  if((fabs(YatFrontOfLKrCell_Anode/10) >= fNRowsFullNumberElectrodes * 2 * fDistanceToNextElectrodeFrontY + 1)&& 
      (( XatFrontOfLKrCell_Anode > (HalfNElectrodesX* fDistanceToNextElectrodeFrontX + XatFrontRefCell))  ||  ( XatFrontOfLKrCell_Anode <(- HalfNElectrodesX* fDistanceToNextElectrodeFrontX + XatFrontRefCell)))) 
  { 
    return false;
  }

  //---------project the hit at the front plate-----  

  G4ThreeVector HitPositionNew;
  HitPositionNew.setX(XatFrontOfLKrCell_Anode);
  HitPositionNew.setY(YatFrontOfLKrCell_Anode);
  HitPositionNew.setZ(ZatFrontOfLKrCell_Anode);

  // Get xCell,yCell from HitPositionNew
  G4int xCell = abs(fTopRightCornerX-HitPositionNew.x())/fLKrCellLength;
  G4int yCell = 128-abs(fTopRightCornerY-HitPositionNew.y())/fLKrCellLength; 
  G4int HitMapChannel = xCell*128 +yCell;

  //G4double *** grid;
  //if (HitMap[HitMapChannel] == 0 || aStep->GetPreStepPoint()->GetGlobalTime() - HitMap[HitMapChannel]->GetTime() > 3000*ns)
  if (HitMap[HitMapChannel] == 0) {
    HitMap[HitMapChannel] = new LKrHit;
    HitMap[HitMapChannel]->SetChannelID(1000.*xCell+yCell);
    HitMap[HitMapChannel]->SetTrackID(TrackID);
    HitMap[HitMapChannel]->SetTime(aStep->GetPreStepPoint()->GetGlobalTime());
    HitMap[HitMapChannel]->SetPosition(HitPositionNew);
    HitMap[HitMapChannel]->SetCurrent(0);
    Collection->insert( HitMap[HitMapChannel] );
    nHits++;
  }
  // Use first particle generating a signal as the reference for the hit properties  
  // NB: current should not be reset to 0!
  if(aStep->GetPreStepPoint()->GetGlobalTime() < HitMap[HitMapChannel]->GetTime()){
    HitMap[HitMapChannel]->SetChannelID(1000.*xCell+yCell);
    HitMap[HitMapChannel]->SetTrackID(TrackID);
    HitMap[HitMapChannel]->SetTime(aStep->GetPreStepPoint()->GetGlobalTime());
    HitMap[HitMapChannel]->SetPosition(HitPositionNew);
  }
  if(edep > HitMap[HitMapChannel]->GetMaxEnergyDeposit()){
    HitMap[HitMapChannel]->SetTrackID(TrackID);
    HitMap[HitMapChannel]->SetMaxEnergyDeposit(edep);
  }

  // 2d map
  //  HitMap[HitMapChannel]->AddEnergy(edep, indX, indY); 
  //  G4cout << "edep" << edep <<" indX "<< indX <<" indY "<<indY <<G4endl;
  // 3d map
  HitMap[HitMapChannel]->AddEnergy(edep, indX, indY, LongitudinalIndex); 
  Int_t idCurr = indY*100+indX;
  G4double current = HitMap[HitMapChannel]->GetCurrent();
  HitMap[HitMapChannel]->SetCurrent(current+edep*(fGevtoCurr1[idCurr]+fGevtoCurr2[idCurr]+fGevtoCurr3[idCurr]));
  //	if((indX <= 0))//||(indY>= 50)) 
  //	  { G4cout << "edep" << edep <<" indX "<< indX <<" indY "<<indY << " LongitudinalIndex " << LongitudinalIndex <<G4endl;
  //	}
  return true;
}

void LKrSD::EndOfEvent(G4HCofThisEvent*) {}

void LKrSD::clear() {}

void LKrSD::DrawAll() {} 

void LKrSD::PrintAll() {}
