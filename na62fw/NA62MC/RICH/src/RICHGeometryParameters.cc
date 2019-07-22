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
//
// Updated by Francesca Bucci to the 2011-02-23 beatch file
// (francesca.bucci@cern.ch) 2011-03-11
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2008-03-30
//
// --------------------------------------------------------------
#include "TVector.h"
#include "TMath.h"

#include "RICHGeometryParameters.hh"
#include "DetectorParameter.hh"

RICHGeometryParameters* RICHGeometryParameters::fInstance = 0;

RICHGeometryParameters::RICHGeometryParameters() : NA62VGeometryParameters(G4String("RICH")) {

  fNChannels = 2*2048;


  ////////////////////////////////////////////////////////////////
  // RICH responsibility region: between STRAW RR and NewCHOD RR

  //  fRespRegionZStart      = 219.546*m;
  //  fRespRegionZEnd        = 237.588*m;// 237.700*m;

  fRespRegionZStart      = 219.603*m;
  fRespRegionZEnd        = 237.588*m;// 237.700*m;
  fResponsibilityRegion.push_back(new ResponsibilityRegion(fRespRegionZStart,fRespRegionZEnd));

  fRICHDetectorZStart    = 219.600*m;
  fRICHDetectorZEnd      = 237.578*m;
  fRICHDetectorZPosition = 0.5*(fRICHDetectorZStart+fRICHDetectorZEnd);

  fRICHDetectorZLength = fRICHDetectorZEnd-fRICHDetectorZStart;
  fRICHDetectorXLength = 4.0*m;
  fRICHDetectorYLength = 4.0*m;

  fMirrorWindowZLength = 252.*mm;
  fMirrorWindowThickness = 4.*mm;
  fMirrorWindowInnerRadius = 90.*mm;
  fMirrorWindowOuterRadius = 1400.*mm;
  fMirrorWindowZPosition = fRICHDetectorZEnd-fMirrorWindowZLength-fRICHDetectorZPosition;
  fMirrorWindowOuterFlangeZLength = 245.*mm;
  fMirrorWindowOuterFlangeInnerRadius = fMirrorWindowOuterRadius;
  fMirrorWindowOuterFlangeRadialThickness = 110.*mm;
  fMirrorWindowOuterFlangeZPosition = fRICHDetectorZEnd-fMirrorWindowOuterFlangeZLength*0.5-fRICHDetectorZPosition; // RICH ref syst

  fInterfaceRingZLength = 80.*mm;
  fInterfaceRingRadialThickness = 260.*mm;
  fInterfaceRingInnerRadius = fMirrorWindowOuterFlangeInnerRadius;
  fInterfaceRingZPosition = fMirrorWindowOuterFlangeZPosition-fMirrorWindowOuterFlangeZLength*0.5-fInterfaceRingZLength*0.5; // RICH ref syst

  fNVesselSections = 4;
  fVesselRadialThickness = 14.*mm;
  fVesselZLength = 0;

  fVesselSectionZLength[0] = 3.480*m;
  fVesselSectionZLength[1] = 3.700*m;
  fVesselSectionZLength[2] = 4.920*m;
  fVesselSectionZLength[3] = 4.903*m;

  for(G4int iSection = 0; iSection < fNVesselSections; iSection++) fVesselZLength += fVesselSectionZLength[iSection];

  fVesselZPosition = fInterfaceRingZPosition - fInterfaceRingZLength*0.5 - fVesselZLength*0.5; // RICH ref syst

  //G4cout<<"//////// Vessel Z Position: "<<fVesselZPosition<<" ////////"<<G4endl;

  fVesselSectionInnerRadius[0] = 1.94*m;
  fVesselSectionInnerRadius[1] = 1.81*m;
  fVesselSectionInnerRadius[2] = 1.676*m;
  fVesselSectionInnerRadius[3] = 1.625*m;

  fVesselSectionPressureFlangeZLength[1] = 50.*mm;
  fVesselSectionPressureFlangeRadialThickness[1] = 120.*mm;
  fVesselSectionPressureFlangeInnerRadius[1] = fVesselSectionInnerRadius[1]+fVesselRadialThickness;
  fVesselSectionPressureFlangeZShift[1] = 0.*mm;
  fVesselSectionPressureFlangeZPosition[1] = fVesselZPosition-fVesselZLength *0.5 + fVesselSectionZLength[0]+fVesselSectionPressureFlangeZShift[1]+fVesselSectionPressureFlangeZLength[1]*0.5;

  fVesselSectionPressureFlangeZLength[2] = 50.*mm;
  fVesselSectionPressureFlangeRadialThickness[2] = 120.*mm;
  fVesselSectionPressureFlangeInnerRadius[2] = fVesselSectionInnerRadius[2]+fVesselRadialThickness;
  fVesselSectionPressureFlangeZShift[2] = 0.*mm;
  fVesselSectionPressureFlangeZPosition[2] = fVesselZPosition-fVesselZLength *0.5 + fVesselSectionZLength[0]+fVesselSectionZLength[1]+fVesselSectionPressureFlangeZShift[2]+fVesselSectionPressureFlangeZLength[2]*0.5;

  fVesselSectionPressureFlangeZLength[3] = 50.*mm;
  fVesselSectionPressureFlangeRadialThickness[3] = 120.*mm;
  fVesselSectionPressureFlangeInnerRadius[3] = fVesselSectionInnerRadius[3]+fVesselRadialThickness;
  fVesselSectionPressureFlangeZShift[3] = 0.*mm;
  fVesselSectionPressureFlangeZPosition[3] = fVesselZPosition-fVesselZLength *0.5 + fVesselSectionZLength[0]+fVesselSectionZLength[1]+fVesselSectionZLength[2]+fVesselSectionPressureFlangeZShift[3]+fVesselSectionPressureFlangeZLength[3]*0.5;

  fVesselSectionUpstreamFlangeZLength[0] = 80.*mm-0.1*mm;
  fVesselSectionUpstreamFlangeRadialThickness[0] = 196.*mm;
  fVesselSectionUpstreamFlangeInnerRadius[0] = fVesselSectionInnerRadius[0]+fVesselRadialThickness;
  fVesselSectionUpstreamFlangeZShift[0] = 0.*mm;
  fVesselSectionUpstreamFlangeZPosition[0] = fVesselZPosition-fVesselZLength *0.5+fVesselSectionUpstreamFlangeZShift[0]+fVesselSectionUpstreamFlangeZLength[0]*0.5;

  fVesselSectionUpstreamFlangeZLength[1] = 60.*mm;
  fVesselSectionUpstreamFlangeRadialThickness[1] = 200.*mm;
  fVesselSectionUpstreamFlangeInnerRadius[1] = fVesselSectionInnerRadius[1]+fVesselRadialThickness;
  fVesselSectionUpstreamFlangeZShift[1] = 105.*mm;
  fVesselSectionUpstreamFlangeZPosition[1] = fVesselZPosition-fVesselZLength *0.5 + fVesselSectionZLength[0]+fVesselSectionUpstreamFlangeZShift[1]+fVesselSectionUpstreamFlangeZLength[1]*0.5;

  fVesselSectionUpstreamFlangeZLength[2] = 60.*mm;
  fVesselSectionUpstreamFlangeRadialThickness[2] = 200.*mm;
  fVesselSectionUpstreamFlangeInnerRadius[2] = fVesselSectionInnerRadius[2]+fVesselRadialThickness;
  fVesselSectionUpstreamFlangeZShift[2] = 105.*mm;
  fVesselSectionUpstreamFlangeZPosition[2] = fVesselZPosition-fVesselZLength *0.5 + fVesselSectionZLength[0]+fVesselSectionZLength[1]+fVesselSectionUpstreamFlangeZShift[2]+fVesselSectionUpstreamFlangeZLength[2]*0.5;

  fVesselSectionUpstreamFlangeZLength[3] = 60.*mm;
  fVesselSectionUpstreamFlangeRadialThickness[3] = 200.*mm;
  fVesselSectionUpstreamFlangeInnerRadius[3] = fVesselSectionInnerRadius[3]+fVesselRadialThickness;
  fVesselSectionUpstreamFlangeZShift[3] = 105.*mm;
  fVesselSectionUpstreamFlangeZPosition[3] = fVesselZPosition-fVesselZLength *0.5 + fVesselSectionZLength[0]+fVesselSectionZLength[1]+fVesselSectionZLength[2]+fVesselSectionUpstreamFlangeZShift[3]+fVesselSectionUpstreamFlangeZLength[3]*0.5;

  fVesselSectionDownstreamFlangeZLength[0] = 90.*mm;
  fVesselSectionDownstreamFlangeRadialThickness[0] = 216.*mm;
  fVesselSectionDownstreamFlangeInnerRadius[0] = fVesselSectionInnerRadius[1];
  fVesselSectionDownstreamFlangeZPosition[0] = fVesselZPosition-fVesselZLength *0.5+fVesselSectionZLength[0]-fVesselSectionDownstreamFlangeZLength[0]*0.5;

  fVesselSectionDownstreamFlangeZLength[1] = 90.*mm;
  fVesselSectionDownstreamFlangeRadialThickness[1] = 216.*mm;
  fVesselSectionDownstreamFlangeInnerRadius[1] = fVesselSectionInnerRadius[2];
  fVesselSectionDownstreamFlangeZPosition[1] = fVesselZPosition-fVesselZLength *0.5+fVesselSectionZLength[0]+fVesselSectionZLength[1]-fVesselSectionDownstreamFlangeZLength[1]*0.5;

  fVesselSectionDownstreamFlangeZLength[2] = 90.*mm;
  fVesselSectionDownstreamFlangeRadialThickness[2] = 216.*mm;
  fVesselSectionDownstreamFlangeInnerRadius[2]=fVesselSectionInnerRadius[3];
  fVesselSectionDownstreamFlangeZPosition[2] = fVesselZPosition-fVesselZLength*0.5+fVesselSectionZLength[0]+fVesselSectionZLength[1]+fVesselSectionZLength[2]-fVesselSectionDownstreamFlangeZLength[2]*0.5;


  fVesselSectionDownstreamFlangeZLength[3] = 90.*mm;
  fVesselSectionDownstreamFlangeRadialThickness[3] = 200.*mm;
  fVesselSectionDownstreamFlangeInnerRadius[3] = 1577.*mm;
  fVesselSectionDownstreamFlangeZPosition[3] = fVesselZPosition-fVesselZLength*0.5+fVesselSectionZLength[0]+fVesselSectionZLength[1]+fVesselSectionZLength[2]+fVesselSectionZLength[3]-fVesselSectionDownstreamFlangeZLength[3]*0.5;


  fConicalWindowOuterFlangeZLength = 70.*mm;
  fConicalWindowOuterFlangeInnerRadius = 1980.*mm;
  fConicalWindowOuterFlangeRadialThickness = 170.*mm;
  fConicalWindowOuterFlangeZPosition = fVesselZPosition - fVesselZLength*0.5 - fConicalWindowOuterFlangeZLength*0.5; // RICH ref syst

  fConicalWindowZLength = 240.*mm; // 420.-70.-110.*mm
  fConicalWindowThickness = 12.*mm;
  fConicalWindowOuterRadius = fConicalWindowOuterFlangeInnerRadius;
  fConicalWindowInnerRadius = 1200.*mm;
  fConicalWindowZPosition = fConicalWindowOuterFlangeZPosition-fConicalWindowZLength*0.5-fConicalWindowOuterFlangeZLength*0.5; // RICH ref syst


  fConicalWindowInnerFlangeZLength = 110.*mm;
  fConicalWindowInnerFlangeInnerRadius = 1100;
  fConicalWindowInnerFlangeRadialThickness = fConicalWindowInnerRadius-fConicalWindowInnerFlangeInnerRadius;
  fConicalWindowInnerFlangeZPosition = fConicalWindowZPosition-fConicalWindowZLength*0.5-fConicalWindowInnerFlangeZLength*0.5; //RICH ref syst


  fPMTsTubeZLength = 377*mm;
  fPMTsTubeCenter = new G4TwoVector(1.6*m,0.*m);
  fPMTsTubeInnerRadius = 347.5*mm;
  fPMTsTubeRadialThickness = 8*mm;
  fPMTsTubeZPosition = fVesselZPosition - fVesselZLength*0.5 - fPMTsTubeZLength*0.5; //RICH ref syst

  fPMTsDiskInnerRadius = 0;
  fPMTsDiskOuterRadius = 360*mm;
  fPMTsDiskZLength = 23*mm;
  fPMTsDiskCenter = new G4TwoVector(1.6*m,0.*m);
  fPMTsDiskZPosition = fPMTsTubeZPosition-fPMTsTubeZLength*0.5-fPMTsDiskZLength*0.5; //RICH ref syst

  fBeamWindowOuterFlangeZLength = 112*mm;
  fBeamWindowOuterFlangeInnerRadius = fConicalWindowInnerFlangeInnerRadius;
  fBeamWindowOuterFlangeRadialThickness = fConicalWindowInnerFlangeRadialThickness;
  fBeamWindowOuterFlangeZShift = 49.*mm;
  fBeamWindowOuterFlangeZPosition = fConicalWindowInnerFlangeZPosition - fConicalWindowInnerFlangeZLength*0.5 - fBeamWindowOuterFlangeZLength*0.5;

  fBeamWindowZLength = 151*mm;
  fBeamWindowOuterRadius = fBeamWindowOuterFlangeInnerRadius;
  fBeamWindowInnerRadius = 121 *mm; // 85.+36.*mm
  fBeamWindowThickness = 2*mm;
  fBeamWindowZPosition = fBeamWindowOuterFlangeZPosition + fBeamWindowOuterFlangeZLength*0.5 - fBeamWindowOuterFlangeZShift - fBeamWindowZLength; // posizione dell'estremo sinistro della calotta nel RICH ref

  fBeamWindowInnerFlangeZLength = 27*mm;
  fBeamWindowInnerFlangeZShift = 10*mm;
  fBeamWindowInnerFlangeZPosition = fBeamWindowZPosition -fBeamWindowInnerFlangeZLength*0.5 + fBeamWindowInnerFlangeZShift; // RICH ref

  fMirrorWindowInnerFlangeZLength = 40.*mm;
  fMirrorWindowInnerFlangeZPosition = fRICHDetectorZEnd-fMirrorWindowZLength-fRICHDetectorZPosition; // RICH ref syst
  //G4cout<<" *********** fMirrorWindowInnerFlangeZLength: "<<fMirrorWindowInnerFlangeZLength <<" ***********"<<G4endl;
  //G4cout<<" ***********fMirrorWindowInnerFlangeZPosition: "<<fMirrorWindowInnerFlangeZPosition<<" ***********"<<G4endl;

  // ------ Beam pipes -------- //
  G4double BeamPipeFinOuterRadius = 89.0*mm;
  G4double BeamPipeOuterRadiusNominal = 85.0*mm;
  G4double BeamPipeInnerRadiusNominal = 84.0*mm;

  //no fins, beam pipe part before beam window flange
  fBeamPipeZLength[0] = 10*mm;
  fBeamPipeOuterRadius[0] = BeamPipeOuterRadiusNominal + 1.0*mm;// 86*mm according to Niels file
  fBeamPipeInnerRadius[0] = BeamPipeInnerRadiusNominal;
  fBeamPipeZPosition[0] = fBeamWindowInnerFlangeZPosition - 0.5*fBeamWindowInnerFlangeZLength - 0.5*fBeamPipeZLength[0];
  fBeamPipeFinZLength[0] = 5.0*mm;
  fBeamPipeFinOuterRadius[0] = fBeamPipeOuterRadius[0];
  fBeamPipeFinSpacing[0] = 40.0*mm;
  fBeamPipeZLengthWFins[0] = 0*mm;
  fBeamPipeInputDisplacementWRTBeam[0]=0;
  fBeamPipeOutputDisplacementWRTBeam[0]=0; 

  //with fins, between [1] and [3], continues beam pipe inside radiator volume
  fBeamPipeZLength[2] = fVesselZPosition - fBeamWindowOuterFlangeZPosition - 0.5*fVesselZLength- 0.5*fBeamWindowOuterFlangeZLength;
  fBeamPipeOuterRadius[2] = BeamPipeOuterRadiusNominal;
  fBeamPipeInnerRadius[2] = BeamPipeInnerRadiusNominal;
  fBeamPipeZPosition[2] = fBeamWindowOuterFlangeZPosition+0.5*fBeamWindowOuterFlangeZLength+0.5*fBeamPipeZLength[2];
  fBeamPipeFinZLength[2] = 5.0*mm;
  fBeamPipeFinOuterRadius[2] = BeamPipeFinOuterRadius;
  fBeamPipeFinSpacing[2] = 40.0*mm;
  fBeamPipeZLengthWFins[2] = fBeamPipeZLength[2];
  fBeamPipeInputDisplacementWRTBeam[2]=0;
  fBeamPipeOutputDisplacementWRTBeam[2]=0;  

  //no fins, beam pipe part inside and before beam window until radiator volume
  fBeamPipeZLength[1] = fBeamPipeZPosition[2] - fBeamWindowInnerFlangeZPosition - 0.5*fBeamPipeZLength[2] + 0.5*fBeamWindowInnerFlangeZLength;
  fBeamPipeOuterRadius[1] = BeamPipeOuterRadiusNominal;
  fBeamPipeInnerRadius[1] = BeamPipeInnerRadiusNominal;
  fBeamPipeZPosition[1] = fBeamWindowInnerFlangeZPosition+(0.5*fBeamPipeZLength[1]-0.5*fBeamWindowInnerFlangeZLength);
  fBeamPipeFinZLength[1] = 5.0*mm;
  fBeamPipeFinOuterRadius[1] = fBeamPipeOuterRadius[1];
  fBeamPipeFinSpacing[1] = 40.0*mm;
  fBeamPipeZLengthWFins[1] = 0*mm;
  fBeamPipeInputDisplacementWRTBeam[1]=0;
  fBeamPipeOutputDisplacementWRTBeam[1]=0; 

  //with fins, beam pipe inside radiator volume
  fBeamPipeZLength[3] = fVesselZLength;
  fBeamPipeOuterRadius[3] = BeamPipeOuterRadiusNominal;
  fBeamPipeInnerRadius[3] = BeamPipeInnerRadiusNominal;
  fBeamPipeZPosition[3] = fVesselZPosition;
  fBeamPipeFinZLength[3] = 5.0*mm;
  fBeamPipeFinOuterRadius[3] = BeamPipeFinOuterRadius;
  fBeamPipeFinSpacing[3] = 40.0*mm;
  fBeamPipeZLengthWFins[3] = fBeamPipeZLength[3];
  fBeamPipeInputDisplacementWRTBeam[3] = 0;
  fBeamPipeOutputDisplacementWRTBeam[3] = 0;

  //no fins, beam pipe part outside radiator downstream up to end of mirror window
  fBeamPipeZLength[4] = 2.0*(fMirrorWindowInnerFlangeZPosition -fVesselZPosition - 0.5*fVesselZLength);
  fBeamPipeOuterRadius[4] = BeamPipeOuterRadiusNominal;
  fBeamPipeInnerRadius[4] = BeamPipeInnerRadiusNominal;
  fBeamPipeZPosition[4] = fMirrorWindowInnerFlangeZPosition; // RICH ref syst
  fBeamPipeFinZLength[4] = 5.0*mm;
  fBeamPipeFinOuterRadius[4] = fBeamPipeOuterRadius[4];
  fBeamPipeFinSpacing[4] = 40.0*mm;
  fBeamPipeZLengthWFins[4] = 0*mm;
  fBeamPipeInputDisplacementWRTBeam[4] = 0;
  fBeamPipeOutputDisplacementWRTBeam[4] = 0;

  //with fins, beam pipe part downstream mirror window
  fBeamPipeZLength[5] = fRICHDetectorZEnd-fRICHDetectorZPosition-(fBeamPipeZPosition[4]+0.5*fBeamPipeZLength[4]);
  fBeamPipeZPosition[5] = fRICHDetectorZEnd-fRICHDetectorZPosition-fBeamPipeZLength[5]*0.5;
  fBeamPipeOuterRadius[5] = BeamPipeOuterRadiusNominal;
  fBeamPipeInnerRadius[5] = BeamPipeInnerRadiusNominal;
  fBeamPipeFinZLength[5] = 5.0*mm;
  fBeamPipeFinOuterRadius[5] = BeamPipeFinOuterRadius;
  fBeamPipeFinSpacing[5] = 40.0*mm;
  fBeamPipeZLengthWFins[5] = fBeamPipeZLength[5];
  fBeamPipeInputDisplacementWRTBeam[5] = 0;
  fBeamPipeOutputDisplacementWRTBeam[5] = 0;


  fBeamWindowInnerFlangeInnerRadius = BeamPipeOuterRadiusNominal;
  fBeamWindowInnerFlangeRadialThickness = fBeamWindowInnerRadius-fBeamWindowInnerFlangeInnerRadius;


  fMirrorWindowInnerFlangeInnerRadius = BeamPipeOuterRadiusNominal;
  fMirrorWindowInnerFlangeRadialThickness = fMirrorWindowInnerRadius - fMirrorWindowInnerFlangeInnerRadius;

  // inclinazione RICH beatch file ottobre

  fInputDisplacementBeatch=0.034*m;
  fInputDisplacementZPositionBeatch=219.445*m;
  fOutputDisplacementBeatch=+0.002*m;
  fOutputDisplacementZPositionBeatch=237.326*m;

  // attenzione: per come l'ho definito, l'angolo di rotazione è positivo
  // la scatola dovrà essere ruotata attorno a y di -l'angolo

  fAngleWRTXaxis=(fInputDisplacementBeatch-fOutputDisplacementBeatch)/(fOutputDisplacementZPositionBeatch-fInputDisplacementZPositionBeatch);
  //G4cout<<"fAngleWRTXaxis: "<<fAngleWRTXaxis<<G4endl;

  fInputDisplacementWRTXaxis=fOutputDisplacementBeatch + fAngleWRTXaxis*(fOutputDisplacementZPositionBeatch-fRICHDetectorZStart);
  fOutputDisplacementWRTXaxis=fOutputDisplacementBeatch + fAngleWRTXaxis*(fOutputDisplacementZPositionBeatch-fRICHDetectorZEnd);

  //G4cout<<"fInputDisplacementWRTXaxis: "<<fInputDisplacementWRTXaxis<<G4endl;
  //G4cout<<"fOutputDisplacementWRTXaxis: "<<fOutputDisplacementWRTXaxis<<G4endl;

  // Additional parameters for the RR definition (the union of two cilinders)
  fRespRegionUpstreamRadius = fConicalWindowOuterFlangeInnerRadius + fConicalWindowOuterFlangeRadialThickness + 100*mm;

  fRespRegionDownstreamRadius = fMirrorWindowOuterRadius+fMirrorWindowOuterFlangeRadialThickness; // was 1.52 m
  //fRespRegionDownstreamRadius = fInterfaceRingInnerRadius+fInterfaceRingRadialThickness; //


  //  G4double dZRespRegion = 2.*fRespRegionUpstreamRadius*TMath::Tan(fAngleWRTXaxis); //correction due to RICH rotation: symmetric in upstream and downstream part

  //  G4cout<<"dZRespRegion: "<<dZRespRegion<<G4endl;

  fRespRegionZLength = fRespRegionZEnd - fRespRegionZStart;

  fRespRegionDownstreamLength =   fMirrorWindowOuterFlangeZLength;
  fRespRegionUpstreamLength = fRespRegionZLength - fRespRegionDownstreamLength;

  //  fRespRegionDownstreamLength =   fMirrorWindowOuterFlangeZLength + fInterfaceRingZLength +20*mm;
  //  fRespRegionUpstreamLength = fRespRegionZLength - fRespRegionDownstreamLength;


  //G4cout<<" *********** RespRegionZLength: "<<fRespRegionZLength<<" ***********"<<G4endl;
  //G4cout<<" *********** RespRegionUpstreamLength: "<<fRespRegionUpstreamLength<<" ***********"<<G4endl;
  //G4cout<<" *********** RespRegionDownstreamLength: "<<fRespRegionDownstreamLength<<" ***********"<<G4endl;

  G4double x_disk_center_jura_lab,z_disk_center_jura_lab;
  fromRICHBoxtoLab(fPMTsDiskCenter->x(),fPMTsDiskZPosition,&x_disk_center_jura_lab,&z_disk_center_jura_lab);
  fPMTsDiskCenter_Jura_lab = new G4TwoVector(x_disk_center_jura_lab,0.*m);

  //G4cout<<"x center disk Jura in lab ref: "<<fPMTsDiskCenter_Jura_lab->x()<<G4endl;

  G4double x_disk_center_saleve_lab,z_disk_center_saleve_lab;
  fromRICHBoxtoLab(-(fPMTsDiskCenter->x()),fPMTsDiskZPosition,&x_disk_center_saleve_lab,&z_disk_center_saleve_lab);
  fPMTsDiskCenter_Saleve_lab = new G4TwoVector(x_disk_center_saleve_lab,0.*m);

  //G4cout<<"x center disk Saleve in lab ref: "<<fPMTsDiskCenter_Saleve_lab->x()<<G4endl;

  fRadiatorInnerRadius = BeamPipeFinOuterRadius;

  // inizio parametri nuovi

  fMirrorFocalLength=17.00*m;
  fMirrorThickness=2.5*cm;
  fMirrorSphereInnerRadius = 2*fMirrorFocalLength;
  fMirrorSphereOuterRadius = fMirrorSphereInnerRadius+fMirrorThickness;
  fSubMirrorExternalRadius = 0.35*m; //radius of the circle delimiting each mirror segment
  fSubMirrorGap[0] = 1*mm;
  fSubMirrorGap[1] = 1*sqrt(3)/2;
  fSubHalfMirrorHoleRadius = BeamPipeFinOuterRadius + 2*mm;


  fMirrorZPosition=fPMTsDiskZPosition+fPMTsDiskZLength*0.5+fMirrorFocalLength+fMirrorThickness*0.5; // posizione degli specchi nel rif del RICH
  fMirrorZPositionRadRef=fMirrorZPosition-fVesselZPosition;// posizione degli specchi nel rif del radiatore

  //G4cout<<"**** posizione specchio nel rif del RICH: "<<fMirrorZPosition<<" ****"<<G4endl;
  //G4cout<<"**** posizione del radiatore nel RICH: "<<fVesselZPosition<<" ****"<<G4endl;
  //G4cout<<"**** posizione specchio nel rif del radiatore: "<<fMirrorZPositionRadRef<<" ****"<<G4endl;


  G4double MirrorCenterOfCurvature_Jura_Lab[3];
  G4double MirrorCenterOfCurvature_Saleve_Lab[3];

  MirrorCenterOfCurvature_Jura_Lab[0]=1.76*m;
  MirrorCenterOfCurvature_Jura_Lab[1]=0.*m;
  MirrorCenterOfCurvature_Jura_Lab[2]=fMirrorZPosition-fMirrorThickness*0.5+fRICHDetectorZPosition-fMirrorFocalLength*2;

  MirrorCenterOfCurvature_Saleve_Lab[0]=-1.39*m;
  MirrorCenterOfCurvature_Saleve_Lab[1]=0.*m;
  MirrorCenterOfCurvature_Saleve_Lab[2]=fMirrorZPosition-fMirrorThickness*0.5+fRICHDetectorZPosition-fMirrorFocalLength*2;

  fMirrorCenterOfCurvature_Jura[1]=MirrorCenterOfCurvature_Jura_Lab[1];

  fMirrorCenterOfCurvature_Saleve[1]=MirrorCenterOfCurvature_Saleve_Lab[1];

  //G4cout<<" x center of curvature jura lab:  " <<MirrorCenterOfCurvature_Jura_Lab[0]<<
  //        " z center of curvature jura lab: "<< MirrorCenterOfCurvature_Jura_Lab[2]<<G4endl;
  fromLabtoRICHBox(MirrorCenterOfCurvature_Jura_Lab[0],MirrorCenterOfCurvature_Jura_Lab[2],&fMirrorCenterOfCurvature_Jura[0],&fMirrorCenterOfCurvature_Jura[2]);
  //G4cout<<" x center of curvature jura RICH box:  " <<fMirrorCenterOfCurvature_Jura[0]<<
  //        " z center of curvature jura RICH box: "<< fMirrorCenterOfCurvature_Jura[2]<<G4endl;

  //G4cout<<" x center of curvature saleve lab:  " <<MirrorCenterOfCurvature_Saleve_Lab[0]<<
  //        " z center of curvature saleve lab: "<< MirrorCenterOfCurvature_Saleve_Lab[2]<<G4endl;
  fromLabtoRICHBox(MirrorCenterOfCurvature_Saleve_Lab[0],MirrorCenterOfCurvature_Saleve_Lab[2],&fMirrorCenterOfCurvature_Saleve[0],&fMirrorCenterOfCurvature_Saleve[2]);
  //G4cout<<" x center of curvature saleve RICH box:  " <<fMirrorCenterOfCurvature_Saleve[0]<<
  //        " z center of curvature saleve RICH box: "<< fMirrorCenterOfCurvature_Saleve[2]<<G4endl;


  //  fMirrorCenterOfCurvature_Jura[0] = 1.77*m;          // queste sono le coordinate nel laboratorio, dunque valide se il RICH non è inclinato
  //  fMirrorCenterOfCurvature_Jura[1] = 0.*m;
  //  fMirrorCenterOfCurvature_Saleve[0] = -1.34*m;
  //  fMirrorCenterOfCurvature_Saleve[1] = 0.*m;


  for(Int_t irow=0; irow<10;irow++){
    for(Int_t icolumn=0; icolumn<2;icolumn++){
      fMirrorCenterOfCurvatureShift_Jura[irow][icolumn]=0.;
      //G4cout<<fMirrorCenterOfCurvatureShift_Jura[irow][icolumn]<<" ";
    }
    //G4cout<<G4endl;
  }


  for(Int_t irow=0; irow<10;irow++){
    for(Int_t icolumn=0; icolumn<2;icolumn++){
      fMirrorCenterOfCurvatureShift_Saleve[irow][icolumn]=0.;
    }
  }

  fMirrorActuatorPinRadius=10*mm;
  fMirrorActuatorPinHeight=20*mm;


  fMirrorActuatorPinR1_Jura[0]=250*mm;
  fMirrorActuatorPinR1_Jura[1]=250*mm;
  fMirrorActuatorPinR1_Jura[2]=250*mm;
  fMirrorActuatorPinR1_Jura[3]=125*mm;
  fMirrorActuatorPinR1_Jura[4]=250*mm;
  fMirrorActuatorPinR1_Jura[5]=252*mm;
  fMirrorActuatorPinR1_Jura[6]=252*mm;
  fMirrorActuatorPinR1_Jura[7]=250*mm;
  fMirrorActuatorPinR1_Jura[8]=250*mm;

  fMirrorActuatorPinR2_Jura[0]=250*mm;
  fMirrorActuatorPinR2_Jura[1]=250*mm;
  fMirrorActuatorPinR2_Jura[2]=250*mm;
  fMirrorActuatorPinR2_Jura[3]=125*mm;
  fMirrorActuatorPinR2_Jura[4]=250*mm;
  fMirrorActuatorPinR2_Jura[5]=252*mm;
  fMirrorActuatorPinR2_Jura[6]=252*mm;
  fMirrorActuatorPinR2_Jura[7]=250*mm;
  fMirrorActuatorPinR2_Jura[8]=250*mm;
  fMirrorActuatorPinR2_Jura[9]=257*mm;

  fMirrorActuatorPinR1_Saleve[0]=250*mm;
  fMirrorActuatorPinR1_Saleve[1]=250*mm;
  fMirrorActuatorPinR1_Saleve[2]=250*mm;
  fMirrorActuatorPinR1_Saleve[3]=125*mm;
  fMirrorActuatorPinR1_Saleve[4]=250*mm;
  fMirrorActuatorPinR1_Saleve[5]=252*mm;
  fMirrorActuatorPinR1_Saleve[6]=252*mm;
  fMirrorActuatorPinR1_Saleve[7]=250*mm;
  fMirrorActuatorPinR1_Saleve[8]=250*mm;
  fMirrorActuatorPinR1_Saleve[9]=257*mm;

  fMirrorActuatorPinR2_Saleve[0]=250*mm;
  fMirrorActuatorPinR2_Saleve[1]=250*mm;
  fMirrorActuatorPinR2_Saleve[2]=250*mm;
  fMirrorActuatorPinR2_Saleve[3]=125*mm;
  fMirrorActuatorPinR2_Saleve[4]=250*mm;
  fMirrorActuatorPinR2_Saleve[5]=252*mm;
  fMirrorActuatorPinR2_Saleve[6]=252*mm;
  fMirrorActuatorPinR2_Saleve[7]=250*mm;
  fMirrorActuatorPinR2_Saleve[8]=250*mm;


  fMirrorStabilizerPinRadius=10*mm;

  fMirrorStabilizerPinHeight_Jura[0]=58*mm;
  fMirrorStabilizerPinHeight_Jura[1]=58*mm;
  fMirrorStabilizerPinHeight_Jura[2]=58*mm;
  fMirrorStabilizerPinHeight_Jura[3]=58*mm;
  fMirrorStabilizerPinHeight_Jura[4]=58*mm;
  fMirrorStabilizerPinHeight_Jura[5]=58*mm;
  fMirrorStabilizerPinHeight_Jura[6]=58*mm;
  fMirrorStabilizerPinHeight_Jura[7]=58*mm;
  fMirrorStabilizerPinHeight_Jura[8]=58*mm;


  fMirrorStabilizerPinD_Jura[0]=+232*mm;
  fMirrorStabilizerPinD_Jura[1]=+234*mm;
  fMirrorStabilizerPinD_Jura[2]=+233*mm;
  fMirrorStabilizerPinD_Jura[3]=-255*mm;
  fMirrorStabilizerPinD_Jura[4]=-265*mm;
  fMirrorStabilizerPinD_Jura[5]=+255*mm;
  fMirrorStabilizerPinD_Jura[6]=+252*mm;
  fMirrorStabilizerPinD_Jura[7]=-253*mm;
  fMirrorStabilizerPinD_Jura[8]=+254*mm;

  fMirrorStabilizerPinHeight_Saleve[0]=58*mm;
  fMirrorStabilizerPinHeight_Saleve[1]=58*mm;
  fMirrorStabilizerPinHeight_Saleve[2]=58*mm;
  fMirrorStabilizerPinHeight_Saleve[3]=58*mm;
  fMirrorStabilizerPinHeight_Saleve[4]=58*mm;
  fMirrorStabilizerPinHeight_Saleve[5]=58*mm;
  fMirrorStabilizerPinHeight_Saleve[6]=58*mm;
  fMirrorStabilizerPinHeight_Saleve[7]=58*mm;
  fMirrorStabilizerPinHeight_Saleve[8]=58*mm;

  fMirrorStabilizerPinD_Saleve[0]=-233*mm;
  fMirrorStabilizerPinD_Saleve[1]=-235*mm;
  fMirrorStabilizerPinD_Saleve[2]=-234*mm;
  fMirrorStabilizerPinD_Saleve[3]=+254*mm;
  fMirrorStabilizerPinD_Saleve[4]=+255*mm;
  fMirrorStabilizerPinD_Saleve[5]=-255*mm;
  fMirrorStabilizerPinD_Saleve[6]=-254*mm;
  fMirrorStabilizerPinD_Saleve[7]=-233*mm;
  fMirrorStabilizerPinD_Saleve[8]=-254*mm;


  // inizio parametri supporto specchi

  fMirrorSupportPanelOuterRadius=1.520*m;
  fMirrorSupportPanelInnerRadius=BeamPipeFinOuterRadius+11.0*mm; // 89*mm+11*mm=100*mm
  fMirrorSupportPanelEquivalentThickness=3.47*mm;
  fMirrorSupportPanelZPosition=fMirrorZPosition+fMirrorThickness*0.5+150*mm+fMirrorSupportPanelEquivalentThickness*0.5;// da definire con l'altezza
  // del cono e la lunghezza del dowell
  fMirrorSupportPanelZPositionRadRef=fMirrorSupportPanelZPosition-fVesselZPosition;// posizione del supporto degli specchi nel rif del radiatore

  fMirrorSupportPanelToothXLenght=220*mm;
  fMirrorSupportPanelToothYLenght=220*mm;
  fMirrorSupportPanelToothZLenght=fMirrorSupportPanelEquivalentThickness;
  fMirrorSupportPanelToothZPosition=fMirrorSupportPanelZPosition;
  fMirrorSupportPanelToothZPositionRadRef=fMirrorSupportPanelToothZPosition-fVesselZPosition;


  fMirrorSupportPanelRingOuterRadius=1.550*m;
  fMirrorSupportPanelRingInnerRadius=fMirrorSupportPanelOuterRadius-0.03*m; //1.49*m

  fMirrorSupportPanelRingThickness=50*mm;
  fMirrorSupportPanelRingZPosition=fMirrorSupportPanelZPosition-fMirrorSupportPanelEquivalentThickness*0.5+fMirrorSupportPanelRingThickness*0.5;

  fMirrorSupportPanelRingZPositionRadRef=fMirrorSupportPanelRingZPosition-fVesselZPosition;


  fMirrorSupportPanelRotation_Jura=1.4*deg; //angolo di rotazione nel riferimento del RICH??
  fMirrorSupportPanelRotation_Saleve=-1.4*deg; //angolo di rotazione nel riferimento del RICH??

  fMirrorSupportPanelConeBottomOuterRadius=58*mm;
  fMirrorSupportPanelConeUpOuterRadius=20*mm;
  fMirrorSupportPanelConeThickness=1.9*mm;

  fMirrorSupportPanelConeHeight[0]=99.5*mm;
  fMirrorSupportPanelConeHeight[1]=99.5*mm;
  fMirrorSupportPanelConeHeight[2]=99.5*mm;
  fMirrorSupportPanelConeHeight[3]=99.5*mm;
  fMirrorSupportPanelConeHeight[4]=99.5*mm;
  fMirrorSupportPanelConeHeight[5]=99.5*mm;
  fMirrorSupportPanelConeHeight[6]=99.5*mm;
  fMirrorSupportPanelConeHeight[7]=99.5*mm;
  fMirrorSupportPanelConeHeight[8]=99.5*mm;

  for(Int_t i=0;i<9;i++){
    fMirrorSupportPanelConeZPosition[i]=fMirrorSupportPanelZPosition-fMirrorSupportPanelEquivalentThickness*0.5-fMirrorSupportPanelConeHeight[i]*0.5;
    fMirrorSupportPanelConeZPositionRadRef[i]=fMirrorSupportPanelConeZPosition[i]-fVesselZPosition;
  }

  fMirrorSupportPanelConeBaseOuterRadius=70*mm;
  fMirrorSupportPanelConeBaseInnerRadius=fMirrorSupportPanelConeBottomOuterRadius;
  fMirrorSupportPanelConeBaseThickness=5*mm;
  fMirrorSupportPanelConeBaseZPosition=fMirrorSupportPanelZPosition-fMirrorSupportPanelEquivalentThickness*0.5-fMirrorSupportPanelConeBaseThickness*0.5;
  fMirrorSupportPanelConeBaseZPositionRadRef=fMirrorSupportPanelConeBaseZPosition-fVesselZPosition;

  fMirrorSupportPanelConeHatOuterRadius=fMirrorSupportPanelConeUpOuterRadius;
  fMirrorSupportPanelConeHatInnerRadius=0*mm;
  fMirrorSupportPanelConeHatThickness=15*mm;

  for(Int_t i=0; i<9; i++){
    fMirrorSupportPanelConeHatZPosition[i]=fMirrorSupportPanelConeZPosition[i]-fMirrorSupportPanelConeHeight[i]*0.5-fMirrorSupportPanelConeHatThickness*0.5;
    fMirrorSupportPanelConeHatZPositionRadRef[i]=fMirrorSupportPanelConeHatZPosition[i]-fVesselZPosition;
  }
  fMirrorSupportPanelConeScrewRadius=2.15*mm;
  fMirrorSupportPanelConeScrewHeight=50*mm;
  fMirrorSupportPanelConeScrewZPosition=fMirrorSupportPanelConeBaseZPosition-fMirrorSupportPanelConeBaseThickness*0.5+fMirrorSupportPanelConeScrewHeight*0.5;
  fMirrorSupportPanelConeScrewZPositionRadRef=fMirrorSupportPanelConeScrewZPosition-fVesselZPosition;

  fMirrorSupportPanelTurnerBaseXLenght=1*cm;
  fMirrorSupportPanelTurnerBaseYLenght=2*cm;
  fMirrorSupportPanelTurnerBaseZLenght=0.5*cm;
  fMirrorSupportPanelTurnerBaseZPosition=fMirrorSupportPanelZPosition+fMirrorSupportPanelEquivalentThickness*0.5+fMirrorSupportPanelTurnerBaseZLenght*0.5;
  fMirrorSupportPanelTurnerBaseZPositionRadRef=fMirrorSupportPanelTurnerBaseZPosition-fVesselZPosition;

  fMirrorSupportPanelTurnerXMaxLenght=0.5*cm;
  fMirrorSupportPanelTurnerXMinLenght=0.5*cm;
  fMirrorSupportPanelTurnerYMaxLenght=2*cm;
  fMirrorSupportPanelTurnerYMinLenght=1.5*cm;
  fMirrorSupportPanelTurnerZLenght=4.5*cm;
  fMirrorSupportPanelTurnerZPosition=fMirrorSupportPanelTurnerBaseZPosition+fMirrorSupportPanelTurnerBaseZLenght*0.5+fMirrorSupportPanelTurnerZLenght*0.5;
  fMirrorSupportPanelTurnerZPositionRadRef=fMirrorSupportPanelTurnerZPosition-fVesselZPosition;

  fMirrorSupportPanelTurnerHoleXMaxLenght=0.5*cm;
  fMirrorSupportPanelTurnerHoleXMinLenght=0.5*cm;
  fMirrorSupportPanelTurnerHoleYMaxLenght=1.5*cm;
  fMirrorSupportPanelTurnerHoleYMinLenght=1.*cm;
  fMirrorSupportPanelTurnerHoleZLenght=3.5*cm;
  fMirrorSupportPanelTurnerHoleZPosition=fMirrorSupportPanelTurnerBaseZPosition+fMirrorSupportPanelTurnerBaseZLenght*0.5+fMirrorSupportPanelTurnerHoleZLenght*0.5;
  fMirrorSupportPanelTurnerHoleZPositionRadRef=fMirrorSupportPanelTurnerHoleZPosition-fVesselZPosition;


  fMirrorSupportPanelPrismBaseXLenght=100*mm;
  fMirrorSupportPanelPrismBaseYLenght=140*mm;
  fMirrorSupportPanelPrismBaseZLenght=5*mm;
  fMirrorSupportPanelPrismBaseZPosition=fMirrorSupportPanelZPosition-fMirrorSupportPanelEquivalentThickness*0.5-fMirrorSupportPanelPrismBaseZLenght*0.5;
  fMirrorSupportPanelPrismBaseZPositionRadRef=fMirrorSupportPanelPrismBaseZPosition-fVesselZPosition;

  fMirrorSupportPanelPrismX1Lenght=100*mm;
  fMirrorSupportPanelPrismX2Lenght=100*mm;
  fMirrorSupportPanelPrismY1Lenght=40*mm;
  fMirrorSupportPanelPrismY2Lenght=116*mm;
  fMirrorSupportPanelPrismZLenght=99.5*mm;
  fMirrorSupportPanelPrismZPosition=fMirrorSupportPanelZPosition-fMirrorSupportPanelEquivalentThickness*0.5-fMirrorSupportPanelPrismZLenght*0.5;
  fMirrorSupportPanelPrismZPositionRadRef=fMirrorSupportPanelPrismZPosition-fVesselZPosition;

  fMirrorSupportPanelPrismHoleX1Lenght=fMirrorSupportPanelPrismX1Lenght-3.8*mm; //spessore di 1.9*mm
  fMirrorSupportPanelPrismHoleX2Lenght=fMirrorSupportPanelPrismX2Lenght-3.8*mm;
  fMirrorSupportPanelPrismHoleY1Lenght=fMirrorSupportPanelPrismY1Lenght-3.8*mm;
  fMirrorSupportPanelPrismHoleY2Lenght=fMirrorSupportPanelPrismY2Lenght-3.8*mm;
  fMirrorSupportPanelPrismHoleZLenght=fMirrorSupportPanelPrismZLenght;
  fMirrorSupportPanelPrismHoleZPosition=fMirrorSupportPanelPrismZPosition;
  fMirrorSupportPanelPrismHoleZPositionRadRef=fMirrorSupportPanelPrismHoleZPosition-fVesselZPosition;

  fMirrorSupportPanelPrismHatXLenght=fMirrorSupportPanelPrismX1Lenght;
  fMirrorSupportPanelPrismHatYLenght=40*mm;
  fMirrorSupportPanelPrismHatZLenght=15*mm;
  fMirrorSupportPanelPrismHatZPosition=fMirrorSupportPanelZPosition-fMirrorSupportPanelEquivalentThickness*0.5-fMirrorSupportPanelPrismZLenght-fMirrorSupportPanelPrismHatZLenght*0.5;
  fMirrorSupportPanelPrismHatZPositionRadRef=fMirrorSupportPanelPrismHatZPosition-fVesselZPosition;

  fMirrorSupportPanelPrismXPosition_Jura=150*mm;
  fMirrorSupportPanelPrismXPosition_Saleve=-150*mm;

  fMirrorSupportPanelPrismScrewRadius=2.15*mm;
  fMirrorSupportPanelPrismScrewHeight=50*mm;
  fMirrorSupportPanelPrismScrewZPosition=fMirrorSupportPanelPrismBaseZPosition-fMirrorSupportPanelPrismBaseZLenght*0.5+fMirrorSupportPanelPrismScrewHeight*0.5;
  fMirrorSupportPanelPrismScrewZPositionRadRef=fMirrorSupportPanelPrismScrewZPosition-fVesselZPosition;

  fMirrorSupportPanelDowelRadius=4.5*mm;
  fMirrorSupportPanelDowelHeight=20*mm;




  // fine parametri supporto specchi


  fConeInputDiameter=18*mm;
  fConeInputRadius=0.5*fConeInputDiameter;
  fConeOutputDiameter=7.5*mm;
  fConeOutputRadius=0.5*fConeOutputDiameter;
  fConeLongitudinalLength=22*mm;

  fMylarConeThickness=50*um;
  fMylarConeInputRadius=fConeInputRadius-0.5*fMylarConeThickness;
  fMylarConeOutputRadius=fConeOutputRadius-0.5*fMylarConeThickness;
  fMylarConeLongitudinalLength=fConeLongitudinalLength;

  fQuartzWindowInnerRadius=0;
  fQuartzWindowOuterRadius=0.5*13.5*mm;
  fQuartzWindowThickness=1*mm;

  fPMWindowInnerRadius=0;
  fPMWindowOuterRadius=0.5*8*mm;
  fPMWindowThickness=1*mm;

  fPMInnerRadius=0;
  fPMOuterRadius=0.5*16.5*mm;
  fPMLength=12*mm;

  fHoneyCombDistance=fConeInputRadius*sqrt(3);


  fPMsPositions= new TVector2[fNChannels];
  fPMsIDs = new G4int*[201];
  for(G4int iID = 0; iID < 201; iID++)
    fPMsIDs[iID] = new G4int[201];
  for(G4int iID1 = 0; iID1 < 201; iID1++)
    for(G4int iID2 = 0; iID2 < 201; iID2++)
      fPMsIDs[iID1][iID2] = -1;

  //fb



  Double_t  SpotRadius=0.30*m;
  Double_t  ConeInputDiameter=0.018*m;
  Double_t  ConeInputRadius=0.5*ConeInputDiameter;
  Double_t  HoneyCombDistance=ConeInputRadius*sqrt(3); //apotema


  Int_t nPM=0;
  Int_t nRow=6;
  Int_t nSuperCellinRow[6];


  Double_t PM_positions[1000][2];
  Double_t alpha=TMath::ATan(HoneyCombDistance/(5*ConeInputRadius));
  Double_t x_pos=ConeInputRadius;
  Double_t y_pos=2*(ConeInputRadius+HoneyCombDistance);

  Double_t x_pos_trasf=x_pos*TMath::Cos(alpha)+y_pos*TMath::Sin(alpha);
  Double_t y_pos_trasf=-x_pos*TMath::Sin(alpha)+y_pos*TMath::Cos(alpha);
  Double_t Row_distance=sqrt(pow(x_pos_trasf,2)+pow(y_pos_trasf,2));

  Double_t x_center_tmp=30*ConeInputRadius;
  Double_t y_center_tmp=-6*HoneyCombDistance;


  Double_t angular_coeff=3.141592654/2-alpha; // coefficiente angolare
  Double_t q=-(3.141592654/2-alpha)*x_center_tmp+y_center_tmp;
  Double_t x_center_r,y_center_r;
  x_center_r=y_center_r=0.;

  Double_t center_distance=0.;
  Int_t cell_dist[6];

  Double_t x_center_row0_up,y_center_row0_up,x_center_row0_down,y_center_row0_down;
  x_center_row0_up=y_center_row0_up=x_center_row0_down=y_center_row0_down=0.;

  Double_t x_Center,y_Center;
  x_Center=y_Center=0.;


  for(Int_t i=0;i<nRow;i++){
    nSuperCellinRow[i]=0;
    cell_dist[i]=0;
  }

  for(Int_t k=0;k<nRow;k++){//first half

    Float_t  Chord=2*sqrt((SpotRadius-Row_distance*k)*(2*SpotRadius-(SpotRadius-Row_distance*k)));
    //G4cout<<"corda: "<<Chord<<G4endl;
    nSuperCellinRow[k]=0;

    if(k<5){
      while((nSuperCellinRow[k]+1)*2*(ConeInputRadius+HoneyCombDistance)<Chord+22.){

        nSuperCellinRow[k]++;
      }
    }else{
      while((nSuperCellinRow[k]+1)*2*(ConeInputRadius+HoneyCombDistance)<Chord){
        nSuperCellinRow[k]++;
      }
    }


    if(k){
      x_center_r=(1*k+5*nSuperCellinRow[k])/2*ConeInputRadius;
      y_center_r=(3*k-nSuperCellinRow[k])/2*HoneyCombDistance;
      //      G4cout<<"x_center_r: "<<x_center_r<<" y_center_r: "<<y_center_r<<G4endl;

      center_distance=TMath::Abs(y_center_r-angular_coeff*x_center_r-q)/sqrt(1+pow(angular_coeff,2));
      //      G4cout<<" center distance: "<<center_distance<<G4endl;
      cell_dist[k]=floor(center_distance/(2*(ConeInputRadius+HoneyCombDistance))+0.2);

      // G4cout<<"cell_dist: "<<cell_dist[k]<<G4endl;
    }

    for(Int_t l=0;l<nSuperCellinRow[k];l++){

      if(!k){
        x_center_row0_up=x_center_row0_up+5*l*ConeInputRadius;
        y_center_row0_up=y_center_row0_up-1*l*HoneyCombDistance;
      }

      for(Int_t m=-1;m<=1;m++){
        for(Int_t n=-(TMath::Abs(m)+1);n<=(TMath::Abs(m)+1);n++){
          if(m){
            if(0==TMath::Abs(n)%2){
              PM_positions[nPM][0]=(n+5*l+1*k+5*cell_dist[k])*ConeInputRadius;
              PM_positions[nPM][1]=(m-1*l+3*k-1*cell_dist[k])*HoneyCombDistance;
              fPMsIDs[n+5*l+1*k+5*cell_dist[k]+100][m-1*l+3*k-1*cell_dist[k]+100]=nPM;
              //G4cout<<"fPMsID["<<n+5*l+1*k+5*cell_dist[k]+100<<"]["<<m-1*l+3*k-1*cell_dist[k]+100<<"] = "<<nPM<<G4endl;
              nPM++;
            }
          }else{
            if(0!=TMath::Abs(n)%2){
              PM_positions[nPM][0]=(n+5*l+1*k+5*cell_dist[k])*ConeInputRadius;
              PM_positions[nPM][1]=(m-1*l+3*k-1*cell_dist[k])*HoneyCombDistance;
              fPMsIDs[n+5*l+1*k+5*cell_dist[k]+100][m-1*l+3*k-1*cell_dist[k]+100]=nPM;
              //G4cout<<"fPMsID["<<n+5*l+1*k+5*cell_dist[k]+100<<"]["<<m-1*l+3*k-1*cell_dist[k]+100<<"] = "<<nPM<<G4endl;
              nPM++;
              //G4cout<<nPM<<G4endl;
            }
          }
        }
      }
    }
    if(!k){
      x_center_row0_up=x_center_row0_up/nSuperCellinRow[k];
      y_center_row0_up=y_center_row0_up/nSuperCellinRow[k];
      //G4cout<<"x_center_row0_up: "<<x_center_row0_up<<"  y_center_row0_up: "<<y_center_row0_up<<G4endl;
    }

    //          G4cout<<"numero di supercelle: "<<nSuperCellinRow[k]<<G4endl;


  }//end first half


  for(Int_t j=0;j<nRow;j++){//second half
    //G4cout<<"j: "<<j<<"  nSuperCellinRow["<<j<<"]: "<<nSuperCellinRow[j]<<G4endl;
    for(Int_t l=0;l<nSuperCellinRow[j];l++){

      if(!j){
        x_center_row0_down=x_center_row0_down+(5*l-1*(j+1))*ConeInputRadius;
        y_center_row0_down=y_center_row0_down-(1*l+3*(j+1))*HoneyCombDistance;
      }

      for(Int_t m=-1;m<=1;m++){
        for(Int_t n=-(TMath::Abs(m)+1);n<=(TMath::Abs(m)+1);n++){
          if(m){
            if(0==TMath::Abs(n)%2){
              PM_positions[nPM][0]=(n+5*l-1*(j+1)+5*(12-nSuperCellinRow[j]-cell_dist[j]))*ConeInputRadius;
              PM_positions[nPM][1]=(m-1*l-3*(j+1)-1*(12-nSuperCellinRow[j]-cell_dist[j]))*HoneyCombDistance;
              fPMsIDs[n+5*l-1*(j+1)+5*(12-nSuperCellinRow[j]-cell_dist[j])+100][m-1*l-3*(j+1)-1*(12-nSuperCellinRow[j]-cell_dist[j])+100]=nPM;
              //G4cout<<"fPMsID["<<n+5*l-1*(j+1)+5*(12-nSuperCellinRow[j]-cell_dist[j])+100<<"]["<<m-1*l-3*(j+1)-1*(12-nSuperCellinRow[j]-cell_dist[j])+100<<"] = "<<nPM<<G4endl;
              nPM++;
              //G4cout<<nPM<<G4endl;
            }
          }else{
            if(0!=TMath::Abs(n)%2){
              PM_positions[nPM][0]=(n+5*l-1*(j+1)+5*(12-nSuperCellinRow[j]-cell_dist[j]))*ConeInputRadius;
              PM_positions[nPM][1]=(m-1*l-3*(j+1)-1*(12-nSuperCellinRow[j]-cell_dist[j]))*HoneyCombDistance;
              fPMsIDs[n+5*l-1*(j+1)+5*(12-nSuperCellinRow[j]-cell_dist[j])+100][m-1*l-3*(j+1)-1*(12-nSuperCellinRow[j]-cell_dist[j])+100]=nPM;
              //G4cout<<"fPMsID["<<n+5*l-1*(j+1)+5*(12-nSuperCellinRow[j]-cell_dist[j])+100<<"]["<<m-1*l-3*(j+1)-1*(12-nSuperCellinRow[j]-cell_dist[j])+100<<"] = "<<nPM<<G4endl;  
              nPM++;
              //G4cout<<nPM<<G4endl;
            }
          }
        }
      }
    }

    if(!j){
      x_center_row0_down=x_center_row0_down/nSuperCellinRow[j];
      y_center_row0_down=y_center_row0_down/nSuperCellinRow[j];
    }
  }


  x_Center=(x_center_row0_up+x_center_row0_down)/2;
  y_Center=(y_center_row0_up+y_center_row0_down)/2;


  //      G4cout<<"x_Center: "<<x_Center<<"  y_Center: "<<y_Center<<G4endl;
  //      G4cout<<"nPM: "<<nPM<<G4endl;

  fGeoIDs = new G4int[976];

  G4int SCID,UpDwID,PMinSC;


  for(Int_t iPM=0;iPM<nPM;iPM++){
    fPMsPositions[iPM].Set(PM_positions[iPM][0]-x_Center,PM_positions[iPM][1]-y_Center);
    //    G4cout<<iPM<<"\t"<<PM_positions[iPM][0]-x_Center<<"\t"<<PM_positions[iPM][1]-y_Center<<G4endl;
    if(iPM<61*8){
      UpDwID=0;
      SCID=iPM/8;
    }else{
      UpDwID=1;
      SCID=(iPM-61*8)/8;
    }
    PMinSC=iPM-(SCID*8+61*8*UpDwID);
    fGeoIDs[iPM]=UpDwID*10000+SCID*100+PMinSC; //RICHChannelID for PMTs without Disk info
    //G4cout<<"iPM: "<<iPM<<" fGeoIDs: "<<fGeoIDs[iPM]<<G4endl;
  }

  fNPMs=2*nPM;


  // G4cout <<"#####################"<< fNPMs << "PMs installed ######################" << G4endl;

}

RICHGeometryParameters* RICHGeometryParameters::GetInstance() {
  if (!fInstance) fInstance = new RICHGeometryParameters();
  return fInstance;
}

void RICHGeometryParameters::fromLabtoRICHBox(G4double x,G4double z,G4double* x_rich,G4double* z_rich){
  *x_rich=(x-fOutputDisplacementWRTXaxis)*TMath::Cos(fAngleWRTXaxis)+(z-fRICHDetectorZLength/2-fRICHDetectorZPosition)*TMath::Sin(fAngleWRTXaxis);
  *z_rich=(z-fRICHDetectorZLength/2-fRICHDetectorZPosition)*TMath::Cos(fAngleWRTXaxis)-(x-fOutputDisplacementWRTXaxis)*TMath::Sin(fAngleWRTXaxis)+fRICHDetectorZLength/2;
  //G4cout<<"x_rich: "<<*x_rich<<"  z_rich: "<<*z_rich<<G4endl;
}

void RICHGeometryParameters::fromRICHBoxtoLab(G4double x,G4double z,G4double* x_lab,G4double* z_lab){
  *x_lab=x*TMath::Cos(fAngleWRTXaxis)-(z-fRICHDetectorZLength/2)*TMath::Sin(fAngleWRTXaxis)+fOutputDisplacementWRTXaxis;
  *z_lab=(z-fRICHDetectorZLength/2)*TMath::Cos(fAngleWRTXaxis)+x*TMath::Sin(fAngleWRTXaxis)+fRICHDetectorZLength/2+z;
  //G4cout<<"x_lab: "<<*x_lab<<"  z_lab: "<<*z_lab<<G4endl;
}
void RICHGeometryParameters::Print(){
  //   G4cout << "fNChannels= "<< fNChannels << G4endl
  //      << "fRICHDetectorZPosition= "<< fRICHDetectorZPosition << G4endl
  //      << "fRICHDetectorRadius= "<< fRICHDetectorRadius << G4endl
  //      << "fRICHDetectorZLength= "<< fRICHDetectorZLength << G4endl
  //      << "fRICHDetectorXLength= "<< fRICHDetectorXLength << G4endl
  //      << "fRICHDetectorYLength= "<< fRICHDetectorYLength << G4endl
  //      << "fBeamWindowInnerRadius= "<< fBeamWindowInnerRadius << G4endl
  //      << "fBeamWindowOuterRadius= "<< fBeamWindowOuterRadius << G4endl
  //      << "fBeamWindowInnerFlangeRadialThickness= "<< fBeamWindowInnerFlangeRadialThickness << G4endl
  //      << "fBeamWindowInnerFlangeZLength= "<< fBeamWindowInnerFlangeZLength << G4endl
  //      << "fBeamWindowOuterFlangeRadialThickness= "<< fBeamWindowOuterFlangeRadialThickness << G4endl
  //      << "fBeamWindowOuterFlangeZLength= "<< fBeamWindowOuterFlangeZLength << G4endl
  //      << "fBeamWindowZLength= "<< fBeamWindowZLength << G4endl
  //      << "fBeamWindowThickness= "<< fBeamWindowThickness << G4endl
  //      << "fNVesselSections= "<< fNVesselSections << G4endl
  //      << "fVesselSectionThickness= "<< fVesselSectionThickness << G4endl
  //      << "fVesselZLength= "<< fVesselZLength << G4endl
  //      << "fInputDisplacementWRTBeam= "<< fInputDisplacementWRTBeam << G4endl
  //      << "fOutputDisplacementWRTBeam= "<< fOutputDisplacementWRTBeam << G4endl
  //      << "fAngleWRTBeam= "<< fAngleWRTBeam << G4endl
  //      << "fBeamPipeInputDisplacementWRTBeam[0]= "<< fBeamPipeInputDisplacementWRTBeam[0] << G4endl
  //      << "fBeamPipeOutputDisplacementWRTBeam[0]= "<< fBeamPipeOutputDisplacementWRTBeam[0] << G4endl
  //      << "fBeamPipeZLength[0]= "<< fBeamPipeZLength[0] << G4endl
  //      << "fBeamPipeInnerRadius[0]= "<< fBeamPipeInnerRadius[0] << G4endl
  //      << "fBeamPipeOuterRadius[0]= "<< fBeamPipeOuterRadius[0] << G4endl
  //      << "fBeamPipeFinZLength[0]= "<< fBeamPipeFinZLength[0] << G4endl
  //       << "fBeamPipeFinOuterRadius[0]= "<< fBeamPipeFinOuterRadius[0] << G4endl
  //      << "fBeamPipeFinSpacing[0]= "<< fBeamPipeFinSpacing[0] << G4endl
  //      << "fBeamPipeZLengthWFins[0]= "<< fBeamPipeZLengthWFins[0] << G4endl
  //      << "fRadiatorInnerRadius= "<< fRadiatorInnerRadius << G4endl
  //      << "fRadiatorOuterRadius= "<< fRadiatorOuterRadius << G4endl
  //      << "fMirrorWindowZLength= "<< fMirrorWindowZLength << G4endl
  //      << "fMirrorWindowInnerRadius= "<< fMirrorWindowInnerRadius << G4endl
  //      << "fMirrorWindowOuterRadius= "<< fMirrorWindowOuterRadius << G4endl
  //      << "fMirrorWindowThickness= "<< fMirrorWindowThickness << G4endl
  //      << "fMirrorWindowInnerFlangeRadialThickness= "<< fMirrorWindowInnerFlangeRadialThickness << G4endl
  //      << "fMirrorWindowInnerFlangeZLength= "<< fMirrorWindowInnerFlangeZLength << G4endl
  //      << "fMirrorWindowOuterFlangeRadialThickness= "<< fMirrorWindowOuterFlangeRadialThickness << G4endl
  //      << "fMirrorWindowOuterFlangeZLength= "<< fMirrorWindowOuterFlangeZLength << G4endl
  //      << "fPMsOuterFlangeZLength= "<< fPMsOuterFlangeZLength << G4endl
  //      << "fPMsOuterFlangeInnerRadius= "<< fPMsOuterFlangeInnerRadius << G4endl
  //      << "fPMsOuterFlangeOuterRadius= "<< fPMsOuterFlangeOuterRadius << G4endl
  //      << "fPMsOuterFlangeThickness= "<< fPMsOuterFlangeThickness << G4endl
  //      << "fPMsSpotInnerRadius= "<< fPMsSpotInnerRadius << G4endl
  //      << "fPMsSpotOuterRadius= "<< fPMsSpotOuterRadius << G4endl
  //      << "fPMsSpotZLength= "<< fPMsSpotZLength << G4endl
  //      << "fMirrorFlangeDistance= "<< fMirrorFlangeDistance << G4endl
  //      << "fMirrorFocalLength= "<< fMirrorFocalLength << G4endl
  //      << "fRingCenterDisplacementX= "<< fRingCenterDisplacementX << G4endl
  //      << "fRingCenterDisplacementY= "<< fRingCenterDisplacementY << G4endl
  //      << "fMirrorMisalignmentTheta= "<< fMirrorMisalignmentTheta << G4endl
  //      << "fMirrorMisalignmentPhi= "<< fMirrorMisalignmentPhi << G4endl
  //      << "fMirrorThickness= "<< fMirrorThickness << G4endl
  //      << "fMirrorSphereInnerRadius= "<< fMirrorSphereInnerRadius << G4endl
  //      << "fMirrorSphereOuterRadius= "<< fMirrorSphereOuterRadius << G4endl
  //      << "fMirrorVesselGap= "<< fMirrorVesselGap << G4endl
  //      << "fMirrorZPosition= "<< fMirrorZPosition << G4endl
  //      << "fNMirrors= "<< fNMirrors << G4endl
  //      << "fSubMirrorExternalRadius= "<< fSubMirrorExternalRadius << G4endl
  //      << "fSpotCenter= "<< fSpotCenter << G4endl
  //      << "fConeInputDiameter= "<< fConeInputDiameter << G4endl
  //      << "fConeInputRadius= "<< fConeInputRadius << G4endl
  //      << "fConeOutputDiameter= "<< fConeOutputDiameter << G4endl
  //      << "fConeOutputRadius= "<< fConeOutputRadius << G4endl
  //      << "fConeLongitudinalLenght= "<< fConeLongitudinalLenght << G4endl
  //      << "fMylarConeThickness= "<< fMylarConeThickness << G4endl
  //      << "fMylarConeInputRadius= "<< fMylarConeInputRadius << G4endl
  //      << "fMylarConeOutputRadius= "<< fMylarConeOutputRadius << G4endl
  //      << "fMylarConeLongitudinalLenght= "<< fMylarConeLongitudinalLenght << G4endl
  //      << "fQuartzWindowInnerRadius= "<< fQuartzWindowInnerRadius << G4endl
  //      << "fQuartzWindowOuterRadius= "<< fQuartzWindowOuterRadius << G4endl
  //      << "fQuartzWindowThickness= "<< fQuartzWindowThickness << G4endl
  //      << "fPMWindowInnerRadius= "<< fPMWindowInnerRadius << G4endl
  //      << "fPMWindowOuterRadius= "<< fPMWindowOuterRadius << G4endl
  //      << "fPMWindowThickness= "<< fPMWindowThickness << G4endl
  //      << "fPMInnerRadius= "<< fPMInnerRadius << G4endl
  //      << "fPMOuterRadius= "<< fPMOuterRadius << G4endl
  //      << "fPMLenght= "<< fPMLenght << G4endl
  //      << "fHoneyCombDistance= "<< fHoneyCombDistance << G4endl
  //      << "fRMin= "<< fRMin << G4endl
  //      << "fRMax= "<< fRMax << G4endl
  //      << "fMinDistance= "<< fMinDistance << G4endl
  //      << "fMaxDistance= "<< fMaxDistance << G4endl
  //      << "fPMsIDs= "<< fPMsIDs << G4endl
  //      << "fPMsPositions= "<< fPMsPositions << G4endl
  //      << "fNPMs= "<< fNPMs << G4endl;
}
