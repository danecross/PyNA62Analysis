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
//            Evelina Marinova (Evelina.Marinova@cern.ch)
//
// --------------------------------------------------------------
#ifndef LKrGeometryParameters_H
#define LKrGeometryParameters_H 1

#include "globals.hh"
#include "TObjArray.h"
#include "G4ThreeVector.hh"


#include "NA62VGeometryParameters.hh"

class LKrGeometryParameters : public NA62VGeometryParameters
{

    public:

        ~LKrGeometryParameters();
        static LKrGeometryParameters* GetInstance();
        TObjArray GetHashTable();
        void Print();

    private:

        static LKrGeometryParameters* fInstance;

    protected:

        LKrGeometryParameters();

    public:


  G4double             GetStartLKrResponsibleRegion()                                   { return fStartLKrResponsibleRegion; };
  void                 SetStartLKrResponsibleRegion(G4double value)                     { fStartLKrResponsibleRegion = value; };

  G4double             GetEndLKrResponsibleRegion()                                     { return fEndLKrResponsibleRegion; };
  void                 SetEndLKrResponsibleRegion(G4double value)                       { fEndLKrResponsibleRegion = value; };  

  G4double             GetDistanceWWtoValve()                                            { return fDistanceWWtoValve; };
  void                 SetDistanceWWtoValve(G4double value)                              { fDistanceWWtoValve = value; };

  G4double             GetWorldZLength()                                                 { return fWorldZLength; };
  void                 SetWorldZLength(G4double value)                                   { fWorldZLength = value; };
  
  G4double             GetWorldXLength()                                                 { return fWorldXLength; };
  void                 SetWorldXLength(G4double value)                                   { fWorldXLength = value; };
  
  G4double             GetWorldYLength()                                                 { return fWorldYLength; };
  void                 SetWorldYLength(G4double value)                                   { fWorldYLength = value; };
  
  G4double             GetLKrDetectorZPosition()                                         { return fLKrDetectorZPosition; };
  void                 SetLKrDetectorZPosition(G4double value)                           { fLKrDetectorZPosition = value; };
  
  G4double             GetLKrDetectorZLength()                                           { return fLKrDetectorZLength; };
  void                 SetLKrDetectorZLength(G4double value)                             { fLKrDetectorZLength = value; };
  
  G4double             GetLKrDetectorXLength()                                           { return fLKrDetectorXLength; };
  void                 SetLKrDetectorXLength(G4double value)                             { fLKrDetectorXLength = value; };
  
  G4double             GetLKrDetectorYLength()                                           { return fLKrDetectorYLength; };
  void                 SetLKrDetectorYLength(G4double value)                             { fLKrDetectorYLength = value; };

  G4double             GetThermalContractionConstantG10()                                { return fThermalContractionConstantG10; };
  void                 SetThermalContractionConstantG10(G4double value)                  { fThermalContractionConstantG10 = value; };
  
  
  G4double             GetDistanceFrontPlateBackPlate()                                  { return fDistanceFrontPlateBackPlate; };
  void                 SetDistanceFrontPlateBackPlate(G4double value)                    { fDistanceFrontPlateBackPlate = value; };
  
  G4double             GetFrontWallPositionZ()                                           { return fFrontWallPositionZ; };
  void                 SetFrontWallPositionZ(G4double value)                             { fFrontWallPositionZ = value; };
  
  G4double             GetBackWallPositionZ()                                            { return fBackWallPositionZ; };
  void                 SetBackWallPositionZ(G4double value)                              { fBackWallPositionZ = value; };
  
  G4double             GetHalfZWidth()                                                   { return fHalfZWidth; };
  void                 SetHalfZWidth(G4double value)                                     { fHalfZWidth = value; };
  
  G4double             GetZtr()                                                          { return fZtr; };
  void                 SetZtr(G4double value)                                            { fZtr = value; };
  
  G4double             GetPositionOfWallZ0()                                             { return fPositionOfWallZ0; };
  void                 SetPositionOfWallZ0(G4double value)                               { fPositionOfWallZ0 = value; };
  
  G4double             GetXbackReferenceCell()                                           { return fXbackReferenceCell; };
  void                 SetXbackReferenceCell(G4double value)                             { fXbackReferenceCell = value; };
  
  G4double             GetHalfSizeCentralGap()                                           { return fHalfSizeCentralGap; };
  void                 SetHalfSizeCentralGap(G4double value)                             {  fHalfSizeCentralGap= value; };
  
  G4double             GetYbackReferenceCell()                                           { return fYbackReferenceCell; };
  void                 SetYbackReferenceCell(G4double value)                             { fYbackReferenceCell = value; };
  
  G4double             GetXbackProjectivityReferenceCell()                               { return fXbackProjectivityReferenceCell; };
  void                 SetXbackProjectivityReferenceCell(G4double value)                 { fXbackProjectivityReferenceCell = value; };
  
  G4double             GetYbackProjectivityReferenceCell()                               { return fYbackProjectivityReferenceCell; };
  void                 SetYbackProjectivityReferenceCell(G4double value)                 { fYbackProjectivityReferenceCell = value; };
  
  G4double             GetXfrontReferenceCell()                                          { return fXfrontReferenceCell; };
  void                 SetXfrontReferenceCell(G4double value)                            { fXfrontReferenceCell = value; };
  
  G4double             GetYfrontReferenceCell()                                          { return fYfrontReferenceCell; };
  void                 SetYfrontReferenceCell(G4double value)                            { fYfrontReferenceCell = value; };
  
  G4double             GetProjectivityPointPositionZ()                                   { return fProjectivityPointPositionZ; };
  void                 SetProjectivityPointPositionZ(G4double value)                     { fProjectivityPointPositionZ = value;  };
  
  G4double             GetSpaceToNextPlate()                                             { return SpaceToNextPlate; };
  void                 SetSpaceToNextPlate(G4double value)                               { SpaceToNextPlate = value; };
  
  G4double             GetProjectivityAxisProjectionZ()                                  { return fProjectivityAxisProjectionZ; };
  void                 SetProjectivityAxisProjectionZ(G4double value)                    { fProjectivityAxisProjectionZ = value; };
  
  G4double             GetIncr()                                                         { return fIncr; };
  void                 SetIncr(G4double value)                                           { fIncr = value; };
  
  G4double             GetHalfCellSizeAtFrontWall()                                      { return fHalfCellSizeAtFrontWall; };
  void                 SetHalfCellSizeAtFrontWall(G4double value)                        { fHalfCellSizeAtFrontWall = value; };
  
  G4double             GetDistanceToNextElectrodeFrontX()                                { return fDistanceToNextElectrodeFrontX; };
  void                 SetDistanceToNextElectrodeFrontX(G4double value)                  { fDistanceToNextElectrodeFrontX = value; };
  
  G4double             GetDistanceToNextElectrodeBackX()                                 { return fDistanceToNextElectrodeBackX; };
  void                 SetDistanceToNextElectrodeBackX(G4double value)                   { fDistanceToNextElectrodeBackX = value; };
  
  G4double             GetDistanceToNextElectrodeFrontY()                                { return fDistanceToNextElectrodeFrontY; };
  void                 SetDistanceToNextElectrodeFrontY(G4double value)                  { fDistanceToNextElectrodeFrontY = value; };
  
  G4double             GetDistanceToNextElectrodeBackY()                                 { return fDistanceToNextElectrodeBackY; };
  void                 SetDistanceToNextElectrodeBackY(G4double value)                   { fDistanceToNextElectrodeBackY = value; };
  
  G4double             GetHalfXWidthF()                                                  { return fHalfXWidthF; };
  void                 SetHalfXWidthF(G4double value)                                    { fHalfXWidthF = value; };
  
  G4double             GetHalfYWidthF()                                                  { return fHalfYWidthF; };
  void                 SetHalfYWidthF(G4double value)                                    { fHalfYWidthF = value; };
  
  G4double             GetHalfXWidthB()                                                  { return fHalfXWidthB; };
  void                 SetHalfXWidthB(G4double value)                                    { fHalfXWidthB = value; };
  
  G4double             GetHalfYWidthB()                                                  { return fHalfYWidthB; };
  void                 SetHalfYWidthB(G4double value)                                    { fHalfYWidthB = value; };
  
  G4double             GetSizeHoleX()                                                    { return fSizeHoleX; };
  void                 SetSizeHoleX(G4double value)                                      { fSizeHoleX = value; };
  
  G4double             GetSizeHoleY()                                                    { return fSizeHoleY; };
  void                 SetSizeHoleY(G4double value)                                      { fSizeHoleY = value; };
  
  G4double             GetSizeHoleYHalfCell()                                            { return fSizeHoleYHalfCell; };
  void                 SetSizeHoleYHalfCell(G4double value)                              { fSizeHoleYHalfCell = value; };
  
  G4double             GetHalfSizeFrontWallSegmentZ()                                    { return fHalfSizeFrontWallSegmentZ; };
  void                 SetHalfSizeFrontWallSegmentZ(G4double value)                      { fHalfSizeFrontWallSegmentZ = value; };
  
  G4double             GetHalfSizeSpacerWallSegmentZ()                                   { return fHalfSizeSpacerWallSegmentZ; };
  void                 SetHalfSizeSpacerWallSegmentZ(G4double value)                     { fHalfSizeSpacerWallSegmentZ = value; };
  
  G4double             GetHalfSizeBackWallSegmentZ()                                     { return fHalfSizeBackWallSegmentZ; };
  void                 SetHalfSizeBackWallSegmentZ(G4double value)                       { fHalfSizeBackWallSegmentZ = value; };
  
  G4double             GetHalfSizeWallVSegmentY()                                        { return fHalfSizeWallVSegmentY; };
  void                 SetHalfSizeWallVSegmentY(G4double value)                          { fHalfSizeWallVSegmentY = value; };
  
  G4double             GetHalfSizeIrregularWallVSegmentY()                               { return fHalfSizeIrregularWallVSegmentY; };
  void                 SetHalfSizeIrregularWallVSegmentY(G4double value)                 { fHalfSizeIrregularWallVSegmentY = value; };
  
  G4double             GetZigHalfSizeX()                                                 { return fZigHalfSizeX; };
  void                 SetZigHalfSizeX(G4double value)                                   { fZigHalfSizeX = value; };
  
  G4double             GetZigHalfSizeY()                                                 { return fZigHalfSizeY;  };
  void                 SetZigHalfSizeY(G4double value)                                   { fZigHalfSizeY = value;  };
  
  G4double             GetZigHalfSizeZ()                                                 { return fZigHalfSizeZ; };
  void                 SetZigHalfSizeZ(G4double value)                                   { fZigHalfSizeZ = value; };
  
  G4double             GetIrrZigHalfSizeX()                                              { return fIrrZigHalfSizeX; };
  void                 SetIrrZigHalfSizeX(G4double value)                                { fIrrZigHalfSizeX = value; };
  
  G4double             GetIrrZigHalfSizeY()                                              { return fIrrZigHalfSizeY; };
  void                 SetIrrZigHalfSizeY(G4double value)                                { fIrrZigHalfSizeY = value; };
  
  G4double             GetIrrZigHalfSizeZ()                                              { return fIrrZigHalfSizeZ; };
  void                 SetIrrZigHalfSizeZ(G4double value)                                { fIrrZigHalfSizeZ = value; };
  
  G4double             GetHalfIrregularCellSizeAtBackWallY()                             { return fHalfIrregularCellSizeAtBackWallY; };
  void                 SetHalfIrregularCellSizeAtBackWallY(G4double value)               { fHalfIrregularCellSizeAtBackWallY = value; };
  
  G4double             GetHalfIrregularCellSizeAtFrontWallY()                            { return fHalfIrregularCellSizeAtFrontWallY; };
  void                 SetHalfIrregularCellSizeAtFrontWallY(G4double value)              { fHalfIrregularCellSizeAtFrontWallY = value;  };
  
  G4double             GetHalfYWidthIrregularB()                                         { return fHalfYWidthIrregularB; };
  void                 SetHalfYWidthIrregularB(G4double value)                           { fHalfYWidthIrregularB = value; };
  
  G4double             GetHalfYWidthIrregularF()                                         { return fHalfYWidthIrregularF; };
  void                 SetHalfYWidthIrregularF(G4double value)                           { fHalfYWidthIrregularF = value; };
  
  G4int                GetHalfNElectrodesX()                                             { return fHalfNElectrodesX; };
  void                 SetHalfNElectrodesX(G4int value)                                  { fHalfNElectrodesX = value; };
  
  G4int                GetHalfNElectrodesY()                                             { return fHalfNElectrodesY; };
  void                 SetHalfNElectrodesY(G4int value)                                  { fHalfNElectrodesY = value; };
  
  G4int                GetNRowsFullNumberElectrodes()                                    { return fNRowsFullNumberElectrodes; };
  void                 SetNRowsFullNumberElectrodes(G4int value)                         { fNRowsFullNumberElectrodes = value; };
  
  G4double             GetNHODHalfLenghtG10Z()                                           { return fNHODHalfLenghtG10Z; };
  void                 SetNHODHalfLenghtG10Z(G4double value)                             { fNHODHalfLenghtG10Z = value; };
  
  G4double             GetNHODHalfLenghtScintillatorZ()                                  { return fNHODHalfLenghtScintillatorZ; };
  void                 SetNHODHalfLenghtScintillatorZ(G4double value)                    { fNHODHalfLenghtScintillatorZ = value; };
  
  G4double             GetRHoleX()                                                       { return fRHoleX; };
  void                 SetRHoleX(G4double value)                                         { fRHoleX = value; };
  
  G4double             GetRHoleY()                                                       { return fRHoleY; };
  void                 SetRHoleY(G4double value)                                         { fRHoleY = value; };
  
  G4double             GetSpaceFromWallToElectrodeAtWallX()                              { return fSpaceFromWallToElectrodeAtWallX; };
  void                 SetSpaceFromWallToElectrodeAtWallX(G4double value)                { fSpaceFromWallToElectrodeAtWallX = value; };
  
  G4double             GetInnerCryostatMinRadius()                                       { return fInnerCryostatMinRadius; };
  void                 SetInnerCryostatMinRadius(G4double value)                         { fInnerCryostatMinRadius = value; };
  
  G4double             GetThicknessSliceColdWindow()                                     { return fThicknessSliceColdWindow; };
  void                 SetThicknessSliceColdWindow(G4double value)                       { fThicknessSliceColdWindow = value; };
  
  G4double             GetThicknessSliceBackColdWindow()                                 { return fThicknessSliceBackColdWindow; };
  void                 SetThicknessSliceBackColdWindow(G4double value)                   { fThicknessSliceBackColdWindow = value; };
  
  G4double             GetThicknessOfInnerCryo()                                         { return fThicknessOfInnerCryo; };
  void                 SetThicknessOfInnerCryo(G4double value)                           { fThicknessOfInnerCryo = value;  };
  
  G4double             GetOuterCryostatMinRadius()                                       { return fOuterCryostatMinRadius; };
  void                 SetOuterCryostatMinRadius(G4double value)                         { fOuterCryostatMinRadius = value; };
  
  G4double             GetThicknessOfOuterCryo()                                         { return fThicknessOfOuterCryo; };
  void                 SetThicknessOfOuterCryo(G4double value)                           { fThicknessOfOuterCryo = value; };
  
  G4double             GetInnerBeamPipeRadius()                                          { return fInnerBeamPipeRadius; };
  void                 SetInnerBeamPipeRadius(G4double value)                            { fInnerBeamPipeRadius = value; };
  
  G4double             GetOuterBeamPipeRadius()                                          { return fOuterBeamPipeRadius; };
  void                 SetOuterBeamPipeRadius(G4double value)                            { fOuterBeamPipeRadius = value; };
  
  G4double             GetRadiusHoleSpacerPlate()                                        { return fRadiusHoleSpacerPlate; };
  void                 SetRadiusHoleSpacerPlate(G4double value)                          { fRadiusHoleSpacerPlate = value; };
  
  G4double             GetLongitudinalLengthInnerCryo()                                  { return fLongitudinalLengthInnerCryo; };
  void                 SetLongitudinalLengthInnerCryo(G4double value)                    { fLongitudinalLengthInnerCryo = value; };
  
  G4double             GetLongitudinalLengthOuterCryo()                                  { return fLongitudinalLengthOuterCryo; };
  void                 SetLongitudinalLengthOuterCryo(G4double value)                    { fLongitudinalLengthOuterCryo = value; };
  
  
  G4double             GetThicknessSteelFoilColdWindow()                                 { return fThicknessSteelFoilColdWindow; };
  void                 SetThicknessSteelFoilColdWindow(G4double value)                   { fThicknessSteelFoilColdWindow = value; };
  
  G4double             GetRadiusSteelFoilColdWindow()                                    { return fRadiusSteelFoilColdWindow; };
  void                 SetRadiusSteelFoilColdWindow(G4double value)                      { fRadiusSteelFoilColdWindow = value; };
  
  G4double             GetRadiusFrontColdWindow()                                        { return fRadiusFrontColdWindow; };
  void                 SetRadiusFrontColdWindow(G4double value)                          { fRadiusFrontColdWindow = value; };
  
  G4double             GetRadiusBackColdWindow()                                         { return fRadiusBackColdWindow; };
  void                 SetRadiusBackColdWindow(G4double value)                           { fRadiusBackColdWindow = value; };
  
  
  G4double             GetRadiusFrontColdWindowBeamPipe()                                { return fRadiusFrontColdWindowBeamPipe; };
  void                 SetRadiusFrontColdWindowBeamPipe(G4double value)                  { fRadiusFrontColdWindowBeamPipe = value; };
  
  G4double             GetThicknesConvexColdWindow()                                     { return fThicknesConvexColdWindow; };
  void                 SetThicknesConvexColdWindow(G4double value)                       { fThicknesConvexColdWindow = value; };
  
  G4double             GetThicknesConvexColdBackWindow()                                 { return fThicknesConvexColdBackWindow; };
  void                 SetThicknesConvexColdBackWindow(G4double value)                   { fThicknesConvexColdBackWindow = value; };
  
  G4double             GetBEATCHPositionFrontWarmWindowEdge()                            { return fBEATCHPositionFrontWarmWindowEdge; };
  void                 SetBEATCHPositionFrontWarmWindowEdge(G4double value)              { fBEATCHPositionFrontWarmWindowEdge = value; };
  
  G4double             GetBEATCHPositionFrontEdgeOuterVessel()                           { return fBEATCHPositionFrontEdgeOuterVessel; };
  void                 SetBEATCHPositionFrontEdgeOuterVessel(G4double value)             { fBEATCHPositionFrontEdgeOuterVessel = value; };
  
  G4double             GetBEATCHPositionOuterWarmFlange1()                               { return fBEATCHPositionOuterWarmFlange1; };
  void                 SetBEATCHPositionOuterWarmFlange1(G4double value)                 { fBEATCHPositionOuterWarmFlange1 = value; };
  
  G4double             GetBEATCHPositionBackEdgeOuterVessel()                            { return fBEATCHPositionBackEdgeOuterVessel; };
  void                 SetBEATCHPositionBackEdgeOuterVessel(G4double value)              { fBEATCHPositionBackEdgeOuterVessel = value; };
  
  G4double             GetBEATCHPositionBackWarmWindowEdge()                             { return fBEATCHPositionBackWarmWindowEdge; };
  void                 SetBEATCHPositionBackWarmWindowEdge(G4double value)               { fBEATCHPositionBackWarmWindowEdge = value; };
  
  G4double             GetVerticalRadiusWarmWindow()                                     { return fVerticalRadiusWarmWindow; };
  void                 SetVerticalRadiusWarmWindow(G4double value)                       { fVerticalRadiusWarmWindow = value; };
  
  G4double             GetRadiusWarmWindow()                                             { return fRadiusWarmWindow; };
  void                 SetRadiusWarmWindow(G4double value)                               { fRadiusWarmWindow = value; };
  
  G4double             GetThicknesConvexWarmWindow()                                     { return fThicknesConvexWarmWindow; };
  void                 SetThicknesConvexWarmWindow(G4double value)                       { fThicknesConvexWarmWindow = value; };
  
  G4double             GetInnerDistanceFrontBackWarmWindow()                             { return fInnerDistanceFrontBackWarmWindow; };
  void                 SetInnerDistanceFrontBackWarmWindow(G4double value)               { fInnerDistanceFrontBackWarmWindow = value; };
  
  G4double             GetDistanceFrontWarmWindowLKr()                                   { return fDistanceFrontWarmWindowLKr; };
  void                 SetDistanceFrontWarmWindowLKr(G4double value)                     { fDistanceFrontWarmWindowLKr = value; };
  
  G4double             GetDistanceBackWarmWindowLKr()                                    { return fDistanceBackWarmWindowLKr; };
  void                 SetDistanceBackWarmWindowLKr(G4double value)                      { fDistanceBackWarmWindowLKr = value; };
  
  G4double             GetLongitudinalLengthBeamPipe()                                   { return fLongitudinalLengthBeamPipe; };
  void                 SetLongitudinalLengthBeamPipe(G4double value)                     { fLongitudinalLengthBeamPipe = value; };
  
  G4double             GetDistanceColdWindowToMonotube()                                 { return fDistanceColdWindowToMonotube; };
  void                 SetDistanceColdWindowToMonotube(G4double value)                   { fDistanceColdWindowToMonotube = value; };
  
  G4double             GetDistanceMonotubeToothFlange()                                  { return fDistanceMonotubeToothFlange; };
  void                 SetDistanceMonotubeToothFlange(G4double value)                    { fDistanceMonotubeToothFlange = value; };
  
  G4double             GetHightToothFlange()                                             { return fHightToothFlange; };
  void                 SetHightToothFlange(G4double value)                               { fHightToothFlange = value; };
  
  G4double             GetLengthToothFlange()                                            { return fLengthToothFlange; };
  void                 SetLengthToothFlange(G4double value)                              { fLengthToothFlange = value; };
  
  G4double             GetDistanceMonotubeBellowToothFlange()                            { return fDistanceMonotubeBellowToothFlange; };
  void                 SetDistanceMonotubeBellowToothFlange(G4double value)              { fDistanceMonotubeBellowToothFlange = value; };
  
  G4double             GetHightBellowToothFlange()                                       { return fHightBellowToothFlange; };
  void                 SetHightBellowToothFlange(G4double value)                         { fHightBellowToothFlange = value; };
  
  G4double             GetLengthBellowToothFlange()                                      { return fLengthBellowToothFlange; };
  void                 SetLengthBellowToothFlange(G4double value)                        { fLengthBellowToothFlange = value; };
  
  G4double             GetDistanceBellowToothFlangeAtBackToRareWindow()                  { return fDistanceBellowToothFlangeAtBackToRareWindow; };
  void                 SetDistanceBellowToothFlangeAtBackToRareWindow(G4double value)    { fDistanceBellowToothFlangeAtBackToRareWindow = value; };
  
  G4double             GetHightFlange1AtColdWindowBeam()                                 { return fHightFlange1AtColdWindowBeam; };
  void                 SetHightFlange1AtColdWindowBeam(G4double value)                   { fHightFlange1AtColdWindowBeam = value; };
  
  G4double             GetLengthFlange1AtColdWindowBeam()                                { return fLengthFlange1AtColdWindowBeam; };
  void                 SetLengthFlange1AtColdWindowBeam(G4double value)                  { fLengthFlange1AtColdWindowBeam = value; };
  
  G4double             GetMinRadiusFlange1AtColdWindowBeam()                             { return fMinRadiusFlange1AtColdWindowBeam; };
  void                 SetMinRadiusFlange1AtColdWindowBeam(G4double value)               { fMinRadiusFlange1AtColdWindowBeam = value; };
  
  G4double             GetHightFlange2AtColdWindowBeam()                                 { return fHightFlange2AtColdWindowBeam; };
  void                 SetHightFlange2AtColdWindowBeam(G4double value)                   { fHightFlange2AtColdWindowBeam = value; };

  G4double             GetLengthFlange2AtColdWindowBeam()                                 { return fLengthFlange2AtColdWindowBeam; };
  void                 SetLengthFlange2AtColdWindowBeam(G4double value)                   { fLengthFlange2AtColdWindowBeam = value; };
  
  G4double             GetHightFlange3AtColdWindowBeam()                                  { return fHightFlange3AtColdWindowBeam; };
  void                 SetHightFlange3AtColdWindowBeam(G4double value)                    { fHightFlange3AtColdWindowBeam = value; };
  
  G4double             GetLengthFlange3AtColdWindowBeam()                                 { return fLengthFlange3AtColdWindowBeam; };
  void                 SetLengthFlange3AtColdWindowBeam(G4double value)                   { fLengthFlange3AtColdWindowBeam = value; };
  
  G4double             GetHightFlange4AtColdWindowBeam()                                  { return fHightFlange4AtColdWindowBeam; };
  void                 SetHightFlange4AtColdWindowBeam(G4double value)                    { fHightFlange4AtColdWindowBeam = value; };
  
  G4double             GetLengthFlange4AtColdWindowBeam()                                 { return fLengthFlange4AtColdWindowBeam; };
  void                 SetLengthFlange4AtColdWindowBeam(G4double value)                   { fLengthFlange4AtColdWindowBeam = value; };
  
  G4double             GetMinRadiusFlange4AtColdWindowBeam()                              { return fMinRadiusFlange4AtColdWindowBeam; };
  void                 SetMinRadiusFlange4AtColdWindowBeam(G4double value)                { fMinRadiusFlange4AtColdWindowBeam = value; };
  
  G4double             GetLengthHolesBoltsBackColdInFlange5()                             { return fLengthHolesBoltsBackColdInFlange5; };
  void                 SetLengthHolesBoltsBackColdInFlange5(G4double value)               { fLengthHolesBoltsBackColdInFlange5 = value; };
  
  G4double             GetLengthHolesBoltsBackColdInFlange4()                             { return fLengthHolesBoltsBackColdInFlange4; };
  void                 SetLengthHolesBoltsBackColdInFlange4(G4double value)               { fLengthHolesBoltsBackColdInFlange4 = value; };
  
  G4double             GetRadiusHolesBoltsFrontCold()                                     { return fRadiusHolesBoltsFrontCold; };
  void                 SetRadiusHolesBoltsFrontCold(G4double value)                       { fRadiusHolesBoltsFrontCold = value; };
  
  G4double             GetLengthHolesBoltsFrontCold()                                     { return fLengthHolesBoltsFrontCold; };
  void                 SetLengthHolesBoltsFrontCold(G4double value)                       { fLengthHolesBoltsFrontCold = value; };
  
  G4double             GetRadiusHeadsBoltsFrontCold()                                     { return fRadiusHeadsBoltsFrontCold; };
  void                 SetRadiusHeadsBoltsFrontCold(G4double value)                       { fRadiusHeadsBoltsFrontCold = value; };
  
  G4double             GetLengthHeadsBoltsFrontCold()                                     { return fLengthHeadsBoltsFrontCold; };
  void                 SetLengthHeadsBoltsFrontCold(G4double value)                       { fLengthHeadsBoltsFrontCold = value; };
  
  G4double             GetDistanceBoltToBeamPipe()                                        { return fDistanceBoltToBeamPipe; };
  void                 SetDistanceBoltToBeamPipe(G4double value)                          { fDistanceBoltToBeamPipe = value; };
  
  G4double             GetAngleCentralAxisBolt()                                          { return fAngleCentralAxisBolt; };
  void                 SetAngleCentralAxisBolt(G4double value)                            { fAngleCentralAxisBolt = value; };  

  G4double             GetHightFlange1AtColdWindowOut()                                   { return fHightFlange1AtColdWindowOut; };
  void                 SetHightFlange1AtColdWindowOut(G4double value)                     { fHightFlange1AtColdWindowOut = value; };
  
  G4double             GetLengthFlange1AtColdWindowOut()                                  { return fLengthFlange1AtColdWindowOut; };
  void                 SetLengthFlange1AtColdWindowOut(G4double value)                    { fLengthFlange1AtColdWindowOut = value; };
  
  G4double             GetMinRadiusFlange1AtColdWindowOut()                               { return fMinRadiusFlange1AtColdWindowOut; };
  void                 SetMinRadiusFlange1AtColdWindowOut(G4double value)                 { fMinRadiusFlange1AtColdWindowOut = value; };
  
  G4double             GetHightFlange2AtColdWindowOut()                                   { return fHightFlange2AtColdWindowOut; };
  void                 SetHightFlange2AtColdWindowOut(G4double value)                     { fHightFlange2AtColdWindowOut = value; };
  
  G4double             GetLengthFlange2AtColdWindowOut()                                  { return fLengthFlange2AtColdWindowOut; };
  void                 SetLengthFlange2AtColdWindowOut(G4double value)                    { fLengthFlange2AtColdWindowOut = value; };
  
  G4double             GetMinRadiusFlange2AtColdWindowOut()                               { return fMinRadiusFlange2AtColdWindowOut; };
  void                 SetMinRadiusFlange2AtColdWindowOut(G4double value)                 { fMinRadiusFlange2AtColdWindowOut = value; };
  
  G4double             GetDistanceFlange2AtColdWindowOutFaceToSteelFoil()                 { return fDistanceFlange2AtColdWindowOutFaceToSteelFoil; };
  void                 SetDistanceFlange2AtColdWindowOutFaceToSteelFoil(G4double value)   { fDistanceFlange2AtColdWindowOutFaceToSteelFoil = value; };
  
  G4double             GetHightBackFlange1AtColdWindowBeam()                              { return fHightBackFlange1AtColdWindowBeam; };
  void                 SetHightBackFlange1AtColdWindowBeam(G4double value)                { fHightBackFlange1AtColdWindowBeam = value; };
  
  G4double             GetLengthBackFlange1AtColdWindowBeam()                             { return fLengthBackFlange1AtColdWindowBeam; };
  void                 SetLengthBackFlange1AtColdWindowBeam(G4double value)               { fLengthBackFlange1AtColdWindowBeam = value; };
  
  G4double             GetMinRadiusBackFlange1AtColdWindowBeam()                          { return fMinRadiusBackFlange1AtColdWindowBeam; };
  void                 SetMinRadiusBackFlange1AtColdWindowBeam(G4double value)            { fMinRadiusBackFlange1AtColdWindowBeam = value; };
  
  G4double             GetHightBackFlange2AtColdWindowBeam()                              { return fHightBackFlange2AtColdWindowBeam; };
  void                 SetHightBackFlange2AtColdWindowBeam(G4double value)                { fHightBackFlange2AtColdWindowBeam = value; };
  
  G4double             GetLengthBackFlange2AtColdWindowBeam()                             { return fLengthBackFlange2AtColdWindowBeam; };
  void                 SetLengthBackFlange2AtColdWindowBeam(G4double value)               { fLengthBackFlange2AtColdWindowBeam = value; };
  
  G4double             GetMinRadiusBackFlange2AtColdWindowBeam()                          { return fMinRadiusBackFlange2AtColdWindowBeam; };
  void                 SetMinRadiusBackFlange2AtColdWindowBeam(G4double value)            { fMinRadiusBackFlange2AtColdWindowBeam = value; };
  
  G4double             GetHightBackFlange3AtColdWindowBeam()                              { return fHightBackFlange3AtColdWindowBeam; };
  void                 SetHightBackFlange3AtColdWindowBeam(G4double value)                { fHightBackFlange3AtColdWindowBeam = value; };
  
  G4double             GetLengthBackFlange3AtColdWindowBeam()                             { return fLengthBackFlange3AtColdWindowBeam; };
  void                 SetLengthBackFlange3AtColdWindowBeam(G4double value)               { fLengthBackFlange3AtColdWindowBeam = value; };
  
  G4double             GetMinRadiusBackFlange3AtColdWindowBeam()                          { return fMinRadiusBackFlange3AtColdWindowBeam; };
  void                 SetMinRadiusBackFlange3AtColdWindowBeam(G4double value)            { fMinRadiusBackFlange3AtColdWindowBeam = value; };
  
  G4double             GetHightBackFlange4AtColdWindowBeam()                              { return fHightBackFlange4AtColdWindowBeam; };
  void                 SetHightBackFlange4AtColdWindowBeam(G4double value)                { fHightBackFlange4AtColdWindowBeam = value; };
  
  G4double             GetLengthBackFlange4AtColdWindowBeam()                             { return fLengthBackFlange4AtColdWindowBeam; };
  void                 SetLengthBackFlange4AtColdWindowBeam(G4double value)               { fLengthBackFlange4AtColdWindowBeam = value; };
  
  G4double             GetMinRadiusBackFlange4AtColdWindowBeam()                          { return fMinRadiusBackFlange4AtColdWindowBeam; };
  void                 SetMinRadiusBackFlange4AtColdWindowBeam(G4double value)            { fMinRadiusBackFlange4AtColdWindowBeam = value; };
  
  G4double             GetZDistanceBetweeBackEdges3and4()                                 { return fZDistanceBetweeBackEdges3and4; };
  void                 SetZDistanceBetweeBackEdges3and4(G4double value)                   { fZDistanceBetweeBackEdges3and4 = value; };
  
  G4double             GetHightBackFlange5AtColdWindowBeam()                              { return fHightBackFlange5AtColdWindowBeam; };
  void                 SetHightBackFlange5AtColdWindowBeam(G4double value)                { fHightBackFlange5AtColdWindowBeam = value; };
  
  G4double             GetLengthBackFlange5AtColdWindowBeam()                             { return fLengthBackFlange5AtColdWindowBeam; };
  void                 SetLengthBackFlange5AtColdWindowBeam(G4double value)               { fLengthBackFlange5AtColdWindowBeam = value; };
  
  G4double             GetMinRadiusBackFlange5AtColdWindowBeam()                          { return fMinRadiusBackFlange5AtColdWindowBeam; };
  void                 SetMinRadiusBackFlange5AtColdWindowBeam(G4double value)            { fMinRadiusBackFlange5AtColdWindowBeam = value; };
  
  G4double             GetHightBackFlangeAtColdWindowOut()                                { return fHightBackFlangeAtColdWindowOut; };
  void                 SetHightBackFlangeAtColdWindowOut(G4double value)                  { fHightBackFlangeAtColdWindowOut = value; };
  
  G4double             GetLengthBackFlangeAtColdWindowOut()                               { return fLengthBackFlangeAtColdWindowOut; };
  void                 SetLengthBackFlangeAtColdWindowOut(G4double value)                 { fLengthBackFlangeAtColdWindowOut = value; };
  
  G4double             GetMinRadiusBackFlangeAtColdWindowOut()                            { return fMinRadiusBackFlangeAtColdWindowOut; };
  void                 SetMinRadiusBackFlangeAtColdWindowOut(G4double value)              { fMinRadiusBackFlangeAtColdWindowOut = value; };
  
  G4double             GetDistanceFaceBackFlangeOutToColdWindow()                         { return fDistanceFaceBackFlangeOutToColdWindow; };
  void                 SetDistanceFaceBackFlangeOutToColdWindow(G4double value)           { fDistanceFaceBackFlangeOutToColdWindow = value; };
  
  G4double             GetHightFlangeAtWarmWindow1Beam()                                  { return fHightFlangeAtWarmWindow1Beam; };
  void                 SetHightFlangeAtWarmWindow1Beam(G4double value)                    { fHightFlangeAtWarmWindow1Beam = value; };
  
  G4double             GetLengthFlangeAtWarmWindow1Beam()                                 { return fLengthFlangeAtWarmWindow1Beam; };
  void                 SetLengthFlangeAtWarmWindow1Beam(G4double value)                   { fLengthFlangeAtWarmWindow1Beam = value; };
  
  G4double             GetMinRadiusFlangeAtWarmWindow1Beam()                              { return fMinRadiusFlangeAtWarmWindow1Beam; };
  void                 SetMinRadiusFlangeAtWarmWindow1Beam(G4double value)                { fMinRadiusFlangeAtWarmWindow1Beam = value; };
  
  G4double             GetHightFlangeAtWarmWindow2Beam()                                  { return fHightFlangeAtWarmWindow2Beam; };
  void                 SetHightFlangeAtWarmWindow2Beam(G4double value)                    { fHightFlangeAtWarmWindow2Beam = value; };
  
  G4double             GetLengthFlangeAtWarmWindow2Beam()                                 { return fLengthFlangeAtWarmWindow2Beam; };
  void                 SetLengthFlangeAtWarmWindow2Beam(G4double value)                   { fLengthFlangeAtWarmWindow2Beam = value; };
  
  G4double             GetMinRadiusFlangeAtWarmWindow2Beam()                              { return fMinRadiusFlangeAtWarmWindow2Beam; };
  void                 SetMinRadiusFlangeAtWarmWindow2Beam(G4double value)                { fMinRadiusFlangeAtWarmWindow2Beam = value; };
  
  G4double             GetHightFlangeAtWarmWindow3Beam()                                  { return fHightFlangeAtWarmWindow3Beam; };
  void                 SetHightFlangeAtWarmWindow3Beam(G4double value)                    { fHightFlangeAtWarmWindow3Beam = value; };
  
  G4double             GetLengthFlangeAtWarmWindow3Beam()                                 { return fLengthFlangeAtWarmWindow3Beam; };
  void                 SetLengthFlangeAtWarmWindow3Beam(G4double value)                   { fLengthFlangeAtWarmWindow3Beam = value; };
  
  G4double             GetMinRadiusFlangeAtWarmWindow3Beam()                              { return fMinRadiusFlangeAtWarmWindow3Beam; };
  void                 SetMinRadiusFlangeAtWarmWindow3Beam(G4double value)                { fMinRadiusFlangeAtWarmWindow3Beam = value; };
  
  G4double             GetElipticalCutFlange3LongAxis()                                   { return fElipticalCutFlange3LongAxis; };
  void                 SetElipticalCutFlange3LongAxis(G4double value)                     { fElipticalCutFlange3LongAxis = value; };
  
  G4double             GetElipticalCutFlange3ShortAxis()                                  { return fElipticalCutFlange3ShortAxis; };
  void                 SetElipticalCutFlange3ShortAxis(G4double value)                    { fElipticalCutFlange3ShortAxis = value; };
  
  G4double             GetElectronicsBackG10HalfLength()                                  { return fElectronicsBackG10HalfLength; };
  void                 SetElectronicsBackG10HalfLength(G4double value)                    { fElectronicsBackG10HalfLength = value; };
  
  G4double             GetElectronicsBackCuHalfLength()                                   { return fElectronicsBackCuHalfLength; };
  void                 SetElectronicsBackCuHalfLength(G4double value)                     { fElectronicsBackCuHalfLength = value; };
  
  G4double             GetElectronicsBackBrassHalfLength()                                { return fElectronicsBackBrassHalfLength; };
  void                 SetElectronicsBackBrassHalfLength(G4double value)                  { fElectronicsBackBrassHalfLength = value; };
  
  G4double             GetElectronicsBackTeflonHalfLength()                               { return fElectronicsBackTeflonHalfLength; };
  void                 SetElectronicsBackTeflonHalfLength(G4double value)                 { fElectronicsBackTeflonHalfLength = value; };
  
  G4double             GetHightFlangeAtWarmWindow4Beam()                                  { return fHightFlangeAtWarmWindow4Beam; };
  void                 SetHightFlangeAtWarmWindow4Beam(G4double value)                    { fHightFlangeAtWarmWindow4Beam = value; };
  
  G4double             GetLengthFlangeAtWarmWindow4Beam()                                 { return fLengthFlangeAtWarmWindow4Beam; };
  void                 SetLengthFlangeAtWarmWindow4Beam(G4double value)                   { fLengthFlangeAtWarmWindow4Beam = value; };
  
  G4double             GetMinRadiusFlangeAtWarmWindow4Beam()                              { return fMinRadiusFlangeAtWarmWindow4Beam; };
  void                 SetMinRadiusFlangeAtWarmWindow4Beam(G4double value)                { fMinRadiusFlangeAtWarmWindow4Beam = value; };
  
  G4double             GetHightFlangeAtWarmWindow5Beam()                                  { return fHightFlangeAtWarmWindow5Beam; };
  void                 SetHightFlangeAtWarmWindow5Beam(G4double value)                    { fHightFlangeAtWarmWindow5Beam = value; };
  
  G4double             GetLengthFlangeAtWarmWindow5Beam()                                 { return fLengthFlangeAtWarmWindow5Beam; };
  void                 SetLengthFlangeAtWarmWindow5Beam(G4double value)                   { fLengthFlangeAtWarmWindow5Beam = value; };
  
  G4double             GetMinRadiusFlangeAtWarmWindow5Beam()                              { return fMinRadiusFlangeAtWarmWindow5Beam; };
  void                 SetMinRadiusFlangeAtWarmWindow5Beam(G4double value)                { fMinRadiusFlangeAtWarmWindow5Beam = value; };
  
  G4double             GetDistanceWarmWindowFlange2EndFlange3Front()                      { return fDistanceWarmWindowFlange2EndFlange3Front; };
  void                 SetDistanceWarmWindowFlange2EndFlange3Front(G4double value)        { fDistanceWarmWindowFlange2EndFlange3Front = value; };
  
  G4double             GetHightFlangeAtWarmWindow1Out()                                   { return fHightFlangeAtWarmWindow1Out; };
  void                 SetHightFlangeAtWarmWindow1Out(G4double value)                     { fHightFlangeAtWarmWindow1Out = value; };
  
  G4double             GetLengthFlangeAtWarmWindow1Out()                                  { return fLengthFlangeAtWarmWindow1Out; };
  void                 SetLengthFlangeAtWarmWindow1Out(G4double value)                    { fLengthFlangeAtWarmWindow1Out = value; };
  
  G4double             GetMinRadiusFlangeAtWarmWindow1Out()                               { return fMinRadiusFlangeAtWarmWindow1Out; };
  void                 SetMinRadiusFlangeAtWarmWindow1Out(G4double value)                 { fMinRadiusFlangeAtWarmWindow1Out = value; };
  
  G4double             GetHightFlangeAtWarmWindow2Out()                                   { return fHightFlangeAtWarmWindow2Out; };
  void                 SetHightFlangeAtWarmWindow2Out(G4double value)                     { fHightFlangeAtWarmWindow2Out = value; };
  
  G4double             GetLengthFlangeAtWarmWindow2Out()                                  { return fLengthFlangeAtWarmWindow2Out; };
  void                 SetLengthFlangeAtWarmWindow2Out(G4double value)                    { fLengthFlangeAtWarmWindow2Out = value; };
  
  G4double             GetMinRadiusFlangeAtWarmWindow2Out()                               { return fMinRadiusFlangeAtWarmWindow2Out; };
  void                 SetMinRadiusFlangeAtWarmWindow2Out(G4double value)                 { fMinRadiusFlangeAtWarmWindow2Out = value; };
  
  G4double             GetDistanceWarmWindowFlange4()                                     { return fDistanceWarmWindowFlange4; };
  void                 SetDistanceWarmWindowFlange4(G4double value)                       { fDistanceWarmWindowFlange4 = value; };
  
  G4double             GetDistanceWarmWindowFlange3()                                     { return fDistanceWarmWindowFlange3; };
  void                 SetDistanceWarmWindowFlange3(G4double value)                       { fDistanceWarmWindowFlange3 = value; };
  
  G4double             GetAngleCentralAxisBoltUpperWarm()                                 { return fAngleCentralAxisBoltUpperWarm; };
  void                 SetAngleCentralAxisBoltUpperWarm(G4double value)                   { fAngleCentralAxisBoltUpperWarm = value; };
  
  G4double             GetRadiusHolesBoltsUpperWarm()                                     { return fRadiusHolesBoltsUpperWarm; };
  void                 SetRadiusHolesBoltsUpperWarm(G4double value)                       { fRadiusHolesBoltsUpperWarm = value; };
  
  G4double             GetLengthHolesBoltsUpperWarmInFlange5()                            { return fLengthHolesBoltsUpperWarmInFlange5; };
  void                 SetLengthHolesBoltsUpperWarmInFlange5(G4double value)              { fLengthHolesBoltsUpperWarmInFlange5 = value; };
  
  G4double             GetDistanceUpperWarmBoltToBeamPipe()                               { return fDistanceUpperWarmBoltToBeamPipe; };
  void                 SetDistanceUpperWarmBoltToBeamPipe(G4double value)                 { fDistanceUpperWarmBoltToBeamPipe = value; };
  
  G4double             GetRadiusHeadsBoltsUpperWarm()                                     { return fRadiusHeadsBoltsUpperWarm; };
  void                 SetRadiusHeadsBoltsUpperWarm(G4double value)                       { fRadiusHeadsBoltsUpperWarm = value; };
  
  G4double             GetLengthHeadsBoltsUpperWarm()                                     { return fLengthHeadsBoltsUpperWarm; };
  void                 SetLengthHeadsBoltsUpperWarm(G4double value)                       { fLengthHeadsBoltsUpperWarm = value; };
  
  G4double             GetAngleCentralAxisBoltLowerWarm()                                 { return fAngleCentralAxisBoltLowerWarm; };
  void                 SetAngleCentralAxisBoltLowerWarm(G4double value)                   { fAngleCentralAxisBoltLowerWarm = value; };
  
  G4double             GetRadiusHolesBoltsLowerWarm()                                     { return fRadiusHolesBoltsLowerWarm; };
  void                 SetRadiusHolesBoltsLowerWarm(G4double value)                       { fRadiusHolesBoltsLowerWarm = value; };
  
  G4double             GetLengthHolesBoltsLowerWarm()                                     { return fLengthHolesBoltsLowerWarm; };
  void                 SetLengthHolesBoltsLowerWarm(G4double value)                       { fLengthHolesBoltsLowerWarm = value; };
  
  G4double             GetDistanceLowerWarmBoltToFlange3()                                { return fDistanceLowerWarmBoltToFlange3; };
  void                 SetDistanceLowerWarmBoltToFlange3(G4double value)                  { fDistanceLowerWarmBoltToFlange3 = value; };
  
  G4double             GetRadiusHeadsBoltsLowerWarm()                                     { return fRadiusHeadsBoltsLowerWarm; };
  void                 SetRadiusHeadsBoltsLowerWarm(G4double value)                       { fRadiusHeadsBoltsLowerWarm = value; };
  
  G4double             GetLengthHeadsBoltsLowerWarm()                                     { return fLengthHeadsBoltsLowerWarm; };
  void                 SetLengthHeadsBoltsLowerWarm(G4double value)                       { fLengthHeadsBoltsLowerWarm = value; };
  
  G4double             GetLengthOfColdWindowPlate()                                       { return fLengthOfColdWindowPlate; };
  void                 SetLengthOfColdWindowPlate(G4double value)                         { fLengthOfColdWindowPlate = value; };
  
  G4double             GetMinimumAddedMaterialFrontPlate()                                { return fMinimumAddedMaterialFrontPlate; };
  void                 SetMinimumAddedMaterialFrontPlate(G4double value)                  { fMinimumAddedMaterialFrontPlate = value;  };
  
  G4double             GetPassiveLKrInsideOctagon()                                       { return fPassiveLKrInsideOctagon; };
  void                 SetPassiveLKrInsideOctagon(G4double value)                         { fPassiveLKrInsideOctagon = value; };
  
  G4double             GetLongitudinalLengthLKrVolume()                                   { return fLongitudinalLengthLKrVolume; };
  void                 SetLongitudinalLengthLKrVolume(G4double value)                     { fLongitudinalLengthLKrVolume = value; };
  
  G4double             GetHalfLengthOfBackPlateOctagon()                                  { return fHalfLengthOfBackPlateOctagon; };
  void                 SetHalfLengthOfBackPlateOctagon(G4double value)                    { fHalfLengthOfBackPlateOctagon = value; };
  
  G4double             GetHalfLengthOfFrontPlateOctagon()                                 { return fHalfLengthOfFrontPlateOctagon; };
  void                 SetHalfLengthOfFrontPlateOctagon(G4double value)                   { fHalfLengthOfFrontPlateOctagon = value; };
  
  G4double             GetLKrCellLength()                                                 { return fLKrCellLength; };
  void                 SetLKrCellLength(G4double value)                                   { fLKrCellLength = value; };
  
  G4double             GetTopRightCornerX()                                               { return fTopRightCornerX; };
  void                 SetTopRightCornerX(G4double value)                                 { fTopRightCornerX = value; };
  
  G4double             GetTopRightCornerY()                                               { return fTopRightCornerY; };
  void                 SetTopRightCornerY(G4double value)                                 { fTopRightCornerY = value; };
  
  G4double             GetLKrCell0X()                                                     { return fLKrCell0X; };
  void                 SetLKrCell0X(G4double value)                                       { fLKrCell0X = value; };
  
  G4double             GetLKrCell0Y()                                                     { return fLKrCell0Y; };
  void                 SetLKrCell0Y(G4double value)                                       { fLKrCell0Y = value; };
  
  G4ThreeVector * GetPositionSteelBars()                                                  { return fPositionSteelBars; };
  
  G4double             GetRadiusSteelBars()                                               { return fRadiusSteelBars; };
  void                 SetRadiusSteelBars(G4double value)                                 { fRadiusSteelBars = value; };

  G4String GetGeVtoCurrent1() {return fGeVtoCurrent1;}
  void SetGeVtoCurrent1(G4String path) {fGeVtoCurrent1 = path;}

  G4String GetGeVtoCurrent2() {return fGeVtoCurrent2;}
  void SetGeVtoCurrent2(G4String path) {fGeVtoCurrent2 = path;}

  G4String GetGeVtoCurrent3() {return fGeVtoCurrent3;}
  void SetGeVtoCurrent3(G4String path) {fGeVtoCurrent3 = path;}

   private:
        G4double fStartLKrResponsibleRegion;
        G4double fEndLKrResponsibleRegion; 

        G4double fDistanceWWtoValve;

        G4double  fWorldZLength;
        G4double  fWorldXLength;
        G4double  fWorldYLength;

        G4double  fLKrDetectorZPosition;

        G4double  fLKrDetectorZLength;
        G4double  fLKrDetectorXLength;
        G4double  fLKrDetectorYLength;
  
        G4double fThermalContractionConstantG10;

        G4double fDistanceFrontPlateBackPlate;
        G4double  fFrontWallPositionZ;
        G4double  fBackWallPositionZ;

        G4double  fHalfZWidth;
        G4double  fZtr;

        G4double fPositionOfWallZ0;



        G4double fXbackReferenceCell ;
        G4double fYbackReferenceCell ;

        G4double fHalfSizeCentralGap;
        G4double fXbackProjectivityReferenceCell ;
        G4double fYbackProjectivityReferenceCell ;

        G4double fXfrontReferenceCell ;
        G4double fYfrontReferenceCell ;

        G4double fProjectivityPointPositionZ;
        G4double SpaceToNextPlate;
        G4double fProjectivityAxisProjectionZ;

        G4double fIncr;
        G4double fHalfCellSizeAtFrontWall;

        G4double fDistanceToNextElectrodeFrontX;
        G4double fDistanceToNextElectrodeBackX;
        G4double fDistanceToNextElectrodeFrontY;
        G4double fDistanceToNextElectrodeBackY;

        G4double fHalfXWidthF; 
        G4double fHalfYWidthF;
        G4double fHalfXWidthB; 
        G4double fHalfYWidthB;

        G4double fSizeHoleX;
        G4double fSizeHoleY;
        G4double fSizeHoleYHalfCell;

        G4double fHalfSizeFrontWallSegmentZ;
        G4double fHalfSizeSpacerWallSegmentZ;
        G4double fHalfSizeBackWallSegmentZ;

        G4double fHalfSizeWallVSegmentY; 
        G4double fHalfSizeIrregularWallVSegmentY; 

        G4double fZigHalfSizeX;
        G4double fZigHalfSizeY;
        G4double fZigHalfSizeZ;

        G4double fIrrZigHalfSizeX; 
        G4double fIrrZigHalfSizeY;
        G4double fIrrZigHalfSizeZ;

        G4double fHalfIrregularCellSizeAtBackWallY;
        G4double fHalfIrregularCellSizeAtFrontWallY;

        G4double fHalfYWidthIrregularB;
        G4double fHalfYWidthIrregularF;

        G4int fHalfNElectrodesX;
        G4int fHalfNElectrodesY;

        G4int  fNRowsFullNumberElectrodes;

        G4double fNHODHalfLenghtG10Z;
        G4double fNHODHalfLenghtScintillatorZ;

        G4double fRHoleX;
        G4double fRHoleY;

        G4double fSpaceFromWallToElectrodeAtWallX;

        G4double fThicknessSliceColdWindow;
        G4double fThicknessSliceBackColdWindow ;

        G4double fInnerCryostatMinRadius; 
        G4double fThicknessOfInnerCryo;

        G4double fOuterCryostatMinRadius ;
        G4double fThicknessOfOuterCryo ;
  
        G4double fThicknessSteelFoilColdWindow;
        G4double fRadiusSteelFoilColdWindow;

        G4double fRadiusFrontColdWindow;
        G4double fRadiusBackColdWindow ;
        G4double fRadiusFrontColdWindowBeamPipe;

        G4double fThicknesConvexColdWindow ;
        G4double fThicknesConvexColdBackWindow;

        G4double fBEATCHPositionFrontWarmWindowEdge;
        G4double fBEATCHPositionFrontEdgeOuterVessel;
        G4double fBEATCHPositionOuterWarmFlange1;

        G4double fBEATCHPositionBackEdgeOuterVessel;
        G4double fBEATCHPositionBackWarmWindowEdge;

        G4double fVerticalRadiusWarmWindow;
        
        G4double fRadiusWarmWindow; 
        G4double fThicknesConvexWarmWindow; 

        G4double fDistanceWarmWindowFlange4;
        G4double fDistanceWarmWindowFlange3;

        G4double fDistanceColdWindowToMonotube;
        
        G4double fDistanceMonotubeToothFlange ;
        G4double fHightToothFlange;
        G4double fLengthToothFlange;
        
        G4double fDistanceMonotubeBellowToothFlange;
        G4double fHightBellowToothFlange;
        G4double fLengthBellowToothFlange;

        G4double fDistanceBellowToothFlangeAtBackToRareWindow; 

        G4double fHightFlange1AtColdWindowBeam;
        G4double fLengthFlange1AtColdWindowBeam;
        G4double fMinRadiusFlange1AtColdWindowBeam;
         
        G4double fHightFlange2AtColdWindowBeam;
        G4double fLengthFlange2AtColdWindowBeam;
        
        G4double fHightFlange3AtColdWindowBeam;
        G4double fLengthFlange3AtColdWindowBeam;
        
        G4double fHightFlange4AtColdWindowBeam;
        G4double fLengthFlange4AtColdWindowBeam ;
        G4double fMinRadiusFlange4AtColdWindowBeam;

        G4double fHightFlange1AtColdWindowOut;
        G4double fLengthFlange1AtColdWindowOut;
        G4double fMinRadiusFlange1AtColdWindowOut;
       
        G4double fHightFlange2AtColdWindowOut;
        G4double fLengthFlange2AtColdWindowOut;
        G4double fMinRadiusFlange2AtColdWindowOut;
        G4double fDistanceFlange2AtColdWindowOutFaceToSteelFoil;

        G4double fRadiusHolesBoltsFrontCold ;
        G4double fLengthHolesBoltsFrontCold ;
        G4double fRadiusHeadsBoltsFrontCold ;
        G4double fLengthHeadsBoltsFrontCold ;
        G4double fDistanceBoltToBeamPipe;

        G4double fLengthHolesBoltsBackColdInFlange5 ;
        G4double fLengthHolesBoltsBackColdInFlange4 ;
        G4double fAngleCentralAxisBolt ;

        G4double fHightBackFlange1AtColdWindowBeam ;
    
        G4double fLengthBackFlange1AtColdWindowBeam ;
        G4double fMinRadiusBackFlange1AtColdWindowBeam ;
    
        G4double fHightBackFlange2AtColdWindowBeam ;
        G4double fLengthBackFlange2AtColdWindowBeam ;
        G4double fMinRadiusBackFlange2AtColdWindowBeam ;
    
        G4double fHightBackFlange3AtColdWindowBeam ;
        G4double fLengthBackFlange3AtColdWindowBeam ;
        G4double fMinRadiusBackFlange3AtColdWindowBeam;
    
        G4double fHightBackFlange4AtColdWindowBeam;
        G4double fLengthBackFlange4AtColdWindowBeam ;
        G4double fMinRadiusBackFlange4AtColdWindowBeam ;
    
        G4double fZDistanceBetweeBackEdges3and4 ;
    
        G4double fHightBackFlange5AtColdWindowBeam ;
        G4double fLengthBackFlange5AtColdWindowBeam ;
        G4double fMinRadiusBackFlange5AtColdWindowBeam ;
    
        G4double fHightBackFlangeAtColdWindowOut;
        G4double fLengthBackFlangeAtColdWindowOut;
        G4double fMinRadiusBackFlangeAtColdWindowOut;
        G4double fDistanceFaceBackFlangeOutToColdWindow;

        G4double fHightFlangeAtWarmWindow1Beam ;
        G4double fLengthFlangeAtWarmWindow1Beam ;
        G4double fMinRadiusFlangeAtWarmWindow1Beam ;

        G4double fHightFlangeAtWarmWindow2Beam ;
        G4double fLengthFlangeAtWarmWindow2Beam ;
        G4double fMinRadiusFlangeAtWarmWindow2Beam ;

        G4double fHightFlangeAtWarmWindow3Beam ;
        G4double fLengthFlangeAtWarmWindow3Beam ;
        G4double fMinRadiusFlangeAtWarmWindow3Beam ;

        G4double fElipticalCutFlange3LongAxis;
        G4double fElipticalCutFlange3ShortAxis;

    
        G4double fHightFlangeAtWarmWindow4Beam ;
        G4double fLengthFlangeAtWarmWindow4Beam ;
        G4double fMinRadiusFlangeAtWarmWindow4Beam ;
        G4double fDistanceWarmWindowFlange2EndFlange3Front;
    
        G4double fHightFlangeAtWarmWindow5Beam ;
        G4double fLengthFlangeAtWarmWindow5Beam ;
        G4double fMinRadiusFlangeAtWarmWindow5Beam ;
   
        G4double fHightFlangeAtWarmWindow1Out ;
        G4double fLengthFlangeAtWarmWindow1Out ;
        G4double fMinRadiusFlangeAtWarmWindow1Out ;
    
        G4double fHightFlangeAtWarmWindow2Out ;
        G4double fLengthFlangeAtWarmWindow2Out ;
        G4double fMinRadiusFlangeAtWarmWindow2Out;
    
        G4double fAngleCentralAxisBoltUpperWarm;
        G4double fRadiusHolesBoltsUpperWarm ;
        G4double fLengthHolesBoltsUpperWarmInFlange5 ;

        G4double fDistanceUpperWarmBoltToBeamPipe ;

        G4double fRadiusHeadsBoltsUpperWarm ;
        G4double fLengthHeadsBoltsUpperWarm;
   
        G4double fAngleCentralAxisBoltLowerWarm ;
        G4double fRadiusHolesBoltsLowerWarm ;
        G4double fLengthHolesBoltsLowerWarm;
   
        G4double fDistanceLowerWarmBoltToFlange3 ;

        G4double fRadiusHeadsBoltsLowerWarm ;
        G4double fLengthHeadsBoltsLowerWarm;

        G4double fElectronicsBackG10HalfLength;
        G4double fElectronicsBackCuHalfLength;
        G4double fElectronicsBackBrassHalfLength;
        G4double fElectronicsBackTeflonHalfLength;

        G4double fInnerBeamPipeRadius;
        G4double fOuterBeamPipeRadius;
        G4double fRadiusHoleSpacerPlate;

        G4double  fLongitudinalLengthInnerCryo;
        G4double  fLongitudinalLengthOuterCryo;

        G4double  fLongitudinalLengthBeamPipe; 

        G4double  fLongitudinalLengthLKrVolume;

        G4double  fHalfLengthOfBackPlateOctagon;
        G4double  fHalfLengthOfFrontPlateOctagon;

        G4double fInnerDistanceFrontBackWarmWindow ;
        G4double  fPassiveLKrInsideOctagon;

        G4double fDistanceFrontWarmWindowLKr;
        G4double fDistanceBackWarmWindowLKr;

        G4double fLengthOfColdWindowPlate;
        G4double fMinimumAddedMaterialFrontPlate;

        G4double fRadiusSteelBars;
        G4ThreeVector fPositionSteelBars[30];

        G4double  fLKrCellLength;

        G4double  fTopRightCornerX;
        G4double  fTopRightCornerY;

        G4double  fLKrCell0X;
        G4double  fLKrCell0Y;

        G4String fGeVtoCurrent1;
        G4String fGeVtoCurrent2;
        G4String fGeVtoCurrent3;
};
#endif
