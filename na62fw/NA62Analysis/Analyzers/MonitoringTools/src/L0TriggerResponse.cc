// ---------------------------------------------------------------
//
// History:
//
// Created by Chris Parkinson (chris.parkinson@cern.ch) 2016-08-08
//
// ---------------------------------------------------------------

/// \class L0TriggerResponse
/// \Brief
/// The response of the L0 trigger-generating detectors to Kmu2, K2pi and K3pi events
/// passing various selection criteria
/// \EndBrief
/// \Detailed
/// Kmu2, K2pi and K3pi events are selected using the K2piSelection, Kmu2Selection and
/// K3piSelection analysers. Only events triggered by the control trigger are used.
/// Any K3pi events are further classified as those that satisfy the Q1, Q2 and QX criteria.
/// Any Kmu2 events are further classified as those that satisfy an M1, M2 and MO2 criteria.
/// For each event, L0 primitives from the RICH, LAV12, MUV3, NewCHOD and L0Calo are sought
/// in the L0TPData within 20ns of the L0 reference time. If more than one primitive is found
/// (per detector) then the PrimitiveID is combined using a logical OR of the IDs.
/// The output of the analyser is a pdf file. For each of the 7 selections and 5 detectors
/// (making 35 combinations) two pages of output are produced. The first page details in
/// which L0TP slot a primitive of the given detector is found, and the logicalOR of the
/// three slots. The second page shows the fraction of events in which each bit of the
/// combined PrimitiveID is 'high' for the given selection.
///
/// A number of control plots are also created to monitor the behaviour of the L0 trigger
/// system. One- and two-dimensional plots showing the correlation of primitives from
/// the L0 primitive-generating detectors are shown. The resolution of the 2D plots can
/// be changed (when in 'histo' mode) using the 'PlotResolution' Parameter as shown:
/// -p"L0TriggerResponse:PlotResolution=0" -> low-resolution plots
/// -p"L0TriggerResponse:PlotResolution=1" -> mid-resolution plots
/// -p"L0TriggerResponse:PlotResolution=2" -> high-resolution plots
/// By default low-resolution plots are produced.
/// High-resolution plots are large and can take a long time to open. It is recommended
/// to use a lower resolution.
/// \author Chris Parkinson (chris.parkinson@cern.ch)
/// \author Lorenza Iacobuzio
/// \author Evgueni Goudzovski (eg@hp.ph.bham.ac.uk)
/// \author Francesco Gonnella
/// \EndDetailed

#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "L0TriggerResponse.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "BeamParameters.hh"
#include "ConfigSettings.hh"

#include "K2piSelection.hh"
#include "Kmu2Selection.hh"
#include "TTDCBSpecialTrigger.hh"
#include "BaseAnalysis.hh"
#include "containers.hh"
#include "NA62ConditionsService.hh"

using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

L0TriggerResponse::L0TriggerResponse(Core::BaseAnalysis *ba) : Analyzer(ba, "L0TriggerResponse") {

  // Request Reco branches
  RequestTree("MUV3", new TRecoMUV3Event, "Reco");
  RequestTree("CHOD", new TRecoCHODEvent, "Reco");
  RequestTree("LAV", new TRecoLAVEvent, "Reco");
  RequestTree("NewCHOD", new TRecoNewCHODEvent, "Reco");
  RequestTree("RICH", new TRecoRICHEvent, "Reco"); // for RICH multiplicity studies
  RequestTree("LKr", new TRecoLKrEvent, "Reco"); // for LKr energy studies

  RequestL0Data();

  Configuration::ConfigSettings::SetNoSkipBadBurst(true);// do not skip bad bursts

  // Request SpecialTrigger branches
  RequestL0SpecialTrigger();
  RequestBeamSpecialTrigger();
  RequestTree("MUV3", new TSpecialTriggerEvent);
  RequestTree("NewCHOD", new TSpecialTriggerEvent);

  ControlData  = false ;
  PhysicsData  = false ;
  fReadingData = true;

  fRunID   = 0 ;
  fBurstID   = 0 ;
  fCount     = 0 ;
  fNRICHHits = -99 ;
  fLKrEnergy = -99 ;
  fTrackMom  = -99 ;
  fZposition = -99 ;
  fNRICHSuperCells = -99 ;
  fHasSpecialTrigger = false;
  fNL0 = 0 ;
  fUTMCHits = 0 ;
  fFineTimeBit=-1;

  fNMasks=16;

  fhRunNumber = NULL;
  fhBurstTime = NULL;

  Q2bit = 14;
  QXbit = 14;
  UTMCbit = 14;
  E20bit = 14;
  E10bit = 14;

  fRef = 0 ;
  fRefTime = 0. ;

  fArgonionCount=0;

  for(int i=0; i<7; ++i) fOccupancy[i]=0;
  fNCOccTS=0;
  fNCOcc100=0;

  MaskDownscales.resize(fNMasks);

  AddParam("PlotResolution", &fPlotResolution, 0);

  fBadTriggerThreshold = 0.75;
  fBadTriggerThresholds.clear();
  fBadTriggerThresholds.resize(16);
  AddParam("ER2threshold",    &(fBadTriggerThresholds[0]),  0.90);
  AddParam("MR2threshold",    &(fBadTriggerThresholds[1]),  0.95);
  AddParam("Q1threshold",     &(fBadTriggerThresholds[2]),  0.90);
  AddParam("MQ1threshold",    &(fBadTriggerThresholds[3]),  0.95);
  AddParam("Q2threshold",     &(fBadTriggerThresholds[4]),  0.75);
  AddParam("Qxthreshold",     &(fBadTriggerThresholds[5]),  0.75);
  AddParam("M1threshold",     &(fBadTriggerThresholds[6]),  0.95);
  AddParam("MO1threshold",    &(fBadTriggerThresholds[7]),  0.95);
  AddParam("M2threshold",     &(fBadTriggerThresholds[8]),  0.90);
  AddParam("MO2threshold",    &(fBadTriggerThresholds[9]),  0.90);
  AddParam("ELAV12threshold", &(fBadTriggerThresholds[10]), 0.75);
  AddParam("E20threshold",    &(fBadTriggerThresholds[11]), 0.75);

  AddParam("UTMCthreshold",     &(fBadTriggerThresholds[12]), 0.85);
  AddParam("EUTMCthreshold",    &(fBadTriggerThresholds[13]), 0.90);
  AddParam("MUTMCthreshold",    &(fBadTriggerThresholds[14]), 0.95);
  AddParam("TCHODthreshold",    &(fBadTriggerThresholds[15]), 0.90);

  fGeo = NewCHODGeometry::GetInstance();

  TString TEMP1[16] = {
    "ML1", "MT1", "MLO1", "MTO1", "MLO2", "MTO2", "MMO2",
    "ML2", "MT2", "MM2", "MO1", "M1", "MO2", "M2", "Any", "Reserved"};
  std::copy(TEMP1, TEMP1+16, MUV3TriggerBitNames);

  TString TEMP2[16] = {
    "HL1", "HT1", "HLO1", "HTO1", "HLO2", "HTO2", "HMO2",
    "HL2", "HT2", "HM2", "HO1", "H1", "HO2", "H2", "Any", "Reserved"};
  std::copy(TEMP2, TEMP2+16, NCTriggerBitNames1);

  TString TEMP3[16] = {
    "QE1", "QE2", "QE3", "QE4", "Q2", "QX", "QTB",
    "NULL", "NULL", "NULL", "HO1", "H1", "HO2", "H2", "Any", "Reserved"};
  std::copy(TEMP3, TEMP3+16, NCTriggerBitNames2);

  TString TEMP4[16] = {
    "QE1", "QE2", "QE3", "QE4", "HO1", "HTO1", "H1",
    "HT1", "H2", "HT2", "Q2", "QX", "UTMC", "UTMCO", "Any", "Reserved"};
  std::copy(TEMP4, TEMP4+16, NCTriggerBitNames3);

  TString TEMP5[16] = {
    "n>4", "n>6", "n>8", "n>10", "NULL", "NULL", "NULL",
    "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "Any", "Reserved"};
  std::copy(TEMP5, TEMP5+16, RICHTriggerBitNames);

  TString TEMP6[16] = {
    "n>2", "n>3", "n>4", "n>5", "IGNORE", "NULL", "NULL",
    "NULL", "IGNORE", "NULL", "NULL", "NULL", "NULL", "NULL", "Any", "Reserved"};
  std::copy(TEMP6, TEMP6+16, LAV12TriggerBitNames);
}

void L0TriggerResponse::InitHist() {

  fReadingData = GetIsTree();

  events.push_back("_K3pi_Q1");
  events.push_back("_K3pi_E20");
  events.push_back("_K3pi_Q2");
  events.push_back("_K3pi_QX");
  events.push_back("_K3pi_QX3H");
  events.push_back("_K3pi_UTMC");
  events.push_back("_Kmu2_M1");
  events.push_back("_Kmu2_MO1");
  events.push_back("_Kmu2_M2");
  events.push_back("_Kmu2_MO2");
  events.push_back("_Kmu2_UTMC");
  events.push_back("_K2pi");
  events.push_back("_K2pi_E20");
  events.push_back("_K2pi_UTMC");
  events.push_back("_Mask0");
  events.push_back("_Mask1");
  events.push_back("_Mask2");
  events.push_back("_Mask3");
  events.push_back("_Mask4");
  events.push_back("_Mask5");
  events.push_back("_Mask6");
  events.push_back("_Mask7");
  events.push_back("_Mask8");
  events.push_back("_Mask9");
  events.push_back("_Mask10");
  events.push_back("_Mask11");
  events.push_back("_Mask12");
  events.push_back("_Mask13");
  events.push_back("_Mask14");
  events.push_back("_Mask15");

  primitives.push_back("_RICH");
  primitives.push_back("_LAV12");
  primitives.push_back("_MUV3");
  primitives.push_back("_NewCHOD");
  primitives.push_back("_Calo");
  primitives.push_back("_CHOD");

  nPerBurst.clear();
  nPerBurst.push_back("ER2");
  nPerBurst.push_back("MR2");
  nPerBurst.push_back("Q1");
  nPerBurst.push_back("MQ1");
  nPerBurst.push_back("Q2");
  nPerBurst.push_back("Qx");
  nPerBurst.push_back("M1");
  nPerBurst.push_back("MO1");
  nPerBurst.push_back("M2");
  nPerBurst.push_back("MO2");
  nPerBurst.push_back("ELAV12");
  nPerBurst.push_back("E20");

  nPerBurst.push_back("UTMC");
  nPerBurst.push_back("EUTMC");
  nPerBurst.push_back("MUTMC");

  nPerBurst.push_back("TCHOD");

  TString name;
  TString title = "";
  if (fReadingData) {

    int MaxB=2500;
    double MaxBR=MaxB-0.5;

    int MaxR=20000;
    double MaxRR=MaxR-0.5;

    ///////////////////////////////////////////
    //// Store the run number
    ///////////////////////////////////////////
    BookHisto(new TH1F("RunNumber","RunNumber;RunNumber;Count",MaxR, -0.5, MaxRR));
    BookHisto(new TH1F("BurstTime","BurstTime;Burst;Burst Unix Time",MaxB, -0.5, MaxBR));

    BookCounter("nControlEvents");

    BookHisto(new TH1F("nControlEvents","nControlEvents;Burst ID; Number of control events", MaxB, -0.5, MaxBR));
    BookHisto(new TH2F("ControlVsArgonion","ControlVsArgonion;Argonion Count (x10^{8});N control events",
          125,0,25,100,0,100000) );

    BookHistoArray(new TH1F("nEventsMask","nEventsMask;Burst ID; Number of triggers per unit argonian count", MaxB, -0.5, MaxBR),fNMasks+1);

    // manually book a histo array to reduce histogram bins
    for(int i=0; i<fNMasks+1; ++i){
      int a=500;
      int b=50000;
      if(i==1){
        a=150;
        b=150000;
      }
      TString ANAME = Form("nEventsVsArgonionMask%i",i);
      BookHisto(new TH2F(ANAME,ANAME+";Argonion Count (x10^{8});N mask events", 125,0,25,a,0,b));
    }

    ///////////////////////////////////////////
    //// Assorted plots
    ///////////////////////////////////////////
    BookHisto(new TH1F("DataType","DataType;Data Type;Count",8,-0.5,7.5));
    BookHisto(new TH1F("TriggerType","TriggerType;Trigger Type;Count",8,-0.5,7.5));
    BookHisto(new TH1F("TriggerFlagsData","TriggerFlagsData;Trigger Flags;Count",fNMasks,-0.5,fNMasks-0.5));
    BookHisto(new TH1F("TriggerFlagsBeforeL1","TriggerFlagsBeforeL1;Trigger Flags;Count",fNMasks,-0.5,fNMasks-0.5));
    BookHisto(new TH2F("MaskCorrelationData","MaskCorrelationData;Tag;Probe",fNMasks,-0.5,fNMasks-0.5,fNMasks,-0.5,fNMasks-0.5));
    BookHisto(new TH2F("MaskCorrelationRaw","MaskCorrelationRaw;Tag;Probe",fNMasks,-0.5,fNMasks-0.5,fNMasks,-0.5,fNMasks-0.5));
    BookHisto(new TH2F("MaskCorrelationRawBurst","MaskCorrelationRawBurst;Tag;Probe",fNMasks,-0.5,fNMasks-0.5,fNMasks,-0.5,fNMasks-0.5));
    BookHisto(new TH2F("MaskCorrelationRawWDS","MaskCorrelationRawWDS;Tag;Probe",fNMasks,-0.5,fNMasks-0.5,fNMasks,-0.5,fNMasks-0.5));

    BookHistoArray(new TH1F("OTS_MM2_Mask","OTS_MM2_Mask;MM^{2} (GeV^{2}/c^{4}); Count", 100, -0.150, 0.150), fNMasks) ;
    BookHistoArray(new TH2F("OTS_MM2vsP_Mask", "OTS_MM2vsP_Mask;Track momentum (GeV/c);MM^{2} (Gev^{2}/c^{4});", 50, 0, 100, 50, -0.150, 0.150), fNMasks);
    BookHisto(new TH1F("K3pi_NCHits_ControlTrig","K3pi_NCHits_ControlTrig; #Delta_{t}(Hit - Trigger);Count", 1001,-500.5*TdcCalib,500.5*TdcCalib));
    BookHisto(new TH1F("CHODNoHits","CHODNoHits",1001,-500.5*TdcCalib,500.5*TdcCalib));

    BookHisto(new TH1F("FineTimeBit_Data", "Fine Time Bit (Data);Burst ID;FTB", MaxB, -0.5, MaxBR));
    BookHisto(new TH1F("FineTimeBit_Hard", "Fine Time Bit (Hard);Burst ID;FTB", MaxB, -0.5, MaxBR));
    BookHisto(new TH1F("FineTimeBit_Null", "Fine Time Bit (NULL);Burst ID;FTB", MaxB, -0.5, MaxBR));

    ///////////////////////////////////////////
    //// Check for primitive in trigger slot
    ///////////////////////////////////////////
    BookHisto(new TH1F("HasPhysicsPrimitive", "HasPhysicsPrimitive", 2, -0.5, 1.5));
    BookHisto(new TH1F("HasControlPrimitive", "HasControlPrimitive", 2, -0.5, 1.5));

    ///////////////////////////////////////////
    //// UTMC plots
    ///////////////////////////////////////////
    BookHisto(new TH1F("UTMC_1","UTMC_1;NewCHOD Hit Multiplicity;Count",50,-0.5,49.5));
    BookHisto(new TH1F("UTMC_C","UTMC_C;NewCHOD Hit Multiplicity;Count",50,-0.5,49.5));
    BookHisto(new TH1F("UTMC_M","UTMC_M;NewCHOD Hit Multiplicity;Count",50,-0.5,49.5));

    ///////////////////////////////////////////
    //// LKr energy plots
    ///////////////////////////////////////////
    BookHisto(new TH1F("LKrEnergy_1","LKrEnergy_1;Reconstructed energy (GeV)", 100,-0.5,99.5));
    BookHisto(new TH1F("LKrEnergy_4","LKrEnergy_4;Reconstructed energy (GeV)", 100,-0.5,99.5));
    BookHisto(new TH1F("LKrEnergy_C","LKrEnergy_C;Reconstructed energy (GeV)", 100,-0.5,99.5));
    BookHisto(new TH1F("LKrEnergy_K2pi","LKrEnergy_K2pi;Reconstructed energy (GeV)", 100,-0.5,99.5));
    BookHisto(new TH1F("LKrEnergy_Kmu2","LKrEnergy_Kmu2;Reconstructed energy (GeV)", 100,-0.5,99.5));
    name = "LKrEnergy2D;Reconstructed energy (GeV);Primitive energy (GeV)" ;
    BookHisto(new TH2F("LKrEnergy2D", name, 100, -0.5, 99.5, 100, -0.5, 99.5));
    BookHisto(new TH2F("LKrEnergy2DB12", name, 100, -0.5, 99.5, 100, -0.5, 99.5));
    BookHisto(new TH2F("LKrEnergy2DB13", name, 100, -0.5, 99.5, 100, -0.5, 99.5));
    BookHisto(new TH2F("LKrEnergy2DB13Only", name, 100, -0.5, 99.5, 100, -0.5, 99.5));

    ///////////////////////////////////////////
    //// NewCHOD quadrant mapping
    ///////////////////////////////////////////
    BookHisto(new TH2F("NewCHODMap", "NewCHODMap;x (m);y (m)", 16, -1.072, 1.072, 20, -1.070, 1.070) ) ;
    BookHisto(new TH2F("NewCHODunMap", "NewCHODunMap;x (m);y (m)", 16, -1.072, 1.072, 20, -1.070, 1.070) ) ;
    BookHisto(new TH2F("NewCHODQuads_1", "NewCHODQuads_1;x (m);y (m)", 16, -1.072, 1.072, 20, -1.070, 1.070) ) ;
    BookHisto(new TH2F("NewCHODQuads_2", "NewCHODQuads_2;x (m);y (m)", 16, -1.072, 1.072, 20, -1.070, 1.070) );
    BookHisto(new TH2F("NewCHODQuads_3", "NewCHODQuads_3;x (m);y (m)", 16, -1.072, 1.072, 20, -1.070, 1.070) );
    BookHisto(new TH2F("NewCHODQuads_4", "NewCHODQuads_4;x (m);y (m)", 16, -1.072, 1.072, 20, -1.070, 1.070) );

    ///////////////////////////////////////////
    //// MUV3 quadrant mapping
    ///////////////////////////////////////////
    BookHisto(new TH2F("MUV3Quads_1", "MUV3Quads_1;x (m);y (m)", 12, -1.32, 1.32, 12, -1.32, 1.32));
    BookHisto(new TH2F("MUV3Quads_2", "MUV3Quads_2;x (m);y (m)", 12, -1.32, 1.32, 12, -1.32, 1.32));
    BookHisto(new TH2F("MUV3Quads_3", "MUV3Quads_3;x (m);y (m)", 12, -1.32, 1.32, 12, -1.32, 1.32));
    BookHisto(new TH2F("MUV3Quads_4", "MUV3Quads_4;x (m);y (m)", 12, -1.32, 1.32, 12, -1.32, 1.32));

    ///////////////////////////////////////////
    //// Spasimir plots
    ///////////////////////////////////////////
    BookHisto(new TH2F("SpasimirPlotI", "SpasimirPlotI;x (m);y (m)", 16, -1.072, 1.072, 20, -1.070, 1.070) );
    BookHisto(new TH2F("SpasimirPlotO", "SpasimirPlotO;x (m);y (m)", 16, -1.072, 1.072, 20, -1.070, 1.070) );
    BookHisto(new TH2F("SpasimirPlotA", "SpasimirPlotA;x (m);y (m)", 16, -1.072, 1.072, 20, -1.070, 1.070) );
    BookHisto(new TH2F("SpasimirPlot_pip", "SpasimirPlot_pip;x (m);y (m)", 16, -1.072, 1.072, 20, -1.070, 1.070) );

    ///////////////////////////////////////////
    //// Plots from Special triggers
    ///////////////////////////////////////////
    BookHisto(new TH1F("nL0TPSpecialTriggers","nL0TPSpecialTriggers;Burst ID;n L0 special triggers",MaxB,-0.5,MaxBR));
    BookHisto(new TH1F("ArgoCount","ArgoCount;Burst ID;Argonion Count (x10^{8})",MaxB,-0.5,MaxBR));
    BookHistoArray(new TH1F("TriggersGend","TriggersGend;Burst ID;N Triggers Generated",MaxB,-0.5,MaxBR), fNMasks);
    BookHistoArray(new TH1F("TriggersSent","TriggersSent;Burst ID;N Triggers Sent",MaxB,-0.5,MaxBR), fNMasks);
    BookHistoArray(new TH1F("Downscaling","Downscaling;Burst ID;Downscaling",MaxB,-0.5,MaxBR), fNMasks);

    ///////////////////////////////////////////
    //// MUV3 EOB plots
    ///////////////////////////////////////////
    BookHisto(new TH1F("MUV3EOBPPScaler","MUV3EOBPPScaler;Channel ID;Count",512,-0.5,511.5));
    BookHisto(new TH1F("MUV3EOBPPTightHits","MUV3EOBPPTightHits;Tile ID;Count",256,-0.5,511.5));
    BookHisto(new TH1F("MUV3EOBPPLooseHits","MUV3EOBPPLooseHits;Tile ID;Count",256,-0.5,511.5));
    BookHistoArray(new TH1F("MUV3EOBPPErrorFlags","MUV3EOBPPErrorFlags;Error bit;Count",16,-0.5,15.5),4);
    BookHistoArray(new TH1F("MUV3EOBPPErrorCounts","MUV3EOBPPErrorCounts;Error bit;Count",16,-0.5,15.5),4);
    BookHisto(new TH1F("MUV3EOBSLTight","MUV3EOBSLTight;Channel ID;Count",256,-0.5,255.5));
    BookHisto(new TH1F("MUV3EOBSLErrorCounts","MUV3EOBSLErrorCounts;Tile ID;Count", 16, -0.5, 15.5)) ;

    ///////////////////////////////////////////
    //// NewCHOD EOB plots
    ///////////////////////////////////////////
    BookHisto(new TH1F("NewCHODEOBPPScaler","NewCHODEOBPPScaler;Channel ID;Count",512,-0.5,511.5));
    BookHisto(new TH1F("NewCHODEOBPPTightHits","NewCHODEOBPPTightHits;Tile ID;Count",256,-0.5,511.5));
    BookHisto(new TH1F("NewCHODEOBPPLooseHits","NewCHODEOBPPLooseHits;Tile ID;Count",256,-0.5,511.5));
    BookHistoArray(new TH1F("NewCHODEOBPPErrorFlags","NewCHODEOBPPErrorFlags;Error bit;Count",16,-0.5,15.5),4);
    BookHistoArray(new TH1F("NewCHODEOBPPErrorCounts","NewCHODEOBPPErrorCounts;Error bit;Count",16,-0.5,15.5),4);
    BookHisto(new TH1F("NewCHODEOBSLTight","NewCHODEOBSLTight;Tile ID;Count",256,-0.5,255.5));
    BookHisto(new TH1F("NewCHODEOBSLErrorCounts","NewCHODEOBSLErrorCounts;Tile ID;Count", 16, -0.5, 15.5)) ;

    ///////////////////////////////////////////
    //// 1D and 2D primitive correlations
    //// DigiTimeRawFine plots
    ///////////////////////////////////////////
    for (UInt_t j=0; j < primitives.size(); j++) {
      name.Form("1DCorr_Phys1E%s", primitives[j].Data());
      title = name + ";Reference FineTime;Count";
      BookHisto(new TH1F(name,title,768+1,-384.5,384.5));
      name.Form("1DCorr_PhysM%s", primitives[j].Data());
      title = name + ";Reference FineTime;Count";
      BookHisto(new TH1F(name,title,768+1,-384.5,384.5));
      name.Form("1DCorr_Control%s", primitives[j].Data());
      title = name + ";Reference FineTime;Count";
      BookHisto(new TH1F(name,title,768+1,-384.5,384.5));
      name.Form("1DCorr_Phys1I%s", primitives[j].Data());
      title = name + ";Reference FineTime;Count";
      BookHisto(new TH1F(name,title,768+1,-384.5,384.5));

      name.Form("2DCorr_Phys1E%s", primitives[j].Data());
      title = name + Form(";Reference FineTime;%s FineTime", primitives[j].Data()) ;
      BookHisto(new TH2F(name,title,256,-0.5,255.5,512+256-1,-255.5,511.5));
      name.Form("2DCorr_PhysM%s", primitives[j].Data());
      title = name + Form(";Reference FineTime;%s FineTime", primitives[j].Data()) ;
      BookHisto(new TH2F(name,title,256,-0.5,255.5,512+256-1,-255.5,511.5));
      name.Form("2DCorr_Control%s", primitives[j].Data());
      title = name + Form(";Reference FineTime;%s FineTime", primitives[j].Data()) ;
      BookHisto(new TH2F(name,title,256,-0.5,255.5,512+256-1,-255.5,511.5));
      name.Form("2DCorr_Phys1I%s", primitives[j].Data());
      title = name + Form(";Reference FineTime;%s FineTime", primitives[j].Data()) ;
      BookHisto(new TH2F(name,title,256,-0.5,255.5,512+256-1,-255.5,511.5));

      name.Form("DigiTimeRaw_Phys1E%s", primitives[j].Data());
      title = name + ";Hit time w.r.t reference;Count";
      BookHisto(new TH1F(name,title,501,-500.5*TdcCalib,500.5*TdcCalib));
      name.Form("DigiTimeRaw_Phys1I%s", primitives[j].Data());
      title = name + ";Hit time w.r.t reference;Count";
      BookHisto(new TH1F(name,title,501,-500.5*TdcCalib,500.5*TdcCalib));
      name.Form("DigiTimeRaw_PhysM%s", primitives[j].Data());
      title = name + ";Hit time w.r.t reference;Count";
      BookHisto(new TH1F(name,title,501,-500.5*TdcCalib,500.5*TdcCalib));
      name.Form("DigiTimeRaw_Control%s", primitives[j].Data());
      title = name + ";Hit time w.r.t reference;Count";
      BookHisto(new TH1F(name,title,501,-500.5*TdcCalib,500.5*TdcCalib));

    }

    ///////////////////////////////////////////
    //// n primitives
    ///////////////////////////////////////////

    for(unsigned int j = 0 ; j < primitives.size() ; ++j){
      name = Form("nPrimitives%s", primitives[j].Data());
      title = name+";Burst ID;N primitives";
      BookHisto(new TH1F(name, title, MaxB, -0.5, MaxBR));
    }

    ///////////////////////////////////////////
    //// Primitive bits
    ///////////////////////////////////////////

    name = "PrimitiveTSBits16_RICH";
    BookHisto(new TH1F(name,name+";Bit combinations;Count",256,-0.5,255.5));
    name = "PrimitiveTSBits16_CHOD";
    BookHisto(new TH1F(name,name+";Bit combinations;Count",256,-0.5,255.5));

    for(unsigned int j = 0 ; j < primitives.size() ; ++j){
      name.Form("PrimitiveFTBits%s_RICH",primitives[j].Data());
      BookHisto(new TH1F(name,name+";Bit combinations;Count",256,-0.5,255.5));
      name.Form("PrimitiveFTBits%s_CHOD",primitives[j].Data());
      BookHisto(new TH1F(name,name+";Bit combinations;Count",256,-0.5,255.5));
    }

    ///////////////////////////////////////////
    //// 2D hit correlations vs RICH
    ///////////////////////////////////////////
    for(unsigned int j = 1 ; j < primitives.size() ; ++j){
      TString hname = primitives[j] ;
      if( hname.Contains("Calo")) continue ;
      name.Form("HitCorrelations2D_Control%s%s", "_RICH" ,primitives[j].Data());
      title = Form(";%s hit times (ns);%s hit times (ns)", "_RICH", primitives[j].Data());
      title = title.ReplaceAll("_","");
      title = name + title ;
      BookHisto(new TH2F(name,title, 101,-500.5*TdcCalib,500.5*TdcCalib, 101,-500.5*TdcCalib,500.5*TdcCalib));
    }

    ///////////////////////////////////////////
    //// 2D hit correlations vs CHOD
    ///////////////////////////////////////////
    for(unsigned int j = 1 ; j < primitives.size()-2 ; ++j){
      name.Form("HitCorrelations2D_Control%s%s", "_CHOD" ,primitives[j].Data());
      title = Form(";%s hit times (ns);%s hit times (ns)", "_CHOD", primitives[j].Data());
      title = title.ReplaceAll("_","");
      title = name + title ;
      BookHisto(new TH2F(name,title, 101,-500.5*TdcCalib,500.5*TdcCalib, 101,-500.5*TdcCalib,500.5*TdcCalib));
    }

    ///////////////////////////////////////////
    //// Efficiency plots as a function of burst, Z position, track momentum, occupancy
    ///////////////////////////////////////////

    for(unsigned int i = 0 ; i < nPerBurst.size() ; ++i){

      ///////////////////////////////////////////
      //// As a function of burst
      name  = nPerBurst[i] ;
      name += "_NumPerBurst" ;
      title = Form( "%s Efficiency Per Burst; Burst Number; Efficiency", nPerBurst[i].Data() ) ;
      BookHisto(new TH1F(name, title, MaxB, -0.5, MaxBR) );
      name  = nPerBurst[i] ;
      name += "_DenPerBurst" ;
      BookHisto(new TH1F(name, name, MaxB, -0.5, MaxBR) );
      name  = nPerBurst[i];
      name += "_BurstEfficiency1D";
      title = name + ";Efficiency;Count";
      BookHisto(new TH1F(name, title, 101, -0.005, 1.005) );

      ///////////////////////////////////////////
      //// As a function of track momentum
      name  = nPerBurst[i] ;
      name += "_NumPerTrackMom" ;
      title = Form( "%s Efficiency vs. Track momentum; Track momentum (GeV/c); Efficiency", nPerBurst[i].Data() ) ;
      BookHisto(new TH1F(name, title, 65, 10, 75) );
      name  = nPerBurst[i] ;
      name += "_DenPerTrackMom" ;
      BookHisto(new TH1F(name, title, 65, 10, 75) );

      ///////////////////////////////////////////
      //// As a function of Z position
      name  = nPerBurst[i] ;
      name += "_NumPerZposition" ;
      title = Form( "%s Efficiency vs. Z position; Z position (m); Efficiency", nPerBurst[i].Data() ) ;
      BookHisto(new TH1F(name, title, 70, 105, 175) );
      name  = nPerBurst[i] ;
      name += "_DenPerZposition" ;
      BookHisto(new TH1F(name, title, 70, 105, 175) );

      ///////////////////////////////////////////
      //// As a function of run ID
      name = nPerBurst[i];
      name += "_NumPerRun";
      title = Form( "%s efficiency vs. run number; Run number; Efficiency", nPerBurst[i].Data());
      BookHisto(new TH1F(name, title, MaxR, -0.5, MaxRR) );
      name = nPerBurst[i];
      name += "_DenPerRun";
      BookHisto(new TH1F(name, title, MaxR, -0.5, MaxRR) );

      ///////////////////////////////////////////
      //// As a function of detector occupancy
      name = nPerBurst[i];
      name += "_NumPerOccupancy";
      title = Form( "%s efficiency vs. occupancy; Detector occupancy; Efficiency", nPerBurst[i].Data());
      BookHisto(new TH1F(name, title, 201, -0.5, 200.5));
      name = nPerBurst[i];
      name += "_DenPerOccupancy";
      BookHisto(new TH1F(name, title, 201, -0.5, 200.5));
    }

    ///////////////////////////////////////////
    //// Efficiency vs occupancy, without sample
    ///////////////////////////////////////////
    for(unsigned int i=0; i<primitives.size();++i){
      name  = primitives[i](1,primitives[i].Length());
      title = Form( "%s efficiency vs. occupancy; Detector occupancy; Efficiency", name.Data());
      name += "_NumPerOccupancy";
      BookHisto(new TH1F(name, title, 201, -0.5, 200.5));
      name  = primitives[i](1,primitives[i].Length());
      name += "_DenPerOccupancy";
      BookHisto(new TH1F(name, title, 201, -0.5, 200.5));
    }

    ///////////////////////////////////////////
    //// Efficiency vs. Timeslot, without sample
    ///////////////////////////////////////////
    name = "NewCHOD_NumPerTimeslot";
    title = "NewCHOD efficiency vs. timeslot occupancy; Detector timeslot occupancy; Efficiency";
    BookHisto(new TH1F(name, title, 201, -0.5, 200.5));
    name = "NewCHOD_DenPerTimeslot";
    BookHisto(new TH1F(name, title, 201, -0.5, 200.5));

    ///////////////////////////////////////////
    //// Efficiency vs. Timesplit, without sample
    ///////////////////////////////////////////
    name = "NewCHOD_NumPerTimesplit";
    title = "NewCHOD efficiency vs. timesplit occupancy; Detector timesplit occupancy; Efficiency";
    BookHisto(new TH1F(name, title, 201, -0.5, 200.5));
    name = "NewCHOD_DenPerTimesplit";
    BookHisto(new TH1F(name, title, 201, -0.5, 200.5));

    ///////////////////////////////////////////
    //// Trigger response plots
    ///////////////////////////////////////////
    for (UInt_t i=0; i < events.size(); i++) {
      for (UInt_t j=0; j < primitives.size(); j++) {
        name.Form("Passed%s%s", events[i].Data(), primitives[j].Data());
        BookHisto(new TH1F(name, "", 16, -0.5, 15.5));
        name.Form("Total%s%s", events[i].Data(), primitives[j].Data());
        BookHisto(new TH1F(name, "", 16, -0.5, 15.5));
        name.Form("L0Trigger_N0%s%s", events[i].Data(), primitives[j].Data());
        BookHisto(new TH2F(name, "", 16, -0.5, 15.5, 2, -0.5, 1.5));
        name.Form("L0Trigger_N1%s%s", events[i].Data(), primitives[j].Data());
        BookHisto(new TH2F(name, "", 16, -0.5, 15.5, 2, -0.5, 1.5));
        name.Form("L0Trigger_N2%s%s", events[i].Data(), primitives[j].Data());
        BookHisto(new TH2F(name, "", 16, -0.5, 15.5, 2, -0.5, 1.5));
        name.Form("L0Trigger_N3%s%s", events[i].Data(), primitives[j].Data());
        BookHisto(new TH2F(name, "", 16, -0.5, 15.5, 2, -0.5, 1.5));
      }
    }

    ///////////////////////////////////////////
    //// L0Calo plots
    ///////////////////////////////////////////

    name = "L0Calo_ClusterEnergy";
    BookHisto(new TH1F(name, "Cluster energy;Energy [GeV]", 200, 0, 100));
    name = "L0Calo_ClusterTime";
    BookHisto(new TH1F(name, "Cluster time;Time wrt trigger time [ns]", 200, -50, 50));
    name = "L0Calo_ClusterXY";
    BookHisto(new TH2F(name, "Cluster coordinates (E>30 GeV);x [m];y [m]", 110, -1.1, 1.1, 110, -1.1, 1.1));

    ///////////////////////////////////////////
    //// L0Calo K2pi efficiency Vs TotEnergy
    ///////////////////////////////////////////

    name = "L0Calo_E20_NumPerE";
    BookHisto(new TH1F(name, name+";Total LKr energy;L0Calo E20 Efficiency", 200, 0, 100));
    name = "L0Calo_E20_DenPerE";
    BookHisto(new TH1F(name, name+";Total LKr energy;L0Calo E20 Efficiency", 200, 0, 100));
    name = "L0Calo_E10_NumPerE";
    BookHisto(new TH1F(name, name+";Total LKr energy;L0Calo E10 Efficiency", 200, 0, 100));
    name = "L0Calo_E10_DenPerE";
    BookHisto(new TH1F(name, name+";Total LKr energy;L0Calo E10 Efficiency", 200, 0, 100));

    name = "L0Calo_ClusterEnergy_2";
    BookHisto(new TH1F(name, "Cluster energy;Energy [GeV]", 200, 0, 100));

    ///////////////////////////////////////////
    //// RICH multiplicity plots
    ///////////////////////////////////////////
    name = "RICH_L0_Mult_Kmu2"; BookHisto(name, new TH1F(name, name, 256, -0.5, 255.5));
    name = "RICH_L0_Mult_K3pi"; BookHisto(name, new TH1F(name, name, 256, -0.5, 255.5));
    name = "RICH_L0_Mult_Kmu2_R3"; BookHisto(name, new TH1F(name, name, 256, -0.5, 255.5));
    name = "RICH_L0_Mult_K3pi_R3"; BookHisto(name, new TH1F(name, name, 256, -0.5, 255.5));

    BookHisto("RICH_L0_Mult_SC_Mult_Kmu2",
        new TH2F("RICH_L0_Mult_SC_Mult_Kmu2",
          "RICH L0 multiplicity vs offline SC multiplicity from Kmu2 events;Offline SC multiplicity;L0 multiplicity",
          256, -0.5, 255.5, 256, -0.5, 255.5));
    name = "RICH_L0_Mult_SC_Mult_K3pi";
    BookHisto("RICH_L0_Mult_SC_Mult_K3pi",
        new TH2F("RICH_L0_Mult_SC_Mult_K3pi",
          "RICH L0 multiplicity vs offline SC multiplicity for K3pi events;Offline SC multiplicity;L0 multiplicity",
          256, -0.5, 255.5, 256, -0.5, 255.5));
    BookHisto("RICH_L0_Mult_SC_Mult",
        new TH2F("RICH_L0_Mult_SC_Mult",
          "RICH L0 multiplicity vs offline SC multiplicity;Offline SC multiplicity;L0 multiplicity",
          256, -0.5, 255.5, 256, -0.5, 255.5));
  }

  else { // Reading my own output

    Bool_t hasRunNumber = RequestHistogram(fAnalyzerName, "RunNumber", true);
    Bool_t hasEOR       = RequestHistogram(fAnalyzerName, "Q1_NumPerRun", true);

    /////////////////////////////////////////////////////
    ///// Histo mode stage 2
    /////////////////////////////////////////////////////
    if(!hasRunNumber && hasEOR){

      for(unsigned int i = 0 ; i < nPerBurst.size() ; ++i){
        name  = nPerBurst[i] ;
        name += "_NumPerRun" ;
        h_EpR.push_back(RequestHistogram(fAnalyzerName, name, true));
        name  = nPerBurst[i] ;
        name += "_DenPerRun" ;
        h_EpR.push_back(RequestHistogram(fAnalyzerName, name, true));
      }

    }

    /////////////////////////////////////////////////////
    ///// Histo mode stage 1
    /////////////////////////////////////////////////////
    if(hasRunNumber && hasEOR){

      std::cout << user_normal() << "Trigger thresholds are: " << endl;
      for(unsigned int i = 0 ; i < nPerBurst.size() ; ++i){
        std::cout << user_normal() << " Trigger " << nPerBurst[i] << " " << fBadTriggerThresholds[i] << std::endl;
      }
      std::cout << user_normal() << std::endl;

      // Retreive run number
      fhRunNumber = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "RunNumber", true));
      fhBurstTime = static_cast<TH1F*>(RequestHistogram(fAnalyzerName, "BurstTime", true));

      h_info.reserve(50);
      h_info.push_back(RequestHistogram(fAnalyzerName, "DataType", true)); // 0
      h_info.push_back(RequestHistogram(fAnalyzerName, "TriggerType", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "TriggerFlagsData", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "TriggerFlagsBeforeL1", true)); // BeforeL1
      h_info.push_back(RequestHistogram(fAnalyzerName, "UTMC_1", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "UTMC_C", true)); // 5
      h_info.push_back(RequestHistogram(fAnalyzerName, "LKrEnergy_1", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "LKrEnergy_C", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "LKrEnergy_K2pi", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "LKrEnergy_Kmu2", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "LKrEnergy2D", true)); // 10
      h_info.push_back(RequestHistogram(fAnalyzerName, "NewCHODQuads_1", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "NewCHODQuads_2", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "NewCHODQuads_3", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "NewCHODQuads_4", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "NewCHODMap", true)); // 15
      h_info.push_back(RequestHistogram(fAnalyzerName, "NewCHODunMap", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "SpasimirPlotI", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "SpasimirPlotO", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "SpasimirPlotA", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "LKrEnergy_4", true)); // 20
      h_info.push_back(RequestHistogram(fAnalyzerName, "MaskCorrelationRaw", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "MaskCorrelationRawWDS", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "SpasimirPlot_pip", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "SpasimirPlot_Mask1", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "ControlVsArgonion", true)); // 25
      h_info.push_back(RequestHistogram(fAnalyzerName, "nL0TPSpecialTriggers", true));
      h_info.push_back(RequestHistogram(fAnalyzerName, "nControlEvents", true)); //27

      h_info.push_back(RequestHistogram(fAnalyzerName, "L0Calo_ClusterEnergy", true)); //28
      h_info.push_back(RequestHistogram(fAnalyzerName, "L0Calo_ClusterTime", true)); //29
      h_info.push_back(RequestHistogram(fAnalyzerName, "L0Calo_ClusterXY", true)); //30

      h_info.push_back(RequestHistogram(fAnalyzerName, "L0Calo_E20_NumPerE", true)); //31
      h_info.push_back(RequestHistogram(fAnalyzerName, "L0Calo_E20_DenPerE", true)); //32
      h_info.push_back(RequestHistogram(fAnalyzerName, "L0Calo_E10_NumPerE", true)); //33
      h_info.push_back(RequestHistogram(fAnalyzerName, "L0Calo_E10_DenPerE", true)); //34

      h_info.push_back(RequestHistogram(fAnalyzerName, "HasControlPrimitive", true)); //35
      h_info.push_back(RequestHistogram(fAnalyzerName, "HasPhysicsPrimitive", true)); //36

      h_info.push_back(RequestHistogram(fAnalyzerName, "UTMC_M", true)); // 37

      // starts at 38.
      for(int i=0; i<fNMasks+1; ++i){
        h_info.push_back(RequestHistogram(fAnalyzerName, Form("nEventsMask%i",i), true));
        h_info.push_back(RequestHistogram(fAnalyzerName, Form("nEventsVsArgonionMask%i",i), true));
      }


      h_muv3quadrants.push_back( static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "MUV3Quads_1", true)));
      h_muv3quadrants.push_back( static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "MUV3Quads_2", true)));
      h_muv3quadrants.push_back( static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "MUV3Quads_3", true)));
      h_muv3quadrants.push_back( static_cast<TH2F*>(RequestHistogram(fAnalyzerName, "MUV3Quads_4", true)));

      for(int i=0; i<fNMasks; ++i){
        h_ots.push_back(RequestHistogram(fAnalyzerName, Form("OTS_MM2_Mask%i",i), true));
        h_ots.push_back(RequestHistogram(fAnalyzerName, Form("OTS_MM2vsP_Mask%i",i), true));
      }

      h_eobdata.push_back(RequestHistogram(fAnalyzerName, "ArgoCount", true));
      for(int i=0; i<fNMasks; ++i){
        h_eobdata.push_back(RequestHistogram(fAnalyzerName, Form("TriggersGend%i",i), true));
      }
      for(int i=0; i<fNMasks; ++i){
        h_eobdata.push_back(RequestHistogram(fAnalyzerName, Form("TriggersSent%i",i), true));
      }
      for(int i=0; i<fNMasks; ++i){
        h_eobdata.push_back(RequestHistogram(fAnalyzerName, Form("Downscaling%i",i), true));
      }

      name = "PrimitiveTSBits16_RICH";
      h_primbits.push_back(RequestHistogram(fAnalyzerName, name, true));
      name = "PrimitiveTSBits16_CHOD";
      h_primbits.push_back(RequestHistogram(fAnalyzerName, name, true));

      for(unsigned int i=0; i<primitives.size(); ++i){
        name = Form("PrimitiveFTBits%s_RICH",primitives[i].Data());
        h_primbits.push_back(RequestHistogram(fAnalyzerName, name, true));
      }

      for(unsigned int i=0; i<primitives.size(); ++i){
        name = Form("PrimitiveFTBits%s_CHOD",primitives[i].Data());
        h_primbits.push_back(RequestHistogram(fAnalyzerName, name, true));
      }

      h_muv3eob.push_back(RequestHistogram(fAnalyzerName, "MUV3EOBPPScaler", true)) ;
      h_muv3eob.push_back(RequestHistogram(fAnalyzerName, "MUV3EOBPPTightHits", true)) ;
      h_muv3eob.push_back(RequestHistogram(fAnalyzerName, "MUV3EOBPPLooseHits", true)) ;
      for(int i = 0 ; i < 4 ; ++i){
        h_muv3eob.push_back(RequestHistogram(fAnalyzerName, Form("MUV3EOBPPErrorCounts%i",i), true)) ;
      }
      h_muv3eob.push_back(RequestHistogram(fAnalyzerName, "MUV3EOBSLTight", true)) ;
      h_muv3eob.push_back(RequestHistogram(fAnalyzerName, "MUV3EOBSLErrorCounts", true)) ;

      h_newchodeob.push_back(RequestHistogram(fAnalyzerName, "NewCHODEOBPPScaler", true)) ;
      h_newchodeob.push_back(RequestHistogram(fAnalyzerName, "NewCHODEOBPPTightHits", true)) ;
      h_newchodeob.push_back(RequestHistogram(fAnalyzerName, "NewCHODEOBPPLooseHits", true)) ;
      for(int i = 0 ; i < 4 ; ++i){
        h_newchodeob.push_back(RequestHistogram(fAnalyzerName, Form("NewCHODEOBPPErrorCounts%i",i), true)) ;
      }
      h_newchodeob.push_back(RequestHistogram(fAnalyzerName, "NewCHODEOBSLTight", true)) ;
      h_newchodeob.push_back(RequestHistogram(fAnalyzerName, "NewCHODEOBSLErrorCounts", true)) ;

      for(unsigned int i = 0 ; i < nPerBurst.size() ; ++i){
        name  = nPerBurst[i] ;
        name += "_NumPerBurst" ;
        h_EpB.push_back(RequestHistogram(fAnalyzerName, name, true));
        name  = nPerBurst[i] ;
        name += "_DenPerBurst" ;
        h_EpB.push_back(RequestHistogram(fAnalyzerName, name, true));
        name  = nPerBurst[i] ;
        name += "_BurstEfficiency1D" ;
        h_E1D.push_back(RequestHistogram(fAnalyzerName, name, true));

        name  = nPerBurst[i] ;
        name += "_NumPerTrackMom";
        h_EpP.push_back(RequestHistogram(fAnalyzerName, name, true));
        name  = nPerBurst[i] ;
        name += "_DenPerTrackMom";
        h_EpP.push_back(RequestHistogram(fAnalyzerName, name, true));

        name  = nPerBurst[i] ;
        name += "_NumPerZposition" ;
        h_EpZ.push_back(RequestHistogram(fAnalyzerName, name, true));
        name  = nPerBurst[i] ;
        name += "_DenPerZposition" ;
        h_EpZ.push_back(RequestHistogram(fAnalyzerName, name, true));

        name  = nPerBurst[i] ;
        name += "_NumPerRun" ;
        h_EpR.push_back(RequestHistogram(fAnalyzerName, name, true));
        name  = nPerBurst[i] ;
        name += "_DenPerRun" ;
        h_EpR.push_back(RequestHistogram(fAnalyzerName, name, true));

        name  = nPerBurst[i] ;
        name += "_NumPerOccupancy" ;
        h_EpO.push_back(RequestHistogram(fAnalyzerName, name, true));
        name  = nPerBurst[i] ;
        name += "_DenPerOccupancy" ;
        h_EpO.push_back(RequestHistogram(fAnalyzerName, name, true));
      }

      h_Occupancy.reserve(primitives.size()*2 + 2);
      for(unsigned int i=0; i<primitives.size(); ++i){
        name = primitives[i](1, primitives[i].Length());
        name += "_NumPerOccupancy" ;
        h_Occupancy.push_back(RequestHistogram(fAnalyzerName, name, true));
        name = primitives[i](1, primitives[i].Length());
        name += "_DenPerOccupancy" ;
        h_Occupancy.push_back(RequestHistogram(fAnalyzerName, name, true));
      }
      name = "NewCHOD_NumPerTimeslot";
      h_Occupancy.push_back(RequestHistogram(fAnalyzerName, name, true));
      name = "NewCHOD_DenPerTimeslot";
      h_Occupancy.push_back(RequestHistogram(fAnalyzerName, name, true));

      name = "NewCHOD_NumPerTimesplit";
      h_Occupancy.push_back(RequestHistogram(fAnalyzerName, name, true));
      name = "NewCHOD_DenPerTimesplit";
      h_Occupancy.push_back(RequestHistogram(fAnalyzerName, name, true));

      h_nprimitives.resize(primitives.size(), NULL);
      for(unsigned int i=0; i<primitives.size(); ++i){
        name = Form("nPrimitives%s", primitives[i].Data());
        h_nprimitives[i] = RequestHistogram(fAnalyzerName, name, true);
      }

      h_RICH.reserve(5) ;
      h_RICH.push_back(RequestHistogram(fAnalyzerName, "RICH_L0_Mult_Kmu2", true));
      h_RICH.push_back(RequestHistogram(fAnalyzerName, "RICH_L0_Mult_K3pi", true));
      h_RICH.push_back(RequestHistogram(fAnalyzerName, "RICH_L0_Mult_Kmu2_R3", true));
      h_RICH.push_back(RequestHistogram(fAnalyzerName, "RICH_L0_Mult_K3pi_R3", true));
      h_RICH.push_back(RequestHistogram(fAnalyzerName, "RICH_L0_Mult_SC_Mult", true));

      h_1Dcorr.reserve(primitives.size()*4);
      for (UInt_t j=0; j < primitives.size(); j++) { // The 5 primitive-generating detectors
        name.Form("1DCorr_Phys1E%s", primitives[j].Data());
        h_1Dcorr.push_back(RequestHistogram(fAnalyzerName, name, true));
        name.Form("1DCorr_PhysM%s", primitives[j].Data());
        h_1Dcorr.push_back(RequestHistogram(fAnalyzerName, name, true));
        name.Form("1DCorr_Control%s", primitives[j].Data());
        h_1Dcorr.push_back(RequestHistogram(fAnalyzerName, name, true));
        name.Form("1DCorr_Phys1I%s", primitives[j].Data());
        h_1Dcorr.push_back(RequestHistogram(fAnalyzerName, name, true));

        name.Form("2DCorr_Phys1E%s", primitives[j].Data());
        h_2Dcorr.push_back(RequestHistogram(fAnalyzerName, name, true));
        name.Form("2DCorr_PhysM%s", primitives[j].Data());
        h_2Dcorr.push_back(RequestHistogram(fAnalyzerName, name, true));
        name.Form("2DCorr_Control%s", primitives[j].Data());
        h_2Dcorr.push_back(RequestHistogram(fAnalyzerName, name, true));
        name.Form("2DCorr_Phys1I%s", primitives[j].Data());
        h_2Dcorr.push_back(RequestHistogram(fAnalyzerName, name, true));

        name.Form("DigiTimeRaw_Phys1E%s", primitives[j].Data());
        h_DigiRaw.push_back(RequestHistogram(fAnalyzerName, name, true));
        name.Form("DigiTimeRaw_Phys1I%s", primitives[j].Data());
        h_DigiRaw.push_back(RequestHistogram(fAnalyzerName, name, true));
        name.Form("DigiTimeRaw_PhysM%s", primitives[j].Data());
        h_DigiRaw.push_back(RequestHistogram(fAnalyzerName, name, true));
        name.Form("DigiTimeRaw_Control%s", primitives[j].Data());
        h_DigiRaw.push_back(RequestHistogram(fAnalyzerName, name, true));

      }

      for(unsigned int j = 1 ; j < primitives.size() ; ++j){
        TString hname = primitives[j] ;
        if( hname.Contains("Calo")) continue ;
        name.Form("HitCorrelations2D_Control%s%s", "_RICH", primitives[j].Data());
        h_HitCorr2D.push_back(RequestHistogram(fAnalyzerName, name, true));
      }
      for(unsigned int j = 1 ; j < primitives.size()-2 ; ++j){
        name.Form("HitCorrelations2D_Control%s%s", "_CHOD", primitives[j].Data());
        h_HitCorr2D.push_back(RequestHistogram(fAnalyzerName, name, true));
      }

      h_temp.reserve(events.size()*primitives.size()*6);
      for (UInt_t i=0; i < events.size(); i++) { // The seven event types
        for (UInt_t j=0; j < primitives.size(); j++) { // The 5 primitive-generating detectors
          name.Form("Passed%s%s", events[i].Data(), primitives[j].Data());
          h_temp.push_back(RequestHistogram(fAnalyzerName, name, true));
          name.Form("Total%s%s", events[i].Data(), primitives[j].Data());
          h_temp.push_back(RequestHistogram(fAnalyzerName, name, true));
          name.Form("L0Trigger_N0%s%s", events[i].Data(), primitives[j].Data());
          h_temp.push_back(RequestHistogram(fAnalyzerName, name, true));
          name.Form("L0Trigger_N1%s%s", events[i].Data(), primitives[j].Data());
          h_temp.push_back(RequestHistogram(fAnalyzerName, name, true));
          name.Form("L0Trigger_N2%s%s", events[i].Data(), primitives[j].Data());
          h_temp.push_back(RequestHistogram(fAnalyzerName, name, true));
          name.Form("L0Trigger_N3%s%s", events[i].Data(), primitives[j].Data());
          h_temp.push_back(RequestHistogram(fAnalyzerName, name, true));
        }
      }
    }
  }
}

void L0TriggerResponse::GetHardcodedFTB(){
  fFineTimeBit = 0 ;
  if(fRunID>5949) fFineTimeBit = 1 ;
  if(fRunID>7876) fFineTimeBit = 2 ;
}

void L0TriggerResponse::ProcessSpecialTriggerUser(int , unsigned int ){
  //if (!fReadingData) return; // no action if reading its own output in --histo mode

  ///////////////////////////////////////////////////
  //// Read Special Triggers
  ///////////////////////////////////////////////////
  fNL0++;

  ///////////////////////////////////////////////////////////////
  // Get FineTimeBit if it's available.
  // NB. To use "real" ftb EOB must be last special trigger!
  ///////////////////////////////////////////////////////////////
  fFineTimeBit=-1;

  L0TPSpecialTrigger* L0TPEOB = GetL0SpecialTrigger();
  if(L0TPEOB==nullptr){
    GetHardcodedFTB();
    std::cout << user_normal() << "L0TPSpecialTrigger is missing."
      << " Using hard-coded granularity: " << fFineTimeBit << std::endl;
    FillHisto("FineTimeBit_Null", fBurstID, fFineTimeBit);
  }
  else if(L0TPEOB->GetTriggerType()==0x23){
    fFineTimeBit = L0TPEOB->GetFineTimeBits(); // get "real" ftb
    std::cout << user_normal() << "Using L0TP granularity: " << fFineTimeBit << std::endl;
    FillHisto("FineTimeBit_Data", fBurstID, fFineTimeBit);
  }
}

void L0TriggerResponse::ProcessEOBEvent(){
  L0TPSpecialTrigger* L0TPEOB = GetL0SpecialTrigger();
  if(L0TPEOB){
    // check L0 masks
    std::vector< L0Mask > L0Masks;
    L0Masks = L0TPEOB->GetL0Masks();
    for(unsigned int i=0;i<L0Masks.size();i++){
      FillHistoArray("TriggersGend",i, fBurstID, L0Masks.at(i).GetNTriggersGenerated());
      FillHistoArray("TriggersSent",i, fBurstID, L0Masks.at(i).GetNTriggersSent());
      FillHistoArray("Downscaling",i,  fBurstID, L0Masks.at(i).GetDownscalingFactor());
      MaskDownscales[i] = L0Masks.at(i).GetDownscalingFactor() ;
    }
    // check L0 primitives
    nPrimitives.clear();
    nPrimitives = L0TPEOB->GetNPrimitives();
    FillHisto("nPrimitives_CHOD",    fBurstID, nPrimitives[0]);
    FillHisto("nPrimitives_RICH",    fBurstID, nPrimitives[1]);
    FillHisto("nPrimitives_LAV12",   fBurstID, nPrimitives[2]);
    FillHisto("nPrimitives_MUV3",    fBurstID, nPrimitives[3]);
    FillHisto("nPrimitives_NewCHOD", fBurstID, nPrimitives[4]);
    FillHisto("nPrimitives_Calo",    fBurstID, nPrimitives[6]);

    fHasSpecialTrigger = true ; // downscaling factor is known.
  }
  else{
    std::cout << user_normal() << "Warning: could not find L0TP special trigger!" << std::endl;
    fHasSpecialTrigger = false ; // downscaling factor not known.
  }

  fArgonionCount=-1.0;
  BeamSpecialTrigger* Beam = GetBeamSpecialTrigger();
  if(Beam){
    fArgonionCount=Beam->GetCountsARGONION()/100000000.;
  }
  else{
    std::cout << user_normal() << "Error: No BeamSpecialTrigger in ProcessEOBEvent!" << std::endl;
  }
  FillHisto("ArgoCount", fBurstID, fArgonionCount) ;

  TSpecialTriggerEvent* MUV3ST = static_cast<TSpecialTriggerEvent*>(GetEvent("MUV3", "SpecialTrigger"));
  if(MUV3ST){
    if(fRunID<7000) PrimitiveEOB(MUV3ST,1, "MUV3");
    else            PrimitiveEOB(MUV3ST,8, "MUV3");
  }
  else{
    std::cout << user_normal() << "Error: No MUV3 special trigger in ProcessEOBEvent!" << std::endl;
  }

  TSpecialTriggerEvent* NewCHODST = static_cast<TSpecialTriggerEvent*>(GetEvent("NewCHOD", "SpecialTrigger"));
  if(NewCHODST){
    if(fRunID<7000) PrimitiveEOB(NewCHODST,16, "NewCHOD");
    else            PrimitiveEOB(NewCHODST,8,  "NewCHOD");
  }
  else{
    std::cout << user_normal() << "Error: No NewCHOD special trigger in ProcessEOBEvent!" << std::endl;
  }
}

void L0TriggerResponse::Process(Int_t) {
  if (!fReadingData) return; // no action if reading its own output in --histo mode

  ///////////////////////////////////////////////////
  /// Check a FineTimeBit is available
  ///////////////////////////////////////////////////
  if(fFineTimeBit==-1){
    GetHardcodedFTB();
    std::cout << user_normal() << "L0TP EOB is missing."
      << " Using hard-coded granularity: " << fFineTimeBit << std::endl;
    FillHisto("FineTimeBit_Hard", fBurstID, fFineTimeBit);
  }

  ///////////////////////////////////////////////////
  /// Trigger and data type plots
  ///////////////////////////////////////////////////
  UChar_t DType = GetL0Data()->GetDataType();
  UChar_t TType = GetL0Data()->GetTriggerType();
  for(int i = 0 ; i < 8 ; ++i){
    bool Dflag = ((DType>>i)&0x1);
    if(Dflag) FillHisto("DataType", i);
    bool Tflag = ((TType>>i)&0x1);
    if(Tflag) FillHisto("TriggerType", i);
  }

  ///////////////////////////////////////////////////
  /// Check for valid event
  ///////////////////////////////////////////////////
  ControlData = (GetL0Data()->GetDataType() & 0x10);
  PhysicsData = (GetL0Data()->GetDataType() & 0x1 );

  if(ControlData){
    L0Primitive ControlPrim = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0CHOD);
    Bool_t HasControlPrimitive = ControlPrim.GetPrimitiveID()>0;
    FillHisto("HasControlPrimitive", HasControlPrimitive);
    ControlData &= HasControlPrimitive;
  }
  if(PhysicsData){
    L0Primitive PhysicsPrim = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0RICH);
    Bool_t HasPhysicsPrimitive = PhysicsPrim.GetPrimitiveID()>0;
    FillHisto("HasPhysicsPrimitive", HasPhysicsPrimitive);
    PhysicsData &= HasPhysicsPrimitive;
  }

  if(!PhysicsData && !ControlData) return ;

  ///////////////////////////////////////////////////
  /// Use best timing to compute standard values
  ///////////////////////////////////////////////////
  if(PhysicsData){
    fRef = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0RICH).GetFineTime();
    fRefTime = fRef*TdcCalib ;

    PrimInfo.clear();
    PrimInfo.reserve(7);
    for(int i=0; i<7; ++i) GetPrimitiveAndTimes(i,true);
  }
  else{
    fRef = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0CHOD).GetFineTime();
    fRefTime = fRef*TdcCalib ;

    PrimInfo.clear();
    PrimInfo.reserve(7);
    for(int i=0; i<7; ++i) GetPrimitiveAndTimes(i,false);
  }

  ///////////////////////////////////////////////////
  /// Get detector occupancies
  ///////////////////////////////////////////////////
  GetDetOccupancies();

  ///////////////////////////////////////////////////
  /// Get total LKr energy
  ///////////////////////////////////////////////////
  fLKrEnergy = 0. ;
  fLKrEnergy = GetLKrTotEnergy() ;

  ///////////////////////////////////////////////////
  /// Get RefTime and PrimInfo
  ///////////////////////////////////////////////////
  fRef = 0 ;

  // *********************************************************************
  // ******************* START CONTROL EVENTS ****************************
  // *********************************************************************
  if(ControlData){
    fRef = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0CHOD).GetFineTime();
    fRefTime = fRef*TdcCalib ;

    PrimInfo.clear();
    PrimInfo.reserve(7);
    for(int i=0; i<7; ++i) GetPrimitiveAndTimes(i,false);

    IncrementCounter("nControlEvents");

    ///////////////////////////////////////////////////
    /// Create other plots
    ///////////////////////////////////////////////////
    FillLKrEnergy2D();
    FillHisto("UTMC_C", fUTMCHits) ;
    FillHisto("LKrEnergy_C", fLKrEnergy/1000.) ;
    RichMultPlot("Calib_Control", kL0RICH);
    FillOccupancyPlots(0); // all except CHOD

    ///////////////////////////////////////////////////
    /// Plot DigiRawFineTime plots
    ///////////////////////////////////////////////////
    DigiTimeRaw("RICH",    false) ;
    DigiTimeRaw("LAV12",   false) ;
    DigiTimeRaw("MUV3",    false) ;
    DigiTimeRaw("NewCHOD", false) ;
    DigiTimeRaw("Calo",    false) ;
    DigiTimeRaw("CHOD",    false) ;

    ///////////////////////////////////////////////////
    /// Plots of primitive bits
    ///////////////////////////////////////////////////
    GetPrimitiveBits(false);

    ///////////////////////////////////////////////////
    /// Create correlation plots
    ///////////////////////////////////////////////////
    PrimCorrelationPlots("1DCorr_Control_RICH",    kL0RICH);
    PrimCorrelationPlots("1DCorr_Control_LAV12",   kL0LAV);
    PrimCorrelationPlots("1DCorr_Control_MUV3",    kL0MUV3);
    PrimCorrelationPlots("1DCorr_Control_NewCHOD", kL0NewCHOD);
    PrimCorrelationPlots("1DCorr_Control_Calo",    kL0Calo);
    PrimCorrelationPlots("1DCorr_Control_CHOD",    kL0CHOD);

    ///////////////////////////////////////////////////
    // NewCHOD Quadrant mapping
    ///////////////////////////////////////////////////
    UInt_t PrimitiveID = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0NewCHOD).GetPrimitiveID();
    TRecoNewCHODEvent* NCevent = GetEvent<TRecoNewCHODEvent>();
    if(NCevent->GetNHits()==1 && PrimitiveID){
      TRecoNewCHODHit* hit = static_cast<TRecoNewCHODHit*>(NCevent->GetHit(0));
      int Tile = hit->GetTileID() ;
      for (Int_t i=0; i<100; i++) {
        if (fGeo->GetScintMap(i)==Tile%100) {
          Int_t BrickID = 100*(Tile/100)+i;
          Double_t x = fGeo->GetBrickCentreX(BrickID) ;
          Double_t y = fGeo->GetBrickCentreY(BrickID) ;
          if(PrimitiveID&0x1) FillHisto("NewCHODQuads_1", x/1000., y/1000.) ;
          if(PrimitiveID&0x2) FillHisto("NewCHODQuads_2", x/1000., y/1000.) ;
          if(PrimitiveID&0x4) FillHisto("NewCHODQuads_3", x/1000., y/1000.) ;
          if(PrimitiveID&0x8) FillHisto("NewCHODQuads_4", x/1000., y/1000.) ;
        }
      }
    }

    ///////////////////////////////////////////////////
    // MUV3 Quadrant mapping
    ///////////////////////////////////////////////////
    PrimitiveID = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0MUV3).GetPrimitiveID();
    TRecoMUV3Event* M3event = GetEvent<TRecoMUV3Event>();
    if(M3event->GetNCandidates()==1 && PrimitiveID){
      TRecoMUV3Candidate* cand = static_cast<TRecoMUV3Candidate*>(M3event->GetCandidate(0));
      Int_t Outer = cand->IsOuter();
      if(Outer){
        TVector3 pos = cand->GetPosition();
        if(PrimitiveID&0x1) FillHisto("MUV3Quads_1", pos.X()/1000.0, pos.Y()/1000.0) ;
        if(PrimitiveID&0x2) FillHisto("MUV3Quads_2", pos.X()/1000.0, pos.Y()/1000.0) ;
        if(PrimitiveID&0x4) FillHisto("MUV3Quads_3", pos.X()/1000.0, pos.Y()/1000.0) ;
        if(PrimitiveID&0x8) FillHisto("MUV3Quads_4", pos.X()/1000.0, pos.Y()/1000.0) ;
      }
    }

    HitCorrelations("_RICH", "_LAV12");
    HitCorrelations("_RICH", "_MUV3");
    HitCorrelations("_RICH", "_NewCHOD");
    HitCorrelations("_RICH", "_CHOD");
    HitCorrelations("_CHOD", "_LAV12");
    HitCorrelations("_CHOD", "_MUV3");
    HitCorrelations("_CHOD", "_NewCHOD");

    ///////////////////////////////////////////////////
    /// Run event selections and produce plots
    ///////////////////////////////////////////////////
    K3piSelection();
    Kmu2Selection();
    K2piSelection();
    LavVetoSelection();
    OnePionSelection();
  }
  // *********************************************************************
  // ******************* END OF CONTROL EVENTS ***************************
  // *********************************************************************

  // *********************************************************************
  // ******************* START PHYSICS EVENTS ****************************
  // *********************************************************************
  if(PhysicsData){
    fRef = GetL0Data()->GetPrimitive(kL0TriggerSlot, kL0RICH).GetFineTime();
    fRefTime = fRef*TdcCalib ;

    PrimInfo.clear();
    PrimInfo.reserve(7);
    for(int i=0; i<7; ++i) GetPrimitiveAndTimes(i,true);

    FillOccupancyPlots(1); //CHOD

    // Passed Mask1 and 4
    Bool_t HasMask1 = (GetL0Data()->GetTriggerFlags() & 0x2);
    Bool_t HasMask4 = (GetL0Data()->GetTriggerFlags() & 0x10);

    if(HasMask1) {
                 FillHisto("LKrEnergy_1",fLKrEnergy/1000.) ;
                 FillL0CaloPlots();
                 FillHisto("UTMC_1", fUTMCHits) ;
    }
    else         FillHisto("UTMC_M", fUTMCHits) ; // physics not Mask1

    if(HasMask4) FillHisto("LKrEnergy_4",fLKrEnergy/1000.) ;

    ///////////////////////////////////////////////////
    /// Plot DigiRawFineTime plots
    ///////////////////////////////////////////////////
    DigiTimeRaw("RICH",    true) ;
    DigiTimeRaw("LAV12",   true) ;
    DigiTimeRaw("MUV3",    true) ;
    DigiTimeRaw("NewCHOD", true) ;
    DigiTimeRaw("Calo",    true) ;
    DigiTimeRaw("CHOD",    true) ;

    ///////////////////////////////////////////////////
    /// Plots of primitive bits
    ///////////////////////////////////////////////////
    GetPrimitiveBits(true);

    ///////////////////////////////////////////////////
    /// Create mask correlation plot
    /// using L1AutoPass events
    ///////////////////////////////////////////////////
    Int_t level1Word = (GetEventHeader()->GetTriggerType()&0x00FF00)>>8;
    Bool_t L1AutoPass = ((level1Word&128) == 128) ;

    UShort_t TrigFlag = GetL0Data()->GetTriggerFlags();
    for(int i=0; i<fNMasks; ++i){
      bool flag = ((TrigFlag>>i)&0x1);
      if(!flag) continue;
      MissingMasses(i) ;
      FillHisto("TriggerFlagsData", i);
      if(fArgonionCount>0.001) FillHistoArray("nEventsMask",i, fBurstID, 1.0/fArgonionCount);
      if(L1AutoPass) FillHisto("TriggerFlagsBeforeL1", i) ;
      for(int j=0; j<fNMasks; ++j){
        bool flag2 = ((TrigFlag>>j)&0x1);
        if(!flag2) continue;
        FillHisto("MaskCorrelationData", i, j);
        if(L1AutoPass) FillHisto("MaskCorrelationRawBurst", i, j);
      }
    }

    ///////////////////////////////////////////////////
    /// Create correlation plots
    ///////////////////////////////////////////////////
    PrimCorrelationPlots("1DCorr_PhysM_RICH",    kL0RICH);
    PrimCorrelationPlots("1DCorr_PhysM_LAV12",   kL0LAV);
    PrimCorrelationPlots("1DCorr_PhysM_MUV3",    kL0MUV3);
    PrimCorrelationPlots("1DCorr_PhysM_NewCHOD", kL0NewCHOD);
    PrimCorrelationPlots("1DCorr_PhysM_Calo",    kL0Calo);
    PrimCorrelationPlots("1DCorr_PhysM_CHOD",    kL0CHOD);

    ///////////////////////////////////////////////////
    /// Trigger response for each trigger mask
    ///////////////////////////////////////////////////
    for(int i = 0 ; i < 16 ; i++){
      int bit = pow(2.,i) ;
      bool L0Mask = (GetL0Data()->GetTriggerFlags() & bit) ;
      if(L0Mask){

        TString name = Form("_Mask%i",i) ;
        ComputeEfficiency(name+"_RICH",    kL0RICH    );
        ComputeEfficiency(name+"_LAV12",   kL0LAV     );
        ComputeEfficiency(name+"_MUV3",    kL0MUV3    );
        ComputeEfficiency(name+"_NewCHOD", kL0NewCHOD );
        ComputeEfficiency(name+"_Calo",    kL0Calo    );
        if(i==1) FillSpasimirPlot(kL0NewCHOD);
      }
    }

    ///////////////////////////////////////////////////
    /// Run CHOD selection and efficiency calc
    ///////////////////////////////////////////////////
    CHODEfficiencySelection();
  }
  // *********************************************************************
  // ******************* END OF PHYSICS EVENTS ***************************
  // *********************************************************************

}

void L0TriggerResponse::GetPrimitiveBits(Bool_t mode){

  TString name = "";

  if(!mode){ // control events (CHOD time)
    GetPrimitiveTSBits(0);

    name.Form("PrimitiveFTBits_CHOD_CHOD");
    GetPrimitiveFTBits(kL0CHOD, name);

    name.Form("PrimitiveFTBits_RICH_CHOD");
    GetPrimitiveFTBits(kL0RICH,name);
    name.Form("PrimitiveFTBits_LAV12_CHOD");
    GetPrimitiveFTBits(kL0LAV,name);
    name.Form("PrimitiveFTBits_MUV3_CHOD");
    GetPrimitiveFTBits(kL0MUV3,name);
    name.Form("PrimitiveFTBits_NewCHOD_CHOD");
    GetPrimitiveFTBits(kL0NewCHOD,name);
    name.Form("PrimitiveFTBits_Calo_CHOD");
    GetPrimitiveFTBits(kL0Calo,name);
  }
  else{ // physics events (RICH time)
    GetPrimitiveTSBits(5);

    name.Form("PrimitiveFTBits_RICH_RICH");
    GetPrimitiveFTBits(kL0RICH, name);

    name.Form("PrimitiveFTBits_CHOD_RICH");
    GetPrimitiveFTBits(kL0CHOD, name);
  }
}


void L0TriggerResponse::GetPrimitiveTSBits(UInt_t det){
  ULong_t time = GetL0Data()->GetTimeStamp();

  TString name = "";
  name.Form("PrimitiveTSBits16%s",primitives[det].Data());
  FillHisto(name, time%256);
}

void L0TriggerResponse::GetPrimitiveFTBits(UInt_t l0det, TString name){
  for(int i=0;i<3;++i){
    L0Primitive a = GetL0Data()->GetPrimitive(i, l0det);
    if(a.GetPrimitiveID()==0) continue ;
    FillHisto(name, a.GetFineTime());
  }
}

void L0TriggerResponse::MissingMasses(int Mask){

  double kmom = 75000. ;
  double kmass = 493.667 ;
  TVector3 kmomV(90, 0., 75000) ;
  TLorentzVector P_k( kmomV, sqrt(kmom*kmom + kmass*kmass) )  ;

  TRecoSpectrometerEvent* Sevent = GetEvent<TRecoSpectrometerEvent>() ;
  for(int i = 0 ; i < Sevent->GetNCandidates() ; ++i){
    TRecoSpectrometerCandidate* fTrack = static_cast<TRecoSpectrometerCandidate*>(Sevent->GetCandidate(i));
    Double_t tdiff = fTrack->GetTime() - fRefTime ;
    if(fabs(tdiff)>30) continue;
    Double_t mm2 = GetM2(fTrack, P_k, MPI) ;
    FillHisto( Form("OTS_MM2_Mask%i", Mask), mm2) ;
    FillHisto( Form("OTS_MM2vsP_Mask%i", Mask), fTrack->GetMomentum()/1000., mm2) ;
  }

}

void L0TriggerResponse::FillL0CaloPlots(){

  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  if (Tracks.size()!=1) return;
  if (Tracks[0].GetCharge()!=1) return;
  if (Tracks[0].GetChi2()>20.0) return;
  if (Tracks[0].GetNChambers()!=4) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kSpectrometer, 0)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kSpectrometer, 1)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kSpectrometer, 2)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kSpectrometer, 3)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kLKr)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kLAV, kLAV12)) return;

  TRecoLKrEvent* LKRevent = GetEvent<TRecoLKrEvent>();
  if (Tracks[0].GetLKrEoP()>0.9 && LKRevent->GetNCandidates()==1) {
    TRecoLKrCandidate* Lcand = static_cast<TRecoLKrCandidate*>(LKRevent->GetCandidate(0));
    Double_t time = Lcand->GetTime()-fRefTime;
    Double_t e    = 1e-3*Lcand->GetClusterEnergy(); // [GeV]
    Double_t xcl  = 1e-3*Lcand->GetClusterX(); // [mm]
    Double_t ycl  = 1e-3*Lcand->GetClusterY(); // [mm]
    FillHisto("L0Calo_ClusterEnergy", e);
    if (e>30.0) {
      FillHisto("L0Calo_ClusterTime", time);
      FillHisto("L0Calo_ClusterXY", xcl, ycl);
    }
  }

}

Double_t L0TriggerResponse::GetM2(TRecoSpectrometerCandidate* fTrack,
    TLorentzVector& P_k,
    Double_t mass_hyp) {
  // input mev, returns gev2
  TVector3 P = fTrack->GetThreeMomentumBeforeMagnet();
  Double_t Ep = sqrt(P.Mag2()+mass_hyp*mass_hyp);
  TLorentzVector P_pi(P,Ep);

  Double_t M2 = (P_k-P_pi).M2();
  return M2/1E6;
}

void L0TriggerResponse::K3piSelection() {

  K3piSelResult result = MyK3piSelection() ;
  if ( !(result.success) ) return;

  if(fLKrEnergy>20000.){
    ComputeEfficiency("_K3pi_E20_RICH",    kL0RICH);
    ComputeEfficiency("_K3pi_E20_LAV12",   kL0LAV);
    ComputeEfficiency("_K3pi_E20_MUV3",    kL0MUV3);
    ComputeEfficiency("_K3pi_E20_NewCHOD", kL0NewCHOD);
    ComputeEfficiency("_K3pi_E20_Calo",    kL0Calo);
  }

  if(result.utmc){
    ComputeEfficiency("_K3pi_UTMC_RICH",    kL0RICH);
    ComputeEfficiency("_K3pi_UTMC_LAV12",   kL0LAV);
    ComputeEfficiency("_K3pi_UTMC_MUV3",    kL0MUV3);
    ComputeEfficiency("_K3pi_UTMC_NewCHOD", kL0NewCHOD);
    ComputeEfficiency("_K3pi_UTMC_Calo",    kL0Calo);
    FillEfficiencies("UTMC", kL0NewCHOD, UTMCbit);
  }

  if( !(result.q1) ) return ;
  ComputeEfficiency("_K3pi_Q1_RICH",    kL0RICH);
  ComputeEfficiency("_K3pi_Q1_LAV12",   kL0LAV);
  ComputeEfficiency("_K3pi_Q1_MUV3",    kL0MUV3);
  ComputeEfficiency("_K3pi_Q1_NewCHOD", kL0NewCHOD);
  ComputeEfficiency("_K3pi_Q1_Calo",    kL0Calo);
  FillEfficiencies("Q1",  kL0NewCHOD, 14);
  RichMultPlot("K3pi_Q1_RICH", kL0RICH);

  if( !(result.q2) ) return ;
  ComputeEfficiency("_K3pi_Q2_RICH",    kL0RICH);
  ComputeEfficiency("_K3pi_Q2_LAV12",   kL0LAV);
  ComputeEfficiency("_K3pi_Q2_MUV3",    kL0MUV3);
  ComputeEfficiency("_K3pi_Q2_NewCHOD", kL0NewCHOD);
  ComputeEfficiency("_K3pi_Q2_Calo",    kL0Calo);
  FillEfficiencies("Q2",  kL0NewCHOD, Q2bit);

  if( !(result.qx) ) return ;
  ComputeEfficiency("_K3pi_QX_RICH",    kL0RICH);
  ComputeEfficiency("_K3pi_QX_LAV12",   kL0LAV);
  ComputeEfficiency("_K3pi_QX_MUV3",    kL0MUV3);
  ComputeEfficiency("_K3pi_QX_NewCHOD", kL0NewCHOD);
  ComputeEfficiency("_K3pi_QX_Calo",    kL0Calo);
  FillEfficiencies("Qx",  kL0NewCHOD, QXbit);

  if( (result.nhits) > 3) return ;
  ComputeEfficiency("_K3pi_QX3H_RICH",    kL0RICH);
  ComputeEfficiency("_K3pi_QX3H_LAV12",   kL0LAV);
  ComputeEfficiency("_K3pi_QX3H_MUV3",    kL0MUV3);
  ComputeEfficiency("_K3pi_QX3H_NewCHOD", kL0NewCHOD);
  ComputeEfficiency("_K3pi_QX3H_Calo",    kL0Calo);
}

bool L0TriggerResponse::OneHitInTime(DownstreamTrack& Track){
  Bool_t oneHitInTime = false;
  for (Int_t i=0; i<Track.GetNNewCHODAssociationRecords(); ++i) {
    Double_t tdiff = Track.GetNewCHODCandidateTime(i) - fRefTime;
    if (fabs(tdiff)<10.0) oneHitInTime=true;
  }
  return oneHitInTime;
}

void L0TriggerResponse::CHODEfficiencySelection(){
  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  if(Tracks.size()!=1) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kSpectrometer, 0)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kSpectrometer, 1)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kSpectrometer, 2)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kSpectrometer, 3)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kLKr))     return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kNewCHOD)) return;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(&Tracks[0], kLAV, kLAV12)) return;
  if(!OneHitInTime(Tracks[0])) return; // within 10ns of trigger reference time
  FillEfficiencies("TCHOD", kL0CHOD, 14);
}

void L0TriggerResponse::OnePionSelection(){

  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  if(Tracks.size()!=1) return;
  if(!OneHitInTime(Tracks[0])) return;
  if(Tracks[0].GetNMUV3AssociationRecords()>0) return ;

  // Filling L0Calo efficiency vs event energy
  // Only meaningful for 2017 data!
  FillL0CaloEfficiency("L0Calo_E10_DenPerE", kL0Calo, E10bit);
  FillL0CaloEfficiency("L0Calo_E20_DenPerE", kL0Calo, E20bit);

  UInt_t Trigger = (PrimInfo[kL0Calo]).PrimitiveID_N3;
  Bool_t pass10 = (Trigger>>E10bit)&1;
  Bool_t pass20 = (Trigger>>E20bit)&1;

  if(pass10 && !pass20){
    FillHisto("L0Calo_ClusterEnergy_2", fLKrEnergy/1000.0);
  }
}

void L0TriggerResponse::K2piSelection() {

  Bool_t k2pi_selected = *(Bool_t*)GetOutput("K2piSelection.EventSelected");
  if (!k2pi_selected) return;

  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  if(!OneHitInTime(Tracks[0])) return;

  fTrackMom  = Tracks[0].GetMomentum();
  fZposition = Tracks[0].GetNominalBeamAxisVertex().Z();

  // Filling energy plot for K2pi events
  FillHisto("LKrEnergy_K2pi", fLKrEnergy/1000.) ;

  // Filling trigger response plots for k2pi events
  ComputeEfficiency("_K2pi_RICH",    kL0RICH);
  ComputeEfficiency("_K2pi_LAV12",   kL0LAV);
  ComputeEfficiency("_K2pi_MUV3",    kL0MUV3);
  ComputeEfficiency("_K2pi_NewCHOD", kL0NewCHOD);
  ComputeEfficiency("_K2pi_Calo",    kL0Calo);

  // Fill RICH efficiency plots if track is above threshold
  if(fTrackMom>15000){
    FillEfficiencies("ER2", kL0RICH, 14);
  }

  if(fLKrEnergy>20000.0){
    ComputeEfficiency("_K2pi_E20_RICH",    kL0RICH);
    ComputeEfficiency("_K2pi_E20_LAV12",   kL0LAV);
    ComputeEfficiency("_K2pi_E20_MUV3",    kL0MUV3);
    ComputeEfficiency("_K2pi_E20_NewCHOD", kL0NewCHOD);
    ComputeEfficiency("_K2pi_E20_Calo",    kL0Calo);
    FillEfficiencies("E20", kL0Calo, E20bit);
  }

  if(fUTMCHits<5){
    ComputeEfficiency("_K2pi_UTMC_RICH",    kL0RICH);
    ComputeEfficiency("_K2pi_UTMC_LAV12",   kL0LAV);
    ComputeEfficiency("_K2pi_UTMC_MUV3",    kL0MUV3);
    ComputeEfficiency("_K2pi_UTMC_NewCHOD", kL0NewCHOD);
    ComputeEfficiency("_K2pi_UTMC_Calo",    kL0Calo);
    FillEfficiencies("EUTMC", kL0NewCHOD, UTMCbit);
  }

  if(Tracks[0].GetMomentum()<15000 || Tracks[0].GetMomentum()>35000) return;
  if(!Tracks[0].NewCHODAssociationExists()) return ;

  for(Int_t j=0; j < Tracks[0].GetNNewCHODAssociationRecords();++j){
    TRecoNewCHODHit* hit = Tracks[0].GetNewCHODCandidate(j);
    int Tile = hit->GetTileID() ;
    for (Int_t i=0; i<100; i++) {
      if (fGeo->GetScintMap(i)==Tile%100) {
        Int_t BrickID = 100*(Tile/100)+i;
        Double_t x = fGeo->GetBrickCentreX(BrickID) ;
        Double_t y = fGeo->GetBrickCentreY(BrickID) ;
        FillHisto("SpasimirPlot_pip", x/1000., y/1000.) ;
      }
    }
  }
}

void L0TriggerResponse::Kmu2Selection() {

  Bool_t km2_selected = *(Bool_t*)GetOutput("Kmu2Selection.EventSelected");
  if (!km2_selected) return;
  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  if(!OneHitInTime(Tracks[0])) return;
  fTrackMom  = Tracks[0].GetMomentum();
  fZposition = Tracks[0].GetNominalBeamAxisVertex().Z();

  ComputeEfficiency("_Kmu2_M1_RICH",    kL0RICH);
  ComputeEfficiency("_Kmu2_M1_LAV12",   kL0LAV);
  ComputeEfficiency("_Kmu2_M1_MUV3",    kL0MUV3);
  ComputeEfficiency("_Kmu2_M1_NewCHOD", kL0NewCHOD);
  ComputeEfficiency("_Kmu2_M1_Calo",    kL0Calo);
  FillHisto("LKrEnergy_Kmu2", fLKrEnergy/1000.) ;
  RichMultPlot("Kmu2_M1_RICH", kL0RICH);
  FillEfficiencies("M1",  kL0MUV3, 11);
  FillEfficiencies("MQ1", kL0NewCHOD, 14);

  if(fTrackMom>15000){
    FillEfficiencies("MR2", kL0RICH,14);
  }

  if(fUTMCHits<5){
    ComputeEfficiency("_Kmu2_UTMC_RICH",    kL0RICH);
    ComputeEfficiency("_Kmu2_UTMC_LAV12",   kL0LAV);
    ComputeEfficiency("_Kmu2_UTMC_MUV3",    kL0MUV3);
    ComputeEfficiency("_Kmu2_UTMC_NewCHOD", kL0NewCHOD);
    ComputeEfficiency("_Kmu2_UTMC_Calo",    kL0Calo);
    FillEfficiencies("MUTMC", kL0NewCHOD, UTMCbit);
  }

  ////////////////////////////////////////////
  /////  Start selection for M2 and MO2
  ////////////////////////////////////////////

  std::vector<SpectrometerMUV3AssociationOutput> SpecMUV3 =
    *(std::vector<SpectrometerMUV3AssociationOutput>*)GetOutput("SpectrometerMUV3Association.Output");

  // Dimuon selection for trigger efficiency
  ComputeDimuonEfficiency(SpecMUV3[0], "_Kmu2_M2_RICH",    kL0RICH);
  ComputeDimuonEfficiency(SpecMUV3[0], "_Kmu2_M2_LAV12",   kL0LAV);
  ComputeDimuonEfficiency(SpecMUV3[0], "_Kmu2_M2_MUV3",    kL0MUV3);
  ComputeDimuonEfficiency(SpecMUV3[0], "_Kmu2_M2_NewCHOD", kL0NewCHOD);
  ComputeDimuonEfficiency(SpecMUV3[0], "_Kmu2_M2_Calo",    kL0Calo);

  // Outer dimuon selection for trigger efficiency
  SpectrometerMUV3AssociationOutput SpecMUV3Outer;
  for (Int_t i=0; i<SpecMUV3[0].GetNAssociationRecords(); i++) {
    if (SpecMUV3[0].GetAssociationRecord(i)->IsOuter()) {
      SpecMUV3Outer.AddAssociationRecord(*(SpecMUV3[0].GetAssociationRecord(i))); // add outer records
    }
  }

  if (SpecMUV3Outer.GetNAssociationRecords() == 0) return;

  ComputeEfficiency("_Kmu2_MO1_RICH",    kL0RICH);
  ComputeEfficiency("_Kmu2_MO1_LAV12",   kL0LAV);
  ComputeEfficiency("_Kmu2_MO1_MUV3",    kL0MUV3);
  ComputeEfficiency("_Kmu2_MO1_NewCHOD", kL0NewCHOD);
  ComputeEfficiency("_Kmu2_MO1_Calo",    kL0Calo);
  FillEfficiencies("MO1", kL0MUV3, 10);

  ComputeDimuonEfficiency(SpecMUV3Outer, "_Kmu2_MO2_RICH",    kL0RICH);
  ComputeDimuonEfficiency(SpecMUV3Outer, "_Kmu2_MO2_LAV12",   kL0LAV);
  ComputeDimuonEfficiency(SpecMUV3Outer, "_Kmu2_MO2_MUV3",    kL0MUV3);
  ComputeDimuonEfficiency(SpecMUV3Outer, "_Kmu2_MO2_NewCHOD", kL0NewCHOD);
  ComputeDimuonEfficiency(SpecMUV3Outer, "_Kmu2_MO2_Calo",    kL0Calo);
}

void L0TriggerResponse::EndOfJobUser() {

  if (fReadingData) { // Data mode: save output
    SaveAllPlots();
    return;
  }
  if (fhRunNumber) { // Histo mode stage 1

    cout << user_normal() << "Histo mode stage 1" << endl;

    // take run number from the histogram.
    fRunID = fhRunNumber->GetMean();

    // Compute trigger efficiencies
    for (UInt_t i=0; i < events.size() ; i++) {
      for (UInt_t j=0; j < primitives.size(); j++) {
        Int_t index = j*6;
        index += (i*(primitives.size()*6));
        fEfficiency.push_back(new TEfficiency(*h_temp[index], *h_temp[index+1]));
      }
    }

    BuildPDFReport();
    return ;
  }
  if(!fhRunNumber && h_EpR[0]){ // Histo mode stage 2
    BuildPDFReport2();
    return ;
  }

  std::cout << user_normal() << "No running criteria were satisfied. No action taken." << std::endl;
  return ;
}

void L0TriggerResponse::BuildPDFReport2() {

  gStyle->SetTextFont(2);
  gStyle->SetTextSize(0.02);
  gStyle->SetPalette(kBird);
  gStyle->SetNumberContours(255);
  gStyle->SetTitleOffset(1.4,"Y");

  TString OutputPDFFileName = fAnalyzerName + "_Stage2.pdf";
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
  gStyle->SetOptStat(0);

  TCanvas *Canvas = new TCanvas("L0TriggerResponse");
  Canvas->Print(Form(OutputPDFFileName + "["), "pdf"); // open file

  std::vector<TEfficiency*> eff;
  std::vector<TGraphAsymmErrors*> gEff;
  eff.reserve(20);
  for(unsigned int i=0; i<h_EpR.size(); ++i){
    Canvas->Clear();
    TH1F* num = (TH1F*)h_EpR[i];
    TH1F* den = (TH1F*)h_EpR[i+1];
    eff.push_back( new TEfficiency(*num, *den) );

    int index = i/2;
    eff[index]->Paint("P");
    gEff.push_back(new TGraphAsymmErrors());
    gEff[index] = eff[index]->GetPaintedGraph();
    gEff[index]->SetName("gEff");
    gEff[index]->SetTitle(num->GetTitle());
    gEff[index]->SetMarkerColor(kBlue);
    gEff[index]->SetMarkerSize(0.5);
    gEff[index]->SetMarkerStyle(21);

    TString sel = num->GetName();
    sel = sel(0,1) ;
    if( sel.Contains("M") ) sel = "Kmu2";
    if( sel.Contains("Q") || sel.Contains("U") ) sel = "K3pi";
    if( sel.Contains("E") ) sel = "K2pi";
    if( sel.Contains("T") ) sel = "1Track";

    TString title = num->GetName();
    title = title.ReplaceAll("_NumPerRun", Form(" efficiency per run (%s events)",sel.Data()));
    title = title.ReplaceAll("MQ1", "Q1");
    title = title.ReplaceAll("ER2", "R2");
    title = title.ReplaceAll("MR2", "R2");
    title = title.ReplaceAll("ELAV12", "LAV12");
    title = title.ReplaceAll("EUTMC", "UTMC");
    title = title.ReplaceAll("MUTMC", "UTMC");
    title = title.ReplaceAll("TCHOD", "CHOD");
    num->SetTitle(title);

    int b = num->FindFirstBinAbove(0);
    int c = num->FindLastBinAbove(0);

    num->Reset(); // really ...

    Canvas->Clear();
    num->Draw();
    num->GetXaxis()->SetRangeUser(b-1,c-1);
    num->GetYaxis()->SetRangeUser(0,1.1);
    gEff[index]->Draw("P");

    Canvas->Update();
    Canvas->Print(OutputPDFFileName, "pdf");

    i++;
  }
  Canvas->Print(Form(OutputPDFFileName + "]"), "pdf"); // open file

  for(unsigned int i=0; i<eff.size(); ++i){
    delete eff[i];
  }
  eff.clear();
  gEff.clear();
}

void L0TriggerResponse::BuildPDFReport() {

  gStyle->SetTextFont(2);
  gStyle->SetTextSize(0.02);
  gStyle->SetPalette(kBird);
  gStyle->SetNumberContours(255);
  gStyle->SetPaintTextFormat(".0f");
  gStyle->SetTitleOffset(1.4,"Y");

  TString OutputPDFFileName = fAnalyzerName + ".pdf";
  gErrorIgnoreLevel = 5000; // suppress messages generated for each page printed
  gStyle->SetOptStat(0);

  //////////////////////////////////////////////////
  /// Title Page
  //////////////////////////////////////////////////

  TCanvas *Canvas = new TCanvas("L0TriggerResponse");
  Canvas->Print(Form(OutputPDFFileName + "["), "pdf"); // open file

  TText* title = new TText(0.1, 0.5, "L0 Trigger Response");
  title->SetTextSize(0.1) ;
  title->SetNDC();
  title->Draw() ;
  Canvas->Print(Form(OutputPDFFileName), "pdf");

  //////////////////////////////////////////////////
  /// Contents Page
  //////////////////////////////////////////////////

  Canvas->Clear() ;

  std::vector<TString> contents ;
  std::vector<TText*> texts ;

  contents.push_back(TString("DataType, Trigger Type, Trigger Flags, Trigger Flags sent by L0TP")) ;
  contents.push_back(TString("Correlation of control triggers and argonian count"));
  contents.push_back(TString("As a function of burst number: N L0TP special triggers, N Triggers Generated, N Triggers Sent, Downscaling"));
  contents.push_back(TString("N triggers per argonion count per burst, and 2D version, for each trigger mask"));
  contents.push_back(TString("Mask Correlations, Mask correlations with downscalings")) ;
  contents.push_back(TString("RICH primitive correlation plots for Mask1, Remaining Physics Masks, Control Triggers"));
  contents.push_back(TString("One-dimension primitive correlation plots for Mask1, Remaining Physics Masks, Control Triggers"));
  contents.push_back(TString("        ... for RICH, LAV12, MUV3, NewCHOD, L0Calo"));
  contents.push_back(TString("Two-dimension primitive correlation plots for Mask1, Remaining Physics Masks, Control Triggers"));
  contents.push_back(TString("        ... for RICH, LAV12, MUV3, NewCHOD, L0Calo"));
  contents.push_back(TString("1D correlation for LAV12, MUV3, L0Calo, in events with trigger overwriting"));
  contents.push_back(TString("2D correlation for LAV12, MUV3, L0Calo, in events with trigger overwriting"));
  contents.push_back(TString("RICH Hit Multiplicity (Kmu2, K3pi events) and online/offline multiplicity comparison"));
  contents.push_back(TString("NewCHOD Hit multiplicity in Control and Mask1 events (b) Mapping check for NewCHOD quadrants"));
  contents.push_back(TString("Spasimir regions (inside, outside) and K2pi events passing \"pnn-like\" selection"));
  contents.push_back(TString("(a) LKr Energy in: Masks 1 and 4, Control, Kmu2 and K2pi events (b) LKr online/offline energy"));
  contents.push_back(TString("MUV3 channel counts and number of tight and loose hits in the PP firmware"));
  contents.push_back(TString("(a) MUV3 missing hits in the PP firmware (b) MUV3 Errors in the PP firmware"));
  contents.push_back(TString("(a) MUV3 Tight hits in the SL firmware (b) MUV3 Errors in the SL firmware"));
  contents.push_back(TString("NewCHOD channel counts and number of tight and loose hits in the PP firmware"));
  contents.push_back(TString("(a) NewCHOD missing hits in the PP firmware (b) NewCHOD Errors in the PP firmware"));
  contents.push_back(TString("(a) NewCHOD Tight hits in the SL firmware (b) NewCHOD Errors in the SL firmware"));
  contents.push_back(TString("Hit times in Mask1, other physics triggers, and control trigger events"));
  contents.push_back(TString("       ... for RICH, LAV12, MUV3, NewCHOD, Calo, CHOD"));
  contents.push_back(TString("Hit time correlationss in control trigger events, w.r.t RICH and CHOD"));
  contents.push_back(TString("Missing mass plot (MM2) and MM2 v.s. track momentum, for each trigger mask"));
  contents.push_back(TString("Trigger efficiencies for Q1, Q2, QX, M1, MO1, M2, MO2, L0Calo"));
  contents.push_back(TString("       ... as a function of burst number, track momentum, vertex Z position"));
  contents.push_back(TString("Trigger Response of: RICH, LAV12, MUV3, NewCHOD, L0Calo..." ));
  contents.push_back(TString("       ... on K3pi events with: Q1, Q2, QX, QX+3hits selections" ));
  contents.push_back(TString("       ... on Kmu2 events with: M1, M2, MO2 selections" ));
  contents.push_back(TString("       ... on K2pi events with simple and E20 selections" ));
  contents.push_back(TString("       ... on each trigger mask from Mask0 to Mask6" ));

  double start= 0.000;
  double end = 0.970 ;
  for(unsigned int i = 0 ; i < contents.size(); ++i){
    double step = (end-start)/double(contents.size());
    texts.push_back( new TText(0.1, end - (i*step), Form("%i : %s", i, contents[i].Data()) ) );
    texts[texts.size()-1]->SetNDC();
    texts[texts.size()-1]->Draw() ;
  }
  Canvas->Print(Form(OutputPDFFileName), "pdf");

  gStyle->SetTextSize(0.03);

  for(unsigned int i = 0 ; i < texts.size(); ++i){
    delete (texts[i]) ;
  }

  //////////////////////////////////////////////////
  /// Plots regarding the basic trigger information
  //////////////////////////////////////////////////
  Canvas->Clear() ;
  Canvas->Divide(2,2);
  for(int i = 0 ; i < 4 ; ++i){
    Canvas->cd(i+1);
    ((TH1F*)h_info[i])->SetFillColor(kGray);
    ((TH1F*)h_info[i])->SetFillStyle(1001);
    ((TH1F*)h_info[i])->Draw() ;
  }

  Canvas->Print(Form(OutputPDFFileName), "pdf");

  //////////////////////////////////////////////////
  /// Argonion and control event counts
  //////////////////////////////////////////////////

  Canvas->Clear() ;
  Canvas->Divide(2,2);

  Canvas->cd(1);
  h_eobdata[0]->GetSumw2()->Set(0);
  h_eobdata[0]->SetMarkerStyle(7);
  h_eobdata[0]->Draw("P") ;
  int low  = h_eobdata[0]->FindFirstBinAbove(0) ;
  int high = h_eobdata[0]->FindLastBinAbove(0) ;
  int min  = 999999;
  for(int i = 0 ; i< h_eobdata[0]->GetNbinsX(); i++){
    double cont = h_eobdata[0]->GetBinContent(i+1) ;
    if( cont < min && cont > 0) min = cont;
  }
  int max = h_eobdata[0]->GetBinContent( h_eobdata[0]->GetMaximumBin());
  h_eobdata[0]->GetXaxis()->SetRangeUser(low-1,high-1);

  Canvas->cd(3);
  h_info[27]->GetSumw2()->Set(0);
  h_info[27]->SetMarkerStyle(7);
  h_info[27]->Draw("P");
  h_info[27]->GetXaxis()->SetRangeUser(low-1,high-1);

  Canvas->cd(2);
  double nContLow = 99999999;
  for(int i =0 ; i < h_info[27]->GetNbinsX(); ++i){
    double cont = h_info[27]->GetBinContent(i+1);
    if(cont < nContLow && cont > 0) nContLow = cont ;
  }
  double nContHigh = h_info[27]->GetMaximum();
  h_info[25]->GetXaxis()->SetRangeUser( int(min)-1, int(max)+1);
  nContLow = (int(nContLow/10000)-1)*10000.;
  if(nContLow<0) nContLow=0;
  nContHigh = (int(nContHigh/10000)+1)*10000.;
  h_info[25]->GetYaxis()->SetRangeUser(nContLow, nContHigh);
  h_info[25]->Draw("colz");

  Canvas->cd(4);
  h_info[26]->GetSumw2()->Set(0);
  h_info[26]->SetMarkerStyle(7);
  h_info[26]->Draw("P");
  h_info[26]->GetXaxis()->SetRangeUser(low-1,high-1);

  Canvas->Print(Form(OutputPDFFileName), "pdf");

  //////////////////////////////////////////////////
  /// EOB data of Beam and L0TP trigger counts
  //////////////////////////////////////////////////

  Canvas->Clear();
  Canvas->Divide(2,2);
  Canvas->cd(1);


  if(h_nprimitives[0]){ // protecting this because it's new
    h_nprimitives[0]->Draw("AXIS");
    int nplow  = h_nprimitives[0]->FindFirstBinAbove(0);
    int nphigh = h_nprimitives[0]->FindLastBinAbove(0);
    h_nprimitives[0]->GetXaxis()->SetRangeUser(nplow-1,nphigh-1);
    std::vector<TLegend*> nplegs(h_nprimitives.size(), NULL);
    for(unsigned int j=0; j<h_nprimitives.size();++j){
      h_nprimitives[j]->SetMarkerStyle(24);
      h_nprimitives[j]->SetMarkerSize(0.5);
      if(j != 4) h_nprimitives[j]->SetMarkerColor(j+1);
      else h_nprimitives[j]->SetMarkerColor(kOrange);
      h_nprimitives[j]->GetSumw2()->Set(0);
      h_nprimitives[j]->Draw("Psame");

      Double_t n  = h_nprimitives.size();
      Double_t x1 = 0.15 + ((0.85-0.15)/n)*j ;
      Double_t x2 = 0.15 + ((0.85-0.15)/n)*(j+1) ;
      nplegs[j] = new TLegend(x1,0.75,x2,0.9);
      TString name( primitives[j](1,primitives[j].Length()));
      nplegs[j]->AddEntry(h_nprimitives[j], name, "p" );
      nplegs[j]->Draw();
    }
    h_nprimitives[0]->GetYaxis()->SetRangeUser(1E6-1, 50E6);
  }

  for(int j = 0 ; j < 3 ; ++j){
    Canvas->cd(j+2) ;
    Canvas->GetPad(j+2)->SetLogy(1);
    Double_t most = -999 ;
    for(int i = 0 ; i < fNMasks ; i++){
      int index = 1 + fNMasks*j + i ;
      most = std::max(most, h_eobdata[index]->GetBinContent(h_eobdata[index]->GetMaximumBin()) ) ;
      h_eobdata[index]->SetMarkerStyle(7);
      if(i>8) h_eobdata[index]->SetMarkerStyle(31);
      h_eobdata[index]->GetSumw2()->Set(0);
      int index2 = (i+1)%8;
      if(index2 != 5) h_eobdata[index]->SetMarkerColor((i%8)+1);
      else h_eobdata[index]->SetMarkerColor(kOrange);
      if(i==0) h_eobdata[index]->Draw("P") ;
      else h_eobdata[index]->Draw("Psame") ;
    }
    h_eobdata[1+j*fNMasks]->GetXaxis()->SetRangeUser(low-1,high-1) ;
    if(j+2==4){
      h_eobdata[1+j*fNMasks]->GetYaxis()->SetRangeUser(0.1, pow(10,ceil(log10(most))+1)+1 ) ;
    }
    else{
      if(most>99) h_eobdata[1+j*fNMasks]->GetYaxis()->SetRangeUser( 999, pow(10,ceil(log10(most))+1)+1 ) ;
    }
  }

  std::vector<TLegend*> legs(8,0) ;
  for(int i = 0 ; i < 8 ; i++){
    double x1 = 0.15 + ((0.85-0.15)/8.)*i ;
    double x2 = 0.15 + ((0.85-0.15)/8.)*(i+1) ;
    legs[i] = new TLegend(x1,0.75,x2,0.9);
    legs[i]->AddEntry(h_eobdata[i*2+1],Form("M%i",i*2),"p");
    legs[i]->AddEntry(h_eobdata[i*2+1+1],Form("M%i",i*2+1),"p");
  }
  for(int i = 2 ; i < 5 ; ++i){
    Canvas->cd(i) ;
    for( int j = 0 ; j < 8 ; ++j) legs[j]->Draw();
  }

  Canvas->Print(Form(OutputPDFFileName), "pdf");
  for(unsigned int i = 0 ; i < legs.size(); ++i) delete (legs[i]) ;
  legs.clear() ;

  //////////////////////////////////////////////////
  /// Check presence of trigger primitives
  //////////////////////////////////////////////////
  Canvas->Clear();
  Canvas->Divide(2,1);
  Canvas->cd(1);
  h_info[35]->Draw();
  h_info[35]->GetYaxis()->SetRangeUser(1, std::max(2.0,1.1*h_info[35]->GetMaximum()));
  if(h_info[35]->GetEntries()) Canvas->GetPad(1)->SetLogy(1);
  TText* HasPrimMean = new TText(0.25,0.85,Form("Frac: %.2f %%",(100.0-h_info[35]->GetMean()*100.0)));
  HasPrimMean->SetNDC();
  HasPrimMean->Draw("same");
  Canvas->cd(2);
  h_info[36]->Draw();
  h_info[36]->GetYaxis()->SetRangeUser(1, std::max(2.0,1.1*h_info[36]->GetMaximum()));
  if(h_info[36]->GetEntries()) Canvas->GetPad(2)->SetLogy(1);
  TText* HasPrimMean2 = new TText(0.25,0.85,Form("Frac: %.2f %%",(100.0-h_info[36]->GetMean()*100.0)));
  HasPrimMean2->SetNDC();
  HasPrimMean2->Draw("same");
  Canvas->Print(Form(OutputPDFFileName), "pdf");

  //////////////////////////////////////////////////
  /// Mask triggers per argonian count
  //////////////////////////////////////////////////

  Canvas->Clear();
  Canvas->SetLogy(0);

  Int_t nLoop=38;
  Int_t contPlots = nLoop + 2*fNMasks;

  h_info[contPlots]->SetTitle("nEventsControlTrigger");
  h_info[contPlots]->GetSumw2()->Set(0);
  h_info[contPlots+1]->SetTitle("nEventsVsArgonionControlTrigger");

  for(int i=0; i<(fNMasks*2) ;++i){

    if(i%4==0){
      Canvas->Clear();
      Canvas->Divide(2,2);
    }
    Canvas->cd((i%4)+1);

    if(i%2==0){
      h_info[nLoop+i]->SetMarkerStyle(7);
      h_info[nLoop+i]->SetMarkerColor(1);
      h_info[nLoop+i]->GetSumw2()->Set(0);
      h_info[nLoop+i]->Draw("P");
      h_info[nLoop+i]->GetXaxis()->SetRangeUser(low-1,high-1);
    }
    else{
      h_info[nLoop+i]->Draw("colz");
      h_info[nLoop+i]->GetXaxis()->SetRangeUser( int(min)-1, int(max)+1);
      double ymax = h_info[nLoop+i-1]->GetMaximum() * (int(max)+1);
      ymax = (int(ymax/1000)+1)*1000.;
      h_info[nLoop+i]->GetYaxis()->SetRangeUser(0.0, ymax);
    }

    if(i%4==3) Canvas->Print(Form(OutputPDFFileName), "pdf");
  }

  //////////////////////////////////////////////////
  /// Mask correlation plots
  //////////////////////////////////////////////////

  Canvas->Clear();
  Canvas->Divide(2,1) ;

  Canvas->cd(1);
  TH2F* MaskCorrelation = static_cast<TH2F*>(h_info[21]);
  MaskCorrelation->SetMarkerSize(1);
  MaskCorrelation->SetMarkerStyle(20);
  MaskCorrelation->Draw("text") ;
  int m1 = MaskCorrelation->FindFirstBinAbove(0);
  int m2 = MaskCorrelation->FindLastBinAbove(0);

  MaskCorrelation->GetXaxis()->SetRangeUser(m1-1,m2-1);
  MaskCorrelation->GetYaxis()->SetRangeUser(m1-1,m2-1);

  Canvas->cd(2) ;
  TH2F* MaskCorrelationDS = static_cast<TH2F*>(h_info[22]) ;
  MaskCorrelationDS->SetMarkerSize(1);
  MaskCorrelationDS->SetMarkerStyle(20);
  MaskCorrelationDS->Draw("text") ;
  MaskCorrelationDS->GetXaxis()->SetRangeUser(m1-1,m2-1);
  MaskCorrelationDS->GetYaxis()->SetRangeUser(m1-1,m2-1);

  Canvas->Print(Form(OutputPDFFileName), "pdf");

  //////////////////////////////////////////////////
  /// 1D primitive time correlation plots
  //////////////////////////////////////////////////

  Canvas->Clear() ;
  Canvas->SetLogy(0) ;

  for(unsigned int i = 0 ; i < h_1Dcorr.size()/4 ; ++i){
    Canvas->Clear();
    Canvas->Divide(3,1) ;
    for(int j = 0 ; j < 4 ; ++j){
      int index = (i*4)+j ;
      ((TH1F*)h_1Dcorr[index])->SetFillColor(kGray);
      ((TH1F*)h_1Dcorr[index])->SetFillStyle(1001);
      if(j==3) continue ;
      Canvas->cd(j+1) ;
      if(i==0) ((TH1F*)h_1Dcorr[index])->Draw() ;
      else ((TH1F*)h_1Dcorr[index])->Draw("same") ;
    }
    Canvas->Print(Form(OutputPDFFileName), "pdf");
  }

  //////////////////////////////////////////////////
  /// Reduce resolution of 2D correlation plots
  //////////////////////////////////////////////////

  std::vector<TH2F*> h_2Dnew(h_2Dcorr.size(),NULL) ;
  for(unsigned int n = 0 ; n < h_2Dcorr.size() ; ++n){

    if(fPlotResolution==1){
      h_2Dnew[n] = new TH2F(h_2Dcorr[n]->GetName(), h_2Dcorr[n]->GetTitle(), 128,-0.5,255.5,256+128-1,-255.5,511.5);
    }
    else if(fPlotResolution==0) {
      h_2Dnew[n] = new TH2F(h_2Dcorr[n]->GetName(), h_2Dcorr[n]->GetTitle(), 64,-0.5,255.5,128+64-1,-255.5,511.5);
    }
    else h_2Dnew[n] = static_cast<TH2F*>(h_2Dcorr[n]); // default is high resolution plots

    TAxis *xaxis = h_2Dcorr[n]->GetXaxis();
    TAxis *yaxis = h_2Dcorr[n]->GetYaxis();
    for (int j=1; j<=yaxis->GetNbins();j++) {
      for (int i=1; i<=xaxis->GetNbins();i++) {
        h_2Dnew[n]->Fill(xaxis->GetBinCenter(i),yaxis->GetBinCenter(j),h_2Dcorr[n]->GetBinContent(i,j));
      }
    }
  }

  //////////////////////////////////////////////////
  /// 2D primitive time correlation plots
  //////////////////////////////////////////////////

  for(unsigned int i = 0 ; i < h_2Dnew.size()/4 ; ++i){
    Canvas->Clear();
    Canvas->Divide(3,1) ;
    for(int j = 0 ; j < 4 ; ++j){
      if(j==3) continue ;
      (Canvas->GetPad(j+1))->SetLogz() ;
      Canvas->cd(j+1) ;
      int index = (i*4)+j ;
      h_2Dnew[index]->Draw("colz");
    }
    Canvas->Print(Form(OutputPDFFileName), "pdf");
  }

  //////////////////////////////////////////////////
  /// 1D correlation plots with trigger confusion
  //////////////////////////////////////////////////

  Canvas->SetLogz(0) ;
  Canvas->Clear();

  Canvas->Divide(3,1) ;
  Canvas->cd(1) ;
  h_1Dcorr[3+4]->Draw() ;
  Canvas->cd(2) ;
  h_1Dcorr[3+4+4]->Draw() ;
  Canvas->cd(3) ;
  h_1Dcorr[3+4+4+4+4]->Draw() ;
  Canvas->Print(Form(OutputPDFFileName), "pdf");

  //////////////////////////////////////////////////
  /// 2D correlation plots with trigger confusion
  //////////////////////////////////////////////////

  Canvas->Clear();
  Canvas->Divide(3,1) ;
  Canvas->cd(1) ;
  h_2Dnew[3+4]->Draw("colz") ;
  Canvas->cd(2) ;
  h_2Dnew[3+4+4]->Draw("colz") ;
  Canvas->cd(3) ;
  h_2Dnew[3+4+4+4+4]->Draw("colz") ;
  Canvas->Print(Form(OutputPDFFileName), "pdf");

  //////////////////////////////////////////////////
  /// Plots of event FT and TS bits
  //////////////////////////////////////////////////

  Canvas->Clear();
  Canvas->Divide(2);
  Canvas->cd(1);
  h_primbits[2]->Draw();
  Canvas->cd(2);
  h_primbits[13]->Draw();
  Canvas->Print(Form(OutputPDFFileName), "pdf");

  Canvas->Clear();
  Canvas->Divide(2);
  Canvas->cd(1);
  h_primbits[0]->Draw();
  Canvas->cd(2);
  h_primbits[1]->Draw();
  Canvas->Print(Form(OutputPDFFileName), "pdf");

  //////////////////////////////////////////////////
  /// Plots of primitive FT bits
  //////////////////////////////////////////////////

  for(unsigned int i=8; i<13;++i){
    Canvas->Clear();
    h_primbits[i]->Draw(); /// ALL,CHOD
    Canvas->Print(Form(OutputPDFFileName), "pdf");
  }

  //////////////////////////////////////////////////
  /// RICH Multiplicity plots -> km2 and k3pi
  //////////////////////////////////////////////////

  Canvas->Clear();
  Canvas->Divide(3,1) ;
  Canvas->cd(1) ;
  h_RICH[0]->SetFillColor(kGray) ;
  h_RICH[0]->SetFillStyle(1001) ;
  h_RICH[0]->Draw() ;
  h_RICH[0]->GetXaxis()->SetRangeUser(0,50) ;
  h_RICH[2]->SetLineColor(kBlack) ;
  h_RICH[2]->SetLineWidth(2) ;
  h_RICH[2]->Draw("same") ;
  Canvas->cd(2) ;
  h_RICH[1]->SetFillColor(kGray) ;
  h_RICH[1]->SetFillStyle(1001) ;
  h_RICH[1]->Draw() ;
  h_RICH[1]->GetXaxis()->SetRangeUser(0,50) ;
  h_RICH[3]->SetLineColor(kBlack) ;
  h_RICH[3]->SetLineWidth(2) ;
  h_RICH[3]->Draw("same") ;

  Canvas->cd(3) ;
  if(static_cast<TH2F*>(h_RICH[4])->GetEntries()>0) Canvas->GetPad(3)->SetLogz() ;
  static_cast<TH2F*>(h_RICH[4])->Draw("colz") ;
  static_cast<TH2F*>(h_RICH[4])->GetXaxis()->SetRangeUser(0,100);
  static_cast<TH2F*>(h_RICH[4])->GetYaxis()->SetRangeUser(0,100);
  Canvas->Print(Form(OutputPDFFileName), "pdf");

  //////////////////////////////////////////////////
  /// Performance of UTMC requirement
  //////////////////////////////////////////////////

  Canvas->Clear();
  Canvas->SetLogz(0);
  Canvas->Divide(2,1);
  Canvas->cd(1) ;

  TH1F* h_utmc_1 = static_cast<TH1F*>(h_info[4]);
  TH1F* h_utmc_c = static_cast<TH1F*>(h_info[5]);
  TH1F* h_utmc_m = static_cast<TH1F*>(h_info[37]);

  h_utmc_1->SetLineColor(kBlack);
  h_utmc_1->SetLineWidth(2);
  h_utmc_c->SetFillColor(kGray);
  h_utmc_c->SetLineColor(kGray+2);
  h_utmc_c->SetFillStyle(1001);
  h_utmc_m->SetLineColor(kRed);
  h_utmc_m->SetLineWidth(2);

  double scale1 = h_utmc_c->GetBinContent(3)/h_utmc_1->GetBinContent(3) ;
  if(h_utmc_1->GetEntries()>0) h_utmc_1->Scale(scale1) ;
  double scaleM = h_utmc_c->GetBinContent(3)/h_utmc_m->GetBinContent(3) ;
  if(h_utmc_m->GetEntries()>0) h_utmc_m->Scale(scaleM) ;

  //h_utmc_1->GetXaxis()->SetRangeUser(-0.5, 19.5);
  h_utmc_1->Draw() ;
  h_utmc_c->Draw("same");
  h_utmc_1->Draw("same");
  h_utmc_1->Draw("axis same");
  h_utmc_m->Draw("same");

  if(h_utmc_1->GetEntries()>0) Canvas->GetPad(1)->SetLogy() ;

  TLegend *leg1 = new TLegend(0.65,0.65,0.9,0.9);
  leg1->AddEntry(h_utmc_1,"Mask1","lf");
  leg1->AddEntry(h_utmc_c,"Control","lf");
  leg1->AddEntry(h_utmc_m,"PhysM","lf");
  leg1->Draw() ;

  // this is used later
  TLegend *leg = new TLegend(0.60,0.65,0.9,0.9);
  leg->AddEntry(h_utmc_1,"Mask1","lf");
  leg->AddEntry(h_utmc_c,"Control","lf");

  //////////////////////////////////////////////////
  /// NewCHOD Quadrant check
  //////////////////////////////////////////////////

  Canvas->cd(2) ;

  std::vector<TH2F*> NCQ(4,NULL);
  NCQ[0] = static_cast<TH2F*>(h_info[11]);
  NCQ[1] = static_cast<TH2F*>(h_info[12]);
  NCQ[2] = static_cast<TH2F*>(h_info[13]);
  NCQ[3] = static_cast<TH2F*>(h_info[14]);

  int ncqindex = -1 ;
  for(int i = 0 ; i < 16 ; i++){
    for(int j = 0 ; j < 20 ; ++j){

      double lowest = -9999 ;

      for(int k = 0 ; k < 4 ; ++k){
        double temp = NCQ[k]->GetBinContent(i+1,j+1) ;
        if(temp > lowest && temp > 5){
          lowest = temp;
          ncqindex = k ;
        }
      }

      if(ncqindex>=0){
        for(int k = 0 ; k < 4 ; ++k){
          if(k == ncqindex) NCQ[k]->SetBinContent(i+1,j+1, 1) ;
          else NCQ[k]->SetBinContent(i+1,j+1, 0) ;
        }
      }
      else{
        for(int k = 0 ; k < 4 ; ++k) NCQ[k]->SetBinContent(i+1,j+1, 0) ;
      }

    }
  }

  for(int k = 0 ; k < 4 ; ++k){
    NCQ[k]->SetLineColor(2+k) ;
    NCQ[k]->SetFillColor(2+k) ;
    if(k==0) NCQ[k]->Draw("box");
    else NCQ[k]->Draw("box same");
  }
  std::vector<TH1*> PlotterInput;
  NewCHODDataQualityPlotter* fDataQualityPlotter = new NewCHODDataQualityPlotter(PlotterInput, "./TEST.pdf");
  fDataQualityPlotter->DrawBoundaries(3) ;
  NCQ[0]->Draw("AXIS same") ;

  Canvas->Print(Form(OutputPDFFileName), "pdf");

  //////////////////////////////////////////////////
  /// Spasimir Plot
  //////////////////////////////////////////////////

  Canvas->Clear() ;
  Canvas->Divide(2,2) ;
  TH2F* spasI = static_cast<TH2F*>(h_info[17]);
  TH2F* spasO = static_cast<TH2F*>(h_info[18]);
  TH2F* spasD = static_cast<TH2F*>(h_info[23]);

  Canvas->cd(1) ;
  spasI->Draw("colz");
  fDataQualityPlotter->DrawBoundaries(3) ;
  spasI->Draw("AXIS same") ;

  Canvas->cd(2) ;
  spasO->Draw("colz");
  fDataQualityPlotter->DrawBoundaries(3) ;
  spasO->Draw("AXIS same") ;

  Canvas->cd(4) ;
  spasD->Draw("colz");
  fDataQualityPlotter->DrawBoundaries(3) ;
  spasD->Draw("AXIS same");

  TPad* pad3 = (TPad*)Canvas->GetPad(3);
  pad3->SetBBoxCenterX( pad3->UtoAbsPixel(-1.0) ) ;

  TPad* pad4 = (TPad*)Canvas->GetPad(4);
  pad4->SetBBoxCenterX( pad4->UtoAbsPixel(-0.55) ) ;

  Canvas->Print(Form(OutputPDFFileName), "pdf");

  //////////////////////////////////////////////////
  /// MUV3 Quadrant check
  //////////////////////////////////////////////////

  Canvas->Clear();
  Canvas->SetLogy(0);

  for(int x=1; x<13; ++x){
    for(int y=1; y<13; ++y){

      Double_t maxCont=-100.0;
      Int_t maxHist=-1;

      for(int k=0; k<4; ++k){
        Double_t temp = h_muv3quadrants[k]->GetBinContent(x,y);
        if(temp>maxCont && temp>5){
          maxCont = h_muv3quadrants[k]->GetBinContent(x,y);
          maxHist=k;
        }
      }
      for(int k=0; k<4; ++k){
        if(k==maxHist && maxCont>0) h_muv3quadrants[k]->SetBinContent(x,y,1.0);
        else                        h_muv3quadrants[k]->SetBinContent(x,y,0.0);
      }

    }
  }

  for(int k = 0 ; k < 4 ; ++k){
    h_muv3quadrants[k]->SetLineColor(2+k) ;
    h_muv3quadrants[k]->SetFillColor(2+k) ;
    if(k==0) h_muv3quadrants[k]->Draw("box");
    else     h_muv3quadrants[k]->Draw("box same");
  }

  std::vector<TLine*> muv3lines(26, NULL);
  for(int x=0; x<13; ++x){
    muv3lines[x*2] = new TLine(-1.320+0.220*x, -1.320, -1.320+0.220*x, 1.320 );
    muv3lines[x*2]->SetLineWidth(2);
    muv3lines[x*2]->Draw("same");
    muv3lines[x*2+1] = new TLine(-1.320, -1.320+0.220*x, 1.320, -1.320+0.220*x);
    muv3lines[x*2+1]->SetLineWidth(2);
    muv3lines[x*2+1]->Draw("same");
  }
  muv3lines[24] = new TLine(-0.2155, 0.0, 0.2155, 0.0);
  muv3lines[24]->SetLineWidth(3);
  muv3lines[24]->SetLineColor(0);
  muv3lines[24]->Draw("same");
  muv3lines[25] = new TLine(0.0, -0.214, 0.0, 0.214);
  muv3lines[25]->SetLineWidth(3);
  muv3lines[25]->SetLineColor(0);
  muv3lines[25]->Draw("same");

  Canvas->Print(Form(OutputPDFFileName), "pdf");

  for(unsigned int i=0; i<26; ++i) delete (muv3lines[i]);
  muv3lines.clear();

  //////////////////////////////////////////////////
  /// Lkr energy distribution plot
  //////////////////////////////////////////////////

  Canvas->Clear() ;
  Canvas->SetLogy(0) ;
  Canvas->Divide(2,1) ;
  Canvas->cd(1);
  TH1F* h_lkrenergy_1   = static_cast<TH1F*>(h_info[6]);
  TH1F* h_lkrenergy_c   = static_cast<TH1F*>(h_info[7]);
  TH1F* h_lkrenergy_4   = static_cast<TH1F*>(h_info[20]);
  TH1F* h_lkrenergy_2pi = static_cast<TH1F*>(h_info[8]);
  TH1F* h_lkrenergy_mu2 = static_cast<TH1F*>(h_info[9]);
  TH2F* h_lkrenergy_2d  = static_cast<TH2F*>(h_info[10]);

  h_lkrenergy_c->SetFillColor(kGray);
  h_lkrenergy_c->SetLineColor(kGray+2);
  h_lkrenergy_c->SetFillStyle(1001);

  h_lkrenergy_1->SetLineColor(kBlack);
  h_lkrenergy_1->SetLineWidth(2);
  h_lkrenergy_2pi->SetLineColor(kBlack);
  h_lkrenergy_2pi->SetLineWidth(2);
  h_lkrenergy_2pi->SetLineStyle(2);

  h_lkrenergy_mu2->SetLineColor(kGray+4) ;
  h_lkrenergy_mu2->SetFillColor(kGray+2) ;
  h_lkrenergy_mu2->SetFillStyle(1001) ;

  h_lkrenergy_4->SetLineColor(kRed) ;
  h_lkrenergy_4->SetLineWidth(3) ;

  Double_t scale=0.0;
  scale = h_lkrenergy_c->Integral(70,95)/h_lkrenergy_1->Integral(70,95) ;
  if(scale>0) h_lkrenergy_1->Scale(scale) ;

  scale = h_lkrenergy_c->Integral()/h_lkrenergy_4->Integral() ;
  if(scale>0) h_lkrenergy_4->Scale(scale) ;

  scale = h_lkrenergy_c->Integral(70,95)/h_lkrenergy_2pi->Integral(70,95) ;
  if(scale>0) h_lkrenergy_2pi->Scale(scale);

  scale = h_lkrenergy_c->GetBinContent(2)/h_lkrenergy_mu2->GetBinContent(2) ;
  if(scale>0) h_lkrenergy_mu2->Scale(scale);

  TText* LKrText = new TText(0.5,0.5,"No data");
  if(h_lkrenergy_c->Integral()>0){
    h_lkrenergy_c->Draw() ;
    (Canvas->GetPad(1))->SetLogy();

    if(h_lkrenergy_mu2->Integral()>0){
      h_lkrenergy_mu2->Draw("same") ;
      leg->AddEntry(h_lkrenergy_mu2,"K #rightarrow #mu^{+}#nu","lf");
    }

    if(h_lkrenergy_1->Integral()>0){
      h_lkrenergy_1->Draw("same") ;
      leg->AddEntry(h_lkrenergy_1,"Mask 1","lf");
    }

    if(h_lkrenergy_2pi->Integral()>0){
      h_lkrenergy_2pi->Draw("same") ;
      leg->AddEntry(h_lkrenergy_2pi,"K #rightarrow #pi^{+}#pi^{0}","lf");
    }

    if(h_lkrenergy_4->Integral()>0){
      h_lkrenergy_4->Draw("same") ;
      leg->AddEntry(h_lkrenergy_4,"Mask 4","lf");
    }

    h_lkrenergy_c->Draw("axissame") ;
    leg->Draw() ;
  }
  else{
    LKrText->Draw();
  }

  //////////////////////////////////////////////////
  /// 2D Lkr energy in data and primitives
  //////////////////////////////////////////////////

  Canvas->cd(2) ;
  h_lkrenergy_2d->Draw("colz") ;
  if(h_lkrenergy_2d->GetEntries()>0) (Canvas->GetPad(2))->SetLogz() ;
  Canvas->Print(Form(OutputPDFFileName), "pdf");

  delete leg ;
  delete LKrText;

  //////////////////////////////////////////////////
  /// L0Calo plots
  //////////////////////////////////////////////////

  Canvas->Clear();
  Canvas->Divide(2);
  Canvas->cd(1);
  TH1F* l0calo_energy = static_cast<TH1F*>(h_info[28]);
  l0calo_energy->Draw();
  Canvas->cd(2);
  TH1F* l0calo_time = static_cast<TH1F*>(h_info[29]);
  l0calo_time->Draw();
  Canvas->Print(Form(OutputPDFFileName), "pdf");

  Canvas->Clear();
  TH2F* l0calo_position = static_cast<TH2F*>(h_info[30]);
  l0calo_position->Draw("colz");
  Canvas->Print(Form(OutputPDFFileName), "pdf");

  L0CaloResponsePerE(Canvas, static_cast<TH1F*>(h_info[31]), static_cast<TH1F*>(h_info[32]), false);
  L0CaloResponsePerE(Canvas, static_cast<TH1F*>(h_info[33]), static_cast<TH1F*>(h_info[34]), false);

  //////////////////////////////////////////////////
  /// MUV3 PPEOB information
  //////////////////////////////////////////////////

  PlotPrimitiveEOBInfo(Canvas, 0,  h_muv3eob);

  //////////////////////////////////////////////////
  /// NewCHOD PPEOB information
  //////////////////////////////////////////////////

  PlotPrimitiveEOBInfo(Canvas, 1, h_newchodeob);

  //////////////////////////////////////////////////
  /// Detector DigiTimeRawFine
  //////////////////////////////////////////////////

  for(unsigned int i = 0 ; i < primitives.size() ; ++i){
    Canvas->Clear() ;
    Canvas->Divide(2,2) ;

    for(int j = 0 ; j < 4 ; ++j){
      int index = 4*i + j ;
      Canvas->cd(j+1) ;
      Canvas->GetPad(j+1)->SetLogy() ;
      h_DigiRaw[index]->SetFillStyle(1001) ;
      h_DigiRaw[index]->SetFillColor(kGray) ;
      h_DigiRaw[index]->SetLineWidth(2) ;
      h_DigiRaw[index]->SetLineColor(1) ;
      h_DigiRaw[index]->Draw() ;
    }
    Canvas->Print(Form(OutputPDFFileName), "pdf");
  }

  //////////////////////////////////////////////////
  /// Hit Time Correlations
  //////////////////////////////////////////////////

  Canvas->Clear() ;
  gStyle->SetPalette(kBird);
  gStyle->SetNumberContours(255);
  Canvas->Divide(2,2) ;
  for(unsigned int i = 0 ; i < 4 ; ++i){
    Canvas->cd(i+1) ;
    Canvas->GetPad(i+1)->SetLogz() ;
    h_HitCorr2D[i]->Draw("colz") ;
    double maxBC = h_HitCorr2D[i]->GetBinContent( h_HitCorr2D[i]->GetMaximumBin()) ;
    h_HitCorr2D[i]->GetZaxis()->SetRangeUser(  h_HitCorr2D[i]->GetMinimum(),
        maxBC/1.);
  }
  Canvas->Print(Form(OutputPDFFileName), "pdf");

  Canvas->Clear() ;
  Canvas->Divide(2,2) ;
  for(unsigned int i = 0 ; i < 4 ; ++i){
    Canvas->cd(i+1) ;
    TPad* apad = (TPad*)Canvas->GetPad(i+1) ;
    apad->SetLogz() ;
    if(i==2) apad->SetBBoxCenterX( apad->UtoAbsPixel(0.5) ) ;

    if(i==3){
      apad->SetBBoxCenterX(apad->UtoAbsPixel(1.0) ) ;
      Canvas->Print(Form(OutputPDFFileName), "pdf");
      break ;
    }
    h_HitCorr2D[i+4]->Draw("colz") ;
    double maxBC = h_HitCorr2D[i+4]->GetBinContent( h_HitCorr2D[i+4]->GetMaximumBin()) ;
    h_HitCorr2D[i+4]->GetZaxis()->SetRangeUser(  h_HitCorr2D[i+4]->GetMinimum(),
        maxBC/1.);
  }

  //////////////////////////////////////////////////
  /// One-track selection (OTS) plots
  //////////////////////////////////////////////////

  for(int i=0; i<fNMasks; ++i){
    Canvas->Clear();
    Canvas->Divide(2,1);
    Canvas->cd(1);
    h_ots[2*i]->SetFillColor(kBlue);
    h_ots[2*i]->SetFillStyle(1001);
    h_ots[2*i]->SetTitle(Form("Missing Mass Squared, Mask %i",i));
    h_ots[2*i]->GetYaxis()->SetTitleOffset(1.4);
    h_ots[2*i]->Draw();
    Canvas->cd(2);
    h_ots[2*i+1]->SetTitle(Form("Missing Mass Squared vs Track momentum, Mask %i",i));
    h_ots[2*i+1]->GetYaxis()->SetTitleOffset(1.4);
    h_ots[2*i+1]->Draw("colz");
    if( h_ots[2*i+1]->Integral()>0) (Canvas->GetPad(2))->SetLogz();
    Canvas->Print(Form(OutputPDFFileName), "pdf");
  }

  //////////////////////////////////////////////////
  /// Trigger response for each burst, Track mom,
  /// Z position, occupancy
  //////////////////////////////////////////////////

  Canvas->Clear() ;
  Canvas->SetLogz(0);

  for(unsigned int i =0 ; i < h_EpB.size() ; ++i){
    fBadTriggerThreshold = fBadTriggerThresholds[i/2];
    ResponsePerBurst(Canvas, (TH1F*)(h_EpB[i]), (TH1F*)(h_EpB[i+1]));
    i++;
  }

  for(unsigned int i =0 ; i < h_EpP.size() ; ++i){
    ResponsePerPorZ(Canvas, (TH1F*)(h_EpP[i]), (TH1F*)(h_EpP[i+1])) ;
    i++;
  }

  for(unsigned int i =0 ; i < h_EpZ.size() ; ++i){
    ResponsePerPorZ(Canvas, (TH1F*)(h_EpZ[i]), (TH1F*)(h_EpZ[i+1])) ;
    i++;
  }

  for(unsigned int i =0 ; i < h_EpO.size() ; ++i){
    ResponsePerPorZ(Canvas, (TH1F*)(h_EpO[i]), (TH1F*)(h_EpO[i+1])) ;
    i++;
  }

  //////////////////////////////////////////////////
  /// Trigger response vs occupancy, no sample
  //////////////////////////////////////////////////
  Canvas->Clear();
  Canvas->SetLogy(0);
  for(unsigned int i=0;i<h_Occupancy.size();++i){
    // just going to borrow this function...
    TH1F* a = (TH1F*)h_Occupancy[i];
    TH1F* b = (TH1F*)h_Occupancy[i+1];
    L0CaloResponsePerE(Canvas, a, b, true);
    i++;
  }

  //////////////////////////////////////////////////
  /// Fill the bad-burst information
  //////////////////////////////////////////////////
  fTriggerBadBursts.open("L0TriggerResponseBadBursts.dat");

  //////////////////////////////////////////////////
  /// Fill output with thresholds used
  //////////////////////////////////////////////////
  fTriggerBadBursts << "### Thresholds used:" << std::endl;
  for(unsigned int i = 0 ; i < nPerBurst.size() ; ++i){
    fTriggerBadBursts << "### ";
    fTriggerBadBursts.width(7) ;
    fTriggerBadBursts << std::left << nPerBurst[i] ;
    fTriggerBadBursts.width(5) ;
    fTriggerBadBursts << fBadTriggerThresholds[i] << std::endl;
  }
  fTriggerBadBursts << std::endl;

  /////////////////////////////////////////////////////////
  //// Fill bad burst info for each burst
  /////////////////////////////////////////////////////////
  std::map< UInt_t, std::vector<TString> >::iterator bbit;
  for( bbit = BadTriggers.begin(); bbit != BadTriggers.end(); ++bbit){
    fTriggerBadBursts << Form("BadBurst %06d %04d", fRunID, (Int_t) bbit->first);
    fTriggerBadBursts.width(11) ;
    Long64_t bursttime = fhBurstTime->GetBinContent( (bbit->first)+1 ) ;
    fTriggerBadBursts << std::left << bursttime ;
    for(unsigned int i = 0 ; i < (bbit->second).size() ; ++i){
      fTriggerBadBursts.width(7) ;
      fTriggerBadBursts << std::left << (bbit->second)[i] ;
    }
    fTriggerBadBursts << endl;
  }
  fTriggerBadBursts.close();

  /////////////////////////////////////////////////////////
  //// Fill run efficiency plot
  /////////////////////////////////////////////////////////
  for(unsigned int i =0 ; i < h_EpR.size() ; ++i){
    for(int j=0 ; j<h_EpB[i]->GetNbinsX(); ++j){
      double frac = double(h_EpB[i]->GetBinContent(j+1)) ;
      frac /= double(h_EpB[i+1]->GetBinContent(j+1));
      if( frac > fBadTriggerThresholds[i]){
        h_EpR[i]->SetBinContent( fRunID+1, h_EpB[i]->GetBinContent(j+1) );
        h_EpR[i+1]->SetBinContent( fRunID+1, h_EpB[i+1]->GetBinContent(j+1) );
      }
    }
    h_EpR[i]->Write();
    h_EpR[i+1]->Write();
    i++;
  }

  //////////////////////////////////////////////////
  /// Trigger response to selection and masks
  //////////////////////////////////////////////////

  Canvas->Clear() ;
  gStyle->SetPalette(kBird);
  gStyle->SetNumberContours(255);
  Canvas->SetLogy(0);
  Canvas->SetLogz(0);

  std::map <TString, UInt_t> labels;
  labels["_RICH"]    = kL0RICH;
  labels["_LAV12"]   = kL0LAV;
  labels["_MUV3"]    = kL0MUV3;
  labels["_NewCHOD"] = kL0NewCHOD;
  labels["_Calo"]    = kL0Calo;
  labels["_CHOD"]    = kL0CHOD;

  TString plotname = "";
  Int_t eff = 0;
  for (UInt_t i=0; i < events.size(); i++) {
    for (UInt_t j=0; j < primitives.size(); j++) {
      if(j==primitives.size()-1){
        eff++;
        continue ; // do not draw CHOD plots
      }
      TString primname = primitives[j](1, primitives[j].Length());
      TString eventname = events[i](1, events[i].Length());
      plotname = Form("%s trigger probability for %s candidates (%.0f events)",
          primname.Data(), eventname.Data(),
          fEfficiency[eff]->GetTotalHistogram()->GetBinContent(1));
      fEfficiency[eff]->SetTitle(plotname);
      TString histname = Form("%s%s", events[i].Data(), primitives[j].Data());
      PageOne(Canvas, histname, labels[primitives[j]], plotname);
      PageTwo(Canvas, fEfficiency[eff], labels[primitives[j]]);
      eff++;
    }
  }

  Canvas->Print(Form(OutputPDFFileName + "]"), "pdf"); // close file
  gErrorIgnoreLevel = -1; // restore the default
}

void L0TriggerResponse::ResponsePerBurst(TCanvas* Canvas, TH1F* num, TH1F* den){

  TString OutputPDFFileName = fAnalyzerName + ".pdf";

  Canvas->Clear() ;
  Canvas->Divide(2,1);
  Canvas->cd(1);
  TEfficiency* a = new TEfficiency(*(num), *(den));
  a->Paint("P");
  TGraphAsymmErrors* gEff = new TGraphAsymmErrors();
  gEff = a->GetPaintedGraph();
  gEff->SetName("gEff");
  gEff->SetTitle(num->GetTitle());
  gEff->SetMarkerColor(kBlue);
  gEff->SetMarkerSize(0.5);
  gEff->SetMarkerStyle(21);

  TString sel = num->GetName() ;

  ///////////////////////////////////////
  //// Fill bad burst container
  ///////////////////////////////////////
  for(int i = 0 ; i < gEff->GetN(); ++i){
    double x=99;
    double y=99;
    gEff->GetPoint(i, x, y) ;

    std::map< UInt_t, std::vector<TString> >::iterator it;
    if(y<fBadTriggerThreshold){
      int pos = sel.First('_');
      TString var = sel(0,pos);

      it = BadTriggers.find(x);
      if(it != BadTriggers.end()){
        BadTriggers[x].push_back(var);
      }
      else{
        std::vector<TString> temp;
        temp.push_back(var);
        BadTriggers[x] = temp;
      }

    }
  }
  ///////////////////////////////////////

  sel = sel(0,1) ;
  if( sel.Contains("M") ) sel = "Kmu2";
  if( sel.Contains("Q") || sel.Contains("U") ) sel = "K3pi";
  if( sel.Contains("E") ) sel = "K2pi";
  if( sel.Contains("T") ) sel = "1Track";

  TH1F* numClone = (TH1F*)num->Clone("numClone");
  numClone->Reset();
  TString title = num->GetName() ;
  title = title.ReplaceAll("_NumPerBurst", Form(" efficiency per burst (%s events)", sel.Data()) ) ;
  title = title.ReplaceAll("MQ1", "Q1");
  title = title.ReplaceAll("ER2", "R2");
  title = title.ReplaceAll("MR2", "R2");
  title = title.ReplaceAll("ELAV12", "LAV12");
  title = title.ReplaceAll("EUTMC", "UTMC");
  title = title.ReplaceAll("MUTMC", "UTMC");
  title = title.ReplaceAll("TCHOD", "CHOD");
  numClone->SetTitle( title );

  int b = num->FindFirstBinAbove(0);
  int c = num->FindLastBinAbove(0);
  numClone->GetXaxis()->SetNdivisions(110);
  numClone->GetXaxis()->SetRangeUser(b-1,c-1);
  numClone->GetYaxis()->SetRangeUser(0,1.1);

  numClone->Draw();
  gEff->Draw("P");
  Canvas->Update();

  TLine aaa(b-1, fBadTriggerThreshold, c-1, fBadTriggerThreshold);
  aaa.SetLineStyle(7);
  aaa.SetLineColor(kRed);
  aaa.SetLineWidth(2);
  aaa.Draw("same");

  Canvas->cd(2);
  TString name = num->GetName();
  name = name.ReplaceAll("NumPerBurst","BurstEfficiency1D");
  TH1F* hist = (TH1F*) fHisto.GetTH1(name);
  hist->SetTitle(title);
  for(int i=0; i < gEff->GetN(); i++){
    double x=0;
    double y=0;
    gEff->GetPoint(i,x,y);
    hist->Fill(y);
  }
  hist->SetFillColor(4);
  hist->SetFillStyle(1001);
  hist->Draw();

  TLine bbb(fBadTriggerThreshold, 0.0, fBadTriggerThreshold, hist->GetMaximum());
  bbb.SetLineStyle(7);
  bbb.SetLineColor(kRed);
  bbb.SetLineWidth(2);
  bbb.Draw("same");

  Canvas->Print(OutputPDFFileName, "pdf");

  // delete gEff;
  delete a ;
  delete numClone;
  //delete hist ;
}

void L0TriggerResponse::ResponsePerPorZ(TCanvas* Canvas, TH1F* num, TH1F* den){
  TString OutputPDFFileName = fAnalyzerName + ".pdf";

  Canvas->Clear() ;
  TEfficiency* a = new TEfficiency(*(num), *(den));
  a->SetTitle(num->GetTitle());

  a->Paint("P");
  TGraphAsymmErrors* gEff = new TGraphAsymmErrors();
  gEff = a->GetPaintedGraph();
  gEff->SetName("gEff");
  gEff->SetMarkerColor(kBlue);
  gEff->SetMarkerSize(0.5);
  gEff->SetMarkerStyle(21);

  TString name = num->GetName();
  TString sel = name(0,1) ;
  if( sel.Contains("M") ) sel = "Kmu2";
  if( sel.Contains("Q") || sel.Contains("U") ) sel = "K3pi";
  if( sel.Contains("E") ) sel = "K2pi";
  if( sel.Contains("T") ) sel = "1Track";

  TH1F* numClone = (TH1F*)num->Clone("numClone");
  numClone->Reset();
  TString title = num->GetName() ;
  title = title.ReplaceAll("MQ1", "Q1");
  title = title.ReplaceAll("ER2", "R2");
  title = title.ReplaceAll("MR2", "R2");
  title = title.ReplaceAll("ELAV12", "LAV12");
  title = title.ReplaceAll("EUTMC", "UTMC");
  title = title.ReplaceAll("MUTMC", "UTMC");
  title = title.ReplaceAll("TCHOD", "CHOD");
  if(title.Contains("TrackMom")){
    title = title.ReplaceAll("_NumPerTrackMom", Form(" efficiency vs. track momentum (%s events)", sel.Data() ) );
  }
  if(title.Contains("Zposition")){
    title = title.ReplaceAll("_NumPerZposition", Form(" efficiency vs. Z position (%s events)", sel.Data()) ) ;
  }
  if(title.Contains("Occupancy")){
    title = title.ReplaceAll("_NumPerOccupancy", Form(" efficiency vs. occupancy (%s events)", sel.Data()) ) ;
    // Set X axis scale for Occupancy
    Int_t last = den->FindLastBinAbove(0);
    numClone->GetXaxis()->SetRangeUser(-0.5, den->GetBinLowEdge(last)+1);
  }
  numClone->SetTitle( title );
  numClone->GetYaxis()->SetRangeUser(0,1.1);

  numClone->Draw();
  gEff->Draw("P");

  Canvas->Update();
  Canvas->Print(OutputPDFFileName, "pdf");

  // delete gEff;
  delete a ;
  delete numClone;
}

void L0TriggerResponse::L0CaloResponsePerE(TCanvas* Canvas, TH1F* num, TH1F* den, bool rescale){
  TString OutputPDFFileName = fAnalyzerName + ".pdf";

  Canvas->Clear() ;
  TEfficiency* a = new TEfficiency(*(num), *(den));
  a->SetTitle(num->GetTitle());

  a->Paint("P");
  TGraphAsymmErrors* gEff = new TGraphAsymmErrors();
  gEff = a->GetPaintedGraph();
  gEff->SetName("gEff");
  gEff->SetMarkerColor(kBlue);
  gEff->SetMarkerSize(0.5);
  gEff->SetMarkerStyle(21);

  TH1F* numClone = (TH1F*)num->Clone("numClone");
  numClone->Reset();
  numClone->GetYaxis()->SetRangeUser(0,1.1);
  if(rescale){
    Int_t last = den->FindLastBinAbove(0);
    numClone->GetXaxis()->SetRangeUser(-0.5, den->GetBinLowEdge(last)+1);
  }
  numClone->Draw();
  TH1F* temp = NULL;
  if(rescale){
    temp = (TH1F*)den->Clone("temp");
    Double_t max = temp->GetBinContent(temp->GetMaximumBin());
    for(Int_t i=1; i<temp->GetNbinsX();++i){
      Double_t c=temp->GetBinContent(i);
      if(c>0) temp->SetBinContent(i, log10(c));
    }
    if(max>0) temp->Scale(1.0/log10(max));
    temp->SetLineColor(14);
    temp->SetFillColor(16);
    temp->SetFillStyle(1001);
    temp->Draw("histsame");
    numClone->Draw("axissame");
  }
  gEff->Draw("P");

  Canvas->Update();
  Canvas->Print(OutputPDFFileName, "pdf");

  // delete gEff;
  delete a ;
  delete numClone;
  if(temp) delete temp;
}


void L0TriggerResponse::CorrelationPlots(Int_t ref, Int_t t0, Int_t t1, Int_t t2, TString histname ){

  if(t0<-9000) t0 = 9999 ;
  if(t1<-9000) t1 = 9999 ;
  if(t2<-9000) t2 = 9999 ;

  if(t0<9000) FillHisto(histname, t0-ref) ;
  if(t1<9000) FillHisto(histname, t1-ref) ;
  if(t2<9000) FillHisto(histname, t2-ref) ;

  histname = histname.ReplaceAll("1D", "2D");
  if(t0<9000) FillHisto(histname, ref, t0);
  if(t1<9000) FillHisto(histname, ref, t1);
  if(t2<9000) FillHisto(histname, ref, t2);

}

void L0TriggerResponse::GetPrimitiveAndTimes(UInt_t L0Detector, Bool_t mode){

  // Get the primitives in three time slots
  L0Primitive Prim_N0 = GetL0Data()->GetPrimitive(kL0PreviousSlot, L0Detector);
  L0Primitive Prim_N1 = GetL0Data()->GetPrimitive(kL0TriggerSlot,  L0Detector);
  L0Primitive Prim_N2 = GetL0Data()->GetPrimitive(kL0NextSlot,     L0Detector);

  // Get the primitive bits
  UInt_t PrimitiveID_N0 = Prim_N0.GetPrimitiveID();
  UInt_t PrimitiveID_N1 = Prim_N1.GetPrimitiveID();
  UInt_t PrimitiveID_N2 = Prim_N2.GetPrimitiveID();

  Int_t T_N0 = PrimitiveID_N0 == 0?-9999:Prim_N0.GetCorrectedFineTime(kL0PreviousSlot, fRef, fFineTimeBit);
  Int_t T_N1 = PrimitiveID_N1 == 0?-9999:Prim_N1.GetCorrectedFineTime(kL0TriggerSlot,  fRef, fFineTimeBit);
  Int_t T_N2 = PrimitiveID_N2 == 0?-9999:Prim_N2.GetCorrectedFineTime(kL0NextSlot,     fRef, fFineTimeBit);

  Double_t Time_N0 = TdcCalib*T_N0;
  Double_t Time_N1 = TdcCalib*T_N1;
  Double_t Time_N2 = TdcCalib*T_N2;

  Double_t Time_N0_ref = Time_N0-fRefTime;
  Double_t Time_N1_ref = Time_N1-fRefTime;
  Double_t Time_N2_ref = Time_N2-fRefTime;

  ///////////////////////////////
  //// Primitives with 10ns cut
  ///////////////////////////////

  UInt_t PrimitiveID_N0X = 0 ;
  UInt_t PrimitiveID_N1X = 0 ;
  UInt_t PrimitiveID_N2X = 0 ;

  // 10ns cut for physics, 20ns for control
  double timecut = 0.0;
  if(mode) timecut = 10.0 ;
  else     timecut = 20.0 ;

  if (fabs(Time_N0_ref) > timecut) PrimitiveID_N0X = 0;
  else PrimitiveID_N0X = PrimitiveID_N0 ;

  if (fabs(Time_N1_ref) > timecut) PrimitiveID_N1X = 0;
  else PrimitiveID_N1X = PrimitiveID_N1 ;

  if (fabs(Time_N2_ref) > timecut) PrimitiveID_N2X = 0;
  else PrimitiveID_N2X = PrimitiveID_N2 ;

  UInt_t PrimitiveID_N3 = PrimitiveID_N0X | PrimitiveID_N1X | PrimitiveID_N2X;

  PrimAndTimes a ;
  a.PrimitiveID_N0 = PrimitiveID_N0X;
  a.PrimitiveID_N1 = PrimitiveID_N1X;
  a.PrimitiveID_N2 = PrimitiveID_N2X;
  a.PrimitiveID_N3 = PrimitiveID_N3;
  a.Time_N0_ref = Time_N0_ref;
  a.Time_N1_ref = Time_N1_ref;
  a.Time_N2_ref = Time_N2_ref;
  a.T_N0 = T_N0;
  a.T_N1 = T_N1;
  a.T_N2 = T_N2;

  PrimInfo.push_back(a) ;
}

void L0TriggerResponse::PrimCorrelationPlots(TString histname, UInt_t L0Detector){

  PrimAndTimes a = PrimInfo[L0Detector];

  if(histname.Contains("Control")){
    // Plot using control events (Control)
    CorrelationPlots(fRef, a.T_N0, a.T_N1, a.T_N2, histname) ;
    return;
  }

  bool L0Mask1I = (GetL0Data()->GetTriggerFlags() & 0x02) ;
  if(L0Mask1I){
    // Physics plot with at least Mask1 (Mask1I)
    histname = histname.ReplaceAll("PhysM","Phys1I") ;
    CorrelationPlots(fRef, a.T_N0, a.T_N1, a.T_N2, histname) ;

    bool L0Mask1E = (GetL0Data()->GetTriggerFlags() == 0x02) ;
    if(L0Mask1E){
      // Physics plot with Mask1-exclusive events (Mask1E)
      histname = histname.ReplaceAll("Phys1I","Phys1E") ;
      CorrelationPlots(fRef, a.T_N0, a.T_N1, a.T_N2, histname) ;
    }
  }
  else{
    // Physics plot excluding Mask1 events (PhysM)
    // Mask1 almost 100% uncorrelated with other events
    // so there's almost no bias here.
    CorrelationPlots(fRef, a.T_N0, a.T_N1, a.T_N2, histname) ;
  }
}

void L0TriggerResponse::FillLKrEnergy2D(){
  double LKrPrimE = 0 ;
  for(UInt_t iTrigSlot = 0; iTrigSlot < 3; iTrigSlot++){
    if(GetL0Data()->GetPrimitive(iTrigSlot,kL0Calo).GetPrimitiveID() >0){

      if(fRunID<7000){
        LKrPrimE+=56*(GetL0Data()->GetPrimitive(iTrigSlot,kL0Calo).GetPrimitiveID()&0x1fff);
      }
      else{
        LKrPrimE+=448*(GetL0Data()->GetPrimitive(iTrigSlot,kL0Calo).GetPrimitiveID()&0x00ff);
      }
    }
  }
  FillHisto("LKrEnergy2D", fLKrEnergy/1000., LKrPrimE/1000.) ;

  Bool_t B12 = (PrimInfo[kL0Calo].PrimitiveID_N3>>12)&1;
  Bool_t B13 = (PrimInfo[kL0Calo].PrimitiveID_N3>>13)&1;
  if(B12) FillHisto("LKrEnergy2DB12", fLKrEnergy/1000., LKrPrimE/1000.) ;
  if(B13) FillHisto("LKrEnergy2DB13", fLKrEnergy/1000., LKrPrimE/1000.) ;
  if(B13 && !B12) FillHisto("LKrEnergy2DB13Only", fLKrEnergy/1000., LKrPrimE/1000.) ;
}

void L0TriggerResponse::ComputeEfficiency(TString histname, UInt_t L0Detector) {

  PrimAndTimes a = PrimInfo[L0Detector];

  std::vector<bool> Trigs(16,false);
  for (Int_t i=0; i<16; i++) {
    Bool_t Trigger_N3 = ((a.PrimitiveID_N3>>i)&1);
    Trigs[i] = Trigger_N3;
  }

  for (Int_t i=0; i<16; i++) {
    Bool_t Trigger_N0 = ((a.PrimitiveID_N0>>i)&1);
    Bool_t Trigger_N1 = ((a.PrimitiveID_N1>>i)&1);
    Bool_t Trigger_N2 = ((a.PrimitiveID_N2>>i)&1);
    Bool_t Trigger_N3 = Trigs[i];

    if (Trigs[i]) FillHisto(Form("Passed%s", histname.Data()), i);
    FillHisto(Form("Total%s", histname.Data()), i);
    FillHisto(Form("L0Trigger_N0%s", histname.Data()), i, (Double_t)Trigger_N0);
    FillHisto(Form("L0Trigger_N1%s", histname.Data()), i, (Double_t)Trigger_N1);
    FillHisto(Form("L0Trigger_N2%s", histname.Data()), i, (Double_t)Trigger_N2);
    FillHisto(Form("L0Trigger_N3%s", histname.Data()), i, (Double_t)Trigger_N3);
  }
}

void L0TriggerResponse::FillSpasimirPlot(UInt_t L0Detector){

  UInt_t Trigger = PrimInfo[L0Detector].PrimitiveID_N3 ;
  Bool_t HO1 = ((Trigger>>4)&1);

  TRecoNewCHODEvent* event = GetEvent<TRecoNewCHODEvent>() ;
  if(event->GetNHits()==1){
    TRecoNewCHODHit* hit = static_cast<TRecoNewCHODHit*>(event->GetHit(0));
    int Tile = hit->GetTileID() ;
    for (Int_t i=0; i<100; i++) {
      if (fGeo->GetScintMap(i)==Tile%100) {
        Int_t BrickID = 100*(Tile/100)+i;
        Double_t x = fGeo->GetBrickCentreX(BrickID) ;
        Double_t y = fGeo->GetBrickCentreY(BrickID) ;
        FillHisto("SpasimirPlotA", x/1000., y/1000.) ;
        if(HO1) FillHisto("SpasimirPlotO", x/1000., y/1000.) ;
        else FillHisto("SpasimirPlotI", x/1000., y/1000.) ;
      }
    }
  }
}

void L0TriggerResponse::FillEfficiencies(TString var, UInt_t L0Detector, UInt_t bit){

  UInt_t Trigger = (PrimInfo[L0Detector]).PrimitiveID_N3;
  Bool_t pass = (Trigger>>bit)&1;

  ////////////////////////////
  /// Each burst
  FillHisto( Form("%s_DenPerBurst",var.Data()), fBurstID);
  if(pass) FillHisto( Form("%s_NumPerBurst",var.Data()), fBurstID) ;

  ////////////////////////////
  /// Track momentum
  if(fTrackMom>10000. && fTrackMom<75000){
    FillHisto( Form("%s_DenPerTrackMom",var.Data()), fTrackMom/1000.);
    if(pass) FillHisto( Form("%s_NumPerTrackMom",var.Data()), fTrackMom/1000.);
  }

  ////////////////////////////
  /// Vertex Z position
  if(fZposition > 105000 && fZposition<175000){
    FillHisto( Form("%s_DenPerZposition", var.Data()), fZposition/1000.);
    if(pass) FillHisto( Form("%s_NumPerZposition",var.Data()), fZposition/1000.);
  }

  ////////////////////////////
  /// Detector Occupancy
  FillHisto( Form("%s_DenPerOccupancy", var.Data()), fOccupancy[L0Detector]);
  if(pass) FillHisto( Form("%s_NumPerOccupancy",var.Data()), fOccupancy[L0Detector]);
}

void L0TriggerResponse::FillL0CaloEfficiency(TString var, UInt_t L0Detector, UInt_t bit){

  UInt_t Trigger = (PrimInfo[L0Detector]).PrimitiveID_N3;
  Bool_t pass = (Trigger>>bit)&1;

  FillHisto(var, fLKrEnergy/1000.0);
  TString var2 = var.ReplaceAll("Den","Num");
  if(pass) FillHisto(var2, fLKrEnergy/1000.0);

}

double L0TriggerResponse::GetLKrTotEnergy(){
  TRecoLKrEvent* LKrEvent = GetEvent<TRecoLKrEvent>();
  double totenergy = 0. ;
  for(int i = 0 ; i < LKrEvent->GetNHits() ; ++i){
    TRecoLKrHit* hit = static_cast<TRecoLKrHit*>(LKrEvent->GetHit(i));
    double diff = hit->GetTime() - fRefTime ;
    if(fabs(diff) > 50.0) continue ;
    if(hit->GetEnergy()<40.) continue ;
    totenergy += hit->GetEnergy() ;
  }
  return totenergy ;
}

void L0TriggerResponse::RichMultPlot(TString histname, UInt_t L0Detector){
  // For NewRICH firmware used in debug mode, plot the hit multiplicity.
  // NB: these plots make no sense for data taken NOT in debug mode.

  // stop here if no plot will be filled.
  if(  !histname.Contains("Calib_Control") && !histname.Contains("K3pi_Q1_RICH") && !histname.Contains("Kmu2_M1_RICH")) return ;

  // 20ns cut here, but we only take the closest
  PrimAndTimes a = PrimInfo[L0Detector];

  // L0 hit multiplicity: the closest primitive to the reference time
  Int_t L0Mult = (a.PrimitiveID_N0 & 0x00FF);
  Double_t PrimDeltaTime = a.Time_N0_ref;

  if (fabs(a.Time_N1_ref) < fabs(a.Time_N0_ref)) {
    L0Mult = (a.PrimitiveID_N1 & 0x00FF);
    PrimDeltaTime = a.Time_N1_ref;
  }

  if (fabs(a.Time_N2_ref) < fabs(a.Time_N0_ref) && fabs(a.Time_N2_ref) < fabs(a.Time_N1_ref)) {
    PrimDeltaTime = a.Time_N2_ref;
    L0Mult = (a.PrimitiveID_N2 & 0x00FF);
  }

  //Consider inefficient an event with large delta time
  if (fabs(PrimDeltaTime) > 12.5) L0Mult = 0;

  if ((a.PrimitiveID_N3 & 0x0F00) || (!a.PrimitiveID_N3)) { // debug mode or no primitives
    if(histname.Contains("Calib_Control")) FillHisto("RICH_L0_Mult_SC_Mult", fNRICHSuperCells, L0Mult);

    if (histname.Contains("K3pi_Q1_RICH")) {
      FillHisto("RICH_L0_Mult_K3pi", L0Mult);
      if (fNRICHSuperCells>=3) FillHisto("RICH_L0_Mult_K3pi_R3", L0Mult);
      FillHisto("RICH_L0_Mult_SC_Mult_K3pi", fNRICHSuperCells, L0Mult);
    }
    if (histname.Contains("Kmu2_M1_RICH")) {
      FillHisto("RICH_L0_Mult_Kmu2", L0Mult);
      if (fNRICHSuperCells>=3) FillHisto("RICH_L0_Mult_Kmu2_R3", L0Mult);
      FillHisto("RICH_L0_Mult_SC_Mult_Kmu2", fNRICHSuperCells, L0Mult);
    }
  }
}

void L0TriggerResponse::ComputeDimuonEfficiency
(SpectrometerMUV3AssociationOutput& SpecMUV3, TString histname, UInt_t L0Detector) {

  if (SpecMUV3.GetNAssociationRecords() < 2) return;
  Int_t EqualTile = 0;
  Int_t TimeDiff = 0;
  for (Int_t iMu=0; iMu<SpecMUV3.GetNAssociationRecords(); iMu++) {
    Int_t Tile1 = SpecMUV3.GetAssociationRecord(iMu)->GetTileID();
    for (Int_t jMu=0; jMu<SpecMUV3.GetNAssociationRecords(); jMu++) {
      Int_t Tile2 = SpecMUV3.GetAssociationRecord(jMu)->GetTileID();
      if (iMu != jMu && Tile1 == Tile2)	EqualTile++;
    }
  }

  if (EqualTile == SpecMUV3.GetNAssociationRecords()*(SpecMUV3.GetNAssociationRecords()-1)) return;

  for (Int_t iMu=0; iMu<SpecMUV3.GetNAssociationRecords(); iMu++) {
    Double_t Time1 = SpecMUV3.GetAssociationRecord(iMu)->GetMuonTime();
    for (Int_t jMu=0; jMu<SpecMUV3.GetNAssociationRecords(); jMu++) {
      Double_t Time2 = SpecMUV3.GetAssociationRecord(jMu)->GetMuonTime();
      if (fabs(Time1-Time2) > 5) TimeDiff++;
    }
  }

  if (TimeDiff == SpecMUV3.GetNAssociationRecords()*(SpecMUV3.GetNAssociationRecords()-1)) return;

  ComputeEfficiency(histname, L0Detector);
  if(histname.Contains("_M2_") ) FillEfficiencies("M2",  kL0MUV3, 13);
  if(histname.Contains("_MO2_")) FillEfficiencies("MO2",  kL0MUV3, 12);
}

void L0TriggerResponse::PageOne(TCanvas* Canvas, TString histname, UInt_t L0Detector, TString plotname) {
  Canvas->Clear() ;
  Canvas->SetGridy(0);
  TString OutputPDFFileName = fAnalyzerName + ".pdf";

  TH1F* hTotal       = static_cast<TH1F*>(fHisto.GetTH1(Form("Total%s", histname.Data())));
  TH2F* L0Trigger_N0 = static_cast<TH2F*>(fHisto.GetTH1(Form("L0Trigger_N0%s", histname.Data())));
  TH2F* L0Trigger_N1 = static_cast<TH2F*>(fHisto.GetTH1(Form("L0Trigger_N1%s", histname.Data())));
  TH2F* L0Trigger_N2 = static_cast<TH2F*>(fHisto.GetTH1(Form("L0Trigger_N2%s", histname.Data())));
  TH2F* L0Trigger_N3 = static_cast<TH2F*>(fHisto.GetTH1(Form("L0Trigger_N3%s", histname.Data())));

  L0Trigger_N0->GetYaxis()->SetLimits(-0.3, 1.7);
  L0Trigger_N0->SetMarkerColor(kRed);
  L0Trigger_N0->SetMarkerSize(1);
  L0Trigger_N0->SetMarkerStyle(20);
  L0Trigger_N0->GetYaxis()->SetTickLength(0.01);
  L0Trigger_N1->GetYaxis()->SetLimits(-0.4, 1.6);
  L0Trigger_N1->SetMarkerColor(kBlack);
  L0Trigger_N1->SetMarkerSize(1);
  L0Trigger_N1->SetMarkerStyle(20);
  L0Trigger_N1->GetYaxis()->SetTickLength(0.01);
  L0Trigger_N2->GetYaxis()->SetLimits(-0.5, 1.5);
  L0Trigger_N2->SetMarkerColor(kBlue);
  L0Trigger_N2->SetMarkerSize(1);
  L0Trigger_N2->SetMarkerStyle(20);
  L0Trigger_N2->GetYaxis()->SetTickLength(0.01);
  L0Trigger_N3->GetYaxis()->SetLimits(-0.6, 1.4);
  L0Trigger_N3->SetMarkerColor(8);
  L0Trigger_N3->SetMarkerSize(1);
  L0Trigger_N3->SetMarkerStyle(20);
  L0Trigger_N3->GetYaxis()->SetTickLength(0.01);

  ///////////////////////////
  // define custom plot axes
  TH2F* h = static_cast<TH2F*>(GetAxisHisto(1, L0Detector));
  h->SetTitle(plotname);
  h->Draw();
  L0Trigger_N1->Draw("text20 same");
  L0Trigger_N0->Draw("text20 same");
  L0Trigger_N2->Draw("text20 same");
  L0Trigger_N3->Draw("text20 same");

  TText *CTrig = new TText
    (0.8, 0.85, Form("Number of control triggers: %.0f", hTotal->GetBinContent(1)));
  CTrig->SetTextFont(2);    // sets font = 0, precision = 2
  CTrig->SetTextAlign(32);  // precision = 2 allows slanted text
  CTrig->SetTextSize(0.03);
  CTrig->SetNDC();
  CTrig->Draw();

  TLegend *leg = new TLegend(0.1,0.8,0.35,0.9);
  leg->AddEntry(L0Trigger_N1,"Slot N");
  leg->AddEntry(L0Trigger_N0,"Slot N-1");
  leg->AddEntry(L0Trigger_N2,"Slot N+1");
  leg->AddEntry(L0Trigger_N3,"OR of 3 slots");
  leg->Draw();
  Canvas->Print(OutputPDFFileName, "pdf");
  delete leg;
  delete CTrig;
  delete h;
}

void L0TriggerResponse::PageTwo(TCanvas* Canvas, TEfficiency* threeslotseff, UInt_t L0Detector) {
  Canvas->Clear() ;
  Canvas->SetGridy(1);
  TString OutputPDFFileName = fAnalyzerName + ".pdf";

  // define custom plot axes
  TH1F* h = (TH1F*)GetAxisHisto(0, L0Detector);

  //////////////////////////////
  // draw efficiency for muons

  threeslotseff->Paint("P");
  TGraphAsymmErrors* gEff = new TGraphAsymmErrors();
  gEff = threeslotseff->GetPaintedGraph();
  if (!gEff) {
    cout << user_normal() << "Cannot read output!"<<endl;
    delete gEff;
    delete h;
    return;
  }
  for (Int_t i=0; i<16; i++) {
    gEff->SetPointEXlow(i,0.);
    gEff->SetPointEXhigh(i,0.);
  }
  gEff->SetMarkerColor(kBlue);
  gEff->SetMarkerSize(0.5);
  gEff->SetMarkerStyle(21);
  gEff->SetName("gEff");

  h->SetTitle(threeslotseff->GetTitle());
  h->Draw();
  gEff->Draw("P");
  Canvas->Update(); // crashes without this??
  Canvas->Print(OutputPDFFileName, "pdf");
  delete gEff;
  delete h;
}

TObject* L0TriggerResponse::GetAxisHisto(bool mode, UInt_t L0Detector) {
  TString TriggerBitNames[16] = {"0", "1", "2", "3", "4", "5",
    "6","7", "8", "9", "A", "B",
    "C", "D", "Any", "Reserved"};

  if (L0Detector == kL0RICH) std::copy(RICHTriggerBitNames,RICHTriggerBitNames+16, TriggerBitNames);
  if (L0Detector == kL0LAV)  std::copy(LAV12TriggerBitNames,LAV12TriggerBitNames+16, TriggerBitNames);
  if (L0Detector == kL0MUV3) std::copy(MUV3TriggerBitNames,MUV3TriggerBitNames+16, TriggerBitNames);
  if (L0Detector == kL0NewCHOD) {

    // run numbers here are approximate ...
    // based on info available in the ELOG and RunConditions database
    if (fRunID < 5181) std::copy(NCTriggerBitNames1,NCTriggerBitNames1+16, TriggerBitNames);
    else if (fRunID < 5577) std::copy(NCTriggerBitNames2,NCTriggerBitNames2+16, TriggerBitNames);
    else std::copy(NCTriggerBitNames3,NCTriggerBitNames3+16 , TriggerBitNames);
  }

  if (!mode) {
    TH1F* h = new TH1F("h", "", 16, -0.5, 15.5);
    for (Int_t i=0; i<16; i++) h->GetXaxis()->SetBinLabel(i+1, TriggerBitNames[i]);
    return h;
  }
  else {
    TString Yaxis[2] = {"NO", "YES"};
    TH2F* h = new TH2F("h", "", 16, -0.5, 15.5, 2, -0.5, 1.5);
    for (Int_t i=0; i<16; i++) h->GetXaxis()->SetBinLabel(i+1, TriggerBitNames[i]);
    for (Int_t i=0; i<2; i++) h->GetYaxis()->SetBinLabel(i+1, Yaxis[i]);
    return h;
  }
}

void L0TriggerResponse::StartOfBurstUser(){
  if (!fReadingData) return;

  SetCounterValue("nControlEvents",0);

  ///////////////////////////////////////////////////
  // Store the run&burst number
  ///////////////////////////////////////////////////

  fRunID= GetEventHeader()->GetRunID() ;
  fBurstID = GetEventHeader()->GetBurstID() ;
  UInt_t bursttime = GetEventHeader()->GetBurstTime();
  /*
     cout << endl << "Reading run " << fRunID
     << " Burst " << fBurstID
     << " Time " << bursttime << endl;
     */
  FillHisto("RunNumber", fRunID);
  FillHisto("BurstTime", fBurstID, double(bursttime) );

  ///////////////////////////////////////////////////
  //// get Q2 and QX bits
  ///////////////////////////////////////////////////

  if(fRunID<5181){
    Q2bit   = 14;
    QXbit   = 14 ;
    UTMCbit = 14;
    E20bit  = 14;
    E10bit  = 14;
  }
  else if(fRunID<5577){
    Q2bit   = 4;
    QXbit   = 5;
    UTMCbit = 14;
    E20bit  = 14;
    E10bit  = 14;
  }
  else if(fRunID<7000){
    Q2bit   = 10;
    QXbit   = 11;
    UTMCbit = 12;
    E20bit  = 14;
    E10bit  = 14;
  }
  else{
    // 2017 data
    Q2bit   = 10;
    QXbit   = 11;
    UTMCbit = 12;
    E20bit  = 12;
    E10bit  = 13;
  }
}

void L0TriggerResponse::EndOfBurstUser(){
  if (!fReadingData) return ;

  FillHisto("nL0TPSpecialTriggers",fBurstID, fNL0);

  ////////////////////////////////////////////////
  ///   Fill Argonion vs Control events plot
  ////////////////////////////////////////////////

  Double_t nControlEvents = GetCounterValue("nControlEvents");
  FillHisto("nControlEvents", fBurstID, nControlEvents);
  FillHisto("ControlVsArgonion", fArgonionCount, nControlEvents) ;
  SetCounterValue("nControlEvents",0);

  for(int i=0; i<fNMasks; ++i){
    TH1F* th = (TH1F*)fHisto.GetTH1( Form("nEventsMask%i",i));
    double cont = th->GetBinContent( th->FindBin(fBurstID) );
    FillHistoArray("nEventsVsArgonionMask", i, fArgonionCount, cont*fArgonionCount) ; // same plot, different name
  }

  if(fArgonionCount>0.001) FillHistoArray("nEventsMask",fNMasks, fBurstID, nControlEvents/fArgonionCount );
  FillHistoArray("nEventsVsArgonionMask", fNMasks, fArgonionCount, nControlEvents) ; // same plot, different name


  ////////////////////////////////////////////////
  ///   Only fill correlations when the
  ///   downscaling factor is known
  ////////////////////////////////////////////////
  if(fHasSpecialTrigger){

    ////////////////////////////////////////////////
    ///   Sort out MaskCorrelation plot
    ////////////////////////////////////////////////

    TH2F* maskCorr       = static_cast<TH2F*>(fHisto.GetTH1("MaskCorrelationRawBurst"));
    TH2F* maskCorrRaw    = static_cast<TH2F*>(fHisto.GetTH1("MaskCorrelationRaw"));
    TH2F* maskCorrRawWDS = static_cast<TH2F*>(fHisto.GetTH1("MaskCorrelationRawWDS"));
    for(int i=0; i<fNMasks; i++){
      for(int j=0; j<fNMasks; j++){
        double downscale = MaskDownscales[j] ;
        if(i==j) downscale = 1.0 ;
        double newcontent = maskCorr->GetBinContent(i+1,j+1) ;
        maskCorrRaw->Fill(i, j, newcontent) ;
        maskCorrRawWDS->Fill(i, j, newcontent*downscale) ;
      }
    }
    maskCorr->Reset() ;
  }
  // Reset variables
  fHasSpecialTrigger = false;
  MaskDownscales.clear();
  MaskDownscales.resize(fNMasks);
  fNL0 = 0;
}

void L0TriggerResponse::StartOfRunUser(){
  if (!fReadingData) return;
  if (GetWithMC()) return;

  TString Line;

  // Read NewCHOD CoarseT0s
  TString NewCHODCoarseT0FileName = "NewCHOD-CoarseT0.dat";
  std::vector<Double_t> StationsT0;
  std::vector<Double_t> ROMezzaninesT0;
  if(NA62ConditionsService::GetInstance()->Open(NewCHODCoarseT0FileName)!=kSuccess) return;
  while(Line.ReadLine(NA62ConditionsService::GetInstance()->Get(NewCHODCoarseT0FileName))){
    if(Line.BeginsWith("#")) continue;
    else if(Line.BeginsWith("StationsT0")){
      TObjArray * l = Line.Tokenize(" ");
      for (Int_t iStation=0;iStation<l->GetEntries()-1;iStation++){
        StationsT0.push_back(static_cast<TObjString*>(l->At(iStation+1))->GetString().Atof());
      }
      delete l;
    }
    else if(Line.BeginsWith("MezzaninesT0_")){
      TObjArray * l = Line.Tokenize(" ");
      for (Int_t iMezzanine=0;iMezzanine<16;iMezzanine++){
        ROMezzaninesT0.push_back(static_cast<TObjString*>(l->At(iMezzanine+1))->GetString().Atof());
      }
      delete l;
    }
  }
  NA62ConditionsService::GetInstance()->Close(NewCHODCoarseT0FileName);

  // Read NewCHOD T0s
  TString NewCHODT0FileName = "NewCHOD-T0.dat";
  if (NA62ConditionsService::GetInstance()->Open(NewCHODT0FileName)!=kSuccess) return;
  while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(NewCHODT0FileName))) {
    if (Line.BeginsWith("#")) continue;
    TObjArray *l = Line.Tokenize(" ");
    Int_t    ROChID = static_cast<TObjString*>(l->At(0))->GetString().Atoi();
    Int_t      ChID = static_cast<TObjString*>(l->At(1))->GetString().Atoi();
    Double_t FineT0 = static_cast<TObjString*>(l->At(2))->GetString().Atof();
    if(fabs(FineT0)>999) FineT0 = 0.0; // -999.999: masked channel, +999.999: failed to compute T0

    fNewCHODT0Map.emplace(ChID,StationsT0[0]+ROMezzaninesT0[ROChID/128]+FineT0);

    delete l;
  }
  NA62ConditionsService::GetInstance()->Close(NewCHODT0FileName);
}

void L0TriggerResponse::PrimitiveEOB(TSpecialTriggerEvent* STE, int ChannelGap, TString DetName){

  if(!STE){
    std::cout << user_normal() << "Error: no STE in PrimitiveEOB function!" << std::endl;
    return;
  }

  for(int i = 0 ; i < STE->GetNSpecialTriggers() ; i++){
    TTDCBSpecialTrigger* TDCBSpecTrig = reinterpret_cast<TTDCBSpecialTrigger*>(STE->GetHit(i));
    if(!TDCBSpecTrig){
      std::cout << user_normal() << "Error: Could not find TDCB special trigger." << std::endl;
      continue;
    }

    int fpga = TDCBSpecTrig->GetFPGAID() ;

    PrimCounter* counter=nullptr;
    PrimRegister* primregister=nullptr;

    TH1F* h = NULL ;
    if(fpga == 4){
      // The SL

      // SL tight hit counts
      counter = TDCBSpecTrig->GetCounter("MUV3_COUNTS");
      if(counter){
        h = (TH1F*)fHisto.GetTH1( Form("%sEOBSLTight",DetName.Data())) ;
        for(unsigned int j = 0 ; j < counter->GetNEntries(); ++j){
          h->SetBinContent(j+1,  counter->GetValue(j));
        }
      }
      else{
        std::cout << user_normal() << "Error: no counter MUV_COUNTS!" << std::endl;
      }

      // SL error counts
      counter = TDCBSpecTrig->GetCounter("MUV3_ERROR");
      if(counter){
        h = (TH1F*)fHisto.GetTH1( Form("%sEOBSLErrorCounts", DetName.Data()) ) ;
        for(unsigned int j = 0 ; j < counter->GetNEntries(); ++j){
          h->SetBinContent(j+1, counter->GetValue(j));
        }
      }
      else{
        std::cout << user_normal() << "Error: no counter MUV_ERROR!" << std::endl;
      }
    }
    else{
      // PPs

      // PP channel counts
      counter = TDCBSpecTrig->GetCounter("CHANNEL_COUNT_L");
      if(counter){
        h = (TH1F*)fHisto.GetTH1(Form("%sEOBPPScaler",DetName.Data())) ;
        int jump = 128/(ChannelGap*2);
        for(int j = 0 ; j < jump ; ++j){ // for groups of 16x2 channels
          for(int m = 0 ; m < ChannelGap ;++m){ // offset of 16 channels
            for(int k = 0 ; k < 2 ; ++k){ // two channels per tile
              int index = (j*ChannelGap*2)+m+(k*ChannelGap);
              int bin = 128*fpga + (j*2*ChannelGap)+k+(m*2)+1;
              h->SetBinContent(bin, counter->GetValue(index));
            }
          }
        }

        ///////////////////////////////////////////////////////////////////////////
        // nasty hack to account for recabling in NewCHOD in April 2018
        // see ELOG entries: 22756 and 22815
        if(DetName.Contains("NewCHOD") && fRunID>5809){
          Double_t temp = h->GetBinContent(257+1);
          h->SetBinContent(257+1, h->GetBinContent(269+1));
          h->SetBinContent(269+1, temp);
        }
        ///////////////////////////////////////////////////////////////////////////
      }
      else{
        std::cout << user_normal() << "Error: no counter CHANNEL_COUNT_L!" << std::endl;
      }

      // PP tight hit counters
      counter = TDCBSpecTrig->GetCounter("M3PSTA_TightHits");
      if(counter){
        h = (TH1F*)fHisto.GetTH1(Form("%sEOBPPTightHits",DetName.Data())) ;
        for(unsigned int j = 0 ; j < counter->GetNEntries(); ++j){
          h->SetBinContent(64*fpga+j+1, counter->GetValue(j));
        }
      }
      else{
        std::cout << user_normal() << "Error: no counter M3PSTA_TightHits!" << std::endl;
      }

      // PP loose hit counters
      counter = TDCBSpecTrig->GetCounter("M3PSTA_LooseHits");
      if(counter){
        h = (TH1F*)fHisto.GetTH1(Form("%sEOBPPLooseHits",DetName.Data())) ;
        for(unsigned int j = 0 ; j < counter->GetNEntries(); ++j){
          h->SetBinContent(64*fpga+j+1, counter->GetValue(j));
        }
      }
      else{
        std::cout << user_normal() << "Error: no counter M3PSTA_LooseHits!" << std::endl;
      }

      // PP error flags
      primregister = TDCBSpecTrig->GetRegister("M3PSTA_ErrorFlags");
      if(primregister){
        int errorflags = primregister->GetValue();
        for(unsigned int j = 0 ; j < 16 ; ++j){
          if((errorflags>>j)&0x1) FillHisto(Form("%sEOBPPErrorFlags%i",DetName.Data(),fpga), j+1) ;
        }
      }
      else{
        std::cout << user_normal() << "Error: no register M3PSTA_ErrorFlags!" << std::endl;
      }

      // PP error counts
      counter = TDCBSpecTrig->GetCounter("M3PSTA_ErrorCounts");
      if(counter){
        h = (TH1F*)fHisto.GetTH1(Form("%sEOBPPErrorCounts%i",DetName.Data(),fpga)) ;
        for(unsigned int j = 0 ; j < counter->GetNEntries(); ++j){
          h->SetBinContent(j+1, counter->GetValue(j));
        }
      }
      else{
        std::cout << user_normal() << "Error: no counter M3PSTA_ErrorCounts!" << std::endl;
      }

    }// pp or sl?
  }// loop over special triggers
}

void L0TriggerResponse::PlotPrimitiveEOBInfo(TCanvas* Canvas, int mode, std::vector<TH1*> h_eob){
  TString OutputPDFFileName = fAnalyzerName + ".pdf";
  Canvas->Clear() ;
  Canvas->SetLogy(0) ;

  Double_t lower=0;
  Double_t upper=0;

  if(mode==0){
    lower=-0.5;
    upper=383.5;
  }
  if(mode==1){
    lower=191.5;
    upper=511.5;
  }

  h_eob[0]->SetLineColor(kBlack) ;
  h_eob[0]->SetFillColor(kMagenta) ;
  h_eob[0]->SetFillStyle(1001) ;
  h_eob[0]->Draw() ;
  h_eob[1]->SetLineWidth(0) ;
  h_eob[1]->SetFillColor(kGray) ;
  h_eob[1]->SetFillStyle(1001) ;
  h_eob[1]->Draw("same") ;
  h_eob[2]->SetLineColor(kBlue) ;
  h_eob[2]->SetLineWidth(2) ;
  h_eob[2]->Draw("same");

  h_eob[0]->Draw("axis same");
  h_eob[0]->GetXaxis()->SetRangeUser(lower,upper);

  TText *TextA = new TText(0.2, 0.85, Form("Counts: %.0f", h_eob[0]->Integral() ));
  TextA->SetNDC() ;
  TText *TextB = new TText(0.2, 0.80, Form("Tight: %.0f", h_eob[1]->Integral() ));
  TextB->SetNDC() ;
  TText *TextC = new TText(0.2, 0.75, Form("Loose: %.0f", h_eob[2]->Integral() ));
  TextC->SetNDC() ;

  TextA->Draw() ;
  TextB->Draw() ;
  TextC->Draw() ;

  Canvas->Print(Form(OutputPDFFileName), "pdf");

  delete TextA;
  delete TextB;
  delete TextC;

  Canvas->Clear() ;
  Canvas->SetLogy();

  TH1F* dhist        = (TH1F*)h_eob[1]->Clone("MissingHits") ;
  TH1F* dhistB       = (TH1F*)h_eob[1]->Clone("MissingHitsB") ;
  TH1F* missingRatio = (TH1F*)h_eob[1]->Clone("MissingRatio") ;

  dhist->Reset();
  dhistB->Reset();
  missingRatio->Reset();
  dhist->SetTitle("Missing Hits") ;
  dhistB->SetTitle("Missing Hits") ;
  missingRatio->SetTitle("Missing Hit Ratio") ;

  for(int i=0; i<258; ++i) missingRatio->SetBinContent(i,1.0);

  for(int i=0; i<256; ++i){
    double a   = h_eob[0]->GetBinContent(i*2+1) ;
    double b   = h_eob[0]->GetBinContent(i*2+1+1) ;
    double num = a+b;
    double c   = h_eob[1]->GetBinContent(i+1) ;
    double d   = h_eob[2]->GetBinContent(i+1) ;
    double val = num - (2*c) - d ;
    if(val>=0) dhist->SetBinContent(i+1, val ) ;
    else dhistB->SetBinContent(i+1, fabs(val) ) ;

    // "efficiency" of each channel.
    // If all hits are missing (val==num) channel was probably blocked in the firmware (e.g. MUV0 channels)
    if(num>0 && (val!=num)) missingRatio->SetBinContent(i+1, 1.0-(val/num));
  }
  dhist->SetLineColor(kBlack) ;
  dhist->SetLineWidth(2) ;
  dhistB->SetLineColor(kRed) ;
  dhistB->SetLineWidth(1) ;
  dhistB->SetLineStyle(2) ;
  double max1 = dhist->GetBinContent( dhist->GetMaximumBin() ) ;
  double max2 = dhistB->GetBinContent( dhistB->GetMaximumBin() ) ;
  if( max1>=max2){
    dhist->Draw() ;
    dhistB->Draw("same") ;
    dhist->GetXaxis()->SetRangeUser(lower,upper);
    dhist->Draw("axis same");
  }
  else{
    dhistB->Draw() ;
    dhist->Draw("same") ;
    dhistB->GetXaxis()->SetRangeUser(lower,upper);
    dhistB->Draw("axis same");
  }

  TText *TextE = new TText(0.2, 0.85, Form("Missing: %.0f", dhist->Integral() ));
  TextE->SetNDC() ;
  TextE->Draw();

  Canvas->Print(Form(OutputPDFFileName), "pdf");
  delete TextE ;

  // now plotting the ratio of missing to all hits.
  Canvas->Clear();
  Canvas->SetLogy(0); // now in linear scale

  missingRatio->SetMarkerStyle(7);
  missingRatio->Draw("P0");

  std::vector<TLine*> small;
  small.reserve(16);
  for(int i=0; i<32; ++i){
    double x=i*16.0;
    if(x<lower || x>upper) continue;
    TLine* a = new TLine(x, missingRatio->GetMinimum(), x, missingRatio->GetMaximum());
    a->SetLineStyle(2);
    a->SetLineColor(17);
    a->Draw();
    small.push_back(a);
  }

  missingRatio->GetXaxis()->SetRangeUser(lower,upper);

  Canvas->Print(Form(OutputPDFFileName), "pdf");
  for(unsigned int i=0; i<small.size(); ++i) delete small[i];
  small.clear();

  // now plotting EOB PP Error Counts
  Canvas->Clear();
  Canvas->SetLogy();

  TLegend *leg = new TLegend(0.7,0.5,0.9,0.9);
  double max = -99 ;
  for(int i = 0 ; i < 4 ; ++i){
    h_eob[3+i]->SetLineColor(1+i) ;
    h_eob[3+i]->SetLineWidth(3) ;
    if( h_eob[3+i]->GetMaximum()>max) max = h_eob[3+i]->GetMaximum();
    leg->AddEntry(h_eob[3+i], Form("PP%i",i), "l");
    if(i==0) h_eob[3+i]->Draw("") ;
    else h_eob[3+i]->Draw("same") ;
  }
  h_eob[3]->GetYaxis()->SetRangeUser(0.0999, pow(10,ceil(log10(max)))) ;
  leg->Draw() ;
  Canvas->Print(Form(OutputPDFFileName), "pdf");
  delete leg;

  Canvas->Clear() ;
  Canvas->SetLogy(0) ;

  h_eob[7]->SetLineWidth(1);
  h_eob[7]->Draw() ;
  h_eob[7]->GetYaxis()->SetRangeUser(0, h_eob[7]->GetMaximum()*1.1) ;
  TText *TextD = new TText(0.2, 0.85, Form("SLTight: %.0f", h_eob[7]->Integral() ));
  TextD->SetNDC() ;
  TextD->Draw();

  ///// New in May 2018

  TH1F* ha = (TH1F*)h_eob[7]->Clone("PPTightClone");
  for(int i=1; i<ha->GetNbinsX()+1; ++i){
    ha->SetBinContent(i, h_eob[1]->GetBinContent(i));
  }
  ha->SetFillStyle(1001);
  ha->SetFillColor(kRed);
  ha->Draw("same");
  h_eob[7]->Draw("same");
  h_eob[7]->Draw("axis same");

  /////

  Canvas->Print(Form(OutputPDFFileName), "pdf");
  delete TextD;

  Canvas->Clear() ;
  Canvas->SetLogy(1) ;
  h_eob[8]->Draw() ;
  Canvas->Print(Form(OutputPDFFileName), "pdf");

  Canvas->Clear() ;
  Canvas->SetLogy(0) ;

  return ;
}

void L0TriggerResponse::DigiTimeRaw(TString detector, Bool_t mode){

  ////////////////////////////////////////////////
  ///   Get correct histogram name
  ////////////////////////////////////////////////
  TString hname = "" ;
  TString hname2 = "" ;
  bool useh2 =false ;
  if(!mode) hname = Form("DigiTimeRaw_Control_%s", detector.Data());
  if(mode){
    if( (GetL0Data()->GetTriggerFlags() & 0x02) ){
      hname = Form("DigiTimeRaw_Phys1I_%s", detector.Data()) ;
      if( (GetL0Data()->GetTriggerFlags() == 0x02) ){
        useh2 = true ;
        hname2 = Form("DigiTimeRaw_Phys1E_%s", detector.Data()) ;
      }
    }
    else{
      hname = Form("DigiTimeRaw_PhysM_%s", detector.Data()) ;
    }
  }

  ////////////////////////////////////////////////
  ///   Remap detector names
  ////////////////////////////////////////////////
  if(detector.Contains("LAV12")) detector = "LAV" ;
  if(detector.Contains("Calo")) detector = "LKr" ;

  ////////////////////////////////////////////////
  ///   Fill DigiTimeRawFine
  ////////////////////////////////////////////////
  TDetectorVEvent* event = GetEvent(detector) ;
  for(int i = 0 ; i < event->GetNHits() ; ++i){
    TDetectorVHit* hit = static_cast<TDetectorVHit*>(event->GetHit(i));
    double diff = hit->GetTime() - fRefTime ;

    FillHisto(hname, diff) ;
    if(useh2) FillHisto(hname2, diff) ;
  }

}

void L0TriggerResponse::HitCorrelations(TString det1, TString det2){

  ////////////////////////////////////////////////
  ///   Get correct histogram name
  ///   Only Control events for now
  ////////////////////////////////////////////////
  TString hname = "";
  if(ControlData) hname = Form("HitCorrelations2D_Control%s%s", det1.Data(), det2.Data());
  else return ;

  ////////////////////////////////////////////////
  ///   Remap detector names
  ////////////////////////////////////////////////
  if(det1.Contains("LAV12")) det1 = "LAV" ;
  if(det1.Contains("Calo")) det1  = "LKr" ;
  det1 = det1.ReplaceAll("_","") ;

  if(det2.Contains("LAV12")) det2 = "LAV" ;
  if(det2.Contains("Calo")) det2  = "LKr" ;
  det2 = det2.ReplaceAll("_","") ;

  ////////////////////////////////////////////////
  ///   Fill Hit Correlation 2D
  ////////////////////////////////////////////////
  TDetectorVEvent* event1 = GetEvent(det1) ;
  TDetectorVEvent* event2 = GetEvent(det2) ;
  for(int i = 0 ; i < event1->GetNHits() ; ++i){
    TDetectorVHit* hit1 = static_cast<TDetectorVHit*>(event1->GetHit(i));
    double diff1 = hit1->GetTime() - fRefTime ;

    for(int j = 0 ; j < event2->GetNHits() ; ++j){

      TDetectorVHit* hit2 = static_cast<TDetectorVHit*>(event2->GetHit(j));
      double diff2 = hit2->GetTime() - fRefTime ;

      FillHisto(hname, diff1, diff2) ;
    }
  }

}

void L0TriggerResponse::LavVetoSelection(){
  if(!MyLavVetoSelection()) return ;
  FillEfficiencies("ELAV12",kL0LAV,14);
}

Bool_t L0TriggerResponse::MyLavVetoSelection(){

  TRecoCedarEvent*  CEDARevent  = GetEvent<TRecoCedarEvent>();

  ////////////////////////////////////////////////
  // Require a good Cedar candidates (>=5 sectors)

  Int_t NCEDARcand = CEDARevent->GetNCandidates();
  Int_t NGoodCedarCand = 0;
  for (Int_t i=0; i<NCEDARcand; i++) {
    TRecoCedarCandidate* Ccand = static_cast<TRecoCedarCandidate*>(CEDARevent->GetCandidate(i));
    if (Ccand->GetNSectors()>4) NGoodCedarCand++;
  }
  if (!NGoodCedarCand) return false;

  //////////////////////////////////////////
  // Require exactly one track in acceptance

  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  if (Tracks.size()!=1) return false;

  TRecoSpectrometerCandidate* Scand = Tracks[0].GetSpectrometerCandidate();
  Int_t    Q            = Tracks[0].GetCharge();
  Double_t Ptrack       = Tracks[0].GetMomentum(); // spectrometer calibration included
  Double_t Ptrackbefore = Tracks[0].GetMomentumBeforeFit();
  Double_t Ttrack       =
    (Tracks[0].CHODAssociationExists()) ? Tracks[0].GetCHODTime() : Tracks[0].GetTrackTime();
  Double_t Chi2track    = Tracks[0].GetChi2();

  if (Q!=1) return false;
  if (Chi2track>20.0) return false;
  if (Scand->GetNChambers()!=4) return false;
  if (fabs(Ptrack-Ptrackbefore)>20000.0) return false; // 20 GeV
  if (Ptrack<5000 || Ptrack>70000) return false;

  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kNewCHOD))         return false;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 0)) return false;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 1)) return false;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 2)) return false;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 3)) return false;
  if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kLKr))             return false;

  /////////////////////////////////////////
  // Zvertex & CDA: track wrt the beam axis

  Double_t cda  = Tracks[0].GetBeamAxisCDA();
  Double_t Zvtx = Tracks[0].GetBeamAxisVertex().Z();

  TVector3 KaonThreeMomentum = BeamParameters::GetInstance()->GetBeamThreeMomentum();
  TLorentzVector Kaon;
  Kaon.SetVectM(KaonThreeMomentum, MKCH);
  TLorentzVector Pion;
  Pion.SetVectM(Scand->GetThreeMomentumBeforeMagnet(), MPI);
  Double_t Mmiss2Pi = (Kaon-Pion).M2();

  Bool_t pas_zvtx = (Zvtx>110000 && Zvtx<180000);
  Bool_t pas_cda  = (cda<25);

  if (!pas_zvtx) return false;
  if (!pas_cda) return false;

  ////////////////////////////////////////////////////////////////////
  // Track-Cedar timing: look for the closest Cedar candidate to track

  Double_t dT_Track_Cedar =  999.999;
  for (Int_t i=0; i<NCEDARcand; i++) {
    TRecoCedarCandidate* Ccand = static_cast<TRecoCedarCandidate*>(CEDARevent->GetCandidate(i));
    if (Ccand->GetNSectors()<5) continue;
    if (fabs(Ccand->GetTime()-fRefTime)>10.0) continue; // cedar candidate in time with trigger
    Double_t dT = Ttrack - Ccand->GetTime(); // CHOD-Cedar, or Track-Cedar if no CHOD association
    if (fabs(dT) < fabs(dT_Track_Cedar)) {
      dT_Track_Cedar = dT;
    }
  }
  if (fabs(dT_Track_Cedar)>10.0) return false;

  ///////////////////////////////////
  // MUV3 veto: no track association
  if (Tracks[0].MUV3AssociationExists()) return false;

  ///////////////////
  // Pion ID with E/p
  Double_t eop = Tracks[0].GetLKrEoP();
  if(eop<0.05 || eop > 0.8) return false ;

  ///////////////////
  // Missing mass requirements
  double pi0mm2 = (MPI0/1000.)*(MPI0/1000.) ;
  double mm2 = (Mmiss2Pi*1e-6) - pi0mm2 ;
  if(fabs(mm2)>0.015) return false;

  ///////////////////
  // Select events with LAV12 vetoing

  // Using LAV matching tool (bad)
  // fLAVMatching->SetReferenceTime(fRefTime);
  // return fLAVMatching->LAVHasTimeMatching(LAVEvent);

  // Counting nhits, seems okay
  int nLAVhits=0;
  TRecoLAVEvent* LAVEvent = GetEvent<TRecoLAVEvent>();
  for (Int_t i=0; i<LAVEvent->GetNHits(); i++) {
    TRecoLAVHit* hit = static_cast<TRecoLAVHit*>(LAVEvent->GetHit(i));
    int station = hit->GetLAVID();
    if(station!=12) continue;
    Double_t dt = hit->GetTime() - fRefTime;
    if(fabs(dt)>5.0) continue;
    if(hit->GetEdgeMask()!=15) continue;
    nLAVhits++;
  }
  //std::cout << " lav hits in time : " << nLAVhits << std::endl;
  // return (nLAVhits>1);

  // Using Tommaso primitive generation
  fPrimitiveCollection.clear();
  LAVPrimitives();
  //std::cout << " lav primitives : " << fPrimitiveCollection.size() << std::endl;
  int nLAVprims=0;
  for(unsigned int i=0; i<fPrimitiveCollection.size(); ++i){
    double dt = fPrimitiveCollection[i].Time - fRefTime;
    if(fabs(dt)<5.0){
      nLAVprims++;
    }
  }
  //std::cout << " lav primitives in time : " << nLAVprims << std::endl;
  return (nLAVprims>0);
}

K3piSelResult L0TriggerResponse::MyK3piSelection(){

  K3piSelResult result ;
  result.success = false ;
  result.q1 = false ;
  result.q2 = false ;
  result.qx = false ;
  result.nhits = true;
  result.utmc = false;

  ///////////////////////////////////
  // Run the vertexing tool
  ///////////////////////////////////

  std::vector<DownstreamTrack> Tracks =
    *(std::vector<DownstreamTrack>*)GetOutput("DownstreamTrackBuilder.Output");
  std::vector<SpectrometerTrackVertex> Vertices =
    *(std::vector<SpectrometerTrackVertex>*)GetOutput("SpectrometerVertexBuilder.Output");

  // Compute the number of three-track vertices
  Int_t NThreeTrackVtx = 0;
  Int_t vtx_index = -1;
  for (UInt_t i=0; i<Vertices.size(); i++) {
    Int_t NTracks = Vertices[i].GetNTracks();
    if (NTracks==3) {
      NThreeTrackVtx++;
      vtx_index = i;
    }
  }

  ///////////////////////////////////
  // Only one vertex
  ///////////////////////////////////

  if (NThreeTrackVtx!=1) return result; // require exactly one three-track vertex

  ///////////////////////////////////
  // Vertex cuts
  ///////////////////////////////////

  Int_t TotalCharge = Vertices[vtx_index].GetCharge();
  if (TotalCharge!=1) return result;
  Double_t Chi2 = Vertices[vtx_index].GetChi2();
  if (Chi2>20.0) return result;
  Double_t Zvertex = Vertices[vtx_index].GetPosition().z();
  if (Zvertex<110000. || Zvertex>165000.) return result;

  fZposition = Zvertex;

  ///////////////////////////////////
  // Cuts on the vertex tracks
  ///////////////////////////////////

  std::vector<bool> quads(4,false);

  TRecoSpectrometerEvent* STRAWevent = GetEvent<TRecoSpectrometerEvent>();
  // Geometric acceptance for the tracks
  for (Int_t i=0; i<3; i++) {
    Int_t iTrack = Vertices[vtx_index].GetTrackIndex(i);

    ////////////////////////////////////////
    // Cuts on the Spectrometer candidates
    ////////////////////////////////////////

    TRecoSpectrometerCandidate* Scand =
      static_cast<TRecoSpectrometerCandidate*>(STRAWevent->GetCandidate(iTrack));
    if (Scand->GetMomentum()<1000) return result ; // 1 GeV to remove erroneous tracks
    if (Scand->GetChi2()>20.) return result ;
    if (Scand->GetNChambers()!=4) return result;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 0)) return result;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 1)) return result;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 2)) return result;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kSpectrometer, 3)) return result;
    if (!GeometricAcceptance::GetInstance()->InAcceptance(Scand, kNewCHOD))         return result;

    ///////////////////////////////////
    // Cuts on the Downstream tracks
    ///////////////////////////////////

    DownstreamTrack dsTrack = Tracks[iTrack];
    if( !dsTrack.NewCHODAssociationExists()) return result;
    Int_t InTimeHit(false);
    for(UInt_t j=0; j< UInt_t(dsTrack.GetNNewCHODAssociationRecords()); ++j){
      Double_t tdiff = dsTrack.GetNewCHODCandidateTime(j) - fRefTime ;
      FillHisto("K3pi_NCHits_ControlTrig",tdiff);
      if(fabs(tdiff)<10.0){
        InTimeHit++;
        TRecoNewCHODHit* nchit = static_cast<TRecoNewCHODHit*>(dsTrack.GetNewCHODCandidate(j));
        int quad = nchit->GetQuadrantID()-1 ;
        quads[quad] = true ;
      }
    }
    if(!InTimeHit) return result;
    if(Scand->GetCharge()<0){
      fTrackMom  = Scand->GetMomentum();
    }
  }

  ///////////////////////////////////
  // More cuts on the vertex
  ///////////////////////////////////

  TLorentzVector v[3] ;
  for (Int_t i=0; i<3; i++) {
    v[i].SetVectM(Vertices[vtx_index].GetTrackThreeMomentum(i), MPI);
  }
  TLorentzVector K3pi_FourMomentum  = v[0] + v[1] + v[2];
  TVector3       K3pi_Momentum      = K3pi_FourMomentum.Vect();
  Double_t       M3pi               = K3pi_FourMomentum.M();
  if (fabs(K3pi_Momentum.Mag()-75000.0)>3000.0) return result;
  if (M3pi<490.0 || M3pi>497.0) return result; // last cut

  ///////////////////////////////////
  // Fill result structure
  ///////////////////////////////////

  result.success = true;

  int quadcount = std::count(quads.begin(), quads.end(), true) ;
  if(quadcount>0) result.q1 = true ;
  if(quadcount>1) result.q2 = true ;
  if( (quads[0]&&quads[2]) || (quads[1]&&quads[3]) ) result.qx = true;

  int nhits = 0 ;
  TRecoNewCHODEvent* NCevent = GetEvent<TRecoNewCHODEvent>();
  for(int i=0 ; i<NCevent->GetNHits(); ++i){
    TRecoNewCHODHit* hit = static_cast<TRecoNewCHODHit*>(NCevent->GetHit(i));
    double diff = hit->GetTime() - fRefTime ;
    if( fabs(diff)<25.0){
      nhits++;
    }
  }

  result.nhits = nhits ;
  result.utmc = (fUTMCHits<5);
  return result;
}

Int_t L0TriggerResponse::LAVPrimitives(){

  TRecoLAVEvent* LAVEvent = GetEvent<TRecoLAVEvent>();
  Double_t fLAVLowThreshold = 0.005;
  Double_t fLAVHighThreshold = 0.015;
  Double_t fLAVBackwardTimeCut  = 0;
  Double_t fLAVForwardTimeCut = 6.5;
  Double_t fLAVTCutPrimitiveLeft =  -12.5;
  Double_t fLAVTCutPrimitiveRight =  12.5;
  Double_t fLAVPrimitiveOffset =  0.0;
  Int_t MAXEDGES=256;

  Int_t nedges[256]={0};
  Array2D<Double_t> edgeTimes = CreateArray2D<Double_t>(256, MAXEDGES, 0);
  Array2D<Int_t> edgeTypes = CreateArray2D(256, MAXEDGES, 0);

  // assume reconstructed hits of the same channel are time sorted.

  Int_t nLAVRecoHits = LAVEvent->GetNHits();
  TClonesArray& hitArray = (* (LAVEvent->GetHits()));
  for (Int_t i=0; i<nLAVRecoHits; i++) {
    TRecoLAVHit* hit = static_cast<TRecoLAVHit*>(hitArray[i]);
    Int_t channel = hit->GetChannelID();
    Int_t station = channel/10000;
    Int_t edgeMask = hit->GetEdgeMask();
    if (station != 12) continue; // only use LAV12
    if (edgeMask!= 15) continue; // only use complete hits with leading edge times for high and low thresholds
    Int_t packedChannel = hit->GetPackedChannelID();
    if (nedges[packedChannel] == MAXEDGES-1) {
      cout << user_normal() << "LAVPrimitives >> max number of edges reached: " << packedChannel << " " << channel << endl;
      continue;
    }

    if (edgeMask&1){
      edgeTimes[packedChannel][nedges[packedChannel]] = hit->GetLeadingEdgeLow();
      edgeTypes[packedChannel][nedges[packedChannel]] = 1;
      nedges[packedChannel]++;
    }

    if (edgeMask&2){
      edgeTimes[packedChannel][nedges[packedChannel]] = hit->GetLeadingEdgeHigh();
      edgeTypes[packedChannel][nedges[packedChannel]] = 2;
      nedges[packedChannel]++;
    }
  }

  std::vector<Double_t> tHitPairs(MAXEDGES,0.0);
  Int_t nHitPairs = 0;

  for (int i=0; i<256; i++) { // maybe in future, change the ordering

    if (nedges[i] < 2) continue;

    int openHit = 0;
    int usedEdgeLast = 0;
    int backwardFlag = 0;

    for (int j=0; j<nedges[i]; j++) {
      if (edgeTypes[i][j] == 1) { // Low threshold
        if (!openHit) { // LL
          openHit = 1;
          backwardFlag = 0;
          if (j>0) {
            if(usedEdgeLast == 0 && edgeTypes[i][j-1] == 2 && edgeTimes[i][j]-edgeTimes[i][j-1] < -fLAVBackwardTimeCut) backwardFlag = 1;
          }
          usedEdgeLast = 0;
        }
        else { // LL LL
          if (backwardFlag){
            tHitPairs[nHitPairs] = edgeTimes[i][j-1]; // check if in the FPGA here there is no slewing correction
            // chHitPairs[nHitPairs] = i;
            nHitPairs++;
          }
          backwardFlag = 0;
          usedEdgeLast = 0;
        }
      }

      if (edgeTypes[i][j] == 2) { // High threshold
        if (openHit) { // LL LH
          double dtLead = edgeTimes[i][j]-edgeTimes[i][j-1];
          if (dtLead < fLAVForwardTimeCut) { // new pair found
            tHitPairs[nHitPairs] = edgeTimes[i][j-1] - fLAVLowThreshold/(fLAVHighThreshold-fLAVLowThreshold)*dtLead;
            // chHitPairs[nHitPairs] = i;
            usedEdgeLast = 1;
            nHitPairs++;
          }
          else if (backwardFlag){
            tHitPairs[nHitPairs] = edgeTimes[i][j-1]; // check if in the FPGA here there is no slewing correction
            // chHitPairs[nHitPairs] = i;
            nHitPairs++;
          }
          openHit = 0;
        }
      }
    }
    if (openHit) {
      if (backwardFlag){
        tHitPairs[nHitPairs] = edgeTimes[i][nedges[i]]; // check if in the FPGA here there is no slewing correction
        // chHitPairs[nHitPairs] = i;
        nHitPairs++;
      }
    }
  }

  // - select a leading edge from high threshold
  // - select a leading edge from low threshold if -cut1 < DeltaT = THigh-TLow < cut2 (cut1 = 0.; cut2 = 6.5 ns)
  // If pair found:
  // - apply slewing correction
  // - evaluate Tcorr
  // - count the pair
  // Algoritmo che usa: Npairs in time wrt the first coming -12.5, 12.5 ns
  // each other coming --> compute average for the primitive time
  // output number of hits (4 thresholds) and time

  Int_t nLAVPrimitives = -fPrimitiveCollection.size();
  std::vector<Int_t> usedHit(MAXEDGES,0);

  for (Int_t i=0; i< nHitPairs; i++) {
    if (usedHit[i] == 1) continue;
    usedHit[i] = 1;
    Double_t primitiveTimes = tHitPairs[i];
    Int_t nHitsPerPrimitive = 1;

    for (Int_t j=0; j< nHitPairs; j++) {
      //      if (chHitPairs[i] == chHitPairs[j]) continue;// no cut is applied at this level
      if (usedHit[j] == 1) continue;
      double dt = tHitPairs[i]-tHitPairs[j];
      if (dt > fLAVTCutPrimitiveLeft && dt < fLAVTCutPrimitiveRight) {
        usedHit[j] = 1;
        nHitsPerPrimitive++;
        primitiveTimes += tHitPairs[j];
      }
    }
    primitiveTimes /= nHitsPerPrimitive;
    SetPrimitive(primitiveTimes+fLAVPrimitiveOffset,nHitsPerPrimitive,2); // LAV is detector 2
  }

  return nLAVPrimitives+fPrimitiveCollection.size();

}

void L0TriggerResponse::SetPrimitive(Double_t finetime, Int_t nhits, Int_t det){
  LAVPrimitive nn;
  nn.Time = finetime;
  nn.NHit=nhits;
  nn.iDet=det;
  fPrimitiveCollection.push_back(nn);
}

void L0TriggerResponse::GetDetOccupancies(){

  // Warning: can't be more than 10.0
  Double_t OccCut=5.0;

  ///////////////////////////////////////////////////////////////
  //// Reset member values
  ///////////////////////////////////////////////////////////////
  fUTMCHits=0;
  fNRICHHits=0;
  fNRICHSuperCells=0;
  for(int i=0;i<7;++i) fOccupancy[i]=0;
  int hitcount=0 ;

  ///////////////////////////////////////////////////////////////
  //// L0CHOD
  ///////////////////////////////////////////////////////////////
  hitcount=0;
  TRecoCHODEvent* Cevent = GetEvent<TRecoCHODEvent>();
  for( int i = 0 ; i < Cevent->GetNHits() ; ++i){
    TRecoCHODHit* hit = static_cast<TRecoCHODHit*>(Cevent->GetHit(i));
    double diff = hit->GetTime() - fRefTime ;
    if(fabs(diff)<OccCut) hitcount++;
  }
  fOccupancy[kL0CHOD] = hitcount;

  ///////////////////////////////////////////////////////////////
  //// L0RICH
  ///////////////////////////////////////////////////////////////
  hitcount=0;
  fNRICHHits=0;
  fNRICHSuperCells=0 ;
  TRecoRICHEvent* Revent = GetEvent<TRecoRICHEvent>();
  for( int i = 0 ; i < Revent->GetNHits() ; ++i){
    TRecoRICHHit* hit = static_cast<TRecoRICHHit*>(Revent->GetHit(i));
    double diff = hit->GetTime() - fRefTime ;
    if(fabs(diff)<10.0){
      if(hit->GetOrSuperCellID()) fNRICHHits++; // hits
      else{
        fNRICHSuperCells++;
        if(fabs(diff)<OccCut) hitcount++; // SC
      }
    }
  }
  fOccupancy[kL0RICH] = hitcount;

  ///////////////////////////////////////////////////////////////
  //// L0LAV
  ///////////////////////////////////////////////////////////////
  hitcount=0;
  TRecoLAVEvent* Levent = GetEvent<TRecoLAVEvent>();
  for( int i = 0 ; i < Levent->GetNHits() ; ++i){
    TRecoLAVHit* hit = static_cast<TRecoLAVHit*>(Levent->GetHit(i));
    if(hit->GetLAVID()!=12) continue; // only LAV12 at L0
    Int_t EM = hit->GetEdgeMask();
    if((EM&1)&&(EM&2)){ // leading low and high thresholds
      double diff = hit->GetTime() - fRefTime ;
      if(fabs(diff)<OccCut) hitcount++;
    }
  }
  fOccupancy[kL0LAV] = hitcount;

  ///////////////////////////////////////////////////////////////
  //// L0MUV3
  ///////////////////////////////////////////////////////////////
  hitcount=0;
  TRecoMUV3Event* Mevent = GetEvent<TRecoMUV3Event>();
  for( int i = 0 ; i < Mevent->GetNHits() ; ++i){
    TRecoMUV3Hit* hit = static_cast<TRecoMUV3Hit*>(Mevent->GetHit(i));
    double diff = hit->GetTime() - fRefTime ;
    if(fabs(diff)<OccCut) hitcount++;
  }
  fOccupancy[kL0MUV3]  = hitcount;

  ///////////////////////////////////////////////////////////////
  //// NewCHOD (equivilent to MUV3 candidates...)
  ///////////////////////////////////////////////////////////////
  hitcount =0;
  fUTMCHits=0;
  fNCOccTS =0;
  fNCOcc100=0;

  UInt_t TimeStamp = GetEventHeader()->GetTimeStamp();
  UInt_t ETimeSplit = TimeStamp/4; // time split of trigger (100ns)

  TRecoNewCHODEvent* NCevent = GetEvent<TRecoNewCHODEvent>();
  for( int i = 0 ; i < NCevent->GetNHits() ; ++i){
    TRecoNewCHODHit* hit = static_cast<TRecoNewCHODHit*>(NCevent->GetHit(i));
    double diff = hit->GetTime() - fRefTime ;
    if(fabs(diff)<10.0){
      if(hit->GetType()==0) fUTMCHits++; // only TIGHT for UTMC
      if(fabs(diff)<OccCut) hitcount++;
    }
    Double_t T0Corr = 0.;
    auto channelT0 = fNewCHODT0Map.find(hit->GetChannelID());
    if(channelT0!=fNewCHODT0Map.end()) T0Corr = channelT0->second;
    Double_t fulltime = hit->GetTime()+T0Corr+TimeStamp*ClockPeriod;
    if(fulltime<0){
      std::cout << user_normal() << " FullTime is negative. This should be impossible " << std::endl;
    }
    else{
      UInt_t ifulltime = fulltime/ClockPeriod;
      if(ifulltime==TimeStamp) fNCOccTS++;

      ifulltime /= 4;
      if(ifulltime==ETimeSplit) fNCOcc100++;
    }
  }
  fOccupancy[kL0NewCHOD] = hitcount;

  ///////////////////////////////////////////////////////////////
  //// TALK
  ///////////////////////////////////////////////////////////////
  hitcount=0;
  fOccupancy[kL0TALK]=0;

  ///////////////////////////////////////////////////////////////
  //// L0Calo (use number of clusters)
  ///////////////////////////////////////////////////////////////
  hitcount=0;
  TRecoLKrEvent* CALevent = static_cast<TRecoLKrEvent*>(GetEvent("LKr"));
  for( int i = 0 ; i < CALevent->GetNCandidates() ; ++i){
    TRecoLKrCandidate* hit = static_cast<TRecoLKrCandidate*>(CALevent->GetCandidate(i));
    double diff = hit->GetTime() - fRefTime ;
    if(fabs(diff)<OccCut) hitcount++;
  }
  fOccupancy[kL0Calo] = hitcount;
}

void L0TriggerResponse::FillOccupancyPlots(Int_t Type){

  Int_t bit = 14;

  UInt_t Trigger=0;
  Bool_t pass=false;

  if(Type){
    Trigger = (PrimInfo[kL0CHOD]).PrimitiveID_N3;
    pass = (Trigger>>bit)&1;
    FillHisto("CHOD_DenPerOccupancy", fOccupancy[kL0CHOD]);
    if(pass) FillHisto("CHOD_NumPerOccupancy", fOccupancy[kL0CHOD]);
  }
  else{
    Trigger = (PrimInfo[kL0RICH]).PrimitiveID_N3;
    pass = (Trigger>>bit)&1;
    FillHisto("RICH_DenPerOccupancy", fOccupancy[kL0RICH]);
    if(pass) FillHisto("RICH_NumPerOccupancy", fOccupancy[kL0RICH]);

    Trigger = (PrimInfo[kL0LAV]).PrimitiveID_N3;
    pass = (Trigger>>bit)&1;
    FillHisto("LAV12_DenPerOccupancy", fOccupancy[kL0LAV]);
    if(pass) FillHisto("LAV12_NumPerOccupancy", fOccupancy[kL0LAV]);

    Trigger = (PrimInfo[kL0MUV3]).PrimitiveID_N3;
    pass = (Trigger>>bit)&1;
    FillHisto("MUV3_DenPerOccupancy", fOccupancy[kL0MUV3]);
    if(pass) FillHisto("MUV3_NumPerOccupancy", fOccupancy[kL0MUV3]);

    Trigger = (PrimInfo[kL0NewCHOD]).PrimitiveID_N3;
    pass = (Trigger>>bit)&1;
    FillHisto("NewCHOD_DenPerOccupancy", fOccupancy[kL0NewCHOD]);
    if(pass) FillHisto("NewCHOD_NumPerOccupancy", fOccupancy[kL0NewCHOD]);

    if(fRef>=64 && fRef<192){
      // only for triggers centered in timeslot
      Trigger = (PrimInfo[kL0NewCHOD]).PrimitiveID_N3;
      pass = (Trigger>>bit)&1;
      FillHisto("NewCHOD_DenPerTimeslot", fNCOccTS);
      if(pass) FillHisto("NewCHOD_NumPerTimeslot", fNCOccTS);

      FillHisto("NewCHOD_DenPerTimesplit", fNCOcc100);
      if(pass) FillHisto("NewCHOD_NumPerTimesplit", fNCOcc100);
    }

    Trigger = (PrimInfo[kL0Calo]).PrimitiveID_N3;
    pass = (Trigger>>bit)&1;
    FillHisto("Calo_DenPerOccupancy", fOccupancy[kL0Calo]);
    if(pass) FillHisto("Calo_NumPerOccupancy", fOccupancy[kL0Calo]);
  }
}
