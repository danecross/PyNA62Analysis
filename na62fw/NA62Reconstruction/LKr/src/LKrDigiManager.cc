#include "LKrDigiManager.hh"
#include "FADCEvent.hh"
#include "TLKrDigi.hh"
#include "LKrParameters.hh"
#include "LKrGeometry.hh"
#include "LKrCommon.hh"
#include <CLHEP/Units/PhysicalConstants.h>
using namespace CLHEP;
//#define SLM 1
#define digfilter digfilter_
#define pulseped pulseped_
#define pulsemax pulsemax_
#define pulserec pulserec_
#define readinit readinit_
#define type_of_call
extern "C"
{
  void type_of_call digfilter(int&,int&,int&,int&,float*,float&,float&,float&);
  void type_of_call pulseped(int&,double*,double*,int&);
  void type_of_call pulsemax(int&,double*,double*,int&,int&,double&,int&,int*);
  void type_of_call pulserec(int&,int&,int&,double*,double*,int&,int&,double&,float&,float&,int&,int&,int&);
  void type_of_call readinit(); 

  // Common
  void* lkr_common_address(const char*);
}
Double_t DeadCellADC[32];
Double_t DeadCellEnergy[32];


LKrDigiManager::LKrDigiManager() :
  fFADCEventADC(nullptr),
  fEnergySamples(nullptr),
  fEnergyPeak(0),
  fPedRef(.0)
{ 
  /// \MemberDescr 
  /// Constructor.
  /// \EndMemberDescr

  fFADCEventEnergy = new FADCEvent(TLKrDigi::Class(),15000); 
  fPar = LKrParameters::GetInstance();
  fGeo = LKrGeometry::GetInstance();
  fCom = LKrCommon::GetInstance();

  fNSampPed = fPar->GetNSamplePedestals();
  fNSampNoise = fPar->GetNSampleNoise();
  fSampTLow = fPar->GetSampleTimeLow();
  fSampTHigh = fPar->GetSampleTimeHigh();
  fSeedECut1 = fPar->GetSeedEnergyCut();
  fSeedECut2 = fPar->GetSeedECutRatio()*fPar->GetSeedEnergyCut();

  for (Int_t i=0; i<32; i++) {
    DeadCellADC[i] = 400.;
    DeadCellEnergy[i] = 0.;
  }
}
LKrDigiManager::~LKrDigiManager(){ 
  if(fFADCEventEnergy) delete fFADCEventEnergy;
}

void LKrDigiManager::Reset()
{

  /// \MemberDescr 
  /// Reset the output class.
  /// \EndMemberDescr

  fFADCEventEnergy->Clear("C");
}

FADCEvent* LKrDigiManager::ADCtoEnergy(){

  /// \MemberDescr 
  /// First rough ADC counts to Energy conversion.
  /// \EndMemberDescr

  // Reset Energy class
  fFADCEventEnergy->Clear("C");

  // Pointer to the ADC class 
  Int_t nHits = fFADCEventADC->GetNHits();                        // Number of hits in the ADC class
  Int_t nSamples = fFADCEventADC->GetNSamples();                   // Number of samples in the ADC class
  TClonesArray &cellsADC = (*(fFADCEventADC->GetHits()));         // Pointer to the hits in the adc class

  // Initialize Energy class
  fFADCEventEnergy->SetNSamples(nSamples);
  fFADCEventEnergy->SetFADCID(11);                                // ID of the classes which stores the energy info

  // Loop over hits
  for (Int_t icell=0; icell<nHits; icell++){
    ResetCell(); // Reset cell variables common to the LKrDigiManager class
    TLKrDigi *cellADC = static_cast<TLKrDigi *>(cellsADC[icell]);                       // Pointer to the FADC hit in ADC counts
    TLKrDigi *cellEnergy = static_cast<TLKrDigi *>(fFADCEventEnergy->AddDigi    (cellADC->GetChannelID()));  // Pointer to the FADC hit in Energy (to be filled)
    cellEnergy->DecodeChannelID();
    Int_t ix = cellADC->GetXCellID();
    Int_t iy = cellADC->GetYCellID();
    Int_t iFlags = cellADC->GetFlags();
    iFlags |= CheckPed(cellADC); // Check pedestals for a given cell      
    //cellEnergy->SetPedestal(fPedRef);
    cellEnergy->SetADCPeakEnergy(fPedRef); // ADCPeakEnergy not used for cellEnergy, used for real pedestal propagation
    cellEnergy->SetFlags(iFlags);      
    if (iFlags) continue;
    CheckCal(ix,iy); // Check calibration constant for a given cell

    // ADC -> energy conversion for each sample
    Double_t* ADCSamples = cellADC->GetAllSamples(); // Pointer to the array containing the adc samples of the FADC hit
    for (Int_t iSample=0; iSample<nSamples; iSample++){
      Int_t count = (Int_t)ADCSamples[iSample];
      Int_t igain = 0;
#ifdef SLM
      if (count>10000){
        igain = (Int_t)(0.0001*count);
        count -= 10000*igain;
      }
#endif
      cellEnergy->AddSample(fSteig[igain]*(count-fOffset[igain]));
#ifdef SLM
      if (fBadGain[igain]){
        iFlags|=(1<<kCREAMNoCalibrationBit);
        cellEnergy->SetFlags(iFlags);
      }
#else
      if (fBadGain[igain] == 0) 
        continue;      
      else {
        iFlags|=(1<<kCREAMNoCalibrationBit);
        cellEnergy->SetFlags(iFlags);
      }
#endif
    }
    if (iFlags) continue;
    fEnergySamples = cellEnergy->GetAllSamples(); 

    // Rough ADC -> energy conversion for the peak
    Int_t countpeak = (Int_t)cellADC->GetADCPeakEnergy();
    Int_t igainpeak = 0;
#ifdef SLM
    if (countpeak>10000){
      igainpeak = (Int_t)(0.0001*countpeak);
      countpeak -= fOffset[igainpeak];
      if (countpeak<0) countpeak = 0;
    }
#endif
    cellEnergy->SetPeakEnergy(fSteig[igainpeak]*countpeak);

    // Energy from digital filter
    if (!fPar->GetDigiFilterPreProcess()) continue;
    Int_t maxsample = (Int_t)(cellADC->GetADCPeakTime()+0.5);
    int icase = cellADC->GetQuality();
    if (maxsample>0 && maxsample<nSamples-1) {
      float phase0 = 0.;
      float energy = 0.;
      float time = 0.;
      float pulse[4];
      for ( Int_t j=0; j<4; j++)  {
        if (maxsample+(-1+j)>=0 && maxsample+(-1+j)<nSamples) {
          pulse[j] = (float)fEnergySamples[maxsample+(-1+j)];
        }
        else pulse[j] = 0;
      }
      digfilter(ix,iy,igainpeak,icase,pulse,energy,time,phase0);
      Double_t globalTime = ClockPeriod*(double)maxsample+(double)time; 
      cellEnergy->SetPeakEnergy((double)energy);
      cellEnergy->SetPeakTime(globalTime);
      cellEnergy->SetGain(igainpeak);
      //copy info also in cellADC digis
      cellADC->SetPeakEnergy((double)energy);
      cellADC->SetPeakTime(globalTime);
    } else {
      cellEnergy->SetPeakEnergy(0.);
      cellEnergy->SetPeakTime(-99999.);
      cellEnergy->SetGain(0);
      //copy info also in cellADC digis
      cellADC->SetPeakEnergy(0.);
      cellADC->SetPeakTime(-99999.);
    }

  }
  return fFADCEventEnergy; 
}

Int_t LKrDigiManager::CalRead(){
  ResetCommon();
  // Dummy vectors for dead cells
  bool dead_cell_added[128][128];
  for (Int_t ix=0;ix<128;ix++) {for (Int_t iy=0;iy<128;iy++) dead_cell_added[ix][iy]=false;}
  Int_t nHits = fFADCEventEnergy->GetNHits(); 
  Int_t nSample = fFADCEventEnergy->GetNSamples(); 
  if (nHits<=0) return -2;
  if (nSample<=0) return -2;
  if (nSample>20) nSample = 20;
  if (fFADCEventEnergy->GetFADCID()!=11) return -2;
  if (fFADCEventADC->GetFADCID()!=10) return -2;
  if (nHits>14000) return -3;
  Int_t nHitsEnergy = nHits;
  Int_t nHitsADC = fFADCEventADC->GetNHits();
  if (nHitsEnergy!=nHitsADC) return -2;

  // Loop over hits
  Int_t jgoodcell = -1; // PAY ATTENTION !!!
  Int_t iflaglkr = 0;
  Int_t ndead = 0;
  TClonesArray &cellsADC = (*(fFADCEventADC->GetHits()));
  TClonesArray &cellsEnergy = (*(fFADCEventEnergy->GetHits()));
  Int_t nHigh = 0;
  fCom->GetCAD()->NSAMPL = nSample;
  for (Int_t icell=0; icell<nHits; icell++)
  {
    TLKrDigi *cellADC = static_cast<TLKrDigi *>(cellsADC[icell]);
    TLKrDigi *cellEnergy = static_cast<TLKrDigi *>(cellsEnergy[icell]);
    Int_t ix = cellADC->GetXCellID();
    Int_t iy = cellADC->GetYCellID();
    if (ix<0 || ix>=128) continue; 
    if (iy<0 || iy>=128) continue;
    //    bool cond = (ix==74)&&(iy==67);
    //    if (cond) continue;
    if ((fPar->GetCellSta(ix,iy)>>12)&1) continue; // Not perimetral cells (perimetral = bit 12) 
    jgoodcell++;
    fCom->GetCAD()->ICLUS6[jgoodcell] = 0; 
    fCom->GetCAD()->ICLUS13[jgoodcell] = 0; 
    fCom->GetCAD()->EPULSE[jgoodcell] = 0; 
    fCom->GetCAD()->TPULSE[jgoodcell] = 0; 
    fCom->GetCAD()->IFLAG[jgoodcell] = 0; 
    fCom->GetCAD()->IXLKR[jgoodcell] = ix; 
    fCom->GetCAD()->IYLKR[jgoodcell] = iy; 
    iflaglkr = cellEnergy->GetFlags(); 
    fCom->GetCUN()->IPOINTLKR[iy][ix] = jgoodcell+1;
    fCom->GetCUN()->INDHIGH[iy][ix] = 0;

    // Dead cells
    fCom->GetCAD()->CALDEAD[jgoodcell] = 0;
    if ((iflaglkr>>kCREAMNoCalibrationBit)&1 || fPar->IsDeadCell(ix,iy)){
      fCom->GetCAD()->CALDEAD[jgoodcell] = 1;
      if (ndead<900){
        fCom->GetDEA()->XDEAD[ndead] = fCom->GetCUN()->XCELL_LKR[ix];
        fCom->GetDEA()->YDEAD[ndead] = fCom->GetCUN()->YCELL_LKR[iy];
        //if (ndead==365) std::cout << ix << " " << iy << " " << fCom->GetDEA()->XDEAD[ndead] << " " << fCom->GetDEA()->YDEAD[ndead] << std::endl;
        //if (ndead==366) std::cout << ix << " " << iy << " " << fCom->GetDEA()->XDEAD[ndead] << " " << fCom->GetDEA()->YDEAD[ndead] << std::endl;
        //if (ndead==367) std::cout << ix << " " << iy << " " << fCom->GetDEA()->XDEAD[ndead] << " " << fCom->GetDEA()->YDEAD[ndead] << std::endl;
      }
      ndead++;
    }
    fCom->GetDEA()->NDEAD = ndead;

    // Store the pulses (both in energy and adc)
    Double_t *adcSamples = cellADC->GetAllSamples();
    Double_t *energySamples = cellEnergy->GetAllSamples();
    fCom->GetCAD()->IBADC[jgoodcell] = (size_t)adcSamples;
    fCom->GetCAD()->IBENE[jgoodcell] = (size_t)energySamples;
    if (fCom->GetCAD()->CALDEAD[jgoodcell]) continue;   

    // Correction for undershoots
    pulseped(nSample,adcSamples,energySamples,fNSampPed); // warning it rewrites the contents of energy and adc samples
#ifdef SLM
    if (fNSampNoise==2) fCom->GetCAD()->EFIRST_SAMPLE[jgoodcell] = (adcSamples[0]<10000 && adcSamples[1]<10000) ? 0.5*(energySamples[0]+energySamples[1]) : 0.;
    else fCom->GetCAD()->EFIRST_SAMPLE[jgoodcell] = (adcSamples[0]<10000) ? energySamples[0] : 0.;
#else
    if (fNSampNoise==2) fCom->GetCAD()->EFIRST_SAMPLE[jgoodcell] = 0.5*(energySamples[0]+energySamples[1]);
    else fCom->GetCAD()->EFIRST_SAMPLE[jgoodcell] = energySamples[0];
#endif

    // Look for a candidate maximum among the samples
    Int_t itest = 0;
    for (Int_t iSamp=7; iSamp>=0; iSamp--) if ((itest=(energySamples[iSamp]>fSeedECut2))) break;
    if (!itest) continue;

    // Reconstruct the seed candidates and store then in the common cunCDE
    Int_t nMax = 0;
    Int_t itsMax[5];
    pulsemax(nSample,adcSamples,energySamples,fSampTLow,fSampTHigh,fSeedECut2,nMax,itsMax);
    for (Int_t imax = 0; imax<nMax; imax++)
    {
      double tguess = 0.;
      float energy;
      float time;
      int icase;
      int jgain;
      int itmax;
      pulserec(ix,iy,nSample,adcSamples,energySamples,itsMax[imax],itsMax[imax],tguess,energy,time,icase,jgain,itmax);
      if (energy<fSeedECut1) continue;
      if (nHigh>=250) return -1;
      fCom->GetCUN()->EHIGH[nHigh] = energy;
      fCom->GetCUN()->THIGH[nHigh] = time;
      fCom->GetCUN()->IXHIGH[nHigh] = ix;
      fCom->GetCUN()->IYHIGH[nHigh] = iy;
      fCom->GetCUN()->INDHIGH[iy][ix] = nHigh+1; // PAY ATTENTION +1 !
      fCom->GetCUN()->ITHIGH[nHigh] = itmax;
      fCom->GetCUN()->IUSED[nHigh] = 0;
      nHigh++;
      fCom->GetCUN()->NHIGH = nHigh;

      for (Int_t jx=max(0,ix-2); jx<min(127,ix+2); jx++) {
        for (Int_t jy=max(0,iy-2); jy<min(127,iy+2); jy++) {
          if (fPar->IsDeadCell(jx,jy)&&!dead_cell_added[jx][jy]) {
            dead_cell_added[jx][jy] =  true;
            jgoodcell++;
            fCom->GetCAD()->ICLUS6[jgoodcell] = 0; 
            fCom->GetCAD()->ICLUS13[jgoodcell] = 0; 
            fCom->GetCAD()->EPULSE[jgoodcell] = 0; 
            fCom->GetCAD()->TPULSE[jgoodcell] = 0; 
            fCom->GetCAD()->IFLAG[jgoodcell] = 0; 
            fCom->GetCAD()->IXLKR[jgoodcell] = jx; 
            fCom->GetCAD()->IYLKR[jgoodcell] = jy; 
            fCom->GetCUN()->IPOINTLKR[jy][jx] = jgoodcell+1;
            fCom->GetCUN()->INDHIGH[jy][jx] = 0;
            fCom->GetCAD()->CALDEAD[jgoodcell] = 1;
            if (ndead<900) {
              fCom->GetDEA()->XDEAD[ndead] = fCom->GetCUN()->XCELL_LKR[jx];
              fCom->GetDEA()->YDEAD[ndead] = fCom->GetCUN()->YCELL_LKR[jy];
            }
            ndead++;
            fCom->GetDEA()->NDEAD = ndead;

            // Store the dummy pulses (both in energy and adc)
            fCom->GetCAD()->IBADC[jgoodcell] = (size_t)(&DeadCellADC[0]);
            fCom->GetCAD()->IBENE[jgoodcell] = (size_t)(&DeadCellEnergy[0]);
          }
        }
      }
    }


  } 
  fCom->GetCAD()->CALNCEL = jgoodcell+1;   

  return 0;
}


Int_t LKrDigiManager::CheckPed(TLKrDigi * cellADC) // TO BE REDONE 
{

  /// \MemberDescr 
  /// Check of the pedestals in cell (ix,iy)
  /// \EndMemberDescr

  Int_t ix = cellADC->GetXCellID();
  Int_t iy = cellADC->GetYCellID();
  Double_t* ADCSamples = cellADC->GetAllSamples();

  Int_t iFlags = 0;
  Int_t istatPed = fPar->GetPedStat(ix,iy);
  Float_t sigmaPed = fPar->GetPedSigma(ix,iy);
#ifdef SLM
  switch (istatPed)
  {
    case 2: case 4:
      return (iFlags|=(1<<kCREAMNoCalibrationBit));
      break;
    case 0: case 1: case 3:
      fPedRef = fPar->GetPedRef(ix,iy);
      break;
    default:
      if (ADCSamples[0]<10000 && ADCSamples[1]<10000) fPedRef = 0.5*(ADCSamples[0]+ADCSamples[1]);
      else return (iFlags|=(1<<kCREAMNoCalibrationBit));
      break;
  }
#else
  if(istatPed == 0) {
    fPedRef = fPar->GetPedRef(ix,iy);
    Double_t pedmeas = 0.5*(ADCSamples[0]+ADCSamples[1]);
    Double_t dpedmeas = 0.5*fabs(ADCSamples[1]-ADCSamples[0]);
    if(pedmeas<=fPedRef-3.*sigmaPed && dpedmeas<3.*sigmaPed) fPedRef = pedmeas;  // use pedmeas for undershoots only (not for "overshoots")
  }
  //  if(istatPed == 0) {
  //    if(fabs(fPar->GetPedRef(ix,iy) - ADCSamples[0]) < 3*sigmaPed)
  //      fPedRef = fPar->GetPedRef(ix,iy);
  //    else 
  //      fPedRef = 0.5*(ADCSamples[0]+ADCSamples[1]);
  //  }
  else
    return (iFlags|=(1<<kCREAMNoCalibrationBit));

#endif
  return iFlags;
}

void LKrDigiManager::CheckCal(Int_t ix, Int_t iy){

  /// \MemberDescr 
  /// Check of the calibration constants for cell (ix,iy) 
  /// \EndMemberDescr
  Int_t istatCal = 99;
  istatCal = fPar->GetCalStat(ix,iy);
#ifdef SLM
  fBadGain[0] = istatCal&(0x1);
  fBadGain[1] = istatCal&(0x2);
  fBadGain[2] = istatCal&(0x4);
  fBadGain[3] = istatCal&(0x8);
#else
  if(istatCal == 10 || istatCal == 11) {
    fBadGain[0] = 1;
  }
  else {
    fBadGain[0] = 0;
  }
#endif
  for (Int_t ig=0; ig<4; ig++){
    fOffset[ig] = fPar->GetCalOffset(ix,iy,ig);
    fSteig[ig] = fPar->GetCalSteig(ix,iy,ig);
  }
  fOffset[0] = fPedRef;
}

void LKrDigiManager::ResetCell()
{
  fPedRef = 0;
  for (Int_t i=0; i<4; i++) 
  {
    fBadGain[i] = 0;
    fOffset[i] = 0;
    fSteig[i] = 0;
  }
}

void LKrDigiManager::ResetCommon()
{
  fCom->ResetEvent();
}
