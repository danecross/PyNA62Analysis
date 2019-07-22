#include <stdlib.h>
#include <iostream>
#include <TChain.h>
#include "CrocusSwapsFinder.hh"
#include "MCSimple.hh"
#include "functions.hh"
#include "Event.hh"
#include "Persistency.hh"
#include "NA62ConditionsService.hh"
using namespace std;
using namespace NA62Analysis;
using namespace NA62Constants;

/// \class CrocusSwapsFinder
/// \Brief
//	Check for swaps in LKr using a list of reconstructed burst running with --histo option
/// \EndBrief
CrocusSwapsFinder::CrocusSwapsFinder(Core::BaseAnalysis *ba) : Analyzer(ba, "CrocusSwapsFinder")
{
  RequestTree("LKr",new TRecoLKrEvent);
}

void CrocusSwapsFinder::InitOutput(){
}

void CrocusSwapsFinder::InitHist(){
  fNHitsSwapWithCrocus    = static_cast<TH3F*> (RequestHistogram("LKrMonitor","hNHitsSwapWithCrocus"   ,true));
  fNHitsRefSwapWithCrocus = static_cast<TH3F*> (RequestHistogram("LKrMonitor","hNHitsRefSwapWithCrocus",true));
}

void CrocusSwapsFinder::DefineMCSimple(){
}

void CrocusSwapsFinder::StartOfRunUser(){
  fSwapsFoundWithCrocus.clear();
  fFirstRunID = GetRunID();
  fFirstBurstID = GetBurstID();
}

void CrocusSwapsFinder::StartOfBurstUser(){
}

void CrocusSwapsFinder::ProcessSpecialTriggerUser(int, unsigned int){
}

void CrocusSwapsFinder::Process(int iEvent){
	if(fMCSimple.fStatus == MCSimple::kMissing){printIncompleteMCWarning(iEvent);return;}
}

void CrocusSwapsFinder::PostProcess(){

}

void CrocusSwapsFinder::EndOfBurstUser(){
}

void CrocusSwapsFinder::EndOfRunUser(){

}

void CrocusSwapsFinder::EndOfJobUser(){
  fLastRunID = GetRunID();
  fLastBurstID = GetBurstID();
  ReadReferenceFile();

  // ---------------- Crocus additional file ---------------- //
  ofstream CrocusAdditionalOutputFile; //additional output file with some more info
  CrocusAdditionalOutputFile.open(Form("LKr-CrocusInfo.run%06d_%04d-run%06d_%04d.dat",fFirstRunID,fFirstBurstID,fLastRunID,fLastBurstID));
  CrocusAdditionalOutputFile << "Cr Sl   Pairs   A-All  A_Good  A_Swap   B_All Chi2B_Good Chi2B_Swap SWAP TESTED" << std::endl;
  // -------------------------------------------------------- //
  //  
  DetectSwapsWithCrocus(CrocusAdditionalOutputFile);
  SaveAllPlots();
}

void CrocusSwapsFinder::DrawPlot(){
}

CrocusSwapsFinder::~CrocusSwapsFinder(){
}

void CrocusSwapsFinder::DetectSwapsWithCrocus(std::ofstream& AdditionalOutputFile){
  //swap finder with Crocus (Algo by Alan Norton, Coding Jurgen Engelfried)
  Int_t NCreamSeen = 0;
  fMaxCrateID = 31;
  fMaxSlotID = 20; //slot numbers arrive up to 20
  for(UInt_t iCrate=0;iCrate<fMaxCrateID+1;iCrate++) {
//    if(fCrateRemap[iCrate]<0) continue;
    for(UInt_t iSlot=0;iSlot<fMaxSlotID+1;iSlot++) {
//      if(fSlotRemap[iSlot]<0) continue;
      if(iCrate== 4 &&  5<=iSlot && iSlot<= 8) continue; // Slots not instrumented
      if(iCrate== 7 && (iSlot==13 || iSlot==14 || iSlot==19 || iSlot==20)) continue; // Slots not instrumented
      if(iCrate==24 && (iSlot== 3 || iSlot== 4 || iSlot== 9 || iSlot==10)) continue; // Slots not instrumented
      if(iCrate==27 && 15<=iSlot && iSlot<=18) continue; // Slots not instrumented
      Int_t Nseen  = 0;
      Int_t Npairs_all = 0;
      Int_t NtestA_all = 0;
      Int_t NtestA_good = 0;
      Int_t NtestA_swap = 0;
      Int_t NtestB_all = 0;
      Double_t ChisqB_good = 0.; 
      Double_t ChisqB_swap = 0.;     
      Int_t Itest,Iswap;
      for (Int_t jx=0;jx<=15;jx++) {
        Int_t kx = jx + 16;
        Iswap = 0;
        Itest = 0;
        if (fNHitsRefSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,jx+1)>=5 || fNHitsRefSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,kx+1)>=5) {
          Nseen++;
          Npairs_all++;
          //Int_t Nerr     = 0;
          Double_t diff_sig = 0.;
          Double_t dsq_good = 0.;
          Double_t dsq_swap = 0.;
          if (fNHitsSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,jx+1)<5 && fNHitsSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,kx)<5) {
            //Nerr = 1;
          }
          else if (fNHitsRefSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,jx+1)<5 || fNHitsRefSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,kx+1)<5) { 
            if (fNHitsSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,jx+1)>=5 && fNHitsSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,kx+1)>=5) {
              //Nerr  = 2;
            }
            else {
              Itest = 1;
              if ( (fNHitsRefSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,jx+1)<5 && fNHitsSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,kx+1)<5) ||  (fNHitsRefSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,kx+1)<5 && fNHitsSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,jx+1)<5) ) Iswap = 1;
            }
          }
          else {          
            Double_t dsum_ref  = fNHitsRefSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,jx+1) + fNHitsRefSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,kx+1);
            Double_t diff_ref  = fNHitsRefSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,jx+1) - fNHitsRefSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,kx+1);
            Double_t dsum      = fNHitsSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,jx+1) + fNHitsSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,kx+1);
            Double_t diff      = fNHitsSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,jx+1) - fNHitsSwapWithCrocus->GetBinContent(iCrate+1,iSlot+1,kx+1);
            diff_sig  = diff_ref/sqrt(dsum_ref);
            Double_t diff_rnor = diff_ref * (dsum/dsum_ref);
            dsq_good  = (diff-diff_rnor)*(diff-diff_rnor) / dsum;
            dsq_swap  = (diff+diff_rnor)*(diff+diff_rnor) / dsum;
            Iswap = -1;
            if ( fabs(diff_sig) > 16. ) {
              if (dsq_good< 4. && dsq_swap>25. ) Iswap = 0;
              if (dsq_good<10. && dsq_swap>50. ) Iswap = 0;
              if (dsq_good>25. && dsq_swap< 4. ) Iswap = 1;
              if (dsq_good>50. && dsq_swap<10. ) Iswap = 1;
              if (Iswap>=0 ) Itest = 2;
            }

            if (Iswap==-1 && fabs(diff_sig)>2.5 && (dsq_good<10. || dsq_swap<10.) ) {
              Itest = 3;
              NtestB_all ++;
              ChisqB_good += dsq_good;
              ChisqB_swap += dsq_swap;
            }             
          }         
          if (Itest>0 && Itest<3) {
            if (Iswap>=0) NtestA_all++;
            if (Iswap==0) NtestA_good++;
            if (Iswap>0) NtestA_swap++;
          }

        }
      } // loop over channels
      if ( Itest==3 && NtestB_all>0 ) {
        ChisqB_good = ChisqB_good / NtestB_all;
        ChisqB_swap = ChisqB_swap / NtestB_all;
      }
      if ( Nseen>0 ) NCreamSeen++;
      Bool_t Tested = false;
      Bool_t Swapped  = false;
      if ( Npairs_all>0 ) {
        Int_t NA      = NtestA_all;
        Int_t NB      = NtestB_all;
        Tested  = (NA>1)||(NB>3);

        Double_t DChisqB = ChisqB_good - ChisqB_swap;
        Double_t FChisqB = DChisqB/TMath::Max(ChisqB_swap,3.);
        if (Tested) {
          if ( NA==0 ) Swapped = (FChisqB>1.) && (NB>5);
          if ( NA==1 ) Swapped = (DChisqB>-1.) && (NtestA_swap==1);
          if ( NA>1  ) Swapped = NtestA_swap>=(NA-NA/4);
        }
        Char_t IF_TEST[12];
        if (!Tested) sprintf(IF_TEST," NO TEST");
        else sprintf(IF_TEST,"        ");
        Char_t IF_SWAP[8];
        if (Tested && Swapped) sprintf(IF_SWAP," YES");
        else sprintf(IF_SWAP,"    ");
        if (Swapped) {
          // ---------------- Crocus additional file ---------------- //
          AdditionalOutputFile << Form("%2d %2d %7d %7d %7d %7d %7d %8.2f %8.2f   %4s  %8s\n",iCrate,iSlot,Npairs_all,NA,NtestA_good,NtestA_swap,NB,ChisqB_good,ChisqB_swap,IF_SWAP,IF_TEST);
          // -------------------------------------------------------- //
        }
        if ( Tested  ) fNCreamsCheckedWithCrocus++;
        if ( Swapped ) fSwapsFoundWithCrocus.push_back(std::make_pair(iCrate, iSlot));
      }
    }
  }
}

void CrocusSwapsFinder::ReadReferenceFile(){

  //Open and read the Crocus reference file
  TString Prefix = "LKr";
  fCrocusReferenceFileName = Form("%s-CrocusRef.dat",Prefix.Data()); //default
  TString Line;
  if (NA62ConditionsService::GetInstance()->Open(fCrocusReferenceFileName)==kSuccess) {
    Bool_t FirstLine = true;
    Int_t NCrocusRefFound = 0;
    while (Line.ReadLine(NA62ConditionsService::GetInstance()->Get(fCrocusReferenceFileName))) {
      if(FirstLine){
        FirstLine = false;
        if (!Line.BeginsWith("CROCUS")) {
          std::cerr << "[CREAMRawDecoder] WARNING: Crocus reference file " << fCrocusReferenceFileName << " has wrong format" << std::endl;
          break;
        }
        else continue;
      }
      if (Line.BeginsWith("#")) continue;
      if (Line.BeginsWith("*")) continue;
      if (Line.Length()<5) continue;
      if (Line.Contains("CROCEND")) break;
      Int_t ipos = 0;
      if (Line.Contains("Cream")) ipos = 1;
      TObjArray *l = Line.Tokenize(" ");
      Int_t Unit = static_cast<TObjString*>(l->At(ipos+1))->GetString().Atoi();
      Int_t iCrate = Unit / 100;
      Int_t iSlot = Unit%100;
      Int_t jx = static_cast<TObjString*>(l->At(ipos+2))->GetString().Atoi();
      Int_t kx = static_cast<TObjString*>(l->At(ipos+3))->GetString().Atoi();
      //printf("Unit %d iCrate %d iSlot %d jx %d kx %d\n",Unit,iCrate,iSlot,jx,kx);
      fNHitsRefSwapWithCrocus->Fill(iCrate,iSlot,jx,static_cast<TObjString*>(l->At(ipos+6))->GetString().Atoi());
      fNHitsRefSwapWithCrocus->Fill(iCrate,iSlot,kx,static_cast<TObjString*>(l->At(ipos+7))->GetString().Atoi());
      NCrocusRefFound++;
      delete l;
    }
    NA62ConditionsService::GetInstance()->Close(fCrocusReferenceFileName);
    std::cout << "[CREAMRawDecoder] Found Reference values for " << NCrocusRefFound << " Channels in file " << fCrocusReferenceFileName << std::endl;
  }
  else  std::cerr << "[CREAMRawDecoder] WARNING: Crocus Reference file " << fCrocusReferenceFileName << " not found " << std::endl;
}

