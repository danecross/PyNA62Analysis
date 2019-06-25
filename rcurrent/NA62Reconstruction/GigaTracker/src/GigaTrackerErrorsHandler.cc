#include "GigaTrackerErrorsHandler.hh" 
#include <iostream>
#include "InfoWriter.h"
#include <iomanip>
#include <string>

#include "TDigiVError.hh"
#include "TDigiGigaTrackerError.hh"

namespace GTK
{
  //==============================================  
  GigaTrackerErrorsHandler::GigaTrackerErrorsHandler() :
    //0 nothing
    //1 first error and warning
    //2 all errors
    //3 all errors and warnings
    //
    // in NA62:
    // fWarningsLe
    //  0: no warnings
    //  1: general information event-related     WARN_EVT
    //  2: general information detector-related  WARN_DET
    //  3: maximum detail                        WARN_MAX
    fOutputLevel(3),
    fDigiEvent(nullptr),
    fErrorIndex (1),
    fChipErrorIndex (1000),
    fWarningIndex (-1),
    fChipWarningIndex (-1000),
    fMaxPrintout(1),
    fErrorHist(nullptr)
  {
  }

  //==============================================  
  GigaTrackerErrorsHandler::~GigaTrackerErrorsHandler()
  {  
  }

  //==============================================  
  GigaTrackerErrorsHandler* GigaTrackerErrorsHandler::fInstance = 0;

  GigaTrackerErrorsHandler* GigaTrackerErrorsHandler::GetInstance(){
    if(fInstance == 0){
    	fInstance = new GigaTrackerErrorsHandler();
    }
    return fInstance;
  }

  //==============================================  
  void GigaTrackerErrorsHandler::Reset(){
    std::map<int, int>::iterator it;    
    for ( it = fmNbErrors.begin(); it != fmNbErrors.end(); ++it ) it->second = 0;
    if(fErrorHist != NULL) fErrorHist->Reset();
  }


  //==============================================  
  void GigaTrackerErrorsHandler::SetErrorHisto(TH2F* hist){
    std::map<const char*, int>::iterator iErr = fMErrorLabels.begin();
    int binYIndex(1);
    //First fatal errors
    for(;iErr!=fMErrorLabels.end();++iErr){
      if(iErr->second <= 0) continue;
      hist->GetYaxis()->SetBinLabel(binYIndex, Form("#color[2]{%s}",iErr->first) );
      binYIndex++;
    }
    std::map<const char*, int*>::iterator iChipErr = fMChipErrorLabels.begin();
    for(;iChipErr!=fMChipErrorLabels.end();++iChipErr){
      if((iChipErr->second)[0] <= 0) continue;
      hist->GetYaxis()->SetBinLabel(binYIndex, Form("#color[2]{%s}",iChipErr->first) );
      binYIndex++;
    }

    //Then the warnings
    iErr = fMErrorLabels.begin();
    for(;iErr!=fMErrorLabels.end();++iErr){
      if(iErr->second>0) continue;
      hist->GetYaxis()->SetBinLabel(binYIndex, iErr->first );
      binYIndex++;
    }
    iChipErr = fMChipErrorLabels.begin();
    for(;iChipErr!=fMChipErrorLabels.end();++iChipErr){
      if((iChipErr->second)[0]>0) continue;
      hist->GetYaxis()->SetBinLabel(binYIndex, iChipErr->first );
      binYIndex++;
    }


    //xlabels
    hist->GetXaxis()->SetBinLabel(31, "G" );
    for (int i(1);i<31;++i){
      hist->GetXaxis()->SetBinLabel(i, Form("%d-%d",(i-1)/10+1,(i-1)%10));
    }


    fErrorHist = hist;
  }

  //==============================================  
  int GigaTrackerErrorsHandler::GetNbFatalErrors(){
    std::map<const char*, int>::iterator iErr = fMErrorLabels.begin();
    int nFatallError(0);

    for(;iErr!=fMErrorLabels.end();++iErr){
      if(iErr->second <= 0) continue;
      nFatallError++;
    }
    std::map<const char*, int*>::iterator iChipErr = fMChipErrorLabels.begin();
    for(;iChipErr!=fMChipErrorLabels.end();++iChipErr){
      if((iChipErr->second)[0] <= 0) continue;
      nFatallError++;
    }
    return nFatallError;
  }

  //==============================================  
  void GigaTrackerErrorsHandler::SetOutputLevel(int level){
    fOutputLevel = level;
  }


  //==============================================  
  int  GigaTrackerErrorsHandler::GetNErrors(){
    return fMErrorLabels.size();
  }

  //==============================================  
  int  GigaTrackerErrorsHandler::GetNChipErrors(){
    return fMChipErrorLabels.size();
  }

  //==============================================  
  int  GigaTrackerErrorsHandler::SetEvent(TGigaTrackerDigiEvent* e){
    fDigiEvent = e;
    return 0;
  }

  //==============================================  
  int GigaTrackerErrorsHandler::RegisterError(const char* label){
    int rv = fErrorIndex;
    std::map<const char*, int>::iterator it = fMErrorLabels.find(label);
    if (it == fMErrorLabels.end()) {
      fMErrorLabels.insert(std::pair<const char*,int>(label,fErrorIndex));
      fmNbErrors.insert(std::pair<int,int>(fErrorIndex,0));
      fErrorIndex++;
      return rv;
    }
    return (*it).second;
  }

  //==============================================  
  int GigaTrackerErrorsHandler::RegisterWarning(const char* label){
    int rv = fWarningIndex;
    std::map<const char*, int>::iterator it = fMErrorLabels.find(label);
    if (it == fMErrorLabels.end()) {
      fMErrorLabels.insert(std::pair<const char*,int>(label,fWarningIndex));
      fmNbErrors.insert(std::pair<int,int>(fWarningIndex,0));
      fWarningIndex--;
      return rv;
    }
    return (*it).second;
  }


  //==============================================  
  void GigaTrackerErrorsHandler::RegisterChipsError(const char* label, int iout[30]){
    //int rv = fErrorIndex;
    std::map<const char*, int*>::iterator it = fMChipErrorLabels.find(label);
    if (it == fMChipErrorLabels.end()) {
      for(int i(0);i<30;i++){
	iout[i] = fChipErrorIndex;
	fmNbErrors.insert(std::pair<int,int>(fChipErrorIndex,0));
	fChipErrorIndex++;
      }
      fMChipErrorLabels.insert(std::pair<const char*,int*>(label,iout));
      return;
    }
    for(int i(0);i<30;i++)    iout[i]=((*it).second)[i];
    return;
  }


  //==============================================  
  void GigaTrackerErrorsHandler::RegisterChipsWarning(const char* label, int iout[30]){
    //int rv = fErrorIndex;
    std::map<const char*, int*>::iterator it = fMChipErrorLabels.find(label);
    if (it == fMChipErrorLabels.end()) {
      for(int i(0);i<30;i++){
	iout[i] = fChipWarningIndex;
	fmNbErrors.insert(std::pair<int,int>(fChipWarningIndex,0));
	fChipWarningIndex--;
      }
      fMChipErrorLabels.insert(std::pair<const char*,int*>(label,iout));
      return;
    }
    for(int i(0);i<30;i++)    iout[i]=((*it).second)[i];
    return;
  }


  //==============================================  
  void GigaTrackerErrorsHandler::ReportNewError(int i, char* msg){

    //check the error exists
    std::string label = GetError(i);
    int yBin(1);
    for(;yBin <= fErrorHist->GetYaxis()->GetNbins()+1; yBin++){
      TString sBin = fErrorHist->GetYaxis()->GetBinLabel(yBin);
      if(sBin == Form("#color[2]{%s}",label.c_str()) || sBin == Form("%s",label.c_str()) ) break;
    }
    if(yBin > fErrorHist->GetYaxis()->GetNbins()) return;
    fmNbErrors[i]++;

    //Efind which chip
    int xE(0), yE(0);
    if(abs(i)<1000){
      xE = 30;
      yE = yBin;
    }
    else{
      xE = abs((i%1000)%30);
      yE = yBin;
    }
    fErrorHist->Fill(xE,yE);
    ReportError(i, msg);
    TDigiVError *Error = static_cast<TDigiVError*>(fDigiEvent->AddError(TDigiGigaTrackerError::GetErrorType(GetError(i))));
    if(i>0) Error->SetFatal(kTRUE);
  }

  //==============================================  
  void GigaTrackerErrorsHandler::ReportError(int i, char* msg){
    //check error exist
    if(  (i<1000 && i>fErrorIndex) || (i>=1000 && i>fChipErrorIndex) ) {
      std::cerr<<"Index "<<i<<" is not valid\n";
      return;
    }
    
    const int WarnNon=0;
    const int WarnEvt=WARN_EVT;
    const int WarnDet=WARN_DET;
    const int WarnMax=WARN_MAX;

    switch(fOutputLevel){
    case WarnNon:
      break;

    case WarnEvt:
      if( fmNbErrors[i] <= fMaxPrintout ) std::cerr<<"[ErrHandler] "<<std::setw(4)<<i<<": "<<msg<<" Occurrence: "<< fmNbErrors[i]<<std::endl;
      break;

    case WarnDet:
      if( fmNbErrors[i] <= fMaxPrintout ) std::cerr<<"[ErrHandler] "<<std::setw(4)<<i<<": "<<msg<<" Occurrence: "<< fmNbErrors[i]<<std::endl;
      break;

    case WarnMax:
      std::cerr<<"[ErrHandler] "<<i<<": "<<msg<<std::endl;
      break;

    }
  }

  //==============================================  
  void GigaTrackerErrorsHandler::PrintSummaryTable(const char* title){
    printf("*%-75.75s*\n",std::string(75,'*').c_str());
    printf("* %-73.73s *\n",title);
    printf("* %-3.3s * %-54.54s * %10.10s *\n","ID","Description","Counts");
    printf("*%-75.75s*\n",std::string(75,'-').c_str());
    std::map<const char*, int>::iterator it;
    for (it = fMErrorLabels.begin() ; it!= fMErrorLabels.end(); ++it) printf("* %3d * %-54.54s * %10d *\n",(*it).second, (*it).first, fmNbErrors[(*it).second]);

    printf("%s * \n",std::string(62+10*13,'-').c_str());
    printf("* %-3.3s * %-54.54s *%62.62s%s%60.60s *\n","ID","Description","","Counts","");
    printf("* %-3.3s * %-49.49s* GTK *","","");
    for(int i(0);i<10;i++) printf("    chip%d   *",i);
    printf("\n%s * \n", std::string(62+10*13,'-').c_str());

    std::map<const char*, int*>::iterator ic;
    for (ic = fMChipErrorLabels.begin() ; ic!= fMChipErrorLabels.end(); ++ic){
      printf("* %3d * %-52.52s %d ",((*ic).second)[0], (*ic).first,1);
      for(int c(0);c<30;c++) {
	printf("* %10d ",fmNbErrors[((*ic).second)[c]]);
	if(c==9 || c == 19) printf("*\n* %3d * %-52.52s %d ",((*ic).second)[c+1],"",c/10+2);
	if(c==29) printf("*\n");
      }
    }
    printf("*%-75.75s*\n",std::string(75,'*').c_str());
  }




  /*
 //==============================================  
void GigaTrackerErrorsHandler::FillErrorHist(TH2F* hist){

  //set the labels
  std::map<const char*, int>::iterator iErr = fMErrorLabels.begin();
  int binYIndex(1);
  for(;iErr!=fMErrorLabels.end();iErr++, binYIndex++){ 
    hist->GetYaxis()->SetBinLabel(binYIndex, iErr->first );
  }
  std::map<const char*, int*>::iterator iChipErr = fMChipErrorLabels.begin();
  for(;iChipErr!=fMChipErrorLabels.end();iChipErr++, binYIndex++){ 
    hist->GetYaxis()->SetBinLabel(binYIndex, iChipErr->first );
  }

  hist->GetXaxis()->SetBinLabel(1, "G" );
  for (int i(2);i<32;i++){

    hist->GetXaxis()->SetBinLabel(i, Form("%d-%d",(i-2)/10+1,(i-2)%10));
  }

  //fill histo
    std::map<const char*, int>::iterator it;
    for (it = fMErrorLabels.begin() ; it!= fMErrorLabels.end(); it++){
      hist->Fill(float(0),it->first,fmNbErrors[it->second]);
    }


    std::map<const char*, int*>::iterator ic;
    for (ic = fMChipErrorLabels.begin() ; ic!= fMChipErrorLabels.end(); ic++){
      for(int c(0);c<30;c++) {
	hist->Fill(c+1,ic->first,fmNbErrors[(ic->second)[c]]);
      }
    }

}*/

  //==============================================  
  std::string GigaTrackerErrorsHandler::GetError(int id){

    std::map<const char*, int>::iterator it;
    for(it=fMErrorLabels.begin();it!=fMErrorLabels.end();++it){
      if(it->second == id) return std::string(it->first);
    }

    std::map<const char*, int*>::iterator itc;
    for(itc=fMChipErrorLabels.begin();itc!=fMChipErrorLabels.end();++itc){
      for(int i(0);i<30;i++){
	if((itc->second)[i] == id) return std::string(itc->first);
      }
    }
    return std::string("");
    
  }


  int  GigaTrackerErrorsHandler::GetNError(int chip, const char* error){
    std::map<const char*, int*>::iterator it = fMChipErrorLabels.find(error);
    if (it == fMChipErrorLabels.end()) {
      std::cout<<"Error "<<error<<" does not exist"<<std::endl;
    }
    int index = (it->second)[chip];
    return fmNbErrors[index];
  }

}//~namespace IImaS
