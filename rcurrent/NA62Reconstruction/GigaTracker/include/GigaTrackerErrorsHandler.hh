#ifndef GigaTrackerErrorsHandler_HH_
#define GigaTrackerErrorsHandler_HH_
#include <map>
#include <TH2F.h>
#include "TGigaTrackerDigiEvent.hh"

#define REPORT_NEW_ERROR(i,msg) do{if(0!=(i)){char fmsg[3000];snprintf(fmsg,3000,"Function: %s at line %d of file %s: %s\n",__func__, __LINE__,__FILE__, (msg));GTK::GigaTrackerErrorsHandler::GetInstance()->ReportNewError(i,fmsg);}}while(0)

#define REPORT_ERROR(i,msg) do{if(0!=(i)){char fmsg[3000];snprintf(fmsg,3000,"Function: %s at line %d of file %s: %s\n",__func__, __LINE__,__FILE__, (msg));GTK::GigaTrackerErrorsHandler::GetInstance()->ReportError(i,fmsg);}}while(0)

#define REPORT_NEW_ERROR_RETURN(i,msg) do{if(0!=(i)){char fmsg[3000];snprintf(fmsg,3000,"Function: %s at line %d of file %s: %s\n",__func__, __LINE__,__FILE__, (msg));GTK::GigaTrackerErrorsHandler::GetInstance()->ReportNewError(i,fmsg);return(i);}}while(0)

#define REPORT_ERROR_RETURN(i,msg) do{if(0!=(i)){char fmsg[3000];snprintf(fmsg,3000,"Function: %s at line %d of file %s: %s\n",__func__, __LINE__,__FILE__, (msg));GTK::GigaTrackerErrorsHandler::GetInstance()->ReportError(i,fmsg);return(i);}}while(0)


namespace GTK
{

  class GigaTrackerErrorsHandler
  {
  public:
    static GigaTrackerErrorsHandler* GetInstance();

    void Reset();
    void SetErrorHisto(TH2F* h);
    void SetOutputLevel(int level);
    int  GetNErrors();
    int  GetNChipErrors();
    int  GetNError(int chip, const char* error);

    int  RegisterError(const char*);
    void RegisterChipsError(const char*, int iout[30]);
    int  RegisterWarning(const char*);
    void RegisterChipsWarning(const char*, int iout[30]);

    void ReportError(int i,char* msg);
    void ReportNewError(int i,char* msg);

    void PrintSummaryTable(const char* title);
    void FillErrorHist(TH2F* hist);
    int GetNbFatalErrors();

    std::string GetError(int);

    int SetEvent(TGigaTrackerDigiEvent* e);

  private:
    GigaTrackerErrorsHandler( );
    ~GigaTrackerErrorsHandler();

    static GigaTrackerErrorsHandler * fInstance;
    int fOutputLevel;

    std::map<const char*, int> fMErrorLabels;
    std::map<const char*, int*> fMChipErrorLabels;

    std::map<int, int> fmNbErrors;

    TGigaTrackerDigiEvent* fDigiEvent;
    int fErrorIndex;
    int fChipErrorIndex;
    int fWarningIndex;
    int fChipWarningIndex;

    int fMaxPrintout;

    TH2F* fErrorHist;

  protected:
  };

}//~namespace IImaS

#endif//~GigaTrackerErrorsHandler_HH_

