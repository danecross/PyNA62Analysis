#include "GigaTrackerNa62HeaderBase.hh" 
#include "RG_Utilities.hh"
#include <iomanip>
#include <string.h> //memset
#include "GigaTrackerErrorsHandler.hh"

namespace GTK
{


  //  ==============================================  
  GigaTrackerNa62HeaderBase::GigaTrackerNa62HeaderBase()
  {

  }

  //  ==============================================  
  GigaTrackerNa62HeaderBase::~GigaTrackerNa62HeaderBase()
  {
   
  }

  //  ==============================================  
  void GigaTrackerNa62HeaderBase::Print(std::ostream & out){
    out<<"Nb Bytes:"<< GetByteNb()<<"\n";
    return;
  }

  std::ostream& operator<< (std::ostream &out, GigaTrackerNa62HeaderBase & h){
    h.Print(out);
    return out;
  }
}//~namespace IImaS


