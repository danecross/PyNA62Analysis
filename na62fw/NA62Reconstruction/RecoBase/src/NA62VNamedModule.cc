#include "NA62VNamedModule.hh"

NA62VNamedModule::NA62VNamedModule(TString name) : fName(name)
{
}

NA62VNamedModule * NA62VNamedModule::Find(TString Name){
    for(UInt_t iLib = 0; iLib < fLibrary.size(); iLib++)
        if(fLibrary[iLib]->GetName().CompareTo(Name) == 0)
            return fLibrary[iLib];
    return 0;
}
