#include "StringInterpreter.hh"

#include "TObjArray.h"
#include "TObjString.h"

TString StringInterpreter(TString InputString){
  TString OutputString = InputString;
  if(InputString.Contains("$")) { // env variable
    TObjArray * subline = InputString.Tokenize("/");
    for(Int_t iDir=0;iDir<subline->GetEntries();iDir++){
      TString DirString = ((TObjString*)(subline->At(iDir)))->GetString();
      if(DirString.Contains("$")) { // convert the string into the env variable
        TString varName = ((TString)((TString)((TString)DirString.Strip(TString::kLeading,'$')).Strip(TString::kLeading,'{')).Strip(TString::kTrailing,'}'));
        DirString = std::getenv(varName.Data());
      }
      if(!iDir) OutputString = DirString;
      else OutputString = OutputString+"/"+DirString;
    }
    delete subline;
  }
  return OutputString;
}
