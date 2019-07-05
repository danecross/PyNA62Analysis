// Do NOT change. Changes will be lost next time file is generated

#define R__DICTIONARY_FILENAME CHANTISlimPersistencyDICT

/*******************************************************************/
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#define G__DICTIONARY
#include "RConfig.h"
#include "TClass.h"
#include "TDictAttributeMap.h"
#include "TInterpreter.h"
#include "TROOT.h"
#include "TBuffer.h"
#include "TMemberInspector.h"
#include "TInterpreter.h"
#include "TVirtualMutex.h"
#include "TError.h"

#ifndef G__ROOT
#define G__ROOT
#endif

#include "RtypesImp.h"
#include "TIsAProxy.h"
#include "TFileMergeInfo.h"
#include <algorithm>
#include "TCollectionProxyInfo.h"
/*******************************************************************/

#include "TDataMember.h"

// Since CINT ignores the std namespace, we need to do so in this file.
namespace std {} using namespace std;

// Header files passed as explicit arguments
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/CHANTI/include/TSlimRecoCHANTICandidate.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/CHANTI/include/TSlimRecoCHANTIEvent.hh"
#include "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/CHANTI/include/TSlimRecoCHANTIHit.hh"

// Header files passed via #pragma extra_include

namespace ROOT {
   static void *new_TSlimRecoCHANTICandidate(void *p = 0);
   static void *newArray_TSlimRecoCHANTICandidate(Long_t size, void *p);
   static void delete_TSlimRecoCHANTICandidate(void *p);
   static void deleteArray_TSlimRecoCHANTICandidate(void *p);
   static void destruct_TSlimRecoCHANTICandidate(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoCHANTICandidate*)
   {
      ::TSlimRecoCHANTICandidate *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoCHANTICandidate >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoCHANTICandidate", ::TSlimRecoCHANTICandidate::Class_Version(), "", 16,
                  typeid(::TSlimRecoCHANTICandidate), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoCHANTICandidate::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoCHANTICandidate) );
      instance.SetNew(&new_TSlimRecoCHANTICandidate);
      instance.SetNewArray(&newArray_TSlimRecoCHANTICandidate);
      instance.SetDelete(&delete_TSlimRecoCHANTICandidate);
      instance.SetDeleteArray(&deleteArray_TSlimRecoCHANTICandidate);
      instance.SetDestructor(&destruct_TSlimRecoCHANTICandidate);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoCHANTICandidate*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoCHANTICandidate*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoCHANTICandidate*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoCHANTIHit(void *p = 0);
   static void *newArray_TSlimRecoCHANTIHit(Long_t size, void *p);
   static void delete_TSlimRecoCHANTIHit(void *p);
   static void deleteArray_TSlimRecoCHANTIHit(void *p);
   static void destruct_TSlimRecoCHANTIHit(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoCHANTIHit*)
   {
      ::TSlimRecoCHANTIHit *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoCHANTIHit >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoCHANTIHit", ::TSlimRecoCHANTIHit::Class_Version(), "TSlimRecoCHANTIHit.hh", 10,
                  typeid(::TSlimRecoCHANTIHit), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoCHANTIHit::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoCHANTIHit) );
      instance.SetNew(&new_TSlimRecoCHANTIHit);
      instance.SetNewArray(&newArray_TSlimRecoCHANTIHit);
      instance.SetDelete(&delete_TSlimRecoCHANTIHit);
      instance.SetDeleteArray(&deleteArray_TSlimRecoCHANTIHit);
      instance.SetDestructor(&destruct_TSlimRecoCHANTIHit);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoCHANTIHit*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoCHANTIHit*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoCHANTIHit*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

namespace ROOT {
   static void *new_TSlimRecoCHANTIEvent(void *p = 0);
   static void *newArray_TSlimRecoCHANTIEvent(Long_t size, void *p);
   static void delete_TSlimRecoCHANTIEvent(void *p);
   static void deleteArray_TSlimRecoCHANTIEvent(void *p);
   static void destruct_TSlimRecoCHANTIEvent(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const ::TSlimRecoCHANTIEvent*)
   {
      ::TSlimRecoCHANTIEvent *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TInstrumentedIsAProxy< ::TSlimRecoCHANTIEvent >(0);
      static ::ROOT::TGenericClassInfo 
         instance("TSlimRecoCHANTIEvent", ::TSlimRecoCHANTIEvent::Class_Version(), "", 70,
                  typeid(::TSlimRecoCHANTIEvent), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &::TSlimRecoCHANTIEvent::Dictionary, isa_proxy, 4,
                  sizeof(::TSlimRecoCHANTIEvent) );
      instance.SetNew(&new_TSlimRecoCHANTIEvent);
      instance.SetNewArray(&newArray_TSlimRecoCHANTIEvent);
      instance.SetDelete(&delete_TSlimRecoCHANTIEvent);
      instance.SetDeleteArray(&deleteArray_TSlimRecoCHANTIEvent);
      instance.SetDestructor(&destruct_TSlimRecoCHANTIEvent);
      return &instance;
   }
   TGenericClassInfo *GenerateInitInstance(const ::TSlimRecoCHANTIEvent*)
   {
      return GenerateInitInstanceLocal((::TSlimRecoCHANTIEvent*)0);
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const ::TSlimRecoCHANTIEvent*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));
} // end of namespace ROOT

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoCHANTICandidate::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoCHANTICandidate::Class_Name()
{
   return "TSlimRecoCHANTICandidate";
}

//______________________________________________________________________________
const char *TSlimRecoCHANTICandidate::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHANTICandidate*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoCHANTICandidate::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHANTICandidate*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoCHANTICandidate::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHANTICandidate*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoCHANTICandidate::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHANTICandidate*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoCHANTIHit::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoCHANTIHit::Class_Name()
{
   return "TSlimRecoCHANTIHit";
}

//______________________________________________________________________________
const char *TSlimRecoCHANTIHit::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHANTIHit*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoCHANTIHit::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHANTIHit*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoCHANTIHit::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHANTIHit*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoCHANTIHit::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHANTIHit*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
atomic_TClass_ptr TSlimRecoCHANTIEvent::fgIsA(0);  // static to hold class pointer

//______________________________________________________________________________
const char *TSlimRecoCHANTIEvent::Class_Name()
{
   return "TSlimRecoCHANTIEvent";
}

//______________________________________________________________________________
const char *TSlimRecoCHANTIEvent::ImplFileName()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHANTIEvent*)0x0)->GetImplFileName();
}

//______________________________________________________________________________
int TSlimRecoCHANTIEvent::ImplFileLine()
{
   return ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHANTIEvent*)0x0)->GetImplFileLine();
}

//______________________________________________________________________________
TClass *TSlimRecoCHANTIEvent::Dictionary()
{
   fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHANTIEvent*)0x0)->GetClass();
   return fgIsA;
}

//______________________________________________________________________________
TClass *TSlimRecoCHANTIEvent::Class()
{
   if (!fgIsA.load()) { R__LOCKGUARD(gInterpreterMutex); fgIsA = ::ROOT::GenerateInitInstanceLocal((const ::TSlimRecoCHANTIEvent*)0x0)->GetClass(); }
   return fgIsA;
}

//______________________________________________________________________________
void TSlimRecoCHANTICandidate::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoCHANTICandidate.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoCHANTICandidate::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoCHANTICandidate::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoCHANTICandidate(void *p) {
      return  p ? new(p) ::TSlimRecoCHANTICandidate : new ::TSlimRecoCHANTICandidate;
   }
   static void *newArray_TSlimRecoCHANTICandidate(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoCHANTICandidate[nElements] : new ::TSlimRecoCHANTICandidate[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoCHANTICandidate(void *p) {
      delete ((::TSlimRecoCHANTICandidate*)p);
   }
   static void deleteArray_TSlimRecoCHANTICandidate(void *p) {
      delete [] ((::TSlimRecoCHANTICandidate*)p);
   }
   static void destruct_TSlimRecoCHANTICandidate(void *p) {
      typedef ::TSlimRecoCHANTICandidate current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoCHANTICandidate

//______________________________________________________________________________
void TSlimRecoCHANTIHit::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoCHANTIHit.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoCHANTIHit::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoCHANTIHit::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoCHANTIHit(void *p) {
      return  p ? new(p) ::TSlimRecoCHANTIHit : new ::TSlimRecoCHANTIHit;
   }
   static void *newArray_TSlimRecoCHANTIHit(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoCHANTIHit[nElements] : new ::TSlimRecoCHANTIHit[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoCHANTIHit(void *p) {
      delete ((::TSlimRecoCHANTIHit*)p);
   }
   static void deleteArray_TSlimRecoCHANTIHit(void *p) {
      delete [] ((::TSlimRecoCHANTIHit*)p);
   }
   static void destruct_TSlimRecoCHANTIHit(void *p) {
      typedef ::TSlimRecoCHANTIHit current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoCHANTIHit

//______________________________________________________________________________
void TSlimRecoCHANTIEvent::Streamer(TBuffer &R__b)
{
   // Stream an object of class TSlimRecoCHANTIEvent.

   if (R__b.IsReading()) {
      R__b.ReadClassBuffer(TSlimRecoCHANTIEvent::Class(),this);
   } else {
      R__b.WriteClassBuffer(TSlimRecoCHANTIEvent::Class(),this);
   }
}

namespace ROOT {
   // Wrappers around operator new
   static void *new_TSlimRecoCHANTIEvent(void *p) {
      return  p ? new(p) ::TSlimRecoCHANTIEvent : new ::TSlimRecoCHANTIEvent;
   }
   static void *newArray_TSlimRecoCHANTIEvent(Long_t nElements, void *p) {
      return p ? new(p) ::TSlimRecoCHANTIEvent[nElements] : new ::TSlimRecoCHANTIEvent[nElements];
   }
   // Wrapper around operator delete
   static void delete_TSlimRecoCHANTIEvent(void *p) {
      delete ((::TSlimRecoCHANTIEvent*)p);
   }
   static void deleteArray_TSlimRecoCHANTIEvent(void *p) {
      delete [] ((::TSlimRecoCHANTIEvent*)p);
   }
   static void destruct_TSlimRecoCHANTIEvent(void *p) {
      typedef ::TSlimRecoCHANTIEvent current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class ::TSlimRecoCHANTIEvent

namespace ROOT {
   static TClass *vectorlEshortgR_Dictionary();
   static void vectorlEshortgR_TClassManip(TClass*);
   static void *new_vectorlEshortgR(void *p = 0);
   static void *newArray_vectorlEshortgR(Long_t size, void *p);
   static void delete_vectorlEshortgR(void *p);
   static void deleteArray_vectorlEshortgR(void *p);
   static void destruct_vectorlEshortgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<short>*)
   {
      vector<short> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<short>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<short>", -2, "vector", 216,
                  typeid(vector<short>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlEshortgR_Dictionary, isa_proxy, 0,
                  sizeof(vector<short>) );
      instance.SetNew(&new_vectorlEshortgR);
      instance.SetNewArray(&newArray_vectorlEshortgR);
      instance.SetDelete(&delete_vectorlEshortgR);
      instance.SetDeleteArray(&deleteArray_vectorlEshortgR);
      instance.SetDestructor(&destruct_vectorlEshortgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<short> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<short>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlEshortgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<short>*)0x0)->GetClass();
      vectorlEshortgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlEshortgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlEshortgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<short> : new vector<short>;
   }
   static void *newArray_vectorlEshortgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<short>[nElements] : new vector<short>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlEshortgR(void *p) {
      delete ((vector<short>*)p);
   }
   static void deleteArray_vectorlEshortgR(void *p) {
      delete [] ((vector<short>*)p);
   }
   static void destruct_vectorlEshortgR(void *p) {
      typedef vector<short> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<short>

namespace ROOT {
   static TClass *vectorlETSlimRecoCHANTIHitgR_Dictionary();
   static void vectorlETSlimRecoCHANTIHitgR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoCHANTIHitgR(void *p = 0);
   static void *newArray_vectorlETSlimRecoCHANTIHitgR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoCHANTIHitgR(void *p);
   static void deleteArray_vectorlETSlimRecoCHANTIHitgR(void *p);
   static void destruct_vectorlETSlimRecoCHANTIHitgR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoCHANTIHit>*)
   {
      vector<TSlimRecoCHANTIHit> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoCHANTIHit>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoCHANTIHit>", -2, "vector", 216,
                  typeid(vector<TSlimRecoCHANTIHit>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoCHANTIHitgR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoCHANTIHit>) );
      instance.SetNew(&new_vectorlETSlimRecoCHANTIHitgR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoCHANTIHitgR);
      instance.SetDelete(&delete_vectorlETSlimRecoCHANTIHitgR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoCHANTIHitgR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoCHANTIHitgR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoCHANTIHit> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoCHANTIHit>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoCHANTIHitgR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoCHANTIHit>*)0x0)->GetClass();
      vectorlETSlimRecoCHANTIHitgR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoCHANTIHitgR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoCHANTIHitgR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoCHANTIHit> : new vector<TSlimRecoCHANTIHit>;
   }
   static void *newArray_vectorlETSlimRecoCHANTIHitgR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoCHANTIHit>[nElements] : new vector<TSlimRecoCHANTIHit>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoCHANTIHitgR(void *p) {
      delete ((vector<TSlimRecoCHANTIHit>*)p);
   }
   static void deleteArray_vectorlETSlimRecoCHANTIHitgR(void *p) {
      delete [] ((vector<TSlimRecoCHANTIHit>*)p);
   }
   static void destruct_vectorlETSlimRecoCHANTIHitgR(void *p) {
      typedef vector<TSlimRecoCHANTIHit> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoCHANTIHit>

namespace ROOT {
   static TClass *vectorlETSlimRecoCHANTICandidategR_Dictionary();
   static void vectorlETSlimRecoCHANTICandidategR_TClassManip(TClass*);
   static void *new_vectorlETSlimRecoCHANTICandidategR(void *p = 0);
   static void *newArray_vectorlETSlimRecoCHANTICandidategR(Long_t size, void *p);
   static void delete_vectorlETSlimRecoCHANTICandidategR(void *p);
   static void deleteArray_vectorlETSlimRecoCHANTICandidategR(void *p);
   static void destruct_vectorlETSlimRecoCHANTICandidategR(void *p);

   // Function generating the singleton type initializer
   static TGenericClassInfo *GenerateInitInstanceLocal(const vector<TSlimRecoCHANTICandidate>*)
   {
      vector<TSlimRecoCHANTICandidate> *ptr = 0;
      static ::TVirtualIsAProxy* isa_proxy = new ::TIsAProxy(typeid(vector<TSlimRecoCHANTICandidate>));
      static ::ROOT::TGenericClassInfo 
         instance("vector<TSlimRecoCHANTICandidate>", -2, "vector", 216,
                  typeid(vector<TSlimRecoCHANTICandidate>), ::ROOT::Internal::DefineBehavior(ptr, ptr),
                  &vectorlETSlimRecoCHANTICandidategR_Dictionary, isa_proxy, 4,
                  sizeof(vector<TSlimRecoCHANTICandidate>) );
      instance.SetNew(&new_vectorlETSlimRecoCHANTICandidategR);
      instance.SetNewArray(&newArray_vectorlETSlimRecoCHANTICandidategR);
      instance.SetDelete(&delete_vectorlETSlimRecoCHANTICandidategR);
      instance.SetDeleteArray(&deleteArray_vectorlETSlimRecoCHANTICandidategR);
      instance.SetDestructor(&destruct_vectorlETSlimRecoCHANTICandidategR);
      instance.AdoptCollectionProxyInfo(TCollectionProxyInfo::Generate(TCollectionProxyInfo::Pushback< vector<TSlimRecoCHANTICandidate> >()));
      return &instance;
   }
   // Static variable to force the class initialization
   static ::ROOT::TGenericClassInfo *_R__UNIQUE_DICT_(Init) = GenerateInitInstanceLocal((const vector<TSlimRecoCHANTICandidate>*)0x0); R__UseDummy(_R__UNIQUE_DICT_(Init));

   // Dictionary for non-ClassDef classes
   static TClass *vectorlETSlimRecoCHANTICandidategR_Dictionary() {
      TClass* theClass =::ROOT::GenerateInitInstanceLocal((const vector<TSlimRecoCHANTICandidate>*)0x0)->GetClass();
      vectorlETSlimRecoCHANTICandidategR_TClassManip(theClass);
   return theClass;
   }

   static void vectorlETSlimRecoCHANTICandidategR_TClassManip(TClass* ){
   }

} // end of namespace ROOT

namespace ROOT {
   // Wrappers around operator new
   static void *new_vectorlETSlimRecoCHANTICandidategR(void *p) {
      return  p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoCHANTICandidate> : new vector<TSlimRecoCHANTICandidate>;
   }
   static void *newArray_vectorlETSlimRecoCHANTICandidategR(Long_t nElements, void *p) {
      return p ? ::new((::ROOT::Internal::TOperatorNewHelper*)p) vector<TSlimRecoCHANTICandidate>[nElements] : new vector<TSlimRecoCHANTICandidate>[nElements];
   }
   // Wrapper around operator delete
   static void delete_vectorlETSlimRecoCHANTICandidategR(void *p) {
      delete ((vector<TSlimRecoCHANTICandidate>*)p);
   }
   static void deleteArray_vectorlETSlimRecoCHANTICandidategR(void *p) {
      delete [] ((vector<TSlimRecoCHANTICandidate>*)p);
   }
   static void destruct_vectorlETSlimRecoCHANTICandidategR(void *p) {
      typedef vector<TSlimRecoCHANTICandidate> current_t;
      ((current_t*)p)->~current_t();
   }
} // end of namespace ROOT for class vector<TSlimRecoCHANTICandidate>

namespace {
  void TriggerDictionaryInitialization_libCHANTISlimPersistency_Impl() {
    static const char* headers[] = {
0    };
    static const char* includePaths[] = {
"/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/NA62/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/CHANTI/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/CHANTI/../../FullReco/CHANTI/include",
"/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include",
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/CHANTI/",
0
    };
    static const char* fwdDeclCode = R"DICTFWDDCLS(
#line 1 "libCHANTISlimPersistency dictionary forward declarations' payload"
#pragma clang diagnostic ignored "-Wkeyword-compat"
#pragma clang diagnostic ignored "-Wignored-attributes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
extern int __Cling_Autoloading_Map;
class __attribute__((annotate("$clingAutoload$TSlimRecoCHANTIHit.hh")))  TSlimRecoCHANTIHit;
namespace std{template <typename _Tp> class __attribute__((annotate("$clingAutoload$bits/allocator.h")))  __attribute__((annotate("$clingAutoload$string")))  allocator;
}
class TSlimRecoCHANTICandidate;
class TSlimRecoCHANTIEvent;
)DICTFWDDCLS";
    static const char* payloadCode = R"DICTPAYLOAD(
#line 1 "libCHANTISlimPersistency dictionary payload"

#ifndef G__VECTOR_HAS_CLASS_ITERATOR
  #define G__VECTOR_HAS_CLASS_ITERATOR 1
#endif

#define _BACKWARD_BACKWARD_WARNING_H
#ifndef TSLIMRECOCHANTICANDIDATE_H
#define TSLIMRECOCHANTICANDIDATE_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t
#include <vector>

#include "TSlimRecoVCandidate.hh"

class TRecoCHANTICandidate;

class TSlimRecoCHANTICandidate : public TSlimRecoVCandidate
{
public:
    TSlimRecoCHANTICandidate()  = default;
    explicit TSlimRecoCHANTICandidate(TRecoCHANTICandidate *candReco);
    virtual ~TSlimRecoCHANTICandidate() = default;

    // setters for members
    void SetXYMult(UShort_t XYMult)     { fXYMult = XYMult;                 }
    void SetXPos(Float_t XPos)          { fXPos = XPos;                     }
    void SetYPos(Float_t YPos)          { fYPos = YPos;                     }
    void SetTime(Float_t time)          { fTime = time;                     }
    void SetXPCharge(Float_t XPCharge)  { fXPCharge = XPCharge;             }
    void SetYPCharge(Float_t YPCharge)  { fYPCharge = YPCharge;             }
    void AddHitIndex(Short_t index)     { fHitsIndexes.emplace_back(index); }

    // getters for members
    UShort_t GetXYMult()                         const { return fXYMult;      }
    Float_t  GetXPos()                           const { return fXPos;        }
    Float_t  GetYPos()                           const { return fYPos;        }
    Float_t  GetTime()                           const { return fTime;        }
    Float_t  GetXPCharge()                       const { return fXPCharge;    }
    Float_t  GetYPCharge()                       const { return fYPCharge;    }
    const std::vector<Short_t>& GetHitsIndexes() const { return fHitsIndexes; }

    // conversion functions
    virtual void FromReco(TRecoVCandidate *candReco);
    virtual void ToReco(TRecoVCandidate *candReco);
private:
    UShort_t fXYMult   = 0;
    Float_t  fXPos     = -999;
    Float_t  fYPos     = -999;
    Float_t fTime     = 0;
    Float_t fXPCharge = -999;
    Float_t fYPCharge = -999;
    std::vector<Short_t> fHitsIndexes;

    ClassDef(TSlimRecoCHANTICandidate, 1)
};

#endif /* TSLIMRECOCHANTICANDIDATE_H */
#ifndef TRECOCHANTIEVENTSLIM_H
#define TRECOCHANTIEVENTSLIM_H

#include <RtypesCore.h>
#include <vector>

#include "TSlimRecoVEvent.hh"
#include "TSlimRecoCHANTICandidate.hh"
#include "TSlimRecoCHANTIHit.hh"

class TRecoCHANTIEvent;

class TSlimRecoCHANTIEvent : public TSlimRecoVEvent
{
public:
    TSlimRecoCHANTIEvent()  = default;
    explicit TSlimRecoCHANTIEvent(TRecoCHANTIEvent *evReco);
    virtual ~TSlimRecoCHANTIEvent() = default;

    void Reset();               // clears the candidate and hit vector
    void ClearHits();
    void ClearCandidates();

    void AddCandidate(TSlimRecoCHANTICandidate c) { fCandidates.emplace_back(std::move(c)); }
    void AddHit(TSlimRecoCHANTIHit h)             { fHits.emplace_back(std::move(h));       }

    Int_t GetNHits()                                       const { return fHits.size();       }
    std::vector<TSlimRecoCHANTIHit>& GetHits()                   { return fHits;              }
    TSlimRecoVHit* GetHit(UInt_t iHit)                           { if(iHit<fHits.size()) return &fHits[iHit]; else return nullptr; }
    Int_t GetNCandidates()                                 const { return fCandidates.size(); }
    std::vector<TSlimRecoCHANTICandidate>& GetCandidates()       { return fCandidates;        }
    TSlimRecoVCandidate* GetCandidate(UInt_t iCand)              { if(iCand<fCandidates.size()) return &fCandidates[iCand]; else return nullptr; }

    // conversion functions
    virtual void FromReco(TRecoVEvent *evReco);
    virtual void ToReco(TRecoVEvent *evReco);
private:
    std::vector<TSlimRecoCHANTIHit> fHits;
    std::vector<TSlimRecoCHANTICandidate> fCandidates;

    ClassDef(TSlimRecoCHANTIEvent, 1)
};

#endif /* TRECOCHANTISLIM_H */
#ifndef TSLIMRECOCHANTIHIT_H
#define TSLIMRECOCHANTIHIT_H

#include <RtypesCore.h>             // ROOT data types, e.g. Float_t

#include "TSlimRecoVHit.hh"

class TRecoCHANTIHit;

class TSlimRecoCHANTIHit : public TSlimRecoVHit
{
public:
    TSlimRecoCHANTIHit()  = default;
    explicit TSlimRecoCHANTIHit(TRecoCHANTIHit *hitReco);
    virtual ~TSlimRecoCHANTIHit() = default;

    // setters for members
    void SetChannelID(Int_t channelID)            { fChannelID = channelID;         }
    void SetQualityFlag(UShort_t QualityFlag)     { fQualityFlag = QualityFlag;     }
    void SetThresholdFlag(UShort_t ThresholdFlag) { fThresholdFlag = ThresholdFlag; }
    void SetMult(UShort_t Mult)                   { fMult = Mult;                   }
    void SetTime(Float_t time)                    { fTime = time;                   }
    void SetTimeWidth(Float_t width)              { fTimeWidth = width;             }
    void SetDeltaTime(Float_t DeltaTime)          { fDeltaTime = DeltaTime;         }
    void SetDeltaWidth(Float_t DeltaWidth)        { fDeltaWidth = DeltaWidth;       }

    // getters for members
    Int_t    GetChannelID()     const { return fChannelID;     };
    UShort_t GetQualityFlag()   const { return fQualityFlag;   };
    UShort_t GetThresholdFlag() const { return fThresholdFlag; };
    UShort_t GetMult()          const { return fMult;          };
    Float_t  GetTime()          const { return fTime;          };
    Float_t  GetTimeWidth()     const { return fTimeWidth;     };
    Float_t  GetDeltaTime()     const { return fDeltaTime;     };
    Float_t  GetDeltaWidth()    const { return fDeltaWidth;    };

    // getters for variables from Standard Persistency interface
    UShort_t GetPlaneID()  const;
    UShort_t GetRingType() const;
    UShort_t GetRingID()   const;
    UShort_t GetSideID()   const;
    Short_t  GetBarID()    const;
    Float_t  GetX()        const;
    Float_t  GetY()        const;
    Float_t  GetZ()        const;

    // conversion functions
    virtual void FromReco(TRecoVHit *hitReco);
    virtual void ToReco(TRecoVHit *hitReco);
private:
    Int_t    fChannelID     = 0;
    UShort_t fQualityFlag   = 0;
    UShort_t fThresholdFlag = 0;
    UShort_t fMult          = 0;
    Float_t  fTime          = -999;
    Float_t  fTimeWidth     = -999;
    Float_t  fDeltaTime     = -999;
    Float_t  fDeltaWidth    = -999;

    ClassDef(TSlimRecoCHANTIHit, 1)
};

#endif /* TSLIMRECOCHANTIHIT_H */

#undef  _BACKWARD_BACKWARD_WARNING_H
)DICTPAYLOAD";
    static const char* classesHeaders[]={
"TSlimRecoCHANTICandidate", payloadCode, "@",
"TSlimRecoCHANTIEvent", payloadCode, "@",
"TSlimRecoCHANTIHit", payloadCode, "@",
nullptr};

    static bool isInitialized = false;
    if (!isInitialized) {
      TROOT::RegisterModule("libCHANTISlimPersistency",
        headers, includePaths, payloadCode, fwdDeclCode,
        TriggerDictionaryInitialization_libCHANTISlimPersistency_Impl, {}, classesHeaders, /*has no C++ module*/false);
      isInitialized = true;
    }
  }
  static struct DictInit {
    DictInit() {
      TriggerDictionaryInitialization_libCHANTISlimPersistency_Impl();
    }
  } __TheDictionaryInitializer;
}
void TriggerDictionaryInitialization_libCHANTISlimPersistency() {
  TriggerDictionaryInitialization_libCHANTISlimPersistency_Impl();
}
