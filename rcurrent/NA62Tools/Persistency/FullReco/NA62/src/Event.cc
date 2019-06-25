// --------------------------------------------------------------
// History:
//
// Created by Emanuele Leonardi (Emanuele Leonardi@cern.ch) 2007-01-10
// Modified by Sergey Podolsky 2011-01-21
// --------------------------------------------------------------

#include "Event.hh"

#include <stdlib.h>
#include "Riostream.h"

ClassImp(Event)

TRandom3* Event::fgRandomDecayState = 0;

Event::Event() {
  fGeneParts = new TClonesArray("GenePart", 25);
  fNGeneParts = 0;
  fKineParts = new TClonesArray("KinePart", 50);
  fNKineParts = 0;
  fEventBoundaries = new TClonesArray("EventBoundary", 1);
  fNMergedEvents = 0;
  fCurrentEventBoundary = 0;
  fNPurgedKineParts = 0;
  fRandomDecayState = new TRandom3();
  fEventNumber = 0;
  fEventWeight = 1.0;
}

void Event::Merge(Event * event) {
  if (fNGeneParts == 0 && fNKineParts == 0) fNMergedEvents = 0;

  Int_t PreviousNKineParts = fNKineParts;
  for(Int_t iSubEvent = 0; iSubEvent < event->GetNMergedEvents(); iSubEvent++){
    EventBoundary* EvBound = static_cast<EventBoundary*>(event->GetEventBoundaries()->At(iSubEvent));
    //cout << "Event::Merge: merging: iSubEvent = " << iSubEvent << " " << EvBound << " StreamID = " << EvBound->GetStreamID() << " ID = " << EvBound->GetID() << endl;
    EvBound->Shift(fNGeneParts, fNKineParts);
    fCurrentEventBoundary = static_cast<EventBoundary*>((fEventBoundaries->ConstructedAt(fNMergedEvents++,"C")));
    *fCurrentEventBoundary = *EvBound;
    //cout << "Event::Merge: merged: iSubEvent = " << iSubEvent << " " << fCurrentEventBoundary << " StreamID = " << fCurrentEventBoundary->GetStreamID() << " ID = " << fCurrentEventBoundary->GetID() << endl;
  }
  for(Int_t iPart = 0; iPart < event->GetNGeneParts(); iPart++){
    GenePart* part = static_cast<GenePart*>(event->GetGeneParts()->At(iPart));
    GenePart* gpart = static_cast<GenePart*>((fGeneParts->ConstructedAt(fNGeneParts++,"C")));
    *gpart = *part;
  }
  for(Int_t iPart = 0; iPart < event->GetNKineParts(); iPart++){
    KinePart* part = static_cast<KinePart*>(event->GetKineParts()->At(iPart));
    KinePart* kpart = static_cast<KinePart*>((fKineParts->ConstructedAt(fNKineParts++,"C")));
    *kpart = *part;
    static_cast<KinePart*>(fKineParts->At(fNKineParts-1))->ShiftParentIndex(PreviousNKineParts);
  }
//    cout << "Event::Merge: End: fNMergedEvents = " << fNMergedEvents << endl;
//    for(Int_t iSubEvent = 0; iSubEvent < fNMergedEvents; iSubEvent++){
//        EventBoundary* EvBound = static_cast<EventBoundary*>(fEventBoundaries->At(iSubEvent));
//        cout << "Event::Merge: iSubEvent = " << iSubEvent << " " << EvBound << " StreamID = " << EvBound->GetStreamID() << " ID = " << EvBound->GetID() << " FirstKP = " << EvBound->GetFirstKinePartIndex() << endl;
//    }
}

void Event::Purge(TEventInfo SubEventToPurge) {
  if (fNGeneParts == 0 && fNKineParts == 0) return;

//    cout << endl << "Event::Purge: Begin: fNMergedEvents = " << fNMergedEvents << endl;
//  for(Int_t iSubEvent = 0; iSubEvent < fNMergedEvents; iSubEvent++){
//      EventBoundary* EvBound = static_cast<EventBoundary*>(fEventBoundaries->At(iSubEvent));
//        cout << "Event::Purge: iSubEvent = " << iSubEvent << " StreamID = " << EvBound->GetStreamID() << " ID = " << EvBound->GetID() << " FirstKP = " << EvBound->GetFirstKinePartIndex() << endl;
//  }

  Int_t NGenePartsRemoved = 0, NKinePartsRemoved = 0;
  for(Int_t iSubEvent = 0; iSubEvent < fNMergedEvents; iSubEvent++){
    EventBoundary* EvBound = static_cast<EventBoundary*>(fEventBoundaries->At(iSubEvent));
    if(EvBound->GetID() == SubEventToPurge.GetID() && EvBound->GetStreamID() == SubEventToPurge.GetStreamID()){
//            cout << "Event::Purge: purging iSubEvent = " << iSubEvent << " StreamID = " << EvBound->GetStreamID() << " ID = " << EvBound->GetID() << endl;
      for(Int_t iPart = EvBound->GetFirstGenePartIndex(); iPart < EvBound->GetFirstGenePartIndex() + EvBound->GetNGeneParts(); iPart++)
	fGeneParts->RemoveAt(iPart);
      NGenePartsRemoved = EvBound->GetNGeneParts();
      for(Int_t iPart = EvBound->GetFirstKinePartIndex(); iPart < EvBound->GetFirstKinePartIndex() + EvBound->GetNKineParts(); iPart++)
	fKineParts->RemoveAt(iPart);
      NKinePartsRemoved = EvBound->GetNKineParts();
      fEventBoundaries->RemoveAt(iSubEvent);
//            cout << "Event::Purge: purged: NGenePartsRemoved = " << NGenePartsRemoved << " NKinePartsRemoved = " << NKinePartsRemoved << endl;
      for(Int_t iSubEvent2 = iSubEvent + 1; iSubEvent2 < fNMergedEvents; iSubEvent2++){
	EventBoundary* EvBound2 = static_cast<EventBoundary*>(fEventBoundaries->At(iSubEvent2));
//                cout << "Event::Purge: shifting iSubEvent = " << iSubEvent2 << " StreamID = " << EvBound2->GetStreamID() << " ID = " << EvBound2->GetID() << endl;
	for(Int_t iPart = EvBound2->GetFirstKinePartIndex(); iPart < EvBound2->GetFirstKinePartIndex() + EvBound2->GetNKineParts(); iPart++)
	  static_cast<KinePart*>(fKineParts->At(iPart))->ShiftParentIndex(-NKinePartsRemoved);
	EvBound2->Shift(-NGenePartsRemoved, -NKinePartsRemoved);
//                cout << "Event::Purge: shifted iSubEvent = " << iSubEvent2 << " StreamID = " << EvBound2->GetStreamID() << " ID = " << EvBound2->GetID() << endl;
      }
      fNMergedEvents--;
    }
  }
  fGeneParts->Compress();
  fNGeneParts -= NGenePartsRemoved;
  fKineParts->Compress();
  fNKineParts -= NKinePartsRemoved;
  fEventBoundaries->Compress();
  fNPurgedKineParts = NKinePartsRemoved;
  if (fNMergedEvents)
    fCurrentEventBoundary = static_cast<EventBoundary*>(fEventBoundaries->At(fNMergedEvents - 1));
  else
    fCurrentEventBoundary = 0;
//    cout << "Event::Purge: End: fNMergedEvents = " << fNMergedEvents << endl;
//  for(Int_t iSubEvent = 0; iSubEvent < fNMergedEvents; iSubEvent++){
//      EventBoundary* EvBound = static_cast<EventBoundary*>(fEventBoundaries->At(iSubEvent));
//        cout << "Event::Purge: iSubEvent = " << iSubEvent << " " << EvBound << " StreamID = " << EvBound->GetStreamID() << " ID = " << EvBound->GetID() << " FirstKP = " << EvBound->GetFirstKinePartIndex() << endl;
//  }
}

EventBoundary * Event::FindEventBoundary(Int_t KinePartIndex){
  for(Int_t iSubEvent = 0; iSubEvent < fNMergedEvents; iSubEvent++){
    EventBoundary* EvBound = static_cast<EventBoundary*>(fEventBoundaries->At(iSubEvent));
    if(KinePartIndex >= EvBound->GetFirstKinePartIndex() && KinePartIndex < EvBound->GetFirstKinePartIndex() + EvBound->GetNKineParts())
      return EvBound;
  }
  return 0;
}

Int_t Event::GetID(){
  // cout << "Event::GetID " << fCurrentEventBoundary << endl;
  if(!fCurrentEventBoundary)
    fCurrentEventBoundary = static_cast<EventBoundary*>(fEventBoundaries->At(fNMergedEvents - 1));
  return fCurrentEventBoundary->GetID();
}

void Event::SetID(Int_t ID){
  // cout << "Event::SetID " << fCurrentEventBoundary << endl;
  if(!fCurrentEventBoundary)
    fCurrentEventBoundary = static_cast<EventBoundary*>(fEventBoundaries->At(fNMergedEvents - 1));
  fCurrentEventBoundary->SetID(ID);
}

Int_t Event::GetStreamID(){
  // cout << "Event::GetStreamID " << fCurrentEventBoundary << endl;
  if(!fCurrentEventBoundary)
    fCurrentEventBoundary = static_cast<EventBoundary*>(fEventBoundaries->At(fNMergedEvents - 1));
  return fCurrentEventBoundary->GetStreamID();
}

void Event::SetStreamID(Int_t StreamID){
  // cout << "Event::SetStreamID " << fCurrentEventBoundary << endl;
  if(!fCurrentEventBoundary)
    fCurrentEventBoundary = static_cast<EventBoundary*>(fEventBoundaries->At(fNMergedEvents - 1));
  fCurrentEventBoundary->SetStreamID(StreamID);
}

Double_t Event::GetTime(){
  // cout << "Event::GetTime " << fCurrentEventBoundary << endl;
  if(!fCurrentEventBoundary)
    fCurrentEventBoundary = static_cast<EventBoundary*>(fEventBoundaries->At(fNMergedEvents - 1));
  return fCurrentEventBoundary->GetTime();
}

void Event::SetTime(Double_t Time){
  // cout << "Event::SetTime " << fCurrentEventBoundary << endl;
  if(!fCurrentEventBoundary)
    fCurrentEventBoundary = static_cast<EventBoundary*>(fEventBoundaries->At(fNMergedEvents - 1));
  fCurrentEventBoundary->SetTime(Time);
}

void Event::Clear(Option_t * /*option*/) {
  fNMergedEvents = 0;
  fNGeneParts = 0;
  fNKineParts = 0;
  fCurrentEventBoundary = 0;
  fNPurgedKineParts = 0;
  fEventNumber = 0;
  fEventWeight = 1.0;
  if (fEventBoundaries)  fEventBoundaries->Clear("C");
  if (fGeneParts)        fGeneParts->Clear("C");
  if (fKineParts)        fKineParts->Clear("C"); // will also call KinePart::Clear
  if (fRandomDecayState) fRandomDecayState->Clear("C");
}

void Event::Print(Option_t * /*option*/) const {
  std::cout << " NGeneParticles " << fNGeneParts << std::endl;
  std::cout << " NKineParticles " << fNKineParts << std::endl;
  PrintAll();
}

void Event::PrintAll() const {
  if (fNGeneParts>0) {
    std::cout << "--- Gene Particles ---" << std::endl;
    fGeneParts->Print();
  }
  if (fNKineParts>0) {
    std::cout << "--- Kine Particles ---" << std::endl;
    fKineParts->Print();
  }
}

GenePart* Event::AddGenePart() {
  if(!fNMergedEvents)
    fCurrentEventBoundary = static_cast<EventBoundary*>((fEventBoundaries->ConstructedAt(fNMergedEvents++,"C")));
  if(!fCurrentEventBoundary)
    fCurrentEventBoundary = static_cast<EventBoundary*>((fEventBoundaries->ConstructedAt(fNMergedEvents - 1,"C")));
  fCurrentEventBoundary->AddGenePart();
  GenePart* part = static_cast<GenePart*>((fGeneParts->ConstructedAt(fNGeneParts++,"C")));
  return part;
}

GenePart* Event::GetGenePart(Int_t i) {
  return static_cast<GenePart*>(fGeneParts->At(i));
}

KinePart* Event::AddKinePart() {
  if(!fNMergedEvents)
    fCurrentEventBoundary = static_cast<EventBoundary*>((fEventBoundaries->ConstructedAt(fNMergedEvents++,"C")));
  if(!fCurrentEventBoundary)
    fCurrentEventBoundary = static_cast<EventBoundary*>((fEventBoundaries->ConstructedAt(fNMergedEvents - 1,"C")));
  fCurrentEventBoundary->AddKinePart();
  KinePart* part = static_cast<KinePart*>((fKineParts->ConstructedAt(fNKineParts++,"C")));
  return part;
}

KinePart* Event::GetKinePart(Int_t i) {
  return static_cast<KinePart*>(fKineParts->At(i));
}

void Event::StoreRandomState(TRandom3* RandomDecayState, Long_t *RanecuState) {
  *fRandomDecayState = *RandomDecayState;
  fRanecuState[0] = RanecuState[0];
  fRanecuState[1] = RanecuState[1];
}
