#ifndef THACEvent_H
#define THACEvent_H

#include "TDetectorVEvent.hh"

class THACEvent : public TDetectorVEvent {

    public:

        THACEvent();
        ~THACEvent();

        void Clear(Option_t* = "");

    private:

        ClassDef(THACEvent,1);
};
#endif
