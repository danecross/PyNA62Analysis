// ---------------------------------------------------------------
// History:
//
// Created by Dmitry Madigozhin (madigo@mail.cern.ch) 2016-11-24
//
// ---------------------------------------------------------------

#ifndef T0JUMPS
#define T0JUMPS

#include <iostream>
#include <fstream>
#include <vector>
#include "TString.h"

using namespace std;

class T0Jumps{

  // here time means the burst "Unix epoch time" in seconds (see http://www.epochconverter.com/)

  private:
    TString fT0JumpFileName;
    int fLastRequestedTime;
    int fLastTime, fLastCover, fLastJump;
    int fFromTime; // only starting from this time the jumps will be taken into account
    int fJump[512];

    bool fExclusive;
    vector<int> fExclusiveRun, fExclusiveRunBegin, fExclusiveRunEnd;

    int ForTime(int time);
    void ZeroStatus();

  public:

    T0Jumps(const char *fname, int from);
    ~T0Jumps();

    void SetExclusive(const char *fname);
    int GetJump(int i, int time);
};

#endif
