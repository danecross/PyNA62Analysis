// ---------------------------------------------------------------
// History:
//
// Modified by Alina KLeimenova (alina.kleimenova@cern.ch) Sep. 2017
//
// Created by Antonino Sergi (Antonino.Sergi@cern.ch) 2012-07-16
//
// ---------------------------------------------------------------

#ifndef GigaTrackerOnlineMonitor_H
#define GigaTrackerOnlineMonitor_H 1

#include "NA62VOnlineMonitor.hh"
#include "TPaveText.h"

class GigaTrackerOnlineMonitor : public NA62VOnlineMonitor {

  public:

    GigaTrackerOnlineMonitor(TRootBrowser*,NA62VReconstruction*,Int_t);
    virtual ~GigaTrackerOnlineMonitor();
    virtual void Update(Int_t);

    void CreateShifterModeTabs();
    void CreateExpertModeTabs();

  private:
    void DrawTable(int);
    int GetPixelID(int ID,int x,int y);
    int GetPixelInfo(int station, int i, int j);
    std::vector< std::vector<int> > fContainer[3];
    struct SortByFirstCol
    {
      bool operator()(const std::vector<int>& flvect,
                      const std::vector<int>& frvect) const
      {
	return flvect[0] > frvect[0];
      }
    };

    double fX1=0.05;
    double fY1=0.15;
    double fX2=0.95;
    double fY2=0.75;

    TPaveText *fTableCont[3][5][6];
};

#endif
