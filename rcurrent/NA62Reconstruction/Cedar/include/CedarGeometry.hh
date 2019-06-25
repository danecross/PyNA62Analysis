// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2011-08-26
//
// ---------------------------------------------------------------

#ifndef CedarGeometry_H
#define CedarGeometry_H 1

class CedarGeometry {

public:

  CedarGeometry();
  ~CedarGeometry();

  int GetNSectors() { return fNSectors; }

private:

  void   CreateGeometry();
  int    fNSectors;

};
#endif
