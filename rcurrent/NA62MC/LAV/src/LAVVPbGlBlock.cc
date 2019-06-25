// LAVVPbGlBlock.hh
// -------------------------------------------------------------------------
// History:
//
// 2010-11-23 Emanuele Leonardi (Emanuele.Leonardi@roma1.infn.it)
//   - New virtual class to create PbGl blocks. Defines interface to
//     be used while positioning block. Concrete instances of this
//     virtual class will be used to implement different simulations
//     of block's physics.
//
// -------------------------------------------------------------------------

#include "LAVVPbGlBlock.hh"

LAVVPbGlBlock::LAVVPbGlBlock() :
  fLogicalVolume(nullptr)
{}
LAVVPbGlBlock::~LAVVPbGlBlock(){}
