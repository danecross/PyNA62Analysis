// ---------------------------------------------------------------
// History:
//
// Created by Evgueni Goudzovski (eg@hep.ph.bham.ac.uk) 2017-07-11
//
// ---------------------------------------------------------------

#include "NA62Global.hh"

std::string TimeString(time_t &time) {
  static char Time[100];
  std::strftime(Time, sizeof(Time), "%a %b %d %H:%M:%S %Y", std::localtime(&time));
  return std::string(Time);
}

std::string TimeString() {
  time_t now = time(0);
  return TimeString(now);
}
