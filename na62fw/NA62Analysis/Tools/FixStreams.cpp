/*
  Author: Nicolas Lurkin
  Email: nicolas.lurkin@cern.ch
  
  Execute with
  ./FixStreams input.root

  Fixes the Streams tree if the input file has been merged with hadd
*/

#include <iostream>

#include "FixStreams.hh"

int main(int argc, char** argv) {
  if(argc != 2){
    std::cout<<"Invalid number of arguments: Expects 1 ROOT input file path."<<std::endl;
    return -1;
  }

  fixStreams(argv[1]);

  return 0;
}
