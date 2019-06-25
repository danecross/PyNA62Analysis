file(REMOVE_RECURSE
  "libBeam.pdb"
  "libBeam.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang C CXX Fortran)
  include(CMakeFiles/Beam.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
