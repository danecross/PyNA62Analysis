file(REMOVE_RECURSE
  "libBeam-static.pdb"
  "libBeam-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang C CXX Fortran)
  include(CMakeFiles/Beam-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
