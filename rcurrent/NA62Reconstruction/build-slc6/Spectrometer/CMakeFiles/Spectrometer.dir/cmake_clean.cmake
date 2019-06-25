file(REMOVE_RECURSE
  "libSpectrometer.pdb"
  "libSpectrometer.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/Spectrometer.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
