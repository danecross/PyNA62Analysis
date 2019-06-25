file(REMOVE_RECURSE
  "libSpectrometer-static.pdb"
  "libSpectrometer-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/Spectrometer-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
