file(REMOVE_RECURSE
  "libSAV.pdb"
  "libSAV.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/SAV.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
