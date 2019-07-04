file(REMOVE_RECURSE
  "libCHANTI.pdb"
  "libCHANTI.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/CHANTI.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
