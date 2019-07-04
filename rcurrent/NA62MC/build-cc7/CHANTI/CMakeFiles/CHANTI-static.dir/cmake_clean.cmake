file(REMOVE_RECURSE
  "libCHANTI-static.pdb"
  "libCHANTI-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/CHANTI-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
