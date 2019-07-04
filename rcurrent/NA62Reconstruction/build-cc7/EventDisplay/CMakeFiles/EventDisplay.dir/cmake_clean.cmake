file(REMOVE_RECURSE
  "libEventDisplay.pdb"
  "libEventDisplay.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/EventDisplay.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
