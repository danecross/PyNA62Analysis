
#include "HLTLibController.hh"

#include <dlfcn.h>

bool HLTLibController::loadLibrary() {
    if (m_is_loaded) {
        return false; //TODO cannot load before close buh...
    }
    m_library = dlopen(m_library_absolute_path.c_str(), RTLD_LAZY);
    if (not m_library) {
      std::cerr << "[HLTLibController] Error " << dlerror() << std::endl;
      return false;
    }

    m_is_loaded = true;
    std::cout << "[HLTLibController] library " <<  m_library_absolute_path << " loaded" << std::endl;
    return true;
}

void HLTLibController::closeLibrary() {
    if (m_is_loaded) {
        dlclose(m_library);
        //std::cout << "library closed" << std::endl;
        m_is_loaded = false;
    }
    if (m_are_symbol_loaded) {
        //TODO you should unload the symbols
        m_are_symbol_loaded = false;
    }
}

bool HLTLibController::loadObjects() {
  try {
    handleInitKtag = reinterpret_cast<functype_initialize*>(dlsym(m_library, "initialize_ktag"));
    handleInitLav = reinterpret_cast<functype_initialize*>(dlsym(m_library, "initialize_lav"));
    handleInitStraw = reinterpret_cast<functype_initialize*>(dlsym(m_library, "initialize_straw"));
    handleLavConfig = reinterpret_cast<functype_lav_conf_file*>(dlsym(m_library, "load_lav_conf"));
    handleStrawConfig = reinterpret_cast<functype_straw_conf_file*>(dlsym(m_library, "load_straw_conf"));
    //createSubevent = reinterpret_cast<functype_subevent_constructor*>(dlsym(m_library, "create_subevent"));
    //createL1storage = reinterpret_cast<functype_l1storage_constructor*>(dlsym(m_library, "create_l1storage"));

  } catch (std::bad_function_call& e) {
    std::cerr << e.what();
    return false;
  }
  m_are_symbol_loaded = true;

  //int const max_fragment_number = 32;
  //na62::l0::Subevent* m_subevent = createSubevent(max_fragment_number);
  //L1InfoToStorage* m_l1Info = createL1storage();

  return true;
}

