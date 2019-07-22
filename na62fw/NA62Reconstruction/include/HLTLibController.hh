#include <string>
#include <iostream>
#include <functional>



//#include "l0/offline/Subevent.h"
//#include "l1/L1InfoToStorage.h"

//#ifdef ONLINEHLT
////HLT
//
//#include <l0/offline/Subevent.h>
//#include <common/decoding/OfflineDecoderHandler.h>
//#include <l1/KtagAlgo.h>
//#include <l1/StrawAlgo.h>
//#include <l1/straw_algorithm/Track.h>
//#include <l1/LAVAlgo.h>
//#include <l1/L1InfoToStorage.h>
//#include <bitset>
//#include <dlfcn.h>
//#endif

#ifndef HLTLibController_H
#define HLTLibController_H
class HLTLibController {
  public:
    explicit HLTLibController(const std::string& absolute_path):m_library_absolute_path(absolute_path), m_is_loaded(false), m_are_symbol_loaded(false), m_library(nullptr) {
        loadLibrary();
    }
    bool loadObjects();

    bool isLibLoaded() {
        return m_is_loaded;
    }
    void* get() {
        return m_library;
    }
    void closeLibrary();

    virtual ~HLTLibController() {
        closeLibrary();
        std::cout << "HLTLibController destroyed" << std::endl;
    }

    using functype_initialize = void();
    std::function<functype_initialize> handleInitKtag;
    std::function<functype_initialize> handleInitLav;
    std::function<functype_initialize> handleInitStraw;

    using functype_lav_conf_file = void(std::string);
    std::function<functype_lav_conf_file> handleLavConfig;

    using functype_straw_conf_file = void(std::string, std::string, std::string);
    std::function<functype_straw_conf_file> handleStrawConfig;

    //using functype_subevent_constructor = na62::l0::Subevent*(const uint_fast16_t pexpectedPacketsNum);
    //std::function<functype_subevent_constructor> createSubevent;

    //using functype_l1storage_constructor = L1InfoToStorage*(void);
    //std::function<functype_l1storage_constructor> createL1storage;

    //na62::l0::Subevent* m_subevent;
    //L1InfoToStorage* m_l1Info;

    private:
    std::string m_library_absolute_path;
    bool m_is_loaded;
    bool m_are_symbol_loaded;
    void* m_library;

    bool loadLibrary();
};

#endif
