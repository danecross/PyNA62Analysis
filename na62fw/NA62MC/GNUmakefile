# To list the possible targets to compile, use
#   make help
# To compile a single target, use
#   make targetName         (example make RICHPersistency)
# To clean a detector, use
#   make clean-detectorName (example make clean-Cedar)
# Compile with debugging information with 
#   make BUILD_TYPE=Debug

DIRS:=$(filter-out build%,$(patsubst %/,%,$(dir $(wildcard */))))
CLEANDIRS:=$(addprefix clean-,$(DIRS))

CMAKE_FLAGS:=
ifdef BUILD_TYPE
    CMAKE_FLAGS += -DCMAKE_BUILD_TYPE=$(BUILD_TYPE)
endif
ifeq "$(SHARED)" "1"
    CMAKE_FLAGS += -DSHARED_LIB=1
endif
ifeq "$(G4UI)" "1"
    CMAKE_FLAGS += -DWITH_GEANT4_UIVIS=On
endif

ifndef SYSTEMINSTALL
  $(error Variable SYSTEMINSTALL is not defined!)
endif

BUILDDIR=build-$(SYSTEMINSTALL)

all: $(BUILDDIR)
	+@make -C $(BUILDDIR) install --no-print-directory $(MAKEFLAGS)

$(BUILDDIR): 
	@cmake -H. -B$(BUILDDIR) $(CMAKE_FLAGS) ${ARGS}

clean:
	rm -rf $(BUILDDIR)
	rm -rf lib-$(SYSTEMINSTALL)
	rm -rf bin-$(SYSTEMINSTALL)
	rm -f config/NA62MC-$(SYSTEMINSTALL)Config.cmake

.DEFAULT: $(BUILDDIR)
	+@make -C $(BUILDDIR) $@ --no-print-directory $(MAKEFLAGS)

$(DIRS): $(BUILDDIR)
	+@make -C $(BUILDDIR) $@ --no-print-directory $(MAKEFLAGS)

$(CLEANDIRS): $(BUILDDIR)
	rm -rf $(BUILDDIR)/$(patsubst clean-%,%,$@)

.PHONY: build_all clean $(DIRS) $(CLEANDIRS)   
