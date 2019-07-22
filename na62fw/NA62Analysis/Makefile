# To list the possible targets to compile, use
#   make help
# To compile a single target, use
#   make targetName         (example make RICHPersistency)
# To clean a detector, use
#   make clean-detectorName (example make clean-Cedar)
# Compile with debugging information with 
#   make NA62_DEBUG=1

DIRS:=$(filter-out build%,$(patsubst %/,%,$(dir $(wildcard */))))
CLEANDIRS:=$(addprefix clean-,$(DIRS))

CMAKE_FLAGS:=
ifdef BUILD_TYPE
	CMAKE_FLAGS += -DCMAKE_BUILD_TYPE=$(BUILD_TYPE)
endif
ifeq "$(BUILD_STD)" "1"
	CMAKE_FLAGS += -DCOMPILE_STD_AN=1
endif
ifeq "$(NA62_DEBUG)" "1"
	CMAKE_FLAGS += -DNA62_DEBUG=1
endif
ifeq "$(FULL_WARNING)" "1"
	CMAKE_FLAGS += -DFULL_WARNING=1
endif

ifndef SYSTEMINSTALL
  $(error Variable SYSTEMINSTALL is not defined!)
endif

BUILDDIR=build-$(SYSTEMINSTALL)

all: $(BUILDDIR)
	+@${MAKE} -C $(BUILDDIR) install --no-print-directory $(MAKEFLAGS)

$(BUILDDIR): 
	@cmake -H. -B$(BUILDDIR) $(CMAKE_FLAGS) ${ARGS}

clean:
	rm -rf $(BUILDDIR)
	rm -rf lib-$(SYSTEMINSTALL)
	rm -rf bin-$(SYSTEMINSTALL)

.DEFAULT: $(BUILDDIR)
	+@make -C $(BUILDDIR) $@ --no-print-directory $(MAKEFLAGS)

$(DIRS): $(BUILDDIR)
	+@make -C $(BUILDDIR) $@ --no-print-directory $(MAKEFLAGS)

$(CLEANDIRS): $(BUILDDIR)
	rm -rf $(BUILDDIR)/$(patsubst clean-%,%,$@)
		
.PHONY: build_all clean $(DIRS) $(CLEANDIRS)   
