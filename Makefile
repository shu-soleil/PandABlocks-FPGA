# Top level make file for building PandA FPGA images

TOP := $(CURDIR)

# Build defaults that can be overwritten by the CONFIG file if present

BUILD_DIR = $(TOP)/build
PYTHON = python2
SPHINX_BUILD = sphinx-build
PANDA_ROOTFS = $(error Define PANDA_ROOTFS in CONFIG file)
MAKE_ZPKG = $(PANDA_ROOTFS)/make-zpkg
APPS = $(patsubst apps/%.app.ini,%,$(wildcard apps/*.app.ini))
TEST_DIR = $(BUILD_DIR)/tests
IP_DIR = $(TOP)/common/ip_repo
FPGA_BUILD_DIR = $(BUILD_DIR)/apps/$(APPS)
TARGET_DIR = $(TOP)/targets/$(TARGET)
SLOW_FPGA_BUILD_DIR = $(BUILD_DIR)/apps/$(APPS)/SlowFPGA

# The CONFIG file is required.  If not present, create by copying CONFIG.example
# and editing as appropriate.
include CONFIG

#default: apps docs
default: $(DEFAULT_TARGETS)
.PHONY: default

# ------------------------------------------------------------------------------
# App source autogeneration

# For every APP in APPS, make build/APP
APP_BUILD_DIRS = $(patsubst %,$(BUILD_DIR)/apps/%,$(APPS))
# Make the built app from the ini file
$(BUILD_DIR)/apps/%: $(TOP)/apps/%.app.ini
	rm -rf $@_tmp $@
	$(PYTHON) -m common.python.generate_app $@_tmp $^
	mv $@_tmp $@

apps: $(APP_BUILD_DIRS)

.PHONY: apps

# ------------------------------------------------------------------------------
# FPGA bitstream generation

# Something like 0.1-1-g5539563-dirty
export GIT_VERSION := $(shell git describe --abbrev=7 --dirty --always --tags)
# Split and append .0 to get 0.1.0, then turn into hex to get 00000100
export VERSION := $(shell ./common/python/parse_git_version.py "$(GIT_VERSION)")
# 8 if dirty, 0 if clean
DIRTY_PRE = $(shell python -c "print 8 if '$(GIT_VERSION)'.endswith('dirty') else 0")
# Something like 85539563
export SHA := $(DIRTY_PRE)$(shell git rev-parse --short HEAD)

# ------------------------------------------------------------------------------
# Documentation

# Generated rst sources from modules are put here, unfortunately it has to be in
# the docs dir otherwise matplotlib plot_directive screws up
DOCS_BUILD_DIR = $(TOP)/docs/build

# The html docs are built into this dir
DOCS_HTML_DIR = $(BUILD_DIR)/html
ALL_RST_FILES = $(shell find docs modules -name '*.rst')
BUILD_RST_FILES = $(wildcard docs/build/*.rst)
SRC_RST_FILES = $(filter-out $(BUILD_RST_FILES),$(ALL_RST_FILES))

$(DOCS_HTML_DIR): docs/conf.py $(SRC_RST_FILES)
	$(SPHINX_BUILD) -b html docs $@

docs: $(DOCS_HTML_DIR)

.PHONY: docs

# ------------------------------------------------------------------------------
# Test just the python framework

python_tests:
	$(PYTHON) -m unittest discover -v tests.python

.PHONY: python_tests

# ------------------------------------------------------------------------------
# Test just the timing for simulations

python_timing:
	$(PYTHON) -m unittest -v tests.test_python_sim_timing

.PHONY: python_timing

#-------------------------------------------------------------------------------
# Remove the Xilinx IP
ip_clean:
	rm -rf $(IP_DIR)
# ------------------------------------------------------------------------------
# Timing test benches using vivado to run FPGA simulations

# every modules/MODULE/BLOCK.timing.ini
TIMINGS = $(wildcard modules/*/*.timing.ini)

# MODULE for every modules/MODULE/BLOCK.timing.ini
MODULES = $(sort $(dir $(patsubst modules/%,%,$(TIMINGS))))

# build/hdl_timing/MODULE for every MODULE
TIMING_BUILD_DIRS = $(patsubst %/,$(BUILD_DIR)/hdl_timing/%,$(MODULES))

# Make the built app from the ini file
$(BUILD_DIR)/hdl_timing/%: modules/%/*.timing.ini
	rm -rf $@_tmp $@
	$(PYTHON) -m common.python.generate_hdl_timing $@_tmp $^
	mv $@_tmp $@

# Make the hdl_timing folders without running tests
hdl_timing: $(TIMING_BUILD_DIRS)

# Make the hdl_timing folders and run all tests, or specific module by setting
# the MODULE argument
hdl_test: $(TIMING_BUILD_DIRS)
	rm -rf $(TEST_DIR)/regression_tests
	rm -rf $(TEST_DIR)/*.jou
	rm -rf $(TEST_DIR)/*.log
	mkdir -p $(TEST_DIR)

	cd $(TEST_DIR) && source $(VIVADO) && vivado -mode batch -notrace \
	 -source ../../tests/hdl/regression_tests.tcl -tclargs $(MODULE)

# Make the hdl_timing folders and run a single test, set TEST argument
single_hdl_test: $(TIMING_BUILD_DIRS)
	rm -rf $(TEST_DIR)/single_test
	rm -rf $(TEST_DIR)/*.jou
	rm -rf $(TEST_DIR)/*.log
	mkdir -p $(TEST_DIR)
	cd $(TEST_DIR) && source $(VIVADO) && vivado -mode batch -notrace \
	 -source ../../tests/hdl/single_test.tcl -tclargs $(TEST)

.PHONY: hdl_timing

# ------------------------------------------------------------------------------
# --- FPGA build
# ------------------------------------------------------------------------------
APP_FILE = $(TOP)/apps/$(APP_NAME)
BUILD_DIR = $(TOP)/build

# Extract FMC and SFP design names from config file

CARRIER_FPGA_TARGETS = carrier-fpga carrier-ip
FPGA_BUILD_DIRS = $(patsubst %,$(BUILD_DIR)/apps/%/FPGA,$(APPS))

$(BUILD_DIR)/apps/%/FPGA: apps
	mkdir -p $@ ; \
	$(MAKE) -C $@ -f $(TARGET_DIR)/Makefile VIVADO=$(VIVADO) \
	    TOP=$(TOP) TARGET_DIR=$(TARGET_DIR) BUILD_DIR=$@ \
	    IP_DIR=$(IP_DIR)

$(CARRIER_FPGA_TARGETS) $(IP_DIR): $(FPGA_BUILD_DIRS)

SLOW_FPGA_BUILD_DIRS = $(patsubst %,$(BUILD_DIR)/apps/%/SlowFPGA,$(APPS))

$(BUILD_DIR)/apps/%/SlowFPGA: tools/virtexHex2Bin apps
	mkdir -p $@ ; \
	source $(ISE)  &&  $(MAKE) -C $@ -f $(TARGET_DIR)/SlowFPGA/Makefile \
            TOP=$(TOP) SRC_DIR=$(TARGET_DIR)/SlowFPGA BOARD=$(BOARD) mcs \
            BUILD_DIR=$@

slow-fpga: $(SLOW_FPGA_BUILD_DIRS)


tools/virtexHex2Bin : tools/virtexHex2Bin.c
	gcc -o $@ $<

.PHONY: carrier-fpga slow-fpga run-tests

# ------------------------------------------------------------------------------
# Build installation package
# ------------------------------------------------------------------------------

FPGA_LISTS=$(patsubst %,$(BUILD_DIR)/apps/%/etc/panda-fpga.list,$(APPS))

$(BUILD_DIR)/apps/%/etc/panda-fpga.list: apps
	$(MAKE_ZPKG) -t $(BUILD_DIR) -b $(BUILD_DIR) -d $(BUILD_DIR) \
            $@ $(filter PandABox-%, "$(subst /, ,$@)")-$(GIT_VERSION)


zpkg: $(FPGA_LISTS) $(FIRMWARE_BUILD)



.PHONY: zpkg

# ------------------------------------------------------------------------------

$(BUILD_DIR)/%:
	mkdir -p $@

# Clean

clean:
	rm -rf $(BUILD_DIR) $(DOCS_BUILD_DIR)
	find -name '*.pyc' -delete

.PHONY: clean
