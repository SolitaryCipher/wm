################################################################################
export TMPDIR   = $(HOME)/tmp
export NIX_PATH = nixpkgs=$(shell realpath vendor/nixpkgs)

################################################################################
TMP_DUMMY     = $(TMPDIR)/.dummy
STACK_OPTS    = --stack-yaml=build/stack.yaml
DIST_PATH     = $(shell stack $(STACK_OPTS) path --dist-dir)
PKG_ROOT_PATH = $(shell stack $(STACK_OPTS) path --local-install-root)

################################################################################
ARCH            = $(shell uname -m)
OS              = $(shell uname -s | tr '[A-Z]' '[a-z]')

#CHECKRC_SRC     = $(DIST_PATH)/build/checkrc/checkrc

XMONAD          = $(PKG_ROOT_PATH)/bin/xmonad
XMONADRC_SRC    = $(DIST_PATH)/build/xmonadrc/xmonadrc
XMONADRC_DEST   = $(HOME)/.xmonad/xmonad-$(ARCH)-$(OS)

TAFFYBARRC_SRC  = $(DIST_PATH)/build/taffybarrc/taffybarrc
TAFFYBARRC_DEST = $(dir $(XMONADRC_DEST))/taffybar
TAFFYBARWP_SRC  = scripts/taffybar.sh
TAFFYBARWP_DEST = $(dir $(XMONADRC_DEST))/taffybar-wrapper.sh

################################################################################
.PHONY: all install restart clean test

################################################################################
all: $(TMP_DUMMY)
	stack $(STACK_OPTS) setup
	stack $(STACK_OPTS) build
	#hlint src *.hs

################################################################################
#test:
#	$(CHECKRC_SRC) # Confirm valid configuration.

################################################################################
clean:
	rm -rf .stack-work build/.stack-work
	rm -rf dist dist-newstyle

################################################################################
install: all
	mkdir -p $(dir $(XMONADRC_DEST))
	install -m 0755 $(XMONADRC_SRC) $(XMONADRC_DEST)
	install -m 0755 $(XMONAD) $(dir $(XMONADRC_DEST))/xmonad
	install -m 0755 $(TAFFYBARRC_SRC) $(TAFFYBARRC_DEST)
	install -m 0755 $(TAFFYBARWP_SRC) $(TAFFYBARWP_DEST)
	ln -sTf $(PWD) $(dir $(XMONADRC_DEST))/config

################################################################################
restart: install
	$(XMONADRC_DEST) --restart
	$(TAFFYBARWP_DEST) restart > /dev/null 2>&1 &

################################################################################
$(TMP_DUMMY):
	mkdir -p $(dir $@)
	touch $@
