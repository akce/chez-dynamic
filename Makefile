# Chez dynamic GNU Makefile.
#
# This makefile installs the source at the destination and then uses
# (compile-library) to compile in place.
#
# This seems to be quicker than manually compiling lib files and takes care
# of dependency ordering, which I always seem to get wrong anyway.
#
# Written by Akce 2020.
# SPDX-License-Identifier: Unlicence

# Library destination directory. This must be an object directory contained in
# (library-directories).  eg, set in CHEZSCHEMELIBDIRS environment variable.
LIBDIR := $(HOME)/lib

# Path to chez scheme executable.
SCHEME = /usr/bin/chez-scheme

# Scheme compile flags.
SFLAGS = -q

# Path to shell exes.
ECHO = /bin/echo
INSTALL = /usr/bin/install

## Should be no need to edit anything below here.

.PHONY: clean compile install

all: install compile

LIBS =	\
	dynamic.chezscheme.sls

# installed lib sources.
ILIBS = $(addprefix $(LIBDIR)/,$(LIBS))

# compiled libs.
ILIBSO = $(ILIBS:.sls=.so)

$(LIBDIR)/%.sls: %.sls
	$(INSTALL) -D -p $< $@

# Note the library-directories setup below ensures our LIBDIR will be first in the search list.
$(LIBDIR)/%.so: $(LIBDIR)/%.sls
	$(ECHO) '(reset-handler abort) (library-directories "'$(LIBDIR)'") (compile-library "'$<'")' | $(SCHEME) $(SFLAGS)

install: $(ILIBS)

compile: $(ILIBSO)

clean:
	$(RM) -f $(ILIBS) $(ILIBSO)
