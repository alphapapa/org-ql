# * Verbosity

# Since the "-v" in "make -v" gets intercepted by Make itself, we have
# to use a variable.

verbose = $(v)

ifneq (,$(findstring vv,$(verbose)))
	VERBOSE = "-vv"
else ifneq (,$(findstring v,$(verbose)))
	VERBOSE = "-v"
endif

# * Rules

# Unless/until I add Cask support to makem.sh, I'm keeping this,
# because Cask lets me run the tests with a different version of Org
# than I have installed personally.
cask:
	@cask exec buttercup -L .

%:
	@./makem.sh $(VERBOSE) $(@)
