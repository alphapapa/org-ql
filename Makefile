# * Arguments

# For consistency, we use only var=val options, not hyphen-prefixed options.

ifdef autoinstall
	AUTOINSTALL = "--auto-install"
endif

ifdef sandbox
	SANDBOX = "--sandbox"
endif

# ** Verbosity

# Since the "-v" in "make -v" gets intercepted by Make itself, we have
# to use a variable.

verbose = $(v)

ifneq (,$(findstring vv,$(verbose)))
	VERBOSE = "-vv"
else ifneq (,$(findstring v,$(verbose)))
	VERBOSE = "-v"
endif

# * Rules

%:
	@./makem.sh $(VERBOSE) $(SANDBOX) $(AUTOINSTALL) $(@)
