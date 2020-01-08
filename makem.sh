#!/bin/bash

# * makem.sh --- Script to aid building and testing Emacs Lisp packages

# https://github.com/alphapapa/makem.sh

# * Commentary:

# makem.sh is a script helps to build, lint, and test Emacs Lisp
# packages.  It aims to make linting and testing as simple as possible
# without requiring per-package configuration.

# It works similarly to a Makefile in that "rules" are called to
# perform actions such as byte-compiling, linting, testing, etc.

# Source and test files are discovered automatically from the
# project's Git repo, and package dependencies within them are parsed
# automatically.

# Output is simple: by default, there is no output unless errors
# occur.  With increasing verbosity levels, more detail gives positive
# feedback.  Output is colored by default to make reading easy.

# When desired, emacs-sandbox.sh can be used as a backend, which
# allows package dependencies to be installed automatically into a
# clean Emacs "sandbox" configuration without affecting the
# developer's personal configuration.  This is especially helpful when
# upstream dependencies may have released new versions that differ
# from those installed in the developer's personal configuration.  See
# <https://github.com/alphapapa/emacs-sandbox.sh>.

# * License:

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# * Functions

function usage {
    cat <<EOF
$0 [OPTIONS] RULES...

Rules:
  all      Run all lints and tests.
  compile  Byte-compile source files.

  lint           Run all lints.
  lint-checkdoc  Run checkdoc.
  lint-compile   Byte-compile source files with warnings as errors.
  lint-package   Run package-lint.

  test, tests     Run all tests.
  test-buttercup  Run Buttercup tests.
  test-ert        Run ERT tests.

  These are especially useful with --sandbox:

    batch        Run Emacs in batch mode, loading project source and test files
                 automatically, with remaining args (after "--") passed to Emacs.
    interactive  Run Emacs interactively, loading project source and test files
                 automatically.

Options:
  -d, --debug    Print debug info.
  -h, --help     I need somebody!
  -v, --verbose  Increase verbosity, up to -vv.
  --debug-load-path  Print load-path.

  -f FILE, --file FILE  Check FILE in addition to discovered files.

  --no-color        Disable color output.
  -C, --no-compile  Don't compile files automatically.

Sandbox options:
  These require emacs-sandbox.sh to be on your PATH.  Find it at
  <https://github.com/alphapapa/emacs-sandbox.sh>.

  -s, --sandbox          Run Emacs with emacs-sandbox.sh in a temporary
                         directory (removing directory on exit).
  -S, --sandbox-dir DIR  Use DIR for the sandbox directory (leaving it
                         on exit).  Implies -s.
  --auto-install         Automatically install package dependencies.
  -i, --install PACKAGE  Install PACKAGE before running rules.

Source files are automatically discovered from git, or may be
specified with options.

Package dependencies are discovered from "Package-Requires" headers in
source files and from a Cask file.
EOF
}

# ** Elisp

# These functions return a path to an elisp file which can be loaded
# by Emacs on the command line with -l or --load.

function elisp-buttercup-file {
    # The function buttercup-run, which is called by buttercup-run-discover,
    # signals an error if it can't find any Buttercup test suites.  We don't
    # want that to be an error, so we define advice which ignores that error.
    local file=$(mktemp)
    cat >$file <<EOF
(defun makem-buttercup-run (oldfun &rest r)
  "Call buttercup-run only if \`buttercup-suites' is non-nil."
  (when buttercup-suites
    (apply oldfun r)))

(advice-add #'buttercup-run :around #'makem-buttercup-run)
EOF
    echo $file
}

function elisp-checkdoc-file {
    # Since checkdoc doesn't have a batch function that exits non-zero
    # when errors are found, we make one.
    local file=$(mktemp)

    cat >$file <<EOF
(defvar makem-checkdoc-errors-p nil)

(defun makem-checkdoc-files-and-exit ()
  "Run checkdoc-file on files remaining on command line, exiting non-zero if there are warnings."
  (let* ((files (mapcar #'expand-file-name command-line-args-left))
         (checkdoc-create-error-function
          (lambda (text start end &optional unfixable)
            (let ((msg (concat (checkdoc-buffer-label) ":"
                               (int-to-string (count-lines (point-min) (or start (point-min))))
                               ": " text)))
              (message msg)
              (setq makem-checkdoc-errors-p t)
              (list text start end unfixable)))))
    (mapcar #'checkdoc-file files)
    (when makem-checkdoc-errors-p
      (kill-emacs 1))))

(makem-checkdoc-files-and-exit)
EOF
    echo $file
}

function elisp-package-initialize-file {
    local file=$(mktemp)

    cat >$file <<EOF
(require 'package)
(setq package-archives (list (cons "gnu" "https://elpa.gnu.org/packages/")
                             (cons "melpa" "https://melpa.org/packages/")
                             (cons "melpa-stable" "https://stable.melpa.org/packages/")
                             (cons "org" "https://orgmode.org/elpa/")))
(package-initialize)
(setq load-prefer-newer t)
EOF
    echo $file
}

# ** Emacs

function run_emacs {
    debug "run_emacs: $emacs_command -Q $batch_arg --load=$package_initialize_file -L \"$load_path\" $@"
    if [[ $debug_load_path ]]
    then
        debug $($emacs_command -Q $batch_arg \
                               --load=$package_initialize_file \
                               -L "$load_path" \
                               --eval "(message \"LOAD-PATH: %s\" load-path)" \
                               2>&1)
    fi

    output_file=$(mktemp)
    $emacs_command -Q $batch_arg \
                   --load=$package_initialize_file \
                   -L "$load_path" \
                   "$@" \
        &>$output_file

    exit=$?
    [[ $exit != 0 ]] && debug "Emacs exited non-zero: $exit"
    if [[ $verbose -gt 1 || $exit != 0 ]]
    then
        cat $output_file
    fi
    rm -f $output_file

    return $exit
}

# ** Compilation

function batch-byte-compile {
    debug "batch-byte-compile: ERROR-ON-WARN:$compile_error_on_warn  FILES:$@"

    [[ $compile_error_on_warn ]] && local error_on_warn=(--eval "(setq byte-compile-error-on-warn t)")

    run_emacs \
        "${error_on_warn[@]}" \
        --funcall batch-byte-compile \
        "$@"
}

# ** Files

function project-elisp-files {
    # Echo list of Elisp files in project.
    git ls-files 2>/dev/null | egrep "\.el$" | exclude-files
}

function project-source-files {
    # Echo list of Elisp files that are not tests.
    project-elisp-files | egrep -v "$test_files_regexp" | feature-files
}

function project-test-files {
    # Echo list of Elisp test files.
    project-elisp-files | egrep "$test_files_regexp"
}

function exclude-files {
    # Filter out paths (STDIN) which should be excluded by default.
    egrep -v "(/\.cask/|-autoloads.el|.dir-locals)"
}

function feature-files {
    # Read paths on STDIN and echo ones that (provide 'a-feature).
    while read path
    do
        debug "PATH: $path"
        egrep "^\\(provide '" "$path" &>/dev/null \
            && echo "$path"
    done
}

function load-files-args {
    # For file in $@, echo "--load $file".
    for file in "$@"
    do
        printf -- '--load %q ' "$file"
    done
}

function files_args {
    # For file in STDIN, echo "$file".
    while read file
    do
        printf -- '%q ' "$file"
    done
}

function test-files-p {
    # Return 0 if $project_test_files is non-empty.
    [[ "${project_test_files[@]}" ]]
}

function buttercup-tests-p {
    # Return 0 if Buttercup tests are found.
    test-files-p || die "No tests found."
    debug "Checking for Buttercup tests..."

    grep "(require 'buttercup)" "${project_test_files[@]}" &>/dev/null
}

function ert-tests-p {
    # Return 0 if ERT tests are found.
    test-files-p || die "No tests found."
    debug "Checking for ERT tests..."

    # We check for this rather than "(require 'ert)", because ERT may
    # already be loaded in Emacs and might not be loaded with
    # "require" in a test file.
    grep "(ert-deftest" "${project_test_files[@]}" &>/dev/null
}

function dependencies {
    # Echo list of package dependencies.

    # Search package headers.
    egrep '^;; Package-Requires: ' $(project-source-files) $(project-test-files) \
        | egrep -o '\([^([:space:]][^)]*\)' \
        | egrep -o '^[^[:space:])]+' \
        | sed -r 's/\(//g' \
        | egrep -v '^emacs$'  # Ignore Emacs version requirement.

    # Search Cask file.
    if [[ -r Cask ]]
    then
        egrep '\(depends-on "[^"]+"' Cask \
            | sed -r -e 's/\(depends-on "([^"]+)".*/\1/g'
    fi
}

# ** Utility

function cleanup {
    # Remove temporary paths (${temp_paths[@]}).

    for path in "${temp_paths[@]}"
    do
        if [[ $debug ]]
        then
            debug "Debugging enabled: not deleting temporary path: $path"
        elif [[ -r $path ]]
        then
            rm -rf "$path"
        else
            debug "Temporary path doesn't exist, not deleting: $path"
        fi
    done
}

function echo_color {
    # This allows bold, italic, etc. without needing a function for
    # each variation.
    local color_code="COLOR_$1"
    shift

    if [[ $color ]]
    then
        echo -e "${!color_code}${@}${COLOR_off}"
    else
        echo "$@"
    fi
}
function debug {
    if [[ $debug ]]
    then
        function debug {
            echo_color yellow "DEBUG ($(ts)): $@" >&2
        }
        debug "$@"
    else
        function debug {
            true
        }
    fi
}
function error {
    echo_color red "ERROR ($(ts)): $@" >&2
    ((errors++))
    return 1
}
function die {
    [[ $@ ]] && error "$@"
    exit $errors
}
function log {
    echo "LOG ($(ts)): $@" >&2
}
function log_color {
    local color=$1
    shift
    echo_color $color "LOG ($(ts)): $@" >&2
}
function success {
    if [[ $verbose -ge 2 ]]
    then
        log_color green "$@" >&2
    fi
}
function verbose {
    # $1 is the verbosity level, rest are echoed when appropriate.
    if [[ $verbose -ge $1 ]]
    then
        [[ $1 -eq 1 ]] && local color=blue
        [[ $1 -ge 2 ]] && local color=cyan

        shift
        log_color $color "$@" >&2
    fi
}

function ts {
    date "+%Y-%m-%d %H:%M:%S"
}

# * Rules

# These functions are intended to be called as rules, like a Makefile.

function all {
    verbose 1 "Running all rules..."

    lint
    tests
}

function compile {
    [[ $compile ]] || return 0
    unset compile  # Only compile once.

    verbose 1 "Compiling..."
    debug "Byte-compile files: ${project_byte_compile_files[@]}"

    batch-byte-compile "${project_byte_compile_files[@]}" \
        && success "Compiling finished without errors." \
            || error "Compilation failed."
}

function batch {
    # Run Emacs with $batch_args and with project source and test files loaded.
    verbose 1 "Executing Emacs with arguments: ${batch_args[@]}"

    run_emacs \
        $(load-files-args "${project_source_files[@]}" "${project_test_files[@]}") \
        "${batch_args[@]}"
}

function interactive {
    # Run Emacs interactively.  Most useful with --sandbox and --auto-install.
    unset batch_arg
    run_emacs \
        $(load-files-args "${project_source_files[@]}" "${project_test_files[@]}")
    batch_arg="--batch"
}

function lint {
    verbose 1 "Linting..."

    lint-checkdoc
    lint-compile
    lint-package
}

function lint-checkdoc {
    verbose 1 "Linting checkdoc..."

    local checkdoc_file="$(elisp-checkdoc-file)"
    temp_paths+=("$checkdoc_file")

    run_emacs \
        --load="$checkdoc_file" \
        "${project_source_files[@]}" \
        && success "Linting checkdoc finished without errors." \
            || error "Linting checkdoc failed."
}

function lint-compile {
    verbose 1 "Linting compilation..."

    compile_error_on_warn=true
    batch-byte-compile "${project_byte_compile_files[@]}" \
        && success "Linting compilation finished without errors." \
            || error "Linting compilation failed."
    unset compile_error_on_warn
}

function lint-package {
    verbose 1 "Linting package..."

    run_emacs \
        --load package-lint \
        --funcall package-lint-batch-and-exit \
        "${project_source_files[@]}" \
        && success "Linting package finished without errors." \
            || error "Linting package failed."
}

function tests {
    verbose 1 "Running all tests..."

    test-ert
    test-buttercup
}

function test-buttercup {
    buttercup-tests-p || return 0
    compile || die

    verbose 1 "Running Buttercup tests..."

    local buttercup_file="$(elisp-buttercup-file)"
    temp_paths+=("$buttercup_file")

    run_emacs \
        --load buttercup \
        --load "$buttercup_file" \
        -f buttercup-run-discover \
        && success "Buttercup tests finished without errors." \
            || error "Buttercup tests failed."
}

function test-ert {
    ert-tests-p || return 0
    compile || die

    verbose 1 "Running ERT tests..."
    debug "Test files: ${project_test_files[@]}"

    run_emacs \
        $(load-files-args "${project_test_files[@]}") \
        -f ert-run-tests-batch-and-exit \
        && success "ERT tests finished without errors." \
            || error "ERT tests failed."
}

# * Defaults

test_files_regexp='^(tests?|t)/'
emacs_command="emacs"
errors=0
verbose=0
compile=true
batch_arg="--batch"

# MAYBE: Disable color if not outputting to a terminal.  (OTOH, the
# colorized output is helpful in CI logs, and I don't know if,
# e.g. GitHub Actions logging pretends to be a terminal.)
color=true

# TODO: Using the current directory (i.e. a package's repo root directory) in
# load-path can cause weird errors in case of--you guessed it--stale .ELC files,
# the zombie problem that just won't die.  It's incredible how many different ways
# this problem presents itself.  In this latest example, an old .ELC file, for a
# .EL file that had since been renamed, was present on my local system, which meant
# that an example .EL file that hadn't been updated was able to "require" that .ELC
# file's feature without error.  But on another system (in this case, trying to
# setup CI using GitHub Actions), the old .ELC was not present, so the example .EL
# file was not able to load the feature, which caused a byte-compilation error.

# In this case, I will prevent such example files from being compiled.  But in
# general, this can cause weird problems that are tedious to debug.  I guess
# the best way to fix it would be to actually install the repo's code as a
# package into the sandbox, but doing that would require additional tooling,
# pulling in something like Quelpa or package-build--and if the default recipe
# weren't being used, the actual recipe would have to be fetched off MELPA or
# something, which seems like getting too smart for our own good.

# TODO: Emit a warning if .ELC files that don't match any .EL files are detected.
load_path="."

# ** Colors

COLOR_off='\e[0m'
COLOR_black='\e[0;30m'
COLOR_red='\e[0;31m'
COLOR_green='\e[0;32m'
COLOR_yellow='\e[0;33m'
COLOR_blue='\e[0;34m'
COLOR_purple='\e[0;35m'
COLOR_cyan='\e[0;36m'
COLOR_white='\e[0;37m'

# * Project files

# MAYBE: Option to not byte-compile test files.  (OTOH, byte-compiling reveals many
# errors that would otherwise go unnoticed, so it's worth it to fix the warnings.)
project_source_files=($(project-source-files))
project_test_files=($(project-test-files))
project_byte_compile_files=("${project_source_files[@]}" "${project_test_files[@]}")

package_initialize_file="$(elisp-package-initialize-file)"
temp_paths+=("$package_initialize_file")

# * Args

args=$(getopt -n "$0" \
              -o dhi:sS:vf:C \
              -l auto-install,debug,debug-load-path,help,install:,verbose,file:,no-color,no-compile,sandbox,sandbox-dir: \
              -- "$@") \
    || { usage; exit 1; }
eval set -- "$args"

while true
do
    case "$1" in
        --auto-install)
            auto_install=true
            ;;
        -d|--debug)
            debug=true
            verbose=2
            ;;
        --debug-load-path)
            debug_load_path=true
            ;;
        -h|--help)
            usage
            exit
            ;;
        -i|--install)
            shift
            sandbox_install_packages_args+=(--install "$1")
            ;;
        -s|--sandbox)
            sandbox=true
            ;;
        -S|--sandbox-dir)
            shift
            sandbox=true
            sandbox_dir="$1"
            ;;
        -v|--verbose)
            ((verbose++))
            ;;
        -f|--file)
            shift
            project_source_files+=("$1")
            project_byte_compile_files+=("$1")
            ;;
        --no-color)
            unset color
            ;;
        -C|--no-compile)
            unset compile
            ;;
        --)
            # Remaining args (required; do not remove)
            shift
            rest=("$@")
            break
            ;;
    esac

    shift
done

debug "ARGS: $args"
debug "Remaining args: ${rest[@]}"

# * Main

trap cleanup EXIT INT TERM

if ! [[ ${project_source_files[@]} ]]
then
    error "No files specified and not in a git repo."
    exit 1
fi

if [[ $sandbox ]]
then
    # Setup sandbox.
    type emacs-sandbox.sh &>/dev/null || die "emacs-sandbox.sh not found."

    if ! [[ $sandbox_dir ]]
    then
        # No sandbox dir specified: make temp dir and remove it on exit.
        sandbox_dir=$(mktemp -d) || die "Unable to make temp dir."
        temp_paths+=("$sandbox_dir")
    fi

    sandbox_basic_args=(
        -d "$sandbox_dir"
    )
    [[ $debug ]] && sandbox_basic_args+=(--debug)

    if [[ $auto_install ]]
    then
        # Add dependencies to package install list.
        deps=($(dependencies))
        debug "Installing dependencies: ${deps[@]}"

        for package in "${deps[@]}"
        do
            sandbox_install_packages_args+=(--install $package)
        done
    fi

    if [[ ${sandbox_install_packages_args[@]} ]]
    then
        # Initialize the sandbox (installs packages once rather than for every rule).
        emacs_command="emacs-sandbox.sh ${sandbox_basic_args[@]} ${sandbox_install_packages_args[@]} -- "
        debug "Initializing sandbox..."

        run_emacs || die "Unable to initialize sandbox."
    fi

    # After the sandbox is initialized and packages are installed, set the command
    # to prevent the package lists from being refreshed on each invocation.
    emacs_command="emacs-sandbox.sh ${sandbox_basic_args[@]} --no-refresh-packages -- "

    debug "Sandbox initialized."
fi

# Run rules.
for rule in "${rest[@]}"
do
    if [[ $batch ]]
    then
        debug "Adding batch argument: $rule"
        batch_args+=("$rule")

    elif [[ $rule = batch ]]
    then
        # Remaining arguments are passed to Emacs.
        batch=true
    elif type "$rule" 2>/dev/null | grep "$rule is a function" &>/dev/null
    then
        $rule
    elif [[ $rule = test ]]
    then
        # Allow the "tests" rule to be called as "test".  Since "test"
        # is a shell builtin, this workaround is required.
        tests
    else
        error "Invalid rule: $rule"
    fi
done

# The batch rule.
[[ $batch ]] && batch

if [[ $errors -gt 0 ]]
then
    log_color red "Finished with $errors errors."
else
    success "Finished without errors."
fi

exit $errors
