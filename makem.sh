#!/bin/bash

# * makem.sh --- Script to aid building and testing Emacs Lisp packages

# * Commentary:


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

# * Safety

# NOTE: These are disabled by default in this template but should be
# enabled when feasible.  Documentation is from the Bash man page.

# ** errexit

# Exit immediately if a pipeline (which may consist of a single simple
# command), a list, or a compound command (see SHELL GRAMMAR above),
# exits with a non-zero status.  The shell does not exit if the
# command that fails is part of the command list immediately following
# a while or until keyword, part of the test following the if or elif
# reserved words, part of any command executed in a && or || list
# except the command follow‐ ing the final && or ||, any command in a
# pipeline but the last, or if the command's return value is being
# inverted with !.  If a compound command other than a subshell
# returns a non-zero status because a command failed while -e was
# being ignored, the shell does not exit.  A trap on ERR, if set, is
# executed before the shell exits.  This option applies to the shell
# environment and each subshell environment separately (see COMMAND
# EXECUTION ENVIRONMENT above), and may cause subshells to exit before
# executing all the commands in the subshell.

# If a compound command or shell function executes in a context where
# -e is being ignored, none of the commands executed within the
# compound command or function body will be affected by the -e
# setting, even if -e is set and a command returns a failure status.
# If a compound command or shell function sets -e while executing in a
# context where -e is ignored, that setting will not have any effect
# until the compound command or the command containing the function
# call completes.

# set -o errexit

# ** nounset

# Treat unset variables and parameters other than the special
# parameters "@" and "*" as an error when performing parameter
# expansion.  If expansion is attempted on an unset variable or
# parameter, the shell prints an error message, and, if not
# interactive, exits with a non-zero status.

# NOTE: When this is not enabled, individual variables can be required
# to be set by using "${var:?}" parameter expansion syntax.

# set -o nounset

# ** pipefail

# If set, the return value of a pipeline is the value of the last
# (rightmost) command to exit with a non-zero status, or zero if all
# commands in the pipeline exit successfully.  This option is disabled
# by default.

# set -o pipefail

# * Elisp

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

# * Functions

# ** Emacs

function run_emacs {
    debug "run_emacs: emacs -Q --batch --load=$package_initialize_file -L \"$load_path\" $@"
    if [[ $debug_load_path ]]
    then
        debug $(emacs -Q --batch \
                      --load=$package_initialize_file \
                      -L "$load_path" \
                      --eval "(message \"LOAD-PATH: %s\" load-path)" \
                      2>&1)
    fi

    output_file=$(mktemp)
    emacs -Q --batch  \
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
    project-elisp-files | egrep -v '^tests?/test-?'
}

function project-test-files {
    # Echo list of Elisp test files.
    project-elisp-files | egrep '^tests?/test-?'
}

function exclude-files {
    # Filter out paths (STDIN) which should be excluded by default.
    egrep -v "(/\.cask/|-autoloads.el)"
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
    error "$@"
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

Options:
  -d, --debug    Print debug info.
  -h, --help     I need somebody!
  -v, --verbose  Increase verbosity, up to -vv.

  --debug-load-path  Print load-path.

  -f FILE, --file FILE  Check FILE in addition to discovered files.

  --no-color        Disable color output.
  -C, --no-compile  Don't compile files automatically.

Source files are automatically discovered from git, or may be
specified with options.
EOF
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
            || error "Compiling failed."
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
    # Run tests.
    test-ert
    test-buttercup
}

function test-buttercup {
    compile || die "Compilation required for tests."

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
    compile || die "Compilation required for tests."

    verbose 1 "Running ERT tests..."
    debug "Test files: ${project_test_files[@]}"

    run_emacs \
        $(load-files-args "${project_test_files[@]}") \
        -f ert-run-tests-batch-and-exit \
        && success "ERT tests finished without errors." \
            || error "ERT tests failed."
}

# * Defaults

# TODO: Disable color if not outputting to a terminal.
color=true
errors=0
verbose=0

compile=true
load_path="."

# TODO: Option to not byte-compile test files.
project_byte_compile_files=($(project-elisp-files))
project_source_files=($(project-source-files))
project_test_files=($(project-test-files))

package_initialize_file="$(elisp-package-initialize-file)"
temp_paths+=("$package_initialize_file")

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

# * Args

args=$(getopt -n "$0" -o dhvf:C -l debug,debug-load-path,help,verbose,file:,no-color,no-compile -- "$@") || { usage; exit 1; }
eval set -- "$args"

while true
do
    case "$1" in
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

for rule in "${rest[@]}"
do
    if type "$rule" 2>/dev/null | grep "$rule is a function" &>/dev/null
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

if [[ $errors -gt 0 ]]
then
    log_color red "Finished with $errors errors."
else
    success "Finished without errors."
fi

exit $errors
