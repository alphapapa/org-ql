#!/usr/bin/env bash

# * makem.sh --- Script to aid building and testing Emacs Lisp packages

# URL: https://github.com/alphapapa/makem.sh
# Version: 0.7.1

# * Commentary:

# makem.sh is a script that helps to build, lint, and test Emacs Lisp
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

# The script can run Emacs with the developer's local Emacs
# configuration, or with a clean, "sandbox" configuration that can be
# optionally removed afterward.  This is especially helpful when
# upstream dependencies may have released new versions that differ
# from those installed in the developer's personal configuration.

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

Linter- and test-specific rules will error when their linters or tests
are not found.  With -vv, rules that run multiple rules will show a
message for unavailable linters or tests.

Rules:
  all      Run all lints and tests.
  compile  Byte-compile source files.

  lint           Run all linters, ignoring unavailable ones.
  lint-checkdoc  Run checkdoc.
  lint-compile   Byte-compile source files with warnings as errors.
  lint-declare   Run check-declare.
  lint-elsa      Run Elsa (not included in "lint" rule).
  lint-indent    Lint indentation.
  lint-package   Run package-lint.
  lint-regexps   Run relint.

  test, tests           Run all tests, ignoring missing test types.
  test-buttercup        Run Buttercup tests.
  test-ert              Run ERT tests.
  test-ert-interactive  Run ERT tests interactively.

  batch        Run Emacs in batch mode, loading project source and test files
               automatically, with remaining args (after "--") passed to Emacs.
  interactive  Run Emacs interactively, loading project source and test files
               automatically, with remaining args (after "--") passed to Emacs.

Options:
  -d, --debug    Print debug info.
  -h, --help     I need somebody!
  -v, --verbose  Increase verbosity, up to -vvv.
  --no-color     Disable color output.

  --debug-load-path  Print load-path from inside Emacs.

  -E, --emacs PATH  Run Emacs at PATH.

  -e, --exclude FILE  Exclude FILE from linting and testing.
  -f, --file FILE     Check FILE in addition to discovered files.

  -c, --compile-batch  Batch-compile files (instead of separately; quicker, but
                                            may hide problems).
  -C, --no-compile     Don't compile files automatically.

Sandbox options:
  -s[DIR], --sandbox[=DIR]  Run Emacs with an empty config in a sandbox DIR.
                            If DIR does not exist, make it.  If DIR is not
                            specified, use a temporary sandbox directory and
                            delete it afterward, implying --install-deps and
                            --install-linters.
  --install-deps            Automatically install package dependencies.
  --install-linters         Automatically install linters.
  -i, --install PACKAGE     Install PACKAGE before running rules.

  An Emacs version-specific subdirectory is automatically made inside
  the sandbox, allowing testing with multiple Emacs versions.  When
  specifying a sandbox directory, use options --install-deps and
  --install-linters on first-run and omit them afterward to save time.

Source files are automatically discovered from git, or may be
specified with options.  Package dependencies are discovered from
"Package-Requires" headers in source files, from -pkg.el files, and
from a Cask file.

Checkdoc's spell checker may not recognize some words, causing the
`lint-checkdoc' rule to fail.  Custom words can be added in file-local
or directory-local variables using the variable
`ispell-buffer-session-localwords', which should be set to a list of
strings.
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

function elisp-elint-file {
    local file=$(mktemp)
    cat >$file <<EOF
(require 'cl-lib)
(require 'elint)
(defun makem-elint-file (file)
  (let ((errors 0))
    (cl-letf (((symbol-function 'orig-message) (symbol-function 'message))
              ((symbol-function 'message) (symbol-function 'ignore))
              ((symbol-function 'elint-output)
               (lambda (string)
                 (cl-incf errors)
                 (orig-message "%s" string))))
      (elint-file file)
      ;; NOTE: \`errors' is not actually the number of errors, because
      ;; it's incremented for non-error header strings as well.
      (kill-emacs errors))))
EOF
    echo "$file"
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
              ;; Return nil because we *are* generating a buffered list of errors.
              nil))))
    (put 'ispell-buffer-session-localwords 'safe-local-variable #'list-of-strings-p)
    (mapcar #'checkdoc-file files)
    (when makem-checkdoc-errors-p
      (kill-emacs 1))))

(setq checkdoc-spellcheck-documentation-flag t)
(makem-checkdoc-files-and-exit)
EOF
    echo $file
}

function elisp-byte-compile-file {
    # This seems to be the only way to make byte-compilation signal
    # errors for warnings AND display all warnings rather than only
    # the first one.
    local file=$(mktemp)
    # TODO: Add file to $paths_temp in other elisp- functions.
    paths_temp+=("$file")

    cat >"$file" <<EOF
(defun makem-batch-byte-compile (&rest args)
  ""
  (let ((num-errors 0)
        (num-warnings 0))
    ;; NOTE: Only accepts files as args, not directories.
    (dolist (file command-line-args-left)
      (pcase-let ((\`(,errors ,warnings) (makem-byte-compile-file file)))
        (cl-incf num-errors errors)
        (cl-incf num-warnings warnings)))
    (zerop num-errors)))

(defun makem-byte-compile-file (filename &optional load)
  "Call \`byte-compile-warn', returning the number of errors and the number of warnings."
  (let ((num-warnings 0)
        (num-errors 0))
    (cl-letf (((symbol-function 'byte-compile-warn)
               (lambda (format &rest args)
                 ;; Copied from \`byte-compile-warn'.
                 (cl-incf num-warnings)
                 (setq format (apply #'format-message format args))
                 (byte-compile-log-warning format t :warning)))
              ((symbol-function 'byte-compile-report-error)
               (lambda (error-info &optional fill &rest args)
                 (cl-incf num-errors)
                 ;; Copied from \`byte-compile-report-error'.
                 (setq byte-compiler-error-flag t)
                 (byte-compile-log-warning
                  (if (stringp error-info) error-info
                    (error-message-string error-info))
                  fill :error))))
      (byte-compile-file filename load))
    (list num-errors num-warnings)))
EOF
    echo "$file"
}

function elisp-check-declare-file {
    # Since check-declare doesn't have a batch function that exits
    # non-zero when errors are found, we make one.
    local file=$(mktemp)

    cat >$file <<EOF
(require 'check-declare)

(defun makem-check-declare-files-and-exit ()
  "Run check-declare-files on files remaining on command line, exiting non-zero if there are warnings."
  (let* ((files (mapcar #'expand-file-name command-line-args-left))
         (errors (apply #'check-declare-files files)))
    (when errors
      (with-current-buffer check-declare-warning-buffer
        (print (buffer-string)))
      (kill-emacs 1))))
EOF
    echo $file
}

function elisp-lint-indent-file {
    # This function prints warnings for indentation errors and exits
    # non-zero when errors are found.
    local file=$(mktemp)

    cat >"$file" <<EOF
(require 'cl-lib)

(defun makem-lint-indent-batch-and-exit ()
  "Print warnings for files which are not indented properly, then exit.
Exits non-zero if mis-indented lines are found.  Checks files in
'command-line-args-left'."
  (let ((errors-p))
    (cl-labels ((lint-file (file)
                           (find-file file)
                           (let ((inhibit-message t))
                             (indent-region (point-min) (point-max)))
                           (when buffer-undo-list
                             ;; Indentation changed: warn for each line.
                             (dolist (line (undo-lines buffer-undo-list))
                               (message "%s:%s: Indentation mismatch" (buffer-name) line))
                             (setf errors-p t)))
                (undo-pos (entry)
                           (cl-typecase (car entry)
                             (number (car entry))
                             (string (abs (cdr entry)))))
                (undo-lines (undo-list)
                            ;; Return list of lines changed in UNDO-LIST.
                            (nreverse (cl-loop for elt in undo-list
                                               for pos = (undo-pos elt)
                                               when pos
                                               collect (line-number-at-pos pos)))))
      (mapc #'lint-file (mapcar #'expand-file-name command-line-args-left))
      (when errors-p
        (kill-emacs 1)))))
EOF

    echo "$file"
}

function elisp-package-initialize-file {
    local file=$(mktemp)

    cat >$file <<EOF
(require 'package)
(setq package-archives (list (cons "gnu" "https://elpa.gnu.org/packages/")
                             (cons "melpa" "https://melpa.org/packages/")
                             (cons "melpa-stable" "https://stable.melpa.org/packages/")))
(package-initialize)
EOF
    echo $file
}

# ** Emacs

function run_emacs {
    # NOTE: The sandbox args need to come before the package
    # initialization so Emacs will use the sandbox's packages.
    local emacs_command=(
        "${emacs_command[@]}"
        -Q
        --eval "(setq load-prefer-newer t)"
        "${args_debug[@]}"
        "${args_sandbox[@]}"
        -l $package_initialize_file
        $arg_batch
        "${args_load_paths[@]}"
    )

    # Show debug message with load-path from inside Emacs.
    [[ $debug_load_path ]] \
        && debug $("${emacs_command[@]}" \
                       --batch \
                       --eval "(message \"LOAD-PATH: %s\" load-path)" \
                    2>&1)

    # Set output file.
    output_file=$(mktemp) || die "Unable to make output file."
    paths_temp+=("$output_file")

    # Run Emacs.
    debug "run_emacs: ${emacs_command[@]} $@ &>\"$output_file\""
    "${emacs_command[@]}" "$@" &>"$output_file"

    # Check exit code and output.
    exit=$?
    [[ $exit != 0 ]] \
        && debug "Emacs exited non-zero: $exit"

    [[ $verbose -gt 1 || $exit != 0 ]] \
        && cat $output_file

    return $exit
}

# ** Compilation

function batch-byte-compile {
    debug "batch-byte-compile: ERROR-ON-WARN:$compile_error_on_warn"

    [[ $compile_error_on_warn ]] && local error_on_warn=(--eval "(setq byte-compile-error-on-warn t)")

    run_emacs \
        --load "$(elisp-byte-compile-file)" \
        "${error_on_warn[@]}" \
        --eval "(unless (makem-batch-byte-compile) (kill-emacs 1))" \
        "$@"
}

function byte-compile-file {
    debug "byte-compile: ERROR-ON-WARN:$compile_error_on_warn"
    local file="$1"

    [[ $compile_error_on_warn ]] && local error_on_warn=(--eval "(setq byte-compile-error-on-warn t)")

    # FIXME: Why is the line starting with "&& verbose 3" not indented properly?  Emacs insists on indenting it back a level.
    run_emacs \
        --load "$(elisp-byte-compile-file)" \
        "${error_on_warn[@]}" \
        --eval "(pcase-let ((\`(,num-errors ,num-warnings) (makem-byte-compile-file \"$file\"))) (when (or (and byte-compile-error-on-warn (not (zerop num-warnings))) (not (zerop num-errors))) (kill-emacs 1)))" \
        && verbose 3 "Compiling $file finished without errors." \
            || { verbose 3 "Compiling file failed: $file"; return 1; }
}

# ** Files

function submodules {
    # Echo a list of submodules's paths relative to the repo root.
    # TODO: Parse with bash regexp instead of cut.
    git submodule status | awk '{print $2}'
}

function project-root {
    # Echo the root of the project (or superproject, if running from
    # within a submodule).
    root_dir=$(git rev-parse --show-superproject-working-tree)
    [[ $root_dir ]] || root_dir=$(git rev-parse --show-toplevel)
    [[ $root_dir ]] || error "Can't find repo root."

    echo "$root_dir"
}

function files-project {
    # Echo a list of files in project; or with $1, files in it
    # matching that pattern with "git ls-files".  Excludes submodules.
    [[ $1 ]] && pattern="/$1" || pattern="."

    local excludes
    for submodule in $(submodules)
    do
        excludes+=(":!:$submodule")
    done

    git ls-files -- "$pattern" "${excludes[@]}"
}

function dirs-project {
    # Echo list of directories to be used in load path.
    files-project-feature | dirnames
    files-project-test | dirnames
}

function files-project-elisp {
    # Echo list of Elisp files in project.
    files-project 2>/dev/null \
        | egrep "\.el$" \
        | filter-files-exclude-default \
        | filter-files-exclude-args
}

function files-project-feature {
    # Echo list of Elisp files that are not tests and provide a feature.
    files-project-elisp \
        | grep -E -v "$test_files_regexp" \
        | filter-files-feature
}

function files-project-test {
    # Echo list of Elisp test files.
    files-project-elisp | grep -E "$test_files_regexp"
}

function dirnames {
    # Echo directory names for files on STDIN.
    while read file
    do
        dirname "$file"
    done
}

function filter-files-exclude-default {
    # Filter out paths (STDIN) which should be excluded by default.
    grep -E -v "(/\.cask/|-autoloads\.el|\.dir-locals)"
}

function filter-files-exclude-args {
    # Filter out paths (STDIN) which are excluded with --exclude.
    if [[ ${files_exclude[@]} ]]
    then
        (
            # We use a subshell to set IFS temporarily so we can send
            # the list of files to grep -F.  This is ugly but more
            # correct than replacing spaces with line breaks.  Note
            # that, for some reason, using IFS="\n" or IFS='\n' doesn't
            # work, and a literal line break seems to be required.
            IFS="
"
            grep -Fv "${files_exclude[*]}"
        )
    else
        cat
    fi
}

function filter-files-feature {
    # Read paths on STDIN and echo ones that (provide 'a-feature).
    while read path
    do
        grep -E "^\\(provide '" "$path" &>/dev/null \
            && echo "$path"
    done
}

function args-load-files {
    # For file in $@, echo "--load $file".
    for file in "$@"
    do
        sans_extension=${file%%.el}
        printf -- '--load %q ' "$sans_extension"
    done
}

function args-load-path {
    # Echo load-path arguments.
    for path in $(dirs-project | sort -u)
    do
        printf -- '-L %q ' "$path"
    done
}

function test-files-p {
    # Return 0 if $files_project_test is non-empty.
    [[ "${files_project_test[@]}" ]]
}

function buttercup-tests-p {
    # Return 0 if Buttercup tests are found.
    test-files-p || die "No tests found."
    debug "Checking for Buttercup tests..."

    grep "(require 'buttercup)" "${files_project_test[@]}" &>/dev/null
}

function ert-tests-p {
    # Return 0 if ERT tests are found.
    test-files-p || die "No tests found."
    debug "Checking for ERT tests..."

    # We check for this rather than "(require 'ert)", because ERT may
    # already be loaded in Emacs and might not be loaded with
    # "require" in a test file.
    grep "(ert-deftest" "${files_project_test[@]}" &>/dev/null
}

function package-main-file {
    # Echo the package's main file.
    file_pkg=$(files-project "*-pkg.el" 2>/dev/null)

    if [[ $file_pkg ]]
    then
        # Use *-pkg.el file if it exists.
        echo "$file_pkg"
    else
        # Use shortest filename (a sloppy heuristic that will do for now).
        for file in "${files_project_feature[@]}"
        do
            echo ${#file} "$file"
        done \
            | sort -h \
            | head -n1 \
            | sed -r 's/^[[:digit:]]+ //'
    fi
}

function dependencies {
    # Echo list of package dependencies.

    # Search package headers.  Use -a so grep won't think that an Elisp file containing
    # control characters (rare, but sometimes necessary) is binary and refuse to search it.
    grep -E -a -i '^;; Package-Requires: ' $(files-project-feature) $(files-project-test) \
        | grep -E -o '\([^([:space:]][^)]*\)' \
        | grep -E -o '^[^[:space:])]+' \
        | sed -r 's/\(//g' \
        | grep -E -v '^emacs$'  # Ignore Emacs version requirement.

    # Search Cask file.
    if [[ -r Cask ]]
    then
        grep -E '\(depends-on "[^"]+"' Cask \
            | sed -r -e 's/\(depends-on "([^"]+)".*/\1/g'
    fi

    # Search -pkg.el file.
    if [[ $(files-project "*-pkg.el" 2>/dev/null) ]]
    then
        sed -nr 's/.*\(([-[:alnum:]]+)[[:blank:]]+"[.[:digit:]]+"\).*/\1/p' $(files-project- -- -pkg.el 2>/dev/null)
    fi
}

# ** Sandbox

function sandbox {
    verbose 2 "Initializing sandbox..."

    # *** Sandbox arguments

    # MAYBE: Optionally use branch-specific sandbox?

    # Check or make user-emacs-directory.
    if [[ $sandbox_dir ]]
    then
        # Directory given as argument: ensure it exists.
        if ! [[ -d $sandbox_dir ]]
        then
            debug "Making sandbox directory: $sandbox_dir"
            mkdir -p "$sandbox_dir" || die "Unable to make sandbox dir."
        fi

        # Add Emacs version-specific subdirectory, creating if necessary.
        sandbox_dir="$sandbox_dir/$(emacs-version)"
        if ! [[ -d $sandbox_dir ]]
        then
            mkdir "$sandbox_dir" || die "Unable to make sandbox subdir: $sandbox_dir"
        fi
    else
        # Not given: make temp directory, and delete it on exit.
        local sandbox_dir=$(mktemp -d) || die "Unable to make sandbox dir."
        paths_temp+=("$sandbox_dir")
    fi

    # Make argument to load init file if it exists.
    init_file="$sandbox_dir/init.el"

    # Set sandbox args.  This is a global variable used by the run_emacs function.
    args_sandbox=(
        --title "makem.sh: $(basename $(pwd)) (sandbox: $sandbox_dir)"
        --eval "(setq user-emacs-directory (file-truename \"$sandbox_dir\"))"
        --load package
        --eval "(setq package-user-dir (expand-file-name \"elpa\" user-emacs-directory))"
        --eval "(setq user-init-file (file-truename \"$init_file\"))"
    )

    # Add package-install arguments for dependencies.
    if [[ $install_deps ]]
    then
        local deps=($(dependencies))
        debug "Installing dependencies: ${deps[@]}"

        # Ensure built-in packages get upgraded to newer versions from ELPA.
        args_sandbox_package_install+=(--eval "(setq package-install-upgrade-built-in t)")

        for package in "${deps[@]}"
        do
            args_sandbox_package_install+=(--eval "(package-install '$package)")
        done
    fi

    # Add package-install arguments for linters.
    if [[ $install_linters ]]
    then
        debug "Installing linters: package-lint relint"

        args_sandbox_package_install+=(
            --eval "(package-install 'elsa)"
            --eval "(package-install 'package-lint)"
            --eval "(package-install 'relint)")
    fi

    # *** Install packages into sandbox

    if [[ ${args_sandbox_package_install[@]} ]]
    then
        # Initialize the sandbox (installs packages once rather than for every rule).
        verbose 1 "Installing packages into sandbox..."

        run_emacs \
            --eval "(package-refresh-contents)" \
            "${args_sandbox_package_install[@]}" \
            && success "Packages installed." \
                || die "Unable to initialize sandbox."
    fi

    verbose 2 "Sandbox initialized."
}

# ** Utility

function cleanup {
    # Remove temporary paths (${paths_temp[@]}).

    for path in "${paths_temp[@]}"
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

function echo-unset-p {
    # Echo 0 if $1 is set, otherwise 1.  IOW, this returns the exit
    # code of [[ $1 ]] as STDOUT.
    [[ $1 ]]
    echo $?
}

function ensure-package-available {
    # If package $1 is available, return 0.  Otherwise, return 1, and
    # if $2 is set, give error otherwise verbose.  Outputting messages
    # here avoids repetition in callers.
    local package=$1
    local direct_p=$2

    if ! run_emacs --load $package &>/dev/null
    then
        if [[ $direct_p ]]
        then
            error "$package not available."
        else
            verbose 2 "$package not available."
        fi
        return 1
    fi
}

function ensure-tests-available {
    # If tests of type $1 (like "ERT") are available, return 0.  Otherwise, if
    # $2 is set, give an error and return 1; otherwise give verbose message.  $1
    # should have a corresponding predicate command, like ert-tests-p for ERT.
    local test_name=$1
    local test_command="${test_name,,}-tests-p"  # Converts name to lowercase.
    local direct_p=$2

    if ! $test_command
    then
        if [[ $direct_p ]]
        then
            error "$test_name tests not found."
        else
            verbose 2 "$test_name tests not found."
        fi
        return 1
    fi
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
    local color_name=$1
    shift
    echo_color $color_name "LOG ($(ts)): $@" >&2
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
        [[ $1 -eq 1 ]] && local color_name=blue
        [[ $1 -eq 2 ]] && local color_name=cyan
        [[ $1 -ge 3 ]] && local color_name=white

        shift
        log_color $color_name "$@" >&2
    fi
}

function ts {
    date "+%Y-%m-%d %H:%M:%S"
}

function emacs-version {
    # Echo Emacs version number.

    # Don't use run_emacs function, which does more than we need.
    "${emacs_command[@]}" -Q --batch --eval "(princ emacs-version)" \
        || die "Unable to get Emacs version."
}

function rule-p {
    # Return 0 if $1 is a rule.
    [[ $1 =~ ^(lint-?|tests?)$ ]] \
        || [[ $1 =~ ^(batch|interactive)$ ]] \
        || [[ $(type -t "$2" 2>/dev/null) =~ function ]]
}

# * Rules

# These functions are intended to be called as rules, like a Makefile.
# Some rules test $1 to determine whether the rule is being called
# directly or from a meta-rule; if directly, an error is given if the
# rule can't be run, otherwise it's skipped.

function all {
    verbose 1 "Running all rules..."

    lint
    tests
}

function compile-batch {
    [[ $compile ]] || return 0
    unset compile  # Only compile once.

    verbose 1 "Compiling..."
    verbose 2 "Batch-compiling files..."
    debug "Byte-compile files: ${files_project_byte_compile[@]}"

    batch-byte-compile "${files_project_byte_compile[@]}"
}

function compile-each {
    [[ $compile ]] || return 0
    unset compile  # Only compile once.

    verbose 1 "Compiling..."
    debug "Byte-compile files: ${files_project_byte_compile[@]}"

    local compile_errors
    for file in "${files_project_byte_compile[@]}"
    do
        verbose 2 "Compiling file: $file..."
        byte-compile-file "$file" \
            || compile_errors=t
    done

    [[ ! $compile_errors ]]
}

function compile {
    if [[ $compile = batch ]]
    then
        compile-batch "$@"
    else
        compile-each "$@"
    fi
    local status=$?

    if [[ $compile_error_on_warn ]]
    then
        # Linting: just return status code, because lint rule will print messages.
        [[ $status = 0 ]]
    else
        # Not linting: print messages here.
        [[ $status = 0 ]] \
            && success "Compiling finished without errors." \
                || error "Compiling failed."
    fi
}

function batch {
    # Run Emacs in batch mode with ${args_batch_interactive[@]} and
    # with project source and test files loaded.
    verbose 1 "Executing Emacs with arguments: ${args_batch_interactive[@]}"

    run_emacs \
        $(args-load-files "${files_project_feature[@]}" "${files_project_test[@]}") \
        "${args_batch_interactive[@]}"
}

function interactive {
    # Run Emacs interactively.  Most useful with --sandbox and --install-deps.
    local load_file_args=$(args-load-files "${files_project_feature[@]}" "${files_project_test[@]}")
    verbose 1 "Running Emacs interactively..."
    verbose 2 "Loading files: ${load_file_args//--load /}"

    [[ $compile ]] && compile

    unset arg_batch
    run_emacs \
        $load_file_args \
        --eval "(load user-init-file)" \
        "${args_batch_interactive[@]}"
    arg_batch="--batch"
}

function lint {
    verbose 1 "Linting..."

    lint-checkdoc
    lint-compile
    lint-declare
    # NOTE: Elint doesn't seem very useful at the moment.  See comment
    # in lint-elint function.
    # lint-elint
    lint-indent
    lint-package
    lint-regexps
}

function lint-checkdoc {
    verbose 1 "Linting checkdoc..."

    local checkdoc_file="$(elisp-checkdoc-file)"
    paths_temp+=("$checkdoc_file")

    run_emacs \
        --load="$checkdoc_file" \
        "${files_project_feature[@]}" \
        && success "Linting checkdoc finished without errors." \
            || error "Linting checkdoc failed."
}

function lint-compile {
    verbose 1 "Linting compilation..."

    compile_error_on_warn=true
    compile "${files_project_byte_compile[@]}" \
        && success "Linting compilation finished without errors." \
            || error "Linting compilation failed."
    unset compile_error_on_warn
}

function lint-declare {
    verbose 1 "Linting declarations..."

    local check_declare_file="$(elisp-check-declare-file)"
    paths_temp+=("$check_declare_file")

    run_emacs \
        --load "$check_declare_file" \
        -f makem-check-declare-files-and-exit \
        "${files_project_feature[@]}" \
        && success "Linting declarations finished without errors." \
            || error "Linting declarations failed."
}

function lint-elsa {
    verbose 1 "Linting with Elsa..."

    # MAYBE: Install Elsa here rather than in sandbox init, to avoid installing
    # it when not needed.  However, we should be careful to be clear about when
    # packages are installed, because installing them does execute code.
    run_emacs \
        --load elsa \
        -f elsa-run-files-and-exit \
        "${files_project_feature[@]}" \
        && success "Linting with Elsa finished without errors." \
            || error "Linting with Elsa failed."
}

function lint-elint {
    # NOTE: Elint gives a lot of spurious warnings, apparently because it doesn't load files
    # that are `require'd, so its output isn't very useful.  But in case it's improved in
    # the future, and since this wrapper code already works, we might as well leave it in.
    verbose 1 "Linting with Elint..."

    local errors=0
    for file in "${files_project_feature[@]}"
    do
        verbose 2 "Linting with Elint: $file..."
        run_emacs \
            --load "$(elisp-elint-file)" \
            --eval "(makem-elint-file \"$file\")" \
            && verbose 3 "Linting with Elint found no errors." \
                || { error "Linting with Elint failed: $file"; ((errors++)) ; }
    done

    [[ $errors = 0 ]] \
        && success "Linting with Elint finished without errors." \
            || error "Linting with Elint failed."
}

function lint-indent {
    verbose 1 "Linting indentation..."

    # We load project source files as well, because they may contain
    # macros with (declare (indent)) rules which must be loaded to set
    # indentation.

    run_emacs \
        --load "$(elisp-lint-indent-file)" \
        $(args-load-files "${files_project_feature[@]}" "${files_project_test[@]}") \
        --funcall makem-lint-indent-batch-and-exit \
        "${files_project_feature[@]}" "${files_project_test[@]}" \
        && success "Linting indentation finished without errors." \
            || error "Linting indentation failed."
}

function lint-package {
    ensure-package-available package-lint $1 || return $(echo-unset-p $1)

    verbose 1 "Linting package..."

    run_emacs \
        --load package-lint \
        --eval "(setq package-lint-main-file \"$(package-main-file)\")" \
        --funcall package-lint-batch-and-exit \
        "${files_project_feature[@]}" \
        && success "Linting package finished without errors." \
            || error "Linting package failed."
}

function lint-regexps {
    ensure-package-available relint $1 || return $(echo-unset-p $1)

    verbose 1 "Linting regexps..."

    run_emacs \
        --load relint \
        --funcall relint-batch \
        "${files_project_source[@]}" \
        && success "Linting regexps finished without errors." \
            || error "Linting regexps failed."
}

function tests {
    verbose 1 "Running all tests..."

    test-ert
    test-buttercup
}

function test-ert-interactive {
    verbose 1 "Running ERT tests interactively..."

    unset arg_batch
    run_emacs \
        $(args-load-files "${files_project_test[@]}") \
        --eval "(ert-run-tests-interactively t)"
    arg_batch="--batch"
}

function test-buttercup {
    ensure-tests-available Buttercup $1 || return $(echo-unset-p $1)
    compile || die

    verbose 1 "Running Buttercup tests..."

    local buttercup_file="$(elisp-buttercup-file)"
    paths_temp+=("$buttercup_file")

    run_emacs \
        $(args-load-files "${files_project_test[@]}") \
        --load "$buttercup_file" \
        --eval "(progn (setq backtrace-on-error-noninteractive nil) (buttercup-run))" \
        && success "Buttercup tests finished without errors." \
            || error "Buttercup tests failed."
}

function test-ert {
    ensure-tests-available ERT $1 || return $(echo-unset-p $1)
    compile || die

    verbose 1 "Running ERT tests..."
    debug "Test files: ${files_project_test[@]}"

    run_emacs \
        $(args-load-files "${files_project_test[@]}") \
        -f ert-run-tests-batch-and-exit \
        && success "ERT tests finished without errors." \
            || error "ERT tests failed."
}

# * Defaults

test_files_regexp='^((tests?|t)/)|-tests?.el$|^test-'

emacs_command=("emacs")
errors=0
verbose=0
compile=true
arg_batch="--batch"
compile=each

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

# ** Package system args

args_package_archives=(
    --eval "(add-to-list 'package-archives '(\"gnu\" . \"https://elpa.gnu.org/packages/\") t)"
    --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)"
)

args_package_init=(
    --eval "(package-initialize)"
)

# * Args

args=$(getopt -n "$0" \
              -o dhce:E:i:s::vf:C \
              -l compile-batch,exclude:,emacs:,install-deps,install-linters,debug,debug-load-path,help,install:,verbose,file:,no-color,no-compile,sandbox:: \
              -- "$@") \
    || { usage; exit 1; }
eval set -- "$args"

while true
do
    case "$1" in
        --install-deps)
            install_deps=true
            ;;
        --install-linters)
            install_linters=true
            ;;
        -d|--debug)
            debug=true
            verbose=2
            args_debug=(--eval "(setq init-file-debug t)"
                        --eval "(setq debug-on-error t)")
            ;;
        --debug-load-path)
            debug_load_path=true
            ;;
        -h|--help)
            usage
            exit
            ;;
        -c|--compile-batch)
            debug "Compiling files in batch mode"
            compile=batch
            ;;
        -E|--emacs)
            shift
            emacs_command=($1)
            ;;
        -i|--install)
            shift
            args_sandbox_package_install+=(--eval "(package-install '$1)")
            ;;
        -s|--sandbox)
            sandbox=true
            shift
            sandbox_dir="$1"

            if ! [[ $sandbox_dir ]]
            then
                debug "No sandbox dir: installing dependencies."
                install_deps=true
            else
                debug "Sandbox dir: $1"
            fi
            ;;
        -v|--verbose)
            ((verbose++))
            ;;
        -e|--exclude)
            shift
            debug "Excluding file: $1"
            files_exclude+=("$1")
            ;;
        -f|--file)
            shift
            args_files+=("$1")
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

# Set package elisp (which depends on --no-org-repo arg).
package_initialize_file="$(elisp-package-initialize-file)"
paths_temp+=("$package_initialize_file")

# * Main

trap cleanup EXIT INT TERM

# Change to project root directory first.
cd "$(project-root)"

# Discover project files.
files_project_feature=($(files-project-feature))
files_project_test=($(files-project-test))
files_project_byte_compile=("${files_project_feature[@]}" "${files_project_test[@]}")

if [[ ${args_files[@]} ]]
then
    # Add specified files.
    files_project_feature+=("${args_files[@]}")
    files_project_byte_compile+=("${args_files[@]}")
fi

debug "EXCLUDING FILES: ${files_exclude[@]}"
debug "FEATURE FILES: ${files_project_feature[@]}"
debug "TEST FILES: ${files_project_test[@]}"
debug "BYTE-COMPILE FILES: ${files_project_byte_compile[@]}"
debug "PACKAGE-MAIN-FILE: $(package-main-file)"

if ! [[ ${files_project_feature[@]} ]]
then
    error "No files specified and not in a git repo."
    exit 1
fi

# Set load path.
args_load_paths=($(args-load-path))
debug "LOAD PATH ARGS: ${args_load_paths[@]}"

# If rules include linters and sandbox-dir is unspecified, install
# linters automatically.
if [[ $sandbox && ! $sandbox_dir ]] && [[ "${rest[@]}" =~ lint ]]
then
    debug "Installing linters automatically."
    install_linters=true
fi

# Initialize sandbox.
[[ $sandbox ]] && sandbox

# Run rules.
for rule in "${rest[@]}"
do
    if [[ $batch || $interactive ]]
    then
        debug "Adding batch/interactive argument: $rule"
        args_batch_interactive+=("$rule")

    elif [[ $rule = batch ]]
    then
        # Remaining arguments are passed to Emacs.
        batch=true
    elif [[ $rule = interactive ]]
    then
        # Remaining arguments are passed to Emacs.
        interactive=true

    elif type -t "$rule" 2>/dev/null | grep function &>/dev/null
    then
        # Pass called-directly as $1 to indicate that the rule is
        # being called directly rather than from a meta-rule.
        $rule called-directly
    elif [[ $rule = test ]]
    then
        # Allow the "tests" rule to be called as "test".  Since "test"
        # is a shell builtin, this workaround is required.
        tests
    else
        error "Invalid rule: $rule"
    fi
done

# Batch/interactive rules.
[[ $batch ]] && batch
[[ $interactive ]] && interactive

if [[ $errors -gt 0 ]]
then
    log_color red "Finished with $errors errors."
else
    success "Finished without errors."
fi

exit $errors
