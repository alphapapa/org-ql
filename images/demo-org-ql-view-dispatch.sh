#!/bin/bash

# NOTE: Run "setxkbmap us" before running.  See https://github.com/jordansissel/xdotool/issues/49

# NOTE: <f1> should be bound to this function in the Emacs window, like this:

# (global-set-key [f1]
#  (defun gif-screencast-start-or-stop ()
#   (interactive)
#   (if gif-screencast-mode
#       (progn
#        (gif-screencast-stop)
#        (setq gc-cons-threshold gc-cons-threshold-original))
#       (setq gc-cons-threshold-original gc-cons-threshold)
#       (setq gc-cons-threshold (* 1024 1024 500))
#       (gif-screencast))))

# * Functions

function ensure_window_name {
    if ! [[ $(xdotool getwindowfocus getwindowname) = $1 ]]
    then
        echo "Wrong window!" >&2
        exit 1
    fi
}

function key {
    xdotool key "$@"
}

function input {
    raw_input "$@"

    # Correct for Helm's input idle delay by sending extra commands to
    # make the screenshots be taken.  Not sure why two are necessary,
    # but they seem to be.
    key ctrl+p
    sleep 0.26
    key ctrl+p
}

function raw_input {
    xdotool type --delay 200 "$@"
}

function view-dispatch {
    key v
    sleep 2
}
function view-refresh {
    key r
    sleep 2
}

function clear-input {
    key ctrl+a ctrl+k
    sleep 0.5
}
function set-field {
    key $1
    shift
    sleep 1
    clear-input
    raw_input "$@"
    sleep 1
    key Return
    sleep 1
}

function set-field-with-unique-completion {
    # "$@" should be a unique completion.
    key $1
    shift
    sleep 0.5
    clear-input
    key Tab
    sleep 0.5
    raw_input "$@"
    key Tab
    sleep 0.75
    key Return
    sleep 1
}

function complete-in-steps {
    # Assumes input is clear.
    key Tab
    sleep 0.75
    key Tab
    sleep 0.75

    for s in "$@"
    do
        raw_input "$s"
        key Tab
        sleep 0.75
    done
    sleep 1
}

# * Script

# No matter what I try, xdotool is not working properly to focus/raise/select
# a window.  And for some bizarre reason, the "xdotool selectwindow" command
# outputs a completely different window ID than "xdotool search" outputs.  In
# fact, the window ID it outputs does not even appear in the output of
# "xprop" for that window.  I have no idea where it's getting that ID.

# So rather than cleanly selecting the proper window in the script, we have to
# do a hacky workaround by sleeping and checking the name of the active
# window.

sleep 3
ensure_window_name "data.org"

# Start gif-screencast
key F1
sleep 2

# M-x org-ql-view RET, TAB, Ov TAB, Ag TAB, RET
key alt+x
sleep 0.2
raw_input "org-ql-view"
sleep 1
key Return
sleep 1

complete-in-steps Ov Ag
key Return
sleep 2

# Edit view.
view-dispatch
set-field t "Space-related"
set-field q "tags:space,spaceship"
set-field-with-unique-completion g pri
view-refresh

# Save query
view-dispatch
key ctrl+s
sleep 1
key Return
sleep 2

# New query: Ambition
view-dispatch
set-field t Ambition
set-field q "category:ambition"
set-field s "priority"
set-field g "parent"
view-refresh

view-dispatch
set-field t "Upcoming: ambition"
set-field g ts
view-refresh

# Save view.
view-dispatch
key ctrl+s
sleep 0.75
key Return

key F1
