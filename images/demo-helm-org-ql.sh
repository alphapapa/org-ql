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
sleep 5

# M-x helm-org-ql RET
key alt+x
sleep 0.2
raw_input "helm-org-ql"
key ctrl+e
sleep 0.5
key Return
sleep 0.5

# "Emacs"
input Emacs
sleep 1

# "tags:Emacs"
key ctrl+a
input 'tags:'
sleep 1.5

# "tags&:Emacs,elisp"
key Left
input "&"
key ctrl+e
input ",elisp"
sleep 2

# "universe"
key ctrl+a ctrl+k
input "universe"
sleep 1

# "tags:universe,space"
key ctrl+a
input "tags:"
key ctrl+e
input ",space"
sleep 1

# "tags&:universe,space"
key ctrl+a Right Right Right Right
input "&"
sleep 2

# "deadline:"
key ctrl+a ctrl+k
input "deadline:"
sleep 2

# "deadline:to=2017-07-06"
input "to=2019-10-06"
sleep 2

# "deadline:to=2017-07-07"
key BackSpace
sleep 0.5
input "7"
sleep 2

# "scheduled:"
key ctrl+a ctrl+k
input "scheduled:"
sleep 2

# "scheduled:on=2017-07-04"
input "on=2019-10-04"
sleep 1

# "scheduled:on=2017-07-05"
key BackSpace
sleep 0.5
input "5"
sleep 2

# property:agenda-group
key ctrl+a ctrl+k
input "property:agenda-group"
sleep 2

# property:agenda-group=plans
input "=plans"
sleep 2

# category:ambition
key ctrl+a ctrl+k
input "category:ambition"
sleep 2

# ts-inactive:
key ctrl+a ctrl+k
input "ts-inactive:"
sleep 2

# closed:
key ctrl+a ctrl+k
input "closed:"
sleep 2

# Show match in persistent buffer
key ctrl+j
sleep 2

# closed:on=2017-07-04 (which is a day too early)
input "on=2019-10-04"
sleep 2

# closed:on=2017-07-05 (which is the closed date)
key BackSpace
input 5
sleep 2

# done:
key ctrl+a ctrl+k
input "done:"
sleep 2

# "todo:"
key ctrl+a ctrl+k
input "todo:"
sleep 2

# "todo: priority:"
input " priority:"
sleep 1

# "todo: priority:A"
input "A"
sleep 1

# "todo: priority:A,B"
input ",B"
sleep 2

# Save to a view buffer
key ctrl+x ctrl+s
sleep 2

# Stop the screencast
key F1
