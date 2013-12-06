#!/bin/bash

conky -c ~/.conky/bottombar_left.conky | dzen2 -x 0 -y 1062 -h 18 -w 960 -ta 'l' -e '' &
