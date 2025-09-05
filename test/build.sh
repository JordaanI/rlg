#!/bin/sh
gsc -cc-options "$(pkg-config --cflags --libs raylib)" -exe main.scm
