#!/bin/sh

aclocal -I m4
automake -ac --foreign
autoconf -i
