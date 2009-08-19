#!/bin/sh

aclocal
automake -ac
autoconf -i
cd src
./autogen.sh
cd ..
