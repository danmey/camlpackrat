#!/bin/sh

aclocal
automake -ac --foreign
autoconf -i

cd src
./autogen.sh
cd ..

cd examples
./autogen.sh
cd ..
