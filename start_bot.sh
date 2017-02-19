#!/bin/sh
make

if [ $? -eq 0 ]; then
  erl -pa ebin deps/jiffy/ebin/ -s malebot
fi
