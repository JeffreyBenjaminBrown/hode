#!/bin/bash

wc src/*hs src/*/*.hs

echo "Count if you skip blank lines, tests, comments, imports, module statements, language pragmas:"
find src -name "*.hs" | grep -v Test | xargs egrep -v "(^ *--|^ *$|^import|^module|^ *{-# *LANGUAGE)" | wc
