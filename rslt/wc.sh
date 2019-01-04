#!/bin/bash

wc src/*hs src/*/*.hs

echo "Count if you skip blank lines, tests, comments, imports, module statements, language pragmas:"
find src -name "*.hs" | egrep -v "Test|Obsolete" | xargs egrep -v "(^ *--|^ *$|^import|^module|^ *{-# *LANGUAGE)" | wc
