# this counts Rslt/, Search/, and the top-level Util.hs

find Rslt/ Search/ -name "*.hs" | xargs egrep -v "^ *($|import |--|\{-#|module |, *module )" | wc
egrep -v "^ *($|import |--|\{-#|module |, *module )" Util.hs | wc
