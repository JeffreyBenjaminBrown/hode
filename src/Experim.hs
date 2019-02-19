{-# LANGUAGE ScopedTypeVariables #-}

import Text.Megaparsec

import Hash.Convert
import Hash.HParse
import Hash.HTypes
import Rslt.Edit
import Rslt.Index
import Rslt.RTypes
import Rslt.RUtil


expr :: String -> Either String Expr
expr s = do (pr :: PRel) <- either (\s -> Left $ show s) Right
                            $ parse pRel "expr" s
            pRelToHExpr pr >>= hExprToExpr

--r <- return $ mkRslt mempty
--e <- return $ hExprToExpr <$> ( pRelToHExpr <$> parse pRel "?" "a" )
--
--x <- lookupInsert 

