-- | A slight modification of
  -- Module      :  Control.Monad.Combinators.Expr
  -- Copyright   :  © 2017–2019 Mark Karpov <markkarpov92@gmail.com>
  -- License     :  BSD 3 clause

{-# LANGUAGE ScopedTypeVariables #-}

module Hash.EitherExpr
--  ( Operator (..)
--  , makeExprParser )
where

import Control.Monad
import Control.Monad.Combinators

-- | This data type specifies operators that work on values of type @a@. An
-- operator is either binary infix or unary prefix or postfix. A binary
-- operator has also an associated associativity.

data Operator m a
  = InfixN  (m (a -> a -> a)) -- ^ Non-associative infix
  | InfixL  (m (a -> a -> a)) -- ^ Left-associative infix
  | InfixR  (m (a -> a -> a)) -- ^ Right-associative infix
  | Prefix  (m (a -> a))      -- ^ Prefix
  | Postfix (m (a -> a))      -- ^ Postfix

data EOperator m e a
  = EInfixN  (m (a -> a -> Either e a)) -- ^ Non-associative infix
  | EInfixL  (m (a -> a -> Either e a)) -- ^ Left-associative infix
  | EInfixR  (m (a -> a -> Either e a)) -- ^ Right-associative infix
  | EPrefix  (m (a -> Either e a))      -- ^ Prefix
  | EPostfix (m (a -> Either e a))      -- ^ Postfix

-- | @'makeExprParser' term table@ builds an expression parser for terms
-- @term@ with operators from @table@, taking the associativity and
-- precedence specified in the @table@ into account.
--
-- @table@ is a list of @[Operator m a]@ lists. The list is ordered in
-- descending precedence. All operators in one list have the same precedence
-- (but may have different associativity).
--
-- Prefix and postfix operators of the same precedence associate to the left
-- (i.e. if @++@ is postfix increment, than @-2++@ equals @-1@, not @-3@).
--
-- Unary operators of the same precedence can only occur once (i.e. @--2@ is
-- not allowed if @-@ is prefix negate). If you need to parse several prefix
-- or postfix operators in a row, (like C pointers—@**i@) you can use this
-- approach:
--
-- > manyUnaryOp = foldr1 (.) <$> some singleUnaryOp
--
-- This is not done by default because in some cases allowing repeating
-- prefix or postfix operators is not desirable.
--
-- If you want to have an operator that is a prefix of another operator in
-- the table, use the following (or similar) wrapper (Megaparsec example):
--
-- > op n = (lexeme . try) (string n <* notFollowedBy punctuationChar)
--
-- 'makeExprParser' takes care of all the complexity involved in building an
-- expression parser. Here is an example of an expression parser that
-- handles prefix signs, postfix increment and basic arithmetic:
--
-- > expr = makeExprParser term table <?> "expression"
-- >
-- > term = parens expr <|> integer <?> "term"
-- >
-- > table = [ [ prefix  "-"  negate
-- >           , prefix  "+"  id ]
-- >         , [ postfix "++" (+1) ]
-- >         , [ binary  "*"  (*)
-- >           , binary  "/"  div  ]
-- >         , [ binary  "+"  (+)
-- >           , binary  "-"  (-)  ] ]
-- >
-- > binary  name f = InfixL  (f <$ symbol name)
-- > prefix  name f = Prefix  (f <$ symbol name)
-- > postfix name f = Postfix (f <$ symbol name)

makeExprParser :: MonadPlus m
  => m a               -- ^ Term parser
  -> [[Operator m a]]  -- ^ Operator table, see 'Operator'
  -> m a               -- ^ Resulting expression parser
makeExprParser = foldl addPrecLevel
{-# INLINEABLE makeExprParser #-}

-- | @addPrecLevel p ops@ adds the ability to parse operators in table @ops@
-- to parser @p@. "Prec" stands for "precedence".

addPrecLevel :: MonadPlus m => m a -> [Operator m a] -> m a
addPrecLevel term ops =
  term' >>= \x -> choice [ras' x, las' x, nas' x, return x]
  where
    (ras, las, nas, prefix, postfix) = foldr splitOp ([],[],[],[],[]) ops
    term' = pTerm (choice prefix) term (choice postfix)
    ras'  = pInfixR (choice ras) term'
    las'  = pInfixL (choice las) term'
    nas'  = pInfixN (choice nas) term'
{-# INLINEABLE addPrecLevel #-}

eAddPrecLevel :: MonadPlus m => m a -> [EOperator m e a] -> m a
eAddPrecLevel term ops =
  term' >>= \x -> choice [ras' x, las' x, nas' x, return x]
  where
    (ras, las, nas, prefix, postfix) = foldr eSplitOp ([],[],[],[],[]) ops
    term' = ePTerm (choice prefix) term (choice postfix)
    ras'  = ePInfixR (choice ras) term'
    las'  = ePInfixL (choice las) term'
    nas'  = ePInfixN (choice nas) term'

-- | @pTerm prefix term postfix@ parses a @term@ surrounded by optional
-- prefix and postfix unary operators. Parsers @prefix@ and @postfix@ are
-- allowed to fail, in this case 'id' is used.

pTerm :: MonadPlus m => m (a -> a) -> m a -> m (a -> a) -> m a
pTerm prefix term postfix = do
  pre  <- option id prefix
  x    <- term
  post <- option id postfix
  return . post . pre $ x
{-# INLINE pTerm #-}

ePTerm :: (Show e, MonadPlus m)
  => m (a -> Either e a)
  -> m (     Either e a)
  -> m (a -> Either e a)
  -> m a
ePTerm prefix term postfix = do
  pre      <- option (Right . id) prefix
  (x :: a) <- term >>= either (\s -> fail $ show s) return
  post     <- option (Right . id) postfix
  either (\s -> fail $ show s) return (pre x)
    >>= either (\s -> fail $ show s) return . post


-- | @pInfixN op p x@ parses non-associative infix operator @op@, then term
-- with parser @p@, then returns result of the operator application on @x@
-- and the term.

pInfixN :: MonadPlus m => m (a -> a -> a) -> m a -> a -> m a
pInfixN op p x = do
  f <- op
  y <- p
  return $ f x y
{-# INLINE pInfixN #-}

ePInfixN :: (Show e, MonadPlus m)
  => m (a -> a -> Either e a) -> m a -> a -> m a
ePInfixN op p x = do
  f <- op
  y <- p
  either (\s -> fail $ show s) return $ f x y

-- | @pInfixL op p x@ parses left-associative infix operator @op@, then term
-- with parser @p@, then returns result of the operator application on @x@
-- and the term.

pInfixL :: MonadPlus m => m (a -> a -> a) -> m a -> a -> m a
pInfixL op p x = do
  f <- op
  y <- p
  let r = f x y
  pInfixL op p r <|> return r
{-# INLINE pInfixL #-}

ePInfixL :: (Show e, MonadPlus m)
  => m (a -> a -> Either e a) -> m a -> a -> m a
ePInfixL op p x = do
  f <- op
  y <- p
  r <- either (\s -> fail $ show s) return $ f x y
  ePInfixL op p r <|> return r

-- | @pInfixR op p x@ parses right-associative infix operator @op@, then
-- term with parser @p@, then returns result of the operator application on
-- @x@ and the term.

pInfixR :: MonadPlus m => m (a -> a -> a) -> m a -> a -> m a
pInfixR op p x = do
  f <- op
  y <- p >>= \r -> pInfixR op p r <|> return r
  return $ f x y
{-# INLINE pInfixR #-}

ePInfixR :: (Show e, MonadPlus m)
  => m (a -> a -> Either e a) -> m a -> a -> m a
ePInfixR op p x = do
  f <- op
  y <- p >>= \r -> ePInfixR op p r <|> return r
  case f x y of Left s -> fail $ show s
                Right v -> return v

type Batch m a =
  ( [m (a -> a -> a)]
  , [m (a -> a -> a)]
  , [m (a -> a -> a)]
  , [m (a -> a)]
  , [m (a -> a)] )

type EBatch m e a =
  ( [m (a -> a -> Either e a)]
  , [m (a -> a -> Either e a)]
  , [m (a -> a -> Either e a)]
  , [m (a -> Either e a)]
  , [m (a -> Either e a)] )

-- | A helper to separate various operators (binary, unary, and according to
-- associativity) and return them in a tuple.

splitOp :: Operator m a -> Batch m a -> Batch m a
splitOp (InfixR  op) (r, l, n, pre, post) = (op:r, l, n, pre, post)
splitOp (InfixL  op) (r, l, n, pre, post) = (r, op:l, n, pre, post)
splitOp (InfixN  op) (r, l, n, pre, post) = (r, l, op:n, pre, post)
splitOp (Prefix  op) (r, l, n, pre, post) = (r, l, n, op:pre, post)
splitOp (Postfix op) (r, l, n, pre, post) = (r, l, n, pre, op:post)

eSplitOp :: EOperator m e a -> EBatch m e a -> EBatch m e a
eSplitOp (EInfixR  op) (r, l, n, pre, post) = (op:r, l, n, pre, post)
eSplitOp (EInfixL  op) (r, l, n, pre, post) = (r, op:l, n, pre, post)
eSplitOp (EInfixN  op) (r, l, n, pre, post) = (r, l, op:n, pre, post)
eSplitOp (EPrefix  op) (r, l, n, pre, post) = (r, l, n, op:pre, post)
eSplitOp (EPostfix op) (r, l, n, pre, post) = (r, l, n, pre, op:post)
