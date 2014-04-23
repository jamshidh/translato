{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-----------------------------------------------------------------------------
--
-- Module      :  Convertable
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Convertable (
    Convertable(..)
) where



class Convertable from to where
    convert::from->to

instance Convertable Int String where
    convert = show

instance Convertable String String where
    convert = id

instance Convertable a (Maybe a) where
    convert x = Just x

instance Convertable (Maybe a) a where
    convert (Just x) = x

instance Convertable String Int where
    convert = read

instance Convertable Int Bool where
    convert = \i -> case i of
                        0 -> False
                        _ -> True

instance Convertable Bool Int where
    convert = \b -> if b then 1 else 0

instance Convertable Int Int where
    convert = id

instance Convertable Bool String where
    convert = show

instance Convertable String Bool where
    convert = read


