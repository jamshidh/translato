-----------------------------------------------------------------------------
--
-- Module      :  ParseError
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

module ParseError (
    ParseError (ParseError),
    errorPosition,
    description,
    showErrors,
    Position (Position),
    line,
    column,
    filename
) where

import Data.List

showErrors::[ParseError]->String
showErrors errors = show (errorPosition (head lastErrors)) ++ "\n    --"
    ++ intercalate "\n    --" (map description lastErrors)
        where lastErrors = (filter ((maximum errors) ==) errors)


-- Position is a location in the input....

data Position = Position { line::Integer, column::Integer, filename::String }

instance Eq Position where
    p1 == p2 = (line p1 == line p2) && (column p1 == column p2)

instance Ord Position where
    p1 <= p2 | line p1 < line p2 = True
    p1 <= p2 | line p1 > line p2 = False
    p1 <= p2 | line p1 == line p2 = column p1 <= column p2

instance Show Position where
    show p = filename p ++ ": line " ++ show (line p) ++ ", column " ++ show (column p)

---------------------

data ParseError = ParseError { errorPosition::Maybe Position, description::String }

-- Error comparison is based on location in file....  Not severity
-- e1 == e2 if they are at the same place, etc.
-- (in "many worlds", this often corresponds to severity....  An earlier error can often be
-- erased by taking a different world path.  If an error occurs and no path is available, it is the
-- last one dealt with before failing, and probably the one you want to see).


instance Eq ParseError where
    e1 == e2 = errorPosition e1 == errorPosition e2

instance Ord ParseError where
    e1 <= e2 = errorPosition e1 <= errorPosition e2


