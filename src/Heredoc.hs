module Heredoc (here) where

import Language.Haskell.TH.Quote

here :: QuasiQuoter
here = QuasiQuoter hereDocExp hereDocPat

hereDocExp s = dataToExpQ (const Nothing) s

hereDocPat = error "heredoc patterns not implemented"

