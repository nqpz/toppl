module Toppl.Runner
  ( Passes
  , Prolog
  , transform
  , render
  ) where

import Data.Text (Text)

import Toppl.Base
import qualified Toppl.Parser as Parser
import qualified Toppl.P0 as P0
import qualified Toppl.P1 as P1
import qualified Toppl.P2 as P2
import qualified Toppl.P3 as P3
import qualified Toppl.P4 as P4
import qualified Toppl.P5 as P5
import qualified Toppl.P6 as P6
import qualified Toppl.P7 as P7


type P p = Either Error p
type Passes = (P P0.Prolog,
               (P P1.Prolog,
                (P P2.Prolog,
                 (P P3.Prolog,
                  (P P4.Prolog,
                   (P P5.Prolog,
                    (P P6.Prolog,
                     (P P7.Prolog,
                      ()))))))))

type Prolog = P7.Prolog

transform :: (Text, FilePath) -> Passes
transform input =
  let p0 = Parser.transform input
      p1 = P0.transform <$> p0
      p2 = P1.transform <$> p1
      p3 = P2.transform <$> p2
      p4 = P3.transform <$> p3
      p5 = P4.transform <$> p4
      p6 = P5.transform <$> p5
      p7 = P7.Prolog <$> p6
  in (p0, (p1, (p2, (p3, (p4, (p5, (p6, (p7, ()))))))))
