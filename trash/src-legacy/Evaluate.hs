module Evaluate where

import Types


evaluate :: Data -> OrError ReturnType
evaluate Data {dummy = Dummy {type_sig = Fn _ _}} = Left Error {
  errorType = Custom "can't evaluate a function",
  errorMark = None
}
evaluate Data {currentArgs = args, function = f} = pure $ f args

