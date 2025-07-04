{-# Language MultiWayIf, LambdaCase #-}

module Value where

import Color
import Cell
import Types

instance Show DummyData where
  show Dummy {current_name = name, type_sig = tp} = colour Magenta name <> " : " <> show tp


instance Show SimpleType where
  show Int    = "int"
  show Giff   = "frames"
  show Colour = "color"
  show Direction = "direction"

instance Show Type where
  show (Type x) = colour Blue (show x)
  show (Fn a b) = colour Blue "fn " <> show a <> " " <> show b

isPossible :: Type -> Type -> Bool
isPossible w f@(Fn a b) = w == f || isPossible w b
isPossible w got        = w == got
  
evaluateRealTypes :: Type -> [Marked DummyData] -> Mark -> Either Error DummyData
evaluateRealTypes want x body_mark= case evaluateTypes want x of
  RealError x    -> Left x
  Success (Marked _ x,[]) -> Right x
  Success (Marked m x,xs) -> Left Error {
    errorType = Custom (show x <> " did not expect any arguments"),
    errorMark = m
  }
  Func f        -> Left Error {
    errorType = Custom "TypeMismatch in the first argument",
    errorMark = body_mark
  }

data ComplexError = 
  RealError Error | 
  Func (Marked DummyData -> Error) | 
  Success (Marked DummyData, [Marked DummyData]) 

evaluateTypes :: Type -> [Marked DummyData] -> ComplexError
evaluateTypes want [] = Func (\(Marked m name) -> Error {
  errorType = Custom (show name <> " is missing arguments"),
  errorMark = m
})
evaluateTypes (Type a) (Marked m x@Dummy {type_sig = Type b} : xs) = 
  if a == b then Success (Marked m x,xs) else Func $ \(Marked _ d) -> Error {
    errorType = TypeMismatch d x,
    errorMark = m
  }
evaluateTypes want (Marked m x@Dummy {type_sig = tp} : xs) = if
  | tp == want -> Success (Marked m x,xs)
  | not (isPossible want tp) -> Func $ \(Marked m d) -> Error {
      errorType = TypeMismatch d x,
      errorMark = m
    }
  | otherwise  -> case tp of
    Type a -> error (show want) --Func (flip TypeMismatch want) -- pure (x,xs)
    Fn a b -> case evaluateTypes a xs of
      RealError x             -> RealError x
      Func f                  -> RealError (f (Marked m x))
      Success (arg@(Marked m argument), leftover) -> 
        case applyDummy x argument of
          Left x -> error "evaluateTypes "
          Right d -> case evaluateTypes want (Marked m d:leftover) of 
            RealError x             -> RealError x
            Func f                  -> RealError (f (Marked m d))
            Success (arg, leftover) -> Success (arg, leftover)

applyDummy :: DummyData -> DummyData -> OrError DummyData
applyDummy Dummy {type_sig = Type _} _ = Left Error {
  errorType = Custom "applied value to non-function",
  errorMark = None
}
applyDummy 
  Dummy {type_sig = Fn a b, current_name = name1} 
  Dummy {type_sig = x, current_name = name2} =
    if x /= a 
    then Left Error {
      errorType = Custom "applied value to non-function",
      errorMark = None
    }
    else pure Dummy {
      current_name = name1 <> " " <> name2,
      type_sig = b
    }

unsafeEval :: Type -> [Data] -> Data
unsafeEval = (fst .) . unsafeEvaluateExpression
  where
    unsafeEvaluateExpression :: Type -> [Data] -> (Data, [Data])
    unsafeEvaluateExpression (Type a) (x@Data {dummy = Dummy {type_sig = Type _}} :xs) = (x,xs)  
    unsafeEvaluateExpression want (x@Data {dummy = Dummy {type_sig = Fn a b}} : xs) = 
      if Fn a b == want then (x,xs) else
        let (arg, leftover) = unsafeEvaluateExpression a xs in
        unsafeEvaluateExpression want (unsafeApplyFn x arg : leftover) 


unsafeApplyFn :: Data -> Data -> Data
unsafeApplyFn 
  Data {dummy = d@Dummy {type_sig = Fn a b}, currentArgs = args, function = f} 
  arg@Data {dummy = d2} = 
  Data {
    dummy = case applyDummy d d2 of
      Right x -> x
      Left _ -> error "can't happen",
    currentArgs    = args <> [arg],
    function       = f
  } 
