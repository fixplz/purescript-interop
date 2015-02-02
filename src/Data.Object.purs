module Data.Object
(
  Prop(..), Method(..),
  runMethod0, runMethodEff0,
  runMethod1, runMethodEff1,
  runMethod2, runMethodEff2,
  runMethod3, runMethodEff3,
  runMethod4, runMethodEff4,
  runMethod5, runMethodEff5,
  getProp,
  setProp,
  nativeRepr
)
where

import Data.Function
import Control.Monad.Eff

type Prop = String
type Method = String

getProp :: forall obj val. Prop -> obj -> val
getProp = runFn2 _get

setProp :: forall obj val. Prop -> obj -> val -> Unit
setProp = runFn3 _set

runMethod0 :: forall obj ret. Method -> obj -> ret
runMethod0 = runFn2 _method0

runMethod1 :: forall obj a ret. Method -> obj -> a -> ret
runMethod1 = runFn3 _method1

runMethod2 :: forall obj a b ret. Method -> obj -> a -> b -> ret
runMethod2 = runFn4 _method2

runMethod3 :: forall obj a b c ret. Method -> obj -> a -> b -> c -> ret
runMethod3 = runFn5 _method3

runMethod4 :: forall obj a b c d ret. Method -> obj -> a -> b -> c -> d -> ret
runMethod4 = runFn6 _method4

runMethod5 :: forall obj a b c d e ret. Method -> obj -> a -> b -> c -> d -> e -> ret
runMethod5 = runFn7 _method5

runMethodEff0 :: forall eff obj ret. Method -> obj -> Eff eff ret
runMethodEff0 mt fn = asEff \_ -> runMethod0 mt fn

runMethodEff1 :: forall eff obj a ret. Method -> obj -> a -> Eff eff ret
runMethodEff1 mt fn a = asEff \_ -> runMethod1 mt fn a

runMethodEff2 :: forall eff obj a b ret. Method -> obj -> a -> b -> Eff eff ret
runMethodEff2 mt fn a b = asEff \_ -> runMethod2 mt fn a b

runMethodEff3 :: forall eff obj a b c ret. Method -> obj -> a -> b -> c -> Eff eff ret
runMethodEff3 mt fn a b c = asEff \_ -> runMethod3 mt fn a b c

runMethodEff4 :: forall eff obj a b c d ret. Method -> obj -> a -> b -> c -> d -> Eff eff ret
runMethodEff4 mt fn a b c d = asEff \_ -> runMethod4 mt fn a b c d

runMethodEff5 :: forall eff obj a b c d e ret. Method -> obj -> a -> b -> c -> d -> e -> Eff eff ret
runMethodEff5 mt fn a b c d e = asEff \_ -> runMethod5 mt fn a b c d e

foreign import nativeRepr
  """
  function nativeRepr(obj) {
    try { return require('util').inspect(obj) }
    catch(err) { return JSON.stringify(obj) }
  }
  """
  :: forall obj. obj -> String

foreign import asEff
  "function asEff(f) { return f }"
  :: forall eff ret. (Unit -> ret) -> Eff eff ret

foreign import _get "function _get(field, obj) { return obj[field] }" :: forall obj val. Fn2 Prop obj val
foreign import _set "function _set(field, obj, val) { obj[field] = val }" :: forall obj val. Fn3 Prop obj val Unit

foreign import _method0 "function _method0(method, obj) { return obj[method]() }":: forall obj ret. Fn2 Method obj ret
foreign import _method1 "function _method1(method, obj, a) { return obj[method](a) }":: forall obj a ret. Fn3 Method obj a ret
foreign import _method2 "function _method2(method, obj, a,b) { return obj[method](a,b) }":: forall obj a b ret. Fn4 Method obj a b ret
foreign import _method3 "function _method3(method, obj, a,b,c) { return obj[method](a,b,c) }":: forall obj a b c ret. Fn5 Method obj a b c ret
foreign import _method4 "function _method4(method, obj, a,b,c,d) { return obj[method](a,b,c,d) }":: forall obj a b c d ret. Fn6 Method obj a b c d ret
foreign import _method5 "function _method5(method, obj, a,b,c,d,e) { return obj[method](a,b,c,d,e) }":: forall obj a b c d e ret. Fn7 Method obj a b c d e ret
