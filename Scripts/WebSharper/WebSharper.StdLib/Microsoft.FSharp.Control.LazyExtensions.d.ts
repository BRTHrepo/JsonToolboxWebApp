import Lazy from "./System.Lazy`1"
export function Force<T0>(x:Lazy<T0>):T0
export function CreateFromValue<T0>(v:T0):Lazy<T0>
export function Create<T0>(f:(() => T0)):Lazy<T0>
export function forceLazy<T0>()
export function cachedLazy<T0>()
