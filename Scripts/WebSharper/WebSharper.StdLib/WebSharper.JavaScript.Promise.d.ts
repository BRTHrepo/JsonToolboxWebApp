import IEnumerable from "./System.Collections.Generic.IEnumerable`1"
import Task from "./System.Threading.Tasks.Task`1"
import FSharpAsync from "./Microsoft.FSharp.Control.FSharpAsync`1"
export function For<T0>(xs:IEnumerable<T0>, f:((a?:T0) => Promise<void>)):Promise<void>
export function AsTask<T0>(p:Promise<T0>):Task<T0>
export function AsAsync<T0>(p:Promise<T0>):FSharpAsync<T0>
export function OfTask<T0>(t:Task<T0>):Promise<T0>
export function OfAsync<T0>(a:FSharpAsync<T0>):Promise<T0>
export function unwrapExn(x):Error
