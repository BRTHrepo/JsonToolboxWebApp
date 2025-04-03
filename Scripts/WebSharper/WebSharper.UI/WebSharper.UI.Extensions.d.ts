import FSharpAsyncBuilder from "../WebSharper.StdLib/Microsoft.FSharp.Control.FSharpAsyncBuilder"
import { View_T } from "./WebSharper.UI.View`1"
import FSharpAsync from "../WebSharper.StdLib/Microsoft.FSharp.Control.FSharpAsync`1"
import Var from "./WebSharper.UI.Var`1"
export function AsyncBuilder_Bind<T0, T1>(this_1:FSharpAsyncBuilder, view:View_T<T0>, continuation:((a?:T0) => FSharpAsync<T1>)):FSharpAsync<T1>
export function AsyncBuilder_Bind_1<T0, T1>(this_1:FSharpAsyncBuilder, var_1:Var<T0>, continuation:((a?:T0) => FSharpAsync<T1>)):FSharpAsync<T1>
