import { FSharpOption } from "../WebSharper.StdLib/Microsoft.FSharp.Core.FSharpOption`1"
export function isIRouter(x):x is IRouter<T0>
export default interface IRouter<T0>{
  WebSharper_Sitelets_IRouter_1$Link(a:T0):FSharpOption<any>
  WebSharper_Sitelets_IRouter_1$Route(a):FSharpOption<T0>
}
