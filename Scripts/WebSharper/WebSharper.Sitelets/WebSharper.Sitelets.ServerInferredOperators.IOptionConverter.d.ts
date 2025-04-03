import { FSharpOption } from "../WebSharper.StdLib/Microsoft.FSharp.Core.FSharpOption`1"
export function isIOptionConverter(x):x is IOptionConverter
export default interface IOptionConverter {
  WebSharper_Sitelets_ServerInferredOperators_IOptionConverter$Get(a):FSharpOption<any>
  WebSharper_Sitelets_ServerInferredOperators_IOptionConverter$Some(a)
}
