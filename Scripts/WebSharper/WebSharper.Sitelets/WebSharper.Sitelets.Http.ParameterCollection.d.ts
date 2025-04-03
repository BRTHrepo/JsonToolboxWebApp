import { FSharpOption } from "../WebSharper.StdLib/Microsoft.FSharp.Core.FSharpOption`1"
import { FSharpList_T } from "../WebSharper.StdLib/Microsoft.FSharp.Collections.FSharpList`1"
export function isParameterCollection(x):x is ParameterCollection
export default interface ParameterCollection {
  WebSharper_Sitelets_Http_ParameterCollection$Item(a:string):FSharpOption<string>
  WebSharper_Sitelets_Http_ParameterCollection$ToList():FSharpList_T<[string, string]>
}
