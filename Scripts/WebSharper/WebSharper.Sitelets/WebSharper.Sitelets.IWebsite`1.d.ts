import IEnumerable from "../WebSharper.StdLib/System.Collections.Generic.IEnumerable`1"
export function isIWebsite(x):x is IWebsite<T0>
export default interface IWebsite<T0>{
  get WebSharper_Sitelets_IWebsite_1$Actions():IEnumerable<T0>
  get WebSharper_Sitelets_IWebsite_1$Sitelet()
}
