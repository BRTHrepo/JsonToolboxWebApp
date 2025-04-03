import IUniqueIdSource from "./WebSharper.IUniqueIdSource"
import IEnumerable from "./System.Collections.Generic.IEnumerable`1"
export function isIRequiresResources(x):x is IRequiresResources
export default interface IRequiresResources {
  WebSharper_IRequiresResources$Requires(a, b, c:IUniqueIdSource):IEnumerable<any>
}
