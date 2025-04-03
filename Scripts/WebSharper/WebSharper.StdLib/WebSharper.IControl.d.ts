import IControlBody from "./WebSharper.IControlBody"
import IRequiresResources from "./WebSharper.IRequiresResources"
export function isIControl(x):x is IControl
export default interface IControl extends IRequiresResources {
  get Body():IControlBody
}
