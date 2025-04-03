import IRequiresResources from "../WebSharper.StdLib/WebSharper.IRequiresResources"
export function isINode(x):x is INode
export default interface INode extends IRequiresResources {
  WebSharper_Web_INode$Write(a, b):void
  get WebSharper_Web_INode$IsAttribute():boolean
}
