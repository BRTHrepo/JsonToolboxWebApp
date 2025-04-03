import IRequiresResources from "./WebSharper.IRequiresResources"
export function isIInitializer(x):x is IInitializer
export default interface IInitializer extends IRequiresResources {
  $init(a:string):void
  $postinit(a:string):void
  $preinit(a:string):void
}
