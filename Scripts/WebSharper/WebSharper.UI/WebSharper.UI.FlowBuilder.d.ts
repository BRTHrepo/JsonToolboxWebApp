import Flow from "./WebSharper.UI.Flow`1"
import Object from "../WebSharper.StdLib/System.Object"
export default class FlowBuilder extends Object {
  ReturnFrom<T0>(inner:Flow<T0>):Flow<T0>
  Return<T0>(value:T0):Flow<T0>
  Bind<T0, T1>(comp:Flow<T0>, func:((a?:T0) => Flow<T1>)):Flow<T1>
}
