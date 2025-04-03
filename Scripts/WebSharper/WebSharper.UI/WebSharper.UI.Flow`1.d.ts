import Doc from "./WebSharper.UI.Doc"
import Var from "./WebSharper.UI.Var`1"
import Object from "../WebSharper.StdLib/System.Object"
export default class Flow<T0>extends Object {
  render:((a:Var<Doc>) => ((a:((a?:T0) => void)) => void));
  get Render():((a:Var<Doc>) => ((a:((a?:T0) => void)) => void))
  static New<T0>(define:((a:((a:T0) => void)) => Doc)):Flow<T0>
  static New_1<T0>(render:((a:Var<Doc>) => ((a:((a?:T0) => void)) => void))):Flow<T0>
  constructor(i:"New", define:((a:((a:T0) => void)) => Doc))
  constructor(i:"New_1", render:((a:Var<Doc>) => ((a:((a?:T0) => void)) => void)))
}
