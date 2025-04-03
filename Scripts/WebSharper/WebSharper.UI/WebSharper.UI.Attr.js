import { Create } from "../WebSharper.Core.JavaScript/Runtime.js"
import { Static, EmptyAttr, AppendTree } from "./WebSharper.UI.Client.Attrs.js"
import { ofSeqNonCopying, TreeReduce } from "./WebSharper.UI.Array.js"
export default class Attr {
  static A1(Item){
    return Create(Attr, {$:1, $0:Item});
  }
  static A2(Item1, Item2){
    return Create(Attr, {
      $:2, 
      $0:Item1, 
      $1:Item2
    });
  }
  static A3(init){
    return Create(Attr, {$:3, $0:init});
  }
  static A4(onAfterRender){
    return Create(Attr, {$:4, $0:onAfterRender});
  }
  static Handler(event, q){
    return Attr.HandlerImpl(event, q);
  }
  static HandlerImpl(event, q){
    return Static((el) => {
      el.addEventListener(event, (d) =>(q(el))(d), false);
    });
  }
  static OnAfterRenderImpl(q){
    return Attr.A4(q);
  }
  static Concat(xs){
    const x=ofSeqNonCopying(xs);
    return TreeReduce(EmptyAttr(), Attr.Append, x);
  }
  static Append(a, b){
    return AppendTree(a, b);
  }
  static Create(name, value){
    return Static((el) => {
      el.setAttribute(name, value);
    });
  }
}
