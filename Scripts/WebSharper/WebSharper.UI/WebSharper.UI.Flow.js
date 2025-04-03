import FlowBuilder from "./WebSharper.UI.FlowBuilder.js"
import Flow from "./WebSharper.UI.Flow`1.js"
import Var from "./WebSharper.UI.Var.js"
import Doc from "./WebSharper.UI.Doc.js"
export function get_Do(){
  return new FlowBuilder();
}
export function Static(doc){
  return new Flow("New_1", (var_1) =>(cont) => {
    Var.Set(var_1, doc);
    return cont();
  });
}
export function Define(f){
  return new Flow("New", (x) => f((a) => {
    x(a);
  }));
}
export function Embed(fl){
  const var_1=Var.Create_1(Doc.Empty);
  (fl.Render(var_1))(() => { });
  return Doc.EmbedView(var_1.View);
}
export function Return(x){
  return new Flow("New_1", () =>(cont) => cont(x));
}
export function Bind(m, k){
  return new Flow("New_1", (var_1) =>(cont) =>(m.Render(var_1))((r) => {
    (k(r).Render(var_1))(cont);
  }));
}
export function Map(f, x){
  return new Flow("New_1", (var_1) =>(cont) =>(x.Render(var_1))((r) => {
    cont(f(r));
  }));
}
