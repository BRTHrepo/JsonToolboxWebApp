import Object from "../WebSharper.StdLib/System.Object.js"
import Var from "./WebSharper.UI.Var.js"
export default class Flow extends Object {
  render;
  get Render(){
    return this.render;
  }
  static New(define){
    return new this("New", define);
  }
  static New_1(render){
    return new this("New_1", render);
  }
  constructor(i, _1){
    let define;
    if(i=="New"){
      define=_1;
      i="New_1";
      _1=(var_1) =>(cont) => Var.Set(var_1, define(cont));
    }
    if(i=="New_1"){
      const render=_1;
      super();
      this.render=render;
    }
  }
}
