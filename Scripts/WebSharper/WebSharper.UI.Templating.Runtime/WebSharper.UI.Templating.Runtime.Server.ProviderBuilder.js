import Object from "../WebSharper.StdLib/System.Object.js"
import { NewGuid } from "../WebSharper.StdLib/System.Guid.js"
import { MarkResizable, SetOptional } from "../WebSharper.Core.JavaScript/Runtime.js"
import { Some } from "../WebSharper.StdLib/Microsoft.FSharp.Core.FSharpOption`1.js"
export default class ProviderBuilder extends Object {
  i;
  k;
  h;
  s;
  static New(src){
    return new this("New", src);
  }
  static New_1(){
    return new this("New_1");
  }
  constructor(i, _1){
    if(i=="New_1"){
      let c;
      super();
      this.i=null;
      this.k=(c=NewGuid(),String(c));
      this.h=MarkResizable([]);
      SetOptional(this, "s", null);
    }
    if(i=="New"){
      const src=_1;
      let c_1;
      super();
      this.i=null;
      this.k=(c_1=NewGuid(),String(c_1));
      this.h=MarkResizable([]);
      SetOptional(this, "s", Some(src));
    }
  }
}
