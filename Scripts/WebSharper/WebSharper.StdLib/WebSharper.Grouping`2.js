import Object from "./System.Object.js"
import { Get } from "./WebSharper.Enumerator.js"
export default class Grouping extends Object {
  k;
  v;
  get System_Linq_IGrouping_2$Key(){
    return this.k;
  }
  GetEnumerator(){
    return Get(this.v);
  }
  constructor(k, v){
    super();
    this.k=k;
    this.v=v;
  }
}
