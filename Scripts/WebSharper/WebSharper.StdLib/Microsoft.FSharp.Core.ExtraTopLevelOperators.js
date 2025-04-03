import Dictionary from "./System.Collections.Generic.Dictionary`2.js"
import { Get } from "./WebSharper.Enumerator.js"
import { isIDisposable } from "./System.IDisposable.js"
import { ofSeq } from "./Microsoft.FSharp.Collections.ArrayModule.js"
import { map } from "./Microsoft.FSharp.Collections.SeqModule.js"
export function dict(s){
  const d=new Dictionary("New_5");
  const e=Get(s);
  try {
    while(e.MoveNext())
      {
        const f=e.Current;
        d.DAdd(f[0], f[1]);
      }
  }
  finally {
    if(typeof e=="object"&&isIDisposable(e))e.Dispose();
  }
  return d;
}
export function create2D(rows){
  const arr=ofSeq(map(ofSeq, rows));
  arr.dims=2;
  return arr;
}
