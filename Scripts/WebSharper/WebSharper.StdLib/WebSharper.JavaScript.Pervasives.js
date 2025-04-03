import { Get } from "./WebSharper.Enumerator.js"
import { isIDisposable } from "./System.IDisposable.js"
export function GetJS(x, items){
  let x_1=x;
  const e=Get(items);
  try {
    while(e.MoveNext())
      x_1=x_1[e.Current];
  }
  finally {
    if(typeof e=="object"&&isIDisposable(e))e.Dispose();
  }
  return x_1;
}
export function NewFromSeq(fields){
  const r={};
  const e=Get(fields);
  try {
    while(e.MoveNext())
      {
        const f=e.Current;
        r[f[0]]=f[1];
      }
  }
  finally {
    if(typeof e=="object"&&isIDisposable(e))e.Dispose();
  }
  return r;
}
