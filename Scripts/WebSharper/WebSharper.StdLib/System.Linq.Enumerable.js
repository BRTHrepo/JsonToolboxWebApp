import { nth, tryFind, delay, append, iter, choose, isEmpty, sum, fold, collect, map, mapi, concat, init, min, max } from "./Microsoft.FSharp.Collections.SeqModule.js"
import { Get } from "./WebSharper.Enumerator.js"
import { isIDisposable } from "./System.IDisposable.js"
import { InvalidOp } from "./Microsoft.FSharp.Core.Operators.js"
import { Some } from "./Microsoft.FSharp.Core.FSharpOption`1.js"
import { enumUsing, enumWhile } from "./Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.js"
import HashSet from "./System.Collections.Generic.HashSet`1.js"
import Dictionary from "./System.Collections.Generic.Dictionary`2.js"
import { get } from "./WebSharper.Nullable.js"
import { ofSeq, iteri } from "./Microsoft.FSharp.Collections.ArrayModule.js"
import OrderedEnumerable from "./WebSharper.OrderedEnumerable`1.js"
import ReverseComparer from "./WebSharper.ReverseComparer`2.js"
import ProjectionComparer from "./WebSharper.ProjectionComparer`2.js"
import { MarkResizable } from "../WebSharper.Core.JavaScript/Runtime.js"
import { set, length } from "./Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.js"
import Grouping from "./WebSharper.Grouping`2.js"
export function ElementAtOrDefault(this_1, index, defaultValue){
  try {
    return nth(index, this_1);
  }
  catch(m){
    return defaultValue;
  }
}
export function FirstOrDefault(this_1, defaultValue){
  const e=Get(this_1);
  try {
    return e.MoveNext()?e.Current:defaultValue;
  }
  finally {
    if(typeof e=="object"&&isIDisposable(e))e.Dispose();
  }
}
export function FirstOrDefault$1(this_1, predicate, defaultValue){
  const m=tryFind(predicate, this_1);
  return m==null?defaultValue:m.$0;
}
export function LastOrDefault(this_1, predicate, defaultValue){
  const m=LastPred(this_1, predicate);
  return m==null?defaultValue:m.$0;
}
export function SingleOrDefault(this_1, predicate, defaultValue){
  const e=Get(this_1);
  try {
    let found=null;
    while(e.MoveNext())
      if(predicate(e.Current))if(found!=null&&found.$==1)InvalidOp("Sequence contains more than one element");
      else found=Some(e.Current);
    return found==null?defaultValue:found.$0;
  }
  finally {
    if(typeof e=="object"&&isIDisposable(e))e.Dispose();
  }
}
export function Where(this_1, predicate){
  return delay(() => enumUsing(Get(this_1), (e) => {
    let i;
    i=0;
    return enumWhile(() => e.MoveNext(), delay(() => append(predicate(e.Current, i)?[e.Current]:[], delay(() => {
      i=i+1;
      return[];
    }))));
  }));
}
export function Union(this_1, second, comparer){
  const tbl=new HashSet("New", this_1, comparer);
  const e=Get(second);
  try {
    while(e.MoveNext())
      tbl.SAdd(e.Current);
  }
  finally {
    if(typeof e=="object"&&isIDisposable(e))e.Dispose();
  }
  return tbl;
}
export function ToDictionary(this_1, keySelector, elementSelector, comparer){
  const d=new Dictionary("New_3", comparer);
  iter((x) => {
    d.DAdd(keySelector(x), elementSelector(x));
  }, this_1);
  return d;
}
export function ToDictionary_1(this_1, keySelector, comparer){
  const d=new Dictionary("New_3", comparer);
  iter((x) => {
    d.DAdd(keySelector(x), x);
  }, this_1);
  return d;
}
export function TakeWhile(this_1, predicate){
  return delay(() => enumUsing(Get(this_1), (e) => enumWhile(() => e.MoveNext()&&predicate(e.Current), delay(() =>[e.Current]))));
}
export function TakeWhile_1(this_1, predicate){
  return delay(() => enumUsing(Get(this_1), (e) => {
    let i;
    i=0;
    return enumWhile(() => e.MoveNext()&&predicate(e.Current, i), delay(() => {
      i=i+1;
      return[e.Current];
    }));
  }));
}
export function Take(this_1, count){
  return delay(() => enumUsing(Get(this_1), (e) => {
    let i;
    i=0;
    return enumWhile(() => i<count&&e.MoveNext(), delay(() => {
      i=i+1;
      return[e.Current];
    }));
  }));
}
export function Sum(this_1){
  const s=choose((x) => x!=null?Some(get(x)):null, this_1);
  return isEmpty(s)?null:sum(s);
}
export function SkipWhile(this_1, predicate){
  return delay(() => enumUsing(Get(this_1), (e) => {
    let predWasTrue;
    predWasTrue=true;
    return append(enumWhile(() => predWasTrue&&e.MoveNext(), delay(() =>!predicate(e.Current)?(predWasTrue=false,[]):[])), delay(() =>!predWasTrue?append([e.Current], delay(() => enumWhile(() => e.MoveNext(), delay(() =>[e.Current])))):[]));
  }));
}
export function SkipWhile_1(this_1, predicate){
  return delay(() => enumUsing(Get(this_1), (e) => {
    let i;
    let predWasTrue;
    i=0;
    predWasTrue=true;
    return append(enumWhile(() => predWasTrue&&e.MoveNext(), delay(() => predicate(e.Current, i)?(i=i+1,[]):(predWasTrue=false,[]))), delay(() =>!predWasTrue?append([e.Current], delay(() => enumWhile(() => e.MoveNext(), delay(() =>[e.Current])))):[]));
  }));
}
export function Skip(this_1, count){
  return delay(() => enumUsing(Get(this_1), (e) => {
    let i;
    i=0;
    return append(enumWhile(() => i<count&&e.MoveNext(), delay(() => {
      i=i+1;
      return[];
    })), delay(() => enumWhile(() => e.MoveNext(), delay(() =>[e.Current]))));
  }));
}
export function Single(this_1, predicate){
  const x=fold((_1, _2) => predicate(_2)?_1!=null?InvalidOp("Sequence contains more than one matching element"):Some(_2):_1, null, this_1);
  return x!=null&&x.$==1?x.$0:InvalidOp("Sequence contains no elements");
}
export function SequenceEqual(this_1, _1, comparer){
  const e1=Get(this_1);
  try {
    const e2=Get(this_1);
    try {
      function go(){
        while(true)
          {
            if(e1.MoveNext()){
              if(!(e2.MoveNext()&&comparer.CEquals(e1.Current, e2.Current)))return false;
            }
            else return!e2.MoveNext();
          }
      }
      return go();
    }
    finally {
      if(typeof e2=="object"&&isIDisposable(e2))e2.Dispose();
    }
  }
  finally {
    if(typeof e1=="object"&&isIDisposable(e1))e1.Dispose();
  }
}
export function SelectMany(this_1, selector, collectionSelector){
  return collect((_1) => {
    const t=_1[0];
    return map((c) => collectionSelector(t, c), _1[1]);
  }, mapi((_1, _2) =>[_2, selector(_2, _1)], this_1));
}
export function SelectMany_1(this_1, selector, collectionSelector){
  return collect((_1) => {
    const t=_1[0];
    return map((c) => collectionSelector(t, c), _1[1]);
  }, map((t) =>[t, selector(t)], this_1));
}
export function SelectMany_2(this_1, selector){
  return concat(mapi((_1, _2) => selector(_2, _1), this_1));
}
export function Select(this_1, selector){
  return mapi((_1, _2) => selector(_2, _1), this_1);
}
export function Reverse(this_1){
  return ofSeq(this_1).slice().reverse();
}
export function Repeat(element, count){
  return init(count, () => element);
}
export function Range(start, count){
  return init(count, (y) => start+y);
}
export function OrderByDescending(this_1, keySelector, comparer){
  return new OrderedEnumerable(this_1, new ReverseComparer(comparer, keySelector));
}
export function OrderBy(this_1, keySelector, comparer){
  return new OrderedEnumerable(this_1, new ProjectionComparer(comparer, keySelector));
}
export function Min(this_1){
  const s=choose((x) => x!=null?Some(get(x)):null, this_1);
  return isEmpty(s)?null:min(s);
}
export function Max(this_1){
  const s=choose((x) => x!=null?Some(get(x)):null, this_1);
  return isEmpty(s)?null:max(s);
}
export function Last(this_1, predicate){
  const m=LastPred(this_1, predicate);
  return m==null?InvalidOp("Sequence contains no matching element"):m.$0;
}
export function LastPred(this_1, predicate){
  return fold((_1, _2) => predicate(_2)?Some(_2):_1, null, this_1);
}
export function Join(outer, inner, outerKeySelector, innerKeySelector, resultSelector, comparer){
  return delay(() => {
    const t=new Dictionary("New_3", comparer);
    const a=ofSeq(delay(() => collect((o) => {
      let o_1;
      const k=outerKeySelector(o);
      if((o_1=null,[t.TryGetValue(k, {get:() => o_1, set:(v) => {
        o_1=v;
      }}), o_1])[0])return[];
      else {
        const pair=[o, MarkResizable([])];
        t.DAdd(k, pair);
        return[pair];
      }
    }, outer)));
    const e=Get(inner);
    try {
      while(e.MoveNext())
        ((() => {
          let o;
          const i=e.Current;
          const m=(o=null,[t.TryGetValue(innerKeySelector(i), {get:() => o, set:(v) => {
            o=v;
          }}), o]);
          return m[0]?void m[1][1].push(i):null;
        })());
    }
    finally {
      if(typeof e=="object"&&isIDisposable(e))e.Dispose();
    }
    return ofSeq(delay(() => collect((m) => {
      const o=m[0];
      return map((i) => resultSelector(o, i), m[1]);
    }, a)));
  });
}
export function Intersect(this_1, second, comparer){
  const t1=new HashSet("New", this_1, comparer);
  return delay(() => {
    const t2=new HashSet("New_1", comparer);
    return collect((x) => t1.Contains(x)&&t2.SAdd(x)?[x]:[], second);
  });
}
export function GroupJoin(outer, inner, outerKeySelector, innerKeySelector, resultSelector, comparer){
  return delay(() => {
    const t=new Dictionary("New_3", comparer);
    const a=ofSeq(delay(() => collect((o) => {
      let o_1;
      const k=outerKeySelector(o);
      if((o_1=null,[t.TryGetValue(k, {get:() => o_1, set:(v) => {
        o_1=v;
      }}), o_1])[0])return[];
      else {
        const pair=[o, MarkResizable([])];
        t.DAdd(k, pair);
        return[pair];
      }
    }, outer)));
    const e=Get(inner);
    try {
      while(e.MoveNext())
        ((() => {
          let o;
          const i=e.Current;
          const m=(o=null,[t.TryGetValue(innerKeySelector(i), {get:() => o, set:(v) => {
            o=v;
          }}), o]);
          return m[0]?void m[1][1].push(i):null;
        })());
    }
    finally {
      if(typeof e=="object"&&isIDisposable(e))e.Dispose();
    }
    iteri((_1, _2) => set(a, _1, resultSelector(_2[0], _2[1])), a);
    return a;
  });
}
export function GroupBy(this_1, keySelector, elementSelector, resultSelector, comparer){
  return map((g) => resultSelector(g.System_Linq_IGrouping_2$Key, g), GroupBy_1(this_1, keySelector, elementSelector, comparer));
}
export function GroupBy_1(this_1, keySelector, elementSelector, comparer){
  return delay(() => ofSeq(delay(() => {
    const t=new Dictionary("New_3", comparer);
    return collect((x) => {
      let o;
      const k=keySelector(x);
      const e=elementSelector(x);
      const m=(o=null,[t.TryGetValue(k, {get:() => o, set:(v) => {
        o=v;
      }}), o]);
      if(m[0]){
        m[1].push(e);
        return[];
      }
      else {
        const a=MarkResizable([]);
        a.push(e);
        t.set_Item(k, a);
        return[new Grouping(k, a)];
      }
    }, this_1);
  })));
}
export function Except(this_1, second, comparer){
  const tbl=new HashSet("New", this_1, comparer);
  const e=Get(second);
  try {
    while(e.MoveNext())
      tbl.Remove(e.Current);
  }
  finally {
    if(typeof e=="object"&&isIDisposable(e))e.Dispose();
  }
  return tbl;
}
export function Distinct(this_1, comparer){
  return delay(() => enumUsing(Get(this_1), (e) => {
    const tbl=new HashSet("New_1", comparer);
    return enumWhile(() => e.MoveNext(), delay(() => tbl.SAdd(e.Current)?[e.Current]:[]));
  }));
}
export function DefaultIfEmpty(this_1, defaultValue){
  return isEmpty(this_1)?[defaultValue]:this_1;
}
export function Average(this_1){
  let x=[];
  const e=Get(this_1);
  try {
    while(e.MoveNext())
      if(e.Current!=null)x.push(get(e.Current));
    return length(x)===0?null:sum(x)/length(x);
  }
  finally {
    if(typeof e=="object"&&isIDisposable(e))e.Dispose();
  }
}
