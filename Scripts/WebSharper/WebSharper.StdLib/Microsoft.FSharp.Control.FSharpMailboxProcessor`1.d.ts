import FSharpAsync from "./Microsoft.FSharp.Control.FSharpAsync`1"
import { FSharpOption } from "./Microsoft.FSharp.Core.FSharpOption`1"
import FSharpEvent from "./Microsoft.FSharp.Control.FSharpEvent`1"
import LinkedList from "./System.Collections.Generic.LinkedList`1"
import Object from "./System.Object"
export default class FSharpMailboxProcessor<T0>extends Object {
  initial:((a:FSharpMailboxProcessor<T0>) => FSharpAsync<void>);
  token:FSharpOption<{c:boolean,r:(() => void)[]}>;
  started:boolean;
  errorEvent:FSharpEvent<Error>;
  mailbox:LinkedList<T0>;
  savedCont:FSharpOption<FSharpAsync<void>>;
  DefaultTimeout:number;
  static Start<T0>(initial:((a:FSharpMailboxProcessor<T0>) => FSharpAsync<void>), token:FSharpOption<{c:boolean,r:(() => void)[]}>):FSharpMailboxProcessor<T0>
  dequeue():T0
  resume():void
  startAsync(a:FSharpAsync<void>):void
  Scan<T1>(scanner:((a?:T0) => FSharpOption<FSharpAsync<T1>>), timeout:FSharpOption<number>):FSharpAsync<T1>
  TryScan<T1>(scanner:((a?:T0) => FSharpOption<FSharpAsync<T1>>), timeout:FSharpOption<number>):FSharpAsync<FSharpOption<T1>>
  PostAndAsyncReply<T1>(msgf:((a:((a:T1) => void)) => T0), timeout:FSharpOption<number>):FSharpAsync<T1>
  PostAndTryAsyncReply<T1>(msgf:((a:((a:T1) => void)) => T0), timeout:FSharpOption<number>):FSharpAsync<FSharpOption<T1>>
  get CurrentQueueLength():number
  Receive(timeout:FSharpOption<number>):FSharpAsync<T0>
  TryReceive(timeout:FSharpOption<number>):FSharpAsync<FSharpOption<T0>>
  Start():void
  set DefaultTimeout_1(v:number):void
  get DefaultTimeout_1():number
  remove_Error(handler:((a:any, b:Error) => void)):void
  add_Error(handler:((a:any, b:Error) => void)):void
  get Error()
  constructor(initial:((a:FSharpMailboxProcessor<T0>) => FSharpAsync<void>), token:FSharpOption<{c:boolean,r:(() => void)[]}>)
}
