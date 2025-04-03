import { Bind } from "../WebSharper.StdLib/WebSharper.Concurrency.js"
import { GetAsync } from "./WebSharper.UI.View.js"
export function AsyncBuilder_Bind(_1, view, continuation){
  return Bind(GetAsync(view), continuation);
}
export function AsyncBuilder_Bind_1(_1, var_1, continuation){
  return Bind(GetAsync(var_1.View), continuation);
}
