import { FSharpOption } from "../WebSharper.StdLib/Microsoft.FSharp.Core.FSharpOption`1"
import FSharpAsync from "../WebSharper.StdLib/Microsoft.FSharp.Control.FSharpAsync`1"
export function isIUserSession(x):x is IUserSession
export default interface IUserSession {
  WebSharper_Web_IUserSession$GetLoggedInUser():FSharpAsync<FSharpOption<string>>
  WebSharper_Web_IUserSession$LoginUser(a:string, b:FSharpOption<boolean>):FSharpAsync<void>
  WebSharper_Web_IUserSession$LoginUser_1(a:string, b:number):FSharpAsync<void>
  WebSharper_Web_IUserSession$Logout():FSharpAsync<void>
  get WebSharper_Web_IUserSession$IsAvailable():boolean
}
