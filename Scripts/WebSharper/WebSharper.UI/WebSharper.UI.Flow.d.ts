import FlowBuilder from "./WebSharper.UI.FlowBuilder"
import Doc from "./WebSharper.UI.Doc"
import Flow from "./WebSharper.UI.Flow`1"
export function get_Do():FlowBuilder
export function Static(doc:Doc):Flow<void>
export function Define<T0>(f:((a:((a?:T0) => void)) => Doc)):Flow<T0>
export function Embed<T0>(fl:Flow<T0>):Doc
export function Return<T0>(x:T0):Flow<T0>
export function Bind<T0, T1>(m:Flow<T0>, k:((a?:T0) => Flow<T1>)):Flow<T1>
export function Map<T0, T1>(f:((a?:T0) => T1), x:Flow<T0>):Flow<T1>
