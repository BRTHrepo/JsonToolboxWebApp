export function isIEqualityComparer(x):x is IEqualityComparer<T0>
export default interface IEqualityComparer<T0>{
  CEquals(a:T0, b:T0):boolean
  CGetHashCode(a:T0):number
}
