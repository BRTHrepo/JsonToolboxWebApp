export function Union2Of2<T0, T1>(Item:T1):(T0 | T1)
export function Union1Of2<T0, T1>(Item:T0):(T0 | T1)
export interface Union1Of2<T0, T1>{
  $:0;
  $0:T0;
}
export interface Union2Of2<T0, T1>{
  $:1;
  $0:T1;
}
