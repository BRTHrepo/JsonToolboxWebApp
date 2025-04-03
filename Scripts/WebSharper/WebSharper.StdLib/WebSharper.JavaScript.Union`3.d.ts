export function Union3Of3<T0, T1, T2>(Item:T2):(T0 | T1 | T2)
export function Union2Of3<T0, T1, T2>(Item:T1):(T0 | T1 | T2)
export function Union1Of3<T0, T1, T2>(Item:T0):(T0 | T1 | T2)
export interface Union1Of3<T0, T1, T2>{
  $:0;
  $0:T0;
}
export interface Union2Of3<T0, T1, T2>{
  $:1;
  $0:T1;
}
export interface Union3Of3<T0, T1, T2>{
  $:2;
  $0:T2;
}
