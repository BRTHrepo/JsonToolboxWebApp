export function Union4Of4<T0, T1, T2, T3>(Item:T3):(T0 | T1 | T2 | T3)
export function Union3Of4<T0, T1, T2, T3>(Item:T2):(T0 | T1 | T2 | T3)
export function Union2Of4<T0, T1, T2, T3>(Item:T1):(T0 | T1 | T2 | T3)
export function Union1Of4<T0, T1, T2, T3>(Item:T0):(T0 | T1 | T2 | T3)
export interface Union1Of4<T0, T1, T2, T3>{
  $:0;
  $0:T0;
}
export interface Union2Of4<T0, T1, T2, T3>{
  $:1;
  $0:T1;
}
export interface Union3Of4<T0, T1, T2, T3>{
  $:2;
  $0:T2;
}
export interface Union4Of4<T0, T1, T2, T3>{
  $:3;
  $0:T3;
}
