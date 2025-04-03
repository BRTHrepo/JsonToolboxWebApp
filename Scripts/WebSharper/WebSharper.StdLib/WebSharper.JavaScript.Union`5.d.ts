export function Union5Of5<T0, T1, T2, T3, T4>(Item:T4):(T0 | T1 | T2 | T3 | T4)
export function Union4Of5<T0, T1, T2, T3, T4>(Item:T3):(T0 | T1 | T2 | T3 | T4)
export function Union3Of5<T0, T1, T2, T3, T4>(Item:T2):(T0 | T1 | T2 | T3 | T4)
export function Union2Of5<T0, T1, T2, T3, T4>(Item:T1):(T0 | T1 | T2 | T3 | T4)
export function Union1Of5<T0, T1, T2, T3, T4>(Item:T0):(T0 | T1 | T2 | T3 | T4)
export interface Union1Of5<T0, T1, T2, T3, T4>{
  $:0;
  $0:T0;
}
export interface Union2Of5<T0, T1, T2, T3, T4>{
  $:1;
  $0:T1;
}
export interface Union3Of5<T0, T1, T2, T3, T4>{
  $:2;
  $0:T2;
}
export interface Union4Of5<T0, T1, T2, T3, T4>{
  $:3;
  $0:T3;
}
export interface Union5Of5<T0, T1, T2, T3, T4>{
  $:4;
  $0:T4;
}
