import IEnumerable from "./System.Collections.Generic.IEnumerable`1"
export default class AggregateException extends Error {
  innerExceptions:Error[];
  static New(message:string, innerException:Error):AggregateException
  static New_1(message:string, innerExceptions:IEnumerable<Error>):AggregateException
  static New_2(innerExceptions:IEnumerable<Error>):AggregateException
  static New_3(innerExceptions:Error[]):AggregateException
  static New_4(message:string, innerExceptions:Error[]):AggregateException
  constructor(i:"New_3", innerExceptions:Error[])
  constructor(i:"New_2", innerExceptions:IEnumerable<Error>)
  constructor(i:"New_1", message:string, innerExceptions:IEnumerable<Error>)
  constructor(i:"New", message:string, innerException:Error)
  constructor(i:"New_4", message:string, innerExceptions:Error[])
}
