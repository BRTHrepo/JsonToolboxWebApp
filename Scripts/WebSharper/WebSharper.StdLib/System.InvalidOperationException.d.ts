export default class InvalidOperationException extends Error {
  static New(message:string):InvalidOperationException
  static New_1():InvalidOperationException
  static New_2(message:string, innerExn:Error):InvalidOperationException
  constructor(i:"New_1")
  constructor(i:"New", message:string)
  constructor(i:"New_2", message:string, innerExn:Error)
}
