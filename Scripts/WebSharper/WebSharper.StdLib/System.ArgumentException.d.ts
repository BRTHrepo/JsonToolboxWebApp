export default class ArgumentException extends Error {
  static New(argumentName:string, message:string):ArgumentException
  static New_1():ArgumentException
  static New_2(message:string):ArgumentException
  constructor(i:"New_1")
  constructor(i:"New", argumentName:string, message:string)
  constructor(i:"New_2", message:string)
}
