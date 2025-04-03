export default class ArgumentOutOfRangeException extends Error {
  static New(argumentName:string, message:string):ArgumentOutOfRangeException
  static New_1(argumentName:string):ArgumentOutOfRangeException
  static New_2():ArgumentOutOfRangeException
  constructor(i:"New_2")
  constructor(i:"New_1", argumentName:string)
  constructor(i:"New", argumentName:string, message:string)
}
