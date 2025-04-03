export default class ArgumentNullException extends Error {
  static New(argumentName:string, message:string):ArgumentNullException
  static New_1(argumentName:string):ArgumentNullException
  static New_2():ArgumentNullException
  constructor(i:"New_2")
  constructor(i:"New_1", argumentName:string)
  constructor(i:"New", argumentName:string, message:string)
}
