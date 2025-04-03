export default class InvalidOperationException extends Error {
  static New(message){
    return new this("New", message);
  }
  static New_1(){
    return new this("New_1");
  }
  static New_2(message, innerExn){
    return new this("New_2", message, innerExn);
  }
  constructor(i, _1, _2){
    if(i=="New_1"){
      i="New";
      _1="Operation is not valid due to the current state of the object.";
    }
    let message;
    if(i=="New"){
      message=_1;
      i="New_2";
      _1=message;
      _2=null;
    }
    if(i=="New_2"){
      const message_1=_1;
      const innerExn=_2;
      super(message_1);
      this.inner=innerExn;
    }
  }
}
