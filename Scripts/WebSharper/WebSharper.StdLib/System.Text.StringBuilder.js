import Object from "./System.Object.js"
export default class StringBuilder extends Object {
  lines;
  currentLine;
  toString(){
    this.currentLine!=""?this.lines.push(this.currentLine):void 0;
    return this.lines.join("\n");
  }
  AppendLine(part){
    this.currentLine=this.currentLine+part;
    this.lines.push(this.currentLine);
    this.currentLine="";
    return this;
  }
  AppendLine_1(part){
    this.currentLine=this.currentLine+String(part);
    this.lines.push(this.currentLine);
    this.currentLine="";
    return this;
  }
  AppendLine_2(part){
    this.currentLine=this.currentLine+String(part);
    this.lines.push(this.currentLine);
    this.currentLine="";
    return this;
  }
  AppendLine_3(part){
    this.currentLine=this.currentLine+String(part);
    this.lines.push(this.currentLine);
    this.currentLine="";
    return this;
  }
  AppendLine_4(part){
    this.currentLine=this.currentLine+String(part);
    this.lines.push(this.currentLine);
    this.currentLine="";
    return this;
  }
  AppendLine_5(part){
    this.currentLine=this.currentLine+String(part);
    this.lines.push(this.currentLine);
    this.currentLine="";
    return this;
  }
  AppendLine_6(part){
    this.currentLine=this.currentLine+String(part);
    this.lines.push(this.currentLine);
    this.currentLine="";
    return this;
  }
  AppendLine_7(part){
    this.currentLine=this.currentLine+String(part);
    this.lines.push(this.currentLine);
    this.currentLine="";
    return this;
  }
  AppendLine_8(part){
    this.currentLine=this.currentLine+String(part);
    this.lines.push(this.currentLine);
    this.currentLine="";
    return this;
  }
  AppendLine_9(part){
    this.currentLine=this.currentLine+String(part);
    this.lines.push(this.currentLine);
    this.currentLine="";
    return this;
  }
  AppendLine_10(part){
    this.currentLine=this.currentLine+String(part);
    this.lines.push(this.currentLine);
    this.currentLine="";
    return this;
  }
  AppendLine_11(part){
    this.currentLine=this.currentLine+String(part);
    this.lines.push(this.currentLine);
    this.currentLine="";
    return this;
  }
  AppendLine_12(part){
    this.currentLine=this.currentLine+String(part);
    this.lines.push(this.currentLine);
    this.currentLine="";
    return this;
  }
  AppendLine_13(part){
    this.currentLine=this.currentLine+part;
    this.lines.push(this.currentLine);
    this.currentLine="";
    return this;
  }
  Append(part){
    this.currentLine=this.currentLine+part;
    return this;
  }
  Append_1(part){
    this.currentLine=this.currentLine+String(part);
    return this;
  }
  Append_2(part){
    this.currentLine=this.currentLine+String(part);
    return this;
  }
  Append_3(part){
    this.currentLine=this.currentLine+String(part);
    return this;
  }
  Append_4(part){
    this.currentLine=this.currentLine+String(part);
    return this;
  }
  Append_5(part){
    this.currentLine=this.currentLine+String(part);
    return this;
  }
  Append_6(part){
    this.currentLine=this.currentLine+String(part);
    return this;
  }
  Append_7(part){
    this.currentLine=this.currentLine+String(part);
    return this;
  }
  Append_8(part){
    this.currentLine=this.currentLine+String(part);
    return this;
  }
  Append_9(part){
    this.currentLine=this.currentLine+String(part);
    return this;
  }
  Append_10(part){
    this.currentLine=this.currentLine+String(part);
    return this;
  }
  Append_11(part){
    this.currentLine=this.currentLine+String(part);
    return this;
  }
  Append_12(part){
    this.currentLine=this.currentLine+String(part);
    return this;
  }
  Append_13(part){
    this.currentLine=this.currentLine+part;
    return this;
  }
  constructor(){
    super();
    this.lines=[];
    this.currentLine="";
  }
}
