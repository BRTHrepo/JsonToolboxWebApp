import TemplateHole from "./WebSharper.UI.TemplateHole"
export default class Event extends TemplateHole {
  name:string;
  fillWith:((a:Element) => ((a:Event) => void));
  get Value():((a:Element) => ((a:Event) => void))
  WithName(n:string):TemplateHole
  get ValueObj()
  get Name():string
  constructor(name:string, fillWith:((a:Element) => ((a:Event) => void)))
}
