import { FSharpOption } from "../WebSharper.StdLib/Microsoft.FSharp.Core.FSharpOption`1"
import TemplateHole from "../WebSharper.UI/WebSharper.UI.TemplateHole"
import Dictionary from "../WebSharper.StdLib/System.Collections.Generic.Dictionary`2"
import TemplateInstance from "./WebSharper.UI.Templating.Runtime.Server.TemplateInstance"
import Var from "../WebSharper.UI/WebSharper.UI.Var`1"
import { View_T } from "../WebSharper.UI/WebSharper.UI.View`1"
import Object from "../WebSharper.StdLib/System.Object"
import IInitializer from "../WebSharper.StdLib/WebSharper.IInitializer"
export default class TemplateInitializer extends Object implements IInitializer {
  static init:number;
  id:string;
  vars:([string, number, FSharpOption<any>])[];
  static initialized:Dictionary<string, Dictionary<string, TemplateHole>>;
  static instances:Dictionary<string, TemplateInstance>;
  static applyVarHole(el:Element, tpl:TemplateHole):void
  static applyTypedVarHole<T0>(bind:((a:Var<T0>) => [((a:Element) => void), ((a:Element) => ((a:FSharpOption<T0>) => void)), View_T<FSharpOption<T0>>]), v:Var<T0>, el:Element):void
  static Create(vars:([string, number, FSharpOption<any>])[]):TemplateInitializer
  static GetInstance(key:string):TemplateInstance
  static GetOrAddHoleFor<T0 extends TemplateHole>(id:string, holeName:string, initHole:(() => T0)):T0
  static GetHolesFor(id:string):Dictionary<string, TemplateHole>
  static get Initialized():Dictionary<string, Dictionary<string, TemplateHole>>
  get Id():string
  InitInstance(key:string):TemplateInstance
  $postinit(key:string):void
  $init(a:string):void
  $preinit(key:string):void
  constructor(id:string, vars:([string, number, FSharpOption<any>])[])
}
