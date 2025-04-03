import Queue from "./System.Collections.Generic.Queue`1"
import Object from "./System.Object"
export default class Scheduler extends Object {
  idle:boolean;
  robin:Queue<(() => void)>;
  tick():void
  Fork(action:(() => void)):void
  constructor()
}
