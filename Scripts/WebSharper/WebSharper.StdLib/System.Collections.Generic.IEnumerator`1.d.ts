export function isIEnumerator(x):x is IEnumerator<T0>
export default interface IEnumerator<T0>{
  get Current():T0
}
