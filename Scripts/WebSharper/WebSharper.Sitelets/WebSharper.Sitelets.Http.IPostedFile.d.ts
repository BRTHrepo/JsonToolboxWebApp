export function isIPostedFile(x):x is IPostedFile
export default interface IPostedFile {
  WebSharper_Sitelets_Http_IPostedFile$SaveAs(a:string):void
  get WebSharper_Sitelets_Http_IPostedFile$ContentLength():number
  get WebSharper_Sitelets_Http_IPostedFile$ContentType():string
  get WebSharper_Sitelets_Http_IPostedFile$FileName():string
  get WebSharper_Sitelets_Http_IPostedFile$InputStream()
  get WebSharper_Sitelets_Http_IPostedFile$Key():string
}
