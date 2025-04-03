export function isIPostedFile(x){
  return"WebSharper_Sitelets_Http_IPostedFile$SaveAs"in x&&"WebSharper_Sitelets_Http_IPostedFile$ContentLength"in x&&"WebSharper_Sitelets_Http_IPostedFile$ContentType"in x&&"WebSharper_Sitelets_Http_IPostedFile$FileName"in x&&"WebSharper_Sitelets_Http_IPostedFile$InputStream"in x&&"WebSharper_Sitelets_Http_IPostedFile$Key"in x;
}
