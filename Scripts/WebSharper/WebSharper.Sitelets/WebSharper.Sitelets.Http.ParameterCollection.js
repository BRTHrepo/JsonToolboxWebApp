export function isParameterCollection(x){
  return"WebSharper_Sitelets_Http_ParameterCollection$Item"in x&&"WebSharper_Sitelets_Http_ParameterCollection$ToList"in x;
}
