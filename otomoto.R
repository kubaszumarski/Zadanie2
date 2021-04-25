
#install.packages( c("RSelenium","seleniumPipes","dplyr","gtools","stringr","xml2", "rvest") )
#java -jar selenium-server-standalone-3.0.1.jar -port 4444

library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)
library(xml2)

#scrapowanie działa bezbłędnie przy zmaksymalizowanym oknie przeglądarki

remDr<-remoteDr(remoteServerAddr="http://localhost",
                port=4444,
                browserName="chrome",
                newSession=TRUE)

#dla kia https://www.otomoto.pl/osobowe/kia/
startUrl<-'https://www.otomoto.pl/osobowe/hyundai/'
remDr %>% go(startUrl)
max_page<-remDr %>%findElement("class name","om-pager")%>%getElementText()
max_page<-gsub('(.*)( )(\\d{1,4})$','\\3',max_page)

wektorLinkow<-c()
for (i in 1:max_page){
  newUrl<- paste0(startUrl,"?search%5Border%5D=created_at%3Adesc&page=",i)
  remDr %>% go(newUrl)
  elems<- remDr %>% findElements(using="class name","offer-title")
  for(j in 1:length(elems)){
    e<-findElementsFromElement(elems[[j]], using = "tag name","a")
    if (length(e)>0){
      link<-e[[1]]%>%getElementAttribute("href")
      wektorLinkow<-c(wektorLinkow,link)
    }
  }
}
wektorLinkowU<-wektorLinkow%>%unique()



zrobWiersz<-function(w,wektorLinkow,remDr){

  remDr%>%go(wektorLinkowU[w])
  cena<-NA
  cena<-remDr%>%findElement("xpath","//div[@class='offer-price']")%>%getElementAttribute("data-price")%>%str_replace_all(" ","")%>%unlist()
  waluta<-remDr%>%findElement("xpath","//div[@class='offer-price']/span/span")%>%getElementText()
 
  
  szczegoly<-remDr%>%findElements("class name", "offer-params__list")
  listaSzczegolowOpis<-c()
  listaSzczegolowWartosci<-c()
  for (i in 1:length(szczegoly)){
   listaSzczegolowOpis<-c(listaSzczegolowOpis, szczegoly[[i]]%>%findElementsFromElement("class name","offer-params__label"))
   listaSzczegolowWartosci<- c(listaSzczegolowWartosci, szczegoly[[i]]%>%findElementsFromElement("class name","offer-params__value"))
   
   }
   nazwyKolumn<-lapply(listaSzczegolowOpis, getElementText)%>%str_replace_all(":","")%>%unlist()
   wartosci<-lapply(listaSzczegolowWartosci, getElementText)%>%unlist()
  df1<-data.frame(matrix(wartosci,nrow = 1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  
  
  df1<-cbind(cena,waluta,df1)
}



auta<-NULL
for (w in 1:length(wektorLinkowU)){
  skip<-FALSE
  tryCatch(
    df1<-zrobWiersz(w,wektorLinkowU,remDr), error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(auta)){
    auta<-df1
  }else{
  auta<-smartbind(auta,df1)
  View(auta)
  }
}

#write.csv2(auta,'hyundai.csv',append = TRUE)

