#daraz searcher and Scrapper
#required Packages
library(jsonlite)
library(httr)
library(shiny)
library(shinythemes)
library(DT)
library(stringi)
library(taRifx)
library(sqldf)
library(xml2)
library(rvest)
library(stringr)
#########################################################################
#########################InterFace#######################################
#########################################################################

shinyApp(
    ui = fluidPage(
        shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
        sidebarPanel(
            textInput("txt", "Enter Searching Terms:", placeholder = "type here"),
            h4("E-Commerce Sites"),
            checkboxInput("cbDARAZ", label = "DARAZ", value = TRUE),
            checkboxInput("cbPICKABOO",label = "PICKABOO", value = TRUE),
            radioButtons("radio",h4("ORDERS"), 
                         c("Default" = "default",
                           "Rating->Review->Price" = "RaRePr",
                           "Review->Rating->Price" = "ReRaPr",
                           "Price->Review->Rating" = "PrReRa",
                           "Price->Rating->Review" = "PrRaRe"
                         )),
            
            actionButton("action", "Search")
            #actionButton("action2", "Button2", class = "btn-primary")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Results"),
                fluidRow(
                    column(12,
                           dataTableOutput('table')
                    )
                )
            )
        )
        
    ),
    server = function(input, output, session) {
        
        observeEvent(input$action, {
            
            
            ###############################DARAZ SCRAPPER############################################
            
            #write key word to search
            #validate( need(input$txt, 'Check at least one letter!') )
            
            df=NULL
            keyword = input$txt
            
            if(input$cbDARAZ==TRUE || input$cbPICKABOO==TRUE){
                
                if(input$cbDARAZ==TRUE){
                    #joining keyword with link to query
                    link = paste("https://www.daraz.com.bd/catalog/?q=",keyword,sep = )
                    
                    #getting JSON embaded in html of the page
                    j = sub("[ ]+window.pageData\\((.+)\\)" , "\\1", 
                            grep("window.pageData", readLines(link), value=TRUE), 
                            perl=TRUE)
                    
                    #removing Script tag
                    j = gsub('<script>window.pageData=|</script>','',j)
                    
                    #parsing JSON
                    df = fromJSON(j)
                    #getting product list array as dataFrame
                    df=  as.data.frame(df$mods$listItems) 
                    #selecting only Specific Columns
                    
                    
                    ##################################PRODUCT DESCRIPTION#########################
                    productDescription= df$description
                    pdsLength=length(productDescription)
                    for (i in 1:pdsLength) {
                        
                        pd=stri_join_list(productDescription[i], sep = "", collapse = NULL)
                        
                        if(length(pd)){
                            productDescription[i] =pd}
                        else{
                            productDescription[i]=" not found"   
                        }
                    }
                    productDescription=unlist(productDescription)
                    ################################################################################
                    
                    
                    productURL= df$productUrl
                    
                    df =as.data.frame( cbind( ProductName= df$name,Source="DARAZ",originalPrice= df$originalPrice,price= df$price,ratingScore = df$ratingScore,review = df$review ,image = df$image))
                    
                    #df$image<-paste0("<img src='",df$image,"' height='52'></img> ",sep="") 
                    df$image <- paste0("<a title='",productDescription,"' target='_blank' href='",productURL,"'>","<img src='", df$image, "'  style='height:60px'>","</a>")
                    
                    # View(df$image)
                    
                    
                    #exporting to csv
                    #write.csv(ex,'C:\\Users\\MAHMUD\\Desktop\\MyData.csv', row.names = FALSE)
                    #View(df)
                    
                    ####################################REMOVE FACTOR####################
                    df=remove.factors(df)
                    ###########################################################
                    
                    
                    ###################################DATA MANUPULATION#############################################
                    #df$originalPrice[is.na(df$originalPrice)]=0
                    
                    MaxRating = max(df$ratingScore)
                    MaxReview = max(df$review)
                    
                    
                    
                    dfLength=length(df$originalPrice)
                    for(i in 1:dfLength){
                        df$ratingScore[i]= round((as.numeric(df$ratingScore[i]) /as.numeric(MaxRating))*100)
                        #        df$review[i]= (df$review[i]/MaxReview)*100
                        if( is.na(df$originalPrice[i])){
                            df$originalPrice[i]=df$price[i]
                        }
                    }
                }
                
                
                ####################PICKABOO######################
                #####################################################
                if( input$cbPICKABOO == TRUE){
                    
                    link = paste("https://www.pickaboo.com/search/result/?q=",keyword, sep = "")
                    
                    webpage <- read_html(link)
                    
                    data<- html_nodes(webpage, 'div#em-grid-mode')
                    
                    product = html_nodes(data,'h2 a')
                    
                    #################NAMES ##############
                    Names = html_text(product)
                    
                    #################LINKS ##############
                    ProductLinks = html_attr(product,"href")
                    
                    #################REVIEW ##############
                    reviews = gsub('\\(|)', '', html_text(html_nodes(data,'span.amount')))
                    
                    #################RATING ##############
                    ratings=strsplit(toString (html_nodes(data,'div.rating')),"," )
                    ratings = as.numeric(gsub("\\D", "", unlist(ratings)))
                    ratings[is.na(ratings)] <- 0
                    
                    #################PIKABOO PRICE EXTRECTION ##############
                    allprice = html_nodes(data,'div.price-box')
                    priceLength = length(allprice)
                    pdf<-data.frame(old=1:priceLength,new=1:priceLength)
                    
                    for(i in 1:priceLength){(i)
                        p=html_text(html_nodes(allprice[i],'span.price'))
                        if(length(p)==1){
                            pdf$old[i]=gsub("\\D", "", p)
                            pdf$new[i]=pdf$old[i]
                        }
                        else{
                            p=gsub("\\D", "", p)
                            pdf$old[i]=p[1]
                            pdf$new[i]=p[2]
                        }
                    }
                    
                    #################IMAGE ##############
                    images = html_attr(html_nodes(data,'img.em-alt-org'),'src')
                    images <- paste0("<a target='_blank' href='",ProductLinks,"'>","<img src='", images, "'  style='height:60px'>","</a>")
                    
                    pickaboo =as.data.frame( cbind( ProductName= Names,Source="PICKABOO",originalPrice= pdf$old,price= pdf$new,ratingScore = ratings,review = reviews ,image = images))
                    
                    if(is.null(df))
                    {
                        df=pickaboo
                    }
                    else{
                        df=rbind(df,pickaboo)
                    }
                }
                ############################################################################
                if(input$radio=="RaRePr"){
                    df = sqldf("select * from df order by ratingScore desc, review desc, price asc")
                }
                if(input$radio=="ReRaPr"){
                    df = sqldf("select * from df order by review desc,ratingScore desc, price asc")
                }
                if(input$radio=="PrReRa"){
                    df = sqldf("select * from df order by price asc,review desc,ratingScore desc ")
                }
                if(input$radio=="PrRaRe"){
                    df = sqldf("select * from df order by price asc,review desc,ratingScore desc ")
                }
                output$table <- renderDataTable(datatable(df,escape = FALSE))
            }
            
        })
        
    }
)

