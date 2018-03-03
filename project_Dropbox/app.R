###
### this app will attempt to read a list of files from a "remote" Dropbox folder
### the programme will then process them and create a single dataframe which
### will then be stored in the "remote" Dropbox folder. will try to work with reactive functions this time
### 
### input to the app will be the Dropbox details .... not too sure at this time...
### 
###for now will assume file format consistency and only pdf docs in the target inputDirectory
###
###
## libraries
library(tm) # text mining... not sure why anymore...
library(pdftools)
library(stringr)
library(dplyr)
library(rdrop2)

token <- drop_auth()
saveRDS(token, "droptoken.rds")

## Only run examples in interactive R sessions
if (interactive()) {
        
        ui <- fluidPage(
                sidebarLayout(
                        sidebarPanel(
                                
                                # inputs
                                helpText("Enter the directory from your root Dropbox",
                                        "Press the submit button when you ready"),
                                
                                textInput("inputDir", "Input Directory"),
                                
                                submitButton("Submit", icon = NULL)
                                
                                # br(),
                                # textInput("outputDir", "Output Directory"),
                                # br(),
                                # textInput("keys", "Key Words") 

                        ),
                        mainPanel(
                                tableOutput("contents1")
                                # br(),
                                # textOutput("contents2"),
                                # br()
                                # textOutput("contents3"),
                                # br(),
                                # textOutput("contents4")

                        )
                )
        )
        
        
        # create list of pdf documents read from input directory from remote dropbox as reactive statement
       
        

        
        server <- function(input, output) {
                
                dataInput <- reactive({
                        
                        req(input$inputDir)
                        
                        token <- readRDS("droptoken.rds")
                        
                        filesInfo <- drop_dir(as.character(input$inputDir), dtoken = token) #list of files in Dropbox /inputDir ("inputDirectory").NB also want to confirm only .pdfs
                        
                        filePaths <- filesInfo$path_display # paths to the files
                        
                        doc_list <- list()
                        for(i in 1:length(filePaths)) {#
                                
                                drop_download(filePaths[i], dtoken = token, overwrite = TRUE) # download each from remote dropbox to current wrkdir
                                
                                temp <- pdf_text(filesInfo$name[i]) # list of single character string per pdf page
                                
                                file.remove(filesInfo$name[i]) # remove the file that was added
                                
                                doc_list[[i]] <- unlist(strsplit(temp,"\n")) # create list of documents split by line
                        }
                        
                        file.remove("droptoken.rds") # cleaning up local working directory
                        
                        return(doc_list)
                        
                })
                
                trial <- reactive({
                
                        req(input$inputDir)
                        
                        all_frame <- vector()
                        for(f in 1: length(dataInput())) {
                                
                                temp_doc <- dataInput()[[f]]
                                
                                # identify first doc end of pages
                                end_pages <- which(str_detect(temp_doc, "Page")) - 1
                        
                                # identify how many pages:
                                page_num <- length(end_pages)
                        
                                # id start of each page:
                                start_pages <- which(str_detect(temp_doc, regex("Transaction Description", ignore_case = TRUE))) + 1
                        
                                # identify end of document # need to think if this is generally true?
                                end_document <-  which(str_detect(temp_doc, "SERVICE FEE")) - 1
                                
                                # create subs of temp doc by cutting off top, end and page break stuff of all but last page:
                                pages_temp <- list()
                                for(i in 1: (page_num - 1)) {
                                        pages_temp[[i]] <- temp_doc[start_pages[i]:end_pages[i]]
                                }
                                
                                # last page also
                                pages_temp[[page_num]] <- temp_doc[start_pages[page_num]:end_document]
                                pages_temp <- unlist(pages_temp)
                                
                                # create vectors of info for document (all its pages)
                                date <- vector()
                                description <- vector()
                                charge <- vector()
                                debit <- vector()
                                credit <- vector()
                                balance <- vector()
                                for(i in 1: length(pages_temp)) {
                                        
                                        date[i] <- str_trim(str_sub(pages_temp[i], 1, 16))
                                        description[i] <- str_trim(str_sub(pages_temp[i], 18, 60))
                                        charge[i] <- str_trim(str_sub(pages_temp[i], 70, 77))
                                        debit[i] <- str_trim(str_sub(pages_temp[i], 110, 135)) 
                                        credit[i] <- str_trim(str_sub(pages_temp[i], 140, 159)) 
                                        balance[i] <- str_trim(str_sub(pages_temp[i], 162, 180))
                                        
                                }
                                tab_pdf <- cbind(date, description = stripWhitespace(description), charge, debit, credit, balance)  
                                
                                # # now combine description:
                                # id which places have date
                                date_places <- which(str_detect(tab_pdf[,1], "\\d{1,2}/\\d{2}/\\d{4}")) 
                                
                                for(i in 1:nrow(tab_pdf)) {
                                        if(!i %in% date_places) {
                                                tab_pdf[i-1,2] <- str_c(tab_pdf[i-1,2], tab_pdf[i,2], sep = ' ')
                                                
                                        }
                                }
                                
                                # get rid of no date cases:
                                tab_pdf <- data.frame(tab_pdf[date_places,], stringsAsFactors = FALSE)
                                
                                # format variables:
                                tab_pdf$date <- as.character(as.Date(tab_pdf$date, format = "%d/%m/%Y"))
                                
                                # for numbers, first need to get rid of white space btw thousands
                                tab_pdf$charge <- str_replace_all(tab_pdf$charge, ' ','')
                                tab_pdf$debit <- str_replace_all(tab_pdf$debit, ' ','')
                                tab_pdf$credit <- str_replace_all(tab_pdf$credit, ' ','')
                                tab_pdf$balance <- str_replace_all(tab_pdf$balance, ' ','')
                                
                                # for balance also need to get rid of '-':
                                tab_pdf$balance <- str_replace_all(tab_pdf$balance, '-', '')
                                
                                # change to numeric
                                tab_pdf$charge <- as.numeric(tab_pdf$charge)
                                tab_pdf$debit <- as.numeric(tab_pdf$debit)
                                tab_pdf$credit <- as.numeric(tab_pdf$credit)
                                tab_pdf$balance <- as.numeric(tab_pdf$balance)
                                
                                # keep list of tables per document
                                all_frame <- rbind(all_frame, tab_pdf)
                                
                        }
                        
                        return(all_frame)
                        
                })
                
                output$contents1 <- renderTable(trial())
                
                
               
                # output$contents1 <- renderText({
                # 
                #         print(input$inputDir)
                # })               

                # output$contents2 <- renderText({
                #         
                #         print(input$outputDir)
                # })
                # 
                # output$contents3 <- renderText({
                #         
                #         print(input$keys)
                # })
                # 
                # output$contents4 <- renderPrint({ # for now just to check...
                # 
                #         dataInput()
                # })

                
                
        }
        
        shinyApp(ui, server)
}

# # Read all the files into a list
# filesInfo <- drop_dir(input$inputDir) #list of files in Dropbox /inputDir ("inputDirectory")
# filePaths <- filesInfo$path_display # paths to the files
# drop_download(filePaths[5]) # download one of them from remote dropbox to current wrkdir
# temp1 <- pdf_text(filesInfo$name[5]) # list of single string per pdf page
# doc <- unlist(strsplit(temp1,"\n"))  # by
# 
# return(doc)
# 