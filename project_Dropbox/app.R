###
### this app aims to read a list of files from a "remote" Dropbox folder. The folder can only
### contain ABSA computer generated pdf documents.
### the programme will then process them and create two dataframes that
### will be stored in a separate "remote" Dropbox folder.
### 
### Next:
### a) check for input files format and pdf consistency.
### b) create drop-down of different standard bank account formats
### c) explore default file and paths for input and output
### d) include new file names for output
### e) improve code and helptexts
### f) deal with oauth2 ... ie connect first and then redirect... possibly write another app that does that.
###



## libraries
library(tm) # text mining... not sure why anymore...
library(pdftools)
library(stringr)
library(dplyr)
library(rdrop2)

token <- drop_auth()
saveRDS(token, "droptoken.rds")

saveData <- function(data, fileName, outputDir) {
        
        # Write the data to a temporary file locally
        filePath <- file.path(tempdir(), fileName)
        write.csv(data, filePath, row.names = FALSE, quote = TRUE)
        
        # Upload the file to Dropbox
        drop_upload(filePath, path = outputDir, dtoken = token)
}

## Only run examples in interactive R sessions
ui <- fluidPage(
        sidebarLayout(
                sidebarPanel(
                        
                        # helpText("If you are a first-time user you need to give the OK",
                        #          "to Dropbox for this app to acess it. I"),
                        # 
                        # actionButton(inputId='ab', label="Authorise", 
                        #              icon = icon("th"), 
                        #              onclick ="window.open('https://www.dropbox.com/oauth2/authorize?client_id=...&redirect_uri=..')"),
                        # 
                        helpText("Select the appropriate bank statement"),
                        selectInput("format", "Select Format", choice = "ABSA Bank Cheque", selected = "ABSA Bank Cheque"),
                        
                        helpText("Enter the directory where the .pdf documents",
                                 "are held. Make sure they are correctly formatted",
                                 "and the directory contains only appropriately",
                                 "formatted documents",
                                 "Only press the submit button when all the windows",
                                 "are filled in"),
                        
                        textInput("inputDir", "Input Directory", value = "inputDirectory"),
                        
                        helpText("Enter your keywords. Write them in lower case",
                                 "and use commas to separate them.",
                                 "Only press the submit button when all the windows",
                                 "are filled in"),
                        
                        textInput("keys", "Key Words"),
                        
                        helpText("Enter the directory where you want the results.",
                                 "The results should be in the form of two .csv files: ",
                                 "one containing all the transactions contained in the .pdf files",
                                 "of the input Directory; another containing only those",
                                 "with your keyword selections. ",
                                 "Only press the submit button when all the windows",
                                 "are filled in"),
                        
                        textInput("outputDir", "Output Directory", value = "outputDirectory"),
                        
                        submitButton("Submit", icon = NULL)
                        
                ),
                
                mainPanel(
                        
                        helpText("A table of transaction containing your keywords"),
                        
                        br(),
                        
                        tableOutput("contents")
                        
                )
        )
)

server <- function(input, output, session) {
        
        # # changing token if new user:
        # 
        # observeEvent(input$newUser, {
        #         
        #         token <- drop_auth(new_user = TRUE, cache = TRUE)
        #         saveRDS(token, "droptoken.rds")
        #                         
        # }, once = TRUE)
        
        dataInput <- reactive({
                req(input$inputDir)
                
                token <- readRDS("droptoken.rds")
                
                filesInfo <- drop_dir(as.character(input$inputDir), dtoken = token) #list of files in Dropbox /inputDir ("inputDirectory").NB also want to confirm only .pdfs
                
                filePaths <- filesInfo$path_display # paths to the files
                
                doc_list <- list()
                for(i in 1:length(filePaths)) {
                        
                        drop_download(filePaths[i], dtoken = token, overwrite = TRUE) # download each from remote dropbox to current wrkdir
                        
                        temp <- pdf_text(filesInfo$name[i]) # list of single character string per pdf page
                        
                        file.remove(filesInfo$name[i]) # remove the file that was added
                        
                        doc_list[[i]] <- unlist(strsplit(temp,"\n")) # create list of documents split by line
                }
                
                return(doc_list)
                
        })
        
        all_transactions <- reactive({
                
                req(input$inputDir)
                req(input$outputDir)
                
                all_frame <- vector()
                for(f in 1:length(dataInput()) ) {
                        
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
                                credit[i] <- str_trim(str_sub(pages_temp[i], 140, 155)) 
                                balance[i] <- str_trim(str_sub(pages_temp[i], 159, 180))
                                
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
                        
                        # for balance if has '-' need to change it to neg value:
                        ind_minus <- str_which(tab_pdf$balance, '-')
                        tab_pdf$balance <- str_replace_all(tab_pdf$balance, '-', '')
                        tab_pdf$balance[ind_minus] <- str_c('-', tab_pdf$balance[ind_minus])
                        
                        # change to numeric
                        tab_pdf$charge <- as.numeric(tab_pdf$charge)
                        tab_pdf$debit <- as.numeric(tab_pdf$debit)
                        tab_pdf$credit <- as.numeric(tab_pdf$credit)
                        tab_pdf$balance <- as.numeric(tab_pdf$balance)
                        
                        # keep list of tables per document
                        all_frame <- rbind(all_frame, tab_pdf)
                        
                        all_frame <- all_frame %>%
                                arrange(date)
                        
                }
                
                outputDir <- as.character(input$outputDir)
                
                saveData(all_frame, "all_transactions.csv", outputDir)
                
                return(all_frame)
                
        })
        
        keys_transactions <- reactive({ # now id and extract on keywords:
                
                req(input$keys)
                req(input$outputDir)
                
                keys <- str_trim(unlist(str_split(input$keys, ",")))
                
                keys_frame <- vector()
                for(i in 1: length(keys)) {
                        
                        a <- all_transactions()[str_detect(all_transactions()$description, regex(keys[i], ignore_case = TRUE)),]
                        b <- a %>%
                                dplyr::mutate(key = keys[i]) %>%
                                dplyr::select(key, dplyr::everything())
                        
                        keys_frame <- rbind(keys_frame, b)
                        
                }
                keys_frame <- keys_frame %>%
                        dplyr::arrange(key,date)
                
                outputDir <- as.character(input$outputDir)
                
                saveData(keys_frame, "keys_transactions.csv", outputDir)
                
                return(keys_frame)
                
        })
        # show key transaction on app
        
        output$contents <- renderTable(keys_transactions())
        
}

shinyApp(ui, server)

#END...for now

