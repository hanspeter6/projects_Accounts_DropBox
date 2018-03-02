###
### This app currently reads in and processes a single pdf statement. It allows one to see
### the extracted dataframe and download it as a .csv file that is in text format. 
### A user who wants to manipulate the frame in excel would need to import it into
### an excel sheet and then format it correctly.
### Tip: date formatting is straight forward as long as one chooses the right format.
### Numeric from General requires use of "Advanced" in import wizard to set digital
### separator from comma to point.
### 
###
## libraries
library(tm)
library(pdftools)
library(stringr)
library(dplyr)

## Only run examples in interactive R sessions
if (interactive()) {
        
        ui <- fluidPage(
                sidebarLayout(
                        sidebarPanel(
                                fileInput("file1", "Choose PDF File",
                                          accept = c(
                                                  "text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv",
                                                  ".pdf")),
                                # Button
                                downloadButton("downloadData", "Download")
                        ),
                        mainPanel(
                                tableOutput("contents")
                        )
                )
        )
        
        server <- function(input, output) {
                output$contents <- renderTable({
                        
                        
                        
                        
                        # input$file1 will be NULL initially. After the user selects
                        # and uploads a file, it will be a data frame with 'name',
                        # 'size', 'type', and 'datapath' columns. The 'datapath'
                        # column will contain the local filenames where the data can
                        # be found.
                        inFile <- input$file1
                        
                        if (is.null(inFile))
                                return(NULL)
                        
                        temp1 <- pdf_text(input$file1$datapath) # list of single string per pdf page
                        temp2 <- unlist(strsplit(temp1,"\n"))  # by line
                        
                        date_pattern <- "\\d{1,2}/\\d{2}/\\d{4}"
                        
                        # identify end of pages:
                        end_pages <- which(str_detect(temp2, "Page")) - 1
                        
                        # identify how many pages:
                        page_num <- length(end_pages)
                        
                        # id start of each page:
                        start_pages <- which(str_detect(temp2, regex("Transaction Description", ignore_case = TRUE))) + 1
                        
                        # identify end of document # need to think if this is generally true?
                        end_document <-  which(str_detect(temp2, "SERVICE FEE")) - 1
                        
                        # since end of documt < end of second page:
                        
                        # create subs of temp2 cutting off top, end and page break stuff of all but last page:
                        pages_temp <- list()
                        for(i in 1: (page_num - 1)) {
                                pages_temp[[i]] <- temp2[start_pages[i]:end_pages[i]]
                        }
                        
                        # last page also
                        pages_temp[[page_num]] <- temp2[start_pages[page_num]:end_document]
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
                        date_places <- which(str_detect(tab_pdf[,1], date_pattern)) 
                        
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
                        
                        return(tab_pdf)
                        # read.csv(inFile$datapath)
                        
                })
                
                # Downloadable csv of selected dataset ----
                output$downloadData <- downloadHandler(
                        filename = "test.csv",
                        content = function(file) {
                                write.csv(tab_pdf, file, row.names = FALSE)
                        }
                )
        }
        
        shinyApp(ui, server)
}