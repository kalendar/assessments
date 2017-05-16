library(shiny)
library(gdata)
library(readxl)
library(markdown)
library(git2r)

repo.url <- 'https://github.com/DAACS/assessments'
passages.dir <- 'repo/reading/passages/'

# Download the assessment repo
path <- file.path('repo')
if(dir.exists(path)) {
	unlink(path, recursive = TRUE, force = TRUE)
}
dir.create(path, recursive=TRUE)
repo <- clone(repo.url, path)

domains <- c('Ideas' = 'ID',
			 'Inference' = 'IN',
			 'Language' = 'LA',
			 'Purpose' = 'PU',
			 'Structure' = 'ST')

passages <- character()
items <- data.frame()

readData <- function() {
	items <- read_excel('repo/reading/ReadingItems.xlsx', sheet = 1)

	items <- as.data.frame(items)
	items$ID <- paste0(items$Year, '-', items$Month, ' #', items$ItemNum)

	files <- list.files(passages.dir, pattern = "*.txt")
	for(i in files) {
		tmp <- scan(paste0(passages.dir, i),
					sep = '\n', what = character(), blank.lines.skip = FALSE, quiet = TRUE)
		passages <- c(passages, paste0(tmp, collapse = '\n'))
	}
	names(passages) <- files

	passages <<- passages
	items <<- items
}

readData()

##### UI
ui <- fluidPage(
	titlePanel("DAACS Reading Item Viewer"),
	sidebarLayout(
		sidebarPanel(width = 3,
			radioButtons('selectType', 'Search by', choices = c('Passage', 'Domain')),
			conditionalPanel(condition = "input.selectType == 'Passage'",
							 selectInput('passage', 'Passage', names(passages))),
			conditionalPanel(condition = "input.selectType == 'Domain'",
							 selectInput('domain', 'Domain', domains)),
			uiOutput('itemSelect'),
			actionButton('previousItem', 'Previous'), actionButton('nextItem', 'Next'),
			uiOutput('editbuttonui'),
			hr(),
			actionButton('refresh', 'Refresh')
		),
		mainPanel(width = 9,
			h2(htmlOutput('stem')),
			fluidRow(column(width = 6,
							h5('A) '), htmlOutput('choiceA')),
					 column(width = 6,
					 	   h5('B) '), htmlOutput('choiceB'))
			),
			fluidRow(column(width = 6,
							h5('C) '), htmlOutput('choiceC')),
					 column(width = 6,
					 	   h5('D) '), htmlOutput('choiceD'))
			), hr(),
			h3(htmlOutput('answer')),
			h3(htmlOutput('domainName')),
			h3('Passage:'),
			htmlOutput('passage')
		)
	)
)

##### Server
server <- function(input, output, session) {
	observeEvent(input$refresh, {
		fetch(repo, 'origin')
		readData()
	})

	observeEvent(input$nextItem, {
		if(input$selectType == 'Domain') {
			choices = items[items$DomainID == input$domain,]$ID
		} else if(input$selectType == 'Passage') {
			choices = items[items$PASSAGE == input$passage,]$ID
		}
		pos <- which(choices == input$item) + 1
		if(pos > length(choices)) { pos <- 1 }
		updateSelectInput(session, 'item', selected = choices[pos])
	})

	observeEvent(input$previousItem, {
		if(input$selectType == 'Domain') {
			choices = items[items$DomainID == input$domain,]$ID
		} else if(input$selectType == 'Passage') {
			choices = items[items$PASSAGE == input$passage,]$ID
		}
		pos <- which(choices == input$item) - 1
		if(pos < 1) { pos <- length(choices) }
		updateSelectInput(session, 'item', selected = choices[pos])
	})

	output$itemSelect <- renderUI({
		req(input$selectType)
		si <- NULL
		if(input$selectType == 'Domain') {
			req(input$domain)
			si <- selectInput('item', "Item", choices = items[items$DomainID == input$domain,]$ID)
		} else if(input$selectType == 'Passage') {
			req(input$passage)
			si <- selectInput('item', "Item", choices = items[items$PASSAGE == input$passage,]$ID)
		}
		return(si)
	})

	output$stem <- renderText({
		html <- items[items$ID == input$item,]$Question
		return(html)
	})

	output$choiceA <- renderText({
		html <- items[items$ID == input$item,]$A
		return(html)
	})

	output$choiceB <- renderText({
		html <- items[items$ID == input$item,]$B
		return(html)
	})

	output$choiceC <- renderText({
		html <- items[items$ID == input$item,]$C
		return(html)
	})

	output$choiceD <- renderText({
		html <- items[items$ID == input$item,]$D
		return(html)
	})

	output$editbuttonui <- renderUI({
		fname <- items[items$ID == input$item,]$PASSAGE
		url <- paste0(repo.url, '/edit/master/reading/passages/', fname)
		shiny::actionButton(inputId='editbutton', label="Edit Passage",
							icon = icon("pencil"),
							onclick =paste0("window.open('", url, "', '_blank')"))
	})

	output$answer <- renderText({
		paste0('Answer: ', items[items$ID == input$item,]$Answer)
	})

	output$domainName <- renderText({
		paste0('Domain: ', names(domains)[domains == items[items$ID == input$item,]$Domain])
	})

	output$passage <- renderText({
		req(input$item)
		txt <- passages[items[items$ID == input$item,]$PASSAGE]
		txt <- paste0('<div style="white-space: pre; word-wrap: normal;
					 	overflow-x: auto; font-size: 11pt; padding: 10px; background: #fffff8;
					   	border-style: solid; border-size:1px; border-color: #111111">',
					  txt, '</div>')
		return(txt)
	})

}

##### Run the application
shinyApp(ui = ui, server = server)
