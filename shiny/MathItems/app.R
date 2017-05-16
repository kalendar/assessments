library(shiny)
library(gdata)
library(markdown)
library(git2r)
library(jsonlite)

repo.url <- 'https://github.com/DAACS/assessments'
items.dir <- 'repo/mathematics/items/'

# Download the assessment repo
path <- file.path('repo')
repo <- NULL
if(dir.exists(path)) {
	#unlink(path, recursive = TRUE, force = TRUE)
	repo <- repository(path)
} else {
	dir.create(path, recursive=TRUE)
	repo <- clone(repo.url, path)
}
config(repo, user.name = 'jbryer', user.email = 'jason@bryer.org')

dir.create(file.path('www'), recursive = TRUE)

##### UI
ui <- fluidPage(
	withMathJax(),

	titlePanel("DAACS Mathematics Item Viewer"),

	fluidRow(
		column(width = 3, uiOutput('domainsUI'),
			              checkboxInput('itemsWithFeedback', 'Items with feedback only', value=TRUE)),
		column(width = 2, div(actionButton('previousItem', 'Previous')), style='text-align:right'),
		column(width = 3, div(uiOutput('itemSelect')), style='text-align:center'),
		column(width = 2, actionButton('nextItem', 'Next'))
	),
	hr(),
	fluidRow(column(width = 12,
	    	h4(htmlOutput('stem'))
		)
	),
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
	fluidRow(column(width = 12,
			h3(htmlOutput('answer')) )
	),
	fluidRow(column(width = 2, uiOutput('edititembuttonui')),
		     column(width = 2, uiOutput('editfeedbackbuttonui')),
			 column(width = 2, actionButton('refresh', 'Refresh Feedback', icon('refresh')))
	),
	fluidRow(column(width = 12,
			 h3('Feedback:'),
			 #uiOutput('feedbackUI'),
	      	 htmlOutput('feedback') )
	)
)

##### Define server logic required to draw a histogram
server <- function(input, output, session) {
	feedback <- reactiveValues()
	math.items <- reactiveValues()

	updateRepo <- function() {
		pull(repo)

		# Copy the image files to the www directory
		file.copy(paste0('repo/mathematics/items/',
						 list.files('repo/mathematics/items', pattern='*.png')),
				  paste0('www/',
				  	   list.files('repo/mathematics/items', pattern='*.png'))
		)
		file.copy(paste0('repo/mathematics/figures/',
						 list.files('repo/mathematics/figures', pattern='*.png')),
				  paste0('www/',
				  	   list.files('repo/mathematics/figures', pattern='*.png'))
		)

		items <- data.frame(Stem=character(), Domain=character(), DifficultyLevel=character(),
							A=character(), B=character(), c=character(), D=character(),
							Answer=character(), Filename=character(), stringsAsFactors = FALSE)
		for(i in list.files('repo/mathematics/items', pattern='.json')) {
			tmp <- fromJSON(paste0('repo/mathematics/items/', i))
			items <- rbind(items, data.frame(
				Stem = tmp$stem,
				Domain = tmp$domain,
				DifficultyLevel = tmp$difficulty,
				A = tmp$A, B = tmp$B, C = tmp$C, D = tmp$D,
				Answer = tmp$answer,
				Filename = tmp$feedback,
				stringsAsFactors = FALSE
			))
		}
		# items <- read.xls('repo/mathematics/MathItems.xlsx', stringsAsFactors=FALSE, verbose = FALSE)
		# items <- items[items$Domain != '',]

		domains <- unique(items$Domain)
		domains <- domains[domains != '']

		# items$Filename <- paste0(items$State, '-', items$Year, '-',
		# 						 formatC(items$Month, width=2, flag='0'), '-',
		# 						 formatC(items$ItemNum, width=2, flag='0'), '.md')

		tmp <- list()
		for(i in items$Filename) { # Read the feedback MD files
			if(file.exists(paste0(items.dir, i))) {
				tmp[[gsub('.md','',i)]] <- paste0(scan(paste0(items.dir, i),
											 what = character(),
											 sep = '\n',
											 blank.lines.skip = FALSE,
											 quiet = TRUE),
										collapse = '\n')
			}
		}

		for(i in names(tmp)) {
			feedback[[i]] <- tmp[[i]]
		}

		math.items$items <- items
		math.items$domains <- domains
	}

	updateRepo()

	observeEvent(input$refresh, {
		updateRepo()
	})

	output$domainsUI <- renderUI({
		selectInput('domain', 'Domain', math.items$domains)
	})

	observeEvent(input$nextItem, {
		choices <- math.items$items[math.items$items$Domain == input$domain,]
		if(input$itemsWithFeedback) {
			choices <- choices[choices$Filename %in% paste0(names(feedback), '.md'),]
		}
		choices <- gsub('.md','', choices$Filename)
		pos <- which(choices == input$item) + 1
		if(pos > length(choices)) { pos <- 1 }
		updateSelectInput(session, 'item', selected = choices[pos])
	})

	observeEvent(input$previousItem, {
		choices <- math.items$items[math.items$items$Domain == input$domain,]
		if(input$itemsWithFeedback) {
			choices <- choices[choices$Filename %in% names(feedback),]
		}
		choices <- gsub('.md','', choices$Filename)
		pos <- which(choices == input$item) - 1
		if(pos < 1) { pos <- length(choices) }
		updateSelectInput(session, 'item', selected = choices[pos])
	})

	output$itemSelect <- renderUI({
		req(input$domain)
		choices <- math.items$items[math.items$items$Domain == input$domain,]
		if(input$itemsWithFeedback) {
			choices <- choices[choices$Filename %in% paste0(names(feedback),'.md'),]
		}
		selectInput('item', NULL, choices = gsub('.md', '', choices$Filename))
	})

	output$stem <- renderText({
		html <- math.items$items[math.items$items$Filename == paste0(input$item, '.md'),]$Stem
		html <- paste(html, "<script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>")
		return(html)
	})

	output$choiceA <- renderText({
		html <- math.items$items[math.items$items$Filename == paste0(input$item, '.md'),]$A
		html <- paste(html, "<script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>")
		return(html)
	})

	output$choiceB <- renderText({
		html <- math.items$items[math.items$items$Filename == paste0(input$item, '.md'),]$B
		html <- paste(html, "<script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>")
		return(html)
	})

	output$choiceC <- renderText({
		html <- math.items$items[math.items$items$Filename == paste0(input$item, '.md'),]$C
		html <- paste(html, "<script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>")
		return(html)
	})

	output$choiceD <- renderText({
		html <- math.items$items[math.items$items$Filename == paste0(input$item, '.md'),]$D
		html <- paste(html, "<script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>")
		return(html)
	})

	output$answer <- renderText({
		paste0('Answer: ', math.items$items[math.items$items$Filename == paste0(input$item, '.md'),]$Answer)
	})

	output$feedbackUI <- renderUI({
		req(input$item)
		textAreaInput('feedback', 'Feedback source', value = feedback[[paste0(input$item, '.md')]],
					  width = '600px', height = '200px')
	})

	output$edititembuttonui <- renderUI({
		fname <- paste0(input$item, '.json')
		url <- paste0(repo.url, '/edit/master/mathematics/items/', fname)
		shiny::actionButton(inputId='editbutton', label="Edit Item",
							icon = icon("pencil"),
							onclick =paste0("window.open('", url, "', '_blank')"))
	})

	output$editfeedbackbuttonui <- renderUI({
		fname <- paste0(input$item, '.md')
		url <- paste0(repo.url, '/edit/master/mathematics/items/', fname)
		shiny::actionButton(inputId='editbutton', label="Edit Feedback",
							icon = icon("pencil"),
							onclick =paste0("window.open('", url, "', '_blank')"))
	})

	output$feedback <- renderText({
		req(input$item)
		# req(input$feedback)
		# html <- markdownToHTML(text = input$feedback, options=c('fragment_only'))
		if(input$item %in% names(feedback)) {
			html <- markdownToHTML(text = feedback[[input$item]], options=c('fragment_only'))
			#if(grep('$$', feedback[[131]], fixed=TRUE) | grep('\\(', feedback[[131]], fixed=TRUE)) {
				html <- paste(html, "<script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>")
			#}
		} else {
			html <- "No feedback found."
		}
		return(html)
	})
}

# Run the application
shinyApp(ui = ui, server = server)

