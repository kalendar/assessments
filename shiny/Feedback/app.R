library(shiny)
library(gdata)
library(markdown)
library(git2r)
library(tools)

repo.url <- 'https://github.com/DAACS/assessments'
edit.url <- paste0(repo.url, '/edit/master/')

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

srl.items <- read.xls('repo/srl/SRL-Items.xlsx', stringsAsFactors=FALSE, verbose = FALSE)
srl.items <- srl.items[srl.items$PrimaryFactor != '' & srl.items$PrimaryFactor != 'Grit',]

source('repo/R/parseMarkdown.R')

overview.files <- c('high','medium','low','high-summary','medium-summary','low-summary')
general.files <- c('help','landing','overview','start','startTips','items','passages','prompt')

toTitleCase <- function(text) {
	tools::toTitleCase(gsub('_', ' ', text))
}

ui <- fluidPage(
	titlePanel("DAACS Feedback Viewer"),

	fluidRow(
		column(width = 3, selectInput('subject', 'Subject', c('SRL'='srl','Mathematics'='mathematics','Reading'='reading','Writing'='writing'))),
		column(width = 3, uiOutput('domainUI')),
		column(width = 3, uiOutput('secondaryDomainUI')),
		column(width = 3, selectInput('level', 'Level', c('Developing'='low','Emerging'='medium','Mastering'='high')),
			   actionButton('refresh', 'Refresh Feedback', icon('refresh')))
	),
	hr(),
	fluidRow(column(width = 12,
	    	h4(htmlOutput('feedback'))
		)
	)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
	pull(repo)
	tmp <- parseMarkdown('repo')
	feedback <- reactiveValues()
	for(i in names(tmp)) {
		feedback[[i]] <- tmp[[i]]
	}

	observeEvent(input$refresh, {
		pull(repo)
		source('repo/R/parseMarkdown.R')
		tmp <- parseMarkdown('repo')
		for(i in names(tmp)) {
			feedback[[i]] <- tmp[[i]]
		}
	})

	output$domainUI <- renderUI({
		domains <- names(feedback[[input$subject]])[
			!names(feedback[[input$subject]]) %in% c(overview.files, general.files)]
		domains <- c('Overview', domains)
		names(domains) <- toTitleCase(domains)
		selectInput('domain', 'Domain', domains)
	})

	output$secondaryDomainUI <- renderUI({
		req(input$domain)
		ui <- NULL
		if(input$subject == 'srl' & input$domain != 'Overview') {
			domains <- c('Overview', names(feedback[['srl']][[input$domain]])[
				!names(feedback[['srl']][[input$domain]]) %in% c(overview.files, general.files)])
			names(domains) <- toTitleCase(domains)
			ui <- selectInput('secondaryDomain', 'Sub-Domain', domains)
		}
		return(ui)
	})

	output$levelUI <- renderUI({
		req(input$subject); req(input$domain)
		ui <- NULL
		if(input$subject != 'srl' & input$domain != 'Overview') {
			ui <- selectInput('level', 'Level', c('Developing','Emerging','Mastering'))
		} else if(input$subject == 'srl' & !is.null(input$secondaryDomain)) {
			if(input$secondaryDomain != 'Overview') {
				ui <- selectInput('level', 'Level', c('Developing'='low','Emerging'='medium','Mastering'='high'))
			}
		}
		return(ui)
	})

	output$editbuttonui <- renderUI({
		fname <- input$item
		url <- paste0(repo.url, '/edit/master/mathematics/items/', fname)
		shiny::actionButton(inputId='editbutton', label="Edit Feedback",
							icon = icon("pencil"),
							onclick =paste0("window.open('", url, "', '_blank')"))
	})

	output$feedback <- renderUI({
		input$refresh
		req(input$subject); req(input$domain)
		ui <- NULL
		if(input$domain == 'Overview') {
			parts <- list()
			parts[[length(parts) + 1]] <- HTML(feedback[[input$subject]][[paste0(input$level)]])
			parts[[length(parts) + 1]] <- shiny::actionButton(inputId = paste0('ID', round(runif(1) * 100000)),
							label = 'Edit This', icon = icon('pencil'),
							onclick = paste0("window.open('", edit.url, input$subject, "/",
							  				 input$level, ".md', '_blank')"))
			subdomains <- names(feedback[[input$subject]])[
				!names(feedback[[input$subject]]) %in% c(overview.files, general.files)]
			for(i in subdomains) {
				parts[[length(parts) + 1]] <- hr()
				parts[[length(parts) + 1]] <- h2(toTitleCase(i))
				parts[[length(parts) + 1]] <- HTML(feedback[[input$subject]][[i]][[paste0(input$level, '-summary')]])
				parts[[length(parts) + 1]] <- shiny::actionButton(inputId = paste0('ID', round(runif(1) * 100000)),
							label = 'Edit This', icon = icon('pencil'),
							onclick = paste0("window.open('", edit.url, input$subject, "/", i, "/",
											 input$level, "-summary.md', '_blank')"))
			}
			ui <- do.call(fluidPage, parts)
		} else if(input$subject != 'srl') {
			parts <- list()
			parts[[length(parts) + 1]] <- HTML(feedback[[input$subject]][[input$domain]][[input$level]])
			parts[[length(parts) + 1]] <- shiny::actionButton(inputId = paste0('ID', round(runif(1) * 100000)),
							label = 'Edit This', icon = icon('pencil'),
							onclick = paste0("window.open('", edit.url, input$subject,
							  				 "/", input$domain, "/",
							  				 input$level, ".md', '_blank')"))
			parts[[length(parts) + 1]] <- hr()
			if(input$subject == 'mathematics') {
				parts[[length(parts) + 1]] <- p("Mathematic items shown here. View all reading items here:")
				parts[[length(parts) + 1]] <- a('shiny.daacs.net/shiny/MathItems', href='http://shiny.daacs.net/shiny/MathItems/',
												target='_blank')
			} else if(input$subject == 'reading') {
				parts[[length(parts) + 1]] <- p('Reading items shown here. View all mathematics items here:')
				parts[[length(parts) + 1]] <- a('shiny.daacs.net/shiny/ReadingItems', href='http://shiny.daacs.net/shiny/ReadingItems/',
												target='_blank')
			} else if(input$subject == 'writing') {
				parts[[length(parts) + 1]] <- p("Student's essay shown here...")
			}
			parts[[length(parts) + 1]] <- hr()
			parts[[length(parts) + 1]] <- HTML(feedback[[input$subject]][[input$domain]][['overview']])
			parts[[length(parts) + 1]] <- shiny::actionButton(inputId = paste0('ID', round(runif(1) * 100000)),
							label = 'Edit This', icon = icon('pencil'),
							onclick = paste0("window.open('", edit.url, input$subject,
							 				 "/", input$domain, "/",
							  				 "overview.md', '_blank')"))
			ui <- do.call(fluidPage, parts)
		} else if(input$subject == 'srl') {
			req(input$secondaryDomain)
			if(input$secondaryDomain == 'Overview') {
				parts <- list()
				parts[[length(parts) + 1]] <- HTML(feedback[[input$subject]][[input$domain]][[paste0(input$level)]])
				parts[[length(parts) + 1]] <- shiny::actionButton(inputId = paste0('ID', round(runif(1) * 100000)),
							label = 'Edit This', icon = icon('pencil'),
							onclick = paste0("window.open('", edit.url, input$subject, "/", input$domain, "/",
							 				 input$level, ".md', '_blank')"))
				subdomains <- names(feedback[[input$subject]][[input$domain]])[
					!names(feedback[[input$subject]][[input$domain]]) %in% c(overview.files, general.files)]
				for(i in subdomains) {
					parts[[length(parts) + 1]] <- hr()
					parts[[length(parts) + 1]] <- h2(toTitleCase(i))
					parts[[length(parts) + 1]] <- HTML(feedback[[input$subject]][[input$domain]][[i]][[paste0(input$level, '-summary')]])
					parts[[length(parts) + 1]] <- shiny::actionButton(inputId = paste0('ID', round(runif(1) * 100000)),
							label = 'Edit This', icon = icon('pencil'),
							onclick = paste0("window.open('", edit.url, input$subject, "/", input$domain, '/', i, "/",
							 				 input$level, "-summary.md', '_blank')"))
				}
				ui <- do.call(fluidPage, parts)
			} else {
				parts <- list()
				parts[[length(parts) + 1]] <- HTML(feedback[[input$subject]][[input$domain]][[input$secondaryDomain]][[input$level]])
				parts[[length(parts) + 1]] <- shiny::actionButton(inputId = paste0('ID', round(runif(1) * 100000)),
							label = 'Edit This', icon = icon('pencil'),
							onclick = paste0("window.open('", edit.url, input$subject,
											 "/", input$domain, "/", input$secondaryDomain, "/",
											 input$level, ".md', '_blank')"))
				parts[[length(parts) + 1]] <- hr()
				parts[[length(parts) + 1]] <- p('SRL item results here...')
				parts[[length(parts) + 1]] <- HTML(
					paste0("<ul>", paste0(paste0("<li>",
							trim(substr(srl.items[srl.items$SecondaryFactor == toTitleCase(input$secondaryDomain),]$Stem, 4, 1000)),
							"</li>"), collapse = ''), "</ul>") )
				parts[[length(parts) + 1]] <- hr()
				parts[[length(parts) + 1]] <- HTML(feedback[[input$subject]][[input$domain]][[input$secondaryDomain]][['overview']])
				parts[[length(parts) + 1]] <- shiny::actionButton(inputId = paste0('ID', round(runif(1) * 100000)),
							label = 'Edit This', icon = icon('pencil'),
							onclick = paste0("window.open('", edit.url, input$subject,
							  				 "/", input$domain, "/", input$secondaryDomain, "/",
							  				 "overview.md', '_blank')"))
				ui <- do.call(fluidPage, parts)
			}
		}
		return(ui)
	})
}

# Run the application
shinyApp(ui = ui, server = server)

