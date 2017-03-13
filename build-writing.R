####################################################################################################
##### NOTE: It is best to source this file, DO NOT run interactively ###############################
####################################################################################################

if(!file.exists('build-writing.R')) {
	stop('Working directory not set correctly. Set the working directory to the location of 
		 of this R script.')
}

rm(list=ls(all=TRUE)) # Clean the environment before starting

library(readxl)
library(markdown)
library(tools)
library(rjson)
library(jsonlite)
source('parseMarkdown.R')
source('buildDomainFeedback.R')

rubric <- read_excel('writing/DAACS-Writing-Rubric.xlsx')
rubric <- as.data.frame(rubric)

feedback <- parseMarkdown('writing')
# domains <- c('connecting_ideas', 'content', 'conventions', 'organization', 'paragraphs', 'sentences')

domains <- list(connecting_ideas = c('ideas', 'links'),
				content = c('explanation','suggestions','summary'), 
				conventions = c(),
				organization = c('structure','transitions'), 
				paragraphs = c(), 
				sentences = c('complexity','correct','structure'))

rubricToHTML <- function(rubric, highlight=0) {
	html <- '<style type="text/css">
		table.tableizer-table {
			font-size: 12px;
			border: 1px solid #CCC; 
			font-family: Arial, Helvetica, sans-serif;
		} 
		.tableizer-table td {
			padding: 4px;
			margin: 3px;
			border: 1px solid #CCC;
		}
		.tableizer-table th {
			background-color: #104E8B; 
			color: #FFF;
			font-weight: bold;
			text-align: center;
		}
		</style>'
	html <- paste0(html, "\n", "<table class='tableizer-table'>")
	if(highlight > 0) {
		colgroup <- paste0("<colgroup>",
			"<col width='10%' style='background-color:white;'>",
			paste0(rep("<col width='30%' style='background-color:white;'>", 
					   (4 - highlight) - 1), collapse=''),
			"<col width='30%' style='background-color:lightyellow;'>",
			paste0(rep("<col width='30%' style='background-color:white;'>", 
					   3 - (4 - highlight)), collapse=''),
			"</colgroup>")
		html <- paste0(html, colgroup)
	}
	html <- paste0(html, "<thead><tr class='tableizer-firstrow'><th>&nbsp;</th>",
				   "<th>Mastering</th><th>Emerging</th><th>Developing</th></tr></thead><tbody>")
	
	for(i in 1:nrow(rubric)) {
		row <- paste0("<tr><td>", toTitleCase(rubric[i,]$SubCriteria),
					  "</td><td>", rubric[i,]$Mastering,
					  "</td><td>", rubric[i,]$Emerging,
					  "</td><td>",  rubric[i,]$Developing, "</td></tr>")
		html <- paste0(html, row)
	}
	
	html <- paste0(html, "</tbody></table>")
	return(html)
}

##### Build JSON Document

json <- list(
	assessmentCategory = "WRITING",
	assessmentType = "WRITING_PROMPT",
	enabled = TRUE,
	label = "Writing",
	scoringType = 'MANUAL',
	writingPrompt = list(
		content = feedback$prompt,
		minWords = 350
	),
	prerequisites = list(list(
		prereqType = "ASSESSMENT",
		reason = "You must complete the Self-Regulated Learning assessment first.",
		assessmentCategory = "COLLEGE_SKILLS",
		statuses = list("COMPLETED", "GRADED")
	))
)

json$content <- list(
	landing = feedback$landing,
	start = feedback$start,
	startTips = feedback$startTips,
	helpLabel = 'Rubric',
	help = feedback$help
)

json$overallRubric <- list(
	# completionScoreMap = list(
	# 	LOW = '[0.0,0.334)',
	# 	MEDIUM = '[0.334,0.667)',
	# 	HIGH = '[0.667,1.0]'
	# ),
	completionScoreMap = NULL,
	supplementTable = list(
		list(
			completionScore = 'LOW',
			content = feedback$`low`,
			contentSummary = feedback$`low-summary`
		),
		list(
			completionScore = 'MEDIUM',
			content = feedback$`medium`,
			contentSummary = feedback$`medium-summary`
		),
		list(
			completionScore = 'HIGH',
			content = feedback$`high`,
			contentSummary = feedback$`high-summary`
		)
	)
)

# Add rubric to feedback
for(i in names(domains)) {
	feedback[[i]][['high']] <- paste0(
		feedback[[i]][['high']], '\n',
		rubricToHTML(rubric[rubric$Criteria == i,], highlight = 3), 
		'\n\n <br /><h2>Your Essay</h2>'
	)
	feedback[[i]][['medium']] <- paste0(
		feedback[[i]][['medium']], '\n',
		rubricToHTML(rubric[rubric$Criteria == i,], highlight = 2), 
		'\n\n <br /><h2>Your Essay</h2>'
	)
	feedback[[i]][['low']] <- paste0(
		feedback[[i]][['low']], '\n',
		rubricToHTML(rubric[rubric$Criteria == i,], highlight = 1), 
		'\n\n <br /><h2>Your Essay</h2>'
	)
	
	feedback[[i]][['high-summary']] <- paste0(
		feedback[[i]][['high-summary']], '\n',
		rubricToHTML(rubric[rubric$Criteria == i,], highlight = 3)
	)
	feedback[[i]][['medium-summary']] <- paste0(
		feedback[[i]][['medium-summary']], '\n',
		rubricToHTML(rubric[rubric$Criteria == i,], highlight = 2)
	)
	feedback[[i]][['low-summary']] <- paste0(
		feedback[[i]][['low-summary']], '\n',
		rubricToHTML(rubric[rubric$Criteria == i,], highlight = 1)
	)
}

json$domains <- list()
for(i in seq_along(domains)) {
	d1 <- names(domains)[i]
	rubric.domain <- rubric[rubric$Criteria == d1,]
	fb <- buildDomainFeedback(feedback[[d1]], d1, includeCompletionScoreMap = FALSE)
	domains2 <- domains[[d1]]
	if(length(domains2) > 0) {
		fb$subDomains <- list()
		for(d2 in domains2) {
			pos <- length(fb$subDomains) + 1
			fb$subDomains[[pos]] <- buildDomainFeedback(feedback[[d1]][[d2]], d2, 
														includeCompletionScoreMap = FALSE)
		}
	}
	json$domains[[((length(json$domains) + 1))]] <- fb
}

##### Save JSON to file

json.out <- jsonlite::toJSON(json, pretty = TRUE, auto_unbox = TRUE)
cat(json.out, file = paste0('build/Writing.json'))
cat(json.out, file = paste0('build/archive/Writing-', format(Sys.time(), format='%Y-%m-%d-%H-%M'), '.json'))
file.edit('build/Writing.json')
