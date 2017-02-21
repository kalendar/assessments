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


feedback <- parseMarkdown('writing')
domains <- c('connecting_ideas', 'content', 'conventions', 'organization', 'paragraphs', 'sentences')

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

json$domains <- list()
for(d in domains) {
	json$domains[[(length(json$domains) + 1)]] <- buildDomainFeedback(feedback[[d]], d,
																	  items[items$SecondaryFactor == d,],
																	  includeCompletionScoreMap = FALSE)
}


json.out <- jsonlite::toJSON(json, pretty = TRUE, auto_unbox = TRUE)
fname <- paste0('build/archive/Writing-', format(Sys.time(), format='%Y-%m-%d-%H-%M'), '.json')
cat(json.out, file = 'build/Writing.json')
cat(json.out, file = fname)
#file.edit(fname)

