####################################################################################################
##### NOTE: It is best to source this file, DO NOT run interactively ###############################
####################################################################################################
if(!file.exists('build-srl.R')) {
	stop('Working directory not set correctly. Set the working directory to the location of 
		 of this R script.')
}

rm(list=ls(all=TRUE)) # Clean the environment before starting

library(gdata)
library(markdown)
library(tools)
library(rjson)
library(jsonlite)
source('parseMarkdown.R')
source('buildDomainFeedback.R')

# Structure of SRL Feedback
# 	Motivation
# 		* Mindset
# 		* Self-Efficacy
# 		* Anxiety
# 		* Mastery Orientation
# 	Metacognition
# 		* Planning
# 		* Monitoring
# 		* Evaluation
# 	Strategies
# 		* Strategies for Managing Environment
#       * Strategies for Managing Time
#       * Strategies for Help Seeking
# 		* Strategies for Understanding


exclude.domains <- c('grit')

items <- read.xls('srl/SRL-Items.xlsx', stringsAsFactors = FALSE)
items <- items[items$PrimaryFactor != '',]
items$Stem <- trim(substr(items$Stem, 4, 1000))
items$PrimaryFactor <- tolower(gsub(' ', '_', items$PrimaryFactor))
items$SecondaryFactor <- tolower(gsub(' ', '_', items$SecondaryFactor))

agreement.levels <- c('Strongly Disagree', 'Disagree', 'Neither Agree nor Disagree',
					  'Agree', 'Strongly Agree')
frequency.levels <- c('Almost Never', 'Not Very Often', 'Somewhat Often',
					  'Pretty Often', 'Almost Always')
grit.levels <- c('Not at All Like Me', 'Not Much Like Me', 'Somewhat Like Me',
				 'Mostly Like Me', 'Very Much Like Me')

##### Convert Markdown files to HTML fragments


feedback <- parseMarkdown('srl')


##### Build the JSON document

items.excluded <- items[(items$SecondaryFactor %in% exclude.domains),]
items <- items[!(items$SecondaryFactor %in% exclude.domains),]

json <- list(
	assessmentCategory = "COLLEGE_SKILLS",
	assessmentType = "LIKERT",
	prerequisites = list(),
	enabled = TRUE,
	label = "Self-Regulated Learning",
	scoringType = 'SUM'
)

json$content <- list(
	landing = feedback$landing,
	start = feedback$start,
	startTips = feedback$startTips,
	helpLabel = 'Help',
	help = feedback$help
)

json$overallRubric <- list(
	completionScoreMap = list(
		LOW = paste0('[0.0,', floor((nrow(items) * 4) / 3), '.0)'),
		MEDIUM = paste0('[', floor((nrow(items) * 4) / 3), '.0,',
						floor((nrow(items) * 4) / 3*2), '.0)'),
		HIGH = paste0('[', floor((nrow(items) * 4) / 3 * 2), '.0,',
					  ceiling(( nrow(items) * 4 + nrow(items.excluded) * 4 )), '.0]')
	),
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


##### Domain level feedback

domains1 <- unique(items$PrimaryFactor)
for(d1 in domains1) {
	items.domain <- items[items$PrimaryFactor == d1,]
	fb <- buildDomainFeedback(feedback[[d1]], d1, items.domain)
	domains2 <- unique(items.domain$SecondaryFactor)
	fb$subDomains <- list()
	for(d2 in domains2) {
		items.domain2 <- items.domain[items.domain$SecondaryFactor == d2,]
		pos <- length(fb$subDomains) + 1
		fb$subDomains[[pos]] <- buildDomainFeedback(feedback[[d1]][[d2]], d2,
													items.domain2)
	}
	json$domains[[((length(json$domains) + 1))]] <- fb
}

feedback.grit <- list('overview' = '',
					  'low' = '', 'low-summary' = '',
					  'medium' = '', 'medium-summary' = '',
					  'high' = '', 'high-summary' = '')
json$domains[[((length(json$domains) + 1))]] <- buildDomainFeedback(feedback.grit,
					'grit', items.excluded[items.excluded$PrimaryFactor == 'grit',],
					domainType = 'ANALYSIS', includeCompletionScoreMap = TRUE)

##### Items

items.agreement <- items[items$AnchorType == 'Agreement',]
items.frequency <- items[items$AnchorType == 'Frequency',]
items.grit <- items.excluded[items.excluded$AnchorType == 'Grit',]

buildAnswers <- function(responses) {
	ans <- list()
	for(i in seq_along(responses)) {
		ans[[i]] <- list(content = responses[i],
						 score = as.integer(i-1))
	}
	return(ans)
}

buildItemGroups <- function(theitems, nGroups, levels,
							responses = buildAnswers(levels),
							responses.reversed = rev(buildAnswers(rev(levels)))) {
	itemGroups <- list()
	for(i in 1:nGroups) {
		json.items <- list()
		for(j in seq(i, nrow(theitems), by=nGroups)) {
			pos <- length(json.items)+1
			json.items[[pos]] <- list(
				#itemContent = 'null',
				question = theitems[j,]$Stem,
				domainId = theitems[j,]$SecondaryFactor, # TODO: This should probably be SecondaryFactor
				itemContent = NULL
			)
			if(theitems[j,]$ReverseCoded == 'Yes') {
				json.items[[pos]]$possibleItemAnswers <- responses.reversed
			} else{
				json.items[[pos]]$possibleItemAnswers <- responses
			}
		}
		itemGroups[[i]] <- json.items
	}
	names(itemGroups) <- rep('items', nGroups)
	return(itemGroups)
}

# Want to have around 10 items per page.
table(items$AnchorType)
# Agreement: 3 groups
# Frequency: 5 groups
# Grit: 1 group
# itemGroups <- list()

# items.agreement <- items.agreement[items.agreement$SecondaryFactor %in% domains,]
# itemGroups[1:2] <- buildItemGroups(items.agreement, 2, agreement.levels)

itemGroups <- list()
itemGroups[1:2] <- buildItemGroups(items.agreement, 2, agreement.levels)
itemGroups[3:6] <- buildItemGroups(items.frequency, 4, frequency.levels)
itemGroups[7:8] <- buildItemGroups(items.grit, 2, grit.levels)

json$itemGroups <- list()
for(i in seq_len(length(itemGroups))) {
	json$itemGroups[[i]] <- list('items' = itemGroups[[i]])
}

##### Save JSON to file

json.out <- jsonlite::toJSON(json, pretty = TRUE, auto_unbox = TRUE)
cat(json.out, file = paste0('build/SRL.json'))
cat(json.out, file = paste0('build/archive/SRL-', format(Sys.time(), format='%Y-%m-%d-%H-%M'), '.json'))
file.edit('build/SRL.json')
