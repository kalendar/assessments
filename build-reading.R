####################################################################################################
##### NOTE: It is best to source this file, DO NOT run interactively ###############################
####################################################################################################

if(!file.exists('build-reading.R')) {
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

domains <- c(ID = 'Ideas',
			 IN = 'Inference',
			 LA = 'Language',
			 PU = 'Purpose',
			 ST = 'Structure')

difficulties <- read.csv('reading/ReadingDifficulty.csv', stringsAsFactors = FALSE)

items <- read_excel('reading/ReadingItems.xlsx', sheet = 1)
items <- as.data.frame(items)

items$Domain <- as.character(factor(items$DomainID,
									levels = names(domains),
									labels = unname(domains)))
table(items$Domain, useNA='ifany')

items <- merge(items, difficulties, by.x = 'PASSAGE', by.y = 'Passage', all.x = TRUE)
table(items$Difficulty)

feedback <- parseMarkdown('reading')

passages <- character()
files <- list.files('reading/passages/', pattern = "*.txt")
for(i in files) {
	tmp <- scan(paste0('reading/passages/', i),
				sep = '\n', what = character(), blank.lines.skip = FALSE, quiet = TRUE)
	passages <- c(passages, paste0(tmp, collapse = '\n'))
}
names(passages) <- files

table(items$PASSAGE %in% names(passages)) # Make sure all items have a passage


##### Build JSON Document

json <- list(
	assessmentCategory = "READING",
	assessmentType = "CAT",
	prerequisites = list(),
	enabled = TRUE,
	label = "Reading",
	scoringType = 'AVERAGE',
	startingDifficulty = 'MEDIUM',
	maxTakenGroups = 4,
	minTakenGroups = 3,
	numQuestionsPerGroup = 6
)

json$content <- list(
	landing = feedback$landing,
	start = feedback$start,
	startTips = feedback$startTips,
	helpLabel = 'Help',
	help = feedback$help
)

json$itemGroupTransitions <- list(
	list(
		groupDifficulty = "EASY",
		transitionMap = list(
			EASY = "(-INF,2]",
			MEDIUM = "[3,INF)"
		)
	),
	list(
		groupDifficulty = "MEDIUM",
		transitionMap = list(
			EASY = "(-INF,2]",
			MEDIUM = "[3,4]",
			HARD = "[5,INF)"
		)
	),
	list(
		groupDifficulty = "HARD",
		transitionMap = list(
			MEDIUM = "(-INF,4]",
			HARD = "[5,INF)"
		)
	)
)

json$overallRubric <- list(
	completionScoreMap = list(
		LOW = '[0.0,0.7)',
		MEDIUM = '[0.7,0.9)',
		HIGH = '[0.9,1.0]'
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

domains <- tolower(unique(items$Domain))
json$domains <- list()
for(d in domains) {
	json$domains[[(length(json$domains) + 1)]] <- buildDomainFeedback(feedback[[d]], d)
}

# Items

#describeBy(items$Mean, items$PASSAGE, mat=TRUE)[,c('group1','mean','sd','median')]

table(items$PASSAGE) # Make sure each passage has the same number of items
itemGroups <- unique(items$PASSAGE)
names(itemGroups) <- paste0('Group', 1:length(itemGroups))
items$Group <- as.character(factor(items$PASSAGE,
								   levels = unname(itemGroups),
								   labels = names(itemGroups)))


json$itemGroups <- list()
for(i in itemGroups) {
	items.group <- items[items$PASSAGE == i,]
	pos <- length(json$itemGroups) + 1
	json$itemGroups[[pos]] <- list(
		#difficulty = sample(c('EASY','MEDIUM','HARD'), 1), # TODO: Use assigned difficulties, not random!
		difficulty = as.character(items.group[1,]$Difficulty),
		items = list()
	)
	for(j in 1:nrow(items.group)) {
		json$itemGroups[[pos]]$items[[j]] <- list(
			domainId = tolower(items.group[j,]$Domain),
			itemContent = list(
				question = list(
					content = passages[items.group[j,]$PASSAGE],
					itemContentType = "PASSAGE"
				),
				feedback = list(
					#content = paste0('The correct answer is ', items.group[j,]$Answer),
					content = items.group[j,]$Feedback,
					itemContentType = "WORD"
				)
			),
			question = items.group[j,]$Question,
			possibleItemAnswers = list(
				list(
					content = items.group[j,]$A,
					score = as.integer(items.group[j,]$Answer == 'A')
				),
				list(
					content = items.group[j,]$B,
					score = as.integer(items.group[j,]$Answer == 'B')
				),
				list(
					content = items.group[j,]$C,
					score = as.integer(items.group[j,]$Answer == 'C')
				),
				list(
					content = items.group[j,]$D,
					score = as.integer(items.group[j,]$Answer == 'D')
				)
			)
		)
	}
}

json.out <- jsonlite::toJSON(json, pretty = TRUE, auto_unbox = TRUE)
cat(json.out, file = paste0('build/Reading.json'))
cat(json.out, file = paste0('build/archive/Reading-', 
							format(Sys.time(), format='%Y-%m-%d-%H-%M'), '.json'))
file.edit(paste0('build/Reading.json'))

