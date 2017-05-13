####################################################################################################
##### NOTE: It is best to source this file, DO NOT run interactively ###############################
####################################################################################################

if(!file.exists('build-mathematics.R')) {
	stop('Working directory not set correctly. Set the working directory to the location of 
		 of this R script.')
}

rm(list=ls(all=TRUE)) # Clean the environment before starting

# Word Documents can be converted to Markdown using [Pandoc](http://pandoc.org/). To install
# pandoc using [Homebrew](http://brew.sh/), use the following command in the Terminal:
#
#     brew install pandoc
#
# Run the following command where INPUT and OUTPUT are the file names.
#
#     pandoc -s INPUT.docx -t markdown -o OUTPUT.md
#
# Clean up the document as necessary. The following to applications are useful for cleaning files:
# * Atom Editor: https://atom.io/
# * Markdown Preview Plus: (can be installed from inside Atom)

library(gdata)
library(markdown)
library(tools)
library(rjson)
library(jsonlite)
source('parseMarkdown.R')
source('buildDomainFeedback.R')

figures.base.url <- 'https://raw.githubusercontent.com/DAACS/DAACS-Website/master/assessments/mathematics/figures/'
items.dir <- 'mathematics/items/'

# items <- read.xls('mathematics/MathItems.xlsx', sheet=1, stringsAsFactors=FALSE)
# 
# items[items$DifficultyLevel == '', ]$DifficultyLevel <- NA
# items[items$Domain == '', ]$Domain <- NA
# items$DifficultyLevel <- toupper(items$DifficultyLevel)
# items$Domain <- tolower(gsub(' ', '_', items$Domain))
# items$Filename <- paste0(items$State, '-', items$Year, '-',
# 						 formatC(items$Month, width=2, flag='0'), '-',
# 						 formatC(items$ItemNum, width=2, flag='0'), '.md')

# Read items from JSON files
items <- data.frame(Stem=character(), Domain=character(), DifficultyLevel=character(),
					A=character(), B=character(), c=character(), D=character(),
					Answer=character(), Filename=character(), stringsAsFactors = FALSE)
for(i in list.files('mathematics/items', pattern='.json')) {
	tmp <- jsonlite::fromJSON(paste0('mathematics/items/', i))
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

feedback <- parseMarkdown('mathematics')

feedback.items <- list()
for(i in items$Filename) { # Read the feedback MD files
	if(file.exists(paste0(items.dir, i))) {
		feedback.items[[i]] <- markdownToHTML(
			paste0(items.dir, i), fragment.only = TRUE
		)
	}
}

# Drop items without an assigned domain or difficulty
items <- items[!is.na(items$Domain) & !is.na(items$DifficultyLevel),]
table(items$Domain, items$DifficultyLevel, useNA='ifany')
nrow(items)

# TODO: Fix items without feedback
table(items$Filename %in% names(feedback.items))
#View(items[!items$Filename %in% names(feedback.items),])
# write.csv(items[!items$Filename %in% names(feedback.items), c(1:8,13:18)], 
# 		  file = 'ItemsNoFeedback.csv', row.names = FALSE)

items <- items[items$Filename %in% names(feedback.items),] # Items with missing feedback

table(items$Domain, items$DifficultyLevel, useNA='ifany')


assignGroups <- function(items, difficulty) {
	nGroups <- min(table(items[items$DifficultyLevel == difficulty,]$Domain))
	for(i in seq_len(nGroups)) {
		rows <- items$DifficultyLevel == difficulty & is.na(items$Group)
		items[rows,][!duplicated(items[rows,]$Domain),]$Group <- paste0(difficulty, i)
	}
	return(items)
}

items$Group <- NA
items <- assignGroups(items, 'EASY')
items <- assignGroups(items, 'MEDIUM')
items <- assignGroups(items, 'HARD')

table(items$Group, useNA='ifany')
items <- items[!is.na(items$Group),]

# Fix img tags to point to Github
row.names(items) <- 1:nrow(items)
Sys.setlocale('LC_ALL','C') # http://r.789695.n4.nabble.com/Strings-from-different-locale-td3023176.html
fixImg <- function(col) {
	tmp <- grep("<img src='", col, fixed = TRUE)
	col[tmp] <- gsub("<img src='", paste0("<img src='", figures.base.url),
				col[tmp], fixed = TRUE)
	return(col)
}
items$Stem <- fixImg(items$Stem)
items$A <- fixImg(items$A)
items$B <- fixImg(items$B)
items$C <- fixImg(items$C)
items$D <- fixImg(items$D)


##### Build the JSON document

json <- list(
	assessmentCategory = "MATHEMATICS",
	assessmentType = "CAT",
	prerequisites = list(),
	enabled = TRUE,
	label = "Mathematics",
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
	helpLabel = 'Formulas',
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
		LOW = '[0.0,0.334)',
		MEDIUM = '[0.334,0.667)',
		HIGH = '[0.667,1.0]'
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

table(items$Domain, items$DifficultyLevel, useNA='ifany')

domains <- tolower(unique(items$Domain))
json$domains <- list()
for(d in domains) {
	json$domains[[(length(json$domains) + 1)]] <- buildDomainFeedback(feedback[[d]], d)
}

itemGroups <- unique(items$Group)
json$itemGroups <- list()
for(i in itemGroups) {
	items.group <- items[items$Group == i,]
	pos <- length(json$itemGroups) + 1
	json$itemGroups[[pos]] <- list(
		difficulty = items.group[1,]$DifficultyLevel,
		items = list()
	)
	for(j in 1:nrow(items.group)) {
		json$itemGroups[[pos]]$items[[j]] <- list(
			domainId = items.group[j,]$Domain,
			itemContent = list(
				question = list(
					content = items.group[j,]$Stem,
					itemContentType = "FORMULA"
					# content = '',
					# itemContentType = "WORD"
				),
				feedback = list(
					content = feedback.items[[items.group[j,]$Filename]],
					itemContentType = "FORMULA"
				)
			),
			# question = items.group[j,]$Stem,
			question = '',
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
cat(json.out, file = 'build/Mathematics.json')
cat(json.out, file = paste0('build/archive/Mathematics-', format(Sys.time(), format='%Y-%m-%d-%H-%M'), '.json'))
file.edit('build/Mathematics.json')

