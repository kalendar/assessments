buildDomainFeedback <- function(feedback, domain, domain.items, includeCompletionScoreMap = TRUE) {
	completionScoreMap <- list()
	if(includeCompletionScoreMap) {
		if(missing(domain.items)) {
			completionScoreMap <- list(
				LOW = '[0.0,0.334)',
				MEDIUM = '[0.334,0.667)',
				HIGH = '[0.667,1.0]'
			)
		} else {
			completionScoreMap <- list(
				LOW = paste0('[0.0,', floor((nrow(domain.items) * 4) / 3), '.0)'),
				MEDIUM = paste0('[', floor((nrow(domain.items) * 4) / 3), '.0,',
								floor((nrow(domain.items) * 4) / 3*2), '.0)'),
				HIGH = paste0('[', floor((nrow(domain.items) * 4) / 3 * 2), '.0,',
							  ceiling((nrow(domain.items) * 4)), '.0]')
			)
		}
	} else {
		completionScoreMap = NULL
	}

	return(list(
		id = domain,
		label = toTitleCase(gsub('_', ' ', domain)),
		content = feedback$overview,
		rubric = list(
			completionScoreMap = completionScoreMap,
			supplementTable = list(
				list(
					completionScore = 'LOW',
					content = feedback$low,
					contentSummary = feedback$`low-summary`
				),
				list(
					completionScore = 'MEDIUM',
					content = feedback$medium,
					contentSummary = feedback$`medium-summary`
				),
				list(
					completionScore = 'HIGH',
					content = feedback$high,
					contentSummary = feedback$`high-summary`
				)
			)
		)
	))
}
