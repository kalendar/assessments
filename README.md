### DAACS Assessments

This directory contains the source files for building the assessments for DAACS. The build-XXX.R files are R scripts used to convert the assessment questions (in Excel format) and feedback (in Markdown format) to JSON which can then be imported into the DAACS software. To build the JSON document, with the R's working directory set to the location of the R scripts, run the following commands:

```
source('build-srl.R')
source('build-reading.R')
source('build-mathematics.R')
source('build-writing.R')
```

Or use the commands in the `build.R` file.

Items for multiple choice assessments (e.g. mathematics and reading) and Likert assessments (e.g. SRL) are provided in Excel files. See each corresponding `build-XXX.R` file for details on the required structure of those files. 

Feedback is sourced from Markdown files. Within each assessment directory, the following files are required:

* `landing.md` - Contains the information shown on the dashboard page when the student hovers over the assessment icon.
* `start.md` - Directions shown when a student starts an assessment.
* `startTips.md` - Quick tips shown on the directions page. This should be a bulleted list.
* `help.md` - Contents of the help page shown when the student clicks the Help button while completing an assessment.

For each domain, the following files must be present:

* `high-summary.md` - Feedback to show on the domain's parent page for a student who scored at the highest level (e.g. shown on the metacogition page for the planning domain; shown on the SRL page for metacognition).
* `medium-summary.md` - Feedback to show on the domain's parent page for a student who scored at the medium level.
* `medium-summary.md` - Feedback to show on the domain's parent page for a student who scored a the lowest level.
* `high.md` - Feedback to show students who scored at the highest level.
* `medium.md` - Feedback to show students who scored at the medium level.
* `low.md` - Feedback to show students who scored at the lowest level.
* `overview.md` - General information shown below the items. This is shown to all students regardless of their score level.



________________________________________________________________________________

*The contents of this website were developed under grant P116F150077 from the U.S. Department of Education. However, those contents do not necessarily represent the policy of the U.S. Department of Education, and you should not assume endorsement by the Federal Government.*

All non-software work is licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/).


