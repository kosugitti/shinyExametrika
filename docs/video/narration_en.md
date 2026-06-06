# narration_en — English narration (one block per scene)

Parsed by build_video.py. Each `## N` header starts a scene; the lines below
are the narration text for that scene. Blank lines and this header block are
ignored.

## 0
shinyExametrika. Test data analysis in your browser, with no coding required.

## 1
This is shinyExametrika, a web app that brings the exametrika package to your browser.
The Guide tab you land on lays out the workflow in four steps: load your data, format it, run an analysis, and view the results.
Let's walk through them.

## 2
First, the Data tab. You can upload your own CSV file, but here we'll use a built-in sample: J15S500, a binary dataset of fifteen items and five hundred examinees.
The preview of the loaded data appears on the right.

## 3
Next we format the data. We tell the app which column is the ID and which items to analyze, then click Format Data.
The header now shows the active dataset and its shape, and only the analysis tabs that fit this data type light up.
Since this is binary data, the binary-compatible analyses become available.

## 4
Let's start with descriptive statistics. The Descriptives tab shows each item's pass rate and the distribution of total scores. A quick feel for the data.

## 5
For a fuller analysis, let's look at item response theory. In the IRT tab we choose a model.
Hovering the question mark next to each parameter shows an explanation.
Running it gives the model fit, each item's discrimination and difficulty, and the item characteristic curves.
Curves further to the right mark harder items. You can download any plot or table with the buttons provided.

## 6
shinyExametrika offers many more models, from latent class analysis to biclustering.
Biclustering, for instance, groups examinees and items at the same time, and visualizes the reordered response pattern as a heatmap.

## 7
The toggle in the top right switches between English and Japanese at any time.
No code. Just upload your data and click. Give shinyExametrika a try for your own test data analysis.

## 8
shinyExametrika. kosugitti dot shinyapps dot io, slash shinyExametrika.
