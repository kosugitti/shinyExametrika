# narration_en — English narration (one block per scene)

build_video.py parses this. `## N` starts a scene. 0 = title card,
9 = end-card narration (now overlaid on the body tail). Same recording as the
Japanese version (the UI is in English). Keep each scene within the same window
as the JA anchors so nothing overlaps.

## 0
shinyExametrika. Test data analysis in your browser — no coding required!

## 1
This is shinyExametrika, a web app that lets you use the features of the exametrika package right in your browser.
The page you see first sums up how to use it in four steps: load your data, set its type, run an analysis, and view the results.
Everything you need is right here, so just follow this flow.
The interface is in English, but if you prefer Japanese, use the language switch at the top right. Pick whichever suits you best.

## 2
To try it right away, use the built-in sample data.
Open the Data tab, choose Sample, and load the fifteen-item, five-hundred-person binary dataset.
A preview appears on the right.

## 3
For a real analysis, you load your own CSV file.
Switch the data source to Upload, and pick your file.
First, specify which column holds the examinee ID.
Then choose the columns to analyze. Leave out extra columns, like ID or group, that aren't part of the analysis.
If a number marks unanswered items, enter it as the missing-value code. Here, it's ninety-nine.
Leave the response type on automatic.
Click Format Data, and the data is formatted. The type now in use shows at the top left — detected as binary.
To make sure it loaded correctly, check the Formatted Data tab.

## 4
exametrika supports four response types.
Binary; ordinal data, such as a five-point rating; nominal data with no order; and rated data — multiple-choice items where you pick the correct option from several.
In exametrika, this is called the rated type.
For rated data, choosing Rated reveals a field for the answer key.
Enter the correct option for each item, in order, separated by commas.
This way, you can load your data to match its nature.

## 5
Once your data is ready, the matching analysis tabs become available.
Let's switch back to the sample data and try item response theory.
…
Pick a model in the IRT tab and run it: you'll see the model fit, each item's discrimination and difficulty, and the ability estimates.
The Plots tab draws the item characteristic curves.

## 6
All your results can be saved.
In the left sidebar, just below the Run button, the output buttons are gathered together.
Save the fit indices, the item parameters, and the examinee parameters, each as a CSV.
Or save all the results at once, as a multi-sheet Excel file.
And the R script button gives you an R script that reproduces everything you did in this session.
Try it on screen, then reproduce it in code.

## 7
There are many more models, like latent class analysis and biclustering.
In biclustering, for example, examinees and items are grouped at the same time, and you can view the rearranged response pattern as an array plot.
…
For all the models you can run in shinyExametrika and exametrika, see the exametrika package website.

## 9
exametrika is also published as an R package, so do use it from R as well.
Its site is managed on GitHub. If you find a bug, or want a new feature, please open an Issue, and check out the Discussions page too.
So — enjoy Exametrika!
