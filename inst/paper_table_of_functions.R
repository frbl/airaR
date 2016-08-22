library('xtable')
column_names <- c('Aira.js', 'Aira-R')
res <- data.frame(
    DetermineOptimalNode = c('Yes','Yes'),
    DetermineLengthOfEffect = c('Yes','Yes'),
    DeterminePercentageEffect = c('Yes','Yes'),
    'Impulse Response Function Analysis' = c('Yes','Yes'),
    'Animations and Visual representation' = c('Yes','No'),
    Interactivity = c('Yes','No'),
    'Orthogonalized Impulse Response Function Analysis' = c('No','Yes'),
    Bootstrapping = c('No','Yes')
)
res <- t(res)

colnames(res) <- column_names
table <- xtable(res, label="tab:supported_functions",
                 caption='Supported functions of both implementations of AIRA', digits = 3, auto= TRUE)
print(table,
       file='inst/output/tab_supported_functions.tex',
       sanitize.text.function=function(str)gsub("."," ",str,fixed=TRUE),
       floating=TRUE,
       booktabs=TRUE, floating.environment = 'table')


print(res)
