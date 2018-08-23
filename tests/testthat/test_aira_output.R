context('aira_output')

testdata_aira_model <- function() {
  data_set <- autovar::read_spss("inst/pp1.sav", to.data.frame=TRUE)
  endodata <- data_set[,c('SomBewegUur', 'SomPHQ')]
  exodata <- data_set[,c('UitbijterPHQ','UitbijterBeweg')]
  var_model <- vars::VAR(endodata, exogen=exodata, p=2, type='const')
  Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = var_model, orthogonalize= TRUE)
}

testdata_multiple_variables <- function() {
  data(Canada, package='vars')
  data_set <- Canada
  var.2c <- vars::VAR(data_set, p=2, type='both')
  Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = var.2c, orthogonalize= TRUE)
}

test_that('print_overview_percentage_effect', {
  test_that('Implement correct percentages', {
    # TODO: The numbers used in hte percentages are made up, they should be done properly
    skip('Implement correct percentages in the below tests')
  })
  test_that('print_overview_percentage_effect works with positive numbers', {
    aira_output <- suppressMessages(AiraOutput$new(aira = testdata_aira_model()))
    result <- aira_output$print_overview_percentage_effect(10)
    expected = "Effect achieved:\n"
    expected = paste(expected, "You could increase your SomBewegUur with 10% by increasing your SomPHQ with 330%\n", sep ="")
    expected = paste(expected, "You could increase your SomPHQ with 10% by decreasing your SomBewegUur with 96%\n", sep ="")
    expect_equal(result, expected)
  })

  test_that('print_overview_percentage_effect works with negative numbers', {
    aira_output <- suppressMessages(AiraOutput$new(aira = testdata_aira_model()))
    result <- aira_output$print_overview_percentage_effect(-10)
    expected = "Effect achieved:\n"
    expected = paste(expected, "You could decrease your SomBewegUur with 10% by decreasing your SomPHQ with 330%\n", sep ="")
    expected = paste(expected, "You could decrease your SomPHQ with 10% by increasing your SomBewegUur with 96%\n", sep ="")
    expect_equal(result, expected)
  })
})

test_that('print_percentage_effect', {
  aira_output <- suppressMessages(AiraOutput$new(aira = testdata_aira_model()))
  test_that('Implement correct percentages', {
    # TODO: The numbers used in hte percentages are made up, they should be done properly
    skip('Implement correct percentages in the below tests')
  })
  test_that('Can print with newlines', {
    result <- aira_output$print_percentage_effect('SomBewegUur', 20, print_newlines = TRUE, print_title = TRUE)
    expected = "Effect achieved:\n"
    expected = paste(expected, "You could increase your SomBewegUur with 20% by increasing your SomPHQ with 660%\n", sep ="")
    expect_equal(result, expected)
  })
  test_that('Can print without newlines', {
    result <- aira_output$print_percentage_effect('SomBewegUur', 20, print_newlines = FALSE, print_title = FALSE)
    expected = "You could increase your SomBewegUur with 20% by increasing your SomPHQ with 660%."
    expect_equal(result, expected)
  })
  test_that('Can print with title', {
    result <- aira_output$print_percentage_effect('SomBewegUur', 20, print_newlines = FALSE, print_title = TRUE)
    expected = "Effect achieved: "
    expected = paste(expected, "You could increase your SomBewegUur with 20% by increasing your SomPHQ with 660%.", sep ="")
    expect_equal(result, expected)
  })
  test_that('Can print without title', {
    result <- aira_output$print_percentage_effect('SomBewegUur', 20, print_newlines = TRUE, print_title = FALSE)
    expected = "You could increase your SomBewegUur with 20% by increasing your SomPHQ with 660%\n"
    expect_equal(result, expected)
  })
})

test_that('export_model', {
  test_that('Rosmalen', {
    json_example ='{
  "links": [
    {
    "source": 0,
    "target": 1,
    "distance": 0.9,
    "weight": "-0.258619869521396"
    }
    ],
    "nodes": [
    {
    "index": 0,
    "name": "SomBewegUur",
    "key": "SomBewegUur",
    "val": -1.8597041
    },
    {
    "index": 1,
    "name": "SomPHQ",
    "key": "SomPHQ",
    "val": 0.1706285
    }
    ]
  }'

    edges <- data.frame(source = 0,
                        target = 1,
                        distance = 0.9,
                        weight = "-0.258619869521396",
                        stringsAsFactors=FALSE
    )

    nodes <- data.frame(index = c(0, 1),
                        name = c('SomBewegUur', 'SomPHQ'),
                        key = c('SomBewegUur', 'SomPHQ'),
                        val = c(-1.8597041, 0.1706285),
                        stringsAsFactors=FALSE
    )
    expected <- list(links = edges, nodes = nodes)

    aira_output <- AiraOutput$new(aira = testdata_aira_model())
    result <- aira_output$export_model()
    expect_equal(result, expected, tolerance=1e-5)
  })
  test_that('Canada', {
    coef(testdata_multiple_variables()$var_model)
    aira_output <- suppressMessages(AiraOutput$new(aira = testdata_multiple_variables()))
    result <- aira_output$export_model()
    expect_equal(names(result),c('links', 'nodes'))

    # The values below have been copied, just testing if everything is in the export function.
    links <- data.frame(source = c(1,3,0),
                        target = c(0,1,3),
                        distance = rep(0.9,3),
                        weight = c(0.1716493, 0.8906147, -0.5761010), stringsAsFactors = FALSE)

    nodes <- data.frame(index = seq(0,3),
                        name = c('e', 'prod', 'rw', 'U'),
                        key = c('e', 'prod', 'rw', 'U'),
                        val = c(-1.6746779, -0.9059553, -2.1666230, 3.4960103), stringsAsFactors = FALSE)

    # The strings are too precise
    result$links$weight <- as.numeric(result$links$weight)

    expect_equal(result$links, links, tolerance= 1e-7)
    expect_equal(result$nodes, nodes, tolerance= 1e-7)
  })
})

test_that('export_var_network', {
  aira <- testdata_multiple_variables()
  aira_output <- AiraOutput$new(aira = aira)

  result <- aira_output$export_var_network()
  number_of_vars <- dim(aira$var_model$y)[2]
  expect_equal(dim(result), c(number_of_vars, number_of_vars))
  names <- dimnames(aira$var_model$y)[[2]]
  expect_equal(dimnames(result)[[1]], names)
  expect_equal(dimnames(result)[[2]], names)

  # Check the coefficients
  expect_equal(result[[2,1]], 0.171649253388995)
  expect_equal(result[[4,2]], 0.890614697744068)
  expect_equal(result[[1,4]], -0.576101035159176)
  result[[2,1]] <- 0
  result[[4,2]] <- 0
  result[[1,4]] <- 0
  expect_equal(unname(result), matrix(0,number_of_vars,number_of_vars))
})

test_that('export_model_to_json', {
  json_example ='{
  "links": [
    {
      "source": 0,
      "target": 1,
      "distance": 0.9,
      "weight": "-0.258619869521396"
    }
  ],
  "nodes": [
    {
      "index": 0,
      "name": "SomBewegUur",
      "key": "SomBewegUur",
      "val": -1.8597041
    },
    {
      "index": 1,
      "name": "SomPHQ",
      "key": "SomPHQ",
      "val": 0.1706285
    }
  ]
}'

  # remove newlines
  expected <- jsonlite::toJSON(jsonlite::fromJSON(json_example))
  aira_output <- AiraOutput$new(aira = testdata_aira_model())
  result <- aira_output$export_model_to_json()
  expect_equal(result, expected)
})
