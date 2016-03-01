context('aira_output')

testdata_aira_model <- function() {
  data_set <- autovar::read_spss("inst/pp1_nieuw_compleet.sav", to.data.frame=TRUE)
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
  aira_output <- AiraOutput$new(aira = testdata_aira_model())
  result <- aira_output$print_overview_percentage_effect(10)
  expected = "Effect achieved:\n"
  expected = paste(expected, "You could increase your SomBewegUur with 10% by increasing your SomPHQ with 36%\n", sep ="")
  expected = paste(expected, "You could increase your SomPHQ with 10% by decreasing your SomBewegUur with 9%\n", sep ="")
  expect_equal(result, expected)
})

test_that('print_percentage_effect', {
  aira_output <- AiraOutput$new(aira = testdata_aira_model())
  result <- aira_output$print_percentage_effect('SomBewegUur', 20)
  expected = "Effect achieved:\n"
  expected = paste(expected, "You could increase your SomBewegUur with 20% by increasing your SomPHQ with 72%\n", sep ="")
  expect_equal(result, expected)
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
    aira_output <- AiraOutput$new(aira = testdata_multiple_variables())
    result <- aira_output$export_model()
    print(result)
  })
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
