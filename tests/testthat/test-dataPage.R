
test_that("Data extracted from example binary file", {
  testServer(DataPageServer, {
    session$setInputs(ChooseExample = "binaryEx")
    
    expect_equal(loaded_data(), rio::import("data/AntiVEGF_Binary_Pairwise_Long.csv"))
  })
})

test_that("Data extracted from example continuous file", {
  testServer(DataPageServer, {
    session$setInputs(ChooseExample = "continuousEx")
    
    expect_equal(loaded_data(), rio::import("data/AntiVEGF_Continuous_Pairwise_Long.csv"))
  })
})

test_that("Binary data matches between .csv and .xlsx files", {
  testServer(DataPageServer, {
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Binary_Pairwise_Long.csv"))
    csv_data = loaded_data()
    
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Binary_Pairwise_Long.xlsx"))
    xlsx_data = loaded_data()
    
    expect_equal(xlsx_data, csv_data)
  })
})

test_that("Continuous data matches between .csv and .xlsx files", {
  testServer(DataPageServer, {
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Continuous_Pairwise_Long.csv"))
    csv_data = loaded_data()
    
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Continuous_Pairwise_Long.xlsx"))
    xlsx_data = loaded_data()
    
    expect_equal(xlsx_data, csv_data)
  })
})

test_that("Server returns wrangled binary data", {
  testServer(DataPageServer, {
    session$setInputs(ChooseExample = "binaryEx", sort_criteria = "File order")
    
    expect_equal(session$returned()$data, wrangled_data())
  })
})

test_that("Server returns wrangled continuous data", {
  testServer(DataPageServer, {
    session$setInputs(ChooseExample = "continuousEx", sort_criteria = "File order")
    
    expect_equal(session$returned()$data, wrangled_data())
  })
})

test_that("Should sort binary data by study name", {
  testServer(DataPageServer, {
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Binary_Pairwise_unsorted.csv"), sort_criteria = "File order")
    unsorted_studies <- unique(wrangled_data()$Study)
    
    session$setInputs(sort_criteria = "Study name")
    sorted_studies <- unique(wrangled_data()$Study)
    
    expect_false(all(unsorted_studies == sorted_studies))
    
    expect_equal(
      sorted_studies,
      !!c("Berg 2015", "Biswas 2011", "Biswas 2011a", "Chakravarthy 2013", "Kodjikian 2013", "Krebs 2013", "Martin 2011", "Schauwvlieghe 2016", "Subramanian 2010"),
      label = format_vector_to_string(sorted_studies)
    )
  })
})

test_that("Should sort continuous data by study name", {
  testServer(DataPageServer, {
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Continuous_Pairwise_unsorted.csv"), sort_criteria = "File order")
    unsorted_studies <- unique(wrangled_data()$Study)
    
    session$setInputs(sort_criteria = "Study name")
    sorted_studies <- unique(wrangled_data()$Study)
    
    expect_false(all(unsorted_studies == sorted_studies))
    
    expect_equal(
      sorted_studies,
      !!c("DRCRnet_2015", "Ekinci_2014", "Nepomuceno_2013", "Wiley_2016"),
      label = format_vector_to_string(sorted_studies)
    )
  })
})

test_that("Should sort binary long data by study size", {
  testServer(DataPageServer, {
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Binary_Pairwise_Long.csv"), sort_criteria = "File order")
    unsorted_studies <- unique(wrangled_data()$Study)
    
    session$setInputs(sort_criteria = "Participant count")
    sorted_studies <- unique(wrangled_data()$Study)
    
    expect_false(all(unsorted_studies == sorted_studies))
    
    expect_equal(
      sorted_studies,
      !!c("Martin 2011", "Chakravarthy 2013", "Kodjikian 2013", "Berg 2015", "Schauwvlieghe 2016", "Krebs 2013", "Biswas 2011", "Biswas 2011a", "Subramanian 2010"),
      label = format_vector_to_string(sorted_studies)
    )
  })
})

test_that("Should sort binary wide data by study size", {
  testServer(DataPageServer, {
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Binary_Pairwise_Wide.csv"), sort_criteria = "File order")
    unsorted_studies <- unique(wrangled_data()$Study)
    
    session$setInputs(sort_criteria = "Participant count")
    sorted_studies <- unique(wrangled_data()$Study)
    
    expect_false(all(unsorted_studies == sorted_studies))
    
    expect_equal(
      sorted_studies,
      !!c("Martin 2011", "Chakravarthy 2013", "Kodjikian 2013", "Berg 2015", "Schauwvlieghe 2016", "Krebs 2013", "Biswas 2011", "Biswas 2011a", "Subramanian 2010"),
      label = format_vector_to_string(sorted_studies)
    )
  })
})

test_that("Should sort continuous long data by study size", {
  testServer(DataPageServer, {
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Continuous_Pairwise_Long.csv"), sort_criteria = "File order")
    unsorted_studies <- unique(wrangled_data()$Study)
    
    session$setInputs(sort_criteria = "Participant count")
    sorted_studies <- unique(wrangled_data()$Study)
    
    expect_false(all(unsorted_studies == sorted_studies))
    
    expect_equal(
      sorted_studies,
      !!c("DRCRnet_2015", "Wiley_2016", "Ekinci_2014", "Nepomuceno_2013"),
      label = format_vector_to_string(sorted_studies)
    )
  })
})

test_that("Should sort continuous wide data by study size", {
  testServer(DataPageServer, {
    session$setInputs(data = data.frame(datapath = "data/AntiVEGF_Continuous_Pairwise_wide.csv"), sort_criteria = "File order")
    unsorted_studies <- unique(wrangled_data()$Study)
    
    session$setInputs(sort_criteria = "Participant count")
    sorted_studies <- unique(wrangled_data()$Study)
    
    expect_false(all(unsorted_studies == sorted_studies))
    
    expect_equal(
      sorted_studies,
      !!c("DRCRnet_2015", "Wiley_2016", "Ekinci_2014", "Nepomuceno_2013"),
      label = format_vector_to_string(sorted_studies)
    )
  })
})
