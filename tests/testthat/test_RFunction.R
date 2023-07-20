library('move2')

test_data <- test_data("input3_move2.rds") #file must be move2!

test_that("all behaviours are classified", {
  
  actual <- rFunction(data = test_data,
                      rooststart = 21,
                      roostend = 7,
                      travelcut = 3)
  
  expect(all(actual$behav %in% c("SResting", "SRoosting", "STravelling", "SFeeding", "Unknown")),
         failure_message = "some locations have been classified into non-permitted groups")
  expect(all(between(actual$cumtimestat_pctl, 0, 1)), 
         failure_message = "cumtimestat calculations have failed")
  expect(all(between(actual$cumtimestat_pctl_BC, 0, 1)),
         failure_message = "cumtimestat calculations have failed")
  
  expect("empPval" %!in% colnames(actual),
         failure_message = "second-stage classification has not been force stopped") # ensure model fitting hasn't run
  
  expect(nrow(actual) == nrow(test_data),
         failure_message = "Locations have been lost during classification. No locations should be removed")
  expect(unique(mt_track_id(actual)) == unique(mt_track_id(test_data)), 
         failure_message = "IDs have been lost during classification")

  })

test_that("error is thrown by invalid timestamps", {
  
  expect_error(
    actual <- rFunction(data = test_data,
                        rooststart = 25, 
                        roostend = -1,
                        travelcut = 3)
  )

  
})

test_that("error is thrown by invalid speed", {
  
  expect_error(
    actual <- rFunction(data = test_data,
                        rooststart = 12, 
                        roostend = 5,
                        travelcut = "not_a_number")
  )
  
  
})