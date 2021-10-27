test_that("Tomba Init", {
  client <- Tomba(key="ta_xxxx",secret="ts_xxxx")
  expect_that( client@key, equals("ta_xxxx") )
  expect_that( client@secret, equals("ts_xxxx") )

})
