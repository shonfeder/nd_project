open Core_kernel

let property name = QCheck.Test.make ~name

let unit name ~expected ~actual testable =
  Alcotest.test_case name `Quick
    begin fun () ->
      Alcotest.check testable "" expected actual
    end

let test name tested =
  Alcotest.test_case name `Quick
    begin fun () ->
      Alcotest.check Alcotest.bool "true" true tested
    end

let property_suite name tests =
  let tests = List.map ~f:QCheck_alcotest.to_alcotest tests in
  (name, tests)

let unit_suite name tests = (name, tests)
