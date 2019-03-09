with builtins;

let numTestPrecisionA = 4.000000000000000000001;
    numTestPrecisionB = 4;
    numTest3 = -4.1;
    numTest4 = -4;
    numTestZeroA = 0;
    numTestZeroB = -0;
    numTestMaxBoundA = 999999999999999999;
    numTestMaxBoundB = 999999999999999998;
    numTestMinBoundA = -999999999999999999;
    numTestMinBoundB = -999999999999999998;
    stringTest1 = "abcd";
    stringTest2 = "abce";
    stringTestBase1 = "foo" + "/" + stringTest1;
    stringTestBase2 = "foo" + "/" + stringTest2;
    stringTestJSONA = toJSON stringTest1;
    stringTestJSONB = toJSON stringTest2;
    stringTestToFileA = toFile "stringTest1" stringTest1;
    stringTestToFileB = toFile "stringTest2" stringTest2;
in [(lessThan numTestPrecisionA numTestPrecisionB)
    (lessThan numTestPrecisionB numTestPrecisionA)
    (lessThan numTest3 numTest4)
    (lessThan numTest4 numTest3)
    (lessThan numTestZeroA numTestZeroB)
    (lessThan numTestZeroB numTestZeroA)
    (lessThan numTestMaxBoundA numTestMaxBoundB)
    (lessThan numTestMaxBoundB numTestMaxBoundA)
    (lessThan numTestMinBoundA numTestMinBoundB)
    (lessThan numTestMinBoundB numTestMinBoundA)
    (lessThan stringTest1 stringTest2)
    (lessThan stringTest2 stringTest1)
    (lessThan stringTestJSONA stringTestJSONB)
    (lessThan stringTestJSONB stringTestJSONA)
    (lessThan stringTest1 stringTestJSONB)
    (lessThan stringTestJSONB stringTest1)
    (lessThan stringTest2 stringTestJSONA)
    (lessThan stringTestJSONA stringTest2)
    (lessThan stringTest1 stringTestToFileB)
    (lessThan stringTestToFileB stringTest1)
    (lessThan stringTestToFileA stringTest2)
    (lessThan stringTest1 (baseNameOf stringTestBase1))
    (lessThan stringTest2 (baseNameOf stringTestBase2))
  ]
