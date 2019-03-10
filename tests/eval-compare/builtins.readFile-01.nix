with builtins;


let testA = toFile "foo" "foo's contents";
    testB = toFile "bar" testA;
in [(hasContext testA) (hasContext testB)]
