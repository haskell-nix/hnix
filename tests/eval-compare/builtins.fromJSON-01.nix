with builtins;

let simpleJSON = "{\"foo\": \"39\", \"bar\": 472}";
    screwyJSON = "{\"4275\": \"Please do not fail.\"}";
    crazyJSON = "     {
    \"response\": {
        \"success\": 1,
        \"current_time\": 1362339098,
        \"prices\": {
            \"35\": {
                \"11\": {
                    \"0\": {
                        \"current\": {
                            \"currency\": \"keys\",
                            \"value\": 39,
                            \"value_high\": 41,
                            \"date\": 1357515306
                        },
                        \"previous\": {
                            \"currency\": \"keys\",
                            \"value\": 37,
                            \"value_high\": 39
                        }
                    }
                },
                \"3\": {
                    \"0\": {
                        \"current\": {
                            \"currency\": \"metal\",
                            \"value\": 0.33,
                            \"value_high\": 0.66
                        }
                    }
                }
            },
            \"5002\": {
                \"6\": {
                    \"0\": {
                        \"current\": {
                            \"currency\": \"usd\",
                            \"value\": 0.39,
                            \"value_high\": 0.42,
                            \"date\": 1358090106
                        }
                    }
                }
            },
            \"5022\": {
                \"6\": {
                    \"1\": {
                        \"current\": {
                            \"currency\": \"metal\",
                            \"value\": 1.33,
                            \"value_high\": 1.55,
                            \"date\": 1357515175
                        }
                    }
                }
            }
        }
    }
}";
in [(fromJSON simpleJSON) (fromJSON screwyJSON) (fromJSON crazyJSON)]
