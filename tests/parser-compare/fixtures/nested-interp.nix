let
  a = "a";
  b = "b";
  c = "c";
in {
  depth1 = "${a}";
  depth2 = "${a}${b}";
  depth3 = "${a}${b}${c}";
  nested_concat = "${a + b + c}";
  nested_if = "${if true then a else b}";
}
