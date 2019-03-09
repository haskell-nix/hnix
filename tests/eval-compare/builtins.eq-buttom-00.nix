let

  plain = (let x = x; in x);
  nested_list = [(let x = x; in x)];
  nested_attrset = { y = (let x = x; in x); };
  nested_list_list = [[(let x = x; in x)]];
  nested_list_attrset = [{ y = (let x = x; in x); }];
  nested_list_function = [(_: let x = x; in x)];
  nested_attrset_list = { y = [(let x = x; in x)]; };
  nested_attrset_attrset = { y = { y = (let x = x; in x); }; };
  nested_attrset_function = { y = (_: let x = x; in x); };

  tests = [
    # (plain == plain) # Diverges
    # (nested_list == nested_list) # Diverges
    # (nested_attrset == nested_attrset) # Diverges
    (nested_list_list == nested_list_list)
    (nested_list_attrset == nested_list_attrset)
    (nested_list_function == nested_list_function)
    (nested_attrset_attrset == nested_attrset_attrset)
    (nested_attrset_list == nested_attrset_list)
    (nested_set_function == nested_set_function)
  ];

in tests
