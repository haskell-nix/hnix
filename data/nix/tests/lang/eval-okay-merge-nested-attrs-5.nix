# Test: deep nested path merge with direct attrset
{ a.b.c = 1; a.b = { d = 2; }; a = { e = 3; }; }
