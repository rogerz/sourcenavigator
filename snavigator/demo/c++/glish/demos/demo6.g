a := [foo=T, bar=9, bletch="foo bar bletch"]
write_value( a, "fleabark" )

a := read_value( "fleabark" )
print a.foo, a.bar, a.bletch
a := 3
