

server := shell("./testcl",async = T)

print $value

whenever server->stdout do
{
	print $value
	server->stdin("hello")
}

server->stdin("Hello")

server->stdin("hello")

