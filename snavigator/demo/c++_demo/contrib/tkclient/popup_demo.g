include "popup.g"

print popup("Enter text here:")
create_popup("Demoing popup message ...")

shell("sleep 3")

message("Gone in 3 seconds")
shell("sleep 3")

message_done()
shell("sleep 3")

message("And back")
shell("sleep 3")

exit
