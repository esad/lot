# Lot Server

Google App Engine service (written in Go), which does the following

1. serves the static assets of the built Lot app from `static/` (itself a symlink to `..build`)

2. accepts arbitrary JSON data posted to `/pickle` and assigns it an unique alphanumeric URL (i.e. `/lE494`)

3. when such URL is loaded, renders `static/index.html` replacing
   
   {{.Data}}

with the pickled JSON data.

This should be picked up by the Lot app which will unpickle it and if everything goes well load the stored state.