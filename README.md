# Name Matching
Matching company names using computation and human review. 

In order to run the code in this repository, you'll need to set up a file called `paths.R` in the root. This file is ignored by git, and should specify your local Dropbox folder path as well as a Google API key for address geocoding, like so:

```
dropbox <- "path"
google_api_key <- "key"
```

You'll also need to set up a file called `data.txt` in order to run the makefile in this repository. This file is algo ignored by git, and should specify your local Dropbox folder path like so:
```
"path"
```

Once you set up `paths.R` and `data.txt` files, you should be able to run this repository by typing `make` into your command line (making sure that you're located in the root of this repository when doing so). If you want to re-run the entire project from scratch, delete all files in the Dropbox folders "generated_data" and "reviewed_data", besides those in the "training" and "address_backups" folders (those are time-intensive to reproduce and deleting them is highly discouraged), and execute `make`. 
