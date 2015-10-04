# Site Archiver

Site Archiver is a program that provides three core functions:

* Archives websites by crawling them and saving all responses
* Serves archived websites over http
* Serves a single page application to provide a user interface for the tool

![Sites](/documentation/sites.png)
![Jobs](/documentation/job.png)

It has a few interesting features:

* All web responses are stored in a relational database. If a page or resource hasn't changed between archive jobs, it doesn't need to be stored again - only the hash of the response body is stored. There is a 1-to-many relationship between these hashes and response bodies.
* The entire response is stored in the database, not just the response body. This means things such as response codes and redirects are still archived correctly.
* Archived sites can be browsed via restful urls. When a site is archived, the archive job is assigned an id. You can access archived pages from permalinks that look like `/job/<JobId>/path/to/page`. Archived html pages will also be modified slightly before they are served to the user - urls are modified to point to the archived version of the site eg `http://domain.com/some/path` will be rewritten as `/job/<JobId>/some/path`.

## Building & Running

The server is written in Haskell (compiled with GHC 7.8.3) and uses [Scotty](https://hackage.haskell.org/package/scotty). Data is stored using sqlite3. The front end UI is built using Angular, Foundation and Sass.

You will need the following to build the application: GHC, cabal-install, node, bower, gulp and sqlite3.

The server serves the front end from a static folder called 'public'. This folder is generated by running the following commands in the '/web' folder:

 npm install
 bower install
 gulp

The server can be compiled & run by using the following commands:

 cabal-install --dependencies-only
 cabal run

The tool should then be accessible on port 3000.

## Possible Features

Features I'd like to add:

* Use E-Tags to make archiving more efficient on subsequent jobs
* Nicer interface & error handling
* Progress reporting while archiving
* Concurrency
* White/black listing of certain urls

## Contributing

Go for it!