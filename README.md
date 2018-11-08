[![Build Status](https://travis-ci.org/ki11men0w/git-helper.svg?branch=master)](https://travis-ci.org/ki11men0w/git-helper)
# Git helper
`gith` CLI utility for some tasks related to `git` source control management system.

This utility is written in `haskell` and you can build it from sources:
```sh
git clone --recursive https://github.com/ki11men0w/git-helper.git
cd git-helper
stack install
```

Or you can download prebuilded executable from the latest release on [github](https://github.com/ki11men0w/git-helper/releases/latest).

To get help try:
```sh
gith --help
```
or if you have a wide terminal:
```sh
gith --help=150
```

This utility uses `git` executable and run it in a shell as `git`.
Commonly it means that `git` utility must be available via `PATH` environment variable and must be runnable as `git` without path spec.
