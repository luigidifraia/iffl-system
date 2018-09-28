# Contributing

If you want to contribute to this project and make it better, your help is very welcome. Remember that the the noblest of all contributions is a good, clean pull request.

## Getting Started

* Make sure you have a [GitHub account](https://github.com/signup/free).
* Create a personal fork of the project on GitHub.
* Clone the fork on your local machine. Your remote repo on GitHub is called `origin`.
* Add the original repository as a remote called `upstream`.
  * Run `git remote add upstream https://github.com/luigidifraia/iffl-system.git`.
* If you created your fork a while ago be sure to pull `upstream` changes into your local repository.
  * To pull from `master`, run `git pull upstream master`.
* Create a new topic branch to work on. Branch from `develop` if it exists, else from `master`.
  * To create a topic branch `my-contribution` based on `master`, run `git checkout -b my-contribution master`.
* Implement/fix your feature(s); comment your code.
* Follow the code style of the project, including indentation.
* Add or change the documentation as needed.
* Make commits of logical and atomic units.
* Push your changes to the topic branch in your fork of the repository.
  * To push to `my-contribution`, run `git push origin my-contribution`.
* From your fork open a pull request in the correct branch. Target this project's `develop` branch if there is one, else go for `master`.
* If the maintainer requests further changes, just push them to your topic branch. The pull request will be updated automatically.
* Once the pull request is approved and merged you can pull the changes from `upstream` to your local repository and delete your extra branch(es).
