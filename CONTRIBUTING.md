# Contributing to Tuatara's packages

Changes come into these packages only through pull requests, and pull requests
only from approved contributors. Anyone may open an issue: to report a
fault, to suggest an idea, or to talk about a change before making it.

## Becoming an approved contributor

Open an issue that says what you would like to work on. A maintainer who
agrees adds you to the organisation's contributors; a pull request from
anyone else is closed by the checks, with a note pointing here.

## Making a change

1. Fork `Tuatara-Editor/tuatara-packages` and clone your fork. Add the project as
   `upstream`:

   ```
   git remote add upstream https://github.com/Tuatara-Editor/tuatara-packages.git
   ```

2. Start each change from the newest `main`, on a branch of its own:

   ```
   git fetch upstream
   git switch -c short-name-of-the-change upstream/main
   ```

3. Change and test as [README.org](README.org) says: `tools/test.sh` runs
   every package's tests against a Tuatara source tree beside this one.
4. Commit with a sign-off, and sign the commit if you can:

   ```
   git commit -s -S
   ```

5. Push the branch to your fork and open a pull request against
   `Tuatara-Editor/tuatara-packages:main`. Keep it to one change: two unrelated
   fixes are two pull requests.
6. A maintainer reviews it. Push further commits to the same branch to
   answer the review; the pull request is merged as one commit, its title
   becoming the commit's first line.
7. After the merge, delete the branch and bring your fork's `main` up to
   date (`git switch main && git pull --ff-only upstream main && git push`).

## What a change needs

- **Its tests.** A fix, or two ways of doing one thing merged into one, comes
  with a test that fails on the code before the change and passes after it.
  A fix for slowness measures what it fixes.
- **Its documentation.** Every definition has a docstring that a user reads in
  the help; the package's part of README.org is updated where the change
  makes it wrong.
- **Tuatara's conventions**, in its
  [HACKING.org](https://github.com/Tuatara-Editor/tuatara/blob/main/HACKING.org):
  one way for each kind of thing, nothing slow on the editor's thread, no
  colours outside the theme, nothing in a file ever run. A package uses only
  the names Tuatara's API exports, and what it needs that Tuatara lacks is
  added to Tuatara rather than written again here.
- **English.** All text, from docstrings to commit messages, is in English;
  any spelling is welcome, kept the same within a package.
- **Nothing private.** No files, names, logos or text that belong to an
  employer, a customer or a case. Test files are small, made up for the test,
  and say nothing about real people.
- **No paths outside the project.** Code, tests, documentation and examples
  refer to no file or folder of anyone's own: no home folder layout, note or
  file name, tag, template, account or machine. They name only the project's
  own files, a test's own temporary folder, what every system has (`/tmp`,
  `/etc`), or plain made-up places (`~/notes/`, `~/Documents/report.org`).

## Sign-off

The `Signed-off-by:` line `git commit -s` adds is your statement, under the
[Developer Certificate of Origin](https://developercertificate.org/), that
you wrote the change or otherwise have the right to give it under the
project's licence, the GNU General Public License, version 3 or later. The
checks refuse a pull request with a commit not signed off by its author;
`git rebase --signoff upstream/main` adds the line to every commit of a
branch.

## Security faults

Report them privately, never in an issue: see
[SECURITY.md](https://github.com/Tuatara-Editor/.github/blob/main/SECURITY.md).
