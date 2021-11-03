## Contributing to Spire

This document is a guide to how to get started contributing to Spire.

### Writing guides or documentation

Often the biggest issue facing open-source projects is a lack of good
documentation, and Spire is no exception here. If you have ideas for
specific pieces of documentation which are absent, feel free to open a
specific issue for that.

We also gladly accept patches for documentation. Anything from fixing
a typo to writing a full tutorial is a great way to help the
project.

The documentation lives in the [`docs/src/main/mdoc`](https://github.com/typelevel/spire/tree/main/docs/src/main/mdoc) directory in the repository. To preview the website locally:

1. [Install Jekyll v4](https://jekyllrb.com/docs/installation/).
2. Run `sbt docs/makeMicrosite`. Change directory to `docs/target/site` and run `jekyll serve -b /spire`.
3. Preview website at http://localhost:4000/spire.

### Reporting bugs, issues, or unexpected behavior

If you encounter anything that is broken, confusing, or could be
better, you should
[open an issue](https://github.com/non/spire/issues). You don't have
to know *why* the error is occurring, or even that an error happens at
all.

If you are trying to do something with Spire, and are having a hard
time, it could be any of the following issues:

 * an actual bug or error
 * an omission or problem with the API
 * a confusing edge case
 * a documentation problem

Feel free to open a bug before you're sure which of these is
happening.  You can also ask questions on the
`#spire` channel on the [Typelevel Discord](https://discord.com/invite/XF3CXcMzqD) to get
other people's opinions.

### Creating or improving tests

Spire uses [munit](https://scalameta.org/munit/) and
[ScalaCheck](https://scalacheck.org/) to test our code. The tests
fulfill a number of important functions:

 * ensure our algorithms return correct results
 * check the visibility of our type class instances
 * confirm that the API works as we expect
 * test edge cases which might otherwise be missed

If you find a bug you are also encouraged to submit a test case (the
code you tried that failed). Adding these failing cases to Spire's
tests provides a good way to ensure the bug is really fixed, and is
also a good opportunity to start contributing.

ALso, when you notice places that lack tests (or where the tests are
sparse, incomplete, or just ugly) feel free to submit a pull request
with improvements!

### Submitting patches or code

Spire is on Github to make it easy to fork the code and change it.
There are very few requirements but here are some suggestions for what
makes a good pull request.

If you're writing a small amount of code to fix a bug, feel free to
just open a pull request immediately. You can even attach some code
snippets to the issue if that's easier.

For adding new code to Spire, it's often smart to create a topic
branch that can be used to collaborate on the design. Features that
require a lot of code, or which represent a big change to Spire, tend
not to get merged to main as quickly. For this kind of work, you
should submit a pull request from your branch, but we will probably
leave the PR open for awhile while commenting on it.

You can always message the `#spire` channel on the [Typelevel Discord](https://discord.com/invite/XF3CXcMzqD)
to get a second opinion on your idea or design.

### Ask questions and make suggestions

Spire strives to be an excellent part of the Scala ecosystem. We
welcome your questions about how Spire works now, and your ideas for
how to make it even better!
