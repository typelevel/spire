---
layout: home
title:  "migration guide"
section: "Home"
position: 7
---

## v0.18.0 Migraion Guide

Spire supports Scala 3 since version 0.18.0!

If you find any erroneous behavior due to Scala 3 compatibility, please feel free to open issue or send PR!

Spire tries keeping compatibility as much as possible, but some APIs change.
If your code uses following APIs, you need to modify them.

## API changes
 
### General
- because the method `Dist.given` clashes with the `given` keyword, it was renamed to `given_`
- the class `DistIterator` has a `next` member that clashes with a `next` method. The field has renamed to `_next`
- `UnboundSyntax` seems to be bogus and it is not used anywhere in `spire`. It was marked as deprecated in scala 2 and removed in scala 3
- parentheses: these are now omitted for 0-arity (empty-parens) methods with no side-effects.
### Macro
- `Checked.tryOrReturn` was removed.
- Use of `Machinist` was removed.
- `PackMacro` is available both Scala 2 and 3.
- `cFor` was deprecated and renamed to `fastFor`. Available both Scala 2 (as `cFor`) and 3(as `fastFor`). Performance-sensitive code should continue using `cfor` on Scala 2.
- `Checked` was partly ported.
- All `Literal`s macros except `radix` are ported.
- `Auto` is NOT yet supported in Scala 3.
- `Fpf` is NOT yet ported to Scala 3.


## Internal Changes

In general, these changes do not affect library users, but may have effect on some developers and contributors.

### General changes:
- do/while constructs were converted to while
- Implicits need type ascription
- Some number conversions need to be explicit
- Remove or rewrite the macros

### Performance

As of Nov 2021, Scala 3 support can cause performance degration compared to Scala 2 mainly because of lack of `specialization`. Some quick benchmarking shows no important difference for code not using `specialization`. On the other hand, code relying on this feature takes a severe performance hit. This is mentioned in [v0.18.0-M1 release](https://github.com/typelevel/spire/releases/tag/v0.18.0-M1) and [#1067](https://github.com/typelevel/spire/pull/1067#issue-998607764) too.

The complete benchmark suite run for 2.13 vs 3 is available here: [#1067 (comment)](https://github.com/typelevel/spire/pull/1067#issuecomment-939369626).


Latest information is available at [release](https://github.com/typelevel/spire/releases) page.
