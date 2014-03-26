---
layout: post
title: Haskell at IMVU
---

This is a copy of [the article I wrote](http://engineering.imvu.com/2014/03/24/what-its-like-to-use-haskell/) for the IMVU Engineering Blog.

Since early 2013, we at IMVU have used Haskell to build several of the REST APIs that power our service.

When the company started, we chose PHP as our application server language, in part, because the founders expected the website to only be a small part of the business!  IMVU was primarily about a downloadable 3D client.  We needed “a website or something” to give users a place to download our client from, but didn’t expect it would have to be much more than that. This shows that predicting the future is hard.
Years later, we have quite a lot of customers, and we primarily use PHP to serve them.  We’re big enough that we run multiple subteams on separate initiatives at the same time.  Performance is becoming important to us not just because it matters to our customers, but because it can easily make the difference between buying 4 servers and buying 40 servers to support some new feature.

So, early in 2012, we found ourselves ready to look for an alternative that would help us be more rigorous.  In particular, we were ready for the idea that sacrificing a tiny bit of short term, straight-line time to market might actually speed us up in the long run.

# How We Got Here

I started learning Haskell in my spare time in part because Haskell seems like the exact opposite of PHP: Natively compiled, statically typed, and very principled.

My initial exploration left me interested in evaluating Haskell at real scale.  A year later, we did a live-fire test in which we taught multiple teammates Haskell while delivering an important new feature under a deadline.

Today, a lot of our backend code is still driven by PHP, but we have a growing amount of Haskell that powers newer features. The process has been exciting not only because we got to actually answer a lot of the questions that keep many people from choosing not to try Haskell, but also because it’s simply a better solution.

The experiment to start developing in Haskell took a lot of internal courage and dedication, and we had to overcome a number of, quite rational, concerns related to adopting a whole new language. Here are the main ones and how they worked out for us:

# Scalability

The first thing we did was to replace a single service with a Haskell implementation.  We picked a service that was high-volume but was not mission critical.

We didn’t do any particular optimization of this new service, but it nevertheless showed excellent performance characteristics in the field.  Our little Haskell server was running on a pair of spare servers that were otherwise set for retirement, and despite this, each machine was handling about 20x as many requests as one of our high-spec PHP servers could manage.

# Reliability

The second thing we did was to take our hands off the Haskell service and leave it running until it fell over.  It ran for months without intervention.

# Training

After the reliability test, we were ready to try a live fire exercise, but we had to wait a bit for the right project.  We got our chance in early 2013.

The rules of the experiment were simple: Train 3 engineers to write the backend for an important new project and keep up with a separate frontend team.  Most of the code was to be new, so there was relatively little room for legacy complications.

We very quickly learned that we had also signed up for a lot of catch-up work to bring the Haskell infrastructure inline with what we’ve had for years in PHP.  We were very busy for awhile, but once we got this infrastructure out of the way, the tables turned and the front-end team became the limiting factor.

Today, training an engineer to be productive in our Haskell code is not much harder than training someone to be productive in our PHP environment.  People who have prior functional programming knowledge seem to find their stride in just a few days.

# Testing

Correctness is becoming very important for us because we sometimes have to change code that predates every current developer.  We have enough users that mistakes become very costly, very quickly.  Solving these sorts of issues in PHP is sometimes achievable but always difficult.  We usually solve them with unit tests and production alerts, but these approaches aren’t sufficient for all cases.

Unit tests are incredible and great, but you’re always at the mercy of the level of discipline of every engineer at every moment. It’s easy to tell your teammates to write tests for everything, but this basically boils down to asking everyone to be at their very best every day.  People make mistakes and things slip through the cracks.

When using Haskell, we actually remove an entire class of defects that we have to write tests for. Thus, the number of tests we have to write is smaller, and thus there are fewer cases we can forget to write tests for.

We like unit testing and test-driven development (TDD) at IMVU and we’ve found that Haskell is better with TDD, but also that TDD is better with Haskell.  It takes fewer tests to get the same degree of reliability out of Haskell.  The static verification takes care of quite a lot of error checking that has to be manually implemented (or forgotten) in PHP.  The Haskell QuickCheck tool is also a wonderful help for developers.
The way Haskell separates pure computations from side effects let us build something that isn’t practical with other languages: We built a custom monad that lets us “switch off” side effects in our tests.  This is incredible because it means that trying to escape the testing sandbox breaks compilation. While we have had to fight intermittent test failures for eight years in PHP (and at times have had multiple engineers simultaneously dedicated to the problem of test intermittency,) our unit tests in Haskell cannot intermittently fail.

# Deployment

Deployment is great. At IMVU, we do continuous deployment, and Haskell is no exception. We build our application as a statically linked executable, and rsync it out to our servers. We can also keep old versions around, so we can switch back, should a deployment result in unexpected errors.

I wouldn’t write an OS kernel in it, but Haskell is way better than PHP as a systems language. We needed a Memcached client for our Haskell code, and rather than try to talk to a C implementation, we just wrote one in Haskell.  It took about a half day to write and performs really well. And, as a side effect, if we ever read back some data we don’t expect from memcached (say, because of an unexpected version change) then Haskell will automatically detect and reject this data.

We’ve consistently found that we unmake whole classes of bugs by defining new data types for concepts to wrap primitive types like integers and strings.  For instance, we have two lines of code that say that “customer IDs” and “product IDs” are represented to the hardware as numbers, but they are not mutually convertible.  Setting up these new types doesn’t take very much work and it makes the type checker a LOT more helpful. PHP, and other popular dynamic server languages like Javascript or Ruby, make doing the same very hard.

Refactoring is a breeze.  We just write the change we want and follow the compile errors.  If it builds, it almost certainly also passes tests.

# Not All Sunshine and Rainbows

Resource leaks in Haskell are nasty.  We once had a bug where an unevaluated dictionary was the source of a space leak that would eventually take our servers down.  We also ran into an issue where an upstream library opened /dev/urandom for randomness, but never closed the file handle.  These issues don’t happen in PHP, with its process-per-request model, and they were more difficult to track down and resolve than they would have been in C++.

The Haskell package manager, Cabal, ended up getting in the way of our development. It lets you specify version ranges of particular packages you want, but it’s important for everyone on the team to have exactly the same versions of every package.  That means controlling transitive dependencies, and Cabal doesn’t really offer a way to handle this precisely. For a language that is so very principled on type algebra, it’s surprising that the package manager doesn’t follow suit regarding package versioning. Instead, we use Cabal for basic package installation, and a custom build tool (written in Haskell.)

# Hiring

I’ll admit that I was very worried that we wouldn’t be able to hire great people if our criteria was expertise in an uncommon language without a comparatively sparse industrial track record, but the honest truth is that we found a great Haskell hacker in the Bay area after about 4 days of looking.

We had a chance to hire him because we were using Haskell, not in spite of it.

# Final Thoughts

While it’s usually difficult to objectively measure things like choice of programming language or softwarestack, we’re now seeing fantastic, obvious productivity and efficiency gains.  Even a year later, all the Haskell code we have runs on just a tiny number of servers and, when we have to make changes to the code, we can do so quickly and confidently.
