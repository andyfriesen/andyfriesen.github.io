---
layout: post
title: Crux - Records
---
This is my second post about Crux.  You may want to read the [first one](/2016/05/19/crux.html) first.

Records are a pretty fundamental idea in every programming language.  It's important to get them right.

We have some requirements:

* Records have to be easy to understand
* Crux must be a delightful programming environment on the web too, so records must be easy to use when working with foreign JavaScript APIs.
* Mutability needs to be convenient and predictable

I think we've come up with something that's both novel and hits all the sweet spots.

# Row Polymorphism

First off, records in Crux are what we call _row polymorphic_.  This means firstly that a record is no more or less than the set of fields it has.  If two records have the same fields, and those fields have the same types, then they have the same record type.  This is also called _structural typing_.  OCaml and TypeScript also make use of this idea.

This is in stark contrast to languages like C# and Java where a type declaration adds a sort of identity to the data type.  This is what we call _nominal typing_ and Crux supports this as well. (I'll get to this another day)

In a structural type system, we don't care so much about exact matches.  Instead, we just care that a value has the properties that a particular function needs.  For instance, we might write a hypotenuse function for points

```
fun hypot(point) {
    sqrt(point.x * point.x + point.y * point.y)
}
```

The `point` parameter of this function clearly needs to have an `x` and a `y`, but we haven't said anything about what other properties it might have.  In Crux, like TypeScript, it doesn't matter:

```
let named_point = {
    name: "My House",
    x: 122.4194,
    y: 37.7749
}
let h = hypot(named_point)
```

This is ok.  As long as the argument satisfies the required properties, additional properties are allowed.

# Mutability

Immutable values are fantastic things to have around.  They're so much easier to reason about.  We've done a lot of work both in environments where things are mutable and immutable by default, and the latter is quite a lot better longterm.

We've also worked in environments where mutability is a fair bit less convenient to get at, and we'd really prefer to be on the other side of that fence.

To that end, we wanted Crux to afford easy access to immutable data, but with a convenient way to strip that off and start changing things.

We use type inference to sort all of this out.

You can mutate a record field just like you think you should:

```
let named_point = {
    name: "My House",
    x: 122.4194,
    y: 37.7749
}
named_point.name = "This name is much better"
```

One thing you can do in Crux is to explicitly declare record fields to be mutable or immutable.  Presently, we do this with a type annotation.  We might add syntax to make this easier.

```
// Define a little type alias, for brevity
type NamedPoint = {
    const name: String,
    mutable x: Number,
    mutable y: Number
}

let named_point : NamedPoint = {
    name: "The Greatest Point",
    x: 999,
    y: 999
}
```

These annotations are optional, and if you don't specify one, the type inference engine will figure it out.

```
fun zero_out(point) {
    point.x = 0
    point.y = 0
}
let my_point = { x: 3, y: 2 } // x and y must be mutable
zero_out(my_point)
```

So far we've talked about mutable and immutable record fields, but there is actually a third state which we haven't figured out a name for yet.  It is a record field that isn't mutated in the current scope, but may or may not be mutable in other scopes.

The reason for this is because we can easily prove that a function _requires_ a mutable field, but we can never prove that a mutable field is _forbidden_.  Consider our first example:

```
fun hypot(point) {
    sqrt(point.x * point.x + point.y * point.y)
}
```

Either a mutable or an immutable `x` and `y` will work just fine.  Every record field is thus one of the following:

* Mutable,
* Immutable, or
* An immutable view into a value which might be mutable.  This is very similar to `const` in C++.

The really nice thing about this scheme is that the type inference engine will generally stay out of your way until you put a type annotation on a record field.

# JavaScript

Lastly, Crux will not be delightful to use if it's difficult to talk to JavaScript code.  To make this easy, we promise that Crux will obey two rules:

* A Crux record maps exactly to a JavaScript object, and
* Calling a function on a Crux record always generates the code for a JS method call

Let's look at a simple example.  Say we want to run this function:

```
fun main() {
    document.body.insertBefore(
        document.createTextNode("Hello!"),
        document.body.firstChild
    )
}
```

First off, Crux doesn't yet know anything about browser APIs.  We'll add this to the standard library someday, but for now, we need to _build_ our standard library. :)

The `document` object is always in scope on a web page, so we'll use the `declare` construct to tell the compiler that it exists.

```
declare document : Document
```

No code is generated from this declaration.  It's just a promise to the compiler.

Next, we need to define the `Document` type:

```
type Document = {
    createTextNode: (String) -> Node,
    body: {
        insertBefore: (Node, Node) -> Node,
        firstChild: Node
    }
}

data Node {}
```

Astute readers might ask about what this means in relation to JS prototypes and method dispatch, and the answer is quite simple: Crux has no awareness whatsoever of these things.  We promise that `document.createTextNode("Hello!")` in Crux will generate the JS `document.createTextNode("Hello!")`, but how that JS statement will be executed is left up to the JS engine.

Note here that we also defined a `Node` type, but didn't say anything at all about its composition.  This is an easy way to make a data type that has no user-inspectable parts.  You can think of it as an inscrutable baton that gets passed around.

You can try it yourself in our [online playground](http://cruxlang.org/try).
