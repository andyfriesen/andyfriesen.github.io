---
layout: post
title: "What does unique_ptr<> cost?"
---
I just watched [Jonathan Blow's proposal](http://www.youtube.com/watch?v=TH9VCN6UkyQ) for a new programming language, which got me thinking about the difficulties that motivated the talk.

In particular, I think `unique_ptr<>` is fantastic, but I'm curious about how it affects compile times and code size.  Let's find out.

First, some C++

```c++
#include <memory>

using std::unique_ptr;

struct S {
    unique_ptr<int[]> ints;
};

int main() {
    const int LEN = 50;
    auto s = S {
        unique_ptr<int[]> { new int[LEN] }
    };

    auto j = 0u;
    for (auto i = 0u; i < LEN; ++i) {
        s.ints[i] = ++j;
    }

    auto sum = 0;
    for (auto i = 0u; i < LEN; ++i) {
        sum += s.ints[i];
    }

    printf("Sum! %i\n", sum);
}
```

Next, the equivalent C:

```c++
#include <stdlib.h>
#include <stdio.h>

typedef struct S {
    int* ints;
} S;

#define LEN 50

int main() {
    S s;
    unsigned i, j;
    int sum;

    s.ints = (int*)malloc(LEN * sizeof(int));

    j = 0u;
    for (i = 0u; i < LEN; ++i) {
        s.ints[i] = ++j;
    }

    sum = 0;
    for (i = 0u; i < LEN; ++i) {
        sum += s.ints[i];
    }

    printf("Sum! %i\n", sum);

    free(s.ints);
}
```

On my machine (a mid-2012 Retina MBP), I get these figures: (averaged over 5 runs of clang and clang++ each)

We'll look at code size too:

```
c1.c:      37ms / 8kb
cpp1.cpp: 123ms / 8kb
```

Wow!  What's going on?

First off, using clang++ to build the C version runs at the same speed.  That should have been obvious, but I wanted to test it anyway.

Secondly, if I add `#include <memory>` to the C version and run it through clang++, I see the same 123ms build times.

So, that's probably most of the picture, but my mental model of templates is that you pay for them in two ways:

1. When you #include the header, you pay the time it takes for the compiler to parse that header, and
2. you pay again to instantiate the template with a particular set of types.

So, what's the instantiation cost?

Let's try this:

```c++
struct S0 { int a0; S0(int i) : a0(i) {} }; auto u0 = unique_ptr<S0> { new S0(0) };
struct S1 { int a1; S1(int i) : a1(i) {} }; auto u1 = unique_ptr<S1> { new S1(1) };
// 998 more!
```

vs this

```c++
struct S0 { int a0; S0(int i) : a0(i) {} }; auto u0 = new S0(0);
struct S1 { int a1; S1(int i) : a1(i) {} }; auto u1 = new S1(1);
// 998 more!

// also
delete u0;
delete u1;
// et cetera
```

The results:

```
1000_structs.cpp:       1303ms / 79kb
1000_unique_ptrs.cpp:   6668ms / 185kb
```

Wow!  It looks like each `unique_ptr<>` costs about 5ms and 100 bytes.

First, let's look at compile speed.  I wonder if it has to do with the number of unique (heh) `unique_ptr<>` instantiations, or mere utterance of the type name.  If we change all the `unique_ptr<>`s so they have the same type, we get:

```
1000_identical_unique_ptrs.cpp: 599ms / 79kb
```

Wait, what?  Why is it faster than doing it the hard way?  Shouldn't our build times be worse because we're asking it to expand a bunch of extra templates?

Also note that file size matches up with what we get when we deallocate explicitly: Our executable gets larger with the number of *kinds* of `unique_ptr<>`s we instantiate, but it doesn't cost anything to use the same kind of pointer many times.  This makes sense: the full implementation shouldn't be much more than a deleted copy constructor, a move constructor, and a destructor.

Could it be that all those `delete` statements cost 120ms?  What happens if we remove them?

```
1000_struct_no_free.cpp: 667ms / 63kb
```

This is unexpected: a very boring, monomorphic built-in language construct costs more to use than a template class.

We still need to look into the code size:

```bash
$ clang++ -S -Os -std=c++11 1000_unique_ptrs.cpp
$ emacs 1000_unique_ptrs.cpp
```

I see this over and over:

```asm
    .private_extern __ZNSt3__110unique_ptrI2S1NS_14default_deleteIS1_EEED1Ev
    .globl  __ZNSt3__110unique_ptrI2S1NS_14default_deleteIS1_EEED1Ev
    .weak_def_can_be_hidden __ZNSt3__110unique_ptrI2S1NS_14default_deleteIS1_EEED1Ev
    .align  1, 0x90
__ZNSt3__110unique_ptrI2S1NS_14default_deleteIS1_EEED1Ev: ## @_ZNSt3__110unique_ptrI2S1NS_14default_deleteIS1_EEED1Ev
    .cfi_startproc
## BB#0:
    pushq   %rbp
Ltmp7:
    .cfi_def_cfa_offset 16
Ltmp8:
    .cfi_offset %rbp, -16
    movq    %rsp, %rbp
Ltmp9:
    .cfi_def_cfa_register %rbp
    movq    %rdi, %rax
    movq    (%rax), %rdi
    movq    $0, (%rax)
    testq   %rdi, %rdi
    je  LBB1_1
## BB#2:                                ## %_ZNKSt3__114default_deleteI2S1EclEPS1_.exit.i.i
    popq    %rbp
    jmp __ZdlPv                 ## TAILCALL
LBB1_1:                                 ## %_ZNSt3__110unique_ptrI2S1NS_14default_deleteIS1_EEED2Ev.exit
    popq    %rbp
    retq
```

FYI, the tail call at the end is the global `operator delete`:

```bash
$ c++filt __ZdlPv
operator delete(void*)
```

It looks like the destructor isn't being inlined.  That's a shame.  I wasn't able to coerce clang into inlining it. (it already has the `__always_inline__` attribute)

# Recap

* You pay a tiny bit of constant overhead just to `#include <memory>`
* You pay a bit for each distinct specialization of `unique_ptr<>`, but it's **cheaper** than what you pay for an explicit `delete` statement.
* You pay a bit of filesize for each kind of `unique_ptr<>`.
* It's basically free to talk about many `unique_ptr<>`s of the same type.

[Code](https://github.com/andyfriesen/benchmark_unique_ptr)
