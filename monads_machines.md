% Monads are machines: an intuitive introduction to monads
% Seno
% 2021-10-25

<!-- # Monads are machines: an intuitive introduction to monads -->

# **WARNING!!!**

What follows is full of vague statements meant to
*complement* the actual formal notions with some intuition, hence improving understatement.

These statements ARE NOT what the concepts are in a strict sense and should not be used in any serious argument. 

Also, the readers must be comfortable with ill-defined notions appealing to their subjective experience of the world, as well as keep pedantic comments to themselves.

# Introduction

We wrote this article is because we believe that its is hard to find a good introduction to monads that gives some satisfactory motivation and intuition. As it is usual with math topics, only the mechanical and/or formal parts are usually introduced. In months studying Haskell, we went to look different sources to try to have a better grasp of the concept, but it always felt that there was always something missing.

This made us went to look to the original academic papers that introduced the concept. The first one to be explicit about monads is @Mog89, but it is a very theoretical paper assuming lot's of category theory. Luckily, Waddler wrote two papers (@Wad90 and @Wad92) that are very accessible. This article is basically @Wad92 revisited, but with some twists:

* In both @Mog89 and @Wad92, monads are said to be a way to make different kinds of "computations." We found more appealing to our intuition to think about monads as different "machines", and explain this idea in [# Monads as "machines"].

* This can be a confusing point. Instead of using the standard way that Haskell uses to define monads, with the maps `return` and `bind`, we chose to use an equivalent formulation using `return`, `join` and `fmap`. This is because it fits much better with the machine metaphor, and we think it was really worth it. We explain how to get the usual definition shortly after, in [# Monadic functions and bind].

* We don't cover all the content in @Wad92, and we moved the examples of monads we judged less important or more confusing to a different section (see [# Examples of "machines"] and [# More examples of "machines"]).

* Notation was changed a bit, since Haskell changed a bit since 1992.

The main thing we will do is to write an interpreter of a very simple version of lambda-calculus, and then use the different monads to change the behavior of this interpreter. Here is a quote from @Wad92:

> Say I write an interpreter in a pure functional language.

> To add error handling to it, I need to modify the result type to include error values, and at each recursive call to check for and handle errors appropriately. Had I used an impure language with exceptions, no such restructuring would be needed.

> To add an execution count to it, I need to modify the the result type to include such a count, and modify each recursive call to pass around such counts appropriately. Had I used an impure language with a global variable that could be incremented, no such restructuring would be needed.

> To add an output instruction to it, I need to modify the result type to include an output list, and to modify each recursive call to pass around this list appropriately. Had I used an impure language that performed output as a side e ect, no such restructuring would be needed.

> Or I could use a *monad*.


# Monads as "machines"

```haskell
{-\# LANGUAGE NoImplicitPrelude, InstanceSigs, FlexibleInstances #-}

import Prelude hiding (Functor, Monad, fmap, return, (>>=), lookup)
import Control.Monad.State.Lazy (State)
```

# Monadic functions and bind

# Representing our lambda calculus in Haskell

# Examples of "machines"

# More examples of "machines"

# The monad laws

# Monad composition