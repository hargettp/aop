# Aspect-Oriented Programming for Prolog

[Aspect-oriented programming](https://en.wikipedia.org/wiki/Aspect-oriented_programming) (AOP) is a technique for enabling software engineers to separate concerns (modeled as an _aspect_) within their code. The intent of such separation is reduction of coupling and simplification of code in each aspect: ideally the author of an aspect doesn't require deep awareness of other aspects in order for their own code to function correctly.

The `aop` library is not a complete implemention of AOP, but is instead an opinionated perspective tailored to Prolog. As such, `aop` adds the following constructions to Prolog:

* Aspects for grouping related *object* and *method* declarations

* Objects and messages for implementing a style of object orientation, mostly by translating object oriented style message sends into ordinary calls to consistenty-structured predicates called *methods*. Object can in fact be any object, since an object can itself be expressed as a variable matching any prolog term

* Ability for aspects to add *actions* invoked synchronously _before_ or _after_ methods on a particular object

* Ability for aspects to add *events* triggered asynchronously _before_ or _after_ method a method

* Introduce new methods into existing objects

* Enabling / disabling aspects dynamically

## Philosophy of Design Choices

Several principles guide the design choices in the `aop` library:

* **Suppport object-oriented style of programming.** Most code written for `aop` can use the `::` operator to send a message on the right hand side (expressed as a compound prolog term or an atom) to the receiver on the left hand side.
* **Leverage native Prolog facilities.** The `::` is just an operator defined as `send_message(Object, Message)`. Further, a default implementation of `send_message` eventually devolve into calls to native Prolog predicates extended with additional arguments to support messaging passing and aspect oriented styles of progrmaming. As all such message sends are just Prolog goals, reasoning about program behavior is generally the same if message sends were expressed as goals.
* **Objects as units of modularity.** Methods defined on an object can be regarded "scoped" to that object, allowing for modular construction fo code beyond just that in native Prolog modules.
* **Tailored to SWI-Prolog.** The library explicitly employs features known to exist in [SWI Prolog](https://swi-prolog.org), a mature and well proven open source implementation of Prolog and related facilities.

## DSL

This library provides a DSL for structuring code for an aspect-oriented style, in line with the opionions of its design.

A typical source file might look something like this:

```prolog
% Module declarations are still useful, although exports are not

:- module(my_aop_module, [

  ]).

% Load this aop library
:- use_module(library(aop/ifc)).

% Starts definition of a new aspect.
:- new_aspect(my_aspect).

  % Defines a new object provided by this aspect; variables in the term
  % can generalize its applicability. The second optional argument
  % is an array of accessors, useful for extracting the arguments
  % to the object's expression. Because accessors are in the same
  % statement as the object itself, the same variables are available.
  :- new_object(my_object(Name), [
    % Defines a method :for accessing the object's name parameter, e.g., 
    % my_object(foo)::name(N) will bind N to foo
    name(Name)
    ]).

    % All clauses inside the object definition become methods on the object. Thus, this method
    % can be used like this: my_object(foo)::print. In this case there are no paraemters to the goal.
    print :-
      % Inside a method, the unary :: operator is equivalent to sending the message
      % to the current object. Usef of the unary :: operator is not required, but a
      % convenience.
      ::name(Name),
      % Calling regular Prolog goals is natural and unchanged from how its
      % done in a non-aop source file
      format("~q~n",[Name]).

  % Ends the currently open object definition.
  :- end_object.

  % Objects are not required to have parameters to their term, or accessors.
  :- new_object(printer).

    print_object(Object) :-
      % Invoking a method on another object uses the binary :: operator
      Object::print.

  :- end_object.

  % This is a contrived sample just to show more code; its not intended to be an
  % indicator of how to design code with aop
  :- new_object(sample) :-
    
    print_it(SomeObject) :-
      printer::print_object(SomeObject).

  :- end_object.

% Ends the currently open aspect definition.
:- end_aspect.
```

## DSL Predicates and Directives

This library adds several new predicates and directives for structuring AOP code.

* `:- new_aspect(_)` / `:- end_aspect` - Brackets an aspect definition, inside which one or more object definitions should appear.
* `:- new_object(_)` / `:- end_object` - Brackets an object definition, with all predicates defined inside interpreted as methods on the defining object.
* `:- in_object(_)` / `:- end_object` - Brackets an extension of an existing object, adding new methods not originally present. USeful for extending an existing object, espeically if defined in a different aspect.
* `:- method <predicate>/<arity>` - Analogous to the `dynamic` directive in ordinary prolog: indicates that objects may implement this method.
* `:- nested_object(foo(Bar))` / `:- end_object` - A helpful trick for creating "nested" objects: that is, an object whose first term argument is actually the enclosing object in which this appears. Thus, `foo(Bar)` if nested inside an object such as `baz(Wazoo)`, then the real object being declared is actually `foo(baz(Wazoo), Bar)`.
* `::<some_method>(This, MethodParameter) :- <body of code>` - When used in the head of a clause, the unary `::` operator indicates that the method takes an explicit `This` parameter as the first argumenbt of the head -- thus making bindings of that variable available in the body of the clause. The variable will be bound to the receiving object, and can be any variable name (e.g., the use of `This` as a name is a just a convention).
* `::this(This)` - in the body of a method, an alternative method for accessing the receiver is this built-in method. Again, the variable name `This` passed as an argument is just a convention, and any variable (or expression) can be used.
* `::at(This, BeforeOrAfter, Message)` - Defines an event handler to be invoked _synchronously_ `before` or `after` the message on the receiving object. 
* `::on(This, BeforeOrAfter, Message)` - Defines an event handler to be invoked _asynchronously_ `before` or `after` the message on the receiving object.
* `:- extension <predicate_name>/<arity>` - Handlers for `on` / `at` directives will only be invoked if this directive is also present for the intended message.
* `:- use_aspect(foo)` and `use_aspect(foo/bar/baz)` - A helper method for loading aspects into an application, similar to `:- use_module(foo)`. The `aop` library by default adds an `./aspects` directory into the current file search path, so that `use_aspect(foo)` can become the traditional `use_module(aspects(foo))` which is equivalent to `use_module(./aspects/foo)`. The key difference is that when the aspect name contains a `/`, then each containing step is loaded first. Thus, the compound `:- use_aspect(foo/bar/baz)` translates to the following:
  
  ```prolog
  :- use_module(aspects(foo)).
  :- use_module(aspects(foo/bar)).
  :- use_module(aspects(foo/bar/baz)).
  ```

## Semantics

Using `aop` is not intended to alter the basic behavior of Prolog applications. In essence, its just providing some semantic sugar to structure the application in a more object and aspect oriented style.

Message sends (for example) using the binary `::` operator translate to a call to an internal `send_message` predicate. Thus:

```prolog
Object::some_method(Parameter1, Parameter2)
```

translates into:

```prolog
send_message(Object, some_method(Parameter1, Parameter2))
```

The actual `send_message` predicate (internal to the `aop` library and not intended to be overwritten or extended with new clauses) attempts to efficiently locate a suitable clause for the method. The basic mechanics involve translating `some_method(Parameter1, Parameter2)` into an equivalent goal with 2 additional parameters in first position: `some_method(Aspect, Object, Parameter1, Parameter2`, and then searching for a module that contains such clauses with that in its head.

For example, if the `some_method` definition originally appears in a `some_aspect` aspect declaration which also contains a `some_object(Foo)` declaration, then `send_message` will eventually find this clause:

```prolog
some_method(some_aspect, some_object(Foo), Parameter1, Parameter2) :-
  <body of clause goes here per usual>.
```

In fact, running `listing(some_method)` will in fact show relevant clauses such as the above. 

## Modules and Aspects

Because methods for a given object may be implemented in many different aspects (that's the point of the AOP style), modules take on a little less meaning in an AOP code, as imports and exports of methods no longer apply to objects, aspects, or methods: methods are essentially "global" to the object for which they are defined. Aspects and objects are global to the application that has loaded them. Structuring code in modules is still recommended, as much of the source code loading machinery of Prolog leverages it, and it will help with interfacing to traditional, non-AOP Prolog code.

For modules that define aspects, its usually a good idea to name the module after the aspect: for example, if the aspect is `some_aspect(Foo)` then a good convention is to name the module `some_aspect`. When multiple source files (and thus multiple modules) are necessary to fully implement an aspect, then sub-modules loadable with `use_module(some_aspect/internals`) can have the module name `some_aspect_internals`.

## Reflection

For situations where introspection of the aspects, objects, and methods available in an application, `aop` defines several predicates and common methods available universally. 

Note that for each of these, there are internal objects or terms used to described them, and the nature of those terms may change over time -- thus accessing details (e.g., name of an aspect) is best done not with unification against the object but by using defined accessors on the object.

* `current_aspect(AspectName)` - When called successively, will iterate over all names of loaded aspects in the application.
* `current_enabled_aspect(AspectName)` - When called successively, will iterate over names of all _enabled_ aspects.
* `enable_aspect(AspectName)` / `disable_aspect(AspectName)` - Aspects can be indepdently enabled or disabled dynamically: methods in a disabled aspect have no effect, and evaluation will fail.
* `current_object(Object)` - Returns the internal description of the object; the accessors `::aspect(Aspect)`, `::object(Object)`, and `::original_module(Module)` are all available for further inspection.
* current_method(Method) - Returns the internal description of a method. The following accessors are available:
  * `::aspect(Aspect)` - Aspect in which the method was declared
  * `::object(Object)` - Object in which the method was declared
  * `::name(Name)` - The name of the method (extracted via `functor/2`)
  * `::arity(Arity)` - The arity of the method as declared (note: does not include any internal arguments passed to implementing clauses)
  * `::declaring_module(Module)` - Module where the method was declared
  * `::predicate(Name/Arity)` - Utility method, combinging `name` and `arity`
* `::method(Method)` - Iterates over the methods defined on the receiver
* `::apply(Partial, Args)` - Constructs a full message by adding `Args` onto the end of the argument list in `Partial`, and then invoking the created message on the receiver

## Assertions

Not only do objects provide scoping for methods, but they can also provide scoping for arbitrary facts or rules. The `aop` library provides on all objects the basic definitions necessary for asserting / retracting facts or rules on the receiving object. Each such assertion is expanded to include the aspect and object as initial arguments in the head of the asserted clause. Assertions are added to the module where the `new_object` definition for the object originally appeared.

Available methods with analogous meanings as the base prolog definitions, but scoped to the object include the following:

* `assert/1`
* `assert/2`
* `asserta/1`
* `asserta/2`
* `assertz/1`
* `assertz/2`
* `retract/1.
* `retractall/1`