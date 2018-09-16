#lang scribble/manual

@(require (for-label racket))
@(require (for-label racket/class))


@title{GriftOS - A Racket MUD driver}

@section[#:tag "getting-started"]{Getting Started}

GriftOS is a driver intended to run a LPC-like MUDlib.  Persistent MUD objects are handled using Racket's class system (see @racket[class])

You can create classes and objects like normal in GriftOS, but you @bold{must} be aware that such objects cannot be saved.  Objects to be saved must be defined using





@section[#:tag "saved"]{Saved Objects}

The majority of the objects you create should be Saved (persistent) Objects.  These are instances of a Saved class.  

@subsection{Defining Saved Classes}

@defform[(define-mud-class* id super-expr (interface-expr ...)
           saved-class-clause ...)
         #:grammar
         [(saved-class-clause
           (code:line class-clause)
           (code:line (init-field/index init-decl ...))
           (code:line (init-field/nosave init-decl ...))
           (code:line (field/index field-decl ...))
           (code:line (field/nosave field-decl ...))
           (code:line (define/index id expr))
           (code:line (define/nosave id expr))
           (code:line (define-values/index (id ...) expr))
           (code:line (define-values/nosave (id ...) expr))
           (code:line (begin saved-class-clause ...)))]
         #:contracts ([interface-expr interface?])]{
 Creates a Saved class, similar to the behavior of @racket[class*], and binds it to @racket[id].  There are a few key differences:
 @itemlist[@item{A Saved class is a @racket[mud-class?] rather than being a @racket[class]}
          @item{Every unique pair of filename and @racket[id] is assigned a unique "Class ID" by the griftOS backend, so that the correct identifier can be dynamically required when deserializing from the database.}
          @item{The super class must inherit from @racket[mud-object%] in order to have the correct database methods.}
          @item{All object variables (identifiers created through @racket[define], @racket[define-values], @racket[field], or @racket[init-field]) are saved when writing the object to the database.
                                                  These forms all have a /nosave variant which skips the variable when serializing.  Additionally, they have an /index form which saves the variable in a separate table (so that the object can be found with a database search rather than by loading all objects
                                                   to inspect them using Racket).}
          @item{Because of the way that deserialization works, @racket[init] and @racket[init-field] require a default value (i.e. the object must be able to be instantiated without any arguments)}
          @item{@racket[id] is automatically provided (otherwise the deserialization could not sucessfully retrieve it from the source file)}]
}


@defform[(define-mud-class id super-expr saved-class-form ...)]{As @racket[define-mud-class*] but without any interfaces}

@defform[(mixin/griftos (from ...) (to ...) saved-class-clause ...)]{
Produces Saved mixin.  Similar to a regular Racket @racket[mixin], but the domain must always include the @racket[savable<%>] interface (it does not need to be listed), and the saved mixin will automatically be augmented to save and index any variables created as part of the class clauses.}

Note that a mixin that does not define any fields or private variables can (and should) be created with the regular @racket[mixin].

@defform[(define-mud-class/mixin id super-expr mixin-expr ...)
         #:contracts ([super-expr (or/c class? mud-class?)]
                      [mixin-expr (-> class? class?)])]{
Creates a Saved class by applying the consumed mixins to the class described by @racket[super-expr], and binds the resulting class to @racket[id].  The first @racket[mixin-expr] is applied last (as in @racket[compose1]).  

Note that this form does not allow any class clauses by itself.  One should create a base class using @racket[define-mud-class*] and then create a mixed class by applying
one or more Saved mixins with @racket[define-mud-class/mixin].  That way you can inherit from the base class as needed.  TODO example

Alternately, one can apply one or more mixins to the base class expression.
}

@defform[(define-inline-class* id super-expr (interface-expr ...)
           saved-class-clause ...)]{
Creates an Inline class, similar to the behavior of @racket[define-mud-class*].  An inline class is not saved directly to the database,
                                                    and therefore does not have a unique object ID number.  Instead, it is able to be written
                                                    via @racket[write] and retrieved again via @racket[read] (using the griftOS reader).

                                                    Note that while it's possible to alias an instance of an inline class, aliasing will be broken
                                                    by serialization / deserialization.  E.g. if two NPCs reference the same inline object,
                                                    then after a server restart they will each reference a different object (with identical fields)}


@defform[(define-inline-class id super-expr saved-class-clause ...)]{As @racket[define-inline-class*] but with no interface expressions}
                                    
@defform[(saved-struct id maybe-super (field ...))]{
Defines a saved struct called id in the same manner as @racket[struct].  There are a few key differences:
@itemlist[@item{The struct is always mutable and transparent, and does not allow any struct options other than those}
          @item{The struct has a custom writer than displays / prints in the object-constructor format, but writes in a way that the griftOS
                reader can dynamically require the appropriate source file in order to deserialize the struct}
          @item{Fields do not allow the @racket[#:auto] option}]

As with Inline objects, saved structs will break aliasing after deserialization.
}

@subsection{Working with Saved Objects}

Saved objects are created in the typical way (i.e. using @racket[new], @racket[instantiate], or @racket[make-object]).  When creating a new object, it will immiediately be inserted into the database.  After creation, the object's @racket[on-create] method will be called.

Generally one should avoid doing too much work in the body of the class constructor, as this will not only be called when creating an object, but als  whenever loading an object from the database.  Most setup work should be placed in the object's @racket[on-create] method.

When a saved object is loaded, GriftOS first create a new object of the correct class @bold{with no initializations}, before calling the @racket[load] method to reinitialize any saved variables.  After variables have been loaded from the database, the object's @racket[on-load] method will be called.

One can manually save an object at any time by using @racket[save-object].  Additionally, objects that have been updated
will periodically be saved to the database, and a background @racket[will-executor] saves Saved Objects immediately before they are garbage collected.

@subsection{Singletons}

@defform[(get-singleton class-expr (id by-name-expr) ...)]{
If the Saved class @racket[class-expr] has a singleton in the database, that singleton object is returned.  If not, a new object is created (by way of @racket[(new class-expr (id by-name-expr) ...)]) and registered as the singleton object for @racket[class-expr].
                   Note that the optional initialization expressions will only be evaluated if a new object must be created.}

