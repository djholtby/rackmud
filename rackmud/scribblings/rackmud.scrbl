#lang scribble/manual

@(require (for-label racket))
@(require (for-label racket/class))
@(require (for-label rackmud))


@title{RackMud - A Racket MUD driver}

@section[#:tag "getting-started"]{Getting Started}

RackMudS is a driver intended to run a LPC-like MUDlib.  Persistent MUD objects are handled using Racket's class system (see @racket[class])

You can create classes and objects like normal in GriftOS, but you @bold{must} be aware that such objects cannot be saved.  Objects to be saved must be defined using

@subsection{Working with Saved Objects}

Saved objects are created in the typical way (i.e. using @racket[new], @racket[instantiate], or @racket[make-object]).  When creating a new object, it will immiediately be inserted into the database.  After creation, the object's @racket[on-create] method will be called.

@subsection{Singletons}

@defform[(get-singleton class-expr (id by-name-expr) ...)]{
If the Saved class @racket[class-expr] has a singleton in the database, that singleton object is returned.  If not, a new object is created (by way of @racket[(new class-expr (id by-name-expr) ...)]) and registered as the singleton object for @racket[class-expr].
                   Note that the optional initialization expressions will only be evaluated if a new object must be created.}

