#lang scribble/manual

@(require (for-label rackmud))


@title{RackMud - A Racket MUD driver}

@section[#:tag "getting-started"]{Getting Started}

RackMud is a driver intended to run a LPC-like MUDlib.  Persistent MUD objects are handled using Racket's class system (see @racket[class])

Objects to be saved must be defined using @racket[define-saved-class] (or @racket[defined-saved-class*]).
You can define classes and objects like normal, using @racket[class], etc. but you @bold{must} be aware that such objects cannot be saved.

@subsection{Working with Saved Objects}

All saved objects must be descended from the @racket[saved-object%] class.

@defclass[saved-object% object% ()]{

Top level expressions should generally be avoided, because the object will be instantiated
prior to being deserialized from the database.  That means during instantiation,
all of its saved variables and fields will be undefined.

Instead, the code should be placed in either
@racket[on-create] or @racket[on-load] as appropriate.

@defmethod[#:mode public
           (on-create) any/c]{
Override this method to add code that will only be run when creating the object for the first time.
                        }

@defmethod[#:mode public
           (on-load)any/c]{
Override this method to add code that will be run after completing deserialization.  It
is also called after @racket[on-create] during initial object creation.
                        }
                                           
                                           }


To define new saved object classes

@defform/subs[
#:literals (inspect init init-field field inherit-field init-rest init-rest
            public pubment public-final override override-final overment augment augride
            augment-final private abstract inherit inherit/super inherit/inner
            rename-super rename-inner begin lambda case-lambda let-values letrec-values
            define-values #%plain-lambda chaperone-procedure)
(define-saved-class* id superclass-expr (interface-expr ...)
  saved-class-clause
  ...)
([saved-class-clause
  class-clause
  (init-field init-decl ...)
  (init-field/nosave init-decl ...)
  (field field-decl ...)
  (field/nosave field-decl ...)
  (define id expr)
  (define/nosave id expr)
  (define-values (id ...) expr ...)
  (define-values/nosave (id ...) expr ...)
  (index index-clause ...)

  (begin saved-class-clause ...)]

[init-decl
  id
  (renamed)
  (maybe-renamed default-value-expr)]

[field-decl
  (maybe-renamed default-value-expr)]

[maybe-renamed
  id
  renamed]

[renamed
  (internal-id external-id)]

[index-clause
 (id)
 (id index-type)
 (id index-type id)]

[index-type
 index-search-type
 (list container-type index-type)])]{

Defines a new class, similar to @racket[(define id (class* (interface-expr ...) class-clause ...))]
but all fields and other member variables will be saved to the database.
For temporary / cache fields, use the /nosave equivalents, e.g. @racket[(define/nosave id value)] to
create a private variable that is not saved.

Note that so the deserializer can find the appropriate class value, the newly defined variable
is automatically provided by the enclosing module.

Additionally, @racket[this] will be bound to be a lazy reference to the current object, rather than
the object value itself.

Finally, you can create automatic database indices using the @racket[index] subform.

@specsubform[(index index-clause ...)]{
Creates database indices for quickly finding objects using this field.

@specsubform[[field-name]]{
                           Creates an index for field name.  The type will be assumed to be a simple
                           atomic type (number, boolean, string, symbol).
                           The name given will be find-<class-name>-by-<field-name>.
                           }

@specsubform[[field-name field-type]]{
                           Creates an index for field name.  The type can be one of the following forms

                           @specsubform[simple]{
An atomic type, equivalent to not specifying the type.  For readability you can also use the name
"string" "symbol" "boolean" or "number" but this is for human eyes only.

The search function generated has an optional second parameter that specifies the type of match to use.
The default is @racket['=] but you can also use @racket['!=] @racket['<] @racket['>] @racket['<=] and
@racket['>=].}

                           @specsubform[list]{
A list of simple values.

The search function generated expects a single atomic value, and matches containers that contain
that value.  Additionally, it can be provided a list of atomic values, and a second parameter
that is either @racket['contains-all] or @racket['contains-any], indicating whether a container
should match if it contains all of the values being search, or any of them.

TODO: Maybe we want exact match, too?}

                           @specsubform[vector]{
As with @racket[list], but a vector instead.
                                                }
@specsubform[set]{
As with @racket[list], but a hash set instead.
                                                }
                           @specsubform[hash]{
A hash table where the keys are atomic types.  @racket[symbol-table] is a special case where
the table is a hasheq (mutable or not) and the keys are symbols (this is a bit more efficient to
construct a query for).

The search function defaults to consuming a key and searching for tables that contain that key.

There is a second optional parameter, value, that defaults to being ignored.

There is a third optional parameter that specifies the search mode. It defaults to @racket['has-key?]
as described above.

The third optional parameter can be set to @racket['has-all-keys?] or @racket['has-any-keys?]
to find tables that contain all listed keys, or any of the listed keys, respectively.  In this
case the first parameter must be a list of keys, rather than a single key.

The third parameter can be set to @racket['has-pair?] in which case it searches for hash tables
that contain a pair where the key is equal to the first parameter, and the value to the second.

There are list forms of this search mode, @racket['has-all-pairs?] and @racket['has-any-pairs?]
where the first parameter is interpreted as a list of keys, and the second as a list of
corresponding values

                                                           }
                           
                           

                           @specsubform[(box field-type)]{
    A box containing the given type.  This will allow the inner type to be searched.  Otherwise
    it cannot be searched.
                           }

                           @specsubform[(vbox field-type)]{
    As with box, but a versioned-box is the containing type.
                           }
                           }

@specsubform[[field-name field-type name]]{
                                      As with @racket[[field-name field-type]] but
the name of the search function will be @racket[name].
}
                          

}
}

@subsection{Singletons}

@defform[(get-singleton class-expr (id by-name-expr) ...)]{
If the Saved class @racket[class-expr] has a singleton in the database, that singleton object is returned.  If not, a new object is created (by way of @racket[(new class-expr (id by-name-expr) ...)]) and registered as the singleton object for @racket[class-expr].
                   Note that the optional initialization expressions will only be evaluated if a new object must be created.}

