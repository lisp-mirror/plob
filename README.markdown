
# Persistent Lisp Objects!

>  Version 2.11 of May 28, 2005

https://plob.sourceforge.net/

> It's funny I thought I should do things differently for a database, but Plob lets you do things in a Lisp way.
> -- M. A.

# What is it about?

 The system PLOB! (Persistent Lisp OBjects!) implements orthogonal persistency for LISP and CLOS objects. Besides offering pure persistency, on the one side it contains important database features like transactions, locking and associative search over persistent objects. On the other side, PLOB! mimics the features of CLOS' Metaobject Protocol (MOP) as far as possible, e.g. by its implemented self-describing persistent objects and support for class changes promoted dynamically to persistent objects (schema evolution).

# Transparent Persistency

An important topic was to make the interface between LISP and persistency as transparent to the user as possible, without sacrificing efficiency. PLOB! solves both problems: By employing the possibilites of CLOS' MOP, the user interface to persistency is as simple as it can be. In fact, using only a few statements of LISP code will make instances of CLOS classes persistent, e.g.

~~~lisp
    CL-USER 3 > (defclass person ()
                  ((name :initarg :name :index (btree :test equal)))
                  (:metaclass persistent-metaclass))
~~~

declares a class person with its instances being persistent and an index for associative search on its slot name (actually, the class metaobject which represents the class person itself will become persistent, too). Using some other statements will make ordinary LISP and structure objects persistent. It is also possible to make CLOS objects persistent which are out of the user's control, for example, instances where the defclass defining the class is not available and cannot be modified as indicated in the example above. Besides that, PLOB! has the notion of persistent packages and persistent symbols for binding persistent objects:

~~~lisp
    CL-USER 4 > (setf #!*a-person*
                      (make-instance 'person :name "Kirschke"))
~~~

will bind the persistent instance generated from the make-instance call to the persistent symbol *a-person*. Similar to transient packages and symbols, an object can be bound to a persistent symbol and thereby gets persistent and identifyable:

~~~lisp
    CL-USER 5 > (setf #!*a-string* "A persistent string.")
    ... some years and LISP processes later ...
    CL-USER 99 > #!*a-string*
    "A persistent string."
~~~

# Client/server architecture

https://plob.sourceforge.net/
