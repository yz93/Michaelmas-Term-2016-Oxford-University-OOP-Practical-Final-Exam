// Observable.scala
// Copyright (c) 2015 J. M. Spivey

import scala.collection.mutable.ArrayBuffer

/** Mixin for classes that can update observers */
trait Observable[T] { this: T =>

    /** The list of registered observers. */
    val observers = new ArrayBuffer[Observable.Observer[T]]

    /** Add another observer to the list. */
    def addObserver(observer: Observable.Observer[T]) {
        observers.append(observer);
    }
    
    /** Notify all observers that the subject has changed */
    def notifyObservers() {
        for (o <- observers) o.refresh(this);
    }
}

object Observable {
    trait Observer[T] {
        def refresh(subject: T)
    }
}
