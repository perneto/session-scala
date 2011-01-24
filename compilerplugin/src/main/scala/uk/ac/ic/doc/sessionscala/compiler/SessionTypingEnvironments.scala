package uk.ac.ic.doc.sessionscala.compiler

trait SessionTypingEnvironments
        extends SessionTypedElementsComponent
        with ScribbleModelFactories
        with ScalaTypeSystemComponent
        with CommonEnvironments
        with InferenceEnvironments
        with CheckingEnvironments
