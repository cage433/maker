in_namespace('Versioning') {

  # Defines a standard trait implemented by all objects that can be sent in a versioned manner. Providing
  # version information with the entities allows for external understanding of changes.
  mixin('Versioned') {
    # The version of this entity. This may be either a sequentially managed version number, or more likely, a hash
    # generated over the content making up the object.
    field 'version', :string
  }

}
