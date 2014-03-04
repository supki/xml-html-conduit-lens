0.3.0.0
=======

  * Helpers to render values of `Element` type

  * Complete set of isos/prisms for types in Text.XML

  * `attr` is now a Lens, not a Traversal, so it can add and remove attributes from the node

  * `Ixed` instance for `Element` traverses child nodes instead of attributes, which seems saner

  * Avoid working with `def` directly, instead take settings endomorphisms as arguments in `*With` functions

0.2.0.0
=======

  * Update to `lens-4.0`

0.1.0.1
=======

  * _Maintenance_: Upper bound on `lens`

0.1.0.0
=======

  * Initial release
