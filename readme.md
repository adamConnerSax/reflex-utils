reflex-utils 

Various form and layout utilities for reflex-dom:

1. A utility to build forms from data structures. Also allows "observing" a reflex Dynamic or Widget as well as observing the flow in a MonadWidget function (f::MonadWidget t m=>a -> m b). See app/formBuilder/Main.hs for examples.  Uses generics-sop to decompose sum-types into individual constructors and update each only when needed when a sum-type is part of a form.  Also, has instances for many common containers ([],Map,IntMap,etc.) and uses listWithKeyShallowDiff (from Reflex-Dom) to keep updates to the widgets as efficient as possible.  Special builders are available for containers where the contained items are instances of Eq, since they can be handled even more efficiently.

2. Some simple wrappers around flex-box css to make it easy to use flex for layout in rows and columns as well as to do centering.

3. Some more complex wrappers around flex and structured grids to allow for post-layout column-width calculation as well as the connecting of styling events to layout divs via tags.

4. An extremely simplified wrapper around the reflex-dom-contrib dynamic-tab function.  For quick deployment of a basic (and static) tabbed layout.

For examples of the latter 3, see app/layout/Main.hs


See stack.yaml for details of GHCJS version as well as relfex, reflex-dom, reflex-dom-contrib versions.
NB; requires a newer version of generics-sop (>=2.1) than is in the resolver given.  

