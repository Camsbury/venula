# venula - Wield your application's state declaratively

This library provides utilities for manipulating state declaratively,
while remaining in the bounds of provided constraints. Useful for
property tests, and just all things state related. All the user
has to provide is:
- A sum type representing the different data domains in their application
- A list of constraints on the above types
- An `Iso'` from their state representation to a list of `Vertex` pairs, representing directed relationships between data
