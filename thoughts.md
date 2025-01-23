- Why am I using the iterator to push all the tokens into a vector when
I could just use the iterator returned by logos in the parser itself?
Pass the logos iterator as a field of the parser struct.


