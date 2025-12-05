- We are using tinycc for preprocessing, but we could in fact use any compiler avaible and preferably the one used to build R/ there are some complications due to the fact that this is likely not the compiler used for building the shared library we will load anyway.

- Right now lot of junk from system headers are generated. we should document and ask user to provide sensible headers
without system includes