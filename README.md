# DESMOS

An implementation of DES encryption/decryption for SML (written specifically for
[Moscow ML](https://mosml.org/), may or may not work with [other](http://mlton.org/) [dialects](https://smlnj.org/)).
Fulfills [Ronald Rivest's correctness test](https://people.csail.mit.edu/rivest/pubs/Riv85.txt) for
DES implementations. This code was written as an assignment for the course Data Security at the
[IT University of Copenhagen](https://itu.dk) in the Fall 2003 semester, and includes a short
report in Danish.

## Security of 3DES

> *"In cryptography, Triple DES (3DES or TDES), officially the Triple Data Encryption Algorithm
> (TDEA or Triple DEA), is a symmetric-key block cipher, which applies the DES cipher algorithm
> three times to each data block. The Data Encryption Standard's (DES) 56-bit key is no longer
> considered adequate in the face of modern cryptanalytic techniques and supercomputing power. A
> CVE released in 2016, [CVE-2016-2183](https://nvd.nist.gov/vuln/detail/CVE-2016-2183) disclosed a
> major security vulnerability in DES and 3DES encryption algorithms. This CVE, combined with the
> inadequate key size of DES and 3DES, NIST has deprecated DES and 3DES for new applications in
> 2017, and for all applications by 2023. It has been replaced with the more secure, more robust
> AES."* &mdash;[Wikipedia](https://en.wikipedia.org/wiki/Triple_DES)

**TL;DR:** don't use this code for anything.
