; ------------------------------------------------------
; This is a PCode based implementation of Forth.
;
; By Chris Curl
;
; It was written using Visual Studio 2013, and is for
; a PC running Windows.
;
; I have tried to document the approach and considerations
; in the source file. Please view that for details.
;
; It contains most of the core Forth words, and seems to 
; look and feel like Forth, but I doubt that it complies
; with the Forth ANS standard.
;
; There is currently no floating point support.
;
; The next big thing for this project is to enhance it
; to generate real machine code. I haven't decided if I 
; want to try a JIT compiler, or if I want to generate
; the host code in real time.
;
; Thanks, Chris
; ------------------------------------------------------
