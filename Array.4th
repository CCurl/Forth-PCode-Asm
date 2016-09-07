\ ********************************************************************************
\ Arrays return the starting address of the array.
\ This is a "counted" array, just like strings, so the first cell is the size.
\ Use COUNT to get the number of elements and address of the first cell.
\ Indexes go from 0 to n-1 (base 0)
\ 10 array test    ... 
\ 12 3 test !array ... stores 12 in position 3 of array 'test'
\ 3 test array@    ... fetch value at position 3 from array 'test'
\ ********************************************************************************

: ARRAY.Module ;

: ARRAY.COUNT DUP 4 + SWAP @ ;
: ARRAY DUP 1+ 4 * ALLOCATE DROP TUCK C! word create.empty ( I_DICTP C, LAST , ) I_LITERAL C, , I_RETURN C, ;
\ ( n array -- bool )
: ?ARRAY.in.bounds @ 0 SWAP BETWEEN ;

( n array -- bool )
: ARRAY.Check.Bounds ?ARRAY.in.bounds
	DUP 0= IF 
		." index out of bounds." 
	THEN ;
: !ARRAY ( val pos array -- ) 2DUP Array.Check.Bounds IF 4 + SWAP 4 * + ! ELSE 2DROP DROP THEN ;
: ARRAY@ ( pos array -- val ) 2DUP Array.Check.Bounds IF 4 + SWAP 4 * + @ ELSE 2DROP THEN ;
: .ARRAY ARRAY.COUNT 0 DO DUP @ . 4 + LOOP DROP ;
